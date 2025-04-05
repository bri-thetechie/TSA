library(shiny)
library(shinythemes)
library(ggplot2)
library(forecast)
library(tseries)
library(readxl)
library(plotly)
library(lmtest)
library(nortest)
library(TTR)
library(tools)
library(uroot)
library(quantmod)
library(rugarch)
library(shinyWidgets) # For dropdownButton

# Custom CSS for dark theme
dark_theme_css <- "
body {
  background-color: #000000 !important;
  color: #FFFFFF !important;
}
.sidebar {
  background-color: #333333 !important;
  color: #FFFFFF !important;
}
.sidebar h3 {
  color: #FFFFFF !important;
}
.sidebar hr {
  border-top: 1px solid #555555 !important;
}
.btn-primary,
.btn-info,
.btn-warning,
.btn-success,
.btn-danger {
  background-color: #333333 !important;
  color: #FFFFFF !important;
  border-color: #555555 !important;
}
.btn-primary:hover,
.btn-info:hover,
.btn-warning:hover,
.btn-success:hover,
.btn-danger:hover {
  background-color: #555555 !important;
  color: #FFFFFF !important;
  border-color: #777777 !important;
}
.selectize-input {
  background-color: #333333 !important;
  color: #FFFFFF !important;
  border-color: #555555 !important;
}
.selectize-dropdown {
  background-color: #333333 !important;
  color: #FFFFFF !important;
  border-color: #555555 !important;
}
.numeric-input input[type=\"number\"] {
  background-color: #333333 !important;
  color: #FFFFFF !important;
  border-color: #555555 !important;
}
.slider-track,
.slider-selection {
  background-color: #FFFFFF !important;
}
.slider-handle {
  background-color: #FFFFFF !important;
  border-color: #555555 !important;
}
.checkbox input[type=\"checkbox\"] + span {
  color: #FFFFFF !important;
}
.radio label {
  color: #FFFFFF !important;
}
.tabbable > .nav > li > a {
  background-color: #333333 !important;
  color: #FFFFFF !important;
  border-color: #555555 !important;
}
.tabbable > .nav > li.active > a,
.tabbable > .nav > li.active > a:hover,
.tabbable > .nav > li.active > a:focus {
  background-color: #000000 !important;
  color: #FFFFFF !important;
  border-color: #555555 #555555 #000000 #555555 !important;
}
.well {
  background-color: #333333 !important;
  color: #FFFFFF !important;
  border-color: #555555 !important;
}
.dropdown-menu {
  background-color: #333333 !important;
  color: #FFFFFF !important;
  border-color: #555555 !important;
}
.dropdown-menu > li > a {
  color: #FFFFFF !important;
}
.dropdown-menu > li > a:hover,
.dropdown-menu > li > a:focus {
  background-color: #555555 !important;
  color: #FFFFFF !important;
}
"

# UI ----
ui <- fluidPage(
  tags$head(tags$style(HTML(dark_theme_css))),
  titlePanel(tags$h1("Automated Time Series Analysis Dashboard", style = "color:#FFFFFF;")),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Data Input", style = "color:#FFFFFF;"),
      fileInput("file", "Upload Data File", accept = c(".csv", ".xlsx")),
      selectInput("column", "Select Numeric Column:", choices = NULL),
      numericInput("frequency", "Set Time Series Frequency:", value = 12, min = 1),
      actionButton("set_freq", "Set Frequency", class = "btn-info"),
      tags$hr(),
      
      tags$h3("Analysis Options", style = "color:#FFFFFF;"),
      numericInput("forecast_horizon", "Forecast Horizon:", value = 10, min = 1), # Moved forecast horizon to top
      tags$hr(),
      actionButton("run_analysis", "Run Automated Analysis", class = "btn-primary btn-lg")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series Plot", plotlyOutput("tsPlot")),
        tabPanel("Stationarity Tests", verbatimTextOutput("adfResult"), verbatimTextOutput("hegyResult")),
        tabPanel("Decomposition", plotlyOutput("decompPlot")),
        tabPanel("Detrending", plotlyOutput("maPlot"), plotlyOutput("detrendedPlot")),
        tabPanel("ACF/PACF", plotlyOutput("acfPlot"), plotlyOutput("pacfPlot")),
        tabPanel("Periodogram", plotOutput("periodogramPlot")),
        tabPanel("ARIMA/SARIMA Model", verbatimTextOutput("arimaSummary")),
        tabPanel("GARCH Model", verbatimTextOutput("garchSummary")),
        tabPanel("Forecasting", plotlyOutput("forecastPlot"), verbatimTextOutput("forecastResults")),
        tabPanel("Residual Analysis",
                 verbatimTextOutput("shapiroTest"),
                 plotOutput("residualPlot"),
                 plotOutput("qqPlot"),
                 plotOutput("ppPlot"),
                 plotlyOutput("residualAcfPlot"),
                 verbatimTextOutput("dwTest"),
                 verbatimTextOutput("residualMean"),
                 verbatimTextOutput("bpTest"),
                 verbatimTextOutput("archLMTest")
        )
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    file_ext <- tools::file_ext(input$file$name)
    
    if (file_ext == "csv") {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (file_ext == "xlsx") {
      df <- read_excel(input$file$datapath)
    } else {
      stop("Invalid file type. Please upload a .csv or .xlsx file.")
    }
    df[, sapply(df, is.numeric), drop = FALSE]
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "column", choices = names(data()))
  })
  
  tsData <- reactiveVal()
  originalTsData <- reactiveVal() # To keep the original data for multiplicative check
  
  observeEvent(input$set_freq, {
    req(input$column)
    if (!is.null(data()[[input$column]])) {
      originalTsData(ts(data()[[input$column]], frequency = input$frequency))
      tsData(ts(data()[[input$column]], frequency = input$frequency))
    }
  })
  
  output$tsPlot <- renderPlotly({
    req(tsData())
    ggplotly(autoplot(tsData()) + theme_bw())
  })
  
  # Reactive values to store model fits and forecasts
  arimaModel <- reactiveVal()
  garchModelFit <- reactiveVal()
  forecast_results <- reactiveVal()
  decomposition <- reactiveVal(NULL)
  detrendedTsData <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    req(tsData())
    original_ts <- originalTsData()
    current_ts <- tsData()
    freq <- frequency(current_ts)
    h <- input$forecast_horizon
    
    # --- Check for Multiplicative Time Series ---
    is_multiplicative <- any(current_ts <= 0)
    if (is_multiplicative) {
      current_ts <- log(abs(current_ts) + 1e-9) # Add a small constant to avoid log(0)
      output$tsPlot <- renderPlotly({ ggplotly(autoplot(current_ts) + labs(title = "Log Transformed Time Series") + theme_bw()) })
    }
    
    # --- Stationarity Test ---
    adf_result <- adf.test(current_ts, alternative = "stationary")
    is_stationary <- adf_result$p.value <= 0.05
    output$adfResult <- renderPrint({ adf_result })
    
    if (is_stationary) {
      # --- Stationary Data: Fit ARIMA ---
      output$hegyResult <- renderPrint("HEGY test not applicable for stationary data.")
      output$decompPlot <- renderPlotly(NULL)
      output$maPlot <- renderPlotly(NULL)
      output$detrendedPlot <- renderPlotly(NULL)
      
      # ACF and PACF
      output$acfPlot <- renderPlotly({ ggplotly(autoplot(Acf(current_ts, lag.max = 2 * freq)) + theme_bw()) })
      output$pacfPlot <- renderPlotly({ ggplotly(autoplot(Pacf(current_ts, lag.max = 2 * freq)) + theme_bw()) })
      output$periodogramPlot <- renderPlot({ plot(spectrum(current_ts, plot = FALSE), main = "Periodogram", bg = "white", fg = "black", col.main = "black", col.lab = "black", col.axis = "black", col = "black") })
      
      # Automatically determine ARIMA order (very basic approach - consider AIC/BIC in real scenario)
      auto_arima_fit <- tryCatch(auto.arima(current_ts), error = function(e) NULL)
      arimaModel(auto_arima_fit)
      output$arimaSummary <- renderPrint({ if (!is.null(auto_arima_fit)) summary(auto_arima_fit) else "Automatic ARIMA failed." })
      
      # Forecasting
      if (!is.null(auto_arima_fit)) {
        fcast <- tryCatch(forecast(auto_arima_fit, h = h), error = function(e) NULL)
        forecast_results(fcast)
        output$forecastPlot <- renderPlotly({ if (!is.null(fcast)) ggplotly(autoplot(fcast) + theme_bw()) else ggplotly(ggplot() + annotate("text", x = 1, y = 1, label = "ARIMA forecast failed.", color = "black") + theme_bw()) })
        output$forecastResults <- renderPrint({ if (!is.null(fcast)) print(fcast) else "ARIMA forecast failed." })
        
        # Residual Analysis
        if (!is.null(fcast) && !is.null(fcast$residuals)) {
          residuals_ts <- fcast$residuals[!is.na(fcast$residuals)]
          output$shapiroTest <- renderPrint({ shapiro.test(residuals_ts) })
          output$residualPlot <- renderPlot({ plot(residuals_ts, type = "l", main = "Residual Plot", bg = "white", fg = "black", col = "black", col.main = "black", col.lab = "black", col.axis = "black") })
          output$qqPlot <- renderPlot({ qqnorm(residuals_ts, bg = "white", fg = "black", col = "black", col.main = "black", col.lab = "black", col.axis = "black"); qqline(residuals_ts, col = "red") })
          output$ppPlot <- renderPlot({ ppoints_res <- pnorm(residuals_ts, mean = mean(residuals_ts), sd = sd(residuals_ts)); plot(sort(ppoints_res), sort(residuals_ts), main = "PP Plot", xlab = "Theoretical", ylab = "Sample", bg = "white", fg = "black", col = "black", col.main = "black", col.lab = "black", col.axis = "black"); abline(0, 1, col = "red") })
          output$residualAcfPlot <- renderPlotly({ ggplotly(autoplot(Acf(residuals_ts)) + theme_bw()) })
          output$dwTest <- renderPrint({ dwtest(residuals_ts ~ 1) })
          output$residualMean <- renderPrint({ paste("Residual Mean:", mean(residuals_ts)) })
          output$bpTest <- renderPrint({ Box.test(residuals_ts, lag = 10, type = "Ljung-Box") })
          arch_lm_test <- tryCatch(lmtest::bgtest(residuals_ts ~ 1, order = 10), error = function(e) "ARCH LM test failed.")
          output$archLMTest <- renderPrint({ arch_lm_test })
          
          # Check for Heteroscedasticity in Residuals and potentially fit GARCH
          if (inherits(arch_lm_test, "htest") && arch_lm_test$p.value <= 0.05) {
            garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)), distribution.model = "norm")
            garch_fit <- tryCatch(ugarchfit(spec = garch_spec, data = residuals_ts), error = function(e) NULL)
            garchModelFit(garch_fit)
            output$garchSummary <- renderPrint({ if (!is.null(garch_fit)) show(garch_fit) else "Automatic GARCH failed." })
            
            if (!is.null(garch_fit)) {
              garch_fcst <- tryCatch(ugarchforecast(garch_fit, n.ahead = h), error = function(e) NULL)
              # Display GARCH forecast (conditional variance)
              output$forecastPlot <- renderPlotly({
                if (!is.null(garch_fcst)) {
                  forc <- sigma(garch_fcst)
                  forecasting <- data.frame(Time = index(forc), Variance = as.numeric(forc))
                  ggplot(forecasting, aes(x = Time, y = Variance)) + geom_line(color = "black") + labs(title = "GARCH Conditional Variance Forecast", x = "Time", y = "Conditional Variance") + theme_bw()
                } else {
                  ggplot() + annotate("text", x = 1, y = 1, label = "GARCH forecast failed.", color = "black") + theme_bw()
                }
              })
              output$forecastResults <- renderPrint({ if (!is.null(garch_fcst)) { cat("GARCH Forecast (Conditional Variance):\n"); print(sigma(garch_fcst)) } else "GARCH forecast failed." })
              
              # Residual analysis of GARCH (standardized residuals) - omitted for brevity, but should be done
            }
          } else {
            output$garchSummary <- renderPrint("Residuals do not show significant heteroscedasticity based on ARCH LM test.")
          }
          
        }
      }
      
    } else {
      # --- Non-Stationary Data ---
      output$acfPlot <- renderPlotly({ ggplotly(autoplot(Acf(current_ts, lag.max = 2 * freq)) + theme_bw()) })
      output$pacfPlot <- renderPlotly({ ggplotly(autoplot(Pacf(current_ts, lag.max = 2 * freq)) + theme_bw()) })
      output$periodogramPlot <- renderPlot({ plot(spectrum(current_ts, plot = FALSE), main = "Periodogram", bg = "white", fg = "black", col = "black", col.main = "black", col.lab = "black", col.axis = "black") })
      
      # HEGY Test for Seasonality
      if (freq > 1) {
        hegy_result <- tryCatch(hegy.test(current_ts), error = function(e) paste("HEGY Error:", e$message))
        output$hegyResult <- renderPrint({ hegy_result })
        is_seasonal_hegy <- !is.character(hegy_result) && any(hegy_result$p.values <= 0.05) # Basic check
      } else {
        output$hegyResult <- renderPrint("HEGY test requires frequency > 1.")
        is_seasonal_hegy <- FALSE
      }
      
      # Decomposition for trend and seasonality
      decomp <- tryCatch(decompose(current_ts, type = ifelse(is_multiplicative, "multiplicative", "additive")), error = function(e) NULL)
      decomposition(decomp)
      output$decompPlot <- renderPlotly({ if (!is.null(decomp)) ggplotly(autoplot(decomp) + theme_bw()) else ggplotly(ggplot() + annotate("text", x = 1, y = 1, label = "Decomposition failed.", color= "black") + theme_bw()) })
      
      if (!is.null(decomp) && !is.null(decomp$trend) && !all(is.na(decomp$trend))) {
        # Detrend using Moving Average
        ma_order_auto <- ifelse(freq > 1, freq, 3) # Adjust MA order based on frequency
        ma_vals <- SMA(current_ts, n = ma_order_auto)
        detrended_ts <- current_ts - ma_vals
        detrendedTsData(detrended_ts)
        
        output$maPlot <- renderPlotly({
          ggplotly(
            ggplot() +
              geom_line(aes(x = time(current_ts), y = current_ts), color = "black") +
              geom_line(aes(x = time(current_ts), y = ma_vals), color = "red") +
              theme_bw() + labs(title = "Time Series with Moving Average")
          )
        })
        output$detrendedPlot <- renderPlotly({
          ggplotly(
            ggplot() +
              geom_line(aes(x = time(detrended_ts), y = detrended_ts), color = "green") +
              theme_bw() + labs(title = "Detrended Time Series (Moving Average)")
          )
        })
        current_ts_for_model <- detrended_ts
      } else {
        output$maPlot <- renderPlotly(NULL)
        output$detrendedPlot <- renderPlotly(NULL)
        current_ts_for_model <- current_ts # Use original if detrending fails
      }
      
      # Fit SARIMA if seasonal (based on HEGY or decomposition)
      if (is_seasonal_hegy || (!is.null(decomp) && !all(is.na(decomp$seasonal)) && var(decomp$seasonal, na.rm = TRUE) > 1e-9)) {
        auto_sarima_fit <- tryCatch(auto.arima(current_ts_for_model, seasonal = TRUE), error = function(e) NULL)
        arimaModel(auto_sarima_fit)
        output$arimaSummary <- renderPrint({ if (!is.null(auto_sarima_fit)) summary(auto_sarima_fit) else "Automatic SARIMA failed." })
        
        if (!is.null(auto_sarima_fit)) {
          fcast <- tryCatch(forecast(auto_sarima_fit, h = h), error = function(e) NULL)
          forecast_results(fcast)
          output$forecastPlot <- renderPlotly({ if (!is.null(fcast)) ggplotly(autoplot(fcast) + theme_bw()) else ggplotly(ggplot() + annotate("text", x = 1, y = 1, label = "SARIMA forecast failed.", color = "black") + theme_bw()) })
          output$forecastResults <- renderPrint({ if (!is.null(fcast)) print(fcast) else "SARIMA forecast failed." })
          
          # Residual Analysis for SARIMA
          if (!is.null(fcast) && !is.null(fcast$residuals)) {
            residuals_ts <- fcast$residuals[!is.na(fcast$residuals)]
            output$shapiroTest <- renderPrint({ shapiro.test(residuals_ts) })
            output$residualPlot <- renderPlot({ plot(residuals_ts, type = "l", main = "Residual Plot (SARIMA)", bg = "white", fg = "black", col = "black", col.main = "black", col.lab = "black", col.axis = "black") })
            output$qqPlot <- renderPlot({ qqnorm(residuals_ts, bg = "white", fg = "black", col = "black", col.main = "black", col.lab = "black", col.axis = "black"); qqline(residuals_ts, col = "red") })
            output$ppPlot <- renderPlot({ ppoints_res <- pnorm(residuals_ts, mean = mean(residuals_ts), sd = sd(residuals_ts)); plot(sort(ppoints_res), sort(residuals_ts), main = "PP Plot (SARIMA)", xlab = "Theoretical", ylab = "Sample", bg = "white", fg = "black", col = "black", col.main = "black", col.lab = "black", col.axis = "black"); abline(0, 1, col = "red") })
            output$residualAcfPlot <- renderPlotly({ ggplotly(autoplot(Acf(residuals_ts)) + theme_bw()) })
            output$dwTest <- renderPrint({ dwtest(residuals_ts ~ 1) })
            output$residualMean <- renderPrint({ paste("Residual Mean (SARIMA):", mean(residuals_ts)) })
            output$bpTest <- renderPrint({ Box.test(residuals_ts, lag = 10, type = "Ljung-Box") })
            arch_lm_test <- tryCatch(lmtest::bgtest(residuals_ts ~ 1, order = 10), error = function(e) "ARCH LM test failed.")
            output$archLMTest <- renderPrint({ arch_lm_test })
            
            # Check for Heteroscedasticity in SARIMA Residuals and potentially fit GARCH
            if (inherits(arch_lm_test, "htest") && arch_lm_test$p.value <= 0.05) {
              garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)), distribution.model = "norm")
              garch_fit <- tryCatch(ugarchfit(spec = garch_spec, data = residuals_ts), error = function(e) NULL)
              garchModelFit(garch_fit)
              output$garchSummary <- renderPrint({ if (!is.null(garch_fit)) show(garch_fit) else "Automatic GARCH on SARIMA residuals failed." })
              
              if (!is.null(garch_fit)) {
                garch_fcst <- tryCatch(ugarchforecast(garch_fit, n.ahead = h), error = function(e) NULL)
                output$forecastPlot <- renderPlotly({
                  if (!is.null(garch_fcst)) {
                    forc <- sigma(garch_fcst)
                    forecasting <- data.frame(Time = index(forc), Variance = as.numeric(forc))
                    ggplot(forecasting, aes(x = Time, y = Variance)) + geom_line(color = "black") + labs(title = "GARCH on SARIMA Residuals - Variance Forecast", x = "Time", y = "Conditional Variance") + theme_bw()
                  } else {
                    ggplot() + annotate("text", x = 1, y = 1, label = "GARCH forecast on SARIMA residuals failed.", color = "black") + theme_bw()
                  }
                })
                output$forecastResults <- renderPrint({ if (!is.null(garch_fcst)) { cat("GARCH on SARIMA Residuals - Variance Forecast:\n"); print(sigma(garch_fcst)) } else "GARCH forecast on SARIMA residuals failed." })
              }
            } else {
              output$garchSummary <- renderPrint("SARIMA residuals do not show significant heteroscedasticity based on ARCH LM test.")
            }
          }
        }
      } else {
        output$arimaSummary <- renderPrint("Time series does not appear stationary or seasonal enough for automatic ARIMA/SARIMA.")
        output$forecastPlot <- renderPlotly(ggplot() + annotate("text", x = 1, y = 1, label = "No ARIMA/SARIMA model fitted.", color = "black") + theme_bw())
        output$forecastResults <- renderPrint("No ARIMA/SARIMA model fitted.")
        output$shapiroTest <- renderPrint(NULL)
        output$residualPlot <- renderPlot(NULL)
        output$qqPlot <- renderPlot(NULL)
        output$ppPlot <- renderPlot(NULL)
        output$residualAcfPlot <- renderPlotly(NULL)
        output$dwTest <- renderPrint(NULL)
        output$residualMean <- renderPrint(NULL)
        output$bpTest <- renderPrint(NULL)
        output$archLMTest <- renderPrint(NULL)
      }
    }
  })
}

shinyApp(ui, server)

