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
  titlePanel(tags$h1("Advanced Time Series Analysis Dashboard", style = "color:#FFFFFF;")),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Data Input", style = "color:#FFFFFF;"),
      fileInput("file", "Upload Data File", accept = c(".csv", ".xlsx")),
      selectInput("column", "Select Numeric Column:", choices = NULL),
      numericInput("frequency", "Set Time Series Frequency:", value = 12, min = 1),
      actionButton("set_freq", "Set Frequency", class = "btn-info"), # Added this button
      tags$hr(),
      
      tags$h3("Analysis Parameters", style = "color:#FFFFFF;"),
      sliderInput("lag", "Lags for ACF/PACF:", min = 1, max = 50, value = 20),
      checkboxInput("run_stationarity", "Run Stationarity Tests (ADF, HEGY)"),
      checkboxInput("run_decomposition", "Run Decomposition (Additive & Multiplicative)"),
      checkboxInput("run_detrending", "Run Moving Average Detrending"),
      conditionalPanel(
        condition = "input.run_detrending == true",
        numericInput("ma_order", "Moving Average Order:", value = 3, min = 1)
      ),
      checkboxInput("run_acf_pacf", "Generate ACF/PACF Plots"),
      checkboxInput("run_arima", "Fit ARIMA/SARIMA Model"),
      conditionalPanel(
        condition = "input.run_arima == true",
        sliderInput("p_order", "AR Order (p):", min = 0, max = 5, value = 1),
        sliderInput("d_order", "Differencing Order (d):", min = 0, max = 2, value = 1),
        sliderInput("q_order", "MA Order (q):", min = 0, max = 5, value = 1),
        checkboxInput("seasonal_arima", "Include Seasonal Components (SARIMA)"),
        conditionalPanel(
          condition = "input.seasonal_arima == true",
          sliderInput("P_order", "Seasonal AR Order (P):", min = 0, max = 5, value = 0),
          sliderInput("D_order", "Seasonal Differencing Order (D):", min = 0, max = 2, value = 0),
          sliderInput("Q_order", "Seasonal MA Order (Q):", min = 0, max = 5, value = 0),
          numericInput("seasonal_period", "Seasonal Period (s):", value = 12, min = 1)
        )
      ),
      checkboxInput("run_garch", "Fit ARCH/GARCH Model"),
      conditionalPanel(
        condition = "input.run_garch == true",
        selectInput("garch_model_type", "GARCH Model Type:",
                    choices = c("sGARCH", "eGARCH", "gjrGARCH"), selected = "sGARCH"),
        numericInput("arch_order", "ARCH Order (p):", value = 1, min = 0),
        numericInput("garch_order", "GARCH Order (q):", value = 1, min = 0)
      ),
      checkboxInput("run_forecast", "Generate Forecast"),
      conditionalPanel(
        condition = "input.run_forecast == true",
        numericInput("forecast_horizon", "Forecast Horizon:", value = 10, min = 1),
        radioButtons("forecast_model_select", "Select Model for Forecasting:",
                     choices = c("ARIMA/SARIMA", "GARCH"), selected = "ARIMA/SARIMA")
      ),
      checkboxInput("run_residuals", "Run Residual Diagnostics"),
      checkboxInput("run_periodogram", "Run Periodogram"),
      tags$hr(),
      actionButton("run_analysis", "Run Analysis", class = "btn-primary btn-lg")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series Plot", plotlyOutput("tsPlot")),
        tabPanel("Stationarity Tests", verbatimTextOutput("adfResult"), verbatimTextOutput("hegyResult")),
        tabPanel("Decomposition", plotlyOutput("decompPlot")),
        tabPanel("Moving Averages & Detrending", plotlyOutput("maPlot"), plotlyOutput("detrendedPlot")),
        tabPanel("ACF/PACF", plotlyOutput("acfPlot"), plotlyOutput("pacfPlot")),
        tabPanel("ARIMA/SARIMA Model", verbatimTextOutput("arimaSummary")),
        tabPanel("GARCH Model", verbatimTextOutput("garchSummary")),
        tabPanel("Forecasting",
                 plotlyOutput("forecastPlot"),
                 verbatimTextOutput("forecastResults")
        ),
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
        ),
        tabPanel("Periodogram", plotOutput("periodogramPlot"))
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
  
  observeEvent(input$set_freq, { # Changed to observeEvent(input$set_freq, ...)
    req(input$column)
    if (!is.null(data()[[input$column]])) {
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
  
  observeEvent(input$run_analysis, {
    req(tsData()) # Ensure tsData is available
    
    # --- Stationarity Tests ---
    if (input$run_stationarity) {
      test_result_adf <- adf.test(tsData(), alternative = "stationary")
      output$adfResult <- renderPrint({ test_result_adf })
      if (frequency(tsData()) <= 1) {
        output$hegyResult <- renderPrint("HEGY test requires a time series with a frequency greater than 1.")
      } else {
        hegy_result <- tryCatch(
          hegy.test(tsData()),
          error = function(e) paste("Error running HEGY test:", e$message)
        )
        output$hegyResult <- renderPrint({ hegy_result })
      }
    } else {
      output$adfResult <- renderPrint(NULL)
      output$hegyResult <- renderPrint(NULL)
    }
    
    # --- Decomposition ---
    if (input$run_decomposition) {
      decomp_add <- tryCatch(decompose(tsData(), type = "additive"), error = function(e) NULL)
      output$decompPlot <- renderPlotly({ if(!is.null(decomp_add)) ggplotly(autoplot(decomp_add) + theme_bw()) else NULL })
      if (any(tsData() <= 0)) {
        output$decompPlot <- renderPrint("Multiplicative decomposition requires strictly positive values.")
      } else {
        decomp_mult <- tryCatch(decompose(tsData(), type = "multiplicative"), error = function(e) NULL)
        output$decompPlot <- renderPlotly({ if(!is.null(decomp_mult)) ggplotly(autoplot(decomp_mult) + theme_bw()) else NULL })
      }
    } else {
      output$decompPlot <- renderPlotly(NULL)
    }
    
    # --- Moving Average & Detrending ---
    if (input$run_detrending) {
      ma_vals <- SMA(tsData(), n = input$ma_order)
      detrended_series <- tsData() - ma_vals
      
      output$maPlot <- renderPlotly({
        ggplotly(
          ggplot() +
            geom_line(aes(x = time(tsData()), y = tsData()), color = "black") + # Changed line color to black for visibility on white
            geom_line(aes(x = time(tsData()), y = ma_vals), color = "red") +
            theme_bw()
        )
      })
      
      output$detrendedPlot <- renderPlotly({
        ggplotly(
          ggplot() +
            geom_line(aes(x = time(detrended_series), y = detrended_series), color = "green") +
            theme_bw()
        )
      })
    } else {
      output$maPlot <- renderPlotly(NULL)
      output$detrendedPlot <- renderPlotly(NULL)
    }
    
    # --- ACF/PACF ---
    if (input$run_acf_pacf) {
      output$acfPlot <- renderPlotly({
        ggplotly(autoplot(Acf(tsData(), lag.max = input$lag)) + theme_bw())
      })
      output$pacfPlot <- renderPlotly({
        ggplotly(autoplot(Pacf(tsData(), lag.max = input$lag)) + theme_bw())
      })
    } else {
      output$acfPlot <- renderPlotly(NULL)
      output$pacfPlot <- renderPlotly(NULL)
    }
    
    # --- ARIMA/SARIMA Modeling ---
    if (input$run_arima) {
      p <- input$p_order
      d <- input$d_order
      q <- input$q_order
      
      if (input$seasonal_arima) {
        P <- input$P_order
        D <- input$D_order
        Q <- input$Q_order
        s <- input$seasonal_period
        model <- tryCatch(
          arima(tsData(), order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = s)),
          error = function(e) paste("Error fitting SARIMA model:", e$message)
        )
      } else {
        model <- tryCatch(
          arima(tsData(), order = c(p, d, q)),
          error = function(e) paste("Error fitting ARIMA model:", e$message)
        )
      }
      arimaModel(model)
      output$arimaSummary <- renderPrint({ summary(arimaModel()) })
    } else {
      arimaModel(NULL)
      output$arimaSummary <- renderPrint(NULL)
    }
    
    # --- ARCH/GARCH Modeling ---
    if (input$run_garch) {
      if (length(tsData()) < 2) {
        output$garchSummary <- renderPrint("Not enough data to fit GARCH model.")
        garchModelFit(NULL)
      } else {
        garch_spec <- ugarchspec(
          variance.model = list(model = input$garch_model_type, garchOrder = c(input$garch_order, input$arch_order)),
          mean.model = list(armaOrder = c(0, 0)),
          distribution.model = "norm"
        )
        
        garch_fit <- tryCatch(
          ugarchfit(spec = garch_spec, data = tsData()),
          error = function(e) paste("Error fitting GARCH model:", e$message)
        )
        garchModelFit(garch_fit)
        output$garchSummary <- renderPrint({
          if (is.character(garch_fit)) {
            garch_fit
          } else {
            show(garch_fit)
          }
        })
      }
    } else {
      garchModelFit(NULL)
      output$garchSummary <- renderPrint(NULL)
    }
    
    # --- Forecasting ---
    if (input$run_forecast) {
      horizon <- input$forecast_horizon
      
      if (input$forecast_model_select == "ARIMA/SARIMA" && !is.null(arimaModel())) {
        model <- arimaModel()
        if (!is.character(model)) {
          fcast <- tryCatch(
            forecast(model, h= horizon),
            error = function(e) paste("Error forecasting with ARIMA/SARIMA:", e$message)
          )
          forecast_results(fcast)
        } else {
          forecast_results("ARIMA/SARIMA model not fitted successfully for forecast.")
        }
      } else if (input$forecast_model_select == "GARCH" && !is.null(garchModelFit())) {
        garch_fit <- garchModelFit()
        if (!is.character(garch_fit)) {
          n.ahead <- horizon
          garch_fcst <- tryCatch(
            ugarchforecast(garch_fit, n.ahead = n.ahead),
            error = function(e) paste("Error forecasting with GARCH:", e$message)
          )
          forecast_results(garch_fcst)
        } else {
          forecast_results("GARCH model not fitted successfully for forecast.")
        }
      } else {
        forecast_results("Please fit an ARIMA/SARIMA or GARCH model first for forecasting.")
      }
      
      output$forecastPlot <- renderPlotly({
        fcast <- forecast_results()
        req(fcast)
        plot_obj <- NULL
        if (inherits(fcast, "forecast")) {
          plot_obj <- autoplot(fcast) + theme_bw()
        } else if (inherits(fcast, "uGARCHforecast")) {
          forc <- sigma(fcast)
          forecasting <- data.frame(
            Time = index(forc),
            Variance = as.numeric(forc)
          )
          plot_obj <- ggplot(forecasting, aes(x = Time, y = Variance)) +
            geom_line(color = "black") + # Changed line color to black for visibility on white
            labs(title = "GARCH Conditional Variance Forecast", x = "Time", y = "Conditional Variance") +
            theme_bw()
        } else {
          plot_obj <- ggplot() + annotate("text", x = 1, y = 1, label = as.character(fcast), color = "black") + # Changed text color to black
            theme_bw()
        }
        if (!is.null(plot_obj)) {
          ggplotly(plot_obj)
        } else {
          ggplotly(ggplot() + theme_bw())
        }
      })
      
      output$forecastResults <- renderPrint({
        fcast <- forecast_results()
        req(fcast)
        if (inherits(fcast, "forecast")) {
          print(fcast)
        } else if (inherits(fcast, "uGARCHforecast")) {
          cat("GARCH Forecast (Conditional Variance):\n")
          print(sigma(fcast))
        } else {
          print(fcast)
        }
      })
    } else {
      forecast_results(NULL)
      output$forecastPlot <- renderPlotly(NULL)
      output$forecastResults <- renderPrint(NULL)
    }
    
    # --- Residual Diagnostics ---
    if (input$run_residuals) {
      arima_res <- if (!is.null(arimaModel()) && !is.character(arimaModel())) residuals(arimaModel()) else NULL
      garch_res <- if (!is.null(garchModelFit()) && !is.character(garchModelFit())) residuals(garchModelFit()) else NULL
      final_residuals <- if (!is.null(arima_res)) arima_res else garch_res
      
      if (!is.null(final_residuals)) {
        output$shapiroTest <- renderPrint({ shapiro.test(final_residuals) })
        output$residualPlot <- renderPlot({
          plot(final_residuals, type = "l", main = "Residual Plot", bg = "white", fg = "black", col.main = "black", col.lab = "black", col.axis = "black", col = "black")
        })
        output$qqPlot <- renderPlot({
          qqnorm(final_residuals, bg = "white", fg = "black", col.main = "black", col.lab = "black", col.axis = "black", col = "black")
          qqline(final_residuals, col = "red")
        })
        output$ppPlot <- renderPlot({
          ppoints <- pnorm(final_residuals, mean = mean(final_residuals), sd = sd(final_residuals))
          plot(sort(ppoints), sort(final_residuals), main = "PP Plot", xlab = "Theoretical Percentiles", ylab = "Sample Percentiles", bg = "white", fg = "black", col.main = "black", col.lab = "black", col.axis = "black", col = "black")
          abline(0, 1, col = "red")
        })
        output$residualAcfPlot <- renderPlotly({
          ggplotly(autoplot(Acf(final_residuals)) + theme_bw())
        })
        output$dwTest <- renderPrint({ dwtest(final_residuals ~ 1) })
        output$residualMean <- renderPrint({ paste("Mean of residuals:", mean(final_residuals)) })
        output$bpTest <- renderPrint({ Box.test(final_residuals, lag = 10, type = "Ljung-Box") })
        output$archLMTest <- renderPrint({
          if (!is.null(arima_res)) lmtest::bgtest(arima_res ~ 1, order = 10)
          else if (!is.null(garch_res)) lmtest::bgtest(garch_res^2 ~ 1, order = 10)
          else "No valid model for ARCH LM Test."
        })
      } else {
        output$shapiroTest <- renderPrint("No valid model fitted for residual analysis.")
        output$residualPlot <- renderPlot(NULL)
        output$qqPlot <- renderPlot(NULL)
        output$ppPlot <- renderPlot(NULL)
        output$residualAcfPlot <- renderPlotly(NULL)
        output$dwTest <- renderPrint(NULL)
        output$residualMean <- renderPrint(NULL)
        output$bpTest <- renderPrint(NULL)
        output$archLMTest <- renderPrint(NULL)
      }
    } else {
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
    
    # --- Periodogram ---
    if (input$run_periodogram) {
      periodogram_data <- spectrum(tsData(), plot = FALSE)
      output$periodogramPlot <- renderPlot({
        plot(periodogram_data$freq, periodogram_data$spec,
             type = "h", xlab = "Frequency", ylab = "Spectral Density",
             main = "Periodogram", bg = "white", fg = "black", col.main = "black", col.lab = "black", col.axis = "black", col = "black")
      })
    } else {
      output$periodogramPlot <- renderPlot(NULL)
    }
  })
}

shinyApp(ui, server)