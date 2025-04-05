# Time Series Analysis Shiny Apps

[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository contains two R Shiny web applications for performing time series analysis. Developed by **Brinda**, **Anupama**, **Aryan**, **Chandrima**, and **Anjali**.

## Overview

This project provides two distinct Shiny applications catering to different levels of user expertise in time series analysis:

1.  **Automated Time Series Analysis Dashboard:** An application designed for quick and comprehensive EDA of time series data with minimal user intervention.
2.  **Advanced Time Series Analysis Dashboard:** An application offering granular control over various time series analysis techniques, allowing users to customize parameters and select specific analyses.

## App 1: Automated Time Series Analysis Dashboard

### Features

* **Automated EDA:** Upload data, set frequency, and run a complete analysis with a single click.
* **Stationarity Tests:** Performs Augmented Dickey-Fuller (ADF) and HEGY tests.
* **Seasonality Detection:** Explores seasonality through decomposition and HEGY test.
* **Decomposition:** Additive and multiplicative decomposition of the time series.
* **Detrending:** Moving average detrending.
* **ACF/PACF:** Autocorrelation and Partial Autocorrelation function plots.
* **Periodogram:** Spectral density estimation.
* **ARIMA/SARIMA Modeling:** Automatic fitting of ARIMA or SARIMA models.
* **GARCH Modeling:** Automatic fitting of GARCH (1,1) model if heteroscedasticity is detected in residuals.
* **Forecasting:** Automated forecasting based on the fitted ARIMA/SARIMA model.
* **Residual Diagnostics:** Shapiro-Wilk test for normality, residual plot, QQ-plot, PP-plot, ACF of residuals, Durbin-Watson test, mean of residuals, Box-Pierce and Ljung-Box tests, and ARCH LM test.

### Usage

1.  Clone this repository.
2.  Open the `app1.R` file in R or RStudio.
3.  Run the application using `shiny::runApp('app1.R')`.
4.  Upload your time series data in `.csv` or `.xlsx` format.
5.  Select the numeric column containing your time series.
6.  Set the frequency of your time series data.
7.  Click the "Run Automated Analysis" button.
8.  Explore the results in the various tabs.

## App 2: Advanced Time Series Analysis Dashboard

### Features

* **Interactive Analysis Selection:** Users can choose which analysis components to run.
* **Parameter Control:** Adjust parameters for ACF/PACF lags, moving average order, and ARIMA/SARIMA/GARCH model orders.
* **Stationarity Tests:** Augmented Dickey-Fuller (ADF) and HEGY tests (optional).
* **Decomposition:** Additive and multiplicative decomposition (optional).
* **Detrending:** Moving average detrending with customizable order (optional).
* **ACF/PACF:** Autocorrelation and Partial Autocorrelation function plots with adjustable lags (optional).
* **ARIMA/SARIMA Modeling:** Manual specification of ARIMA (p, d, q) and SARIMA (P, D, Q, s) orders (optional).
* **GARCH Modeling:** Selection of GARCH model type (sGARCH, eGARCH, gjrGARCH) and manual specification of ARCH (p) and GARCH (q) orders (optional).
* **Forecasting:** Generate forecasts using either the fitted ARIMA/SARIMA or GARCH model with a user-defined horizon (optional).
* **Residual Diagnostics:** Comprehensive residual analysis for fitted models (optional).
* **Periodogram:** Spectral density estimation (optional).

### Usage

1.  Clone this repository.
2.  Open the `app2.R` file in R or RStudio.
3.  Run the application using `shiny::runApp('app2.R')`.
4.  Upload your time series data in `.csv` or `.xlsx` format.
5.  Select the numeric column containing your time series.
6.  Set the frequency of your time series data and click "Set Frequency".
7.  Check the boxes next to the analyses you want to perform and adjust the parameters as needed.
8.  Click the "Run Analysis" button.
9.  View the results in the corresponding tabs.

## Quick Start Guide

Follow these steps to get started with the Time Series Analysis Shiny Apps:

**Prerequisites:**

* Make sure you have **R** (version 4.0 or higher) and **RStudio** installed on your system.
* Ensure you have installed all the necessary R packages. You can do this by running the following command in your R console:
    ```R
    install.packages(c("shiny", "shinythemes", "ggplot2", "forecast", "tseries", "readxl", "plotly", "lmtest", "nortest", "TTR", "tools", "uroot", "quantmod", "rugarch", "shinyWidgets"))
    ```

**Running the Applications:**

**For the Automated Time Series Analysis Dashboard:**

1.  Clone this repository to your local machine.
2.  Open the `app1.R` file in RStudio.
3.  Click the "Run App" button in the top right corner of the script editor (or use the command `shiny::runApp('app1.R')` in the R console).
4.  A web browser window will open with the application.
5.  In the "Data Input" section, click "Browse" to upload your time series data file (supported formats: `.csv` or `.xlsx`).
6.  Select the column containing your time series data from the "Select Numeric Column" dropdown.
7.  Enter the frequency of your time series data in the "Set Time Series Frequency" field (e.g., 12 for monthly data, 252 for daily stock market data).
8.  Click the "Set Frequency" button.
9.  Specify the "Forecast Horizon" if you wish to generate forecasts.
10. Click the "Run Automated Analysis" button.
11. Navigate through the different tabs to explore the results of the automated analysis, including plots, test outputs, and model summaries.

**For the Advanced Time Series Analysis Dashboard:**

1.  Clone this repository to your local machine.
2.  Open the `app2.R` file in RStudio.
3.  Click the "Run App" button (or use `shiny::runApp('app2.R')`).
4.  A web browser window will open with the application.
5.  In the "Data Input" section, upload your data file and select the numeric column and frequency as described above.
6.  Click the "Set Frequency" button.
7.  In the "Analysis Parameters" section, check the boxes next to the specific time series analyses you want to perform.
8.  Adjust the parameters for each selected analysis (e.g., lags for ACF/PACF, orders for ARIMA/GARCH models).
9.  If you want to generate a forecast, check the "Generate Forecast" box and set the horizon and the model to use.
10. If you want to analyze residuals, check the "Run Residual Diagnostics" box.
11. Click the "Run Analysis" button.
12. View the results of each selected analysis in the corresponding tabs.

**Exploring the Apps:**

* Each tab in the main panel displays the results of a specific analysis (e.g., Time Series Plot, Stationarity Tests, Decomposition, etc.).
* Hover over plots (especially those generated with `plotly`) for interactive tooltips.
* Review the text outputs for the results of statistical tests and model summaries.

We encourage you to explore the different options and parameters in both dashboards to gain a deeper understanding of your time series data.

## Future Work / Roadmap

We plan to continue enhancing these Time Series Analysis Shiny Apps with new features and improvements. Here are some of the areas we are considering for future development:

**For Both Applications:**

* **Enhanced Data Input:**
    * Allow users to paste data directly into a text input area.
    * Explore options for connecting to online data sources for real-time or historical data retrieval (e.g., financial APIs).
* **Improved Visualization:**
    * Provide more options for customizing plot appearance, such as titles, axis labels, colors, and themes.
    * Implement functionality to download generated plots in various formats (PNG, JPEG, PDF).
* **Session Management:**
    * Enable users to save their current analysis session (data, parameters, results) and load it later.
* **Basic Reporting:**
    * Add the ability to generate a simple report summarizing the analysis performed.
* **Interactive Plots:**
    * Enhance plot interactivity with zoom, pan, and more detailed tooltips.
* **Accessibility:**
    * Work on improving the accessibility of the applications for users with disabilities.

**For the Automated Time Series Analysis Dashboard:**

* **Smarter Automatic Modeling:**
    * Implement more robust automatic model selection by comparing multiple ARIMA models and potentially other model types (like ETS) based on information criteria.
    * Explore automated detection of different seasonal patterns.
    * Consider automatic outlier detection and handling.
* **Explanations of Results:**
    * Provide brief, user-friendly explanations of the statistical test outputs and what they mean in the context of time series analysis.

**For the Advanced Time Series Analysis Dashboard:**

* **More Model Choices:**
    * Include additional time series models such as Exponential Smoothing (ETS), state-space models, and potentially dynamic regression.
    * Offer more advanced options for GARCH model specification and distribution assumptions.
* **Model Comparison:**
    * Allow users to fit and compare multiple models based on various performance metrics.
* **Customizable Diagnostics:**
    * Give users more control over which residual diagnostic tests and plots are generated.
* **Scenario Forecasting:**
    * Enable users to perform scenario analysis for forecasting by adjusting model parameters.
* **Backtesting:**
    * Implement basic backtesting functionality to evaluate model performance on historical data.

We welcome feedback and suggestions from the community as we continue to develop these tools.

## Installation

To run these applications, you need to have R and the following R packages installed:

```R
install.packages(c("shiny", "shinythemes", "ggplot2", "forecast", "tseries", "readxl", "plotly", "lmtest", "nortest", "TTR", "tools", "uroot", "quantmod", "rugarch", "shinyWidgets"))
