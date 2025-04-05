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

## Installation

To run these applications, you need to have R and the following R packages installed:

```R
install.packages(c("shiny", "shinythemes", "ggplot2", "forecast", "tseries", "readxl", "plotly", "lmtest", "nortest", "TTR", "tools", "uroot", "quantmod", "rugarch", "shinyWidgets"))
