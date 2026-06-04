# U.S. Autosales Time-Series Forecasting

### Basic Information

* **Members:** Dana Ortiz, [dana.ortiz@gwu.edu](mailto:dana.ortiz@gwu.edu)
* **Date:** May 2026
* **Model Version:** 1.17
* **License:** MIT
* **Model Implementation Code:** [Autosales Forecasting](Autosales%20Forecasting.R)

### Intended Use

* **Intended Uses:** This project is an educational example of time-series forecasting for monthly U.S. auto sales. The analysis compares deterministic trend models, cyclical trend models, exponential smoothing models, ARIMA models, and regression-based time-series models using economic predictors. The use case mirrors business forecasting for understanding sales patterns, seasonality, trend changes, and forecast performance.
* **Out-of-Scope Use Cases:** Any real-world automotive production planning, financial forecasting, investment decisions, supply-chain planning, dealership inventory decisions, or economic policy decisions. This project is strictly for educational demonstrations and should not be used as the sole basis for operational or financial decisions.

### Training Data

* **Data Dictionary:**

| Name              | Modeling Role    | Measurement Level | Description                                                                |
| ----------------- | ---------------- | ----------------- | -------------------------------------------------------------------------- |
| **Month**         | input/time index | int               | Month number used to identify seasonal patterns                            |
| **Autosales**     | target           | numeric           | Monthly U.S. auto sales, used as the main time-series variable to forecast |
| **Inflation**     | input/predictor  | numeric           | Inflation measure used as an external predictor in regression-based models |
| **Unemploy**      | input/predictor  | numeric           | Unemployment measure used as an external predictor                         |
| **PPI**           | input/predictor  | numeric           | Producer Price Index measure used as an external predictor                 |
| **GasPrices**     | input/predictor  | numeric           | Gas price measure used as an external predictor                            |
| **trend**         | engineered input | int               | Time index used to model long-term trend                                   |
| **dummy1**        | engineered input | binary            | Indicator for one structural trend period in the switching-slope model     |
| **dummy2**        | engineered input | binary            | Indicator for a later structural trend period in the switching-slope model |
| **int1**          | engineered input | numeric           | Interaction between `dummy1` and the time index                            |
| **int2**          | engineered input | numeric           | Interaction between `dummy2` and the time index                            |
| **m2–m12**        | engineered input | binary            | Monthly seasonal dummy variables, using January as the reference month     |
| **Fourier terms** | engineered input | numeric           | Sine and cosine terms used in the cyclical trend model                     |

* **Source of Training Data:** `US Autosales.csv`, containing monthly U.S. auto sales and related economic predictor variables.
* **How training data was divided into training and validation data:** The full dataset contains 312 monthly observations. The first 288 observations were used as the training sample, and the final 24 observations were reserved as a holdout sample for forecast evaluation.
* **Number of rows in training and validation data:**

  * Original dataset rows: 312
  * Training rows: 288
  * Holdout rows: 24
  * Time frequency: Monthly

### Test Data

* **Source of test data:** The test data came from the final 24-month holdout portion of `US Autosales.csv`.
* **Number of rows in test data:** 24
* **State any differences in columns between training and test data:** There were no differences in columns between the training and holdout data. Both contain the same original variables. The holdout period was used only for out-of-sample forecast evaluation.

### Model Details

* **Columns used as inputs in the final model:** `trend`, `dummy1`, `dummy2`, `int1`, `int2`, `m2`, `m3`, `m4`, `m5`, `m6`, `m7`, `m8`, `m9`, `m10`, `m11`, `m12`, `Inflation`, `Unemploy`, `PPI`, `GasPrices`
* **Column(s) used as target(s) in the final model:** `Autosales`
* **Type of model:** Time-series forecasting model comparison, including deterministic trend models, exponential smoothing, ARIMA, corrected deterministic models, and regression models with ARIMA errors.
* **Software used to implement the model:** R, RStudio, `forecast`, `TSA`
* **Version of the modeling software:** R version 4.5.3
* **Hyperparameters or other settings of your model:**

```r
n_hold <- 24
n_train <- n - n_hold
```

Deterministic switching-slope trend model:

```r
y ~ trend + dummy1 + dummy2 + int1 + int2 +
  m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12
```

Cyclical trend model:

```r
forecast::fourier(AS, K = K)
```

Holt-Winters exponential smoothing models:

```r
hw(AS, seasonal = "additive", h = n_hold)
hw(AS, seasonal = "multiplicative", h = n_hold)
```

ARIMA target model:

```r
Arima(
  AS,
  order = c(0, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)
```

Corrected deterministic model:

```r
Arima(
  AS,
  xreg = det_xreg_train,
  order = c(1, 0, 1)
)
```

Regression model with economic predictors:

```r
y ~ trend + dummy1 + dummy2 + int1 + int2 +
  m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
  Inflation + Unemploy + PPI + GasPrices
```

Corrected regression model:

```r
Arima(
  AS,
  xreg = reg_xreg_train,
  order = c(1, 0, 1)
)
```

### Quantitative Analysis

* **Metrics Used to Evaluate:** MAPE, RMSE, MAE, residual ACF/PACF plots, and Ljung-Box tests for residual autocorrelation.

| Model Class                       | Models Compared                                                                | Evaluation Approach                                                              |
| --------------------------------- | ------------------------------------------------------------------------------ | -------------------------------------------------------------------------------- |
| **Deterministic models**          | Switching-slope trend with seasonal dummies; cyclical trend with Fourier terms | Training and holdout MAPE, RMSE, MAE; residual diagnostics                       |
| **Exponential smoothing models**  | Holt-Winters additive; Holt-Winters multiplicative                             | Training and holdout MAPE, RMSE, MAE; residual diagnostics                       |
| **Stochastic models**             | Seasonal ARIMA target model                                                    | Training and holdout MAPE, RMSE, MAE; residual ACF and Ljung-Box test            |
| **Corrected deterministic model** | Deterministic model with ARIMA errors                                          | Training and holdout MAPE, RMSE, MAE; residual diagnostics                       |
| **Regression models**             | Regression with economic predictors; corrected regression with ARIMA errors    | Predictor relationship analysis, residual analysis, training and holdout metrics |

The script prints comparison tables for Section 2, Section 3, and the overall model comparison. The main evaluation compares both in-sample fit and out-of-sample holdout performance, which is important because a model can fit the training data well but perform worse on future observations.

### Ethical Considerations

* **Potential negative impacts of using this model:**

  * *Math or Software Problems:* Time-series forecasts are sensitive to the selected training window, structural breaks, seasonal assumptions, and model specification. A model that performs well on historical data may not generalize if economic conditions change. The regression models also depend on the availability and accuracy of external predictors such as inflation, unemployment, PPI, and gas prices.
  * *Real World Risks:* If this type of model were used in a real automotive context, inaccurate forecasts could contribute to overproduction, underproduction, inventory shortages, excess inventory costs, or poor financial planning. Forecasts may also be misinterpreted as certainty rather than estimates with uncertainty.

* **Uncertainties relating to the impacts of using the model:**

  * *Math or Software Uncertainties:* The project compares multiple model classes, but each model is still based on assumptions about trend, seasonality, autocorrelation, and predictor relationships. Structural changes in the auto market may reduce the reliability of historical patterns.
  * *Real World Uncertainties:* Auto sales can be affected by many factors not included in the dataset, such as interest rates, consumer confidence, supply-chain disruptions, vehicle prices, policy changes, credit availability, and major economic shocks.

* **Unexpected Results:** Some models may fit the training data closely while performing less well on the 24-month holdout period. This is expected in time-series forecasting because complex models can overfit historical patterns. Residual diagnostics are important because leftover autocorrelation suggests that a model has not fully captured the structure of the series.

### AI Use Disclosure

AI tools were used for README drafting and organization of project documentation. All final code, outputs, model comparisons, and conclusions should be reviewed by the project author before submission.
