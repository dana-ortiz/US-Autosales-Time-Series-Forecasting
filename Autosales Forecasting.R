library(forecast)
library(TSA)

options(width = 200)
graphics.off()

data <- read.csv("C:/Users/danao/Downloads/US Autosales.csv", header = TRUE)

AUTOSALES <- ts(data$Autosales, start = c(1995, 1), frequency = 12)
INFLATION <- ts(data$Inflation, start = c(1995, 1), frequency = 12)
UNEMPLOY  <- ts(data$Unemploy, start = c(1995, 1), frequency = 12)
PPI       <- ts(data$PPI, start = c(1995, 1), frequency = 12)
GAS       <- ts(data$GasPrices, start = c(1995, 1), frequency = 12)

n <- length(AUTOSALES)
n_hold <- 24
n_train <- n - n_hold

AS <- window(AUTOSALES, end = time(AUTOSALES)[n_train])
AS_hold <- AUTOSALES[(n_train + 1):n]

holdout_start <- time(AUTOSALES)[n_train + 1]

cat("Total number of observations:", n, "\n")
cat("Training sample size:", n_train, "\n")
cat("Hold-out sample size:", n_hold, "\n")
cat("Frequency:", frequency(AUTOSALES), "\n")

reset_plot <- function(){
  par(mfrow = c(1, 1))
  par(mar = c(5, 4, 4, 2) + 0.1)
}

MAPE <- function(actual, predicted){
  mean(abs(actual - predicted) / abs(actual)) * 100
}

RMSE <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}

MAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}

get_first_col <- function(x){
  if(is.matrix(x) || is.data.frame(x)){
    as.numeric(x[, 1])
  } else {
    as.numeric(x)
  }
}

plot_acf_fixed <- function(x, figure_title){
  reset_plot()
  acf_obj <- acf(
    x,
    lag.max = 36,
    plot = FALSE,
    na.action = na.pass
  )
  plot(
    acf_obj,
    ylim = c(-1, 1),
    col = "blue",
    lwd = 2,
    xlab = "Lag",
    main = figure_title
  )
}

plot_pacf_fixed <- function(x, figure_title){
  reset_plot()
  pacf_obj <- pacf(
    x,
    lag.max = 36,
    plot = FALSE,
    na.action = na.pass
  )
  plot(
    pacf_obj,
    ylim = c(-1, 1),
    col = "blue",
    lwd = 2,
    xlab = "Lag",
    main = figure_title
  )
}

one_step_lm_forecasts <- function(df, initial_train, model_formula){
  h <- nrow(df) - initial_train
  preds <- rep(NA, h)
  
  for(i in 1:h){
    fit_i <- lm(model_formula, data = df[1:(initial_train + i - 1), ])
    preds[i] <- predict(fit_i, newdata = df[initial_train + i, , drop = FALSE])
  }
  
  return(preds)
}

one_step_hw_forecasts <- function(series, initial_train, seasonal_type){
  h <- length(series) - initial_train
  preds <- rep(NA, h)
  
  for(i in 1:h){
    temp_series <- ts(
      series[1:(initial_train + i - 1)],
      start = start(series),
      frequency = frequency(series)
    )
    
    fit_i <- hw(temp_series, seasonal = seasonal_type, h = 1)
    preds[i] <- as.numeric(fit_i$mean[1])
  }
  
  return(preds)
}

one_step_arima_target_forecasts <- function(series, initial_train){
  h <- length(series) - initial_train
  preds <- rep(NA, h)
  
  for(i in 1:h){
    temp_series <- ts(
      series[1:(initial_train + i - 1)],
      start = start(series),
      frequency = frequency(series)
    )
    
    fit_i <- Arima(
      temp_series,
      order = c(0, 1, 1),
      seasonal = list(order = c(0, 1, 1), period = 12)
    )
    
    preds[i] <- forecast(fit_i, h = 1)$mean[1]
  }
  
  return(preds)
}

one_step_arimax_forecasts <- function(y, xreg_full, initial_train, order_used){
  h <- length(y) - initial_train
  preds <- rep(NA, h)
  
  for(i in 1:h){
    temp_y <- ts(
      y[1:(initial_train + i - 1)],
      start = start(y),
      frequency = frequency(y)
    )
    
    temp_xreg <- xreg_full[1:(initial_train + i - 1), , drop = FALSE]
    new_xreg <- xreg_full[(initial_train + i), , drop = FALSE]
    
    fit_i <- Arima(
      temp_y,
      xreg = temp_xreg,
      order = order_used
    )
    
    preds[i] <- forecast(fit_i, xreg = new_xreg, h = 1)$mean[1]
  }
  
  return(preds)
}

Month <- cycle(AUTOSALES)
time_index <- 1:n

dummy1 <- ifelse(time_index > 168 & time_index <= 240, 1, 0)
dummy2 <- ifelse(time_index >= 241, 1, 0)

int1 <- dummy1 * time_index
int2 <- dummy2 * time_index

m2  <- as.integer(Month == 2)
m3  <- as.integer(Month == 3)
m4  <- as.integer(Month == 4)
m5  <- as.integer(Month == 5)
m6  <- as.integer(Month == 6)
m7  <- as.integer(Month == 7)
m8  <- as.integer(Month == 8)
m9  <- as.integer(Month == 9)
m10 <- as.integer(Month == 10)
m11 <- as.integer(Month == 11)
m12 <- as.integer(Month == 12)

full_df <- data.frame(
  y = as.numeric(AUTOSALES),
  trend = time_index,
  dummy1 = dummy1,
  dummy2 = dummy2,
  int1 = int1,
  int2 = int2,
  m2 = m2,
  m3 = m3,
  m4 = m4,
  m5 = m5,
  m6 = m6,
  m7 = m7,
  m8 = m8,
  m9 = m9,
  m10 = m10,
  m11 = m11,
  m12 = m12,
  Inflation = as.numeric(INFLATION),
  Unemploy = as.numeric(UNEMPLOY),
  PPI = as.numeric(PPI),
  GasPrices = as.numeric(GAS)
)

train_df <- full_df[1:n_train, ]
hold_df <- full_df[(n_train + 1):n, ]

reset_plot()
ts.plot(
  AUTOSALES,
  col = "blue",
  lwd = 2,
  ylab = "Autosales",
  xlab = "Time",
  main = "Figure 1. Monthly U.S. Auto Sales"
)

reset_plot()
boxplot(
  as.numeric(AUTOSALES) ~ factor(Month, levels = 1:12, labels = month.abb),
  col = "lightblue",
  xlab = "Month",
  ylab = "Autosales",
  main = "Figure 2. Seasonal Boxplots of Monthly U.S. Auto Sales"
)

reset_plot()
TSA::periodogram(
  AS,
  main = "Figure 3. Periodogram of the Training Series"
)

pgram_obj <- spec.pgram(
  AS,
  detrend = TRUE,
  log = "no",
  plot = FALSE
)

dominant_periods_table <- data.frame(
  Frequency = pgram_obj$freq,
  Spectrum = pgram_obj$spec,
  Period_in_Months = 1 / pgram_obj$freq
)

dominant_periods_table <- dominant_periods_table[is.finite(dominant_periods_table$Period_in_Months), ]
dominant_periods_table <- dominant_periods_table[dominant_periods_table$Period_in_Months > 1, ]
dominant_periods_table <- dominant_periods_table[order(-dominant_periods_table$Spectrum), ]
dominant_periods_table <- head(dominant_periods_table, 4)

dominant_periods_table$Frequency <- round(dominant_periods_table$Frequency, 4)
dominant_periods_table$Spectrum <- round(dominant_periods_table$Spectrum, 4)
dominant_periods_table$Period_in_Months <- round(dominant_periods_table$Period_in_Months, 2)

print(dominant_periods_table, row.names = FALSE)

plot_acf_fixed(
  as.numeric(AS),
  "Figure 4. ACF of the Training Series"
)

switch_formula <- y ~ trend + dummy1 + dummy2 + int1 + int2 +
  m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12

fit_switch <- lm(switch_formula, data = train_df)

summary(fit_switch)

pred_switch_train <- as.numeric(fitted(fit_switch))
residual_switch <- as.numeric(residuals(fit_switch))
lb_switch <- Box.test(residual_switch, lag = 24, type = "Ljung-Box")

pred_switch_hold <- one_step_lm_forecasts(
  df = full_df,
  initial_train = n_train,
  model_formula = switch_formula
)

all_pred_switch <- ts(
  c(pred_switch_train, pred_switch_hold),
  start = start(AUTOSALES),
  frequency = frequency(AUTOSALES)
)

reset_plot()
ts.plot(
  AUTOSALES,
  col = "blue",
  lwd = 2,
  ylab = "Autosales",
  xlab = "Time",
  main = "Figure 5. Actual versus Fitted/One-Step-Ahead Forecast: Switching-Slope Trend + Seasonal Dummies"
)
lines(all_pred_switch, col = "red", lwd = 2)
abline(v = holdout_start, col = "darkgray", lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("Actual", "Fitted/one-step-ahead forecast", "Hold-out start"),
  col = c("blue", "red", "darkgray"),
  lwd = c(2, 2, 2),
  lty = c(1, 1, 2),
  bty = "n"
)

period_val <- frequency(AS)
max_K <- floor(period_val / 2)
K_candidates <- 1:min(6, max_K)

aic_vec <- rep(0, length(K_candidates))
fits_cycle <- vector("list", length(K_candidates))

for(j in 1:length(K_candidates)){
  K <- K_candidates[j]
  
  xreg_cycle_train <- as.data.frame(forecast::fourier(AS, K = K))
  names(xreg_cycle_train) <- paste0("F", 1:ncol(xreg_cycle_train))
  
  cycle_df_train <- data.frame(
    y = as.numeric(AS),
    trend = 1:n_train,
    xreg_cycle_train
  )
  
  fits_cycle[[j]] <- lm(y ~ ., data = cycle_df_train)
  aic_vec[j] <- AIC(fits_cycle[[j]])
}

k_table_cycle <- data.frame(
  K = K_candidates,
  AIC = round(aic_vec, 3)
)

print(k_table_cycle, row.names = FALSE)

K_best <- K_candidates[which.min(aic_vec)]
fit_cycle <- fits_cycle[[which.min(aic_vec)]]

summary(fit_cycle)

pred_cycle_train <- as.numeric(fitted(fit_cycle))
residual_cycle <- as.numeric(residuals(fit_cycle))
lb_cycle <- Box.test(residual_cycle, lag = 24, type = "Ljung-Box")

xreg_cycle_future <- as.data.frame(forecast::fourier(AS, K = K_best, h = n_hold))
names(xreg_cycle_future) <- paste0("F", 1:ncol(xreg_cycle_future))

cycle_df_future <- data.frame(
  trend = (n_train + 1):n,
  xreg_cycle_future
)

pred_cycle_hold <- as.numeric(predict(fit_cycle, newdata = cycle_df_future))

all_pred_cycle <- ts(
  c(pred_cycle_train, pred_cycle_hold),
  start = start(AUTOSALES),
  frequency = frequency(AUTOSALES)
)

reset_plot()
ts.plot(
  AUTOSALES,
  col = "blue",
  lwd = 2,
  ylab = "Autosales",
  xlab = "Time",
  main = sprintf("Figure 6. Actual versus Fitted/Forecast: Cyclical Trend (K = %d)", K_best)
)
lines(all_pred_cycle, col = "red", lwd = 2)
abline(v = holdout_start, col = "darkgray", lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("Actual", "Fitted/Forecast", "Hold-out start"),
  col = c("blue", "red", "darkgray"),
  lwd = c(2, 2, 2),
  lty = c(1, 1, 2),
  bty = "n"
)

reset_plot()
ts.plot(
  residual_switch,
  col = "blue",
  lwd = 2,
  ylab = "Residual",
  xlab = "Time",
  main = "Figure 7. Residual Plot: Switching-Slope Trend + Seasonal Dummies"
)
abline(h = 0, col = "red", lwd = 2)

plot_acf_fixed(
  residual_switch,
  "Figure 8. Residual ACF: Switching-Slope Trend + Seasonal Dummies"
)

coef_switch <- coef(fit_switch)

trend_component <- coef_switch["(Intercept)"] +
  coef_switch["trend"] * time_index +
  coef_switch["dummy1"] * dummy1 +
  coef_switch["dummy2"] * dummy2 +
  coef_switch["int1"] * int1 +
  coef_switch["int2"] * int2

trend_component_ts <- ts(
  trend_component,
  start = start(AUTOSALES),
  frequency = frequency(AUTOSALES)
)

reset_plot()
ts.plot(
  AUTOSALES,
  col = "blue",
  lwd = 2,
  ylab = "Autosales",
  xlab = "Time",
  main = "Figure 9. Switching-Slope Trend Component Over the Full Series"
)
lines(trend_component_ts, col = "red", lwd = 2)
abline(v = holdout_start, col = "darkgray", lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("Actual", "Estimated switching trend", "Hold-out start"),
  col = c("blue", "red", "darkgray"),
  lwd = c(2, 2, 2),
  lty = c(1, 1, 2),
  bty = "n"
)

hw_add <- hw(AS, seasonal = "additive", h = n_hold)

pred_hw_add_train <- get_first_col(hw_add$fitted)
residual_hw_add <- get_first_col(hw_add$residuals)
lb_hw_add <- Box.test(residual_hw_add, lag = 24, type = "Ljung-Box")

pred_hw_add_hold <- one_step_hw_forecasts(
  series = AUTOSALES,
  initial_train = n_train,
  seasonal_type = "additive"
)

all_pred_hw_add <- ts(
  c(pred_hw_add_train, pred_hw_add_hold),
  start = start(AUTOSALES),
  frequency = frequency(AUTOSALES)
)

reset_plot()
ts.plot(
  AUTOSALES,
  col = "blue",
  lwd = 2,
  ylab = "Autosales",
  xlab = "Time",
  main = "Figure 10. Actual versus Fitted/One-Step-Ahead Forecast: Holt-Winters Additive"
)
lines(all_pred_hw_add, col = "red", lwd = 2)
abline(v = holdout_start, col = "darkgray", lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("Actual", "Fitted/one-step-ahead forecast", "Hold-out start"),
  col = c("blue", "red", "darkgray"),
  lwd = c(2, 2, 2),
  lty = c(1, 1, 2),
  bty = "n"
)

plot_acf_fixed(
  residual_hw_add,
  "Figure 11. Residual ACF: Holt-Winters Additive"
)

hw_mul <- hw(AS, seasonal = "multiplicative", h = n_hold)

pred_hw_mul_train <- get_first_col(hw_mul$fitted)
residual_hw_mul <- get_first_col(hw_mul$residuals)
lb_hw_mul <- Box.test(residual_hw_mul, lag = 24, type = "Ljung-Box")

pred_hw_mul_hold <- one_step_hw_forecasts(
  series = AUTOSALES,
  initial_train = n_train,
  seasonal_type = "multiplicative"
)

all_pred_hw_mul <- ts(
  c(pred_hw_mul_train, pred_hw_mul_hold),
  start = start(AUTOSALES),
  frequency = frequency(AUTOSALES)
)

reset_plot()
ts.plot(
  AUTOSALES,
  col = "blue",
  lwd = 2,
  ylab = "Autosales",
  xlab = "Time",
  main = "Figure 12. Actual versus Fitted/One-Step-Ahead Forecast: Holt-Winters Multiplicative"
)
lines(all_pred_hw_mul, col = "red", lwd = 2)
abline(v = holdout_start, col = "darkgray", lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("Actual", "Fitted/one-step-ahead forecast", "Hold-out start"),
  col = c("blue", "red", "darkgray"),
  lwd = c(2, 2, 2),
  lty = c(1, 1, 2),
  bty = "n"
)

plot_acf_fixed(
  residual_hw_mul,
  "Figure 13. Residual ACF: Holt-Winters Multiplicative"
)

diagnostics_table <- data.frame(
  Model = c(
    "Switching-Slope Trend + Seasonal Dummies",
    "Cyclical Trend",
    "Holt-Winters Additive",
    "Holt-Winters Multiplicative"
  ),
  Ljung_Box_Statistic = round(c(
    as.numeric(lb_switch$statistic),
    as.numeric(lb_cycle$statistic),
    as.numeric(lb_hw_add$statistic),
    as.numeric(lb_hw_mul$statistic)
  ), 4),
  p_value = signif(c(
    lb_switch$p.value,
    lb_cycle$p.value,
    lb_hw_add$p.value,
    lb_hw_mul$p.value
  ), 4)
)

print(diagnostics_table, row.names = FALSE)

comparison_table_section2 <- data.frame(
  Model = c(
    "Switching-Slope Trend + Seasonal Dummies",
    "Cyclical Trend",
    "Holt-Winters Additive",
    "Holt-Winters Multiplicative"
  ),
  Training_MAPE = c(
    MAPE(as.numeric(AS), pred_switch_train),
    MAPE(as.numeric(AS), pred_cycle_train),
    MAPE(as.numeric(AS), pred_hw_add_train),
    MAPE(as.numeric(AS), pred_hw_mul_train)
  ),
  Training_RMSE = c(
    RMSE(as.numeric(AS), pred_switch_train),
    RMSE(as.numeric(AS), pred_cycle_train),
    RMSE(as.numeric(AS), pred_hw_add_train),
    RMSE(as.numeric(AS), pred_hw_mul_train)
  ),
  Training_MAE = c(
    MAE(as.numeric(AS), pred_switch_train),
    MAE(as.numeric(AS), pred_cycle_train),
    MAE(as.numeric(AS), pred_hw_add_train),
    MAE(as.numeric(AS), pred_hw_mul_train)
  ),
  Holdout_MAPE = c(
    MAPE(as.numeric(AS_hold), pred_switch_hold),
    MAPE(as.numeric(AS_hold), pred_cycle_hold),
    MAPE(as.numeric(AS_hold), pred_hw_add_hold),
    MAPE(as.numeric(AS_hold), pred_hw_mul_hold)
  ),
  Holdout_RMSE = c(
    RMSE(as.numeric(AS_hold), pred_switch_hold),
    RMSE(as.numeric(AS_hold), pred_cycle_hold),
    RMSE(as.numeric(AS_hold), pred_hw_add_hold),
    RMSE(as.numeric(AS_hold), pred_hw_mul_hold)
  ),
  Holdout_MAE = c(
    MAE(as.numeric(AS_hold), pred_switch_hold),
    MAE(as.numeric(AS_hold), pred_cycle_hold),
    MAE(as.numeric(AS_hold), pred_hw_add_hold),
    MAE(as.numeric(AS_hold), pred_hw_mul_hold)
  )
)

comparison_table_section2[, 2:7] <- round(comparison_table_section2[, 2:7], 4)

print(comparison_table_section2, row.names = FALSE)

plot_acf_fixed(
  AS,
  "Figure 14. ACF of the Original Training Series"
)

plot_pacf_fixed(
  AS,
  "Figure 15. PACF of the Original Training Series"
)

d_y <- ndiffs(AS)
D_y <- nsdiffs(AS)

cat("Recommended nonseasonal differences:", d_y, "\n")
cat("Recommended seasonal differences:", D_y, "\n")

diff_y <- diff(AS, differences = d_y)

if(D_y > 0){
  diff_y <- diff(diff_y, lag = 12, differences = D_y)
}

reset_plot()
ts.plot(
  diff_y,
  col = "blue",
  lwd = 2,
  ylab = "Differenced Autosales",
  xlab = "Time",
  main = "Figure 16. Differenced Training Series for ARIMA Identification"
)
abline(h = 0, col = "red", lwd = 2)

plot_acf_fixed(
  diff_y,
  "Figure 17. ACF of the Differenced Training Series"
)

plot_pacf_fixed(
  diff_y,
  "Figure 18. PACF of the Differenced Training Series"
)

fit_arima <- Arima(
  AS,
  order = c(0, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

summary(fit_arima)

residual_arima <- residuals(fit_arima)

plot_acf_fixed(
  residual_arima,
  "Figure 19. Residual ACF for the ARIMA Target Model"
)

pred_arima_train <- as.numeric(fitted(fit_arima))

pred_arima_hold <- one_step_arima_target_forecasts(
  series = AUTOSALES,
  initial_train = n_train
)

lb_arima <- Box.test(residual_arima, lag = 24, type = "Ljung-Box")

det_xreg_full <- as.matrix(full_df[, c(
  "trend", "dummy1", "dummy2", "int1", "int2",
  "m2", "m3", "m4", "m5", "m6", "m7", "m8",
  "m9", "m10", "m11", "m12"
)])

det_xreg_train <- det_xreg_full[1:n_train, , drop = FALSE]

fit_corrected_det <- Arima(
  AS,
  xreg = det_xreg_train,
  order = c(1, 0, 1)
)

summary(fit_corrected_det)

pred_corrected_det_train <- as.numeric(fitted(fit_corrected_det))
residual_corrected_det <- residuals(fit_corrected_det)

pred_corrected_det_hold <- one_step_arimax_forecasts(
  y = AUTOSALES,
  xreg_full = det_xreg_full,
  initial_train = n_train,
  order_used = c(1, 0, 1)
)

lb_corrected_det <- Box.test(residual_corrected_det, lag = 24, type = "Ljung-Box")

plot_acf_fixed(
  residual_corrected_det,
  "Figure 20. Residual ACF for the Corrected Deterministic Model"
)

cor_table <- data.frame(
  Predictor = c("Inflation", "Unemployment", "PPI", "Gas Prices"),
  Correlation_with_Autosales = round(c(
    cor(as.numeric(AUTOSALES), as.numeric(INFLATION), use = "complete.obs"),
    cor(as.numeric(AUTOSALES), as.numeric(UNEMPLOY), use = "complete.obs"),
    cor(as.numeric(AUTOSALES), as.numeric(PPI), use = "complete.obs"),
    cor(as.numeric(AUTOSALES), as.numeric(GAS), use = "complete.obs")
  ), 4)
)

print(cor_table, row.names = FALSE)

reset_plot()
pairs(
  full_df[, c("y", "Inflation", "Unemploy", "PPI", "GasPrices")],
  main = "Figure 21. Target Variable and Predictor Relationships"
)
reset_plot()

reg_formula <- y ~ trend + dummy1 + dummy2 + int1 + int2 +
  m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
  Inflation + Unemploy + PPI + GasPrices

fit_regression <- lm(reg_formula, data = train_df)

summary(fit_regression)

pred_reg_train <- as.numeric(fitted(fit_regression))
residual_reg <- as.numeric(residuals(fit_regression))

pred_reg_hold <- one_step_lm_forecasts(
  df = full_df,
  initial_train = n_train,
  model_formula = reg_formula
)

lb_reg <- Box.test(residual_reg, lag = 24, type = "Ljung-Box")

plot_acf_fixed(
  residual_reg,
  "Figure 22. Residual ACF for the Regression Model"
)

reg_xreg_full <- as.matrix(full_df[, c(
  "trend", "dummy1", "dummy2", "int1", "int2",
  "m2", "m3", "m4", "m5", "m6", "m7", "m8",
  "m9", "m10", "m11", "m12",
  "Inflation", "Unemploy", "PPI", "GasPrices"
)])

reg_xreg_train <- reg_xreg_full[1:n_train, , drop = FALSE]

fit_corrected_reg <- Arima(
  AS,
  xreg = reg_xreg_train,
  order = c(1, 0, 1)
)

summary(fit_corrected_reg)

pred_corrected_reg_train <- as.numeric(fitted(fit_corrected_reg))
residual_corrected_reg <- residuals(fit_corrected_reg)

pred_corrected_reg_hold <- one_step_arimax_forecasts(
  y = AUTOSALES,
  xreg_full = reg_xreg_full,
  initial_train = n_train,
  order_used = c(1, 0, 1)
)

lb_corrected_reg <- Box.test(residual_corrected_reg, lag = 24, type = "Ljung-Box")

plot_acf_fixed(
  residual_corrected_reg,
  "Figure 23. Residual ACF for the Corrected Regression Model"
)

section3_comparison <- data.frame(
  Model = c(
    "ARIMA Target Model",
    "Deterministic Model",
    "Corrected Deterministic Model",
    "Regression Model",
    "Corrected Regression Model"
  ),
  Training_MAPE = c(
    MAPE(as.numeric(AS), pred_arima_train),
    MAPE(as.numeric(AS), pred_switch_train),
    MAPE(as.numeric(AS), pred_corrected_det_train),
    MAPE(as.numeric(AS), pred_reg_train),
    MAPE(as.numeric(AS), pred_corrected_reg_train)
  ),
  Training_RMSE = c(
    RMSE(as.numeric(AS), pred_arima_train),
    RMSE(as.numeric(AS), pred_switch_train),
    RMSE(as.numeric(AS), pred_corrected_det_train),
    RMSE(as.numeric(AS), pred_reg_train),
    RMSE(as.numeric(AS), pred_corrected_reg_train)
  ),
  Training_MAE = c(
    MAE(as.numeric(AS), pred_arima_train),
    MAE(as.numeric(AS), pred_switch_train),
    MAE(as.numeric(AS), pred_corrected_det_train),
    MAE(as.numeric(AS), pred_reg_train),
    MAE(as.numeric(AS), pred_corrected_reg_train)
  ),
  Holdout_MAPE = c(
    MAPE(as.numeric(AS_hold), pred_arima_hold),
    MAPE(as.numeric(AS_hold), pred_switch_hold),
    MAPE(as.numeric(AS_hold), pred_corrected_det_hold),
    MAPE(as.numeric(AS_hold), pred_reg_hold),
    MAPE(as.numeric(AS_hold), pred_corrected_reg_hold)
  ),
  Holdout_RMSE = c(
    RMSE(as.numeric(AS_hold), pred_arima_hold),
    RMSE(as.numeric(AS_hold), pred_switch_hold),
    RMSE(as.numeric(AS_hold), pred_corrected_det_hold),
    RMSE(as.numeric(AS_hold), pred_reg_hold),
    RMSE(as.numeric(AS_hold), pred_corrected_reg_hold)
  ),
  Holdout_MAE = c(
    MAE(as.numeric(AS_hold), pred_arima_hold),
    MAE(as.numeric(AS_hold), pred_switch_hold),
    MAE(as.numeric(AS_hold), pred_corrected_det_hold),
    MAE(as.numeric(AS_hold), pred_reg_hold),
    MAE(as.numeric(AS_hold), pred_corrected_reg_hold)
  )
)

section3_comparison[, 2:7] <- round(section3_comparison[, 2:7], 4)

print(section3_comparison, row.names = FALSE)

section3_diagnostics <- data.frame(
  Model = c(
    "ARIMA Target Model",
    "Deterministic Model",
    "Corrected Deterministic Model",
    "Regression Model",
    "Corrected Regression Model"
  ),
  Ljung_Box_Statistic = round(c(
    as.numeric(lb_arima$statistic),
    as.numeric(lb_switch$statistic),
    as.numeric(lb_corrected_det$statistic),
    as.numeric(lb_reg$statistic),
    as.numeric(lb_corrected_reg$statistic)
  ), 4),
  p_value = signif(c(
    lb_arima$p.value,
    lb_switch$p.value,
    lb_corrected_det$p.value,
    lb_reg$p.value,
    lb_corrected_reg$p.value
  ), 4)
)

print(section3_diagnostics, row.names = FALSE)

overall_comparison <- rbind(
  comparison_table_section2,
  section3_comparison
)

overall_comparison <- overall_comparison[!duplicated(overall_comparison$Model), ]

overall_comparison[, 2:7] <- round(overall_comparison[, 2:7], 4)

print(overall_comparison, row.names = FALSE)
