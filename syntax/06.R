# Neural network that tests whether shootings predict motor vehicle theft (MVT) over neighborhood effects

# don't run 
# install packages
# install.packages(
  # c("torch", 
  # "sf",
  # "sfdep",
  # "ggplot2",
  # "scales"
  # )
  # )

# aggregate point-level data to neighbourhood-level of analysis ----------------

# don't run
# annual variations in MVTs provides visual evidence for training data (2022-24) and test data (2025) split
# print(fig03)

# skeleton for tract-level data
skeleton <- tidyr::crossing(GeoUID = tracts$GeoUID, year = as.character(2022:2025))

# aggregate shootings and fatal shootings
agg_shoot <- joined %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::filter(year <= 2025) %>%
  dplyr::group_by(GeoUID, year) %>%
  dplyr::summarise(
    shootings = sum(shootings, na.rm = TRUE),
    fatal = sum(homicides, na.rm = TRUE),
    .groups = "drop"
     )

# aggregate motor vehicle thefts
agg_auto <- autotheft %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(year >= 2022) %>%
  dplyr::filter(year <= 2025) %>%
  dplyr::group_by(GeoUID, year) %>%
  dplyr::summarise( # split into residential vs. non-residential premises, plus overall total
    mvt_total          = sum(mvt, na.rm = TRUE),
    mvt_residential    = sum(mvt[premises == "Residential"], na.rm = TRUE),
    mvt_nonresidential = sum(mvt[premises == "Non-residential"], na.rm = TRUE),
    .groups = "drop"
    )

# join together
crime <- skeleton %>%
  dplyr::left_join(agg_shoot, by = c("GeoUID", "year")) %>%
  dplyr::left_join(agg_auto,  by = c("GeoUID", "year")) %>%
  dplyr::mutate(
    dplyr::across(c(
      shootings, 
      fatal, 
      mvt_total, 
      mvt_residential, 
      mvt_nonresidential
      ), 
      ~ tidyr::replace_na(., 0)),
    non_fatal = shootings - fatal
    )

# neighbourhood effects
neighbourhoodFx <- df_tracts %>%
  dplyr::select(
    GeoUID, 
    immigration, 
    housing_precarity, 
    housing_tenure, 
    residential_mobility, 
    pop,
    pop_15_34,
    Pd
    )

# join together
crime <- crime %>% dplyr::left_join(neighbourhoodFx, by = "GeoUID")

# calculate crime rates per 10,000 population
crime <- crime %>%
  dplyr::mutate(
    shootings = (shootings / pop) * 10000,
    fatal = (fatal / pop) * 10000,
    non_fatal = (non_fatal / pop) * 10000,
    mvt_total = (mvt_total / pop) * 10000,
    mvt_residential = (mvt_residential / pop) * 10000,
    mvt_nonresidential = (mvt_nonresidential / pop) * 10000
    ) %>%
  dplyr::mutate(
    dplyr::across(
      c(shootings, fatal, non_fatal, mvt_total, mvt_residential, mvt_nonresidential),
      ~ tidyr::replace_na(., 0)
      )
    )



# compute spatial weights ------------------------------------------------------

# tract-level geoids
nbhd_geom <- tracts %>% 
  dplyr::distinct(GeoUID, .keep_all = TRUE) %>% 
  dplyr::select(GeoUID, geometry)

# compute spatial lags
crime_lag <- crime %>%
  dplyr::group_by(year) %>%
  dplyr::group_modify(~ {
    yr_geom <- nbhd_geom %>% dplyr::filter(GeoUID %in% .x$GeoUID)
    yr_data <- .x[match(yr_geom$GeoUID, .x$GeoUID), ]
    
    nb_yr <- spdep::poly2nb(yr_geom, queen = TRUE)
    lw_yr <- spdep::nb2listw(nb_yr, style = "W", zero.policy = TRUE)
    
    yr_data$mvt_tot_lag    <- spdep::lag.listw(lw_yr, yr_data$mvt_total, zero.policy = TRUE)
    yr_data$mvt_res_lag    <- spdep::lag.listw(lw_yr, yr_data$mvt_residential, zero.policy = TRUE)
    yr_data$mvt_nonres_lag <- spdep::lag.listw(lw_yr, yr_data$mvt_nonresidential, zero.policy = TRUE)
    yr_data$shootings_lag  <- spdep::lag.listw(lw_yr, yr_data$shootings, zero.policy = TRUE)
    yr_data$fatal_lag      <- spdep::lag.listw(lw_yr, yr_data$fatal, zero.policy = TRUE)
    yr_data$non_fatal_lag  <- spdep::lag.listw(lw_yr, yr_data$non_fatal, zero.policy = TRUE)
    
    yr_data
    }
  ) %>%
  dplyr::ungroup()



# log predictors
crime_lag <- crime_lag %>%
  dplyr::mutate( # log1p(x) = log(x + 1), avoids log(0)
    mvt_total_log       = log1p(mvt_total),
    mvt_tot_lag_log     = log1p(mvt_tot_lag),
    mvt_res_log         = log1p(mvt_residential), 
    mvt_res_lag_log     = log1p(mvt_res_lag), 
    mvt_nonres_log      = log1p(mvt_nonresidential), 
    mvt_nonres_lag_log  = log1p(mvt_nonres_lag), 
    shootings_log       = log1p(shootings),
    fatal_log           = log1p(fatal),
    non_fatal_log       = log1p(non_fatal),
    shootings_lag_log   = log1p(shootings_lag),
    fatal_lag_log       = log1p(fatal_lag),
    non_fatal_lag_log   = log1p(non_fatal_lag),
    Pd_log              = log1p(Pd)
    )

# vector of predictors
predictors <- c(
  "mvt_nonres_lag_log",
  "fatal_log",
  "non_fatal_log",
  "fatal_lag_log",
  "non_fatal_lag_log",
  "immigration", 
  "housing_precarity",
  "housing_tenure", 
  "residential_mobility", 
  "Pd_log"
  )

# scale logged predictors
crime_lag[predictors] <- scale(crime_lag[predictors])

# define years for training/test data split
idx <- which(crime_lag$year %in% c(2022, 2023, 2024))

# predictors
x <- as.matrix(crime_lag[predictors])

# training data for the predictors
x_train <- torch::torch_tensor(x[idx, ],  dtype = torch::torch_float())

# test data for the predictors
x_test  <- torch::torch_tensor(x[-idx, ], dtype = torch::torch_float())

# outcome
y <- as.matrix(crime_lag$mvt_nonres_log, ncol = 1)

# training data for the outcome
y_train <- torch::torch_tensor(y[idx, , drop = FALSE],  dtype = torch::torch_float())

# test data for the outcome
y_test  <- torch::torch_tensor(y[-idx, , drop = FALSE], dtype = torch::torch_float())



# set seed for replication
set.seed(1234)

# formulate and train a simple feedforward model
model <- torch::nn_sequential(
  torch::nn_linear(ncol(x), 16), torch::nn_relu(),
  torch::nn_linear(16, 8),      torch::nn_relu(),
  torch::nn_linear(8, 1)
  )
optimizer <- torch::optim_adam(model$parameters, lr = 0.01)

# the number of times that the machine learning model sees the entire training dataset
n_epochs <- 100
for (epoch in 1:n_epochs) {
  optimizer$zero_grad()
  pred <- model(x_train)
  loss <- torch::nnf_mse_loss(pred, y_train)
  loss$backward()
  optimizer$step()
  
  if (epoch %% 25 == 0) {
    cat("Epoch", epoch, "- Train MSE:", loss$item(), "\n")
  }
}



# evaluate the model on the test data
test_pred <- model(x_test)

# loss function that calculates the average of the squared differences between the predicted and actual values 
test_loss <- torch::nnf_mse_loss(test_pred, y_test)
cat("\nFinal Test MSE:", test_loss$item(), "\n")

# mean squared error
baseline_mse <- mean((crime_lag$mvt_total[-idx] - mean(crime_lag$mvt_total[idx]))^2)
cat("Baseline (mean-only) MSE:", baseline_mse, "\n")

# calculate pseudo R-squared
r2_pseudo <- 1 - (test_loss$item() / baseline_mse)
cat("Pseudo R-squared (test):", r2_pseudo, "\n")



# predicted log values 
pred_hat <- model(torch::torch_tensor(x, dtype = torch::torch_float()))

# attach to dataset
crime_lag$predicted_mvt_nonres_log <- as.numeric(torch::as_array(pred_hat))

# exponentiate to turn back to crime rates
crime_lag$predicted_mvt_nonresidential <- expm1(crime_lag$predicted_mvt_nonres_log)

# residual (error)
crime_lag$residual <- crime_lag$predicted_mvt_nonresidential - crime_lag$mvt_nonresidential

# calculate residual on log scale
crime_lag$residual_log <- crime_lag$predicted_mvt_nonres_log - log1p(crime_lag$mvt_nonresidential)


# manually calculate mean squared error for the non transformed data
test_idx <- if (exists("idx")) -idx else NULL 
mse_test <- mean((crime_lag$predicted_mvt_nonresidential[test_idx] - crime_lag$mvt_nonresidential[test_idx])^2, na.rm = TRUE)
cat("Mean squared error for test data:", mse_test, "\n")



# first, plot the correlation between the predicted and actual values ----------------------------

# fit linear model for the actual and predicted values
fit <- stats::lm(mvt_nonresidential ~ predicted_mvt_nonresidential, data = crime_lag[-idx, ])
b <- stats::coef(fit)[2]
R2 <- summary(fit)$r.squared

# labels
label <- paste0("Slope = ", round(b, 2), "\nR\u00b2 = ", round(R2, 2))

# plot the predicted values against the actual values
fig05 <- ggplot2::ggplot(crime_lag[-idx, ], ggplot2::aes(x = predicted_mvt_nonresidential, y = mvt_nonresidential)) +
  ggplot2::geom_point(alpha = 0.4, color = "darkred") +
  ggplot2::geom_smooth(method = "lm", color = "blue", se = TRUE) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::annotate(
    "text",
    x = 50, 
    y = 85,
    label = label, 
    hjust = 0, 
    vjust = 1, 
    size = 4
    ) +
  ggplot2::coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
  ggplot2::labs(x = "Predicted non-residential MVTs per 10,000 population",
       y = "Actual non-residential MVTs per 10,000 population",
       title = "Actual vs. Predicted Non-Residential MVT per 10,000 population, 2025)") +
  ggplot2::theme_classic()
print(fig05)

# output high resolution figure
output(
  filename = "fig05.png", 
  figure = fig05, 
  path = "~/Desktop/TPS/", 
  width = 10, 
  height = 10
  )


# plot the actual and predicted values as spatial maps, and plot the residuals to vizualize prediction error

# first, prepare the data for plotting
pred_sf <- tracts %>%
  dplyr::distinct(GeoUID, .keep_all = TRUE) %>%
  dplyr::select(GeoUID, geometry) %>%
  dplyr::left_join(
    crime_lag %>%
      dplyr::filter(year == "2025") %>%
      dplyr::select(GeoUID, year, mvt_nonresidential, predicted_mvt_nonresidential, residual),
    by = "GeoUID"
    )

# plot the actual MVTs per 10,000 population
fig06 <- pred_sf %>%
  ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes(fill = mvt_nonresidential)) +
  ggplot2::scale_fill_gradient(low = "white", high = "darkred", na.value = "grey90") +
  ggplot2::labs(title = paste("Actual MVTs per 10,000 population,", "2025"), fill = "MVT") +
  ggplot2::theme_minimal()
print(fig06)

  # output high resolution figure
  output(
    filename = "fig06.png", 
    figure = fig06, 
    path = "~/Desktop/TPS/", 
    width = 10, 
    height = 10
    )
 
# plot the predicted MVTs per 10,000 population
fig07 <- pred_sf %>%
  ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes(fill = predicted_mvt_nonresidential)) +
  ggplot2::scale_fill_gradient(low = "white", high = "darkred", na.value = "grey90") +
  ggplot2::labs(title = paste("Predicted MVTs per 10,000 population,", "2025"), fill = "Predicted") +
  ggplot2::theme_minimal()
print(fig07)

  # output high resolution figure
  output(
    filename = "fig07.png", 
    figure = fig07, 
    path = "~/Desktop/TPS/", 
    width = 10, 
    height = 10
    )

# plot the residuals (errors)
fig08 <- pred_sf %>%
  ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes(fill = residual)) +
  ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "grey90") +
  ggplot2::labs(title = paste("Prediction error in the actual MVTs per 10,000 population,", "2025"), fill = "Residual") +
  ggplot2::theme_minimal()
print(fig08)

  # output high resolution figure
  output(
    filename = "fig08.png", 
    figure = fig08, 
    path = "~/Desktop/TPS/", 
    width = 10, 
    height = 10
    )





# end .R script



