fit_with_weights <- function(df, outcome_var, weight_col, grid_n = 100) {
  # Add the weight column dynamically
  df <- df %>% mutate(W = .data[[weight_col]])
  
  # Dynamically build formula: outcome ~ CNT * ESCS
  form <- as.formula(paste(outcome_var, "~ CNT * ESCS"))
  
  # Fit weighted model
  model <- lm(form, data = df, weights = W)
  
  # Country-specific slope of ESCS
  trends <- as.data.frame(emtrends(model, ~ CNT, var = "ESCS"))
  
  # Prediction grid
  grid <- df %>%
    group_by(CNT) %>%
    summarise(
      ESCS_min = min(ESCS, na.rm = TRUE),
      ESCS_max = max(ESCS, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    do(data.frame(
      CNT = .$CNT,
      ESCS = seq(.$ESCS_min, .$ESCS_max, length.out = grid_n)
    )) %>%
    ungroup()
  
  # Predicted values
  grid$pred <- predict(model, newdata = grid)
  
  # Return all components
  list(
    outcome = outcome_var,
    weight_col = weight_col,
    model = model,
    emtrends = trends,
    predictions = grid
  )
}
