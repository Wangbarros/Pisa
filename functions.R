create_averages = function(df){
  suffixes <- df %>%
    select(matches("^PV\\d+")) %>%
    names() %>%
    str_replace("^PV\\d+", "") %>%
    unique()
  
  for (s in suffixes) {
    df[[paste0("avg_", s)]] <- df %>%
      select(matches(paste0("^PV\\d+", s, "$"))) %>%
      rowMeans(na.rm = TRUE)
  }
  
  return(df)
}

run_full_analysis <- function(outcome, df, grid_n = 100) {
  
  if (any(grepl("W_FSTURWT", names(df)))) {
    weight_string = "W_FSTURWT"
  }else{
    weight_string = "W_FSTR"
  }
  
  rep_weights <- paste0(weight_string, 1:80)
  pisa_design <- svrepdesign(
    weights = ~W_FSTUWT,
    repweights = df[, rep_weights],
    data = df,
    type = "BRR",
    fay.r = 0.5,   # Fay's adjustment
    mse = TRUE
  )
  
  
  # Build formula
  form <- as.formula(paste0(outcome, " ~ CNT * ESCS"))
  
  # Fit model
  m <- svyglm(form, design = pisa_design)
  
  # Confidence intervals
  ci <- confint(m)
  coefs <- coef(m)
  coef_table <- data.frame(
    term = names(coefs),
    estimate = coefs,
    lower = ci[,1],
    upper = ci[,2]
  )
  
  # Slopes
  slopes_df <- emtrends(m, ~ CNT, var = "ESCS", data = df) %>% as.data.frame()
  
  # Intercepts
  intercepts_df <- emmeans(m, ~ CNT, at = list(ESCS = 0), data = df) %>% as.data.frame()
  
  # Combine slope + intercept summaries
  slope_intercept_table <- slopes_df %>%
    select(CNT, ESCS.trend, SE, lower.CL, upper.CL) %>%
    rename(
      slope = ESCS.trend,
      slope_SE = SE,
      slope_lower = lower.CL,
      slope_upper = upper.CL
    ) %>%
    left_join(
      intercepts_df %>%
        select(CNT, emmean, SE, lower.CL, upper.CL) %>%
        rename(
          intercept = emmean,
          intercept_SE = SE,
          intercept_lower = lower.CL,
          intercept_upper = upper.CL
        ),
      by = "CNT"
    )
  
  # Prediction grid
  pred_grid <- slope_intercept_table %>%
    rowwise() %>%
    do(data.frame(
      CNT = .$CNT,
      ESCS = seq(min(df$ESCS, na.rm = TRUE),
                 max(df$ESCS, na.rm = TRUE),
                 length.out = grid_n),
      slope = .$slope,
      intercept = .$intercept,
      slope_SE = .$slope_SE,
      intercept_SE = .$intercept_SE
    )) %>%
    ungroup() %>%
    mutate(
      pred = intercept + slope * ESCS,
      lower = pred - 1.96 * sqrt(intercept_SE^2 + ESCS^2 * slope_SE^2),
      upper = pred + 1.96 * sqrt(intercept_SE^2 + ESCS^2 * slope_SE^2)
    )
    
  cluster_data <- slope_intercept_table %>%
    select(intercept, slope) %>%
    scale()  # standardize so slope and intercept are on comparable scales
  
  # Keep country names for labeling
  country_names <- slope_intercept_table$CNT
  dist_matrix <- dist(cluster_data)
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  optimal_k <- fviz_nbclust(cluster_data, FUN = hcut)$data %>%
    filter(y == max(y)) %>%
    pull(clusters)
  
  slope_intercept_table$cluster <- cutree(hc, k = optimal_k)
  
  p2 = ggplot(slope_intercept_table, aes(x = intercept, y = slope, label = CNT, color = factor(cluster))) +
    geom_point(size = 3) +
    geom_text(nudge_y = -1, size = 3) +
    geom_errorbar(aes(ymin = slope_lower, ymax = slope_upper), width = 0) +
    geom_errorbarh(aes(xmin = intercept_lower, xmax = intercept_upper), height = 0) +
    theme_minimal() +
    labs(
      x = "Intercept (ESCS = 0)",
      y = "Slope (Effect of ESCS on MATH)",
      color = "Cluster",
      title = "Country-level intercepts and slopes (Hierarchical Clustering)"
    )
  
  inter_plot = plot(intercepts_df)
  slopes_plot = plot(slopes_df)
  
  
  p2
  
  list(
    form = form,
    coef_table = coef_table,
    slope_intercept_table = slope_intercept_table,
    prediction_grid = pred_grid,
    clutering_plot = p2,
    inter_plot = inter_plot,
    slopes_plot = slopes_plot
  )
}
