library(dplyr)
library(emmeans)
library(survey)
library(ggplot2)
library(factoextra)

#setwd("~/Dropbox/PostDoc/Pisa")

#df2022 = read.csv("data/df2022_filtered.csv")
df2022 = read.csv("data/df2022.csv")

df2022 <- df2022 %>%
  select(-starts_with("FL"), -starts_with("WB"))

names(df2022)
df2022$CNT = as.factor(df2022$CNT)
unique(df2022$CNT)

contrasts(df2022$CNT) <- contr.sum
df2022$CNT <- factor(df2022$CNT)


df2022$MATH = (df2022$PV1MATH + df2022$PV2MATH + df2022$PV3MATH + df2022$PV4MATH + df2022$PV5MATH
   + df2022$PV6MATH + df2022$PV7MATH + df2022$PV8MATH + df2022$PV9MATH + df2022$PV10MATH)/10

rep_weights <- paste0("W_FSTURWT", 1:80)
pisa_design <- svrepdesign(
  weights = ~W_FSTUWT,
  repweights = df2022[, rep_weights],
  data = df2022,
  type = "BRR",
  fay.r = 0.5,   # Fay's adjustment
  mse = TRUE
)

form <- MATH ~ CNT * ESCS 
svy_model <- svyglm(form, design = pisa_design)
coefs <- coef(svy_model)
ci <- confint(svy_model)

result = data.frame(
  term = names(coefs),
  estimate = coefs,
  lower = ci[,1],
  upper = ci[,2]
)

slopes_df <- emtrends(svy_model, ~ CNT, var = "ESCS") %>% as.data.frame()
intercepts_df <- emmeans(svy_model, ~ CNT, at = list(ESCS = 0)) %>% as.data.frame()



coef_df <- slopes_df %>%
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

grid_n <- 100
pred_grid <- coef_df %>%
  rowwise() %>%
  do(data.frame(
    CNT = .$CNT,
    ESCS = seq(min(df2022$ESCS, na.rm = TRUE), max(df2022$ESCS, na.rm = TRUE), length.out = grid_n),
    slope = .$slope,
    intercept = .$intercept,
    slope_SE = .$slope_SE,
    intercept_SE = .$intercept_SE
  )) %>%
  ungroup() %>%
  mutate(
    MATH_pred = intercept + slope * ESCS
  )

pred_grid <- pred_grid %>%
  mutate(
    lower = MATH_pred - 1.96 * sqrt(intercept_SE^2 + (ESCS^2) * slope_SE^2),
    upper = MATH_pred + 1.96 * sqrt(intercept_SE^2 + (ESCS^2) * slope_SE^2)
  )



cluster_data <- coef_df %>%
  select(intercept, slope) %>%
  scale()  # standardize so slope and intercept are on comparable scales

# Keep country names for labeling
country_names <- coef_df$CNT
dist_matrix <- dist(cluster_data)
hc <- hclust(dist_matrix, method = "ward.D2")

optimal_k <- fviz_nbclust(cluster_data, FUN = hcut, method = "silhouette")$data %>%
  filter(y == max(y)) %>%
  pull(clusters)

coef_df$cluster <- cutree(hc, k = optimal_k)


# p1 = ggplot(pred_grid, aes(x = ESCS, y = MATH_pred, color = CNT, fill = CNT)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
#   theme_minimal() +
#   labs(
#     x = "ESCS",
#     y = "Predicted MATH",
#     title = "Predicted MATH vs ESCS by Country",
#     color = "Country",
#     fill = "Country"
#   )

# p2 = ggplot(coef_df, aes(x = intercept, y = slope, label = CNT, color = factor(cluster))) +
#   geom_point(size = 3) +
#   geom_text(nudge_y = -1, size = 3) +
#   geom_errorbar(aes(ymin = slope_lower, ymax = slope_upper), width = 0) +
#   geom_errorbarh(aes(xmin = intercept_lower, xmax = intercept_upper), height = 0) +
#   theme_minimal() +
#   labs(
#     x = "Intercept (ESCS = 0)",
#     y = "Slope (Effect of ESCS on MATH)",
#     color = "Cluster",
#     title = "Country-level intercepts and slopes (Hierarchical Clustering)"
#   )

# inter_plot = plot(intercepts_df)
# slopes_plot = plot(slopes_df)


reslist = list(coef_df = coef_df, pred_grid = pred_grid,
               intercepts_df = intercepts_df, slopes_df = slopes_df)


current_time <- Sys.time()
formatted_time <- format(current_time, "%Y-%m-%d-%H-%M")

name_file = paste0('linear_results','_',
                   formatted_time,'.RData')

save(reslist, file = paste0('Results/',name_file))

