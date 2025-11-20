library(dplyr)
library(emmeans)
library(survey)
library(ggplot2)
library(factoextra)
library(stringr)

#setwd("~/Dropbox/PostDoc/Pisa")
source("functions.R")

df2022 = read.csv("data/df2022_filtered.csv")
#df2018 = read.csv("data/df2018_filtered.csv")
#df2015 = read.csv("data/df2015_filtered.csv")
df2012 = read.csv("data/df2012_filtered.csv")

#df2022 = read.csv("data/df2022.csv")
#df2018 = read.csv("data/df2018.csv")
#df2015 = read.csv("data/df2015.csv")
#df2012 = read.csv("data/df2012.csv")


df2022$CNT = as.factor(df2022$CNT)
#df2018$CNT = as.factor(df2018$CNT)
#df2015$CNT = as.factor(df2015$CNT)
df2012$CNT = as.factor(df2012$CNT)

df2022 = create_averages(df2022)
df2012 = create_averages(df2012)

avg_vars <- grep("^avg_", names(df), value = TRUE)
avg_vars = c("avg_MATH", "avg_READ")

results_2022 <- lapply(avg_vars, function(v) {
  run_full_analysis(v, df2022)
})
names(results_2022) <- avg_vars

results_2012 <- lapply(avg_vars, function(v) {
  run_full_analysis(v, df2012)
})
names(results_2012) <- avg_vars

results_2022$avg_MATH$clutering_plot
results_2012$avg_MATH$clutering_plot


reslist = list(coef_df = coef_df, pred_grid = pred_grid,
               intercepts_df = intercepts_df, slopes_df = slopes_df)


current_time <- Sys.time()
formatted_time <- format(current_time, "%Y-%m-%d-%H-%M")

name_file = paste0('linear_results','_',
                   formatted_time,'.RData')

save(reslist, file = paste0('Results/',name_file))

