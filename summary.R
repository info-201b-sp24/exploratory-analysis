library(dplyr)

icecream_data <- read.csv('icecream_data.csv')

mean_rating <- round(mean(icecream_data$rating, na.rm = T), 2)
median_rating <- median(icecream_data$rating, na.rm = T)
mean_rating_count <- round(mean(icecream_data$rating_count, na.rm = T), 2)
median_rating_count <- median(icecream_data$rating_count, na.rm = T)
# rounding the means as they both returned very long decimal values
total_icecreams <- nrow(icecream_data)

summary_stats <- list(
  total_icecreams = total_icecreams,
  mean_rating = mean_rating,
  median_rating = median_rating,
  mean_rating_count = mean_rating_count,
  median_rating_count = median_rating_count
)