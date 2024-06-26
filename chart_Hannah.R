library(dplyr)
library(ggplot2)

df <- read.csv('icecream_data.csv')

classify_ice_cream <- function(ingredients) {
  dairy_keywords <- c('milk', 'cream', 'butter')
  if (any(sapply(dairy_keywords, grepl, ingredients, ignore.case = TRUE))) {
    return('dairy')
  } else {
    return('dairy-free')
  }
}

df$category <- sapply(df$description, classify_ice_cream)

ggplot(df, aes(x = category, y = rating, fill = category)) +
  geom_boxplot() +
  scale_fill_manual(values = c("dairy" = "#fca9c9", "dairy-free" = "#b0f7f1")) +
  labs(title = 'Distribution of Ratings for Dairy and Dairy-Free Ice Creams',
       x = 'Category', y = 'Rating') +
  theme_minimal()
