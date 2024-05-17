library(dplyr)
library(ggplot2)

# Load dataset
df <- read.csv('icecream_data.csv')

# Example function to determine dairy vs dairy-free
classify_ice_cream <- function(ingredients) {
  dairy_keywords <- c('milk', 'cream', 'butter')
  if (any(sapply(dairy_keywords, grepl, ingredients, ignore.case = TRUE))) {
    return('dairy')
  } else {
    return('dairy-free')
  }
}

# Apply the classification to the dataset
df$category <- sapply(df$ingredients, classify_ice_cream)

# Group by brand and category, then calculate the average rating
grouped <- df %>%
  group_by(brand, category) %>%
  summarize(average_rating = mean(rating, na.rm = TRUE))

# Convert the summary to a data frame
grouped <- as.data.frame(grouped)

# Plotting
ggplot(rating_summary, aes(x = RatingCount, y = AverageRating, color = Category)) +
  geom_point() +
  labs(title = "Rating Count vs Average Rating for Dairy and Dairy-Free Ice Creams",
       x = "Rating Count",
       y = "Average Rating") +
  theme_minimal()
