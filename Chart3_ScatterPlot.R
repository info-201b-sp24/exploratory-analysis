# The chosen scatter plot effectively illustrates the potential correlation between two critical factors: 
# the popularity and satisfaction level of ice cream flavors. By plotting each flavor as a point, 
# with its position determined by both its rating count and average rating, the plot offers a straightforward
# visual representation. Through this chart, we can quickly discern patterns: whether highly-rated flavors 
# tend to attract more ratings or if there's no clear relationship between popularity and satisfaction.
# Ultimately, the plot provides actionable insights into consumer preferences, aiding manufacturers and 
# retailers in understanding which flavors resonate most with their audience and guiding strategic decisions accordingly.

# Load required libraries
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

# Calculate rating count and average rating
rating_summary <- df %>%
  mutate(Category = ifelse(grepl("dairy-free", tolower(description)), "Dairy-Free", "Dairy")) %>%
  group_by(name, Category) %>%
  summarise(RatingCount = rating_count[1],  # Assuming each flavor has the same rating count
            AverageRating = mean(rating, na.rm = TRUE))

# Ensure that Category is a factor with correct levels
rating_summary$Category <- factor(rating_summary$Category, levels = c("dairy", "dairy-free"))

# Plotting scatter plot showing correlation between rating count and average rating
ggplot(rating_summary, aes(x = RatingCount, y = AverageRating, color = Category)) +
  geom_point() +
  scale_color_manual(values = c("dairy" = "#fca9c9", "dairy-free" = "#b0f7f1")) +
  labs(title = "Correlation between Rating Count and Average Rating for Ice Cream Flavors",
       x = "Rating Count",
       y = "Average Rating")
