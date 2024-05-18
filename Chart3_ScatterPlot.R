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

df$category <- sapply(df$ingredients, classify_ice_cream)

rating_summary <- df %>%
  mutate(Category = ifelse(grepl("dairy-free", tolower(description)), "Dairy-Free", "Dairy")) %>%
  group_by(name, Category) %>%
  summarise(RatingCount = rating_count[1],  # Assuming each flavor has the same rating count
            AverageRating = mean(rating, na.rm = TRUE))

rating_summary$Category <- factor(rating_summary$Category, levels = c("Dairy", "Dairy-Free"))

ggplot(rating_summary, aes(x = RatingCount, y = AverageRating, color = Category)) +
  geom_point() +
  scale_color_manual(values = c("Dairy" = "#fca9c9", "Dairy-Free" = "#b0f7f1")) +
  labs(title = "Correlation between Rating Count and Average Rating for Ice Cream Flavors",
       x = "Rating Count",
       y = "Average Rating")
