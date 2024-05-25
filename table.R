library(dplyr)
library(knitr)
library(kableExtra)

icecream_data <- read.csv('icecream_data.csv')

classify_ice_cream <- function(ingredients) {
  dairy_keywords <- c('milk', 'cream', 'butter')
  if (any(sapply(dairy_keywords, grepl, ingredients, ignore.case = T))) {
    return('dairy')
  } else {
    return('dairy-free')
  }
}

# creating new col for non-dairy
icecream_data$class <-
  sapply(icecream_data$description, classify_ice_cream)

# aggregating by brand
aggregated_data <- icecream_data %>%
  group_by(brand) %>%
  summarise(
    avg_rating = round(mean(rating, na.rm = T), 2),
    total_ratings = sum(rating_count, na.rm = T),
    total_icecreams = n(),
    dairy_free_count = sum(class == 'dairy-free', na.rm = T)
  ) %>%
  # sorting by average rating
  arrange(desc(avg_rating))

# styling table bc it was ugly printed
styled_table <- aggregated_data %>%
  kable("html", caption = "Ice Creams Grouped by Brand", escape = F) %>%
  kable_styling(full_width = F) %>%
  row_spec(0, bold = T, color = "#fca9c9") %>%  
  column_spec(1, bold = T, color = "#9bd1cd") 

saveRDS(styled_table, "styled_table.rds")
