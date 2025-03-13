library(tidyverse)

Netflix <- read.csv(file = 'netflix_titles.csv', na.strings = c("", "NA"), header = TRUE, sep = ',')

class(Netflix)
str(Netflix)

sum(!complete.cases(Netflix))

missing_data_summary <- data.frame(
  Feature = colnames(Netflix),
  Missing_Values = sapply(Netflix, function(x) sum(is.na(x)))
)
missing_data_summary

Netflix$director[is.na(Netflix$director)] <- "Unknown"
sum(is.na(Netflix$director))

bootstrap <- function(series){
  
  tb <- table(series[complete.cases(series)])
  
  prob <- tb / sum(tb)
  
  smpl <- sample(names(tb), prob=prob, size=length(sum(!complete.cases(series))))
  
  series[!complete.cases(series)] <- smpl
  return (series)
}

Netflix$cast <- bootstrap(Netflix$cast)
Netflix$country <- bootstrap(Netflix$country)
sum(is.na(Netflix$cast))
sum(is.na(Netflix$country))

top_date <- names(sort(table(Netflix$date_added), decreasing = TRUE))[1]
Netflix$date_added[is.na(Netflix$date_added)] <- top_date
sum(is.na(Netflix$date_added))

top_rating <- names(sort(table(Netflix$rating), decreasing = TRUE))[1]
Netflix$rating[is.na(Netflix$rating)] <- top_rating
sum(is.na(Netflix$rating))

top_duration <- names(sort(table(Netflix$duration), decreasing = TRUE))[1]
Netflix$duration[is.na(Netflix$duration)] <- top_duration
sum(is.na(Netflix$duration))

sum(!complete.cases(Netflix))

missing_data_summary <- data.frame(
  Feature = colnames(Netflix),
  Missing_Values = sapply(Netflix, function(x) sum(is.na(x)))
)
missing_data_summary