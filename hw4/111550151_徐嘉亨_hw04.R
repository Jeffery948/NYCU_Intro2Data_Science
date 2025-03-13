library(tidyverse)

Netflix <- read.csv(file = 'netflix_titles.csv', na.strings = c("", "NA"), header = TRUE, sep = ',')

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

top_date <- names(sort(table(Netflix$date_added), decreasing = TRUE))[1]
Netflix$date_added[is.na(Netflix$date_added)] <- top_date

top_rating <- names(sort(table(Netflix$rating), decreasing = TRUE))[1]
Netflix$rating[is.na(Netflix$rating)] <- top_rating

top_duration <- names(sort(table(Netflix$duration), decreasing = TRUE))[1]
Netflix$duration[is.na(Netflix$duration)] <- top_duration

missing_data_summary <- data.frame(
  Feature = colnames(Netflix),
  Missing_Values = sapply(Netflix, function(x) sum(is.na(x)))
)
missing_data_summary

# Above codes are to deal with missing value

Type <- table(Netflix$type)
Typeplot  <- barplot(Type, main = "Content Types", col = c('red', 'blue'), xlab = "Type", ylab = "Count", ylim = c(0, 7000))
text(x = Typeplot, y = Type, labels = c(Type[1], Type[2]), pos = 3)

boxplot(Netflix$release_year, main = "Release Year (with Outliers)")
boxplot(Netflix$release_year, outline = FALSE, main = "Release Year (without Outliers)")

ggplot(Netflix, aes(x = release_year)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ggtitle("Release Year Density Plot")

country_counts <- table(Netflix$country)
top_country_counts <- country_counts[head(order(-country_counts), 10)]
pie(top_country_counts, labels = names(top_country_counts), main = "Top 10 Producing Countries (Pie Chart)")

Netflix$date_added <- mdy(Netflix$date_added)  # Convert to Date format
time_series <- table(year(Netflix$date_added))
plot(names(time_series), as.numeric(time_series), 
     type = "o", main = "Content Added Over Time", xlab = "Year", ylab = "Count")

rating_counts <- table(Netflix$rating)

threshold <- 400

frequent_ratings <- rating_counts[rating_counts >= threshold]
other_ratings <- sum(rating_counts[rating_counts < threshold])

rating_data <- c(frequent_ratings, Other = other_ratings)

pie(rating_data, labels = paste(names(rating_data), "\n", rating_data), 
    main = "Rating Distribution (Pie Chart)")

convert_to_minutes <- function(duration) {
  if (grepl("min", duration, ignore.case = TRUE)) {
    # Extract minutes for durations in minutes
    return(as.numeric(gsub("\\D", "", duration)))
  } else if (grepl("Season", duration, ignore.case = TRUE)) {
    # Convert seasons to minutes (assuming 10 episodes per season and 30 minutes per episode)
    return(as.numeric(gsub("\\D", "", duration)) * 10 * 30)
  } else {
    # Handle other formats (e.g., "1 season" or other non-standard formats)
    return(NA)
  }
}

duration_minutes <- sapply(Netflix$duration, convert_to_minutes)
release <- Netflix$release_year[!is.na(duration_minutes)]
duration_minutes <- duration_minutes[!is.na(duration_minutes)]

plot(release, duration_minutes,
     main = "Duration vs Release Year",
     xlab = "Release Year",
     ylab = "Duration (minutes)")

plot(Netflix$release_year[Netflix$type == "Movie"], Netflix$duration_minutes[Netflix$type == "Movie"],
     main = "Duration vs Release Year (Movies)",
     xlab = "Release Year",
     ylab = "Duration (minutes)")

plot(Netflix$release_year[Netflix$type == "TV Show"], Netflix$duration_minutes[Netflix$type == "TV Show"],
     main = "Duration vs Release Year (TV Shows)",
     xlab = "Release Year",
     ylab = "Duration (minutes)")

movie_data <- Netflix[Netflix$type == "Movie", ]
movie_lm <- lm(duration_minutes ~ release_year, data = movie_data)

tvshow_data <- Netflix[Netflix$type == "TV Show", ]
tvshow_lm <- lm(duration_minutes ~ release_year, data = tvshow_data)

combined_lm <- lm(duration_minutes ~ release_year, data = Netflix)

plot(Netflix$release_year, Netflix$duration_minutes,
     main = "Duration vs Release Year with Linear Regression",
     xlab = "Release Year",
     ylab = "Duration (minutes)", ylim = c(0, 1500))
S
# Add regression lines to the plot
abline(movie_lm, col = "red", lwd = 2) # Red line for movies
abline(tvshow_lm, col = "blue", lwd = 2) # Blue line for TV shows
abline(combined_lm, col = "green", lwd = 2) # Green line for combined data