library(tidyverse)
library(psych)
library(ggplot2)
library(naniar)
library(corrplot)

Netflix <- read.csv(file = 'netflix_titles.csv', na.strings = c("", "NA"), header = TRUE, sep = ',')

dim(Netflix) 
nrow(Netflix) 
ncol(Netflix)

head(Netflix,10)
tail(Netflix,10)
summary(Netflix)

plot(Netflix)
describe(Netfilx)

gg_miss_upset(Netflix)

Netflix$director[is.na(Netflix$director)] <- "Unknown"

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
# deal with missing value

pie(table(Netflix$type), main = "Content Types", col = c('red', 'blue'))

hist(Netflix$release_year, main = "Distribution of Release Years", xlab = "Release Year", ylab = "Frequency")

ggplot(Netflix, aes(x = release_year)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ggtitle("Release Year Density Plot")

boxplot(Netflix$release_year, main = "Release Year (with Outliers)")
boxplot(Netflix$release_year, outline = FALSE, main = "Release Year (without Outliers)")

Netflix%>%filter(!str_detect(rating,regex('min')))%>%
  mutate(rating=rating%>%fct_infreq()%>%
           fct_rev())%>%ggplot()+geom_bar(aes(rating,fill=rating))+
  guides(fill=F)+labs(title='Ratings of movie or Tv show')

Netflix$date_added <- mdy(Netflix$date_added)  # Convert to Date format
time_series <- table(year(Netflix$date_added))
plot(names(time_series), as.numeric(time_series), 
     type = "o", main = "Content Added Over Time", xlab = "Year", ylab = "Count")

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

release <- Netflix$release_year[!is.na(duration_minutes)]
duration_minutes <- duration_minutes[!is.na(duration_minutes)]

duration_minutes <- sapply(Netflix$duration, convert_to_minutes)
plot(release, duration_minutes,
     main = "Duration vs Release Year",
     xlab = "Release Year",
     ylab = "Duration (minutes)")
cor(Netfilx$release_year, duration_minutes)

dir<-Netflix%>%count(director,sort=TRUE)%>%filter(director!="Unknown",n>10)
ggplot(dir)+geom_bar(mapping=aes(x=reorder(director,n,FUN=median),n,fill=n),stat='identity')+
  guides(fill=F)+coord_flip()+labs(title='Top directors',x='director',y='count')