library(tidyverse)

Netfilx <- read.csv(file = 'netflix_titles.csv', header = TRUE, sep = ',')

class(Netfilx)
str(Netfilx)
head(Netfilx, 10)

summary(Netfilx)
table(Netfilx$type)
table(Netfilx$release_year)

top_countries <- sort(table(Netfilx$country), decreasing = TRUE)
TC <- head(top_countries, 11)
TC <- TC[-3]
TC

top_directors <- sort(table(Netfilx$director), decreasing = TRUE)
td <- head(top_directors, 11)
td[2:11]

top_cast <- sort(table(Netfilx$cast), decreasing = TRUE)
tc <- head(top_cast, 11)
tc[2:11]

Duration <- sort(table(Netfilx$duration), decreasing = TRUE)
head(Duration, 10)