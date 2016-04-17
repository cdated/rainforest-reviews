# Install ggplot2 if necessary
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tm")) install.packages("tm")
if (!require("wordcloud")) install.packages("wordcloud")

library("ggplot2")
library("wordcloud")

# Read in reviews dataset
reviews = read.csv("datasets/hungry_caterpillar.csv", header = FALSE)

# Remove unnecessary columns
useful <- reviews[1,1:10]

# Convert review dates to sensible format
dates <- reviews[,7]
reviews[,7] = as.Date(dates, "on %B %d, %Y")

release_date <- as.Date("2013-01-01")
days_since_release <- as.numeric(reviews[,7] - release_date)

hist(days_since_release)

titles <- reviews[,9]
View(table(titles))

review_body = reviews[,10]

# Review titled five stars
five_stars_reviews <- reviews[reviews[,9]=="Five Stars",10]
View(table(five_stars_reviews))

review_source <- VectorSource(review_body)
#review_source <- VectorSource(five_stars_reviews)
#review_source <- VectorSource(titles)

corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, c('book', 'stars'))

dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 20)
wordcloud(corpus, max.words = 100, random.order = FALSE)

body_vector <- as.vector(review_body)
char_counts <- nchar(body_vector[nchar(body_vector) < 1000])
#char_counts <- nchar(body_vector)

print(sum(char_counts)/length(body_vector))

hist(char_counts)
plot(log2(char_counts))