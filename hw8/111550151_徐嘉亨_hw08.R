library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(fpc)
library(cluster)
library(e1071)

speech = file.path('elon musk\\')
docs <- VCorpus(DirSource(speech))
summary(docs)

inspect(docs[1])

writeLines(as.character(docs[1]))

docs <- tm_map(docs,removePunctuation)

for (j in seq(docs)){
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub('e2\u0080e ', " ", docs[[j]])
  docs[[j]] <- gsub('e2\u0080', " ", docs[[j]])
  docs[[j]] <- gsub("e2\u2028e", " ", docs[[j]])
  docs[[j]] <- gsub("e2\u2028", " ", docs[[j]])
  docs[[j]] <- gsub("\u0080 ", " ", docs[[j]])
}

docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[1]))

dtm <- DocumentTermMatrix(docs)
dtm

dt=as.matrix(dtm)
dim(dt)

freq <- colSums(as.matrix(dtm))
length(freq)

freq

dtms <- removeSparseTerms(dtm, 0.2)
dtms

freq2 <- colSums(as.matrix(dtms))
length(freq2)

freq2

freq3 <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq3, 20)
tail(freq3, 20)

findFreqTerms(dtm, lowfreq=50)

wf <- data.frame(word=names(freq3), freq=freq3)
head(wf)

p <- ggplot(subset(wf, freq3>20), aes(x = reorder(word, -freq), y = freq, color = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p

findAssocs(dtms, "well", corlimit=0.70)
findAssocs(dtms, c("just", "things", "make"), corlimit=0.80)

set.seed(1234)
wordcloud(names(freq3), freq3, min.freq = 10, colors = brewer.pal(8, "Dark2"))
wordcloud(names(freq3), freq3, max.words = 100, colors = brewer.pal(8, "Dark2"))

dtmss <- dtms[,0:40]
dtmss

d <- dist(t(dtmss), method="euclidian")
fit <- hclust(d=d, method="complete")
fit

plot(fit, hang=-1)
groups <- cutree(fit, k=6) 
rect.hclust(fit, k=6, border="red")

kfit <- kmeans(d, 2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

x.df = as.data.frame(as.matrix(dtm))
dim(x.df)

x.df$class_label = c(rep(0,2),rep(1,2))

index = sample(1:2, 1)

train = x.df[-c(index,index+2),]
test = x.df[c(index,index+2),]

table(train$class_label)

s = findFreqTerms(dtm, lowfreq = 10)
s = c(s,'class_label')
s

model.glm = glm(class_label ~ ., train[,s],family="binomial")

coef(model.glm)

p <- predict(model.glm, test, type = "response")

labels <- ifelse(p > 0.5, "1", "0")

tab=table('predict'=labels,'real'=test$class_label);tab

sum(diag(tab))/sum(tab)

model_svm = svm(class_label ~., data = train[,s],kernel = "radial",gamma = 0.00390625,cost = 0.25)
model_svm

p.svm = ifelse(predict(model_svm,newdata = test) < 0.5,0,1)
tab2=table('predict'=p.svm,'real'=test$class_label);tab2

sum(diag(tab2))/sum(tab2)