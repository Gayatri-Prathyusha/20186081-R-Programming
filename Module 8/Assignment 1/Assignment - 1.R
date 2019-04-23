wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
summary(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)
corpusAdded = VCorpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
table(wikiTest$Vandal)
wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
table(wikiTest$Vandal, testPredictCART)
(618+12)/(618+533+12)
grepl("cat","dogs and cats",fixed=TRUE) # TRUE
grepl("cat","dogs and rats",fixed=TRUE) # FALSE
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)
(514+248)/(514+104+297+248)
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictTestCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictTestCART4)
(595+241)/(595+23+304+241)



