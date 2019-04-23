trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(nchar(trials$abstract)) # or
max(nchar(trials$abstract))
table(nchar(trials$abstract) == 0)
trials$title[which.min(nchar(trials$title))]
corpusTitle = VCorpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
tr(dtmTitle)
str(dtmAbstract)
csAbstract = colSums(dtmAbstract)
which.max(csAbstract)
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
str(dtm)
set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(train$trial)
730/(730+572)
trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)
predTrain = predict(trialCart)[,2]
summary(predTrain)
table(train$trial, predTrain >= 0.5)
#accuracy 
(631+441)/(631+441+99+131)
#sensitivity 
441/(441+131)
# specificity 
631/(631+99)
predTest = predict(trialCART, newdata=test)[,2]
table(test$trial, predTest >= 0.5)
(261+162)/(261+162+83+52)
library(ROCR)
pred = prediction(predTest, test$trial)
as.numeric(performance(pred, "auc")@y.values)

