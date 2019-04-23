pisatest = read.csv("pisa2009test.csv")
pisatrain = read.csv("pisa2009train.csv")
str(pisatrain)
tapply(pisatrain$readingScore, pisatrain$male,mean)
pisatrain = na.omit(pisatrain)
str(pisatrain)
pisatest = na.omit(pisatest)
str(pisatrain)
table(pisatrain$grade)
table(pisatrain$male)
table(pisatrain$raceeth)
pisatrain$raceeth = relevel(pisatrain$raceeth,"White")
pisatest$raceeth = relevel(pisatest$raceeth,"White")
table(pisatrain$raceeth)
lmScore = lm(readingScore~.,data = pisatrain)
summary(lmScore)
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisatrain))
RMSE
summary(lmScore)
29.542707 * 2
redtest = predict(lmScore,newdata = pisatest)
summary(redtest)
637.7 - 353.2
SSE = sum((redtest - pisatest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisatest))
RMSE

mean(pisatrain$readingScore)
SST = sum((mean(pisatrain$readingScore) - pisatest$readingScore)^2)
SST
testsetRsquared = 1 - SSE/SST
testsetRsquared
