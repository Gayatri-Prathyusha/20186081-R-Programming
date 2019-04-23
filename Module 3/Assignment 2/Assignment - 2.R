FluTrain = read.csv("FluTrain.csv")
hist(FluTrain$ILI, breaks = 100)
plot(log(FluTrain$ILI), log(FluTrain$Queries))
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
cor(log(FluTrain$ILI), FluTrain$Queries)
# correalation_value ^ 2 =  R-squaredvalue
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
subset(FluTest, Week=="2012-03-11 - 2012-03-17")
# relative_error = (Observed ILI - Estimated ILI)/Observed ILI
(2.293422 - 2.187378)/2.293422
SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)
FluTrain$ILI[nrow(FluTrain)-1]
FluTrain$ILI[nrow(FluTrain)]
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2 - FluTest$ILI)^2)
SSE
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
SSE = sum((PredTest1 - FluTest$ILI)^2)
SSE
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

