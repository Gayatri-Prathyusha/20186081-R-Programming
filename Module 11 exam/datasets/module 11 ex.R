#Question 1
bank = read.csv("bank-full.csv")
summary(bank)
set.seed(1000)
split = sample.split(bank, SplitRatio = 0.6)
train = subset(bank, split == TRUE)
test = subset(bank, split == FALSE)

#Question 1 -1:
Model1 = glm(y ~ age + balance +campaign +duration, data = train, family = "binomial")
summary(Model1)
#AIC = 15851

Model2 = glm(y ~ age+ balance +duration, data = train, family = "binomial")
summary(Model2)
#AIC = 16038


#Question 1- 2
predictmodel1 = predict(Model1,newdata = train, type = "response")
table(Model1$y , predictmodel1 > 0.5)
#sensitivity  
529/(2588 +529)
#Sensitivity = 0.1697145

#Specifity
23118/(23118 + 359)
#specifity = 0.9847084


#Question 1- 3
predicttest = predict(Model1, newdata = test, type = "response")
ROCRpred = prediction(predicttest, test$y)
as.numeric(performance(ROCRpred, "auc")@y.values)
#Model1 AUC = 0.8094504

#Question 1- 4
CARTModel11 = rpart(y ~ age +balance + duration,method = "class", data = bank)
prp(CARTModel11)
#2 Splits 

# Question 1- 5
predicttest = predict(CARTModel11, newdata = test)
predicttest = predicttest[,2]
ROCRpred = prediction(predicttest, test$y)
as.numeric(performance(ROCRpred, "auc")@y.values)
#0.6741044



#Question 1- 6
str(bank)
table(bank$marital == "married" & bank$job == "technician")
# proportion = 4052
cartinf = rpart(y ~ marital + job,data = bank, cp = 0.0)
cartinf
 
 
#Question 3 -1 :
read.table("Movies.txt", header = TRUE, sep = "\t",stringsAsFactors = FALSE)
read.delim("Movies.txt",stringsAsFactors = FALSE)
Movie = read.delim("Movies.txt",header = TRUE, sep = "\t", quote = "\"", dec = "|",fill = TRUE)
summary(Movie)
str(Movie)
Movies = read.table("Movies.txt", header = TRUE, sep = "|",quote = "\"")
read.table("Movies.txt", header = TRUE, sep = "|",quote = "\"")


str(Movies)
summary(Movies)
colnames(Movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western") 
colnames(Movies)

Movies$ID = NULL
Movies$ReleaseDate = NULL
Movies$VideoReleaseDate = NULL
Movies$IMDB = NULL

table(Movies$Action & Movies$Horror)


distances = dist(Movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies)
clusterGroups = cutree(clusterMovies, k = 10)
str(Movies)

#Question 5 -1:

climate = read.csv("climate_change.csv")
summary(climate)
str(climate)
sum(unique(climate$year))







