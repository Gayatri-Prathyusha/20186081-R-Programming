machine = read.csv("machine.csv")
summary(machine)
str(machine)

#Question 1
#Compute the model R2 (the "Multiple R-squared" value)? Consider the entire dataset as training dataset (1Mark)

lmScore = lm(PRP ~ X + MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, data =machine )
summary(lmScore)

# Ans : 0.87

#Question 2

#Which variables are significant in the model? We will consider a variable significant only if the p-value is below 0.05. 1Mark

step(lmScore)

Output:

MYCT
MMIN
MMAX
CACH
CHMAX


#Question 3
#Compute the correlations between all the variables in the training set. Which of the following independent variables is MMAX highly correlated with (absolute correlation greater than 0.7)?1Mark

cor(machine$X, machine$MMAX)
cor(machine$MYCT,machine$MMAX)
cor(machine$MMIN,machine$MMAX)
cor(machine$CHMAX,machine$MMAX)
machine$VendorName = NULL
machine$ModelName = NULL
machine$ERP =NULL
str(machine)
cor(machine)
#ans :
MMIN = 0.7578


#Question 4
#Which of the independent variable is highly correlated with PRP?

cor(machine)
#ans:
MMIN
MMAX
CACH


#Question 5:
#Given that the correlations are so high, let us focus on the MMAX variable and build a model with only variables which have correlation is between -0.3 to 0.3 with MMAX. Compute the coefficient of MMAX in this reduced model.1Mark
#variables between -0.42 nd  +0.42 are X and MYCT

lmmax  =  lm(PRP ~ MMAX+ X +MYCT, data = machine)
summary(lmmax)
#ans:
MMAX         1.201e-02  


#Question 6:
#Compute the above reduced model R2 : 1Mark

summary(lmmax)
#ans:
Multiple R-squared:  0.7503

#Question 7:
#Compute the R2 value of the model produced by the step function.1Mark

newmodel = step(lmmax)
summary(newmodel)
#ans:
Multiple R-squared:  0.7492


#Question 8:
#Split your data set into train and test sets(75: 25). Compute the testing set R2 using the model produced from the step function on trained data set. (Set seed = 6) 2 Marks

install.packages("caTools")
library(caTools)
set.seed(6) 
split = sample.split(machine,SplitRatio = 0.75)
split

machinetrain = subset(machine , split == TRUE)
machinetest = subset(machine , split ==FALSE)
nrow(machinetrain)
nrow(machinetest) 

model =lm(PRP ~ X + MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, data =machinetrain)

summary(model)

a = step(model)
 prediction = predict(a, newdata = machinetest)
 SSE = sum((prediction - machinetest$PRP)^2)
  SSE
 #SST = 476701.2
 
 SST = sum((machinetest$PRP - mean(machinetrain$PRP))^2) 
 SST
#SST =  3047293
 
 R2 = 1 - (SSE /  SST)
 R2
#R2 =  0.8435657
 

 




