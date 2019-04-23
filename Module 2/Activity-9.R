wine = read.csv("wine.csv")
Model1 = lm(Price ~ HarvestRain + WinterRain, data= wine)
summary(Model1)