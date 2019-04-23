climate_change = read.csv("climate_change.csv")
training_data = subset(climate_change, Year < 2007)
testing_data = subset(climate_change, Year >2006)
training_model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training_data)
summary(training_model)
cor(training_data)
training_model = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = training_data)
summary(training_model)
old_model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = training_data)
new_model = step(old_model)
prediction =predict(new_model, newdata = testing_data)
SSE = sum((prediction - testing_data$Temp)^2)
SST = sum((mean(training_data$Temp) - testing_data$Temp)^2)
R2 = 1 - SSE/SST
R2



