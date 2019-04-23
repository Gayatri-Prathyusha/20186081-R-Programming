MVT = read.csv("mvtWeek1.csv")
summary(MVT)
str(MVT)
which.max(MVT$ID)
MVT$ID[18134]
which.min(MVT$Beat)
MVT$Beat[4756]
MVT$Arrest = as.numeric(MVT$Arrest)
summary(MVT)
MVT$Date[1]
DateConvert = as.Date(strptime(MVT$Date,"%m/%d/%y%H:%M"))
summary(DateConvert)
MVT$Month = months(DateConvert)
MVT$Weekday = weekdays(DateConvert)
MVT$Date = DateConvert
table(MVT$Date)
summary(MVT$Date)
summary(MVT$Month)
table(MVT$Month)
which.min(table(MVT$Month))
which.max(table(MVT$Weekday))
table(MVT$Arrest,MVT$Month)
hist(MVT$Date, breaks = 100)
boxplot(MVT$Date~MVT$Arrest)
table(subset(MVT$Arrest,MVT$Year == 2001))
c = 2152/18517
c = 2152/(2152+18517)
c
table(subset(MVT$Arrest,MVT$Year == 2007))
c= 1212/(1212+13068)
c
table(subset(MVT$Arrest, MVT$Year == 2012))
550/(550+13542)
sort(table(MVT$LocationDescription), decreasing = TRUE)[1:6]
indices = MVT$LocationDecription %in% c("STREET","GAS STATION","PARKING LOT/GARAGE(NON.RESID.)","ALLEY","DRIVEWAY - RESIDENTIAL")
Top5 = subset(MVT, indices == TRUE)
str(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription)
sort(table(MVT$LocationDescription))
hist(MVT$Date, breaks = 10)
which.max(table(subset(MVT,LocationDescription == "GAS STATION")$Weekday))
subset(MVT,LocationDescription == "GAS STATION")
which.min(table(subset(MVT,LocationDescription == "DRIVEWAY - RESIDENTIAL")$Weekday))

