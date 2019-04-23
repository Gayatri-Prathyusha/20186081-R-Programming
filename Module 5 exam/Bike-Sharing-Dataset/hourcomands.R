#Question 1
hour = read.csv("hour.csv")
summary(hour)
str(hour)
nrow(hour)
#17379

#Question 2
ncol(hour)
#17

#Question 3
max(hour$temp) - min(hour$temp)
#0.98

#Question 4
table(hour$workingday == 1)
#11865


#Question 5
table(hour$season)
#season : season (1:springer, 2:summer, 3:fall, 4:winter)
#4242

#Question 6
hourdate = hour$dteday
betterdate = as.Date(hour$dteday, format = "%Y-%d-%m")
summary(betterdate)
#01,2012

#Question 7
hour$dteday = as.Date(hour$dteday)

count = subset(hour$cnt,hour$dteday >= "2012-05-18" & hour$dteday <= "2012-05-21")
sum(count)

#27421


#Question 8
hour$mnth = months(betterdate)
hour$yr = as.numeric(format(betterdate,"%Y"))
tapply(hour$cnt, hour$mnth, mean,na.rm = TRUE)
tapply(hour$cnt,hour$yr,mean,na.rm =TRUE))

12,2012

#Question 9
Tuesday, 2

#Question 10
hist(hour$yr , hour$cnt, breaks = 100)
increases

#question 11
0.4176



#question 12







