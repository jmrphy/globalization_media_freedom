require(countrycode)
source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

### Identify incorrect cases in Model 1
classifs<-data.frame(model1$result$fitted.values, model1$result$y, model1$result$data)
model1.anomolies<-classifs[(classifs$model1.result.y==1 & classifs$model1.result.fitted.values<.5) |
                           (classifs$model1.result.y==0 & classifs$model1.result.fitted.values>.5),]
model1.anomolies$country<-countrycode(model1.anomolies$scode, "cown", "country.name")
model1.anomolies$Country<-countrycode(model1.anomolies$scode, "cown", "country.name")
model1.anomolies$Year<-model1.anomolies$year

### Identify incorrect cases in Model 3
classifs<-data.frame(model3$result$fitted.values, model3$result$y, model3$result$data)
model3.anomolies<-classifs[(classifs$model3.result.y==1 & classifs$model3.result.fitted.values<.5) |
                           (classifs$model3.result.y==0 & classifs$model3.result.fitted.values>.5),]
model3.anomolies$Country<-countrycode(model3.anomolies$scode, "cown", "country.name")
model3.anomolies$Year<-model3.anomolies$year



### Cases unexplained by Model 1 but explained by Model 3
model3.cases<-subset(model1.anomolies, !(Country %in% model3.anomolies$Country), select=c("Country", "Year"))



