require(foreign)

media<-read.csv("~/Dropbox/Data General/Media Freedom/VanBelleCleaned.csv")

econ<-read.dta("~/Dropbox/Data General/Sorens and Ruger 2012/invrep-replicate.dta")
econ$scode<-econ$ccode

require(plm)
econ2<-pdata.frame(econ, index=c("scode", "year"))
econ2$lpolity2<-lag(econ2$polity2)
econ2$ldpolity2<-diff(econ2$lpolity2)
econ2$lrgdpch<-lag(econ2$rgdpch)
econ2$lgrgdpch<-lag(econ2$grgdpch)
econ2$lopenk<-lag(econ2$openk)
econ2$lfdiinward<-lag(econ2$fdiinward)
econ2$lfdiinflow<-lag(econ2$fdiinflow)
econ2$lfpistock<-lag(econ2$fpistock)
econ2$lfpi<-lag(econ2$fpi)
econ2$ldopenk<-diff(econ2$lopenk)
econ<-as.data.frame(econ2)
rm(econ2)

df<-merge(media, econ, by=c("scode", "year"))

df$interp<-ifelse(df$year>=1996, 1, 0)


require(arm)
modelvars<-subset(df, select=c("scode", "year", "interp", "fp", "lfp", "fp2", "lfp2", "lopenk",
          "ldopenk", "lfdiinward", "lfdiinflow", "lfpi", "lfpistock", "lpolity2", "ldpolity2", "lrgdpch", "lgrgdpch")) 
modelvars[,8:17]<-sapply(modelvars[,8:17], rescale)




rm(econ,media)

attach(df)
countryavgs <-aggregate(df, by=list(scode), 
                        FUN=mean, na.rm=TRUE)
detach(df)

require(countrycode)
countryavgs$country<-countrycode(countryavgs$Group.1, "cown", "iso3c")

countryavgs$dem<-ifelse(countryavgs$polity2>-2.21, "Greater than median democracy", "Less than median democracy")
countryavgs$dem<-as.factor(countryavgs$dem)

setwd("~/Dropbox/Projects/glob_media_freedom/data")

write.csv(countryavgs, "out_countryavgs.csv")
write.csv(df, "out_df.csv")
write.csv(modelvars, "out_modelvars.csv")
