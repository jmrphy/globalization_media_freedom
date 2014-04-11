require(foreign)
require(countrycode)

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

setwd("~/Dropbox/Data General/KOF")
require(gdata)
kof<-read.xls("globalization_2012_long.xls", sheet=2, na.strings=c("."))
kof$scode<-countrycode(kof$X.1, "iso3c", "cown")
kof$year<-kof$X.2
kof<-kof[2:8321, c(1,4:14)]
kof[,c(2:10,12)]<-as.data.frame(sapply(kof[,c(2:10,12)], function(x) as.numeric(levels(x))[x]))

kof<-subset(kof, select=c("scode", "year", "economic.globalization", "actual.flows", "restrictions", "political.globalization", "information.flows", "overall.globalization.index"))

kof2<-pdata.frame(kof, index=c("scode", "year"))
kof2$leconglob<-lag(kof2$economic.globalization)
kof2$ldeconglob<-diff(kof2$leconglob)
kof2$lrestrict<-lag(kof2$restrictions)
kof2$ldrestrict<-diff(kof2$lrestrict)
kof2$lflows<-lag(kof2$actual.flows)
kof2$ldflows<-diff(kof2$lflows)
kof2$lpolglob<-lag(kof2$political.globalization)
kof2$ldpolglob<-diff(kof2$lpolglob)
kof2$linfoglob<-lag(kof2$information.flows)
kof2$ldinfoglob<-diff(kof2$linfoglob)
kof2$loverallglob<-lag(kof2$overall.globalization.index)
kof2$ldoverallglob<-diff(kof2$loverallglob)
kof<-as.data.frame(kof2)


df<-merge(df, kof, by=c("scode", "year"), all=TRUE)

df<-subset(df, !duplicated(subset(df,select=c(scode,year))))

df$year1<-df$year
df$year2<-as.numeric(df$year)^2
df$year3<-as.numeric(df$year)^3

require(arm)
modelvars<-subset(df, select=c("scode", "interp", "fp", "lfp", "fp2", "lfp2", "year", "year1", "year2", "year3", "lopenk",
                               "ldopenk", "lfdiinward", "lfdiinflow", "lfpi", "lfpistock", "lpolity2", "ldpolity2",
                               "lrgdpch", "lgrgdpch", "economic.globalization", "leconglob", "ldeconglob", "lrestrict", "ldrestrict",
                               "lpolglob", "ldpolglob", "linfoglob", "ldinfoglob",
                               "lflows", "ldflows", "loverallglob", "ldoverallglob", "actual.flows", "restrictions",
                               "political.globalization", "information.flows", "overall.globalization.index")) 
modelvars[,8:38]<-sapply(modelvars[,8:38], rescale)




rm(econ,media,kof,kof2)

attach(df)
countryavgs <-aggregate(df, by=list(scode), 
                        FUN=mean, na.rm=TRUE)
detach(df)


countryavgs$country<-countrycode(countryavgs$Group.1, "cown", "iso3c")

countryavgs$dem<-ifelse(countryavgs$polity2>-2.21, "Greater than median democracy", "Less than median democracy")
countryavgs$dem<-as.factor(countryavgs$dem)

setwd("~/Dropbox/gh_projects/globalization_media_freedom/data")

write.csv(countryavgs, "out_countryavgs.csv")
write.csv(df, "out_df.csv")
write.csv(modelvars, "out_modelvars.csv")