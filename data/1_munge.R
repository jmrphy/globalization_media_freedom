require(foreign)
require(countrycode)

media<-read.csv("~/Dropbox/Data General/Media Freedom/VanBelleCleaned.csv")

econ<-read.dta("~/Dropbox/Data General/Sorens and Ruger/bronsetmi.dta")

econ$fdiinward<-econ$fdistock2
econ$fdiinward2<-log(econ$fdiinward+1)

econ$fpistock<-econ$fpistockgdp
econ$fpistock2<-log(econ$fpistock +1)

econ$openk<-econ$trade
econ$openk2<-log(econ$openk)

econ$rgdpch<-econ$gdppc
econ$rgdpch2<-log(econ$rgdpch)

econ$internet<-lag(log(econ$internet+1))
econ$oil<-lag(econ$oil)
econ$ethfrac<-lag(econ$ethfrac)
econ$relfrac<-lag(econ$relfrac)








econ$scode<-econ$ccode

require(plm)
econ2<-pdata.frame(econ, index=c("scode", "year"))

econ2$lpolity2<-lag(econ2$polity2)
econ2$dpolity2<-diff(econ2$polity2)

econ2$lrgdpch<-lag(econ2$rgdpch)
econ2$lrgdpch2<-lag(econ2$rgdpch2)

econ2$drgdpch<-diff(econ2$rgdpch)
econ2$drgdpch2<-diff(econ2$rgdpch2)

econ2$lopenk<-lag(econ2$openk)
econ2$lopenk2<-lag(econ2$openk2)
econ2$dopenk<-diff(econ2$openk)
econ2$dopenk2<-diff(econ2$openk2)

econ2$lfdiinward<-lag(econ2$fdiinward)
econ2$lfdiinward2<-lag(econ2$fdiinward2)
econ2$dfdiinward<-diff(econ2$fdiinward)
econ2$dfdiinward2<-diff(econ2$fdiinward2)


econ2$lfpistock<-lag(econ2$fpistock)
econ2$lfpistock2<-lag(econ2$fpistock2)
econ2$dfpistock<-diff(econ2$fpistock)
econ2$dfpistock2<-diff(econ2$fpistock2)


econ<-as.data.frame(econ2)
rm(econ2)

df<-merge(media, econ, by=c("scode", "year"))


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


df<-merge(df, kof, by=c("scode", "year"), all.x=TRUE)

df$year1<-df$year
df$year2<-as.numeric(df$year)^2
df$year3<-as.numeric(df$year)^3
df$country<-countrycode(df$scode, "cown", "country.name")

require(gdata)
fh<-read.xls("~/Dropbox/gh_projects/globalization_media_freedom/data/FreedomHouse_1980_2013.xls", sheet=2)
fh<-fh[3:212,1:62]
fh<-fh[,c(1,23:62)]
fh<-fh[, c(1, seq(2, ncol(fh), by = 2))]


names(fh)[1]<-paste("Country")
names(fh)[2:21]<-paste(1993:2012)
fh<-fh[3:210,]

require(reshape)

fhlong<-reshape(fh, direction="long", varying=list(names(fh)[2:21]), v.names="FHscore", 
                idvar=c("Country"), timevar="Year", times=1993:2012)

fhlong$FHscore[fhlong$FHscore=="N/A"]<-NA

fhlong$FHscore<-as.numeric(levels(fhlong$FHscore))[fhlong$FHscore]

fhlong$FHscore<-100-fhlong$FHscore
fh<-fhlong
rm(fhlong)

require(countrycode)
fh$scode<-countrycode(fh$Country, "country.name", "cown")

fh$scode[fh$Country=="Germany, East"]<-NA
fh$scode[fh$Country=="Germany, West"]<-NA
fh$scode[fh$Country=="Yemen, North"]<-NA
fh$scode[fh$Country=="Yemen, South"]<-NA
fh$scode[fh$Country=="Cyprus (Turkish)"]<-NA
fh$scode[fh$Country=="USSR"]<-NA

fh$year<-fh$Year

fh<-fh[with(fh, order(scode, year)), ]

df<-merge(df, fh, by=c("scode", "year"), all.x=TRUE)

require(arm)
modelvars<-subset(df, select=c("scode", "warl", "onset", "oil", "fp", "lfp", "fp2", "lfp2", "year", "year1", "year2", "year3", "FHscore",  "lopenk", "lopenk2",
                               "dopenk", "dopenk2", "lfdiinward", "lfdiinward2", "dfdiinward", "dfdiinward2", "lfpistock", "lfpistock2", "dfpistock", "dfpistock2", "lpolity2", "dpolity2",
                               "lrgdpch", "lrgdpch2", "drgdpch", "drgdpch2", "economic.globalization", "leconglob", "ldeconglob", "lrestrict", "ldrestrict",
                               "lpolglob", "ldpolglob", "linfoglob", "ldinfoglob", "leconglob", "ldeconglob",
                               "lflows", "ldflows", "loverallglob", "ldoverallglob", "actual.flows", "restrictions",
                               "political.globalization", "information.flows", "overall.globalization.index",
                                "internet", "ethfrac", "relfrac")) 
modelvars[,10:54]<-sapply(modelvars[,10:54], rescale)




rm(econ,media,kof,kof2)

attach(df)
countryavgs <-aggregate(df, by=list(scode), 
                        FUN=mean, na.rm=TRUE)
detach(df)


countryavgs$country<-countrycode(countryavgs$Group.1, "cown", "iso3c")

countryavgs$dem<-ifelse(countryavgs$polity2>median(countryavgs$polity2, na.rm=TRUE), "Greater than median democracy", "Less than median democracy")
countryavgs$dem<-as.factor(countryavgs$dem)

setwd("~/Dropbox/gh_projects/globalization_media_freedom/data")

df<-subset(df, !duplicated(subset(df,select=c(scode,year))))
modelvars<-subset(modelvars, !duplicated(subset(modelvars,select=c(scode,year))))
countryavgs<-subset(countryavgs, !duplicated(subset(countryavgs,select=c(scode,year))))


write.csv(countryavgs, "out_countryavgs.csv")
write.csv(df, "out_df.csv")
write.csv(modelvars, "out_modelvars.csv")