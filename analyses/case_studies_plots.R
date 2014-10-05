require(ggplot2)
require(reshape2)
df<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv")

df$Polity2<-df$polity2
df$Trade<-df$openk
df$FDI<-df$fdiinward
df$FPI<-df$fpistock
df$FH.Score<-df$FHscore

cases<-subset(df, year>=1993 & year<=2003 & (country=="MEXICO" |
                                               country=="ARGENTINA"),
              select=c("country", "year", "FH.Score", "FDI", "FPI", "Trade"))

cases[3:8]<-sapply(cases[3:8], function(x) scale(x))

dfm <- melt(cases, id.vars=c("country","year"))

case_time_series_plot<-ggplot(data=dfm) +
  geom_line(aes(x=year, y=value, linetype=variable, colour=variable)) +
  theme_bw() +
  labs(x="Year", y="Value") +
  facet_wrap( ~ country)