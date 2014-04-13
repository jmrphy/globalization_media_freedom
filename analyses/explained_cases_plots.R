require(ggplot2)
require(reshape2)
require(gridExtra)
df<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv")

df$Polity2<-df$polity2
df$Trade<-df$openk
df$FDI<-df$fdiinward
df$FPI<-df$fpistock
df$FH.Score<-df$FHscore
df$GDPPC<-df$rgdpch/100

# Data for HAITI and Malawi begin in 1970
cases<-subset(df, year>=1970 & year<=2003 & (country=="HAITI" |
                                               country=="MALAWI"),
              select=c("country", "year", "fp", "FDI", "FPI", "Trade", "FH.Score", "Polity2", "GDPPC"))

dfm <- melt(cases, id.vars=c("country","year"))

haiti_malawi_plots<-ggplot(data=subset(dfm, variable!="fp")) +
  geom_rect(data = subset(cases, fp == 0),
            aes(ymin = -Inf, ymax = Inf, xmin = year-0.5, xmax = year+0.5), alpha = 0.1) +
  geom_line(aes(x=year, y=value, linetype=variable, colour=variable)) +
  theme_bw() +
  theme(legend.position="none") +
  labs(x="Year", y="Value") +
  facet_wrap( ~ country)

# Data for Ukraine and Armenia begins in 1990
cases<-subset(df, year>=1990 & year<=2003 & (country=="UKRAINE" |
                                               country=="ARMENIA"),
              select=c("country", "year", "fp", "FDI", "FPI", "Trade", "FH.Score", "Polity2", "GDPPC"))

dfm <- melt(cases, id.vars=c("country","year"))

ukraine_armenia_plots<-ggplot(data=subset(dfm, variable!="fp")) +
  geom_rect(data = subset(cases, fp == 0),
            aes(ymin = -Inf, ymax = Inf, xmin = year-0.5, xmax = year+0.5), alpha = 0.1) +
  geom_line(aes(x=year, y=value, linetype=variable, colour=variable)) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(x="Year", y="Value") +
  facet_wrap( ~ country)

cases<-subset(df, year>=1970 & year<=2003 & country=="KUWAIT",
              select=c("country", "year", "fp", "FDI", "FPI", "Trade", "FH.Score", "Polity2", "GDPPC"))

dfm <- melt(cases, id.vars=c("country","year"))

kuwait_plot<-ggplot(data=subset(dfm, variable!="fp")) +
  geom_rect(data = subset(cases, fp == 0),
            aes(ymin = -Inf, ymax = Inf, xmin = year-0.5, xmax = year+0.5), alpha = 0.1) +
  geom_line(aes(x=year, y=value, linetype=variable, colour=variable)) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  labs(x="Year", y="Value")

