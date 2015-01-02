require(ggplot2)
require(gridExtra)

df<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv")
countryavgs<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_countryavgs.csv")

kofintroplot<-ggplot(subset(countryavgs, pop>2), aes(x=FHscore, y=economic.globalization)) +
  geom_smooth(method=lm, se=FALSE) +
  geom_point(alpha=.5) +
  geom_text(aes(label=country),hjust=.5, vjust=-.2, size=4, alpha=.5) +
  xlab("Mean Media Freedom") +
  ylab("Mean Score on KOF Index") +
  theme_bw() +
  ggtitle("General Economic Globalization & Media Freedom, 1960-2011") +
  facet_wrap(~dem)


tradeintroplot<-ggplot(subset(countryavgs, pop>2), aes(x=FHscore, y=openk)) +
  geom_smooth(method=lm, se=FALSE) +
  geom_point(alpha=.5) +
  geom_text(aes(label=country),hjust=.5, vjust=-.2, size=4, alpha=.5) +
  xlab("Mean Media Freedom") +
  ylab("Mean Trade (% of GDP)") +
  theme_bw() +
  ggtitle("Trade Openness & Media Freedom, 1960-2011") + 
  facet_wrap(~dem)
