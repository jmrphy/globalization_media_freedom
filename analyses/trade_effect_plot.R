require(Zelig)

source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

trade.r<-seq(min(zvars.trade$lopenk2),max(zvars.trade$lopenk2), 1) # ~ min to max in sample
x.trade<-setx(z.out.trade$zelig, lopenk2=3)
s.out<-sim(z.out.trade, x = x.trade)
trade.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=4, ylim=0:1,
        xlab="Log of Trade Level (% GDP)",
        ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out.trade, lopenk2=0)
x.hi <- setx(z.out.trade, lopenk2=mean(zvars.trade$lopenk2))
s.out.trade <- sim(z.out.trade, x = x.lo, x1 = x.hi)


trade.change.r<-seq(min(zvars.trade$dopenk2),max(zvars.trade$dopenk2), .5) # ~ min to max in sample
x.trade.change<-setx(z.out.trade, dopenk2=trade.change.r)
s.out<-sim(z.out.trade, x = x.trade.change)
trade.change.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=4, ylim=0:1,
                    xlab="Change in Log of Trade Level (% GDP)",
                    ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out.trade, dopenk2=0)
x.hi <- setx(z.out.trade, dopenk2=mean(zvars.trade$dopenk2)+sd(zvars.trade$dopenk2))
s.out.trade.change <- sim(z.out.trade, x = x.lo, x1 = x.hi)

#summary(s.out.trade)
#mean(s.out.trade.change$qi$fd)
