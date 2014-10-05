require(Zelig)

source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

trade.r<-seq(min(df$lopenk),max(df$lopenk), 50) # ~ min to max in sample
x.trade<-setx(z.out, lopenk=trade.r)
s.out<-sim(z.out, x = x.trade)
trade.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=4, ylim=0:1,
        xlab="Trade Level (% GDP)",
        ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out, lopenk=0)
x.hi <- setx(z.out, lopenk=mean(df$lopenk)+sd(df$lopenk))
s.out.trade <- sim(z.out, x = x.lo, x1 = x.hi)

#summary(s.out.trade)
#mean(s.out.trade$qi$fd)
