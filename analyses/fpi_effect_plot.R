require(Zelig)

source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

fpi.r <- seq(min(zvars$lfpistock2), max(zvars$lfpistock2),1)
x.fpi<-setx(z.out, lfpistock2=fpi.r)
s.out<-sim(z.out, x = x.fpi)
fpi.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=1, ylim=c(0,1),
                  xlab="Log of FPI Stock (% GDP)",
                  ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out, lfpistock2=0)
x.hi <- setx(z.out, lfpistock2=mean(zvars$lfpistock2))
s.out.fpi <- sim(z.out, x = x.lo, x1 = x.hi)
mean(s.out.fpi$qi$fd)
summary(s.out.fpi)

# Changes

fpi.r.change <- seq(min(zvars$dfpistock2), max(zvars$dfpistock2),.5)
x.fpi.change<-setx(z.out, dfpistock2=fpi.r.change)
s.out<-sim(z.out, x = x.fpi.change)
fpi.plot.change<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=1, ylim=c(0,1),
                  xlab="Change in log of FPI Stock (% GDP)",
                  ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out, dfpistock2=0)
x.hi <- setx(z.out, dfpistock2=mean(zvars$dfpistock2))
s.out.fpi.change <- sim(z.out, x = x.lo, x1 = x.hi)
mean(s.out.fpi.change$qi$fd)
summary(s.out.fpi)
