require(Zelig)

source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

fpi.r <- min(df$lfpistock):max(df$lfpistock)
x.fpi<-setx(z.out, lfpistock=fpi.r)
s.out<-sim(z.out, x = x.fpi)
fpi.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=4, ylim=c(0,1), xlim=c(0,400),
                  xlab="FPI Stock (% GDP)",
                  ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out, lfpistock=0)
x.hi <- setx(z.out, lfpistock=mean(df$lfpistock)+sd(df$lfpistock))
s.out.fpi <- sim(z.out, x = x.lo, x1 = x.hi)
mean(s.out.fpi$qi$fd)