require(Zelig)

source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

fdi.r <- min(zvars$dfdiinward2):max(zvars$dfdiinward2)
x.fdi<-setx(z.out, dfdiinward2=fdi.r)
s.out<-sim(z.out, x = x.fdi)
fdi.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=3, ylim=0:1,
                  xlab="FDI Inflow (% GDP)",
                  ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out, dfdiinward2=0)
x.hi <- setx(z.out, dfdiinward2=mean(zvars$dfdiinward2)+sd(zvars$dfdiinward2))
s.out.fdi <- sim(z.out, x = x.lo, x1 = x.hi)
summary(s.out.fdi)
mean(s.out.fdi$qi$fd)

fdi.stock.r <- min(zvars$lfdiinward2):max(zvars$lfdiinward2)
x.fdi.stock<-setx(z.out, lfdiinward2=fdi.stock.r)
s.out<-sim(z.out, x = x.fdi.stock)
fdi.stock.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=3, ylim=0:1,
                  xlab="FDI Stock (% GDP)",
                  ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out, lfdiinward2=0)
x.hi <- setx(z.out, lfdiinward2=mean(zvars$lfdiinward2)+sd(zvars$lfdiinward2))
s.out.fdi.stock <- sim(z.out, x = x.lo, x1 = x.hi)
summary(s.out.fdi.stock)
mean(s.out.fdi.stock$qi$fd)

