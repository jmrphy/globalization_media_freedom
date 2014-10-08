require(Zelig)

source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

fdi.r <- min(df$lfdiinflow):max(df$lfdiinflow)
x.fdi<-setx(z.out, lfdiinflow=fdi.r)
s.out<-sim(z.out, x = x.fdi)
fdi.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=3, ylim=0:1,
                  xlab="FDI Inflow (% GDP)",
                  ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out, lfdiinflow=0)
x.hi <- setx(z.out, lfdiinflow=mean(df$lfdiinflow)+sd(df$lfdiinflow))
s.out.fdi <- sim(z.out, x = x.lo, x1 = x.hi)
summary(s.out.fdi)
mean(s.out.fdi$qi$fd)

fdi.stock.r <- min(df$lfdiinward):max(df$lfdiinward)
x.fdi.stock<-setx(z.out, lfdiinward=fdi.stock.r)
s.out<-sim(z.out, x = x.fdi.stock)
fdi.stock.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=3, ylim=0:1,
                  xlab="FDI Stock (% GDP)",
                  ylab="Expected Probability of Media Freedom")

x.lo <- setx(z.out, lfdiinward=0)
x.hi <- setx(z.out, lfdiinward=mean(df$lfdiinward)+sd(df$lfdiinward))
s.out.fdi.stock <- sim(z.out, x = x.lo, x1 = x.hi)
summary(s.out.fdi.stock)
mean(s.out.fdi.stock$qi$fd)

