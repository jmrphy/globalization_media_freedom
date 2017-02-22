require(Zelig)

source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

# From first submission with old zelig
# 
# trade.r<-seq(min(zvars.trade$lopenk2),max(zvars.trade$lopenk2), 1) # ~ min to max in sample
# x.trade<-setx(z5, lopenk2=trade.r)
# s.out<-sim(z5, x = x.trade)
# trade.plot<-plot(s.out, ci=c(90,95,99), qi="pv", leg=4, ylim=0:1,
#    ylab="Expected Probability of Media Freedom")

# x.lo <- setx(z.out.trade, lopenk2=0)
# x.hi <- setx(z.out.trade, lopenk2=mean(zvars.trade$lopenk2))
# s.out.trade <- sim(z.out.trade, x = x.lo, x1 = x.hi)

# For now submission with new zelig

z.out.trade$setrange(lopenk2=seq(min(zvars.trade$lopenk2):max(zvars.trade$lopenk2)))
z.out.trade$sim()

ci.plot(z.out.trade,
        ci=c(90,95,99),
        leg=4, ylim=0:1,
        xlab="Log of Trade Level (% GDP)",
        ylab="Expected Probability of Media Freedom")


# Calucate QIs for country moving from min to mean of trade
z.out.trade$setx(lopenk2=min(zvars.trade$lopenk2))
z.out.trade$setx1(lopenk2=mean(zvars.trade$lopenk2))
z.out.trade$sim()

pv.mintrade <- z.out.trade$getqi(qi="pv", xvalue="x")
pv.avgtrade <- z.out.trade$getqi(qi="pv", xvalue="x1")


z.out.trade$setx(dopenk2=0)
z.out.trade$setx1(dopenk2=mean(zvars.trade$dopenk2)+sd(zvars.trade$dopenk2))
z.out.trade$sim()

pv.trade.still <- z.out.trade$getqi(qi="pv", xvalue="x")
pv.trade.up <- z.out.trade$getqi(qi="pv", xvalue="x1")
pv.fd <- z.out.trade$getqi(qi="fd", xvalue="x1")

# Change in Trade plot

# From first submission using old zelig
# trade.change.r<-seq(min(zvars.trade$dopenk2),max(zvars.trade$dopenk2), .5) # ~ min to max in sample
# x.trade.change<-setx(z.out.trade, dopenk2=trade.change.r)
# s.out<-sim(z.out.trade, x = x.trade.change)
# trade.change.plot<-plot.ci(s.out, ci=c(90,95,99), qi="ev", leg=4, ylim=0:1,
#                     xlab="Change in Log of Trade Level (% GDP)",
#                     ylab="Expected Probability of Media Freedom")

# x.lo <- setx(z.out.trade, dopenk2=0)
# x.hi <- setx(z.out.trade, dopenk2=mean(zvars.trade$dopenk2)+sd(zvars.trade$dopenk2))
# s.out.trade.change <- sim(z.out.trade, x = x.lo, x1 = x.hi)


z.out.trade$setrange(dopenk2=seq(min(zvars.trade$dopenk2):max(zvars.trade$dopenk2)))
z.out.trade$sim()

ci.plot(z.out.trade,
        ci=c(90,95,99),
        leg=4,
        ylim=0:1,
        xlab="Change in Log of Trade Level (% GDP)",
        ylab="Expected Probability of Media Freedom")