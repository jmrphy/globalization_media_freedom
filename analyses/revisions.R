require(mediation)
require(ZeligChoice)


# Guarding against post-treatment; are countries like China skewing; request to interact open*dem and wealthy dems as exception
zvars.trade$dem <- ifelse(zvars.trade$polity2>quantile(zvars.trade$polity2, .5), "Democracy", "Non-Democracy")
zvars.trade$rich <- ifelse(zvars.trade$lrgdpch2>quantile(zvars.trade$lrgdpch2, .5), "Wealthy", "Non-Wealthy")

zvars.trade$dem <- cut(zvars.trade$polity2, 3)

by.dem.model <- zelig(fp ~ lopenk2 + as.factor(scode) + as.factor(year),
                 model = "logit",
                 by="dem",
  data=zvars.trade)

interact.model <- zelig(fp ~ lopenk2*lpolity2,
                 model = "logit",
                 data=zvars.trade)

# Consider full ordinal version of the DV

#zvars.trade$mediascore<-5 - zvars.trade$mediascore #restores to Whitten-Woodrings coding

require(plm)
zvars.trade.mnom<-pdata.frame(zvars.trade, index=c("scode", "year"))

zvars.trade.mnom$lmedia<-lag(zvars.trade.mnom$mediascore)
zvars.trade.mnom$l2media<-lag(zvars.trade.mnom$mediascore,2)
zvars.trade.mnom$l3media<-lag(zvars.trade.mnom$mediascore,3)
zvars.trade.mnom$l4media<-lag(zvars.trade.mnom$mediascore,4)
zvars.trade.mnom$l5media<-lag(zvars.trade.mnom$mediascore, 5)
zvars.trade.mnom$l6media<-lag(zvars.trade.mnom$mediascore,6)
zvars.trade.mnom$l7media<-lag(zvars.trade.mnom$mediascore, 7)
zvars.trade.mnom$l8media<-lag(zvars.trade.mnom$mediascore, 8,)
zvars.trade.mnom$l9media<-lag(zvars.trade.mnom$mediascore,9)
zvars.trade.mnom$l10media<-lag(zvars.trade.mnom$mediascore,10)
zvars.trade.mnom$dmedia<-diff(zvars.trade.mnom$mediascore) # Positive is repression
zvars.trade.mnom$mediarepress <- ifelse(zvars.trade.mnom$dmedia>0, 1, 0)

zvars.trade.mnom$media12 <- ifelse(zvars.trade.mnom$dmedia>0 & zvars.trade.mnom$mediascore==2, 1, 0)

zvars.trade.mnom$media23 <- ifelse(zvars.trade.mnom$dmedia>0 & zvars.trade.mnom$mediascore==3, 1, 0)

zvars.trade.mnom$media34 <- ifelse(zvars.trade.mnom$dmedia>0 & zvars.trade.mnom$mediascore==4, 1, 0)

zvars.trade.mnom <- as.data.frame(zvars.trade.mnom)

repress.model <- zelig(dmedia ~ lmedia + dopenk2 + lopenk2 + scode + year,
                      model = "ls",
                      data=zvars.trade.mnom)
summary(repress.model)

zvars.trade$mediascore<-as.factor(zvars.trade$mediascore)

mnom.model <- zelig(mediascore ~ lopenk2 + scode + year,
                model = "mlogit", data = zvars.trade)
summary(mnom.model)

x.lo.trade <- setx(mnom.model, lopenk2 = quantile(zvars.trade$lopenk, .2))
x.hi.trade <- setx(mnom.model, lopenk2 = quantile(zvars.trade$lopenk, .8))

s.out.mlogit <- sim(mnom.model, x = x.lo.trade, x1 = x.hi.trade)
summary(s.out.mlogit)
plot(s.out.mlogit)

First, whereas trade-closed countries are, as conventional wisdom might expect, substantially more likely to be directly controlled
than clearly free, this pattern does not hold for trade-open countries. Trade-open countries are just as likely to be clearly controlled (.29) as clearly free (.28).

High-trade slightly less likely to be directly controlled than low-trade, but also slightly more likely to be uncompetive arena.
On the other end of the scale, low-trade substantially less likely to be clearly free than high-trade, but also substantially more
likely to be compromised-competitive.

As the authors of the data indicate, the substantive difference between categories is greatest moving
between uncompetitive (3) and compromised-competitive (2). Therefore, if we take the difference between these two levels
as the sharpest dividing line between media freedom and media repression, then it should be the clearest indicator of any
effect trade openness might have on media freedom. It is between these two levels that the
relationship between trade and media freedom takes the counter-intuitive form predicted by the hypothesis: the most
clearly differentiated and puzzling difference in the probability of media types within one type of trade-openness is
the difference in probability between a trade-open country being plainly uncompetitive (.26) or competitive-compromised (.17).

At the same time, for trade-closed countries, the probability of being in 2 or 3 is the same. 

This suggests that trade liberalizations cause moves from 2-3. Trade-closed countries do not move between them. 

Trade-liberalizing countries are likely to be compromised-competive.

If compromising the media sustains trade liberalization and democracy and growth follow, we might expect media to eventually become freeny
The problem with this expectation is that it does not appear to occurr in the long-run. Consider Argentina and Mexico. Both countries,
liberalized trade when they were compromised-competitive (2). Mexico would eventually see media repression worsen, while Argentina would
stay in that category until the end of the sample in 2011. In each case, when considered with the Freedom House Freedom of the Press score,
these cases of democratic trade liberalization were clearly supported by compromised media freedom which, despite some temporary observed
increases likely due to the effects of democratization and economic and technological environment, were both characterized by modest but measurable decreases
in media freedom, within the category of "compromised",by the end of the sample in 2011.


rade-liberalizing
either repress or transition to complete freedom. If they are democracies, they will 


Trade-open countries
are more likely to be uncompetitve than competitive-compromised  because trade liberalization gives incentives 

are
trade-closed countries are substantially
more likely to be competitive but compromised (.23) than trade-open countries (.17)

If trade-open countries are more likely to be clearly free 

# Conditional logit to check fixed effects

# High trade makes very free media more likely to be compromised

res.clogit <- lm(media12 ~ lopenk2 + lpolity2 + as.factor(scode),
                 data=subset(zvars.trade.mnom,
                             lmedia==1))
summ.clogit <- summary(res.clogit)
summ.clogit

res.clogit <- lm(media23 ~ lopenk2 + lpolity2 + as.factor(scode),
                 data=subset(zvars.trade.mnom, lmedia<=2))
summ.clogit <- summary(res.clogit)
summ.clogit

res.clogit <- lm(media34 ~ lopenk2 + lpolity2 + as.factor(scode),
                 data=subset(zvars.trade.mnom, lmedia<=3))
summ.clogit <- summary(res.clogit)
summ.clogit


res.clogit <- clogit(as.numeric(media34) ~ lopenk2 +  strata(scode),
                                data=subset(zvars.trade.mnom,
                                            lmedia==3))
summ.clogit <- summary(res.clogit)
summ.clogit


res.clogit <- clogit(as.numeric(media23) ~
                       lopenk2 + strata(scode),
                 data=subset(zvars.trade.mnom,
                             lmedia==2))
summ.clogit <- summary(res.clogit)
summ.clogit

res.clogit <- clogit(as.numeric(media12) ~  lopenk2 + strata(scode),
                     data=subset(zvars.trade.mnom,
                                 lmedia==1))
summ.clogit <- summary(res.clogit)
summ.clogit


# openness is post-treatment of democracy
# so threat is  democracy --> openness --> fp
# or openness --> democracy --> fp

# Outcome = fp ~ openk2
# Meditator = lpolity2
model.m <- lm(polity2 ~ lpolity2 + lrgdpch2 + openk2 + lfp + oil + internet + ethfrac + relfrac + onset + warl,
                 data=zvars.trade)
 
model.y <- glm(fp ~ polity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + openk2 + dopenk2 + oil + internet + ethfrac + relfrac + onset + warl,
               family=binomial(),
               data=zvars.trade)

out.1 <- mediate(model.m, model.y, sims = 1000, treat = "openk2", mediator = "polity2")

summary(out.1)

# Mediation analysis

In the previous sections, Democracy has been included on the right-hand side of the regression equations in order to control for
its effects on media freedom. But if Trade affects Democracy, then including Democracy as a covariate would bias the estimates.
To consider this possibility, I conducted a mediation analysis

For the mediation analysis, non-lagged versions of democracy and openness were used.
Lagged FP was used on the right-hand side of the mediation model, but results do not change if it is removed.

Democracy does not appreciably mediate the effect of openness on media freedom.
The average causal mediation effect is .002 and statistically insignificant (p=.14).
The average direct effect of openness on media freedom is negative (-.05) and statistically significant (p=0).


# Outcome = fp ~ democracy
# Meditator = openk2
model.m <- lm(openk2 ~ polity2 + lrgdpch2 + lopenk2 + lfp + oil + internet + ethfrac + relfrac + onset + warl,
              data=zvars.trade)

model.y <- glm(fp ~ polity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + openk2 + dopenk2 + oil + internet + ethfrac + relfrac + onset + warl,
               family=binomial(),
               data=zvars.trade)

out.1 <- mediate(model.m, model.y, sims = 1000, treat = "polity2", mediator = "openk2")

summary(out.1)


library(dplyr)

modelvars <- modelvars %>%
  group_by(scode) %>%
  filter(n() >= 10)


