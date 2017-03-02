require(Matching)
require(rbounds)

# source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

setwd("~/Dropbox/gh_projects/globalization_media_freedom")

attach(zvars)
md <- as.data.frame(cbind(fp, lpolity2, lrgdpch2, spline1, dpolity2, drgdpch2, lfdiinward2, dfdiinward2,
                          lfpistock2, dopenk2,
                          dfpistock2,
                          spline2, spline3, lopenk2, oil, internet,
                          ethfrac, relfrac, onset, warl))
detach(zvars)

md$treatment<-ifelse(md$lopenk2>mean(md$lopenk2, na.rm=TRUE), TRUE, FALSE)

md <- na.omit(md)

ps  <- glm(treatment ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 +
             spline2 + spline3 + oil + internet + dopenk2 + lfdiinward2 + dfdiinward2 + lfpistock2 +
             dfpistock2 +
             ethfrac + relfrac + onset + warl,
           family = binomial, data = md)

X <- cbind(ps$fitted, md$lpolity2, md$lrgdpch2, md$dpolity2, md$drgdpch2, md$oil, md$internet,
           md$ethfrac, md$relfrac, md$onset, md$warl, md$dopenk2, md$lfdiinward2, md$dfdiinward2, md$lfpistock2,
             md$dfpistock2)

BalanceMatrix <- cbind(md$lpolity2, md$lrgdpch2, md$spline1, md$drgdpch2, md$dpolity2,
                       md$spline2, md$spline3, md$dopenk2,
                       md$oil, md$internet, md$lfdiinward2, md$dfdiinward2, md$lfpistock2,
                         md$dfpistock2,
                       md$ethfrac, md$relfrac, md$onset, md$warl)

treatment <- md$treatment

Y <- md$fp

# genout <- GenMatch(Tr=treatment, X=X, BalanceMatrix=BalanceMatrix, estimand="ATT", 
#                   M=1, pop.size=1000, max.generations=100, wait.generations=5)

# save(genout, file="analyses/matching/saved_matches/genout.RData")
load("analyses/matching/saved_matches/genout.RData")

matched_trade <- Match(Y=Y, Tr=treatment, X=X, estimand="ATT", 
                   M=1, BiasAdjust=FALSE, Weight.matrix=genout)


# summary(matched_trade)
# matched_trade$est

# binarysens(matched_trade, Gamma = 3, GammaInc = 0.01)

# Matching for R&R

"match on levels of trade openness, what is predicted level of media repression?"

md<-subset(zvars, select=c("fp", "lopenk2", "lpolity2"))
md<-md[complete.cases(md),]
md$fp <- as.factor(md$fp)

set.seed(333)
md$treatment<-ifelse(md$lopenk2>quantile(md$lopenk2, .5), 1, 0)
#md$treatment<-ifelse(md$fp==1, TRUE, FALSE)

m.out <- matchit(treatment ~ lpolity2,
                 data = md, method = "exact")

#summary(m.out)
#plot(m.out)

m.data <- match.data(m.out)

z.out <- zelig(fp ~ treatment, model = "logit", data = m.data)

x.out <- setx(z.out, treatment=0)
x1.out <- setx(z.out, treatment=1)
s.out <- sim(z.out, x = x.out, x1 = x1.out)
summary(s.out)
plot(s.out)
