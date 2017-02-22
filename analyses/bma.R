require(BMA)
require(ggplot2)

# source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

f <- formula(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 +
               spline2 + spline3 + lopenk2 + dopenk2 +
               lfdiinward2 + dfdiinward2 + lfpistock2  +
               dfpistock2 + oil + log(internet+1) + ethfrac + relfrac + onset + warl)
glm.out <- bic.glm(f, data = zvars, glm.family = "binomial", robust=TRUE)
imageplot.bma(glm.out, color=c("grey", "black", "white"), order="probne0")
legend("bottom", legend=c("Positive","Negative", "None"), fill=c("grey", "black", "white"), title="Estimated Effect")


bmastats<-as.data.frame(glm.out$probne0)
bmastats$Variables<-row.names(bmastats)
names(bmastats)<-c("Probability", "Variables")
bmastats$Variables<-as.factor(bmastats$Variables)
bmastats$Variables<-reorder(bmastats$Variables, bmastats$Probability)
bma.plot.fpi<-ggplot(bmastats, aes(x=Variables, y=Probability)) +
  geom_bar(stat="identity") +
  theme_bw() +
  coord_flip() +
  labs(y="Probability of Inclusion")


