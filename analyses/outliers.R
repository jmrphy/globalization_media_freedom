require(car)
source("/Users/justin/Dropbox/gh_projects/globalization_media_freedom/analyses/main_regressions.R")

z.out.controls.glm<-glm(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + lfdiinward2 + dfdiinward2 + oil + internet + ethfrac + relfrac + onset + warl,
                        family=binomial(),
                        data=controls)

#residualPlots(z.out.controls.glm)
#influenceIndexPlot(z.out.controls.glm, vars=c("Cook", "hat"), id.n=5)
removed.cooks.d<-compareCoefs(z.out.controls.glm, update(z.out.controls.glm, subset=-c(4536, 1626,2033,5273,5701)), print=FALSE)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

controls.no.trade.outliers<-controls
controls.no.trade.outliers$lopenk2<-remove_outliers(controls$lopenk2)
controls.no.trade.outliers$dopenk2<-remove_outliers(controls$dopenk2)

z.out.controls.no.trade.outliers<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + lfdiinward2 + dfdiinward2 + oil + internet + ethfrac + relfrac + onset + warl,
                                        model="logit",
                                        robust=TRUE,
                                        data=controls.no.trade.outliers,
                                        cite=F)

