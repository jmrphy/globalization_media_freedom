require(Zelig)
require(rms)

modelvars<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_modelvars.csv")
df<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv")
df$fp<-as.factor(df$fp)

modelvars$spline<-rcs(as.numeric(modelvars$year1),3)
modelvars<-subset(modelvars, select=c("fp", "scode", "year", "year1", "year2", "year3", "lpolity2",  "ldpolity2", "lrgdpch", "lgrgdpch", "interp", "lopenk", 
  "ldopenk", "lfdiinward", "lfdiinflow", "lfpistock", "lfpi", "spline", "lpolglob", "ldpolglob", "leconglob", "ldeconglob", "loverallglob", "linfoglob", "ldinfoglob", "ldoverallglob"))
modelvars<-modelvars[complete.cases(modelvars),]

df$spline<-rcs(as.numeric(df$year1),3)
df<-subset(df, select=c("fp", "scode", "year", "year1", "year2", "year3", "lpolity2",  "ldpolity2", "lrgdpch", "lgrgdpch", "interp", "lopenk", 
                                      "ldopenk", "lfdiinward", "lfdiinflow", "lfpistock", "lfpi", "spline", "lpolglob", "ldpolglob", "leconglob", "ldeconglob", "loverallglob", "linfoglob", "ldinfoglob", "ldoverallglob"))
df<-df[complete.cases(df),]


### Model 1, Baseline ###
model1<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline,
              model="logit",
              robust=TRUE,
              data=modelvars,
              cite=F)

model1classif<-table(model1$result$fitted.values>.5, model1$result$y)
model1correct<-(model1classif[1,1] + model1classif[2,2])
model1correct

model2<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lopenk + ldopenk + lfdiinward + lfdiinflow + lfpistock  + lfpi,
                 model="logit",
                 robust=TRUE,
                 data=modelvars,
                 cite=F)

model2classif<-(table(model2$result$fitted.values>.5, model2$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
model2correct

model3<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lopenk + ldopenk + lfdiinward + lfdiinflow + lfpistock  + lfpi + loverallglob + ldoverallglob,
                 model="logit",
                 robust=TRUE,
                 data=modelvars,
                 cite=F)

model3classif<-(table(model3$result$fitted.values>.5, model3$result$y))
model3correct<-(model3classif[1,1] + model3classif[2,2])
model3correct

z.out<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + year1 + year2 +
                        lopenk + ldopenk + lfdiinward + lfdiinflow + lfpistock  + lfpi + loverallglob +
                        ldoverallglob,
              model="logit",
              robust=TRUE,
              data=df,
              cite=F)

z.out.classif<-(table(z.out$result$fitted.values>.5, z.out$result$y))
zcorrect<-(z.out.classif[1,1] + z.out.classif[2,2])
zcorrect