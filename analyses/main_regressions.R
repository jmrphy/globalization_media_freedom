require(Zelig)
require(splines)

modelvars<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_modelvars.csv")
df<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv")
df$fp<-as.factor(df$fp)


modelvars<-subset(modelvars, select=c("fp", "scode", "year", "lpolity2", "ldpolity2", "lrgdpch", "lgrgdpch", "lopenk", "ldopenk", "lfdiinward", "lfdiinflow", "lfpistock", "lfpi"))
modelvars<-modelvars[complete.cases(modelvars),]

spline<-ns(1972:2003, df=3)
spline<-as.data.frame(spline)
names(spline)<-c("spline1", "spline2", "spline3")
spline$year<-1972:2003
modelvars<-merge(modelvars, spline, by="year")
modelvars<-modelvars[with(modelvars, order(scode, year)), ]

df<-subset(df, select=c("fp", "scode", "year", "lpolity2", "ldpolity2", "lrgdpch", "lgrgdpch", "lopenk", "ldopenk", "lfdiinward", "lfdiinflow", "lfpistock", "lfpi"))
df<-df[complete.cases(df),]

df<-merge(df, spline, by="year")
df<-df[with(df, order(scode, year)), ]


### Model 1, Baseline ###
model1<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + spline1 + spline2 + spline3,
              model="logit",
              robust=TRUE,
              data=modelvars,
              cite=F)

model1classif<-table(model1$result$fitted.values>.5, model1$result$y)
model1correct<-(model1classif[1,1] + model1classif[2,2])
(model1correct/4052)*100
#summary(model1)

model2<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + spline1 + spline2 + spline3 + lopenk + ldopenk + lfdiinward + lfdiinflow + lfpistock  + lfpi,
                 model="logit",
                 robust=TRUE,
                 data=modelvars,
                 cite=F)

model2classif<-(table(model2$result$fitted.values>.5, model2$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
(model2correct/4052)*100
summary(model2)


z.out<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + spline1 + spline2 + spline3 + lopenk + ldopenk + lfdiinward + lfdiinflow + lfpistock  + lfpi,
              model="logit",
              robust=TRUE,
              data=df,
              cite=F)

z.out.classif<-(table(z.out$result$fitted.values>.5, z.out$result$y))
zcorrect<-(z.out.classif[1,1] + z.out.classif[2,2])
zcorrect