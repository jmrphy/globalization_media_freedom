require(Zelig)
require(splines)
require(ggplot2)


modelvars<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_modelvars.csv")
df<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv")
df$fp<-as.factor(df$fp)

full.raw<-subset(df, select=c("fp", "country", "year", "lpolity2", "dpolity2", "lrgdpch", "drgdpch2", "lopenk", "dopenk2", "lfdiinward", "dfdiinward2", "lfpistock2", "dfpistock", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
full.raw$fp<-as.numeric(full.raw$fp)-1
full.raw.complete<-full.raw[complete.cases(full.raw),] 

controls<-subset(modelvars, select=c("fp", "scode", "year", "lpolity2", "dpolity2", "lrgdpch", "lrgdpch2", "drgdpch","drgdpch2", "lopenk2", "dopenk2", "lfdiinward2", "dfdiinward2", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
modelvars<-subset(modelvars, select=c("fp", "scode", "year", "lpolity2", "dpolity2", "lrgdpch", "lrgdpch2", "drgdpch","drgdpch2", "lopenk", "dopenk", "lopenk2", "dopenk2", "lfdiinward", "lfdiinward2", "dfdiinward","dfdiinward2", "lfpistock", "lfpistock2", "dfpistock", "dfpistock2", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
#modelvars<-modelvars[complete.cases(modelvars),]

spline<-ns(1960:2011, df=3)
spline<-as.data.frame(spline)
names(spline)<-c("spline1", "spline2", "spline3")
spline$year<-1960:2011
modelvars<-merge(modelvars, spline, by="year")
modelvars<-modelvars[with(modelvars, order(scode, year)), ]


df<-subset(df, select=c("fp", "lfp", "scode", "year", "lpolity2", "dpolity2", "lrgdpch", "lrgdpch2", "drgdpch","drgdpch2", "lopenk", "dopenk", "openk2", "lopenk2", "dopenk2", "lfdiinward", "lfdiinward2", "dfdiinward","dfdiinward2", "lfpistock", "lfpistock2", "dfpistock", "dfpistock2", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
#df<-df[complete.cases(df),] 

controls<-merge(controls, spline, by="year")
controls<-controls[with(controls, order(scode, year)), ]


df<-merge(df, spline, by="year")
df<-df[with(df, order(scode, year)), ]

### This is where I left off

### Model 1, Baseline ###
model1vars<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lopenk2", "dopenk2"))
model1vars<-model1vars[complete.cases(model1vars),]
model1<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3,
              model="logit",
              robust=TRUE,
              data=model1vars,
              cite=F)

model1classif<-table(model1$result$fitted.values>.5, model1$result$y)
model1correct<-(model1classif[1,1] + model1classif[2,2])
(model1correct/nrow(model1vars))*100
#summary(model1)

model2vars<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lopenk2", "dopenk2"))
model2vars<-model2vars[complete.cases(model2vars),]
model2<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2,
              model="logit",
              robust=TRUE,
              data=model2vars,
              cite=F)

model2classif<-(table(model2$result$fitted.values>.5, model2$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
(model2correct/nrow(model2vars))*100
summary(model2)

model3vars<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lfdiinward2", "dfdiinward2"))
model3vars<-model3vars[complete.cases(model3vars),]
model3<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lfdiinward2 + dfdiinward2,
              model="logit",
              robust=TRUE,
              data=model3vars,
              cite=F)

model3classif<-(table(model3$result$fitted.values>.5, model3$result$y))
model3correct<-(model3classif[1,1] + model3classif[2,2])
(model3correct/nrow(model3vars))*100
summary(model3)

model4vars<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lfpistock2", "dfpistock2"))
model4vars<-model4vars[complete.cases(model4vars),]
model4<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lfpistock2 + dfpistock2,
              model="logit",
              robust=TRUE,
              data=model4vars,
              cite=F)

model4classif<-(table(model4$result$fitted.values>.5, model4$result$y))
model4correct<-(model4classif[1,1] + model4classif[2,2])
(model4correct/nrow(model4vars))*100
summary(model4)

controls<-controls[complete.cases(controls),]
z.out.controls<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + lfdiinward2 + dfdiinward2 + oil + internet + ethfrac + relfrac + onset + warl,
                      model="logit",
                      robust=TRUE,
                      data=controls,
                      cite=F)

#plot(z.out.controls)



          
iv.corrs<-mean(abs(cor(z.out.controls$result$residuals, controls$warl)),
     abs(cor(z.out.controls$result$residuals, controls$onset)),
     abs(cor(z.out.controls$result$residuals, controls$relfrac)),
     abs(cor(z.out.controls$result$residuals, controls$ethfrac)),
     abs(cor(z.out.controls$result$residuals, controls$internet)),
     abs(cor(z.out.controls$result$residuals, controls$oil)),
     abs(cor(z.out.controls$result$residuals, controls$dfdiinward2)),
     abs(cor(z.out.controls$result$residuals, controls$lfdiinward2)),
     abs(cor(z.out.controls$result$residuals, controls$dopenk2)),
     abs(cor(z.out.controls$result$residuals, controls$spline3)),
     abs(cor(z.out.controls$result$residuals, controls$spline2)),
     abs(cor(z.out.controls$result$residuals, controls$spline1)),
     abs(cor(z.out.controls$result$residuals, controls$lopenk2)),
     abs(cor(z.out.controls$result$residuals, controls$lpolity2)),
     abs(cor(z.out.controls$result$residuals, controls$dpolity2)),
     abs(cor(z.out.controls$result$residuals, controls$lrgdpch2)),
     abs(cor(z.out.controls$result$residuals, controls$drgdpch2))
)

iv.corrs.max<-max(abs(cor(z.out.controls$result$residuals, controls$warl)),
               abs(cor(z.out.controls$result$residuals, controls$onset)),
               abs(cor(z.out.controls$result$residuals, controls$relfrac)),
               abs(cor(z.out.controls$result$residuals, controls$ethfrac)),
               abs(cor(z.out.controls$result$residuals, controls$internet)),
               abs(cor(z.out.controls$result$residuals, controls$oil)),
               abs(cor(z.out.controls$result$residuals, controls$dfdiinward2)),
               abs(cor(z.out.controls$result$residuals, controls$lfdiinward2)),
               abs(cor(z.out.controls$result$residuals, controls$dopenk2)),
               abs(cor(z.out.controls$result$residuals, controls$spline3)),
               abs(cor(z.out.controls$result$residuals, controls$spline2)),
               abs(cor(z.out.controls$result$residuals, controls$spline1)),
               abs(cor(z.out.controls$result$residuals, controls$lopenk2)),
               abs(cor(z.out.controls$result$residuals, controls$lpolity2)),
               abs(cor(z.out.controls$result$residuals, controls$dpolity2)),
               abs(cor(z.out.controls$result$residuals, controls$lrgdpch2)),
               abs(cor(z.out.controls$result$residuals, controls$drgdpch2))
)

    

z.out.controlsclassif<-(table(z.out.controls$result$fitted.values>.5, z.out.controls$result$y))
z.out.controlscorrect<-(z.out.controlsclassif[1,1] + z.out.controlsclassif[2,2])
(z.out.controlscorrect/nrow(z.out.controls))*100
summary(z.out.controls)

z.out.controlsfpi<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lopenk2", "dopenk2", "lfdiinward2", "dfdiinward2", "lfpistock2", "dfpistock2", "oil", "internet", "ethfrac", "relfrac", "onset", "warl"))
z.out.controlsfpi<-z.out.controlsfpi[complete.cases(z.out.controlsfpi),]
z.out.controls.fpi<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + lfdiinward2 + dfdiinward2 + lfpistock2  + dfpistock2 + oil + internet + ethfrac + relfrac + onset + warl,
              model="logit",
              robust=TRUE,
              data=z.out.controlsfpi,
              cite=F)


zvars.trade<-subset(df, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lopenk2", "dopenk2", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
zvars.trade<-zvars.trade[complete.cases(zvars.trade),]
z.out.trade<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + oil + internet + ethfrac + relfrac + onset + warl,
              model="logit",
              robust=TRUE,
              data=zvars.trade,
              cite=F)

zvars<-subset(df, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lopenk2", "dopenk2", "lfdiinward2", "dfdiinward2", "lfpistock2", "dfpistock2", "scode", "year", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
zvars<-zvars[complete.cases(zvars),]
z.out<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + lfdiinward2 + dfdiinward2 + lfpistock2  + dfpistock2 + oil + internet + ethfrac + relfrac + onset + warl,
             model="logit",
             robust=TRUE,
             data=zvars,
             cite=F)


reverse.cause<-subset(df, select=c("lfp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "openk2", "lopenk2", "scode", "year"))
reverse.cause<-reverse.cause[complete.cases(reverse.cause),]
z.out.reverse1<-zelig(openk2 ~ lfp + as.factor(scode) + as.factor(year),
                   model="ls",
                   robust=TRUE,
                   data=reverse.cause,
                   cite=F)

z.out.reverse2<-zelig(openk2 ~ lfp +  lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + as.factor(scode) + as.factor(year),
                      model="ls",
                      robust=TRUE,
                      data=reverse.cause,
                      cite=F)

z.out.reverse3<-zelig(openk2 ~ lfp + lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + lopenk2 + as.factor(scode) + as.factor(year),
                      model="ls",
                      robust=TRUE,
                      data=reverse.cause,
                      cite=F)

