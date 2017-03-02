require(Zelig)
require(splines)
require(ggplot2)


modelvars<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_modelvars.csv")
df<-read.csv("~/Dropbox/gh_projects/globalization_media_freedom/data/out_df.csv")
df$fp<-as.factor(df$fp)

controls<-subset(modelvars, select=c("fp", "scode", "year", "lpolity2", "dpolity2", "lrgdpch", "lrgdpch2", "drgdpch","drgdpch2", "lopenk2", "dopenk2", "lfdiinward2", "dfdiinward2", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
modelvars<-subset(modelvars, select=c("fp", "scode", "year", "lpolity2", "dpolity2", "lrgdpch", "lrgdpch2", "drgdpch","drgdpch2", "lopenk", "dopenk", "lopenk2", "dopenk2", "lfdiinward", "lfdiinward2", "dfdiinward","dfdiinward2", "lfpistock", "lfpistock2", "dfpistock", "dfpistock2", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
#modelvars<-modelvars[complete.cases(modelvars),]

spline<-ns(1960:2011, df=3)
spline<-as.data.frame(spline)
names(spline)<-c("spline1", "spline2", "spline3")
spline$year<-1960:2011
modelvars<-merge(modelvars, spline, by="year")
modelvars<-modelvars[with(modelvars, order(scode, year)), ]


df<-subset(df, select=c("fp", "lfp", "fp.d", "mediascore", "scode", "year", "polity2", "lpolity2", "dpolity2", "lrgdpch", "lrgdpch2", "drgdpch","drgdpch2", "lopenk", "dopenk", "openk2", "lopenk2", "dopenk2", "lfdiinward", "lfdiinward2", "dfdiinward","dfdiinward2", "lfpistock", "lfpistock2", "dfpistock", "dfpistock2", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
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
              data=model1vars,
              cite=F)

model1classif<-table(unlist(model1$getfitted())>.5, model1vars$fp)
model1correct<-(model1classif[1,1] + model1classif[2,2])
(model1correct/nrow(model1vars))*100
#summary(model1)

model2vars<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lopenk2", "dopenk2"))
model2vars<-model2vars[complete.cases(model2vars),]
model2<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2,
              model="logit",
              data=model2vars,
              cite=F)

model2classif<-table(unlist(model2$getfitted())>.5, model2vars$fp)
model2correct<-(model2classif[1,1] + model2classif[2,2])
(model2correct/nrow(model2vars))*100
summary(model2)

model3vars<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lfdiinward2", "dfdiinward2"))
model3vars<-model3vars[complete.cases(model3vars),]
model3<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lfdiinward2 + dfdiinward2,
              model="logit",
              data=model3vars,
              cite=F)

model3classif<-table(unlist(model3$getfitted())>.5, model3vars$fp)
model3correct<-(model3classif[1,1] + model3classif[2,2])
(model3correct/nrow(model3vars))*100
summary(model3)

model4vars<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lfpistock2", "dfpistock2"))
model4vars<-model4vars[complete.cases(model4vars),]
model4<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lfpistock2 + dfpistock2,
              model="logit",
              data=model4vars,
              cite=F)

model4classif<-table(unlist(model4$getfitted())>.5, model4vars$fp)
model4correct<-(model4classif[1,1] + model4classif[2,2])
(model4correct/nrow(model4vars))*100
summary(model4)

controls<-controls[complete.cases(controls),]
z.out.controls<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + lfdiinward2 + dfdiinward2 + oil + internet + ethfrac + relfrac + onset + warl,
                      model="logit",
                      data=controls,
                      cite=F)

#plot(z.out.controls)

iv.corrs<-mean(abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$warl)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$onset)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$relfrac)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$ethfrac)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$internet)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$oil)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$dfdiinward2)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$lfdiinward2)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$dopenk2)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$spline3)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$spline2)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$spline1)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$lopenk2)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$lpolity2)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$dpolity2)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$lrgdpch2)),
     abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$drgdpch2))
)

iv.corrs.max<-max(abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$warl)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$onset)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$relfrac)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$ethfrac)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$internet)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$oil)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$dfdiinward2)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$lfdiinward2)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$dopenk2)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$spline3)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$spline2)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$spline1)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$lopenk2)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$lpolity2)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$dpolity2)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$lrgdpch2)),
               abs(cor(unlist(z.out.controls$getfitted()) - controls$fp, controls$drgdpch2))
)

    

z.out.controlsclassif<-(table(unlist(z.out.controls$getfitted())>.5, controls$fp))
z.out.controlscorrect<-(z.out.controlsclassif[1,1] + z.out.controlsclassif[2,2])
(z.out.controlscorrect/nrow(controls))*100
summary(z.out.controls)

z.out.controlsfpi<-subset(modelvars, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lopenk2", "dopenk2", "lfdiinward2", "dfdiinward2", "lfpistock2", "dfpistock2", "oil", "internet", "ethfrac", "relfrac", "onset", "warl"))
z.out.controlsfpi<-z.out.controlsfpi[complete.cases(z.out.controlsfpi),]
z.out.controls.fpi<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + lfdiinward2 + dfdiinward2 + lfpistock2  + dfpistock2 + oil + internet + ethfrac + relfrac + onset + warl,
              model="logit",
              data=z.out.controlsfpi,
              cite=F)


zvars.trade<-subset(df, select=c("scode", "year", "fp", "mediascore", "lfp", "fp.d", "polity2", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "openk2", "lopenk2", "dopenk2", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
zvars.trade<-zvars.trade[complete.cases(zvars.trade),]

# In first submission with old zelig
# z.out.trade<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + oil + internet + ethfrac + relfrac + onset + warl,
#               model="logit",
#               data=zvars.trade)
#trade.r<-seq(min(zvars.trade$lopenk2),max(zvars.trade$lopenk2), 1) # ~ min to max in sample

# For resubmission with new zelig
z.out.trade <- zlogit$new()
z.out.trade$zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + oil + internet + ethfrac + relfrac + warl,
      data=zvars.trade)

zvars<-subset(df, select=c("fp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "spline1", "spline2", "spline3", "lopenk2", "dopenk2", "lfdiinward2", "dfdiinward2", "lfpistock2", "dfpistock2", "scode", "year", "oil", "internet", "ethfrac", "relfrac", "warl", "onset"))
zvars<-zvars[complete.cases(zvars),]
z.out<-zelig(fp ~ lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + spline1 + spline2 + spline3 + lopenk2 + dopenk2 + lfdiinward2 + dfdiinward2 + lfpistock2  + dfpistock2 + oil + internet + ethfrac + relfrac + onset + warl,
             model="logit",
             data=zvars,
             cite=F)


reverse.cause<-subset(df, select=c("lfp", "lpolity2", "dpolity2", "lrgdpch2", "drgdpch2", "openk2", "lopenk2", "scode", "year"))
reverse.cause<-reverse.cause[complete.cases(reverse.cause),]
z.out.reverse1<-zelig(openk2 ~ lfp + as.factor(scode) + as.factor(year),
                   model="ls",
                   data=reverse.cause,
                   cite=F)

z.out.reverse2<-zelig(openk2 ~ lfp +  lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + as.factor(scode) + as.factor(year),
                      model="ls",
                      data=reverse.cause,
                      cite=F)

z.out.reverse3<-zelig(openk2 ~ lfp + lpolity2 + dpolity2 + lrgdpch2 + drgdpch2 + lopenk2 + as.factor(scode) + as.factor(year),
                      model="ls",
                      data=reverse.cause,
                      cite=F)

