### Model 2, trade and kof
model2<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lopenk + ldopenk + loverallglob + ldoverallglob,
              model="logit",
              robust=TRUE,
              data=modelvars,
              cite=F)

model2classif<-prop.table(table(model2$result$fitted.values>.5, model2$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
model2correct

### Model 3, fdi and kof
model3<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lfdiinward + lfdiinflow + loverallglob + ldoverallglob,
              model="logit",
              robust=TRUE,
              data=modelvars,
              cite=F)

model3classif<-prop.table(table(model3$result$fitted.values>.5, model3$result$y))
model3correct<-(model3classif[1,1] + model3classif[2,2])
model3correct

### Model 4, fpi and kof
model4<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lfpistock  + lfpi + loverallglob + ldoverallglob,
              model="logit",
              robust=TRUE,
              data=modelvars,
              cite=F)

model4classif<-prop.table(table(model4$result$fitted.values>.5, model4$result$y))
model4correct<-(model4classif[1,1] + model4classif[2,2])
model4correct

model5<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lopenk + ldopenk + lfdiinward + lfdiinflow + lfpistock  + lfpi + loverallglob + ldoverallglob,
              model="logit",
              robust=TRUE,
              data=modelvars,
              cite=F)

model5classif<-prop.table(table(model5$result$fitted.values>.5, model5$result$y))
model5correct<-(model5classif[1,1] + model5classif[2,2])
model5correct

for(p in seq(.35,.9,.05)){
  t1=table(model5$result$fitted.values>p, model5$result$y)
  cat(p,(t1[2,1]+t1[1,2])/sum(t1),"\n")
}

### Model , IPE levels and changes ###

model2<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + leconglob + ldeconglob,
              model="logit",
              robust=TRUE,
              data=modelvars,
              cite=F)

model2classif<-prop.table(table(model2$result$fitted.values>.5, model2$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
model2correct

model2vars<-subset(modelvars, select=c("fp", "interp", "lpolity2", "ldpolity2", "lrgdpch", "lgrgdpch", "lopenk", "lfdiinward", "lfpistock", "spline", "ldeconglob"))
model2vars<-model2vars[complete.cases(model2vars),]
model2<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lopenk + lfdiinward + lfpistock + ldeconglob,
              model="logit",
              robust=TRUE,
              data=model2vars,
              cite=F)

model2classif<-prop.table(table(model2$result$fitted.values>.5, model2$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
model2correct



model2vars<-subset(modelvars, select=c("fp", "interp", "lpolity2", "ldpolity2", "lrgdpch", "lgrgdpch", "lfdiinward", "lfdiinflow", "spline", "leconglob", "ldeconglob"))
model2vars<-model2vars[complete.cases(model2vars),]
model2<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lfdiinward + lfdiinflow + leconglob + ldeconglob,
              model="logit",
              robust=TRUE,
              data=model2vars,
              cite=F)

model2classif<-prop.table(table(model2$result$fitted.values>.5, model2$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
model2correct

model2vars<-subset(modelvars, select=c("fp", "interp", "lpolity2", "ldpolity2", "lrgdpch", "lgrgdpch", "lfpistock", "lfpi", "spline", "leconglob", "ldeconglob"))
model2vars<-model2vars[complete.cases(model2vars),]
model2<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lfpistock + lfpi + leconglob + ldeconglob,
              model="logit",
              robust=TRUE,
              data=model2vars,
              cite=F)

model2classif<-prop.table(table(model2$result$fitted.values>.5, model2$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
model2correct

### Model3, considering  multicollinearity of neoliberalism ###

model3vars<-subset(modelvars, select=c("fp", "interp", "lpolity2", "ldpolity2", "lrgdpch", "lgrgdpch", "lopenk", "ldopenk", "lfdiinflow", "lfdiinward", "lfpi", "lfpistock", "spline", "ldeconglob", "leconglob"))
model3vars<-model3vars[complete.cases(model3vars),]
model3<-zelig(fp ~ lpolity2 + ldpolity2 + lrgdpch + lgrgdpch + interp + spline + lopenk + lfdiinward + lfpistock + ldopenk + lfdiinflow + lfpi + ldeconglob,
              model="logit",
              robust=TRUE,
              data=model3vars,
              cite=F)

model3classif<-prop.table(table(model3$result$fitted.values>.5, model3$result$y))
model3correct<-(model3classif[1,1] + model3classif[2,2])
model3correct