for(p in seq(.35,.9,.05)){
  t1=table(model1$result$fitted.values>p, model1$result$y)
  cat(p,(t1[2,1]+t1[1,2])/sum(t1),"\n")
}

model1classif<-prop.table(table(model1$result$fitted.values>.5, model1$result$y))
model1correct<-(model1classif[1,1] + model1classif[2,2])
model1correct

for(p in seq(.35,.9,.05)){
  t1=table(model2$result$fitted.values>p, model2$result$y)
  cat(p,(t1[2,1]+t1[1,2])/sum(t1),"\n")
}

model2classif<-prop.table(table(model2$result$fitted.values>.5, model5$result$y))
model2correct<-(model2classif[1,1] + model2classif[2,2])
model2correct