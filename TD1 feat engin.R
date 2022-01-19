  # settings ####
  setwd("C:/Users/doblin/Documents/clients/2014 07 01 - ESILV/supports/support 2021 2022/TD1 feat engin")
  require(data.table) ; require(ggplot2) ; require(ggthemes); require(cowplot) ; require(dummies) ; require(plotly)
  bleu<-rgb(red=0, green=174, blue=239, maxColorValue = 255)
  # fonctions ####
  eval<-function(pred,Test) {
    conf<-table(pred,Test$delaiExp) 
    Conf<-as.data.table(conf) ; Conf[,Prediction:=as.integer(pred)]; Conf[,Reel:=as.integer(V1)]; 
    mat<-ggplot(Conf,aes(x=Prediction,y=Reel))+geom_point(aes(size=N,col=abs(Reel-Prediction)))+theme_tufte()+scale_color_gradient(low="green",high="red")+theme(legend.position = "none")#+scale_size_continuous(range = c(0.2,15))
    Confpct<-copy(Conf)
    Confpct[,ecart:=Reel-Prediction]
    Confpct[ecart< -3,ecart:=-3] ; Confpct[ecart>3,ecart:=3]
    Confpct<-Confpct[,.(N=sum(N)),by=.(ecart)]
    Confpct[,N:=N/sum(Confpct$N)]
    bar<-ggplot(Confpct,aes(x=ecart,y=N,label=paste0(round(N*1000)/10,"%")))+geom_bar(stat="identity",aes(fill=abs(ecart)))+theme_tufte()+scale_fill_gradient(low="green",high="red")+geom_text(nudge_y = 0.01)+theme_void()+theme(legend.position = "none")+ylim(0,1)
    plot_grid(mat, bar, labels=c("Matrice", "Taux d'erreur  : Reel-Prediction"), ncol = 2, nrow = 1)
  # return(Confpct[ecart==0]$N)
    }
  # acquisition ####
  ss<-fread("logistique.csv")
  setnames(ss,nett(names(ss)))
  # ss<-ss[,.(datedecreationdecommande,providerservice_id,dateexpe)]# fwrite(ss,file="logistique.csv")
  # feat engin####
  ss[,datedecreationdecommande:=as.POSIXct(datedecreationdecommande,"%Y-%m-%d %H:%M:%S",tz="GMT")]
  ss[,dateexpe:=as.Date(dateexpe,"%d/%m/%Y",tz="GMT")]
  ss[,commHeure:=as.integer(format(datedecreationdecommande,"%H"))] 
  ss[,commJourS:=format(datedecreationdecommande,"%A")]
  ss[,commDate:=as.Date(datedecreationdecommande)]
  ss[,delaiExp:=as.integer(as.Date(dateexpe)-as.Date(datedecreationdecommande))]
  ss<-ss[order(datedecreationdecommande)]
  ss[,compt:=seq_len (.N)]
  
  
  # ext<-ss[as.Date(datedecreationdecommande)>as.Date("2019-12-01")]
  # save(ext,file="ext.rda")
  dates<-seq.Date(from=min(as.Date(ss$datedecreationdecommande)),
                  to=max(as.Date(ss$datedecreationdecommande)),by="days")
  stock<-sapply(dates,function(x) nrow(ss[commDate<x & dateexpe>=x]))
  retJ2<-sapply(dates,function(x) nrow(ss[commDate==x-2 & dateexpe>=x]))
  retJ4<-sapply(dates,function(x) nrow(ss[commDate==x-4 & dateexpe>=x]))
  veillePost18h<-sapply(dates,function(x) nrow(ss[commDate==x-1 & commHeure>=18]))
  TraiteVeille<-sapply(dates,function(x) max(nrow(ss[dateexpe==x]),nrow(ss[dateexpe==x-1])))
  # ajouter fluctuation
  Temp<-data.table(commDate=dates,stock=stock,retJ2=retJ2,retJ4=retJ4,veillePost18h=veillePost18h,TraiteVeille=TraiteVeille)
  
  setkey(ss,commDate) ; setkey(Temp,commDate) ; ss<-Temp[ss]
  ss[,ratiorattrap:=stock/TraiteVeille]
  ss[,providerservice_id:=as.factor(providerservice_id)]
  # Dataviz ####
  ggplot(ss[delaiExp>=0],aes(x=delaiExp))+geom_bar(fill=bleu)+theme_tufte()
  ggplot(ss[delaiExp>=0],aes(x=commHeure))+geom_bar(fill=bleu)+theme_tufte()
  ggplot(ss[delaiExp>=0],aes(x=commDate))+geom_bar(fill=bleu)+theme_tufte()#+scale_fill_gradient(low="green",high = "red")
  perim<-copy(ss) ; perim<-perim[delaiExp>=0] ;perim[delaiExp>5,delaiExp:=5]  
  perim[,delaiExp:=factor(delaiExp,levels=as.character(5:0))]
  
  ggplot(perim,aes(x=commDate))+geom_bar(aes(fill=as.factor(delaiExp)))+theme_tufte()+scale_fill_manual(values=c("0"="green","1"="lightgreen","2"="orange","3"="maroon","4"="red","5"="black"))#+facet_wrap(~round(commHeure/3),ncol=1)
  ggplot(perim,aes(x=commDate))+geom_bar(aes(fill=as.factor(delaiExp)))+theme_tufte()+scale_fill_manual(values=c("0"="green","1"="lightgreen","2"="orange","3"="maroon","4"="red","5"="black"))+facet_wrap(~round(commHeure<19),ncol=1)
  ggplot(perim,aes(x=dateexpe))+geom_bar(aes(fill=as.factor(delaiExp)))+theme_tufte()+scale_fill_manual(values=c("0"="green","1"="lightgreen","2"="orange","3"="maroon","4"="red","5"="black"))#+facet_wrap(~round(commHeure/3),ncol=1)
  ggplot(ss[delaiExp>=0],aes(x=1))+geom_bar(position="fill",aes(fill=as.factor(providerservice_id)))+theme_tufte()+coord_polar(theta="y")+theme_void()
  # modele ####
  perim<-copy(ss)
  perim<-perim[commDate>min(perim$commDate)+2]
  perim<-perim[,.(stock,providerservice_id,commHeure,commJourS,delaiExp,retJ2,retJ4,ratiorattrap,TraiteVeille)]
  
  perim[,commJourS:=as.factor(commJourS)]
  perim<-perim[delaiExp>=0]
  perim[delaiExp>6,delaiExp:=6]
  perimMat<-as.matrix(dummy.data.frame(perim))
  Test<-sample(1:nrow(perim),nrow(perim)/3)
  perimTest<-perim[Test] ; perimTrain<-perim[-Test]
  perimMatTrain<-perimMat[-Test,] ; perimMatTest<-perimMat[Test,]
  
  require(rpart) ; require(rpart.plot)
  arbre<-rpart(delaiExp~.,perimTrain)
  predArbre<-round(predict(arbre,perimTest))
  eval(predArbre,perimTest)
  rpart.plot(arbre,cex=0.6)

require(randomForest)
rf<-randomForest(delaiExp~.,perimTrain,ntree=100,mtry=2)
predRF<-round(predict(rf,perimTest))
save(rf,predRF,file="predRF.rda")
eval(predRF,perimTest)
imp=data.table(feat=rownames(rf$importance),importance=rf$importance)
imp[,feat:=factor(feat,levels=imp[order(importance.IncNodePurity)]$feat)]
ggplot(imp,aes(x=feat,y=importance.IncNodePurity))+geom_bar(stat="identity")+theme_tufte()+coord_flip()
predRFNotRound<-predict(rf,perimTest)
ggplot(data.table(predRFNotRound=predRFNotRound,delaiExp=perimTest$delaiExp),aes(x=predRFNotRound))+geom_histogram(aes(fill=as.factor(delaiExp)))+theme_tufte()
temp<-copy(perimTest) ;temp[,predRFNotRound:=predRFNotRound]
rf2<-randomForest(delaiExp~predRFNotRound,temp,ntree=100,mtry=2)

require(dismo) ;require(doParallel) ; require(gbm)
nbclust=5 ; cl<-makeCluster(nbclust) ; registerDoParallel(cl) # ouvre de 2 clusters de calcul - ajustez  ? votre pc
reglageGBM <-foreach(i=1:nbclust,.combine=rbind,.packages=c("dismo","gbm","pROC","data.table","ggplot2","cowplot","ggthemes")) %dopar% { 
  temp<-c()  ;shrinkage = 0.02 ; TC= i#{for (TC in 3:7) {
    n.minobsinnode<-1 #;  TC=3#shrinkage=.03 ;
    mod.gbm <- gbm.step(data=perimTrain, gbm.x = which(names(perimTrain)!="delaiExp"), gbm.y = which(names(perimTrain)=="delaiExp"), family = "gaussian",step.size=30,tree.complexity = TC,learning.rate = shrinkage, bag.fraction = 0.8,n.folds=5,max.trees=50000,verbose=0)
    pred.gbm<-round(predict(mod.gbm,n.trees=mod.gbm$n.trees,perimTest))
    perf=eval(pred.gbm,perimTest)
    temp<-rbind(temp,c(TC,perf,gbm.perf(mod.gbm),mod.gbm$n.trees))
  # }}  ;
    temp<-as.data.table(temp)
}   ; stopCluster(cl)
mod.gbm <- gbm.step(data=perimTrain, gbm.x = which(names(perimTrain)!="delaiExp"), gbm.y = which(names(perimTrain)=="delaiExp"), family = "gaussian",step.size=30,tree.complexity = 6,learning.rate = 0.02, bag.fraction = 0.8,n.folds=5,max.trees=50000,verbose=0)
pred.gbm<-round(predict(mod.gbm,n.trees=mod.gbm$n.trees,perimTest))
pred.gbm[pred.gbm<0]<-0
save(mod.gbm,pred.gbm,file="predGBM.rda")
eval(pred.gbm,perimTest)
impGBM<-as.data.table(summary(mod.gbm))
impGBM[,var:=as.character(var)]
impGBM[,var:=factor(var, levels=impGBM[order(rel.inf)]$var)]
ggplot(impGBM[rel.inf>0.5],aes(x=var,y=rel.inf))+geom_bar(stat="identity",aes(fill=rel.inf))+theme_tufte()+coord_flip() 
gbm.plot(mod.gbm, n.plots=12, write.title = FALSE)
impgGBMinter <- gbm.interactions(mod.gbm)
impCroiseeMelt<-impgGBMinter$rank.list
impCroiseeDcast<-dcast(impCroiseeMelt,var1.names~var2.names,value.var="int.size") 
ggplotly(ggplot(impCroiseeMelt,aes(x=var1.names,y=var2.names))+geom_point(col="red",aes(size=int.size,alpha=int.size))+theme_tufte())

setnames(reglageGBM,c("TC","perf","perfgbm","nbarbres"))
# analyse : summary(mod.gbm) ; gbm.plot.fits(mod.gbm) ; plot.gbm(mod.gbm, i.var = c(1,2)) ; interact.gbm(mod.gbm,ttTrainRF,i.var=5,gbm.perf(mod.gbm)) ;pretty(mod.gbm, i.tree = 1)
save(reglageGBM,file="reglageGBM.rda")
ggplot(reglageGBM,aes(x=TC,y=perf))+geom_point()+theme_tufte()+geom_smooth(span=0.2,method="loess")

require(xgboost)
dtrain <- xgb.DMatrix(data = perimMatTrain[,-which(colnames(perimMatTrain)=="delaiExp")], label = perimMatTrain[,"delaiExp"]) ;  
dvalid <- xgb.DMatrix(data = perimMatTest[,-which(colnames(perimMatTest)=="delaiExp")], label = perimMatTest[,"delaiExp"]) ;  
reglXGB3<-c() 
tt<-Sys.time() ; nn<-0
eta<-0.03#for (eta in 10^seq(-3,-2,length.out = 4)) { #seq(-0.1,-1.5,length.out = 6)
for (max.depth in sort(rep(2:10,1))) {
  mtry=0.95#for (mtry in seq(0.1,1,length.out = 9)) { #mtry=0.6#
  gamma=0#for (gamma in 10^seq(-4,3,length.out = 7)) { #gamma=0# 
  lambda=0#for (lambda in 10^seq(-5,2,length.out = 10)) {
    xgb.params <- list("objective"  = "reg:squarederror", "eta" = eta, "subsample" = 0.9, "colsample_bytree" = mtry, "max_depth" = max.depth, "gamma" = gamma,"lambda"=lambda,"nthread" = 10)
    nn<-nn+1
    mod.xgb.cv <- xgb.cv(data = dtrain, params=xgb.params, verbose = 0, nfold=3,maximize=FALSE ,nrounds = 100000, early_stopping_rounds = 30)
    mod.xgb  <- xgb.train(data = dtrain, params=xgb.params, nrounds = mod.xgb.cv$best_iteration,  verbose = 0,maximize=FALSE)
    pred.xgb <-round(predict(mod.xgb, dvalid,ntreelimit=mod.xgb.cv$best_ntreelimit)) # pr?diction
    perf=eval(pred.xgb,perimTest)
    reglXGB3  <-rbind(reglXGB3,c(max.depth,eta,gamma,lambda,mtry,perf,mod.xgb.cv$best_iteration))
    print(c(nn,max.depth,mod.xgb.cv$best_iteration,perf))
    # print(c(eta,gamma,lambda,mtry,perf,difftime(Sys.time(),tt,units="min")))
    tt<-Sys.time()
    save(reglXGB3,file="reglXGB3.rda")
  }#}#}}
xgb.params <- list("objective"  = "reg:squarederror", "eta" = 0.03, "subsample" = 0.9, "colsample_bytree" = 0.9, "max_depth" = 6, "gamma" = 0,"lambda"=0,"nthread" = 10)
mod.xgb.cv <- xgb.cv(data = dtrain, params=xgb.params, verbose = 0, nfold=3,maximize=FALSE ,nrounds = 100000, early_stopping_rounds = 30)
mod.xgb  <- xgb.train(data = dtrain, params=xgb.params, nrounds = mod.xgb.cv$best_iteration,  verbose = 0,maximize=FALSE)
pred.xgb <-round(predict(mod.xgb, dvalid,ntreelimit=mod.xgb.cv$best_ntreelimit)) # pr?diction
save(mod.xgb,pred.xgb,file="predxgb.rda")
eval(pred.xgb,perimTest)
importance <- xgb.importance(model = mod.xgb)
importance[,Feature:=factor(Feature,levels=importance[order(Gain)]$Feature)]
ggplot(importance,aes(x=Feature,y=Gain))+geom_bar(stat="identity")+theme_tufte()+coord_flip()

