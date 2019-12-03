rmse<-function(X,Y){
  out=sqrt(sum((X-Y)^2,na.rm=TRUE)/length(X[!is.na(X)]))/(max(X,na.rm=TRUE)-min(X,na.rm=TRUE))
  return(out)
}

rsq<-function(X,Y){
  out=1-(sum((X-Y)^2,na.rm=TRUE)/sum((X-mean(X,na.rm=TRUE))^2))
  return(out)
}


CalibrateModel<-function(Datas,AttrCol,RespCol){

  FUNC=BGAFutureL~BGANowL+Temp+PAR+Stability+Wind+TempDiff+TempAcc+Turb+Cond
  #FUNC=BGAFutureL~BGANowL+Temp+PAR
  FUNC2=BGAFutureL~BGANowL+Temp+PAR+Wind

  Func4=BGAFutureL~A*exp(a*Temp)*PAR/(PAR+Ik+PAR^2/B)+E*BGANowL+FF*Wind+G*TempDiff+H*TempAcc-C+D*Turb+M*Stability+L*Cond
  #Func4=BGAFutureL~A*exp(a*Temp)*PAR/(PAR+Ik*Temp+PAR^2/B)+E*BGANowL+FF*Wind+G*TempDiff+H*TempAcc-C
  Controls=nls.lm.control(maxiter=1024,maxfev=30000)
  
  model.nls=nlsLM(Func4,data=Datas,start=list(A=0.5e-03,a=1.0e-01,Ik=1.0,B=4.0e+04,C= 0.000000e+00,E= 9.757346e-01,FF=2.0e-03,G= 1.0e+0,H= 6.396827e+02,D=0,M=0,L=0),lower=c(0,0,0,0,0,0,-0.01,0,0,0,-1,-1),control=Controls)
  model.lm=lm(FUNC,data=Datas)
  model.dt=rpart(FUNC,data=Datas)
  ddata=NULL
  ddata=list(data=data.frame(Datas[,AttrCol]),label=Datas[,RespCol])
  model.boost=xgboost(data=as.matrix(ddata$data),label=ddata$label,nrounds=200,print_every_n = 250L,objective="reg:linear",verbose=0,maxdepth=7,eta=0.01)
  model.boostlm=xgboost(data=as.matrix(ddata$data),label=ddata$label,nrounds=200,print_every_n = 250L,objective="reg:linear",verbose=0,booster="gblinear")
  out=list(model.dt,model.boost,model.lm,model.boostlm,model.nls)
  return(out)
}


PredictModels<-function(models,newdatas,AttrCol,RespCol){
  response=newdatas[,RespCol]
  responses=response
  preds=newdatas[,AttrCol[1]]

  for (m in 1:length(models)){
    if(m == 4 | m == 2){
      preds=predict(models[[m]],newdata=as.matrix(newdatas[,AttrCol]))
    }else{
      preds=predict(models[[m]],newdata=data.frame(newdatas[,AttrCol]))
    }

    responses=cbind(responses,preds)
  }
  responses=cbind(newdatas[,6],responses)
  return((responses))
}


PerformanceCalc<-function(Dts,RespCol,PredCols){
  N=length(PredCols)
  rsqs<-vector(mode="numeric",length=N)
  rmses<-vector(mode="numeric",length=N)
  rocs<-vector(mode="numeric",length=N)
  cors<-vector(mode="numeric",length=N)
  
  for (i in (1:N)){
    RocData<-as.numeric(roc(Dts[,RespCol]>quantile(Dts[,RespCol],0.75),Dts[,PredCols[i]])$auc)
    CorData<-cor(Dts[,RespCol],Dts[,PredCols[i]],use="complete.obs")
    RsqData<-rsq(Dts[,RespCol],Dts[,PredCols[i]])
    RMSESData<-rmse(Dts[,RespCol],Dts[,PredCols[i]])
    rsqs[i]=RsqData
    rmses[i]=RMSESData
    rocs[i]=RocData
    cors[i]=CorData
  }
  return(data.frame(rsqs,rmses,rocs,cors))
}

PanelFigureHFPaper<-function(Datas=Pred.14151617_18_4d,Dates=Datas_18_4d$Dates){
  PlotData=data.frame(Datas,Dates)
  names(PlotData)=c('Naive','Response','dt','bdt','lm','blm','nlm','Dates')
  pp=ggplot(data=PlotData,aes(x=Dates))
  p1=pp+geom_line(aes(y=dt))+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank())+ylab("A")
  p1=p1+geom_line(aes(y=Response),color="grey")
  p2=pp+geom_line(aes(y=bdt))+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank())+ylab("A")
  p2=p2+geom_line(aes(y=Response),color="grey")
  p3=pp+geom_line(aes(y=lm))+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank())+ylab("A")
  p3=p3+geom_line(aes(y=Response),color="grey")
  p4=pp+geom_line(aes(y=blm))+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.title.y=element_blank())+ylab("A")
  p4=p4+geom_line(aes(y=Response),color="grey")
  p5=pp+geom_line(aes(y=nlm))+ theme(axis.title.y=element_blank(),axis.title.x=element_blank())+ylab("A")
  p5=p5+geom_line(aes(y=Response),color="grey")
  out=plot_grid(p1,p2,p3,p4,p5,labels=c("A","B","C","D","E"),align="v",ncol = 1,label_x=0.12,rel_heights = c(1,1,1,1,1.15))
  return(out)
}

