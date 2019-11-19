
library("rpart")
library("minpack.lm")
library("xgboost")
library("pROC")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("Functions.r")
DATAPATH="../data/clean_data/"
OUTPATH="../output/"


Datas_18_1d<-read.csv(paste(DATAPATH,"Datas_18_1d.csv",sep=""))
Datas_17_1d<-read.csv(paste(DATAPATH,"Datas_17_1d.csv",sep=""))
Datas_16_1d<-read.csv(paste(DATAPATH,"Datas_16_1d.csv",sep=""))
Datas_15_1d<-read.csv(paste(DATAPATH,"Datas_15_1d.csv",sep=""))
Datas_14_1d<-read.csv(paste(DATAPATH,"Datas_14_1d.csv",sep=""))

Datas1617181415_1d<-rbind(Datas_16_1d,Datas_17_1d,Datas_18_1d,Datas_14_1d,Datas_15_1d)
Datas14151617_18_1d<-rbind(Datas_14_1d,Datas_15_1d,Datas_16_1d,Datas_17_1d)
Datas18141516_17_1d<-rbind(Datas_18_1d,Datas_14_1d,Datas_15_1d,Datas_16_1d)
Datas17181415_16_1d<-rbind(Datas_17_1d,Datas_18_1d,Datas_14_1d,Datas_15_1d)
Datas16171814_15_1d<-rbind(Datas_16_1d,Datas_17_1d,Datas_18_1d,Datas_14_1d)
Datas16171815_14_1d<-rbind(Datas_16_1d,Datas_17_1d,Datas_18_1d,Datas_15_1d)


Datas_18_4d<-read.csv(paste(DATAPATH,"Datas_18_4d.csv",sep=""))
Datas_17_4d<-read.csv(paste(DATAPATH,"Datas_17_4d.csv",sep=""))
Datas_16_4d<-read.csv(paste(DATAPATH,"Datas_16_4d.csv",sep=""))
Datas_15_4d<-read.csv(paste(DATAPATH,"Datas_15_4d.csv",sep=""))
Datas_14_4d<-read.csv(paste(DATAPATH,"Datas_14_4d.csv",sep=""))

Datas1617181415_4d<-rbind(Datas_16_4d,Datas_17_4d,Datas_18_4d,Datas_14_4d,Datas_15_4d)
Datas14151617_18_4d<-rbind(Datas_14_4d,Datas_15_4d,Datas_16_4d,Datas_17_4d)
Datas18141516_17_4d<-rbind(Datas_18_4d,Datas_14_4d,Datas_15_4d,Datas_16_4d)
Datas17181415_16_4d<-rbind(Datas_17_4d,Datas_18_4d,Datas_14_4d,Datas_15_4d)
Datas16171814_15_4d<-rbind(Datas_16_4d,Datas_17_4d,Datas_18_4d,Datas_14_4d)
Datas16171815_14_4d<-rbind(Datas_16_4d,Datas_17_4d,Datas_18_4d,Datas_15_4d)


Datas_18_7d<-read.csv(paste(DATAPATH,"Datas_18_7d.csv",sep=""))
Datas_17_7d<-read.csv(paste(DATAPATH,"Datas_17_7d.csv",sep=""))
Datas_16_7d<-read.csv(paste(DATAPATH,"Datas_16_7d.csv",sep=""))
Datas_15_7d<-read.csv(paste(DATAPATH,"Datas_15_7d.csv",sep=""))
Datas_14_7d<-read.csv(paste(DATAPATH,"Datas_14_7d.csv",sep=""))

Datas1617181415_7d<-rbind(Datas_16_7d,Datas_17_7d,Datas_18_7d,Datas_14_7d,Datas_15_7d)
Datas14151617_18_7d<-rbind(Datas_14_7d,Datas_15_7d,Datas_16_7d,Datas_17_7d)
Datas18141516_17_7d<-rbind(Datas_18_7d,Datas_14_7d,Datas_15_7d,Datas_16_7d)
Datas17181415_16_7d<-rbind(Datas_17_7d,Datas_18_7d,Datas_14_7d,Datas_15_7d)
Datas16171814_15_7d<-rbind(Datas_16_7d,Datas_17_7d,Datas_18_7d,Datas_14_7d)
Datas16171815_14_7d<-rbind(Datas_16_7d,Datas_17_7d,Datas_18_7d,Datas_15_7d)


Datas_18_14d<-read.csv(paste(DATAPATH,"Datas_18_14d.csv",sep=""))
Datas_17_14d<-read.csv(paste(DATAPATH,"Datas_17_14d.csv",sep=""))
Datas_16_14d<-read.csv(paste(DATAPATH,"Datas_16_14d.csv",sep=""))
Datas_15_14d<-read.csv(paste(DATAPATH,"Datas_15_14d.csv",sep=""))
Datas_14_14d<-read.csv(paste(DATAPATH,"Datas_14_14d.csv",sep=""))

Datas1617181415_14d<-rbind(Datas_16_14d,Datas_17_14d,Datas_18_14d,Datas_14_14d,Datas_15_14d)
Datas14151617_18_14d<-rbind(Datas_14_14d,Datas_15_14d,Datas_16_14d,Datas_17_14d)
Datas18141516_17_14d<-rbind(Datas_18_14d,Datas_14_14d,Datas_15_14d,Datas_16_14d)
Datas17181415_16_14d<-rbind(Datas_17_14d,Datas_18_14d,Datas_14_14d,Datas_15_14d)
Datas16171814_15_14d<-rbind(Datas_16_14d,Datas_17_14d,Datas_18_14d,Datas_14_14d)
Datas16171815_14_14d<-rbind(Datas_16_14d,Datas_17_14d,Datas_18_14d,Datas_15_14d)



# Attribute and response columns for xgboost models
Attr=c(6,7,8,10,11,12,13)
Resp=5

# Calibrate entire dataset
Models.whole.1d=CalibrateModel(Datas1617181415_1d,AttrCol=Attr,RespCol=Resp)
Models.whole.4d=CalibrateModel(Datas1617181415_4d,AttrCol=Attr,RespCol=Resp)
Models.whole.7d=CalibrateModel(Datas1617181415_7d,AttrCol=Attr,RespCol=Resp)


#Calibrate Datas_1d

Models.14151617_18_1d=CalibrateModel(Datas14151617_18_1d,AttrCol=Attr,RespCol=Resp)
Models.18141516_17_1d=CalibrateModel(Datas18141516_17_1d,AttrCol=Attr,RespCol=Resp)
Models.17181415_16_1d=CalibrateModel(Datas17181415_16_1d,AttrCol=Attr,RespCol=Resp)
Models.16171814_15_1d=CalibrateModel(Datas16171814_15_1d,AttrCol=Attr,RespCol=Resp)
Models.16171815_14_1d=CalibrateModel(Datas16171815_14_1d,AttrCol=Attr,RespCol=Resp)

Pred.14151617_18_1d=PredictModels(Models.14151617_18_1d,newdata=(Datas_18_1d),AttrCol=Attr,RespCol=Resp)
Pred.18141516_17_1d=PredictModels(Models.18141516_17_1d,newdata=(Datas_17_1d),AttrCol=Attr,RespCol=Resp)
Pred.17181415_16_1d=PredictModels(Models.17181415_16_1d,newdata=(Datas_16_1d),AttrCol=Attr,RespCol=Resp)
Pred.16171814_15_1d=PredictModels(Models.16171814_15_1d,newdata=(Datas_15_1d),AttrCol=Attr,RespCol=Resp)
Pred.16171815_14_1d=PredictModels(Models.16171815_14_1d,newdata=(Datas_14_1d),AttrCol=Attr,RespCol=Resp)

A=PerformanceCalc(Pred.14151617_18_1d,RespCol=2,PredCols = c(1,3,4,5,6,7))
B=PerformanceCalc(Pred.18141516_17_1d,RespCol=2,PredCols = c(1,3,4,5,6,7))
C=PerformanceCalc(Pred.17181415_16_1d,RespCol=2,PredCols = c(1,3,4,5,6,7))
D=PerformanceCalc(Pred.16171814_15_1d,RespCol=2,PredCols = c(1,3,4,5,6,7))
E=PerformanceCalc(Pred.16171815_14_1d,RespCol=2,PredCols = c(1,3,4,5,6,7))


rocs_1day=rbind(E$rocs,D$rocs,C$rocs,B$rocs,A$rocs)

cors_1day=rbind(E$cors,D$cors,C$cors,B$cors,A$cors )
rsqs_1day=rbind(E$rsqs,D$rsqs,C$rsqs,B$rsqs,A$rsqs)
rmses_1day=rbind(E$rmses,D$rmses,C$rmses,B$rmses,A$rmses)

Header=c("Dt","Rf","lm","blm","nlm","naive")

write.table(x = round(rocs_1day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rocs_1day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(cors_1day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'cors_1day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(rsqs_1day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rsqs_1day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(rmses_1day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rmses_1day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")



# Calibrate Datas_4d

Models.14151617_18_4d=CalibrateModel(Datas14151617_18_4d,AttrCol=Attr,RespCol=Resp)
Models.18141516_17_4d=CalibrateModel(Datas18141516_17_4d,AttrCol=Attr,RespCol=Resp)
Models.17181415_16_4d=CalibrateModel(Datas17181415_16_4d,AttrCol=Attr,RespCol=Resp)
Models.16171814_15_4d=CalibrateModel(Datas16171814_15_4d,AttrCol=Attr,RespCol=Resp)
Models.16171815_14_4d=CalibrateModel(Datas16171815_14_4d,AttrCol=Attr,RespCol=Resp)

Pred.14151617_18_4d=PredictModels(Models.14151617_18_4d,newdata=(Datas_18_4d),AttrCol=Attr,RespCol=Resp)
Pred.18141516_17_4d=PredictModels(Models.18141516_17_4d,newdata=(Datas_17_4d),AttrCol=Attr,RespCol=Resp)
Pred.17181415_16_4d=PredictModels(Models.17181415_16_4d,newdata=(Datas_16_4d),AttrCol=Attr,RespCol=Resp)
Pred.16171814_15_4d=PredictModels(Models.16171814_15_4d,newdata=(Datas_15_4d),AttrCol=Attr,RespCol=Resp)
Pred.16171815_14_4d=PredictModels(Models.16171815_14_4d,newdata=(Datas_14_4d),AttrCol=Attr,RespCol=Resp)

A=PerformanceCalc(Pred.14151617_18_4d,RespCol=2,PredCols = c(1,3,4,5,6,7))
B=PerformanceCalc(Pred.18141516_17_4d,RespCol=2,PredCols = c(1,3,4,5,6,7))
C=PerformanceCalc(Pred.17181415_16_4d,RespCol=2,PredCols = c(1,3,4,5,6,7))
D=PerformanceCalc(Pred.16171814_15_4d,RespCol=2,PredCols = c(1,3,4,5,6,7))
E=PerformanceCalc(Pred.16171815_14_4d,RespCol=2,PredCols = c(1,3,4,5,6,7))


rocs_4day=rbind(E$rocs,D$rocs,C$rocs,B$rocs,A$rocs)
cors_4day=rbind(E$cors,D$cors,C$cors,B$cors,A$cors )
rsqs_4day=rbind(E$rsqs,D$rsqs,C$rsqs,B$rsqs,A$rsqs )
rmses_4day=rbind(E$rmses,D$rmses,C$rmses,B$rmses,A$rmses)

Header=c("Dt","Rf","lm","blm","nlm","naive")

write.table(x = round(rocs_4day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rocs_4day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(cors_4day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'cors_4day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(rsqs_4day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rsqs_4day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(rmses_4day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rmses_4day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")




#Calibrate Datas_7d

Models.14151617_18_7d=CalibrateModel(Datas14151617_18_7d,AttrCol=Attr,RespCol=Resp)
Models.18141516_17_7d=CalibrateModel(Datas18141516_17_7d,AttrCol=Attr,RespCol=Resp)
Models.17181415_16_7d=CalibrateModel(Datas17181415_16_7d,AttrCol=Attr,RespCol=Resp)
Models.16171814_15_7d=CalibrateModel(Datas16171814_15_7d,AttrCol=Attr,RespCol=Resp)
Models.16171815_14_7d=CalibrateModel(Datas16171815_14_7d,AttrCol=Attr,RespCol=Resp)

Pred.14151617_18_7d=PredictModels(Models.14151617_18_7d,newdata=(Datas_18_7d),AttrCol=Attr,RespCol=Resp)
Pred.18141516_17_7d=PredictModels(Models.18141516_17_7d,newdata=(Datas_17_7d),AttrCol=Attr,RespCol=Resp)
Pred.17181415_16_7d=PredictModels(Models.17181415_16_7d,newdata=(Datas_16_7d),AttrCol=Attr,RespCol=Resp)
Pred.16171814_15_7d=PredictModels(Models.16171814_15_7d,newdata=(Datas_15_7d),AttrCol=Attr,RespCol=Resp)
Pred.16171815_14_7d=PredictModels(Models.16171815_14_7d,newdata=(Datas_14_7d),AttrCol=Attr,RespCol=Resp)

A=PerformanceCalc(Pred.14151617_18_7d,RespCol=2,PredCols = c(1,3,4,5,6,7))
B=PerformanceCalc(Pred.18141516_17_7d,RespCol=2,PredCols = c(1,3,4,5,6,7))
C=PerformanceCalc(Pred.17181415_16_7d,RespCol=2,PredCols = c(1,3,4,5,6,7))
D=PerformanceCalc(Pred.16171814_15_7d,RespCol=2,PredCols = c(1,3,4,5,6,7))
E=PerformanceCalc(Pred.16171815_14_7d,RespCol=2,PredCols = c(1,3,4,5,6,7))


rocs_7day=rbind(E$rocs,D$rocs,C$rocs,B$rocs,A$rocs)
cors_7day=rbind(E$cors,D$cors,C$cors,B$cors,A$cors )
rsqs_7day=rbind(E$rsqs,D$rsqs,C$rsqs,B$rsqs,A$rsqs )
rmses_7day=rbind(E$rmses,D$rmses,C$rmses,B$rmses,A$rmses)

Header=c("Dt","Rf","lm","blm","nlm","naive")


write.table(x = round(rocs_7day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rocs_7day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(cors_7day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'cors_7day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(rsqs_7day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rsqs_7day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(rmses_7day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rmses_7day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")




# Calibrate Datas_14d

Models.14151617_18_14d=CalibrateModel(Datas14151617_18_14d,AttrCol=Attr,RespCol=Resp)
Models.18141516_17_14d=CalibrateModel(Datas18141516_17_14d,AttrCol=Attr,RespCol=Resp)
Models.17181415_16_14d=CalibrateModel(Datas17181415_16_14d,AttrCol=Attr,RespCol=Resp)
Models.16171814_15_14d=CalibrateModel(Datas16171814_15_14d,AttrCol=Attr,RespCol=Resp)
Models.16171815_14_14d=CalibrateModel(Datas16171815_14_14d,AttrCol=Attr,RespCol=Resp)


Pred.14151617_18_14d=PredictModels(Models.14151617_18_14d,newdata=(Datas_18_14d),AttrCol=Attr,RespCol=Resp)
Pred.18141516_17_14d=PredictModels(Models.18141516_17_14d,newdata=(Datas_17_14d),AttrCol=Attr,RespCol=Resp)
Pred.17181415_16_14d=PredictModels(Models.17181415_16_14d,newdata=(Datas_16_14d),AttrCol=Attr,RespCol=Resp)
Pred.16171814_15_14d=PredictModels(Models.16171814_15_14d,newdata=(Datas_15_14d),AttrCol=Attr,RespCol=Resp)
Pred.16171815_14_14d=PredictModels(Models.16171815_14_14d,newdata=(Datas_14_14d),AttrCol=Attr,RespCol=Resp)

A=PerformanceCalc(Pred.14151617_18_14d,RespCol=2,PredCols = c(1,3,4,5,6,7))
B=PerformanceCalc(Pred.18141516_17_14d,RespCol=2,PredCols = c(1,3,4,5,6,7))
C=PerformanceCalc(Pred.17181415_16_14d,RespCol=2,PredCols = c(1,3,4,5,6,7))
D=PerformanceCalc(Pred.16171814_15_14d,RespCol=2,PredCols = c(1,3,4,5,6,7))
E=PerformanceCalc(Pred.16171815_14_14d,RespCol=2,PredCols = c(1,3,4,5,6,7))


rocs_14day=rbind(E$rocs,D$rocs,C$rocs,B$rocs,A$rocs)
cors_14day=rbind(E$cors,D$cors,C$cors,B$cors,A$cors )
rsqs_14day=rbind(E$rsqs,D$rsqs,C$rsqs,B$rsqs,A$rsqs )
rmses_14day=rbind(E$rmses,D$rmses,C$rmses,B$rmses,A$rmses)

Header=c("Dt","Rf","lm","blm","nlm","naive")

write.table(x = round(rocs_14day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rocs_14day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(cors_14day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'cors_14day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(rsqs_14day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rsqs_14day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")
write.table(x = round(rmses_14day[,c(2,3,4,5,6,1)],digits=2),file = paste(OUTPATH,'rmses_14day.csv',sep=""),row.names = FALSE,col.names = Header,sep=",")



