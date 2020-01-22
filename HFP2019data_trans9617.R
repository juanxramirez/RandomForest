#RandomForest
if(!requireNamespace("randomForest", quietly = TRUE))
  install.packages("randomForest", quiet = TRUE, dependencies = TRUE)
library(randomForest)

#import data
highHFP.drivers.nogain<-read.delim("G:/Conservation Solution Lab/Juan/data_tables/random_forest/final_2/Suitable_vs_unsuitable/trans9617_final_2_last_two.txt")

#note, response variable is seen as a number, convert to a factor
highHFP.drivers.nogain$trans9617<-as.factor(highHFP.drivers.nogain$trans9617)
highHFP.drivers.nogain$Order<-as.factor(highHFP.drivers.nogain$Order)
highHFP.drivers.nogain$Diet<-as.factor(highHFP.drivers.nogain$Diet)
nrow(highHFP.drivers.nogain)

#check all variables types
class(highHFP.drivers.nogain$Order)
class(highHFP.drivers.nogain$Diet)
class(highHFP.drivers.nogain$Massg)
class(highHFP.drivers.nogain$high_hfp_extent_suitable)
class(highHFP.drivers.nogain$high_hfp_extent_unsuitable)
class(highHFP.drivers.nogain$GISfrag_suitable)
class(highHFP.drivers.nogain$Percentage_change_suitable)
class(highHFP.drivers.nogain$Percentage_change_unsuitable)
class(highHFP.drivers.nogain$mean_distance_unsuitable)
class(highHFP.drivers.nogain$proportion_suitable)
class(highHFP.drivers.nogain$proportion_unsuitable)
class(highHFP.drivers.nogain$trans9617)

#set seed for the random forest model (keep the same results based on the number of seeds (random distribution setted))
set.seed(500) 

#select variables and assign names for plotting labels
vars<-c("Order", "Diet", "Massg", "high_hfp_extent_suitable", "high_hfp_extent_unsuitable", "GISfrag_suitable", "Percentage_change_suitable", "Percentage_change_unsuitable", "mean_distance_unsuitable", "proportion_suitable", "proportion_unsuitable", "trans9617")
names<-c("Order", "Diet", "Body mass", "High HFP extent suitable", "High HFP extent matrix", "Habitat fragmentation", "High HFP change suitable", "High HFP change matrix", "Distance between patches", "Proportion suitable", "Proportion matrix", "trans9617")

#data.imputed<-rfImpute(trans9617~., data=highHFP.drivers.nogain[,vars], iter=6, ntree=1000)


#run random forest model
#highHFP.drivers.RFnogain.noRS<- randomForest(trans9617~., data=data.imputed, ntree=1000, importance=TRUE, proximity=TRUE)

#na.action = na.omit
data_na.omit<-na.omit(highHFP.drivers.nogain)

highHFP.drivers.RFnogain.noRS<- randomForest(trans9617~., data=data_na.omit[,vars], na.action = na.omit, ntree=1000, importance=TRUE)

#highHFP.drivers.RFnogain.noRS

#colour-code variables according to their class
col.imp<-c("#D55E00", "#D55E00", "#D55E00", "#0072B2", "#0072B2", "#009E73", "#0072B2", "#0072B2", "#009E73", "#009E73", "#009E73")

#extract importance
imp.DF<-data.frame(importance(highHFP.drivers.RFnogain.noRS))
imp.DF$col<-col.imp
imp.DF$names<-names[-length(names)]

#plot importance
par(mfrow=c(2,1),mar=c(4,14,3,2),oma=c(2,0,0,0)) 

imp.3<-imp.DF[order(imp.DF$MeanDecreaseAccuracy),]
barplot(imp.3[,3],names.arg=imp.3$names,horiz=TRUE,las=1,col=imp.3$col,main="a",xlab=colnames(imp.3)[3],xlim=c(0,100),cex.names=0.8)

imp.4<-imp.DF[order(imp.DF$MeanDecreaseGini),]
barplot(imp.4[,4],names.arg=imp.4$names,horiz=TRUE,las=1,col=imp.4$col,main="b",xlab=colnames(imp.4)[4],xlim=c(0,250),cex.names=0.8)

legend("bottomright", col=c("#D55E00", "#0072B2", "#009E73"),c("Life-history","Pressure","Environment"), ncol=1, cex=0.8, pch=15, x.intersp = 0.5, y.intersp = 0.2, text.width = 30)


