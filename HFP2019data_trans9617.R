library(randomForest)

# import data
highHFP.drivers.nogain<-read.delim("G:/Conservation Solution Lab/Juan/data_tables/random_forest/final/trans9617_last_two.txt")

# note: nearctic realm (NA) is seen as a missing value, re-convert it into a factor level
#highHFP.drivers.nogain$Order<-as.character(highHFP.drivers.nogain$Order)
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Afrosoricida"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Carnivora"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Cetartiodactyla"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Chiroptera"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Cingulata"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Dasyuromorphia"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Dermoptera"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Didelphimorphia"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Diprotodontia"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Eulipotyphla"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Lagomorpha"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Macroscelidea"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Microbiotheria"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Monotremata"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Notoryctemorphia"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Paucituberculata"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Peramelemorphia"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Perissodactyla"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Pholidota"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Pilosa"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Primates"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Proboscidea"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Rodentia"
#highHFP.drivers.nogain$Order[is.na(highHFP.drivers.nogain$Order)]<-"Scandentia"
#highHFP.drivers.nogain$Order<-as.factor(highHFP.drivers.nogain$Order)

# note, response variable is seens as a number, convert to a factor
highHFP.drivers.nogain$trans9617<-as.factor(highHFP.drivers.nogain$trans9617)
highHFP.drivers.nogain$Order<-as.factor(highHFP.drivers.nogain$Order)
nrow(highHFP.drivers.nogain)

class(highHFP.drivers.nogain$trans9617)

#set seed for the random forest (keep the same results based on the number of seeds (random distribution setted))
set.seed(500) 

# select variables and assign names for plotting labels
vars<-c("Order", "extent_hs", "extent_ls", "extent_ms", "Inverse_GISfrag", "Percentage_change_hs", "Percentage_change_ls", "Percentage_change_ms", "hs_average_hfp_2000", "ls_average_hfp_2000", "ms_average_hfp_2000", "Massg", "Proportion_hs", "Proportion_ls", "Proportion_ms", "mean_distance_to_matrix", "trans9617")
names<-c("Order", "High HFP extent HS","High HFP extent LS", "High HFP extent MS", "Habitat fragmentation", "High HFP change HS","High HFP change LS", "High HFP change MS", "Mean HFP value 2000 HS","Mean HFP value 2000 LS", "Mean HFP value 2000 MS", "Body mass", "Proportion HS", "Proportion LS", "Proportion MS", "Mean distance from edge to the matrix", "trans9617")

#data.imputed<-rfImpute(trans9617~., data=highHFP.drivers.nogain[,vars], iter=6, ntree=1000)


# run random forest model
#highHFP.drivers.RFnogain.noRS<- randomForest(trans9617~., data=data.imputed, ntree=1000, importance=TRUE, proximity=TRUE)

## na.action = na.omit
highHFP.drivers.RFnogain.noRS<- randomForest(trans9617~., data=highHFP.drivers.nogain[,vars], na.action = na.omit, ntree=1000, importance=TRUE)


#colour-code variables according to their class
col.imp<-c("#0072B2", "#D55E00", "#D55E00", "#D55E00", "#009E73", "#D55E00", "#D55E00", "#D55E00", "#D55E00", "#D55E00", "#D55E00", "#0072B2", "#009E73", "#009E73", "#009E73", "#009E73")

#extract importance
imp.DF<-data.frame(importance(highHFP.drivers.RFnogain.noRS))
imp.DF$col<-col.imp
imp.DF$names<-names[-length(names)]

#plot importance
par(mfrow=c(2,1),mar=c(4,14,3,2),oma=c(2,0,0,0)) 

imp.3<-imp.DF[order(imp.DF$MeanDecreaseAccuracy),]
barplot(imp.3[,3],names.arg=imp.3$names,horiz=TRUE,las=1,col=imp.3$col,main="a",xlab=colnames(imp.3)[3],xlim=c(0,80),cex.names=0.8)

imp.4<-imp.DF[order(imp.DF$MeanDecreaseGini),]
barplot(imp.4[,4],names.arg=imp.4$names,horiz=TRUE,las=1,col=imp.4$col,main="b",xlab=colnames(imp.4)[4],xlim=c(0,120),cex.names=0.8)

legend("bottomright", col=c("#0072B2", "#D55E00", "#009E73"),c("Life-history","Pressure","Environment"), ncol=1, cex=0.8, pch=15, x.intersp = 0.5, y.intersp = 0.2, text.width = 30)

