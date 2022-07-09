library(readr)
library(dplyr)
library(caret)
library(pROC)

RecallBest <- function(Y, prTr) {
  
  j<-sort(unique(prTr))
  
  
  Rec2<-NULL
  for (i in 1:length(j))  {
    
    predC<-ifelse(prTr>=j[i],1,0)
    Rec2<-c(Rec2,MLmetrics::Recall (Y, predC, positive="0")+MLmetrics::Recall (Y, predC,  positive="1"))
    
    
  }
  return (j[which.max(Rec2)])
}

#setwd("E:/R/_cp2022")
train<-read_csv("train.csv")
test<-read_csv("test_dataset_test.csv")

train2<-rbind(train[,1:33],test)
train2$Пол[is.na(train2$Пол)]<-"Ж"


#Select features
#tr<-train2[1:955,]
#Y0<-as.factor(train$`Прочие заболевания сердца`)
#naiveWrapper(tr,Y0, size=32, iterations = 500)

train2<-train2%>%select("Пол",  "Образование", "Вы работаете?", "Сахарный диабет", 
                        "Регулярный прим лекарственных средств")
#############
#Conjunction
i<-2
j<-3
train3<-data.frame(new=(apply (train2[,-c(i,j)],1, function (x) paste(x, collapse="_"))), cbind(train2[,c(i,j)]))


tabl<-as.data.frame(table(train3$new))
train3$new[train3$new%in%as.character(tabl$Var1[tabl$Freq<=5])]<-"rare"
train3<-train3%>%mutate_all(as.factor)

tr<-data.frame(train3[1:955,])
tst<-data.frame(train3[956:1593,])

train<-as.data.frame(train)

#LOOCV
predb<-NULL
ans2<-NULL
for (k in 1:5)
{
  Y<-train[,k+34] 
  tr$Y<- Y

  pred0<-NULL
  for (i in 1:955) {
    tr0<-tr[-i,]
    val<-tr[i,]
    #LOOCV models
    model<-glm(Y~., data=tr0,family=binomial)
    pred0<-c(pred0,predict(model,val, type="response"))
  }
  
  predBase<-ifelse(pred0>0.5,1,0)
  Rec1<-(MLmetrics::Recall(Y, predBase, "0")+MLmetrics::Recall(Y, predBase, "1"))/2
  Rec1[is.na(Rec1)]<-0.5
  
  #thres tuning
  thres<-RecallBest(Y, pred0)

  predCor<-ifelse(pred0>=thres,1,0)
  Rec2<-(MLmetrics::Recall(Y, predCor, "0")+MLmetrics::Recall(Y, predCor, "1"))/2
  
#Monitoring  
#  print (c(Rec1,Rec2, thres))
#  ans2<-rbind(ans2, c(Rec1,Rec2, thres))
#  write.table(t(ans2), "clipboard",dec=",", row.names = F, col.names = F)

  #Model
  model<-glm(Y~., data=tr,family=binomial)
  
  prTst<-predict(model,tst, type="response")
  predb[[k]]<-ifelse(prTst>=thres,1,0)
  }

sam<-read_csv("sample_solution.csv")
sam$`Артериальная гипертензия`<-predb[[1]]
sam$`ОНМК`<-predb[[2]]
sam$`Стенокардия, ИБС, инфаркт миокарда`<-predb[[3]]
sam$`Сердечная недостаточность`<-predb[[4]]
sam$`Прочие заболевания сердца`<-predb[[5]]

write_csv(sam, "final_0707.csv")