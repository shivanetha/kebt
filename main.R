train=read.csv("/data/in/files/tempTrain.csv")
model<-glm(Buy~recency+Frequency+monetary+m_p+age_periods,family=quasibinomial(link='logit'),data=train)

train$pred=round(predict(model, data.frame(recency=train$recency,Frequency=train$Frequency, monetary=train$monetary, age_periods=train$age_periods, m_p=train$m_p),type='response'),2)
train$churn=round(1-train$pred,2)
train$periods=round(1/train$churn)

getCLV<-function(r,f,m,m_p,age,n,cost,periods,dr,pModel)
{  
  df<-data.frame(period=c(0),r=c(r),f=c(f),m=c(m),tp=c(m_p),age=c(age),n=c(n),value=c(0),p=c(0))
  for(i in 1:periods){
    backstep<-df[df$period==i-1,]
    nrow<-nrow(backstep)
    for(j in 1:nrow){
      r<-backstep[j,]$r
      f<-backstep[j,]$f
      m<-backstep[j,]$m
      tp<-backstep[j,]$tp
      age<-backstep[j,]$age
      n<-backstep[j,]$n
      p<-backstep[j,]$p
      p<-predict(pModel,data.frame(recency=r,Frequency=f,monetary=m,m_p=tp,age_periods=age),type='response')[1]
      buyers<-n*p
      df<-rbind(df,c(i,r,f+1,m,m_p,buyers,buyers*(m_p-cost) / (1+dr)^i))
      df<-rbind(df,c(i,r+1,f,m,m_p, n-buyers,(n-buyers)*(-cost)  / (1+dr)^i ))
    }
  }
  return(sum(df$value))
}
train=train[train$periods<10,]
for(k in 1:nrow(train))
{
  train$clv[k]=getCLV(train$recency[k], train$Frequency[k],train$monetary[k],train$m_p[k],train$age_periods[k],train$n[k],0,train$periods[k],train$Dicsount_Percentage[k],model)
}
res=data.frame(c(train))
write.csv(res,"res.csv")
write.csv(train,"train.csv")
