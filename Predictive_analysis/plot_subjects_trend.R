for (i in 1:41){
  temp <- Parkinson[Parkinson$subject==i,]
  ii <- order(temp$test_time)
  temp <- temp[ii,]
  par(mfcol=c(1,2))
  plot(temp$test_time,temp$motor_UPDRS,main = paste("Subject",i),xlab = "test time", ylab = "motor_UPDRS")
  plot(temp$test_time,temp$total_UPDRS,main = paste("Subject",i),xlab = "test time", ylab = "total_UPDRS",type = "l",col="red")
  par(mfcol=c(1,1))
}


par(mfrow=c(3,4))
plot(Parkinson_norm$Jitter...,Parkinson_norm$Jitter.RAP)
plot(Parkinson_norm$Jitter...,Parkinson_norm$Jitter.DDP)
plot(Parkinson_norm$Jitter...,Parkinson_norm$Jitter.Abs.)
plot(Parkinson_norm$Jitter...,Parkinson_norm$Jitter.PPQ5)
plot(Parkinson_norm$Jitter.RAP,Parkinson_norm$Jitter.DDP)
plot(Parkinson_norm$Jitter.RAP,Parkinson_norm$Jitter.Abs.)
plot(Parkinson_norm$Jitter.RAP,Parkinson_norm$Jitter.PPQ5)
plot(Parkinson_norm$Jitter.DDP,Parkinson_norm$Jitter.Abs.)
plot(Parkinson_norm$Jitter.DDP,Parkinson_norm$Jitter.PPQ5)
plot(Parkinson_norm$Jitter.Abs.,Parkinson_norm$Jitter.PPQ5)

par(mfrow=c(3,5))
plot(Parkinson_norm$Shimmer,Parkinson_norm$Shimmer.dB.)
plot(Parkinson_norm$Shimmer,Parkinson_norm$Shimmer.APQ3)
plot(Parkinson_norm$Shimmer,Parkinson_norm$Shimmer.APQ5)
plot(Parkinson_norm$Shimmer,Parkinson_norm$Shimmer.APQ11)
plot(Parkinson_norm$Shimmer,Parkinson_norm$Shimmer.DDA)
plot(Parkinson_norm$Shimmer.dB.,Parkinson_norm$Shimmer.APQ3)
plot(Parkinson_norm$Shimmer.dB.,Parkinson_norm$Shimmer.APQ5)
plot(Parkinson_norm$Shimmer.dB.,Parkinson_norm$Shimmer.APQ11)
plot(Parkinson_norm$Shimmer.dB.,Parkinson_norm$Shimmer.DDA)
plot(Parkinson_norm$Shimmer.APQ3,Parkinson_norm$Shimmer.APQ5)
plot(Parkinson_norm$Shimmer.APQ3,Parkinson_norm$Shimmer.APQ11)
plot(Parkinson_norm$Shimmer.APQ3,Parkinson_norm$Shimmer.DDA)
plot(Parkinson_norm$Shimmer.APQ5,Parkinson_norm$Shimmer.APQ11)
plot(Parkinson_norm$Shimmer.APQ3,Parkinson_norm$Shimmer.DDA)
plot(Parkinson_norm$Shimmer.APQ11,Parkinson_norm$Shimmer.DDA)

par(mfrow=c(2,3))
plot(Parkinson$NHR,1/Parkinson$HNR)
plot(Parkinson_norm$NHR,Parkinson_norm$RPDE)
plot(Parkinson_norm$NHR,Parkinson_norm$DFA)
plot(Parkinson_norm$NHR,Parkinson_norm$PPE)
plot(Parkinson_norm$NHR,Parkinson_norm$Jitter...)
plot(Parkinson_norm$NHR,Parkinson_norm$Shimmer)

par(mfrow=c(2,2))
plot(Parkinson_norm$RPDE,Parkinson_norm$DFA)
plot(Parkinson_norm$RPDE,Parkinson_norm$PPE)
plot(Parkinson_norm$RPDE,Parkinson_norm$Jitter...)
plot(Parkinson_norm$RPDE,Parkinson_norm$Shimmer)

par(mfrow=c(2,2))
plot(Parkinson_norm$DFA,Parkinson_norm$PPE)
plot(Parkinson_norm$DFA,Parkinson_norm$Jitter...)
plot(Parkinson_norm$DFA,Parkinson_norm$Shimmer)

par(mfrow=c(1,2))
plot(Parkinson_norm$PPE,Parkinson_norm$Jitter...)
plot(Parkinson_norm$PPE,Parkinson_norm$Shimmer)

Parkinson_norm_15var <- Parkinson_norm[,c(1:8,12,16,18:22)]


#[1] "motor_UPDRS"   "total_UPDRS"   "Jitter..."    
#[8] "Shimmer"  "NHR"           "HNR"           "RPDE"          "DFA"         "PPE" 

subjects <- Parkinson[Parkinson$subject==1,]
ii <- order(subjects$test_time)
subjects <- subjects[ii,]

res.lm.1 <- lm(motor_UPDRS~Jitter...+Shimmer+NHR+RPDE+DFA+PPE, data=subjects)
plot(motor_UPDRS~test_time, data=subjects,col="green")

#draw regression line and scatter plot.
test_Parkinson <- Parkinson_norm_9var[,c("Jitter...","Shimmer","NHR","RPDE","DFA","PPE")]
par(mfrow=c(2,3))
Parkinson_names <- c("Jitter...","Shimmer","NHR","RPDE","DFA","PPE")
for(i in 1:6){
  plot(test_Parkinson[,i],Parkinson_norm_9var$motor_UPDRS,xlab = Parkinson_names[i],ylab="Motor UPDRS")
  temp_formular <- as.formula(paste("motor_UPDRS~",list(var=Parkinson_names[i])))
  temp <- lm(temp_formular,data = Parkinson_norm_9var)
  abline(temp,col="grey",lwd=2)
}

#cross-validation
install.packages("DAAG")
library(lattice,DAAG)
#draw histograph to get the distribution
par(mfrow=c(3,3))
sapply(colnames(Parkinson_norm[,c(-1,-2,-3,-4)]), function(x){
  d<- density(Parkinson_norm[,x])
  plot(d,main=x)
})
temp<-names(Parkinson_norm)
temp.exp<-""
for (i in 7:22){
  temp.exp<-paste(temp.exp,"+",temp[i])
}
temp.exp<-substr(temp.exp,4,nchar(temp.exp))
temp.exp<-paste("motor_UPDRS","~",temp.exp)
temp.exp<- as.formula(temp.exp)

res.lm.norm<- lm(motor_UPDRS ~ Jitter... + Jitter.Abs. + Jitter.RAP +
                   Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB. + 
                   Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + 
                   Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE,
                 data = Parkinson_norm)

#lm for selected features
Parkinson_names <- c("Jitter...","Shimmer","NHR","RPDE","DFA","PPE")
temp.exp2<-""
for (i in Parkinson_names){
  temp.exp2<-paste(temp.exp2,"+",i)
}
temp.exp2<-substr(temp.exp2,4,nchar(temp.exp2))
temp.exp2<-paste("motor_UPDRS","~",temp.exp2)
temp.exp2<- as.formula(temp.exp2)

res.lm.norm.6var <- lm(motor_UPDRS ~ Jitter... + Shimmer + NHR + RPDE + DFA + PPE,
                       data=Parkinson_norm)

res.cv.lm.norm <- CVlm(Parkinson_norm,res.lm.norm,m=5)

#variable selection
anova(res.lm.norm,res.lm.norm.6var)

library(MASS)
res.step <- stepAIC(res.lm.norm,direction = "both")







