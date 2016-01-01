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
