library(MASS)
library(data.table)

rm(list=ls())

t <- as.data.table(read.csv("hof_data.csv", header = TRUE))
#r <- lda(formula = TenderloinFlag ~ Year + Month + Time_6am + X + Y, data=t)

t <- t[sample(nrow(t)),]
folds <- cut(seq(1,nrow(t)),breaks=4,labels=FALSE)
i<-1
#Perform 10 fold cross validation
for(i in 1:4) {
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- as.data.frame(t[testIndexes, ])
  trainData <- as.data.frame(t[-testIndexes, ])
  
  rt <- lda(formula = HOF_FLAG ~ HR + H + RBI + AVG + SLG + OBP, data=trainData)
  plda <- predict(object=rt, newdata=testData)
  plda_all <- as.data.table(cbind(i, plda$posterior, plda$class, testData$HOF_FLAG))
  
  if(i==1)
  {
    pt <- plda_all
    
  }
  else
  {
    pt <- rbind (pt, plda_all)
  }
}

colnames(pt) <- c("fold","prob0","prob1","pclass","FLAG")
ptx <- pt
p <- as.data.table(c(0.1, 0.25, 0.5, 0.75, 0.90))
colnames(p) <- c("k")

for(ii in 1:nrow(p)) {
  K<-p$k[ii]
  ptx$pclass <- 0
  ptx$pclass[pt$prob1>=K] <- (1)
  if(ii==1) {
    ptt <- as.data.table(c(ptx$pclass))
  } 
  else {
    ptt <- as.data.table(cbind(ptt,ptx$pclass))
  }
  
}

colnames(ptt) <- c("P010", "P025", "P050", "P075", "P090")
pt <- cbind(pt,ptt)
N <- nrow(pt)
cm1 <- cbind("010",as.data.table(table(pt$FLAG, pt$P010)))
cm2 <- cbind("025",as.data.table(table(pt$FLAG, pt$P025)))
cm3 <- cbind("050",as.data.table(table(pt$FLAG, pt$P050)))
cm4 <- cbind("075",as.data.table(table(pt$FLAG, pt$P075)))
cm5 <- cbind("090",as.data.table(table(pt$FLAG, pt$P090)))

cm <- rbind(cm1,cm2,cm3,cm4,cm5)
colnames(cm) <- c("k","Actual","Pred","N")

roc <- as.data.table(
  rbind(
    cv.modelaccuracy(cm1,0.10,N),
    cv.modelaccuracy(cm2,0.25,N),
    cv.modelaccuracy(cm3,0.50,N),
    cv.modelaccuracy(cm4,0.75,N),
    cv.modelaccuracy(cm5,0.90,N)
  )
)

rocs <- subset(roc, y==1)

par(mfrow=c(1,2))
plot(rocs$K,rocs$sens, typ="l",xlab="Kappa",ylab="Sensitivity")
plot(rocs$K,rocs$spec, typ="l",xlab="Kappa",ylab="Specificity")
#plot(rocs$K,rocs$spec, type="n", xlimits=c(0,1), ylimits=c(0,1)) 

write.csv(cm, "hof_confusionmatrix.csv", row.names=TRUE)
write.csv(roc, "hof_roc.csv", row.names=TRUE)
write.csv(pt, "hof_predictions.csv", row.names=TRUE)

#################################################


cv.modelaccuracy <- function(ft, fk, fn) {
  #ft <- cm2
  #fk <- 0.025
  #fn <- 10000
  
  colnames(ft) <- c("K","truth","pred","n")
  #spec <- subset(ft, truth==0 & pred==0)$n/fn
  
  fn <- subset(ft, truth==1 & pred==0)$n + subset(ft, truth==1 & pred==1)$n
  sens <- if(length(subset(ft, truth==1 & pred==1)$n) == 0)
  {0} else {subset(ft, truth==1 & pred==1)$n/fn}
  
  
  fn <- subset(ft, truth==0 & pred==1)$n + subset(ft, truth==0 & pred==0)$n
  spec <- if(length(subset(ft, truth==0 & pred==0)$n) == 0)
  {0} else {subset(ft, truth==0 & pred==0)$n/fn}
  
  foutput <- rbind(cbind(1, fk, sens, spec),cbind(0, fk, spec, sens))
  colnames(foutput) <- c("y", "K","sens","spec")
  foutput
}



