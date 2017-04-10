# library(readr)
# 
# # Read data
# dta <- read.csv("C:\\STAT636\\Sample Codesf_crime.csv")
# 
# # Number of rows
# nrows = dim(dta)[1]
# 
# # Logical for Tenderloin and for Train
# dta$ISTENDERLOIN = 0
# dta$ISTENDERLOIN[(which(dta$PdDistrict == 'TENDERLOIN'))] = 1
# dta$assess = 0;
# 
# # Shuffle data
# dta_rand <- dta[sample(nrow(dta)),]
# 
# # Reset row names
# rownames(df2) = 1:nrows
# 
# # Create 5 different dataframes for 5 different folds
# dta1 = dta_rand
# dta2 = dta_rand
# dta3 = dta_rand
# dta4 = dta_rand
# dta5 = dta_rand
# 
# # inc_idx = seq(1,nrows, by= nrows/5)
# 
# dta1$assess[c(8001:10000)] = 1
# dta2$assess[c(6001:8000)] = 1
# dta3$assess[c(4001:6000)] = 1
# dta4$assess[c(2001:4000)] = 1
# dta5$assess[c(1:2000)] = 1
# 
# write_csv(dta1,'fold1.csv')
# write_csv(dta2,'fold2.csv')
# write_csv(dta3,'fold3.csv')
# write_csv(dta4,'fold4.csv')
# write_csv(dta5,'fold5.csv')



### Cross-Validation 
# The cross-validation takes output from JMP. The 'assess' column was used for the training and validation
# split in each dataset. Assess = 0 is training, Assess = 1 is validation
# Kappas for TENDERLOIN yes
k1 = 0.1
k2 = 0.25
k3 = 0.5
k4 = 0.75
k5 = 0.9


dta1_out <- read.csv("C:\\STAT636\\Sample Code\\fold1_LDA_output_corrected.csv")
dta2_out <- read.csv("C:\\STAT636\\Sample Code\\fold2_LDA_output.csv")
dta3_out <- read.csv("C:\\STAT636\\Sample Code\\fold3_LDA_output.csv")
dta4_out <- read.csv("C:\\STAT636\\Sample Code\\fold4_LDA_output.csv")
dta5_out <- read.csv("C:\\STAT636\\Sample Code\\fold5_LDA_output.csv")


folds = list(dta1_out,dta2_out,dta3_out,dta4_out,dta5_out)
names(folds) = c('fold_1', 'fold_2', 'fold_3', 'fold_4', 'fold_5')

kappas = list(k1,k2,k3,k4,k5)
names(kappas) = c("k1","k2","k3","k4","k5")

## Calculate and record Sensitivities and Specificities
Model_Scoring_Sensitivity = data.frame()
Model_Scoring_Specificity = data.frame()

for(i in 1:5) {
  fold = folds[[i]][folds[[i]]$assess == 1,]
  
  for (j in 1:5){
    # Set Kappa threshold
    k = kappas[[j]]
    
    # Go through each of the five folds
    # Relabel based on each of the kappas
    folds[[i]]$Pred.ISTENDERLOIN <- 0
    folds[[i]]$Pred.ISTENDERLOIN[which(folds[[i]]$Prob.1.>k)] = 1
    
    # Calculate Sensitivity
    TP = dim(dta1_out[which(dta1_out$Pred.ISTENDERLOIN == 1 & dta1_out$ISTENDERLOIN == 1),])[1]
    FN = dim(dta1_out[which(dta1_out$Pred.ISTENDERLOIN == 0 & dta1_out$ISTENDERLOIN == 1),])[1]
    Sens = TP/(TP+FN)
    
    # Calulate Specificity
    TN = dim(dta1_out[which(dta1_out$Pred.ISTENDERLOIN == 0 & dta1_out$ISTENDERLOIN == 0),])[1]
    FP = dim(dta1_out[which(dta1_out$Pred.ISTENDERLOIN == 1 & dta1_out$ISTENDERLOIN == 0),])[1]
    Specif = TN/(TN+FP)
    
    Model_Scoring_Sensitivity[i,j] = Sens
    Model_Scoring_Specificity[i,j] = Specif
    
    # # Calculate Sensitivity and Specificity
    # # Sensitivity is (Predicted using Kappa) / Actual TENDERLOIN 
    # Model_Scoring_Sensitivity[i,j] = sum(folds[[i]]$Pred.ISTENDERLOIN) / sum(folds[[i]]$ISTENDERLOIN)
    # # Specificity is (What was not Predicted using Kappa) / Actual NOT TENDERLOIN
    # Model_Scoring_Specificity[i,j] = dim(folds[[i]][which(folds[[i]]$Pred.ISTENDERLOIN == 0),])[1] /dim(folds[[i]][which(folds[[i]]$ISTENDERLOIN == 0),])[1]
    
  }
}

colnames(Model_Scoring_Sensitivity) <- c(paste("kappa1",as.character(kappas[[1]]), sep = '='),paste("kappa1",as.character(kappas[[2]]), sep = '='),paste("kappa1",as.character(kappas[[3]]), sep = '='), paste("kappa1",as.character(kappas[[4]]), sep = '='), paste("kappa1",as.character(kappas[[5]]), sep = '='))
colnames(Model_Scoring_Specificity) <- c(paste("kappa1",as.character(kappas[[1]]), sep = '='),paste("kappa1",as.character(kappas[[2]]), sep = '='),paste("kappa1",as.character(kappas[[3]]), sep = '='), paste("kappa1",as.character(kappas[[4]]), sep = '='), paste("kappa1",as.character(kappas[[5]]), sep = '='))
row.names(Model_Scoring_Sensitivity) <- c(names(folds[1:5]))
row.names(Model_Scoring_Specificity) <- c(names(folds[1:5]))


# 
# ## calculate Sensitivity and Specificity for one dataframe only
# 
# k1
# 
# # If Kappa for Probability 1 is met, classify as 1
# dta1_out$Pred.ISTENDERLOIN <- 0
# dta1_out$Pred.ISTENDERLOIN[which(dta1_out$Prob.1.>k5)] = 1 #Update with new value using K
# 
# 
# # Calculate Sensitivity
# TP = dim(dta1_out[which(dta1_out$Pred.ISTENDERLOIN == 1 & dta1_out$ISTENDERLOIN == 1),])[1]
# FN = dim(dta1_out[which(dta1_out$Pred.ISTENDERLOIN == 0 & dta1_out$ISTENDERLOIN == 1),])[1]
# Sens = TP/(TP+FN)
# 
# # Calulate Specificity
# TN = dim(dta1_out[which(dta1_out$Pred.ISTENDERLOIN == 0 & dta1_out$ISTENDERLOIN == 0),])[1]
# FP = dim(dta1_out[which(dta1_out$Pred.ISTENDERLOIN == 1 & dta1_out$ISTENDERLOIN == 0),])[1]
# Specif = TN/(TN+FP)
# 
# 
# 
# Sens_Misclass = sum(dta1_out) / (sum(dta1_out$ISTENDERLOIN) +sum(dta1_out$K1_Pred.IsTENDERLOIN))
# 
# # Calulate Specificity
# Specif_Misclass = dim(dta1_out[which(dta1_out$K1_Pred.IsTENDERLOIN == 0),])[1] /dim(dta1_out[which(dta1_out$ISTENDERLOIN == 0),])[1]
# 
# 



### Alternative ways to iterate

# m = 1
# 
# for(i in 1:5) {
#   # Go through each of the five folds
#   fold = folds[[i]]
#   
#   print(paste("this is fold",names(folds[i]),sep = ''))
#   
#   for (j in 1:5){
#     # Go through each of the five kappas
#     k = kappas[[j]]
#     
#     print(paste("this is kappa",names(kappas[j]),sep = ''))
#     
#     # Relabel based on each of the kappas
#     fold$Pred.ISTENDERLOIN <- 0
#     fold$Pred.ISTENDERLOIN[which(fold$Prob.1.>k)] = 1
#     
#     Fold_Sens[m] = sum(fold$Pred.ISTENDERLOIN) / sum(fold$ISTENDERLOIN)
#     Fold_Specif[m] = dim(fold[which(fold$Pred.ISTENDERLOIN == 0),])[1] /dim(fold[which(fold$ISTENDERLOIN == 0),])[1]
#     
#     m = m+1
#     
#     print(paste("this is m counter",as.character(m)))
#   }
#   
# }
# 


