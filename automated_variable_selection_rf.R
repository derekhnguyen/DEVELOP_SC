#SET WORKSPACE AND LIBRARIES
library(corrplot)
library(VSURF)
library(rfUtilities)
library(dplyr)
library(randomForest)
library(ggplot2)

getwd()

#VARIABLE SELECTION STARTING WITH ALL PREDICTOR VARIABLES
data <- read.csv('train_dat_random_water_pt_mod2m_buff_061819.csv')
names(data)

set.seed(123)
dat_v<-dplyr::filter(data, !grepl('Open Water', data$communityc))  #isolate all the data that is not open water
dat_w<- data %>% filter(data$communityc == "Open Water")      #isolate the open water points
water200<-sample_n(dat_w, 200)           #take random sample of 200 from the water points subset
data<-rbind(dat_v, water200)             #merge the non water points with the 200 randomly selected water points
nrow(data)

rastercolumns <- data[, 1:32]


#IDENTIFY PREDICTORS AND RESPONSE VARIABLE IN THE CSV
all_predictors <- data %>% dplyr::select(1:32)

#all_predictors <-read.csv('water_pt_mod_pred_only.csv')
Response_Variable <- as.factor(data$presence)

#RUN VARIABLE SELECTION ANALYSIS (VSURF, RFUTILITIES)
VSURF_all <- VSURF(x = all_predictors, y = Response_Variable)


################################# addjusting content based on automated varaible selection process 

# define predictor list based on Run
inputPredictors <- VSURF_all$varselect.pred
#
# # ordered predictors from our variable selection
predictors <- all_predictors[,c(inputPredictors)]
# Calculate correlation coefficient matrix
correlation <-cor(predictors, method="pearson")
#change self correlation value

# #define the list of top 15 predictors
varNames <- colnames(correlation)
#loop through the top 5 predictors to remove correlated varables.
for( i in 1:5){
  print(i)
  # Test for correlations with predictors
  vars <- correlation[(i+1):nrow(correlation),i] > 0.7 | correlation[(i+1):nrow(correlation),i] < -0.7
  # Select correlated values names
  corVar <- names(which(vars == TRUE))
  #test is any correlated variables exist
  if(length(corVar) >0 ){
    # loop through the list of correlated variables
    for(j in corVar){
      # remove varable name from variable list
      varNames <- varNames[which(varNames != j)]
      print(varNames)
      # remove row from correlation dataframe ### Indexing on the this is not working at the moment. Leave out for the time being.
      # correlation <- correlation[!vars,]
      # print(dim(correlation))
      print(paste0("the variable ", j, " was removed"))
    }
    
  } else{
    print("no correlated Varaibles")
  }
}


selected_predictors <- all_predictors[,varNames]



# ### vsurf predict is the highest level of variable selection. 
# indexList <- VSURF_all$varselect.interp #This prints the column numbers representing the top predictors
# 
# names(all_predictors) #find the names of the columns that match up with the numbers from VSURF
# 
# top<-cbind("VV_range", "B2_T1", "ndviT2", "B2_T2", "B5_T2", "VH_range", "T2med_VV",
#            "L8_T1_TCW", "L8_T2_TCG", "L8_T1_TCG","T1med_VV","B3_T1","ndviT1")
# top
# 
# all_predictors<-subset(all_predictors, select=top)
# head(all_predictors)
# 

############################################################################################
###   Use rfUtilies to reorder predictors, check and drop highly correlated variables    ###
############################################################################################
# 
# varimportance_cov <- rf.modelSel(all_predictors, as.factor(dat$presence), imp.scale="se")
# plot(varimportance_cov)
# 
# varimportance_cov <- cbind(rownames(varimportance_cov$sel.importance), varimportance_cov$sel.importance)
# rownames(varimportance_cov) <- NULL
# colnames(varimportance_cov) <- c("name", "imp")
# 
# varimportance_cov_ord <- varimportance_cov[order(-varimportance_cov$imp),]
# varimportance_cov_ord
# 
# # drop columns that are not as important based on rfmodelsel
# raster_cov_names <- varimportance_cov_ord$name[1:13]
# raster_cov_names
# rastercolumns_cov <- rastercolumns[,as.character(raster_cov_names)]
# 
# # calculate correlation coefficient matrix
# correlation <-cor(rastercolumns_cov, method="pearson")
# 
# # plot the correlation. the darker the number, the more correlated the two variables
# corrplot(correlation,method="number")
# 
# #drop correlated variables, plot correlation of remaining variables
# rastercolumns_cov <- rastercolumns[,as.character(raster_cov_names)[-c(6,9,11,12,13)]]
# correlation <-cor(rastercolumns_cov, method="pearson")
# corrplot(correlation,method="number")
# 
# # make data set with just presence and raster columns of choice
# pr<-as.factor(dat$presence) # use as.factor() to have random forest run a classification instead of regression
# data_cov_model <- cbind(rastercolumns_cov, pr)


###########################################################
#############  RANDOM FOREST  #############################
###########################################################
pr <- as.factor(data$presence)
selPred <- selected_predictors

# Now let's restrict our final predictors to those that are most important (Change manually) 
set.seed(123)
rf_model1 = randomForest(pr ~ ., data=selPred, importance = TRUE, ntree = 2000, mtry = 2)
rf_model1

# LONG STORY SHORT: higher values of %IncMSE mean that a predictor is more important relative to other predictors
importance(rf_model1)
varImpPlot(rf_model1)

#Check training accuracy statistics

#install.packages('caret')
library(caret)

predicted_training<-rf_model1$predicted
observed_training <- pr
accuracy_training <- confusionMatrix(data=predicted_training,
                                     reference=observed_training,
                              positive = "1") #Throws an error, ignore & print anyway
accuracy_training




####Class accuracy 

commClass <- unique(data$communityc)

df <-data.frame(matrix(ncol = 9, nrow = length(commClass)))
colnames(df) <- c('class', 'nponits' ,'PCC',
                  "sensitivty","specificity",
                  "TP", "FP", "FN", "TN") 
n=1
for(i in commClass){
  print(i)
  pre <- as.numeric(as.character(predicted_training))
  obs <- as.numeric(as.character(observed_training))
  commVect <-data$communityc == i
  test1 <- confusionMatrix(data = predicted_training[commVect], 
                           reference = observed_training[commVect],
                           positive = "1")
  confusion <- test1$table
  
 #accuracy_training <- accuracy(x = predicted_training[commVect], y = observed_training[commVect])
  TP <- confusion[2,2]
  FP <- confusion[2,1]
  FN <- confusion[1,2]
  TN <- confusion[1,1]
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  pcc <- 100*(sum(TP,TN) / sum(TP,TN,FP,FN))
  df[n,] <- c(i, length(pre[data$communityc == i]), pcc,
              sensitivity, specificity, TP, FP,FN,TN)

  n <- n+1
}
View(df)
#write.csv('statsbyclass.csv')


###########################################################
#############  VALIDATION #################################
###########################################################

validation<-read.csv('validation.csv')
predicted_testing<- predict(rf_model1, validation, type = "class")
observed_testing<-validation$presence

accuracy_testing <- accuracy(x=predicted_testing,
                             y=observed_testing) #Throws an error, ignore & print anyway
accuracy_testing

xt<-cbind(validation,predicted_testing)
write.csv(xt,file="validation_output.csv")
