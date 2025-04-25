library(raster)
library(rgdal)
library(maptools)
library(sp)
library(randomForest)
library(rpart)
library(stringr)
library(caret)
library(kernlab)
library(gbm)

###############################################################################################
######### Digital soil prediction script. Example model prediction for parameter sand #########
###############################################################################################

## Specify the percentage that will be used for training. the remaining will be used for testing
split <- 0.8

#### Specify path to a directory that results will be written to
outdir <- "C:/Soil/Stats/"

###### Import table with response and predictors
table_sand <- read.table("C:/Soil/Sand_inner.txt", header = T, sep = ";", dec = ".")

table_sand <- table_sand[complete.cases(table_sand),]

#### Partition the table into training and testing based on earlier stated proportion. Seed is set to
#### ensure that same split is done for each soil parameter
trainIndex <- createDataPartition(c(table_sand["Sand"], recursive=T), p=split, list = F)
  
#### Training samples
training <- table_sand[trainIndex,]
  
#### Testing samples
testing <- table_sand[-trainIndex,]
  
#### Define parameters for controling model training. 10 fold resampling with 5 repeats
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = T)
  
#### Train models. Four models will be trained - linear regression, random forest, support vector machines,
#### and Stochastic Gradient Boosting. Seed is set to ensure same samples are used in each model
  

#### Train linear regression model. "var" is the parameter being modelled
set.seed(246)
modLM <- train(Sand~., data = training, method="lm", trControl=ctrl)
save(modLM, file=str_c(outdir, "modLM", ".Rdata"))
  
#### Train Random Forest
set.seed(246)
modRF <- train(Sand~., data = training, method="rf", trControl=ctrl, importance=T)
save(modRF, file=str_c(outdir, "modRF", ".Rdata"))
  
#### Train support vector machines
set.seed(246)
modSVM <- train(Sand~., data = training, method="svmLinear", trControl=ctrl)
save(modSVM, file=str_c(outdir, "modSVM", ".Rdata"))
  
#### Train Stochastic Gradient Boosting
set.seed(246)
modGBM <- train(Sand~., data = training, method="gbm", trControl=ctrl, verbose=F)
save(modGBM, file=str_c(outdir, "modGBM", ".Rdata"))
  
#### Results - internal validation. Extract accuracy measures (RMSE, RSquared) for all four models 
results <- summary(resamples(list(LM=modLM, RF=modRF, SVM=modSVM, GBM=modGBM)))
  
### summarize the results
statistics <- results$statistics
  
write.table(statistics, file = str_c(outdir, "statistics.txt"), sep = ";", dec = ".")
  
# boxplots of results
bwplot(resamples(list(LM=modLM, RF=modRF, SVM=modSVM, GBM=modGBM)))
  
#### validation using testing data in smaller catchment

#### predict testing data with generated models, e.g. RF  
pred <- as.data.frame(predict(modRF, testing))
    
#### extract the corresponding observed data for parameter under consideration, i.e. sand
obs <- testing["Sand"]

#### Extract RMSE amd R2
predVals_inner <- postResample(pred, obs)

#### extract RMSE
rmse_inner <- predVals_inner[[1]]

#### extract R2
r2_inner <- predVals_inner[[2]]

#### calculate sMAPE
sMAPE_inner <- sum(abs(obs-pred)/((obs+pred)/2))/nrow(pred)
    

  
#### validation using testing data in bigger catchment
#### Perform same process as above, but with validation data outside small catchment

##### Import sample table in bigger catchment
table_sand_val <- read.table("C:/Soil/Sand_outer.txt", header = T, sep = ";", dec = ".")

table_sand_val <- table_sand_val[complete.cases(table_sand_val),]
  
#### Predict parameter using model, e.g. RF    
pred <- as.data.frame(predict(modRF, table_sand_val))

#### Extract the corresponding observed data for parameter under consideration

obs <- table_sand_val["Sand"]
    
#### Extract statistics    
predVals_outer <- postResample(pred, obs)

#### Extract RMSE
rmse_outer <- predVals_outer[[1]]

### Extract R2
r2_outer <- predVals_outer[[2]]

##### Calculate sMAPE
sMAPE_outer <- sum(abs(obs-pred)/((obs+pred)/2))/nrow(pred)

