#Wine Quality Project for Datascience Capstone
# Author: Rajesh Kulkarni
# Version Date: 01/03/2022
# Note: this process may take two hours
#Start timer
start_time <- Sys.time()
#Process started
start_time
#set up variable vector for version number
v<-as.numeric(substr(R.Version()$version.string,11,13))
#limit digits displayed to 5
options(digits = 5)
#install packages if not already loaded
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
#if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")


library(plyr) ##to access version number of R in use
library(caret) ##to provide functions for training datasets
library(tidyverse)
library(dplyr) ##provide data manipualtion functionality eg mutate()
library(rpart) ##to perform regression tree analyses
library(randomForest) ##to perform randomforest analyses
library(e1071) ##additional functions to support randomForest
library(data.table)
library(knitr) ##to provide tabulation of results
#library(matrixStats)

# Wine data download

# https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv
# https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv

quantile_ratio <- 0.25 #quartile #0.2 quintile

dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", dl)

#                 col.names = c("fixedAcidity","volatileAcidity","CitricAcid","residualSugar","Chlorides","freeSulphurDioxide","totalSulphurDioxide","density","pH","sulphates","alcohol","quality"))
#Input variables (based on physicochemical tests):
#  1 - fixed acidity
#2 - volatile acidity
#3 - citric acid
#4 - residual sugar
#5 - chlorides
#6 - free sulfur dioxide
#7 - total sulfur dioxide
#8 - density
#9 - pH
#10 - sulphates
#11 - alcohol
#Output variable (based on sensory data): 
#  12 - quality (score between 0 and 10)


red_wines_init <- read.csv(dl, sep = ";") #str_split_fixed(readLines(dl), "\\;;", 12)
colnames(red_wines_init) <- c("fixedAcidity","volatileAcidity","CitricAcid","residualSugar","Chlorides","freeSulphurDioxide","totalSulphurDioxide","density","pH","sulphates","alcohol","quality")

#populate red wines according to version
if (v >= 4.0) { red_wines_init <- as.data.frame(red_wines_init) %>% mutate(
  fixedAcidity = as.numeric(fixedAcidity),
  volatileAcidity = as.numeric(volatileAcidity),
  CitricAcid = as.numeric(CitricAcid),
  residualSugar = as.numeric(residualSugar),
  Chlorides = as.numeric(Chlorides),
  freeSulphurDioxide = as.numeric(freeSulphurDioxide),
  totalSulphurDioxide = as.numeric(totalSulphurDioxide),
  density  = as.numeric(density),
  pH = as.numeric(pH),
  sulphates = as.numeric(sulphates),
  alcohol = as.numeric(alcohol),
  quality = as.character(quality))} else {
    as.data.frame(red_wines_init) %>% mutate(
      fixedAcidity = as.numeric(fixedAcidity),
      volatileAcidity = as.numeric(volatileAcidity),
      CitricAcid = as.numeric(CitricAcid),
      residualSugar = as.numeric(residualSugar),
      Chlorides = as.numeric(Chlorides),
      freeSulphurDioxide = as.numeric(freeSulphurDioxide),
      totalSulphurDioxide = as.numeric(totalSulphurDioxide),
      density  = as.numeric(density),
      pH = as.numeric(pH),
      sulphates = as.numeric(sulphates),
      alcohol = as.numeric(alcohol),
      quality = as.character(quality))                
  }

fact_red_wines<-as.data.frame(red_wines_init) %>% mutate(
  fact_fixedAcidity = cut(fixedAcidity, quantile(fixedAcidity,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_volatileAcidity = cut(volatileAcidity, quantile(volatileAcidity,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_CitricAcid = cut(CitricAcid, quantile(CitricAcid,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_residualSugar = cut(residualSugar, quantile(residualSugar,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_Chlorides = cut(Chlorides, quantile(Chlorides,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_freeSulphurDioxide = cut(freeSulphurDioxide, quantile(freeSulphurDioxide,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_totalSulphurDioxide = cut(totalSulphurDioxide, quantile(totalSulphurDioxide,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_density  = cut(density, quantile(density,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_pH = cut(pH, quantile(pH,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_sulphates = cut(sulphates, quantile(sulphates,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_alcohol = cut(alcohol, quantile(alcohol,seq(0,1,quantile_ratio)),include.lowest = TRUE))

red_wines<-select(fact_red_wines, c("fact_fixedAcidity","fact_volatileAcidity","fact_CitricAcid","fact_residualSugar","fact_Chlorides","fact_freeSulphurDioxide","fact_totalSulphurDioxide","fact_density","fact_pH","fact_sulphates","fact_alcohol","quality"))

# Validation sets will be 10% of each type of wine data
validation_ratio <- 0.1
#
if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}
test_index <- createDataPartition(y = red_wines$quality, times = 1, p = validation_ratio, list = FALSE)
train_red <- red_wines[-test_index,]
test_red <- red_wines[test_index,]

dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", dl)

white_wines_init <- read.csv(dl, sep = ";") #str_split_fixed(readLines(dl), "\\;;", 12)
colnames(white_wines_init) <- c("fixedAcidity","volatileAcidity","CitricAcid","residualSugar","Chlorides","freeSulphurDioxide","totalSulphurDioxide","density","pH","sulphates","alcohol","quality")

#populate white wines according to version
if (v >= 4.0) {
  white_wines_init <- as.data.frame(white_wines_init) %>% mutate(
    fixedAcidity = as.numeric(fixedAcidity),
    volatileAcidity = as.numeric(volatileAcidity),
    CitricAcid = as.numeric(CitricAcid),
    residualSugar = as.numeric(residualSugar),
    Chlorides = as.numeric(Chlorides),
    freeSulphurDioxide = as.numeric(freeSulphurDioxide),
    totalSulphurDioxide = as.numeric(totalSulphurDioxide),
    density  = as.numeric(density),
    pH = as.numeric(pH),
    sulphates = as.numeric(sulphates),
    alcohol = as.numeric(alcohol),
    quality = as.character(quality))} else {
    as.data.frame(white_wines_init) %>% mutate(
        fixedAcidity = as.numeric(fixedAcidity),
        volatileAcidity = as.numeric(volatileAcidity),
        CitricAcid = as.numeric(CitricAcid),
        residualSugar = as.numeric(residualSugar),
        Chlorides = as.numeric(Chlorides),
        freeSulphurDioxide = as.numeric(freeSulphurDioxide),
        totalSulphurDioxide = as.numeric(totalSulphurDioxide),
        density  = as.numeric(density),
        pH = as.numeric(pH),
        sulphates = as.numeric(sulphates),
        alcohol = as.numeric(alcohol),
        quality = as.character(quality))                
    }

fact_white_wines<-as.data.frame(white_wines_init) %>% mutate(
  fact_fixedAcidity = cut(fixedAcidity, quantile(fixedAcidity,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_volatileAcidity = cut(volatileAcidity, quantile(volatileAcidity,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_CitricAcid = cut(CitricAcid, quantile(CitricAcid,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_residualSugar = cut(residualSugar, quantile(residualSugar,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_Chlorides = cut(Chlorides, quantile(Chlorides,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_freeSulphurDioxide = cut(freeSulphurDioxide, quantile(freeSulphurDioxide,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_totalSulphurDioxide = cut(totalSulphurDioxide, quantile(totalSulphurDioxide,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_density  = cut(density, quantile(density,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_pH = cut(pH, quantile(pH,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_sulphates = cut(sulphates, quantile(sulphates,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_alcohol = cut(alcohol, quantile(alcohol,seq(0,1,quantile_ratio)),include.lowest = TRUE))

white_wines<-select(fact_white_wines, c("fact_fixedAcidity","fact_volatileAcidity","fact_CitricAcid","fact_residualSugar","fact_Chlorides","fact_freeSulphurDioxide","fact_totalSulphurDioxide","fact_density","fact_pH","fact_sulphates","fact_alcohol","quality"))

if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}
test_index <- createDataPartition(y = white_wines$quality, times = 1, p = validation_ratio, list = FALSE)
test_white <- white_wines[test_index,]
train_white <- white_wines[-test_index,]

#merge red and white initial datasets to analyse "combined" wine data

wine_data_init<-rbind(red_wines_init,white_wines_init)
fact_wine_data<-as.data.frame(wine_data_init) %>% mutate(
  fact_fixedAcidity = cut(fixedAcidity, quantile(fixedAcidity,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_volatileAcidity = cut(volatileAcidity, quantile(volatileAcidity,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_CitricAcid = cut(CitricAcid, quantile(CitricAcid,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_residualSugar = cut(residualSugar, quantile(residualSugar,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_Chlorides = cut(Chlorides, quantile(Chlorides,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_freeSulphurDioxide = cut(freeSulphurDioxide, quantile(freeSulphurDioxide,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_totalSulphurDioxide = cut(totalSulphurDioxide, quantile(totalSulphurDioxide,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_density  = cut(density, quantile(density,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_pH = cut(pH, quantile(pH,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_sulphates = cut(sulphates, quantile(sulphates,seq(0,1,quantile_ratio)),include.lowest = TRUE),
  fact_alcohol = cut(alcohol, quantile(alcohol,seq(0,1,quantile_ratio)),include.lowest = TRUE))

wine_data<-select(fact_wine_data, c("fact_fixedAcidity","fact_volatileAcidity","fact_CitricAcid","fact_residualSugar","fact_Chlorides","fact_freeSulphurDioxide","fact_totalSulphurDioxide","fact_density","fact_pH","fact_sulphates","fact_alcohol","quality"))

#combined wines
if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}
test_index <- createDataPartition(y = wine_data$quality, times = 1, p = validation_ratio, list = FALSE)
train_wine <- wine_data[-test_index,]
test_wine <- wine_data[test_index,]
Sys.time() - start_time

#Finished creating training and test sets for Combined, Red & White wine.



#DEBUG -output dataframes to XLSX files in working directory (getwd())
#if(!require(tidyverse)) install.packages("writexl", repos = "http://cran.us.r-project.org")
#library("writexl")
#write_xlsx(train_red, "train_red.xlsx")
#write_xlsx(test_red, "test_red.xlsx")
#write_xlsx(train_red, "train_white.xlsx")
#write_xlsx(test_red, "test_white.xlsx")
#write_xlsx(train_red, "train_wine.xlsx")
#write_xlsx(test_red, "test_wine.xlsx")
#write_xlsx(fact_wine_data, "fact_wine_data.xlsx")
#write_xlsx(red_wines_init, "red_wines_init.xlsx")
#write_xlsx(white_wines_init, "white_wines_init.xlsx")
#DEBUG END  
# End populating datasets  
#set up variables for numbers of rosw to quote in results section
num_rows_comb <- nrow(wine_data)
num_rows_comb_train <- nrow(train_wine)
num_rows_comb_test <- nrow(test_wine)
num_rows_red <- nrow(red_wines)
num_rows_red_train <- nrow(train_red)
num_rows_red_test <- nrow(test_red)
num_rows_white <- nrow(white_wines)
num_rows_white_train <- nrow(train_white)
num_rows_white_test <- nrow(test_white)

#Process algorithms for Combined, Red, White wine datasets in succession

#
#Combined wine data
#

#rpart combined wines
train_rpart_combined <- train(quality ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.01, 0.0001)), #seq(0, 0.05, 0.002)
                     data = train_wine)
train_rpart_combined$bestTune
y_hat_rpart_combined <- predict(train_rpart_combined, test_wine)
rpart_comb_acc<-confusionMatrix(factor(as.integer(y_hat_rpart_combined)), factor(test_wine$quality))$overall["Accuracy"]
rpart_comb_acc
plot(train_rpart_combined$finalModel, margin = 0.1)
text(train_rpart_combined$finalModel, cex = 0.3)
#combined wines rpart finished
Sys.time() - start_time

if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}

#rf combined

control <- trainControl(method="cv", number = 10)
train_rf_combined <- train(quality ~ .,
                  method = "rf",
                  ntree = 150,
                  trControl = control,
                  tuneGrid=data.frame(mtry = seq(5,20)), #seq(5,15)
                  data = train_wine)
ggplot(train_rf_combined, highlight = TRUE)
train_rf_combined$bestTune
imp_combined<-varImp(train_rf_combined)
imp_combined
y_hat_rf_combined <- predict(train_rf_combined, test_wine)
rf_comb_acc<-confusionMatrix(factor(as.integer(y_hat_rf_combined)), 
                factor(test_wine$quality))$overall["Accuracy"]
rf_comb_acc
#knn combined
if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}
train_knn_combined <- train(quality ~ ., method = "knn", data = train_wine,
                            tuneGrid = data.frame(k = seq(20, 40, 1)))
y_hat_knn_combined <- predict(train_knn_combined, test_wine)
knn_comb_acc<-confusionMatrix(factor(as.integer(y_hat_knn_combined)), 
                factor(test_wine$quality))$overall["Accuracy"]
knn_comb_acc
ggplot(train_knn_combined,highlight=TRUE)
train_knn_combined$bestTune
Sys.time() - start_time
#knn_cv combined
if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv_combined <- train(quality ~ ., method = "knn", data = train_wine,
                               tuneGrid = data.frame(k = seq(1, 20, 1)),
                               trControl = control)
y_hat_knn_cv_combined <- predict(train_knn_cv_combined, test_wine)
knn_cv_comb_acc<-confusionMatrix(factor(as.integer(y_hat_knn_cv_combined)), 
                factor(test_wine$quality))$overall["Accuracy"]
knn_cv_comb_acc
ggplot(train_knn_cv_combined,highlight=TRUE)
train_knn_cv_combined$bestTune
Sys.time() - start_time
#finished knn_cv combined

#
#red_wines
#

#rpart red
train_rpart_red <- train(quality ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.01, 0.0001)),
                     data = train_red)
train_rpart_red$bestTune
y_hat_rpart_red <- predict(train_rpart_red, test_red)
rpart_red_acc<-confusionMatrix(factor(as.integer(y_hat_rpart_red)), 
                factor(test_red$quality))$overall["Accuracy"]
rpart_red_acc
plot(train_rpart_red$finalModel, margin = 0.1)
text(train_rpart_red$finalModel, cex = 0.5)
#red rpart finished
Sys.time() - start_time

if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}

#rf red

control <- trainControl(method="cv", number = 10)
train_rf_red <- train(quality ~ .,
                  method = "rf",
                  ntree = 150,
                  trControl = control,
                  tuneGrid=data.frame(mtry = seq(5,20)),
                  data = train_red)
ggplot(train_rf_red)
train_rf_red$bestTune
imp_rf_red<-varImp(train_rf_red)
imp_rf_red
y_hat_rf_red <- predict(train_rf_red, test_red)
rf_red_acc<-confusionMatrix(factor(as.integer(y_hat_rf_red)), 
                factor(test_red$quality))$overall["Accuracy"]
rf_red_acc
# rf red finished
#knn red
train_knn_red <- train(quality ~ ., method = "knn", data = train_red,
                       tuneGrid = data.frame(k = seq(30, 50, 1)))
y_hat_knn_red <- predict(train_knn_red, test_red)
knn_red_acc<-confusionMatrix(factor(as.integer(y_hat_knn_red)), 
                factor(test_red$quality))$overall["Accuracy"]
knn_red_acc
ggplot(train_knn_red,highlight=TRUE)
train_knn_red$bestTune
#knn red finished 
Sys.time() - start_time
#knn_cv red
if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv_red <- train(quality ~ ., method = "knn", data = train_red,
                          tuneGrid = data.frame(k = seq(5, 15, 1)),
                          trControl = control)
y_hat_knn_cv_red <- predict(train_knn_cv_red, test_red)
knn_cv_red_acc<-confusionMatrix(factor(as.integer(y_hat_knn_cv_red)), 
                factor(test_red$quality))$overall["Accuracy"]
knn_cv_red_acc
ggplot(train_knn_cv_red,highlight=TRUE)
train_knn_cv_red$bestTune
#finished knn_cv red
Sys.time() - start_time
#
#white wines
#

#rpart white
train_rpart_white <- train(quality ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.01, 0.0001)),
                     data = train_white)
train_rpart_white$bestTune
y_hat_rpart_white <- predict(train_rpart_white, test_white)
rpart_white_acc<-confusionMatrix(factor(as.integer(y_hat_rpart_white)), 
                factor(test_white$quality))$overall["Accuracy"]
rpart_white_acc
plot(train_rpart_white$finalModel, margin = 0.1)
text(train_rpart_white$finalModel, cex = 0.5)
#rpart finished
Sys.time() - start_time

if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}

#rf white

control <- trainControl(method="cv", number = 10)
train_rf_white <- train(quality ~ .,
                  method = "rf",
                  ntree = 150,
                  trControl = control,
                  tuneGrid=data.frame(mtry = seq(5,20)),
                  data = train_white)
ggplot(train_rf_white, highlight = TRUE)
train_rf_white$bestTune
imp_rf_white<-varImp(train_rf_white)
imp_rf_white
y_hat_rf_white <- predict(train_rf_white, test_white)
rf_white_acc<-confusionMatrix(factor(as.integer(y_hat_rf_white)), 
                factor(test_white$quality))$overall["Accuracy"]
rf_white_acc
#white rf finished
#knn white
train_knn_white <- train(quality ~ ., method = "knn", data = train_white,
                         tuneGrid = data.frame(k = seq(1, 25, 1)))
y_hat_knn_white <- predict(train_knn_white, test_white)
knn_white_acc<-confusionMatrix(factor(as.integer(y_hat_knn_white)), 
                factor(test_white$quality))$overall["Accuracy"]
knn_white_acc
ggplot(train_knn_white,highlight=TRUE)
train_knn_white$bestTune
#white knn finished
Sys.time() - start_time
#knn_cv white
if (v <= 3.5) {
  set.seed(1)} else {  
    set.seed(1, sample.kind="Rounding")}

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv_white <- train(quality ~ ., method = "knn", data = train_white,
                            tuneGrid = data.frame(k = seq(1, 20, 1)),
                      trControl = control)
y_hat_knn_cv_white <- predict(train_knn_cv_white, test_white)
knn_cv_white_acc<-confusionMatrix(factor(as.integer(y_hat_knn_cv_white)), 
                factor(test_white$quality))$overall["Accuracy"]
knn_cv_white_acc
ggplot(train_knn_cv_white,highlight=TRUE)
train_knn_cv_white$bestTune
#finished knn_cv white
#summary rows of data
row_total_summary<-data.frame(Wine_set="Combined Complete",RowCount=num_rows_comb)
row_total_summary<-bind_rows(row_total_summary,data.frame(Wine_set="Combined Training Set",RowCount=num_rows_comb_train))
row_total_summary<-bind_rows(row_total_summary,data.frame(Wine_set="Combined Test Set",RowCount=num_rows_comb_test))
row_total_summary<-bind_rows(row_total_summary,data.frame(Wine_set="Red Wine Complete Set",RowCount=num_rows_red))
row_total_summary<-bind_rows(row_total_summary,data.frame(Wine_set="Red Wine Training Set",RowCount=num_rows_red_train))
row_total_summary<-bind_rows(row_total_summary,data.frame(Wine_set="Red Wine Test Set",RowCount=num_rows_red_test))
row_total_summary<-bind_rows(row_total_summary,data.frame(Wine_set="White Wine Complete Set",RowCount=num_rows_red))
row_total_summary<-bind_rows(row_total_summary,data.frame(Wine_set="White Wine Training Set",RowCount=num_rows_white_train))
row_total_summary<-bind_rows(row_total_summary,data.frame(Wine_set="White Wine Test Set",RowCount=num_rows_white_test))


row_total_summary%>%knitr::kable(row.names = NA, caption = "Rows per data frame")

#Summary accuracy output
accuracy_summary<-data.frame(Algorithm="RPart",Wine_set="Combined",Accuracy=rpart_comb_acc)
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="randomForest",Wine_set="Combined",Accuracy=rf_comb_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="knn",Wine_set="Combined",Accuracy=knn_comb_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="knn cv",Wine_set="Combined",Accuracy=knn_cv_comb_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="RPart",Wine_set="Red",Accuracy=rpart_red_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="randomForest",Wine_set="Red",Accuracy=rf_red_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="knn",Wine_set="Red",Accuracy=knn_red_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="knn cv",Wine_set="Red",Accuracy=knn_cv_red_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="RPart",Wine_set="White",Accuracy=rpart_white_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="randomForest",Wine_set="White",Accuracy=rf_white_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="knn",Wine_set="White",Accuracy=knn_white_acc))
accuracy_summary<-bind_rows(accuracy_summary,data.frame(Algorithm="knn cv",Wine_set="White",Accuracy=knn_cv_white_acc))
accuracy_summary%>%knitr::kable(row.names = NA, caption = "Accuracies per algorithmic model")
Sys.time() - start_time
#
imp_combined
#
imp_rf_red
#
imp_rf_white
#
#finished run
time_ended<-Sys.time()
start_time
time_ended
time_ended - start_time
