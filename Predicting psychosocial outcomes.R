#START
rm(list=ls()) # clear the workspace
##################################################################################################################################################################
#Predicting psychosocial outcomes at discharge from ABI neurorehbailitation
#Scottish Data, one site (2009-2020) 236 patients

.libPaths() #check which library R is writing to
#If needed, swap their order so the C drive is first (must be running R in admin mode)
myPaths <- .libPaths()   # get the paths
myPaths <- c(myPaths[2], myPaths[1])  # switch them
.libPaths(myPaths)  # reassign them




#Install packages if needed
install.packages("devtools") 
install.packages("doParallel")
install.packages("readr") 
install.packages("caret") 
install.packages("corrplot") 
install.packages("e1071") 
install.packages("randomForest") 
install.packages("pROC") 
install.packages("gtools") 
install.packages("tidyverse") 
install.packages("glmnet")
install_git("https://github.com/BavoDC/CalibrationCurves")
install_git("https://github.com/ddsjoberg/dca")
install.packages("MLeval")


#load required libraries - 
library(doParallel) #parallel processing
library(readr) #loading csv
library(caret) #modelling meta package
library(corrplot) #correlation stuff
library(glmnet) #elastic net
library(e1071) #linear svm
library(randomForest) #random forest
library(pROC) #ROC methods
library(gtools) #permute
library(devtools) #install from github
library(tidyverse) # sorting data
library(CalibrationCurves) #calibration curves
library(dca) #decision curves
library(MLeval) # get model metrics


#enable multicore which roughly halfs time for analysis runs
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

options(max.print=1000000)

#don't use scientific notation (revert back with options(scipen=0)
options(scipen=999)
options(digits = 4)

setwd("C:/Users/student/Documents/R documents") #choose where to store data



##################################################################################################################################################################
#Data Inspection
#

#load data - assumes ABI_outcomes.csv in working directory as set earlier
#missing is blank
ABI_outcomes = read_csv("ABI_outcomes.csv")

#look at the structure of the data
str(ABI_outcomes)
#tell R which columns are factors (categories)
#change character cols to factor

#individually...
#ABI_outcomes$Cohort = as.factor(ABI_outcomes$Cohort)

ABI_outcomes$gender = as.factor(ABI_outcomes$gender)
ABI_outcomes$diagnosis = as.factor(ABI_outcomes$diagnosis)
ABI_outcomes$preinjury_psychosis = as.factor(ABI_outcomes$preinjury_psychosis)
ABI_outcomes$drug_dependence = as.factor(ABI_outcomes$drug_dependence)
ABI_outcomes$alcohol_abuse = as.factor(ABI_outcomes$alcohol_abuse)
ABI_outcomes$multiple_trauma = as.factor(ABI_outcomes$multiple_trauma)
ABI_outcomes$other_medical_condition = as.factor(ABI_outcomes$other_medical_condition)
ABI_outcomes$outcome_accommodation = as.factor(ABI_outcomes$outcome_accommodation)

ABI_outcomes$outcome_supervision = as.factor(ABI_outcomes$outcome_supervision)
ABI_outcomes$outcome_occupation = as.factor(ABI_outcomes$outcome_occupation)
                                
#or use custom method to search for character columnes and convert to factors...
# custom methods
csv_to_factor <- function(ABI_outcomes.csv)
{
  cols_char_csv = colnames(ABI_outcomes.csv[, sapply(ABI_outcomes.csv, class) == 'character'])
  for (i in seq(1:length(cols_char_csv)))
  {
    ABI_outcomes.csv[[cols_char_csv[i]]] = as.factor(ABI_outcomes.csv[[cols_char_csv[i]]])
  }
  return(ABI_outcomes.csv)
}
ABI_outcomes = csv_to_factor(ABI_outcomes)
str(ABI_outcomes)

#Look at columns and missing data by study
#Percentage Missing Values Per Dataset Per Column

#(sum(is.na(x))/length(x))*100 sum of NAs in column #replace x with column name $ #number of rows in column (x100 for percent)

(sum(is.na(ABI_outcomes$simd_rank))/length(ABI_outcomes$simd_rank))*100
(sum(is.na(ABI_outcomes$gender))/length(ABI_outcomes$gender))*100
(sum(is.na(ABI_outcomes$age_at_injury_weeks))/length(ABI_outcomes$age_at_injury_weeks))*100
(sum(is.na(ABI_outcomes$age_at_admission_weeks))/length(ABI_outcomes$age_at_admission_weeks))*100
(sum(is.na(ABI_outcomes$days_between_injury_admission))/length(ABI_outcomes$days_between_injury_admission))*100
(sum(is.na(ABI_outcomes$diagnosis))/length(ABI_outcomes$diagnosis))*100
(sum(is.na(ABI_outcomes$preinjury_psychosis))/length(ABI_outcomes$preinjury_psychosis))*100
(sum(is.na(ABI_outcomes$drug_dependence))/length(ABI_outcomes$drug_dependence))*100
(sum(is.na(ABI_outcomes$alcohol_abuse))/length(ABI_outcomes$alcohol_abuse))*100
(sum(is.na(ABI_outcomes$multiple_trauma))/length(ABI_outcomes$multiple_trauma))*100
(sum(is.na(ABI_outcomes$other_medical_condition))/length(ABI_outcomes$other_medical_condition))*100
(sum(is.na(ABI_outcomes$hads_a))/length(ABI_outcomes$hads_a))*100
(sum(is.na(ABI_outcomes$hads_d))/length(ABI_outcomes$hads_d))*100
(sum(is.na(ABI_outcomes$baseline_mpai_total))/length(ABI_outcomes$baseline_mpai_total))*100
(sum(is.na(ABI_outcomes$baseline_mpai_abilties))/length(ABI_outcomes$baseline_mpai_abilties))*100
(sum(is.na(ABI_outcomes$baseline_mpai_adjustment))/length(ABI_outcomes$baseline_mpai_adjustment))*100
(sum(is.na(ABI_outcomes$baseline_mpai_participation))/length(ABI_outcomes$baseline_mpai_participation))*100
(sum(is.na(ABI_outcomes$wais_vci))/length(ABI_outcomes$wais_vci))*100
(sum(is.na(ABI_outcomes$wais_pri))/length(ABI_outcomes$wais_pri))*100
(sum(is.na(ABI_outcomes$wais_wmi))/length(ABI_outcomes$wais_wmi))*100
(sum(is.na(ABI_outcomes$wais_psi))/length(ABI_outcomes$wais_psi))*100
(sum(is.na(ABI_outcomes$wais_fsiq))/length(ABI_outcomes$wais_fsiq))*100
(sum(is.na(ABI_outcomes$topf_vci))/length(ABI_outcomes$topf_vci))*100
(sum(is.na(ABI_outcomes$topf_pri))/length(ABI_outcomes$topf_pri))*100
(sum(is.na(ABI_outcomes$topf_wmi))/length(ABI_outcomes$topf_wmi))*100
(sum(is.na(ABI_outcomes$topf_psi))/length(ABI_outcomes$topf_psi))*100
(sum(is.na(ABI_outcomes$topf_fsiq))/length(ABI_outcomes$topf_fsiq))*100
(sum(is.na(ABI_outcomes$executive_functioning))/length(ABI_outcomes$executive_functioning))*100
(sum(is.na(ABI_outcomes$memory))/length(ABI_outcomes$memory))*100
(sum(is.na(ABI_outcomes$neuro_administered))/length(ABI_outcomes$neuro_administered))*100

#TOPF has highest level of mising data in whole dataset 

#Pattern of missingness is not really testable but it is assumed to be not missing at random (related to outcome of interest)
#SPSS analysis (separate) showed no difference of missingness between those with and without primary outcome so bias is less likely due to missing outcome
#However, results of imputation are often nearly as unbiased with not missing at random (NMAR) data as with MAR 
#(see Schafer JL, Graham JW. Missing Data: Our View of the State of the Art. Psychological Methods 2002, Vol. 7, No. 2,147-177).

#Get all the columns
colnames(ABI_outcomes)

#Look at the pattern of data by column - descriptive stats - by simd_rank column

(ABI_outcomes$simd_rank)  #by column name

  summary(ABI_outcomes$simd_rank) #summary

(na.action = na.omit) #Remove NAs
  
  (ABI_outcomes$simd_rank)
  
    length(ABI_outcomes$simd_rank)#length 
  
  (na.action = na.omit)#Remove NAs
    
#Change simd_rank to other columns, e.g:

    (ABI_outcomes$memory)  #by column name
    
    summary(ABI_outcomes$memory) #summary
    
    (na.action = na.omit) #Remove NAs
    
    (ABI_outcomes$memory)
    
    length(ABI_outcomes$memory)#length 
    
    (na.action = na.omit)#Remove NAs
    
#################################################################################################################################################################
#Preprocessing 


#Start with outcome_accommodation (accomodation status at discharge)
#
#Change and repeat for each outcome variable

#Training Data Preprocessing #taking out outcomes to not do preprocessing on 
Training_ABI_accommodation = ABI_outcomes[ ,!(colnames(ABI_outcomes) %in% c("outcome_length_of_stay_weeks","outcome_accommodation","outcome_participation","outcome_supervision","outcome_occupation","outcome_qol"))]

#Dummy code (not outcomes - required to remain factor) 
dummies = dummyVars(~ ., data = Training_ABI_accommodation, fullRank = T)#rules for dummy coding
Training_ABI_accommodation = data.frame(predict(dummies, newdata = Training_ABI_accommodation))#second data frame using the rules just created

#remove zero and near zero variance columns
nzv_cols = nearZeroVar(Training_ABI_accommodation)
if(length(nzv_cols) > 0) Training_ABI_accommodation = Training_ABI_accommodation[, -nzv_cols]

#Look at correlation - impute just for correlation as not missing at random - we will impute properly during cross-validation
#
preProcessImp = preProcess(Training_ABI_accommodation, method = c("center","scale","knnImpute")) #knn requires standardisation, default k=5
Training_Acc_Imp = data.frame(predict(preProcessImp, Training_ABI_accommodation))



#Make correlation matrix (Pearson is default)
cor_all_Acc = cor(Training_Acc_Imp)
pdf("corPlot_Acc.pdf", width = 25, height = 25)
corrplot(cor_all_Acc, method = "number")
dev.off()

#Remove any columns with >0.7 correlation from training
hc = findCorrelation(cor_all_Acc, cutoff=0.7)  
hc = sort(hc)
Training_ABI_accommodation = Training_ABI_accommodation[,-c(hc)] #Remove highly correlated columns (hc) from defined cut off values

#Add factor outcome back in 
Training_ABI_accommodation$outcome_accommodation = ABI_outcomes$outcome_accommodation

#remove na from outcome - not allowed to impute outcome - leak information from training to test
Training_ABI_accommodation = Training_ABI_accommodation[which(!is.na(Training_ABI_accommodation[, "outcome_accommodation"])),]

#remove columns with more than 20% missing data default
Training_ABI_accommodation = Training_ABI_accommodation[, colMeans(is.na(Training_ABI_accommodation)) <= 0.2] #change to .5 for secondary analyses to impute data with >50% complete data


##################################################################################################################################################################
#Model Specification (some done before) & Estimation for accommodation
#

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 10 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification #mine have repeats to 10 for larger sets >100p's
control = trainControl(method="repeatedcv", number=5, repeats=10, classProbs=TRUE, savePredictions = TRUE, verboseIter = TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")


#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
#caret does that for you to not leak data across the sets 
preProcess = c("center", "scale","knnImpute") #default k=5

set.seed(987)#make replicable (splits are random)#therefore in publication people will have same results 
ABI_acc_mod = train(outcome_accommodation ~ ., #Outcome against all predictors
                      data = Training_ABI_accommodation, #data
                      method ="glmnet", #elastic net #or change to rf etc. caret website has a list of available models and what they're called
                      metric="ROC", #performance metric - ROC for classification problems
                      tuneLength = 10, #for elastic net, how big a grid of alpha and lambda - 10*10 - only works correctly for selectionFunction = "best" #or for rf how many trees growing
                      preProc = preProcess, #Our preprocessing for each train fold
                      trControl = control, #Our tuning method rules
                      na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Resampling results across tuning parameters
#training performance shouldn't be reported as overoptimistic 
ABI_acc_mod

#plot performance against alpha for each amount of lambda
plot(ABI_acc_mod)
#plot regularisation paths for final model alpha as lambda increases
plot(ABI_acc_mod$finalModel, label = T,xvar = "lambda")
#plot best lambda
abline(v=log(ABI_acc_mod$bestTune$lambda))
#Zoom in
plot(ABI_acc_mod$finalModel,label = T, xvar = "lambda", xlim = c(-2.5,-0.5), ylim = c(-0.5,0.5))
abline(v=log(ABI_acc_mod$bestTune$lambda))

#Look at final coefficients #svm or rf do not get coefficients
coef(ABI_acc_mod$finalModel, ABI_acc_mod$bestTune$lambda)
#Look at odds ratios - no SE provided as estimates biased due to regularisation
exp(coef(ABI_acc_mod$finalModel, ABI_acc_mod$bestTune$lambda))
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_acc_mod)#or rank by other things depending on algorithm 
#Plot them
varImp(ABI_acc_mod)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()

#Try unregularised GLM to compare
set.seed(987)#make replicable (splits are random)
ABI_acc_mod_GLM = train(outcome_accommodation ~ ., #Outcome against all predictors
                          data = Training_ABI_accommodation, #data
                          method ="glm", #elastic net
                          metric="ROC", #performance metric - ROC for classification problems
                          preProc = preProcess, #Our preprocessing for each train fold
                          trControl = control, #Our tuning method rules
                          na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

ABI_acc_mod_GLM
#check variable importance
varImp(ABI_acc_mod_GLM)


#get stats for training model - overly optimistic
x <- evalm(ABI_acc_mod_GLM)
## get roc curve plotted in ggplot2
roc_acc_glm <- x$roc
## get AUC and other metrics
x$stdres
#get calibration curve
x$cc
# 


#Try linear kernel SVM to compare
#SVM does not feature select but works ok with high dimensions
set.seed(987)
ABI_acc_mod_lSVM = train(outcome_accommodation ~ ., #Outcome against all predictors
                           data = Training_ABI_accommodation, #data
                           method ="svmLinear2", #SVM linear kernel
                           metric="ROC", #performance metric - ROC for classification problems
                           tuneLength = 10, #how many Costs 
                           preProc = preProcess, #Our preprocessing for each train fold
                           trControl = control, #Our tuning method rules
                           na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Apparent performance? ROC 0.7905
ABI_acc_mod_lSVM
plot(ABI_acc_mod_lSVM)

#SVM variable importance # can inspect variable importance but is not as relevant as with GLM and RF as does not use all data points
ABI_acc_mod_lSVMImp <- varImp(ABI_acc_mod_lSVM, scale = FALSE)
ABI_acc_mod_lSVMImp

# or alternative methodABI_acc_mod_lsvmroc_imp2 <- varImp(ABI_acc_mod_lSVM, scale = FALSE)
# ABI_acc_mod_lsvmroc_imp2

#Try random forest to compare
#Feature selects
set.seed(987)
ABI_acc_mod_rf = train(outcome_accommodation ~ ., #Outcome against all predictors
                         data = Training_ABI_accommodation, #data
                         method ="rf", #random forest
                         metric="ROC", #performance metric - ROC for classification problems
                         tuneLength = 10, #how many variables available to be randomly sampled
                         ntree = 500, #how many trees to grow
                         preProc = preProcess, #Our preprocessing for each train fold
                         trControl = control, #Our tuning method rules
                         na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Apparent performance? best performing so far
ABI_acc_mod_rf
plot(ABI_acc_mod_rf)
#Look at variable importance
#Intuitively, the random shuffling means that, on average, the shuffled variable has no predictive power. 
#This importance is a measure of by how much removing a variable decreases accuracy, 
#and vice versa - by how much including a variable increases accuracy.
varImp(ABI_acc_mod_rf)
#Plot them
varImp(ABI_acc_mod_rf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()

##How does the feature selection compare?
# Both LR and RF have neuro administered and baseline MPAI adjustement for the most predictive value for accommodation, 
# with svm identifying neur_administered, executive functioning and adjustment



##################################################################################################################################################################


##################################################################################################################################################################
#Internal Validation for accommodation for elastic net, rf and svm
#
#5x Repeated 5-fold nested 20x repeated 5-fold cross-validation!
#
#repeat outer 5 fold cv loop 5 times
#Takes 10 to 15mins
#
#
#
#
#repeating the same thing we did above (20x repeated 5-fold cross-validation for a grid of 10*10 alpha/lambdas) 5x5 or 25 times which takes 25 times as long!
#building 5*5*20*5*100 or 250,000 models! On my relatively slow internet and laptop - ~15mins

#Custom method for glmnet
#
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess,
                            method = "glmnet") #what preprocessing on the fly?
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    #createFolds splits the data into k groups (defaults to 10 groups, & as list) 
    #when returnTrain = TRUE, the values returned are the sample positions corresponding to the data used during training, 
    #returns a list or matrix of row position integers corresponding to the training data
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #lapply returns a list of the same length as splits to results, each element of which is the result of applying function to the corresponding element of splits, dat = Training_ABI_Accommodation is additional argument for function, the result of the function is the data.frame created for each split. This sorts out the ordering problem when collating the results I assume
    #split number (row position integer) becomes x in function
    #holdout is a vector of numbers from 1 to the number of rows except those with where the row numbers are in x (no duplicates or attribute names due to unique() ). N.B. the rows in x are those used in the training data with the held out removed (returnTrain is True) so this just recreates held out
    #the data.frame is the combination of the index vector (the row numbers for the holdout data) and the obs vector (the Training_ABI_accommodationp$outcome_accommodation values corresponding to the index)
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #seq generates the sequence 1, 2, ..., length(splits) for each of the 5 separate 5ths of the data
    #having two square braces only gives the dataframe column
    #create your model with the 4/5 of data with the 1/5 removed (this will happen 5 times in the manual cv outer loop. The inner repeatedcv loop is specified within the train method tuneLength. Caret's default for GLMNet is to first work out the lambda range using GLMNet for an alpha of 0.5 then set up a tune grid with n of these lambdas derived from GLMnet but n alphas. Here n = 10.
    # a column of predictions (pred) created by the testing the model on the held out 1/5 is added to the corresponding vector of the same indices and the actual observations (obs)
    #model added to mods vector for use later
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10-15 mins roughly...
results_mods_accommodation = nestedRepeatedCV(dataset = Training_ABI_accommodation,outcomeVariable = "outcome_accommodation", grid = F, control = control, preProcess = preProcess, tuneLength = 10, method = "glmnet")
#and for unregularised GLM to compare (with for nested cross validation- will tune)
results_mods_acc_GLM = nestedRepeatedCV(dataset = Training_ABI_accommodation,outcomeVariable = "outcome_accommodation", grid = F, control = control, preProcess = preProcess, tuneLength = 10,method = "glm")

#results for elastic net
results_accommodation = results_mods_accommodation$results
mods_accommodation = results_mods_accommodation$mods

results_accommodation
mods_accommodation

#results for GLM
results_acc_GLM = results_mods_acc_GLM$results
mods_acc_GLM = results_mods_acc_GLM$mods

results_acc_GLM
mods_acc_GLM

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

#for elastic net
results_accommodation_seq = combineResultsInSequence(results_accommodation)
results_accommodation_seq

#for GLM
results_acc_GLM_seq = combineResultsInSequence(results_acc_GLM)
results_acc_GLM_seq

#Build a ROC curve object for elastic net 
roc_results_accommodation_seq = roc(predictor = results_accommodation_seq$pred, response = results_accommodation_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_accommodation_seq = roc_results_accommodation_seq$auc
#internal validity of our elastic net model
roc_results_accommodation_seq 
#AUC 0.788 (95% CI 0.743-0.832) - looks good but need to calculate p-value by permutation testing

#Build a ROC curve object for GLM
roc_results_acc_GLM_seq = roc(predictor = results_acc_GLM_seq$pred, response = results_acc_GLM_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_acc_GLM_seq = roc_results_acc_GLM_seq$auc
#internal validity of our elastic net model
roc_results_acc_GLM_seq 
#AUC 0.628 (95% CI 0.0.522-0.735) - looks much poorer than elastic net but need to calculate p-value by permutation testing

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}
#permutation for elastic net
permutationPValue(results_seq = results_accommodation_seq, auc_seq = auc_results_accommodation_seq, predictYes = T, seq = T, dataSet = Training_ABI_accommodation, outcomeVariable = "outcome_accommodation")
#significant <0.0001 expected given confidence intervals- woohoo!

#permutation for GLM
permutationPValue(results_seq = results_acc_GLM_seq, auc_seq = auc_results_acc_GLM_seq, predictYes = T, seq = T, dataSet = Training_ABI_accommodation, outcomeVariable = "outcome_accommodation")
#significant <0.0023 for GLM

#https://github.com/nogueirs/JMLR2018
#Method from Nogueirs PhD on predictor stability for feature selection algorithms
#
# run from here until end of df_coefs_Acc = coefEvaluation(mods = mods_accommodation) as a complete section of code
getStability <- function(X,alpha=0.05) {
  ## the input X is a binary matrix of size M*d where:
  ## M is the number of bootstrap replicates
  ## d is the total number of features
  ## alpha is the level of significance (e.g. if alpha=0.05, we will get 95% confidence intervals)
  ## it's an optional argument and is set to 5% by default
  ### first we compute the stability
  
  M<-nrow(X)
  d<-ncol(X)
  hatPF<-colMeans(X)
  kbar<-sum(hatPF)
  v_rand=(kbar/d)*(1-kbar/d)
  stability<-1-(M/(M-1))*mean(hatPF*(1-hatPF))/v_rand ## this is the stability estimate
  
  ## then we compute the variance of the estimate
  ki<-rowSums(X)
  phi_i<-rep(0,M)
  for(i in 1:M){ 
    phi_i[i]<-(1/v_rand)*((1/d)*sum(X[i,]*hatPF)-(ki[i]*kbar)/d^2-(stability/2)*((2*kbar*ki[i])/d^2-ki[i]/d-kbar/d+1))
  }
  phi_bar=mean(phi_i)
  var_stab=(4/M^2)*sum((phi_i-phi_bar)^2) ## this is the variance of the stability estimate
  
  ## then we calculate lower and upper limits of the confidence intervals
  z<-qnorm(1-alpha/2) # this is the standard normal cumulative inverse at a level 1-alpha/2
  upper<-stability+z*sqrt(var_stab) ## the upper bound of the (1-alpha) confidence interval
  lower<-stability-z*sqrt(var_stab) ## the lower bound of the (1-alpha) confidence interval
  
  return(list("stability"=stability,"variance"=var_stab,"lower"=lower,"upper"=upper))
  
}

#Look at coefficients across 25 best models from nested cv
coefEvaluation = function(mods, isGLM = F)
 {
  coefs = NULL
  
  for (j in seq(1:length(mods)))
  {
    for (i in seq(1:length(mods[[j]])))
    {
      if(isGLM)
      {
        coefs = rbind(coefs, coef(mods[[j]][[i]]$finalModel))
      }else
      {
        coefs = c(coefs, coef(mods[[j]][[i]]$finalModel, mods[[j]][[i]]$bestTune$lambda))
      }
    }
  }
  
  lengthC = NULL
  if (isGLM)
  {
    lengthC = length(coefs[1,])
  }
  else
  {
    lengthC = length(coefs[[1]])
  }
  
  #just get numbers
  coefs_extract = NULL
  
  if(isGLM)
  {
    coefs_extract = coefs
  }
  else
  {
    for(i in seq(1:length(coefs)))
    {
      coefs_extract = rbind(coefs_extract, coefs[[i]][1:lengthC])
    }
  }
  
  #get matrix of coefficients presence (1) or absence (0)
  #Presence or absence of predictors across all 14 LOSOCV models
  coefs_presence = NULL
  coefs_presence = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presenceint = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presence[coefs_presence != 0] <- 1
  coefs_presenceint[coefs_presenceint != 0] <- 1
  
  #stability of feature selection http://jmlr.org/papers/volume18/17-514/17-514.pdf
  #stability across final 25 best models
  print(getStability(coefs_presence))
  
  #get rank of coef by importance as in sports ranking
  coefs_rank = NULL
  
  for(i in seq(1:(length(mods)*length(mods[[1]]))))
  {
    #rank absolute value excluding the intercept for each model
    coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[i,1:lengthC]), ties.method = "min"))
  }
  
  # rank the mean ranks of each column across all models
  coefs_rank_mean = colMeans(coefs_rank)
  
  #Invert order of rank to identify top models
  coefs_order = rank(-coefs_rank_mean)
  
  #Get the column names (not the intercept)
  if(isGLM)
  {
    coef_names = colnames(coefs[,1:lengthC])
  }else
  {
    coef_names = dimnames(coefs[[1]])[[1]][1:lengthC]
  }
  coefs_means = colMeans(coefs_extract)[1:lengthC]
  
  df = data.frame(coef_names, coefs_order, coefs_means, colMeans(coefs_presenceint))
}

df_coefs_acc = coefEvaluation(mods = mods_accommodation)

# Run coefficient code to here for complete code section
  
#Gives stability 0.144 which is low (basically interpreted the same as a correlation coefficient)
#stability lower than 0·4 shows poor agreement between the models, 0·4 to 0·75 shows intermediate to good agreement, and higher than 0·75 shows excellent agreement.
View(df_coefs_acc)
#Only 2 coefficients (adjustment and neuroadministered) selected across all 25 nested best models, expected given stability above

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_accommodation_seq$pred, y=results_accommodation_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.986 - good fit 

#calibration for GLM 
val.prob.ci.2(p=results_acc_GLM_seq$pred, y=results_acc_GLM_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.009 - very poor fit 


#######################################################################################################################
#
#for rf

nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess) #what preprocessing on the fly? #specify 
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    #createFolds splits the data into k groups (defaults to 10 groups, & as list) 
    # same process as before
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
   
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="rf", metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="rf", metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_acc_rf = nestedRepeatedCV(dataset = Training_ABI_accommodation,outcomeVariable = "outcome_accommodation", grid = F, control = control, preProcess = preProcess, tuneLength = 10)

results_acc_rf = results_mods_acc_rf$results
mods_acc_rf = results_mods_acc_rf$mods


#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_acc_rf_seq = combineResultsInSequence(results_acc_rf)

#Build a ROC curve object
roc_results_acc_rf_seq = roc(predictor = results_acc_rf_seq$pred, response = results_acc_rf_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_acc_rf_seq = roc_results_acc_rf_seq$auc
#What is the internal validity of our elastic net model?
roc_results_acc_rf_seq # rf AUC 0.81 95% CI: 0.768-0.852 - need permutation test for significance below


#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_acc_rf_seq, auc_seq = auc_results_acc_rf_seq, predictYes = T, seq = T, dataSet = Training_ABI_accommodation, outcomeVariable = "outcome_accommodation")
#shows significant <0.0001 expected given confidence intervals- good result


#https://github.com/nogueirs/JMLR2018
#Method from Nogueirs PhD on predictor stability for feature selection algorithms
getStability <- function(X,alpha=0.05) {
  ## the input X is a binary matrix of size M*d where:
  ## M is the number of bootstrap replicates
  ## d is the total number of features
  ## alpha is the level of significance (e.g. if alpha=0.05, we will get 95% confidence intervals)
  ## it's an optional argument and is set to 5% by default
  ### first we compute the stability
  
  M<-nrow(X)
  d<-ncol(X)
  hatPF<-colMeans(X)
  kbar<-sum(hatPF)
  v_rand=(kbar/d)*(1-kbar/d)
  stability<-1-(M/(M-1))*mean(hatPF*(1-hatPF))/v_rand ## this is the stability estimate
  
  ## then we compute the variance of the estimate
  ki<-rowSums(X)
  phi_i<-rep(0,M)
  for(i in 1:M){ 
    phi_i[i]<-(1/v_rand)*((1/d)*sum(X[i,]*hatPF)-(ki[i]*kbar)/d^2-(stability/2)*((2*kbar*ki[i])/d^2-ki[i]/d-kbar/d+1))
  }
  phi_bar=mean(phi_i)
  var_stab=(4/M^2)*sum((phi_i-phi_bar)^2) ## this is the variance of the stability estimate
  
  ## then we calculate lower and upper limits of the confidence intervals
  z<-qnorm(1-alpha/2) # this is the standard normal cumulative inverse at a level 1-alpha/2
  upper<-stability+z*sqrt(var_stab) ## the upper bound of the (1-alpha) confidence interval
  lower<-stability-z*sqrt(var_stab) ## the lower bound of the (1-alpha) confidence interval
  
  return(list("stability"=stability,"variance"=var_stab,"lower"=lower,"upper"=upper))
  
}

#Look at coefficients across best models from nested cv
coefEvaluation = function(mods, isGLM = F, isRF = F)
{
  coefs = NULL
  
  for (j in seq(1:length(mods)))
  {
    for (i in seq(1:length(mods[[j]])))
    {
      if(isGLM)
      {
        coefs = rbind(coefs, coef(mods[[j]][[i]]$finalModel))
        if (anyNA(coefs))
        {
          stop("No unique GLM fit")
        }
      }else if(isRF)
      {
        coefs = cbind(coefs, mods[[j]][[i]]$finalModel$importance)
      }else
      {
        coefs = c(coefs, coef(mods[[j]][[i]]$finalModel, mods[[j]][[i]]$bestTune$lambda))
      }
    }
  }
  
  lengthC = NULL
  if (isGLM)
  {
    lengthC = length(coefs[1,])
  }else if (isRF)
  {
    lengthC = length(coefs[,1])
  }else
  {
    lengthC = length(coefs[[1]])
  }
  
  #just get numbers
  coefs_extract = NULL
  
  if(isGLM || isRF)
  {
    coefs_extract = coefs
  }else
  {
    for(i in seq(1:length(coefs)))
    {
      coefs_extract = rbind(coefs_extract, coefs[[i]][1:lengthC])
    }
  }
  
  #get matrix of coefficients presence (1) or absence (0)
  #Presence or absence of predictors across all 14 LOSOCV models
  coefs_presence = NULL
  if(isRF)
  {
    coefs_presence = coefs_extract
    coefs_presenceint = coefs_extract
  }else
  {
    coefs_presence = coefs_extract[1:length(coefs),1:lengthC]
    coefs_presenceint = coefs_extract[1:length(coefs),1:lengthC]
  }
  coefs_presence[coefs_presence != 0] <- 1
  coefs_presenceint[coefs_presenceint != 0] <- 1
  
  #stability of feature selection http://jmlr.org/papers/volume18/17-514/17-514.pdf
  #stability across final 25 best models
  print(getStability(coefs_presence))
  
  #get rank of coef by importance as in sports ranking
  coefs_rank = NULL
  
  if(isRF)
  {
    for(i in seq(1:length(coefs_extract[1,])))
    {
      #rank absolute value excluding the intercept for each model
      coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[,i]), ties.method = "min"))
    }
  }else
  {
    for(i in seq(1:(length(mods)*length(mods[[1]]))))
    {
      #rank absolute value excluding the intercept for each model
      coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[i,1:lengthC]), ties.method = "min"))
    }
  }
  
  
  # rank the mean ranks of each column across all models
  coefs_rank_mean = colMeans(coefs_rank)
  
  #Invert order of rank to identify top models
  coefs_order = rank(-coefs_rank_mean)
  
  #Get the column names (not the intercept)
  if(isGLM)
  {
    coef_names = colnames(coefs[,1:lengthC])
  }else if (isRF)
  {
    coef_names = row.names(coefs)
  }else
  {
    coef_names = dimnames(coefs[[1]])[[1]][1:lengthC]
  }
  
  if(isRF)
  {
    coefs_means = rowMeans(coefs_extract)
  }else
  {
    coefs_means = colMeans(coefs_extract)[1:lengthC]
  }
  
  if(isRF)
  {
    feature_names = coef_names
    features_order = coefs_order
    mean_RF_importance = coefs_means
    df = data.frame(feature_names, features_order, mean_RF_importance, rowMeans(coefs_presenceint))
  }else
  {
    df = data.frame(coef_names, coefs_order, coefs_means, colMeans(coefs_presenceint))
  }
}

#compare top predictors for elastic net and rf- note SVM does not feature select in the same way 
df_coefs_acc = coefEvaluation(mods = mods_accommodation) #for elastic net
df_coefs_acc_rf = coefEvaluation(mods = mods_acc_rf, isRF = T) #for rf- note this is just ranking variable importance as does not use coefficients 

#stability not relevant for rf
#stability lower than 0·4 shows poor agreement between the models, 0·4 to 0·75 shows intermediate to good agreement, and higher than 0·75 shows excellent agreement.

View(df_coefs_acc)
View(df_coefs_acc_rf)


#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_acc_rf_seq$pred, y=results_acc_rf_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.90 -  - for rf may be mildly overfitting but much better than LR

########################################################################################################################
#for svm
#
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess) #what preprocessing on the fly? #specify 
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #as described before
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="svmLinear2", metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="svmLinear2", metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_acc_svm = nestedRepeatedCV(dataset = Training_ABI_accommodation,outcomeVariable = "outcome_accommodation", grid = F, control = control, preProcess = preProcess, tuneLength = 10)

results_acc_svm = results_mods_acc_svm$results

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_acc_svm_seq = combineResultsInSequence(results_acc_svm)



#build a ROC curve object
roc_results_acc_svm_seq = roc(predictor = results_acc_svm_seq$pred, response = results_acc_svm_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_acc_svm_seq = roc_results_acc_svm_seq$auc
#What is the internal validity of our elastic net model?
roc_results_acc_svm_seq # svm AUC 0.765 95% CI: 0.71-0.82- need permutation test for significance below

#compare ROCS between our ML final models #need to compute FDR corrections (either available https://www.sdmproject.com/utilities/?show=FDR) or code below
roc.test(roc_results_acc_rf_seq,roc_results_accommodation_seq)
roc.test(roc_results_acc_rf_seq,roc_results_acc_svm_seq)
roc.test(roc_results_accommodation_seq,roc_results_acc_svm_seq)
roc.test(roc_results_acc_GLM_seq,roc_results_acc_rf_seq)
roc.test(roc_results_acc_GLM_seq,roc_results_acc_svm_seq)
roc.test(roc_results_acc_GLM_seq,roc_results_accommodation_seq)

#FDR p value corrections
p.adjust(p=c(0.08,0.04,0.5,0.002,0.03,0.01),method ="fdr")
# adjusted p values in order of roc tests above= 0.096 0.060 0.500 0.012 0.060 0.030

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_acc_svm_seq, auc_seq = auc_results_acc_svm_seq, predictYes = T, seq = T, dataSet = Training_ABI_accommodation, outcomeVariable = "outcome_accommodation")
#shows significant <0.0001 expected given confidence intervals- good result again

#correct all permutation results with FDR after testing each outcome with each algorithm, available here https://www.sdmproject.com/utilities/?show=FDR

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_acc_svm_seq$pred, y=results_acc_svm_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 1.13 - potentially underfitting for svm 

#######################################################################################################################
################################################################################################
########################################################
############################

#Preprocessing for particpation...
#
#Training Data Preprocessing #taking out outcomes to not do preprocessing on 
Training_ABI_participation = ABI_outcomes[ ,!(colnames(ABI_outcomes) %in% c("outcome_length_of_stay_weeks","outcome_accommodation","outcome_participation","outcome_supervision","outcome_occupation","outcome_qol"))]

#Dummy code (not outcomes - required to remain factor) 
dummies = dummyVars(~ ., data = Training_ABI_participation, fullRank = T)#rules for dummy coding
Training_ABI_participation = data.frame(predict(dummies, newdata = Training_ABI_participation))#second data frame using the rules just created

#remove zero and near zero variance columns
nzv_cols = nearZeroVar(Training_ABI_participation)
if(length(nzv_cols) > 0) Training_ABI_participation = Training_ABI_participation[, -nzv_cols]

#Look at correlation - impute just for correlation as not missing at random - we will impute properly during cross-validation
#
preProcessImp = preProcess(Training_ABI_participation, method = c("center","scale","knnImpute")) #knn requires standardisation, default k=5
Training_Part_Imp = data.frame(predict(preProcessImp, Training_ABI_participation))



#Make correlation matrix (Pearson is default)
cor_all_Part = cor(Training_Part_Imp)
pdf("corPlot_Part.pdf", width = 25, height = 25)
corrplot(cor_all_Part, method = "number")
dev.off()

#Remove any columns with >0.7 correlation from training
hc = findCorrelation(cor_all_Part, cutoff=0.7) # put any value as a "cutoff" 
hc = sort(hc)
Training_ABI_participation = Training_ABI_participation[,-c(hc)] #Remove highly correlated columns (hc)

#Add factor outcome back in 
Training_ABI_participation$outcome_participation = ABI_outcomes$outcome_participation

#remove na from outcome - not allowed to impute outcome - leak information from training to test
Training_ABI_participation = Training_ABI_participation[which(!is.na(Training_ABI_participation[, "outcome_participation"])),]

#remove columns with more than 20% missing data default
Training_ABI_participation = Training_ABI_participation[, colMeans(is.na(Training_ABI_participation)) <= 0.2] #my study change to .5 for secondary analyses


##################################################################################################################################################################
#Model Specification (some done before) & Estimation- participation
#

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 10 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification #mine have repeats to 10 for larger sets >100p's
control = trainControl(method="repeatedcv", number=5, repeats=10, classProbs=TRUE, savePredictions=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)#caret does that for you to not leak data across the sets 
preProcess = c("center", "scale","knnImpute") #default k=5

set.seed(987)#make replicable (splits are random)#therefore in publication people will have same results 
ABI_part_mod = train(outcome_participation ~ ., #Outcome against all predictors
                     data = Training_ABI_participation, #data
                     method ="glmnet", #elastic net #or change to rf etc. caret website has a list of available models and what they're callled
                     metric="ROC", #performance metric - ROC for classification problems
                     tuneLength = 10, #for elastic net, how big a grid of alpha and lambda - 10*10 - only works correctly for selectionFunction = "best" #or for rf how many trees growing
                     preProc = preProcess, #Our preprocessing for each train fold
                     trControl = control, #Our tuning method rules
                     na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Resampling results across tuning parameters
#training performance shouldn't be reported as overoptimistic 
ABI_part_mod

#plot performance against alpha for each amount of lambda
plot(ABI_part_mod)
#plot regularisation paths for final model alpha as lambda increases
plot(ABI_part_mod$finalModel, label = T,xvar = "lambda")
#plot best lambda
abline(v=log(ABI_part_mod$bestTune$lambda))
#Zoom in
plot(ABI_part_mod$finalModel,label = T, xvar = "lambda", xlim = c(-2.5,-0.5), ylim = c(-0.5,0.5))
abline(v=log(ABI_part_mod$bestTune$lambda))

#Look at final coefficients #svm or rf do not get coefficients
coef(ABI_part_mod$finalModel, ABI_part_mod$bestTune$lambda)
#Look at odds ratios - no SE provided as estimates biased due to regularisation
exp(coef(ABI_part_mod$finalModel, ABI_part_mod$bestTune$lambda))
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_part_mod)#or rank by other things depending on algorithm 
#Plot them
varImp(ABI_part_mod)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()


#Try unregularised GLM to compare
set.seed(987)#make replicable (splits are random)
ABI_part_mod_GLM = train(outcome_participation ~ ., #Outcome against all predictors
                         data = Training_ABI_participation, #data
                         method ="glm", #elastic net
                         metric="ROC", #performance metric - ROC for classification problems
                         preProc = preProcess, #Our preprocessing for each train fold
                         trControl = control, #Our tuning method rules
                         na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias
#ROC
ABI_part_mod_GLM
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_part_mod_GLM)
#get stats 
xp <- evalm(ABI_part_mod_GLM)
## get roc curve plotted in ggplot2
roc_part_glm <- xp$roc
## get AUC and other metrics
xp$stdres
#get calibration curve
xp$cc
# 

#Try linear kernel SVM to compare
#SVM do not feature select but work ok with high dimensions
set.seed(987)
ABI_part_mod_lSVM = train(outcome_participation ~ ., #Outcome against all predictors
                          data = Training_ABI_participation, #data
                          method ="svmLinear2", #SVM linear kernel
                          metric="ROC", #performance metric - ROC for classification problems
                          tuneLength = 10, #how many Costs 
                          preProc = preProcess, #Our preprocessing for each train fold
                          trControl = control, #Our tuning method rules
                          na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Apparent performance?
ABI_part_mod_lSVM
plot(ABI_part_mod_lSVM)

#SVM variable importance- as described before, SVM feature selection not as helpful as RF and RLR so for interest only 
ABI_part_mod_lSVMImp <- varImp(ABI_part_mod_lSVM, scale = FALSE)
ABI_part_mod_lSVMImp


#Try random forest to compare
#Feature selects
set.seed(987)
ABI_part_mod_rf = train(outcome_participation ~ ., #Outcome against all predictors
                        data = Training_ABI_participation, #data
                        method ="rf", #random forest
                        metric="ROC", #performance metric - ROC for classification problems
                        tuneLength = 10, #how many mtrys
                        ntree = 500, #how nany trees to grow
                        preProc = preProcess, #Our preprocessing for each train fold
                        trControl = control, #Our tuning method rules
                        na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Apparent performance? ROC 0.7664 -- RLR slightly superior
ABI_part_mod_rf
plot(ABI_part_mod_rf)
#Look at variable importance
#Intuitively, the random shuffling means that, on average, the shuffled variable has no predictive power. 
#This importance is a measure of by how much removing a variable decreases accuracy, 
#and vice versa - by how much including a variable increases accuracy.
varImp(ABI_part_mod_rf)
#Plot them
varImp(ABI_part_mod_rf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()


######################################################################################################

#
#
# 
##Internal Validation for participation by elastic net, rf and svm
#5x Repeated 5-fold nested 20x repeated 5-fold cross-validation!
#
#repeat outer 5 fold cv loop 5 times
#Takes 10 to 15mins
#
#
#
#
#repeating the same thing we did above (20x repeated 5-fold cross-validation for a grid of 10*10 alpha/lambdas) 5x5 or 25 times which takes 25 times as long!

#Custom method for glmnet only
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess,
                            method = "glmnet") #what preprocessing on the fly?
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    #createFolds splits the data into k groups (defaults to 10 groups, & as list) 
    #when returnTrain = TRUE, the values returned are the sample positions corresponding to the data used during training, 
    #returns a list or matrix of row position integers corresponding to the training data
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_part = nestedRepeatedCV(dataset = Training_ABI_participation,outcomeVariable = "outcome_participation", grid = F, control = control, preProcess = preProcess, tuneLength = 10, method = "glmnet")

results_part = results_mods_part$results
mods_part = results_mods_part$mods

results_part
mods_part

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_part_seq = combineResultsInSequence(results_part)

results_part_seq

#Build a ROC curve object
roc_results_part_seq = roc(predictor = results_part_seq$pred, response = results_part_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_part_seq = roc_results_part_seq$auc
#internal validity of our elastic net model
roc_results_part_seq 
#AUC 0.728 (95% CI 0.675-0.781) - looks reasonable but need to calculate p-value by permutation testing

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_part_seq, auc_seq = auc_results_part_seq, predictYes = T, seq = T, dataSet = Training_ABI_participation, outcomeVariable = "outcome_participation")
#significant <0.0001 expected given confidence intervals!

#https://github.com/nogueirs/JMLR2018
#Method from Nogueirs PhD on predictor stability for feature selection algorithms
#
# run from here until end of df_coefs_part = coefEvaluation(mods = mods_participation) as a complete section of code
getStability <- function(X,alpha=0.05) {
  ## the input X is a binary matrix of size M*d where:
  ## M is the number of bootstrap replicates
  ## d is the total number of features
  ## alpha is the level of significance (e.g. if alpha=0.05, we will get 95% confidence intervals)
  ## it's an optional argument and is set to 5% by default
  ### first we compute the stability
  
  M<-nrow(X)
  d<-ncol(X)
  hatPF<-colMeans(X)
  kbar<-sum(hatPF)
  v_rand=(kbar/d)*(1-kbar/d)
  stability<-1-(M/(M-1))*mean(hatPF*(1-hatPF))/v_rand ## this is the stability estimate
  
  ## then we compute the variance of the estimate
  ki<-rowSums(X)
  phi_i<-rep(0,M)
  for(i in 1:M){ 
    phi_i[i]<-(1/v_rand)*((1/d)*sum(X[i,]*hatPF)-(ki[i]*kbar)/d^2-(stability/2)*((2*kbar*ki[i])/d^2-ki[i]/d-kbar/d+1))
  }
  phi_bar=mean(phi_i)
  var_stab=(4/M^2)*sum((phi_i-phi_bar)^2) ## this is the variance of the stability estimate
  
  ## then we calculate lower and upper limits of the confidence intervals
  z<-qnorm(1-alpha/2) # this is the standard normal cumulative inverse at a level 1-alpha/2
  upper<-stability+z*sqrt(var_stab) ## the upper bound of the (1-alpha) confidence interval
  lower<-stability-z*sqrt(var_stab) ## the lower bound of the (1-alpha) confidence interval
  
  return(list("stability"=stability,"variance"=var_stab,"lower"=lower,"upper"=upper))
  
}

#Look at coefficients across 25 best models from nested cv
coefEvaluation = function(mods, isGLM = F)
{
  coefs = NULL
  
  for (j in seq(1:length(mods)))
  {
    for (i in seq(1:length(mods[[j]])))
    {
      if(isGLM)
      {
        coefs = rbind(coefs, coef(mods[[j]][[i]]$finalModel))
      }else
      {
        coefs = c(coefs, coef(mods[[j]][[i]]$finalModel, mods[[j]][[i]]$bestTune$lambda))
      }
    }
  }
  
  lengthC = NULL
  if (isGLM)
  {
    lengthC = length(coefs[1,])
  }
  else
  {
    lengthC = length(coefs[[1]])
  }
  
  #just get numbers
  coefs_extract = NULL
  
  if(isGLM)
  {
    coefs_extract = coefs
  }
  else
  {
    for(i in seq(1:length(coefs)))
    {
      coefs_extract = rbind(coefs_extract, coefs[[i]][1:lengthC])
    }
  }
  
  #get matrix of coefficients presence (1) or absence (0)
  #Presence or absence of predictors across all 14 LOSOCV models
  coefs_presence = NULL
  coefs_presence = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presenceint = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presence[coefs_presence != 0] <- 1
  coefs_presenceint[coefs_presenceint != 0] <- 1
  
  #stability of feature selection http://jmlr.org/papers/volume18/17-514/17-514.pdf
  #stability across final 25 best models
  print(getStability(coefs_presence))
  
  #get rank of coef by importance as in sports ranking
  coefs_rank = NULL
  
  for(i in seq(1:(length(mods)*length(mods[[1]]))))
  {
    #rank absolute value excluding the intercept for each model
    coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[i,1:lengthC]), ties.method = "min"))
  }
  
  # rank the mean ranks of each column across all models
  coefs_rank_mean = colMeans(coefs_rank)
  
  #Invert order of rank to identify top models
  coefs_order = rank(-coefs_rank_mean)
  
  #Get the column names (not the intercept)
  if(isGLM)
  {
    coef_names = colnames(coefs[,1:lengthC])
  }else
  {
    coef_names = dimnames(coefs[[1]])[[1]][1:lengthC]
  }
  coefs_means = colMeans(coefs_extract)[1:lengthC]
  
  df = data.frame(coef_names, coefs_order, coefs_means, colMeans(coefs_presenceint))
}

df_coefs_part = coefEvaluation(mods = mods_part)

# Run coefficient code to here for complete code section

#Gives stability 0.633 which is quite good (basically interpreted the same as a correlation coefficient)
#stability lower than 0·4 shows poor agreement between the models, 0·4 to 0·75 shows intermediate to good agreement, and higher than 0·75 shows excellent agreement.
View(df_coefs_part)
#Only 1 coefficient (neuroadministered) selected across all 25 nested best models

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_part_seq$pred, y=results_part_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 1.369 - poor calibration!Underfitting!

#########################################
#
#
# For GLM 
#and for unregularised GLM to compare (only 2 inner and outer repeats due to no need for nested cross validation- will tune)
results_mods_part_GLM = nestedRepeatedCV(dataset = Training_ABI_participation,outcomeVariable = "outcome_participation", grid = F, control = control, preProcess = preProcess, tuneLength = 10,method = "glm")

#results for GLM
results_part_GLM = results_mods_part_GLM$results
mods_part_GLM = results_mods_part_GLM$mods

results_part_GLM
mods_part_GLM

#for GLM combine results in sequence
results_part_GLM_seq = combineResultsInSequence(results_part_GLM)
results_part_GLM_seq

#Build a ROC curve object for GLM
roc_results_part_GLM_seq = roc(predictor = results_part_GLM_seq$pred, response = results_part_GLM_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_part_GLM_seq = roc_results_part_GLM_seq$auc
#internal validity of our elastic net model
roc_results_part_GLM_seq 
#AUC Area under the curve: 0.667 95% CI: 0.613-0.722 (DeLong - looks much poorer than elastic net but need to calculate p-value by permutation testing


#permutation for GLM
permutationPValue(results_seq = results_part_GLM_seq, auc_seq = auc_results_part_GLM_seq, predictYes = T, seq = T, dataSet = Training_ABI_participation, outcomeVariable = "outcome_participation")
#significant <0.0027 for GLM

#calibration for GLM 
val.prob.ci.2(p=results_part_GLM_seq$pred, y=results_part_GLM_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.22 - very poor fit 
##############################################################################################################
#
#for rf

nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess) #what preprocessing on the fly? #specify 
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="rf", metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="rf", metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_part_rf = nestedRepeatedCV(dataset = Training_ABI_participation,outcomeVariable = "outcome_participation", grid = F, control = control, preProcess = preProcess, tuneLength = 10)

results_part_rf = results_mods_part_rf$results
mods_part_rf = results_mods_part_rf$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_part_rf_seq = combineResultsInSequence(results_part_rf)

#Build a ROC curve object
roc_results_part_rf_seq = roc(predictor = results_part_rf_seq$pred, response = results_part_rf_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_part_rf_seq = roc_results_part_rf_seq$auc
#What is the internal validity of our elastic net model?
roc_results_part_rf_seq # rf AUC 0.728 95% CI: 0.678-0.778 - need permutation test for significance below


#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_part_rf_seq, auc_seq = auc_results_part_rf_seq, predictYes = T, seq = T, dataSet = Training_ABI_participation, outcomeVariable = "outcome_participation")
#shows significant <0.0001 expected given confidence intervals- good result
#https://github.com/nogueirs/JMLR2018


#https://github.com/nogueirs/JMLR2018
#Method from Nogueirs PhD on predictor stability for feature selection algorithms
getStability <- function(X,alpha=0.05) {
  ## the input X is a binary matrix of size M*d where:
  ## M is the number of bootstrap replicates
  ## d is the total number of features
  ## alpha is the level of significance (e.g. if alpha=0.05, we will get 95% confidence intervals)
  ## it's an optional argument and is set to 5% by default
  ### first we compute the stability
  
  M<-nrow(X)
  d<-ncol(X)
  hatPF<-colMeans(X)
  kbar<-sum(hatPF)
  v_rand=(kbar/d)*(1-kbar/d)
  stability<-1-(M/(M-1))*mean(hatPF*(1-hatPF))/v_rand ## this is the stability estimate
  
  ## then we compute the variance of the estimate
  ki<-rowSums(X)
  phi_i<-rep(0,M)
  for(i in 1:M){ 
    phi_i[i]<-(1/v_rand)*((1/d)*sum(X[i,]*hatPF)-(ki[i]*kbar)/d^2-(stability/2)*((2*kbar*ki[i])/d^2-ki[i]/d-kbar/d+1))
  }
  phi_bar=mean(phi_i)
  var_stab=(4/M^2)*sum((phi_i-phi_bar)^2) ## this is the variance of the stability estimate
  
  ## then we calculate lower and upper limits of the confidence intervals
  z<-qnorm(1-alpha/2) # this is the standard normal cumulative inverse at a level 1-alpha/2
  upper<-stability+z*sqrt(var_stab) ## the upper bound of the (1-alpha) confidence interval
  lower<-stability-z*sqrt(var_stab) ## the lower bound of the (1-alpha) confidence interval
  
  return(list("stability"=stability,"variance"=var_stab,"lower"=lower,"upper"=upper))
  
}

#Look at coefficients across best models from nested cv
coefEvaluation = function(mods, isGLM = F, isRF = F)
{
  coefs = NULL
  
  for (j in seq(1:length(mods)))
  {
    for (i in seq(1:length(mods[[j]])))
    {
      if(isGLM)
      {
        coefs = rbind(coefs, coef(mods[[j]][[i]]$finalModel))
        if (anyNA(coefs))
        {
          stop("No unique GLM fit")
        }
      }else if(isRF)
      {
        coefs = cbind(coefs, mods[[j]][[i]]$finalModel$importance)
      }else
      {
        coefs = c(coefs, coef(mods[[j]][[i]]$finalModel, mods[[j]][[i]]$bestTune$lambda))
      }
    }
  }
  
  lengthC = NULL
  if (isGLM)
  {
    lengthC = length(coefs[1,])
  }else if (isRF)
  {
    lengthC = length(coefs[,1])
  }else
  {
    lengthC = length(coefs[[1]])
  }
  
  #just get numbers
  coefs_extract = NULL
  
  if(isGLM || isRF)
  {
    coefs_extract = coefs
  }else
  {
    for(i in seq(1:length(coefs)))
    {
      coefs_extract = rbind(coefs_extract, coefs[[i]][1:lengthC])
    }
  }
  
  #get matrix of coefficients presence (1) or absence (0)
  #Presence or absence of predictors across all 14 LOSOCV models
  coefs_presence = NULL
  if(isRF)
  {
    coefs_presence = coefs_extract
    coefs_presenceint = coefs_extract
  }else
  {
    coefs_presence = coefs_extract[1:length(coefs),1:lengthC]
    coefs_presenceint = coefs_extract[1:length(coefs),1:lengthC]
  }
  coefs_presence[coefs_presence != 0] <- 1
  coefs_presenceint[coefs_presenceint != 0] <- 1
  
  #stability of feature selection http://jmlr.org/papers/volume18/17-514/17-514.pdf
  #stability across final 25 best models
  print(getStability(coefs_presence))
  
  #get rank of coef by importance as in sports ranking
  coefs_rank = NULL
  
  if(isRF)
  {
    for(i in seq(1:length(coefs_extract[1,])))
    {
      #rank absolute value excluding the intercept for each model
      coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[,i]), ties.method = "min"))
    }
  }else
  {
    for(i in seq(1:(length(mods)*length(mods[[1]]))))
    {
      #rank absolute value excluding the intercept for each model
      coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[i,1:lengthC]), ties.method = "min"))
    }
  }
  
  
  # rank the mean ranks of each column across all models
  coefs_rank_mean = colMeans(coefs_rank)
  
  #Invert order of rank to identify top models
  coefs_order = rank(-coefs_rank_mean)
  
  #Get the column names (not the intercept)
  if(isGLM)
  {
    coef_names = colnames(coefs[,1:lengthC])
  }else if (isRF)
  {
    coef_names = row.names(coefs)
  }else
  {
    coef_names = dimnames(coefs[[1]])[[1]][1:lengthC]
  }
  
  if(isRF)
  {
    coefs_means = rowMeans(coefs_extract)
  }else
  {
    coefs_means = colMeans(coefs_extract)[1:lengthC]
  }
  
  if(isRF)
  {
    feature_names = coef_names
    features_order = coefs_order
    mean_RF_importance = coefs_means
    df = data.frame(feature_names, features_order, mean_RF_importance, rowMeans(coefs_presenceint))
  }else
  {
    df = data.frame(coef_names, coefs_order, coefs_means, colMeans(coefs_presenceint))
  }
}

#compare top predictors for elastic net (run previously) and rf- note SVM does not feature select in the same way 
df_coefs_part_rf = coefEvaluation(mods = mods_part_rf, isRF = T) #for rf- note this is just ranking variable importance as does not use coefficients 

#stability not relevant for rf


View(df_coefs_part_rf)


#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_part_rf_seq$pred, y=results_part_rf_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 1.037 -  - for rf good fit

################################################################################
#for svm
#
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess) #what preprocessing on the fly? #specify 
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="svmLinear2", metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="svmLinear2", metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_part_svm = nestedRepeatedCV(dataset = Training_ABI_participation,outcomeVariable = "outcome_participation", grid = F, control = control, preProcess = preProcess, tuneLength = 10)

results_part_svm = results_mods_part_svm$results
mods_part_svm = results_mods_part_svm$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_part_svm_seq = combineResultsInSequence(results_part_svm)

#Build a ROC curve object
roc_results_part_svm_seq = roc(predictor = results_part_svm_seq$pred, response = results_part_svm_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_part_svm_seq = roc_results_part_svm_seq$auc
#What is the internal validity of our elastic net model?
roc_results_part_svm_seq # svm AUC 0.721 95% CI: 0.670-0.772- need permutation test for significance below

#compare rocs
#compare ROCS between our ML final models #need to compute bonferroni corrections (available https://www.sdmproject.com/utilities/?show=FDR)
roc.test(roc_results_part_rf_seq,roc_results_part_seq)
roc.test(roc_results_part_rf_seq,roc_results_part_svm_seq)
roc.test(roc_results_part_seq,roc_results_part_svm_seq)
roc.test(roc_results_part_GLM_seq,roc_results_part_rf_seq)
roc.test(roc_results_part_GLM_seq,roc_results_part_svm_seq)
roc.test(roc_results_part_GLM_seq,roc_results_part_seq)

#adjust p values with FDR 
p.adjust(p=c(1,0.7,0.6,0.004,0.000006,0.00007),method="fdr")

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_part_svm_seq, auc_seq = auc_results_part_svm_seq, predictYes = T, seq = T, dataSet = Training_ABI_participation, outcomeVariable = "outcome_participation")
#shows significant <0.0001 expected given confidence intervals- good result again


#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_part_svm_seq$pred, y=results_part_svm_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.841 - potentially overfitting

#
#
#############################################################################################################################
#####################################################################################
#####################################################
##################################
##
#

#Preprocessing supervision
#
#Training Data Preprocessing #taking out outcomes to not do preprocessing on 
Training_ABI_supervision = ABI_outcomes[ ,!(colnames(ABI_outcomes) %in% c("outcome_length_of_stay_weeks","outcome_accommodation","outcome_supervision","outcome_supervision","outcome_occupation","outcome_qol"))]

#Dummy code (not outcomes - required to remain factor) 
dummies = dummyVars(~ ., data = Training_ABI_supervision, fullRank = T)#rules for dummy coding
Training_ABI_supervision = data.frame(predict(dummies, newdata = Training_ABI_supervision))#second data frame using the rules just created

#remove zero and near zero variance columns
nzv_cols = nearZeroVar(Training_ABI_supervision)
if(length(nzv_cols) > 0) Training_ABI_supervision = Training_ABI_supervision[, -nzv_cols]

#Look at correlation - impute just for correlation as not missing at random - we will impute properly during cross-validation
#
preProcessImp = preProcess(Training_ABI_supervision, method = c("center","scale","knnImpute")) #knn requires standardisation, default k=5
Training_sup_Imp = data.frame(predict(preProcessImp, Training_ABI_supervision))



#Make correlation matrix (Pearson is default)
cor_all_sup = cor(Training_sup_Imp)
pdf("corPlot_sup.pdf", width = 25, height = 25)
corrplot(cor_all_sup, method = "number")
dev.off()

#Remove any columns with >0.7 correlation from training
hc = findCorrelation(cor_all_sup, cutoff=0.7) # choose "cutoff" 
hc = sort(hc)
Training_ABI_supervision = Training_ABI_supervision[,-c(hc)] #Remove highly correlated columns (hc)

#Add factor outcome back in 
Training_ABI_supervision$outcome_supervision = ABI_outcomes$outcome_supervision

#remove na from outcome - not allowed to impute outcome - leak information from training to test
Training_ABI_supervision = Training_ABI_supervision[which(!is.na(Training_ABI_supervision[, "outcome_supervision"])),]

#remove columns with more than 20% missing data default
Training_ABI_supervision = Training_ABI_supervision[, colMeans(is.na(Training_ABI_supervision)) <= 0.2] #my study change to .5 for secondary analyses


##################################################################################################################################################################
#Model Specification (some done before) & Estimation
#

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 10 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification #mine have repeats to 10 for larger sets >100p's
control = trainControl(method="repeatedcv", number=5, repeats=10, classProbs=TRUE, savePredictions=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)#caret does that for you to not leak data across the sets 
preProcess = c("center", "scale","knnImpute") #default k=5

set.seed(987)#make replicable (splits are random)#therefore in publication people will have same results 
ABI_sup_mod = train(outcome_supervision ~ ., #Outcome against all predictors
                    data = Training_ABI_supervision, #data
                    method ="glmnet", #elastic net #or change to rf etc. caret website has a list of available models and what they're callled
                    metric="ROC", #performance metric - ROC for classification problems
                    tuneLength = 10, #for elastic net, how big a grid of alpha and lambda - 10*10 - only works correctly for selectionFunction = "best" #or for rf how many trees growing
                    preProc = preProcess, #Our preprocessing for each train fold
                    trControl = control, #Our tuning method rules
                    na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Resampling results across tuning parameters
# #training performance shouldn't be reported as overoptimistic
ABI_sup_mod

#plot performance against alpha for each amount of lambda
plot(ABI_sup_mod)
#plot regularisation paths for final model alpha as lambda increases
plot(ABI_sup_mod$finalModel, label = T,xvar = "lambda")
#plot best lambda
abline(v=log(ABI_sup_mod$bestTune$lambda))
#Zoom in
plot(ABI_sup_mod$finalModel,label = T, xvar = "lambda", xlim = c(-2.5,-0.5), ylim = c(-0.5,0.5))
abline(v=log(ABI_sup_mod$bestTune$lambda))

#Look at final coefficients #svm or rf do not get coefficients
coef(ABI_sup_mod$finalModel, ABI_sup_mod$bestTune$lambda)
#Look at odds ratios - no SE provided as estimates biased due to regularisation
exp(coef(ABI_sup_mod$finalModel, ABI_sup_mod$bestTune$lambda))
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_sup_mod)#or rank by other things depending on algorithm 
#Plot them
varImp(ABI_sup_mod)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()


#Try unregularised GLM to compare
set.seed(987)#make replicable (splits are random)
ABI_sup_mod_GLM = train(outcome_supervision ~ ., #Outcome against all predictors
                        data = Training_ABI_supervision, #data
                        method ="glm", #elastic net
                        metric="ROC", #performance metric - ROC for classification problems
                        preProc = preProcess, #Our preprocessing for each train fold
                        trControl = control, #Our tuning method rules
                        na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias
#ROC 
ABI_sup_mod_GLM
#variable importance 
varImp(ABI_sup_mod_GLM)
#get stats 
xs <- evalm(ABI_sup_mod_GLM)
## get roc curve plotted in ggplot2
roc_sup_glm <- xs$roc
## get AUC and other metrics
xs$stdres
#get calibration curve
xs$cc
#

#Try linear kernel SVM to compare
#SVM do not feature select but work ok with high dimensions
set.seed(987)
ABI_sup_mod_lSVM = train(outcome_supervision ~ ., #Outcome against all predictors
                         data = Training_ABI_supervision, #data
                         method ="svmLinear2", #SVM linear kernel
                         metric="ROC", #performance metric - ROC for classification problems
                         tuneLength = 10, #how many Costs 
                         preProc = preProcess, #Our preprocessing for each train fold
                         trControl = control, #Our tuning method rules
                         na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Apparent performance? 
ABI_sup_mod_lSVM
plot(ABI_sup_mod_lSVM)

#svm variable importance
ABI_sup_mod_lSVMImp <- varImp(ABI_sup_mod_lSVM, scale = FALSE)
ABI_sup_mod_lSVMImp


#Try random forest to compare
#Feature selects
set.seed(987)
ABI_sup_mod_rf = train(outcome_supervision ~ ., #Outcome against all predictors
                       data = Training_ABI_supervision, #data
                       method ="rf", #random forest
                       metric="ROC", #performance metric - ROC for classification problems
                       tuneLength = 10, #how many mtrys
                       ntree = 500, #how nany trees to grow
                       preProc = preProcess, #Our preprocessing for each train fold
                       trControl = control, #Our tuning method rules
                       na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Apparent performance? - best performing algorithm for supervision
ABI_sup_mod_rf
plot(ABI_sup_mod_rf)
#Look at variable importance
#Intuitively, the random shuffling means that, on average, the shuffled variable has no predictive power. 
#This importance is a measure of by how much removing a variable decreases accuracy, 
#and vice versa - by how much including a variable increases accuracy.
varImp(ABI_sup_mod_rf)
#Plot them
varImp(ABI_sup_mod_rf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()

#
#
#
######################################################################################################################
#
# Internal cross validation for supervision
#Custom method for glmnet only
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess,
                            method = "glmnet") #what preprocessing on the fly?
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_sup = nestedRepeatedCV(dataset = Training_ABI_supervision,outcomeVariable = "outcome_supervision", grid = F, control = control, preProcess = preProcess, tuneLength = 10, method = "glmnet")

results_sup = results_mods_sup$results
mods_sup = results_mods_sup$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_sup_seq = combineResultsInSequence(results_sup)

results_sup_seq

#Build a ROC curve object
roc_results_sup_seq = roc(predictor = results_sup_seq$pred, response = results_sup_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_sup_seq = roc_results_sup_seq$auc
#internal validity of our elastic net model
roc_results_sup_seq 
#AUC 0.767 (95% CI 0.722-0.812) - looks reasonable but need to calculate p-value by permutation testing

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_sup_seq, auc_seq = auc_results_sup_seq, predictYes = T, seq = T, dataSet = Training_ABI_supervision, outcomeVariable = "outcome_supervision")
#significant <0.0001 expected given confidence intervals!

#https://github.com/nogueirs/JMLR2018
#Method from Nogueirs PhD on predictor stability for feature selection algorithms
#
# run from here until end of df_coefs_sup = coefEvaluation(mods = mods_sup) as a complete section of code
getStability <- function(X,alpha=0.05) {
  ## the input X is a binary matrix of size M*d where:
  ## M is the number of bootstrap replicates
  ## d is the total number of features
  ## alpha is the level of significance (e.g. if alpha=0.05, we will get 95% confidence intervals)
  ## it's an optional argument and is set to 5% by default
  ### first we compute the stability
  
  M<-nrow(X)
  d<-ncol(X)
  hatPF<-colMeans(X)
  kbar<-sum(hatPF)
  v_rand=(kbar/d)*(1-kbar/d)
  stability<-1-(M/(M-1))*mean(hatPF*(1-hatPF))/v_rand ## this is the stability estimate
  
  ## then we compute the variance of the estimate
  ki<-rowSums(X)
  phi_i<-rep(0,M)
  for(i in 1:M){ 
    phi_i[i]<-(1/v_rand)*((1/d)*sum(X[i,]*hatPF)-(ki[i]*kbar)/d^2-(stability/2)*((2*kbar*ki[i])/d^2-ki[i]/d-kbar/d+1))
  }
  phi_bar=mean(phi_i)
  var_stab=(4/M^2)*sum((phi_i-phi_bar)^2) ## this is the variance of the stability estimate
  
  ## then we calculate lower and upper limits of the confidence intervals
  z<-qnorm(1-alpha/2) # this is the standard normal cumulative inverse at a level 1-alpha/2
  upper<-stability+z*sqrt(var_stab) ## the upper bound of the (1-alpha) confidence interval
  lower<-stability-z*sqrt(var_stab) ## the lower bound of the (1-alpha) confidence interval
  
  return(list("stability"=stability,"variance"=var_stab,"lower"=lower,"upper"=upper))
  
}

#Look at coefficients across 25 best models from nested cv
coefEvaluation = function(mods, isGLM = F)
{
  coefs = NULL
  
  for (j in seq(1:length(mods)))
  {
    for (i in seq(1:length(mods[[j]])))
    {
      if(isGLM)
      {
        coefs = rbind(coefs, coef(mods[[j]][[i]]$finalModel))
      }else
      {
        coefs = c(coefs, coef(mods[[j]][[i]]$finalModel, mods[[j]][[i]]$bestTune$lambda))
      }
    }
  }
  
  lengthC = NULL
  if (isGLM)
  {
    lengthC = length(coefs[1,])
  }
  else
  {
    lengthC = length(coefs[[1]])
  }
  
  #just get numbers
  coefs_extract = NULL
  
  if(isGLM)
  {
    coefs_extract = coefs
  }
  else
  {
    for(i in seq(1:length(coefs)))
    {
      coefs_extract = rbind(coefs_extract, coefs[[i]][1:lengthC])
    }
  }
  
  #get matrix of coefficients presence (1) or absence (0)
  #Presence or absence of predictors across all 14 LOSOCV models
  coefs_presence = NULL
  coefs_presence = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presenceint = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presence[coefs_presence != 0] <- 1
  coefs_presenceint[coefs_presenceint != 0] <- 1
  
  #stability of feature selection http://jmlr.org/papers/volume18/17-514/17-514.pdf
  #stability across final 25 best models
  print(getStability(coefs_presence))
  
  #get rank of coef by importance as in sports ranking
  coefs_rank = NULL
  
  for(i in seq(1:(length(mods)*length(mods[[1]]))))
  {
    #rank absolute value excluding the intercept for each model
    coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[i,1:lengthC]), ties.method = "min"))
  }
  
  # rank the mean ranks of each column across all models
  coefs_rank_mean = colMeans(coefs_rank)
  
  #Invert order of rank to identify top models
  coefs_order = rank(-coefs_rank_mean)
  
  #Get the column names (not the intercept)
  if(isGLM)
  {
    coef_names = colnames(coefs[,1:lengthC])
  }else
  {
    coef_names = dimnames(coefs[[1]])[[1]][1:lengthC]
  }
  coefs_means = colMeans(coefs_extract)[1:lengthC]
  
  df = data.frame(coef_names, coefs_order, coefs_means, colMeans(coefs_presenceint))
}

df_coefs_sup = coefEvaluation(mods = mods_sup)

# Run coefficient code to here for complete code section

#Gives stability 0.1878 which is low (basically interpreted the same as a correlation coefficient)
#stability lower than 0·4 shows poor agreement between the models, 0·4 to 0·75 shows intermediate to good agreement, and higher than 0·75 shows excellent agreement.
View(df_coefs_sup)
#2 selected stable coefficients (neuro administered, and adjustment) across all 25 nested best models

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_sup_seq$pred, y=results_sup_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 1.10


########################################################################
#
#
#For GLM 
#and for unregularised GLM to compare (only 2 inner and outer repeats due to no need for nested cross validation- will tune)
results_mods_sup_GLM = nestedRepeatedCV(dataset = Training_ABI_supervision,outcomeVariable = "outcome_supervision", grid = F, control = control, preProcess = preProcess, tuneLength = 10,method = "glm")

#results for GLM
results_sup_GLM = results_mods_sup_GLM$results
mods_sup_GLM = results_mods_sup_GLM$mods

results_sup_GLM
mods_sup_GLM

#for GLM combine results in sequence
results_sup_GLM_seq = combineResultsInSequence(results_sup_GLM)
results_sup_GLM_seq

#Build a ROC curve object for GLM
roc_results_sup_GLM_seq = roc(predictor = results_sup_GLM_seq$pred, response = results_sup_GLM_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_sup_GLM_seq = roc_results_sup_GLM_seq$auc
#internal validity of our elastic net model
roc_results_sup_GLM_seq 
#AUC 0.73 95% CI: 0.683-0.777 (DeLong)- looks much poorer than elastic net but need to calculate p-value by permutation testing


#permutation for GLM
permutationPValue(results_seq = results_sup_GLM_seq, auc_seq = auc_results_sup_GLM_seq, predictYes = T, seq = T, dataSet = Training_ABI_supervision, outcomeVariable = "outcome_supervision")
#significant <0.0023 for GLM

#calibration for GLM 
val.prob.ci.2(p=results_sup_GLM_seq$pred, y=results_sup_GLM_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.24 - very poor fit 


##############################################################################################################
#
#for rf

nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess) #what preprocessing on the fly? #specify 
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="rf", metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="rf", metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_sup_rf = nestedRepeatedCV(dataset = Training_ABI_supervision,outcomeVariable = "outcome_supervision", grid = F, control = control, preProcess = preProcess, tuneLength = 10)

results_sup_rf = results_mods_sup_rf$results
mods_sup_rf = results_mods_sup_rf$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_sup_rf_seq = combineResultsInSequence(results_sup_rf)

#Build a ROC curve object
roc_results_sup_rf_seq = roc(predictor = results_sup_rf_seq$pred, response = results_sup_rf_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_sup_rf_seq = roc_results_sup_rf_seq$auc
#What is the internal validity of our rf model?
roc_results_sup_rf_seq # rf AUC 0.81 95% CI: 0.771-.849- need permutation test for significance below

#variable importance 
#compare top predictors for elastic net (run previously) and rf- note SVM does not feature select in the same way 
df_coefs_sup_rf = coefEvaluation(mods = mods_sup_rf, isRF = T) #for rf- note this is just ranking variable importance as does not use coefficients 

#stability not relevant for rf

View(df_coefs_sup_rf)

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_sup_rf_seq, auc_seq = auc_results_sup_rf_seq, predictYes = T, seq = T, dataSet = Training_ABI_supervision, outcomeVariable = "outcome_supervision")
#shows significant <0.0001 expected given confidence intervals- good result


#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_sup_rf_seq$pred, y=results_sup_rf_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is .885 -  - for rf 

####################################################################################################################
#for svm
#
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess) #what preprocessing on the fly? #specify 
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="svmLinear2", metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="svmLinear2", metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_sup_svm = nestedRepeatedCV(dataset = Training_ABI_supervision,outcomeVariable = "outcome_supervision", grid = F, control = control, preProcess = preProcess, tuneLength = 10)

results_sup_svm = results_mods_sup_svm$results
mods_sup_svm = results_mods_sup_svm$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_sup_svm_seq = combineResultsInSequence(results_sup_svm)

#Build a ROC curve object
roc_results_sup_svm_seq = roc(predictor = results_sup_svm_seq$pred, response = results_sup_svm_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_sup_svm_seq = roc_results_sup_svm_seq$auc
#What is the internal validity of our elastic net model?
roc_results_sup_svm_seq # svm AUC 0.751 95% CI: 0.705-0.798- need permutation test for significance below

#compare ROCS between our ML final models #need to compute bonferroni corrections (available https://www.sdmproject.com/utilities/?show=FDR)
roc.test(roc_results_sup_rf_seq,roc_results_sup_seq)
roc.test(roc_results_sup_rf_seq,roc_results_sup_svm_seq)
roc.test(roc_results_sup_seq,roc_results_sup_svm_seq)
roc.test(roc_results_sup_GLM_seq,roc_results_sup_rf_seq)
roc.test(roc_results_sup_GLM_seq,roc_results_sup_svm_seq)
roc.test(roc_results_sup_GLM_seq,roc_results_sup_seq)


#FDR p value corrections
p.adjust(p=c(0.02,0.002,0.1,0.03,0.4,0.2),method ="fdr")

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_sup_svm_seq, auc_seq = auc_results_sup_svm_seq, predictYes = T, seq = T, dataSet = Training_ABI_supervision, outcomeVariable = "outcome_supervision")
#shows significant <0.0001 expected given confidence intervals- good result again
#https://github.com/nogueirs/JMLR2018

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_sup_svm_seq$pred, y=results_sup_svm_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 1.01 


######################################################################################################################
############################################################################
########################################################
#############################
#
#
#For occupation preprocessing
#
#
#
#
#Preprocessing outcome- occupation
#
#Training Data Preprocessing #taking out outcomes to not do preprocessing on 
Training_ABI_occupation = ABI_outcomes[ ,!(colnames(ABI_outcomes) %in% c("outcome_length_of_stay_weeks","outcome_accommodation","outcome_occupation","outcome_supervision","outcome_participation","outcome_qol"))]

#Dummy code (not outcomes - required to remain factor) 
dummies = dummyVars(~ ., data = Training_ABI_occupation, fullRank = T)#rules for dummy coding
Training_ABI_occupation = data.frame(predict(dummies, newdata = Training_ABI_occupation))#second data frame using the rules just created

#remove zero and near zero variance columns
nzv_cols = nearZeroVar(Training_ABI_occupation)
if(length(nzv_cols) > 0) Training_ABI_occupation = Training_ABI_occupation[, -nzv_cols]

#Look at correlation - impute just for correlation as not missing at random - we will impute properly during cross-validation
#
preProcessImp = preProcess(Training_ABI_occupation, method = c("center","scale","knnImpute")) #knn requires standardisation, default k=5
Training_occ_Imp = data.frame(predict(preProcessImp, Training_ABI_occupation))



#Make correlation matrix (Pearson is default)
cor_all_occ = cor(Training_occ_Imp)
pdf("corPlot_occ.pdf", width = 25, height = 25)
corrplot(cor_all_occ, method = "number")
dev.off()

#Remove any columns with >0.7 correlation from training
hc = findCorrelation(cor_all_occ, cutoff=0.7) # choose "cutoff" 
hc = sort(hc)
Training_ABI_occupation = Training_ABI_occupation[,-c(hc)] #Remove highly correlated columns (hc)

#Add factor outcome back in 
Training_ABI_occupation$outcome_occupation = ABI_outcomes$outcome_occupation

#remove na from outcome - not allowed to impute outcome - leak information from training to test
Training_ABI_occupation = Training_ABI_occupation[which(!is.na(Training_ABI_occupation[, "outcome_occupation"])),]

#remove columns with more than 20% missing data default
Training_ABI_occupation = Training_ABI_occupation[, colMeans(is.na(Training_ABI_occupation)) <= 0.2] #my study change to .5 for secondary analyses


##################################################################################################################################################################
#Model Specification (some done before) & Estimation
#

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 10 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification #mine have repeats to 10 for larger sets >100p's
control = trainControl(method="repeatedcv", number=5, repeats=10, classProbs=TRUE, savePredictions=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)#caret does that for you to not leak data across the sets 
preProcess = c("center", "scale","knnImpute") #default k=5

set.seed(987)#make replicable (splits are random)#therefore in publication people will have same results 
ABI_occ_mod = train(outcome_occupation ~ ., #Outcome against all predictors
                    data = Training_ABI_occupation, #data
                    method ="glmnet", #elastic net #or change to rf etc. caret website has a list of available models and what they're callled
                    metric="ROC", #performance metric - ROC for classification problems
                    tuneLength = 10, #for elastic net, how big a grid of alpha and lambda - 10*10 - only works correctly for selectionFunction = "best" #or for rf how many trees growing
                    preProc = preProcess, #Our preprocessing for each train fold
                    trControl = control, #Our tuning method rules
                    na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Resampling results across tuning parameters
#training performance shouldn't be reported as overoptimistic 
ABI_occ_mod

#plot performance against alpha for each amount of lambda
plot(ABI_occ_mod)
#plot regularisation paths for final model alpha as lambda increases
plot(ABI_occ_mod$finalModel, label = T,xvar = "lambda")
#plot best lambda
abline(v=log(ABI_occ_mod$bestTune$lambda))
#Zoom in
plot(ABI_occ_mod$finalModel,label = T, xvar = "lambda", xlim = c(-2.5,-0.5), ylim = c(-0.5,0.5))
abline(v=log(ABI_occ_mod$bestTune$lambda))

#Look at final coefficients #svm or rf do not get coefficients
coef(ABI_occ_mod$finalModel, ABI_occ_mod$bestTune$lambda)
#Look at odds ratios - no SE provided as estimates biased due to regularisation
exp(coef(ABI_occ_mod$finalModel, ABI_occ_mod$bestTune$lambda))
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_occ_mod)#or ranks by other things depending on algorithm 
#Plot them
varImp(ABI_occ_mod)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()

#biggest predictors for occupation male gender, neuroadministered, mpai abilities, dx other, age at admission...

#Try unregularised GLM to compare
set.seed(987)#make replicable (splits are random)
ABI_occ_mod_GLM = train(outcome_occupation ~ ., #Outcome against all predictors
                        data = Training_ABI_occupation, #data
                        method ="glm", #elastic net
                        metric="ROC", #performance metric - ROC for classification problems
                        preProc = preProcess, #Our preprocessing for each train fold
                        trControl = control, #Our tuning method rules
                        na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias
#ROC 
ABI_occ_mod_GLM
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_occ_mod_GLM)
#get stats 
xo <- evalm(ABI_occ_mod_GLM)
## get roc curve plotted in ggplot2
roc_acc_glm <- xo$roc
## get AUC and other metrics
xo$stdres
#get calibration curve
xo$cc
#


#Try linear kernel SVM to compare
#SVM do not feature select but work ok with high dimensions
set.seed(987)
ABI_occ_mod_lSVM = train(outcome_occupation ~ ., #Outcome against all predictors
                         data = Training_ABI_occupation, #data
                         method ="svmLinear2", #SVM linear kernel
                         metric="ROC", #performance metric - ROC for classification problems
                         tuneLength = 10, #how many Costs 
                         preProc = preProcess, #Our preprocessing for each train fold
                         trControl = control, #Our tuning method rules
                         na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Apparent performance? 
ABI_occ_mod_lSVM
plot(ABI_occ_mod_lSVM)

#svm variable importance for interest only 
ABI_occ_mod_lSVMImp <- varImp(ABI_occ_mod_lSVM, scale = FALSE)
ABI_occ_mod_lSVMImp


#Try random forest to compare
#Feature selects
set.seed(987)
ABI_occ_mod_rf = train(outcome_occupation ~ ., #Outcome against all predictors
                       data = Training_ABI_occupation, #data
                       method ="rf", #random forest
                       metric="ROC", #performance metric - ROC for classification problems
                       tuneLength = 10, #how many mtrys
                       ntree = 500, #how nany trees to grow
                       preProc = preProcess, #Our preprocessing for each train fold
                       trControl = control, #Our tuning method rules
                       na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias


#Apparent performance? ROC best for occupation
ABI_occ_mod_rf
plot(ABI_occ_mod_rf)
#Look at variable importance
#Intuitively, the random shuffling means that, on average, the shuffled variable has no predictive power. 
#This importance is a measure of by how much removing a variable decreases accuracy, 
#and vice versa - by how much including a variable increases accuracy.
varImp(ABI_occ_mod_rf)
#Plot them
varImp(ABI_occ_mod_rf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()



#
##################################################################################################################################################################
#Now next step is internal validation performance
#
#

#Custom method for glmnet only
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess,
                            method = "glmnet") #what preprocessing on the fly?
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_occ = nestedRepeatedCV(dataset = Training_ABI_occupation,outcomeVariable = "outcome_occupation", grid = F, control = control, preProcess = preProcess, tuneLength = 10, method = "glmnet")

results_occ = results_mods_occ$results
mods_occ = results_mods_occ$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_occ_seq = combineResultsInSequence(results_occ)

results_occ_seq

#Build a ROC curve object
roc_results_occ_seq = roc(predictor = results_occ_seq$pred, response = results_occ_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_occ_seq = roc_results_occ_seq$auc
#internal validity of our elastic net model
roc_results_occ_seq 
#AUC 0.648 (95% CI 0.61-0.685) - looks reasonable but need to calculate p-value by permutation testing

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_occ_seq, auc_seq = auc_results_occ_seq, predictYes = T, seq = T, dataSet = Training_ABI_occupation, outcomeVariable = "outcome_occupation")
#significant <0.0001 expected given confidence intervals!

#https://github.com/nogueirs/JMLR2018
#Method from Nogueirs PhD on predictor stability for feature selection algorithms
#
# run from here until end of df_coefs_occ = coefEvaluation(mods = mods_occ) as a complete section of code
getStability <- function(X,alpha=0.05) {
  ## the input X is a binary matrix of size M*d where:
  ## M is the number of bootstrap replicates
  ## d is the total number of features
  ## alpha is the level of significance (e.g. if alpha=0.05, we will get 95% confidence intervals)
  ## it's an optional argument and is set to 5% by default
  ### first we compute the stability
  
  M<-nrow(X)
  d<-ncol(X)
  hatPF<-colMeans(X)
  kbar<-sum(hatPF)
  v_rand=(kbar/d)*(1-kbar/d)
  stability<-1-(M/(M-1))*mean(hatPF*(1-hatPF))/v_rand ## this is the stability estimate
  
  ## then we compute the variance of the estimate
  ki<-rowSums(X)
  phi_i<-rep(0,M)
  for(i in 1:M){ 
    phi_i[i]<-(1/v_rand)*((1/d)*sum(X[i,]*hatPF)-(ki[i]*kbar)/d^2-(stability/2)*((2*kbar*ki[i])/d^2-ki[i]/d-kbar/d+1))
  }
  phi_bar=mean(phi_i)
  var_stab=(4/M^2)*sum((phi_i-phi_bar)^2) ## this is the variance of the stability estimate
  
  ## then we calculate lower and upper limits of the confidence intervals
  z<-qnorm(1-alpha/2) # this is the standard normal cumulative inverse at a level 1-alpha/2
  upper<-stability+z*sqrt(var_stab) ## the upper bound of the (1-alpha) confidence interval
  lower<-stability-z*sqrt(var_stab) ## the lower bound of the (1-alpha) confidence interval
  
  return(list("stability"=stability,"variance"=var_stab,"lower"=lower,"upper"=upper))
  
}

#Look at coefficients across 25 best models from nested cv
coefEvaluation = function(mods, isGLM = F)
{
  coefs = NULL
  
  for (j in seq(1:length(mods)))
  {
    for (i in seq(1:length(mods[[j]])))
    {
      if(isGLM)
      {
        coefs = rbind(coefs, coef(mods[[j]][[i]]$finalModel))
      }else
      {
        coefs = c(coefs, coef(mods[[j]][[i]]$finalModel, mods[[j]][[i]]$bestTune$lambda))
      }
    }
  }
  
  lengthC = NULL
  if (isGLM)
  {
    lengthC = length(coefs[1,])
  }
  else
  {
    lengthC = length(coefs[[1]])
  }
  
  #just get numbers
  coefs_extract = NULL
  
  if(isGLM)
  {
    coefs_extract = coefs
  }
  else
  {
    for(i in seq(1:length(coefs)))
    {
      coefs_extract = rbind(coefs_extract, coefs[[i]][1:lengthC])
    }
  }
  
  #get matrix of coefficients presence (1) or absence (0)
  #Presence or absence of predictors across all 14 LOSOCV models
  coefs_presence = NULL
  coefs_presence = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presenceint = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presence[coefs_presence != 0] <- 1
  coefs_presenceint[coefs_presenceint != 0] <- 1
  
  #stability of feature selection http://jmlr.org/papers/volume18/17-514/17-514.pdf
  #stability across final 25 best models
  print(getStability(coefs_presence))
  
  #get rank of coef by importance as in sports ranking
  coefs_rank = NULL
  
  for(i in seq(1:(length(mods)*length(mods[[1]]))))
  {
    #rank absolute value excluding the intercept for each model
    coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[i,1:lengthC]), ties.method = "min"))
  }
  
  # rank the mean ranks of each column across all models
  coefs_rank_mean = colMeans(coefs_rank)
  
  #Invert order of rank to identify top models
  coefs_order = rank(-coefs_rank_mean)
  
  #Get the column names (not the intercept)
  if(isGLM)
  {
    coef_names = colnames(coefs[,1:lengthC])
  }else
  {
    coef_names = dimnames(coefs[[1]])[[1]][1:lengthC]
  }
  coefs_means = colMeans(coefs_extract)[1:lengthC]
  
  df = data.frame(coef_names, coefs_order, coefs_means, colMeans(coefs_presenceint))
}

df_coefs_occ = coefEvaluation(mods = mods_occ)

# Run coefficient code to here for complete code section

#Gives stability 0.3455 which is low (basically interpreted the same as a correlation coefficient)
#stability lower than 0·4 shows poor agreement between the models, 0·4 to 0·75 shows intermediate to good agreement, and higher than 0·75 shows excellent agreement.
View(df_coefs_occ)
#3 stable coefficients (, male gender, neuroadministered, mpai abilties) selected across all 25 nested best models

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_occ_seq$pred, y=results_occ_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.623

###########################################################################################
#
#
#For GLM 
#and for unregularised GLM to compare (only 2 inner and outer repeats due to no need for nested cross validation- will tune)
results_mods_occ_GLM = nestedRepeatedCV(dataset = Training_ABI_occupation,outcomeVariable = "outcome_occupation", grid = F, control = control, preProcess = preProcess, tuneLength = 10,method = "glm")

#results for GLM
results_occ_GLM = results_mods_occ_GLM$results
mods_occ_GLM = results_mods_occ_GLM$mods

results_occ_GLM
mods_occ_GLM

#for GLM combine results in sequence
results_occ_GLM_seq = combineResultsInSequence(results_occ_GLM)
results_occ_GLM_seq

#Build a ROC curve object for GLM
roc_results_occ_GLM_seq = roc(predictor = results_occ_GLM_seq$pred, response = results_occ_GLM_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_occ_GLM_seq = roc_results_occ_GLM_seq$auc
#internal validity of our elastic net model
roc_results_occ_GLM_seq 
#AUC 0.651 (95% CI 0.591-0.712) - the same elastic net but need to calculate p-value by permutation testing


#permutation for GLM
permutationPValue(results_seq = results_occ_GLM_seq, auc_seq = auc_results_occ_GLM_seq, predictYes = T, seq = T, dataSet = Training_ABI_occupation, outcomeVariable = "outcome_occupation")
#significant <0.0001 for GLM

#calibration for GLM 
val.prob.ci.2(p=results_occ_GLM_seq$pred, y=results_occ_GLM_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.25 - very poor fit 
##############################################################################################################
#
#for rf

nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess) #what preprocessing on the fly? #specify 
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="rf", metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="rf", metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_occ_rf = nestedRepeatedCV(dataset = Training_ABI_occupation,outcomeVariable = "outcome_occupation", grid = F, control = control, preProcess = preProcess, tuneLength = 10)

results_occ_rf = results_mods_occ_rf$results
mods_occ_rf = results_mods_occ_rf$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_occ_rf_seq = combineResultsInSequence(results_occ_rf)

#Build a ROC curve object
roc_results_occ_rf_seq = roc(predictor = results_occ_rf_seq$pred, response = results_occ_rf_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_occ_rf_seq = roc_results_occ_rf_seq$auc
#What is the internal validity of our elastic net model?
roc_results_occ_rf_seq # rf AUC 0.723 95% CI: 0.688-0.759  need permutation test for significance below


#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_occ_rf_seq, auc_seq = auc_results_occ_rf_seq, predictYes = T, seq = T, dataSet = Training_ABI_occupation, outcomeVariable = "outcome_occupation")
#shows significant <0.0001 expected given confidence intervals- good result


#compare top predictors for elastic net (run previously) and rf- note SVM does not feature select in the same way 
df_coefs_occ_rf = coefEvaluation(mods = mods_occ_rf, isRF = T) #for rf- note this is just ranking variable importance as does not use coefficients 

#stability not relevant for rf


View(df_coefs_occ_rf)

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_occ_rf_seq$pred, y=results_occ_rf_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 1.138 -  - for rf 

################################################################################
#for svm
#
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess) #what preprocessing on the fly? #specify 
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="svmLinear2", metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method="svmLinear2", metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_occ_svm = nestedRepeatedCV(dataset = Training_ABI_occupation,outcomeVariable = "outcome_occupation", grid = F, control = control, preProcess = preProcess, tuneLength = 10)

results_occ_svm = results_mods_occ_svm$results
mods_occ_svm = results_mods_occ_svm$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_occ_svm_seq = combineResultsInSequence(results_occ_svm)

#Build a ROC curve object
roc_results_occ_svm_seq = roc(predictor = results_occ_svm_seq$pred, response = results_occ_svm_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_occ_svm_seq = roc_results_occ_svm_seq$auc
#What is the internal validity of our elastic net model?
roc_results_occ_svm_seq # svm AUC 0.614 95% CI 0.576-0.653 - need permutation test for significance below

#compare ROCS between our ML final models #need to compute bonferroni corrections (available https://www.sdmproject.com/utilities/?show=FDR)
roc.test(roc_results_occ_rf_seq,roc_results_occ_seq)
roc.test(roc_results_occ_rf_seq,roc_results_occ_svm_seq)
roc.test(roc_results_occ_seq,roc_results_occ_svm_seq)
roc.test(roc_results_occ_GLM_seq,roc_results_occ_rf_seq)
roc.test(roc_results_occ_GLM_seq,roc_results_occ_svm_seq)
roc.test(roc_results_occ_GLM_seq,roc_results_occ_seq)


#FDR p value corrections
p.adjust(p=c(0.000002,0.00000000008,0.00001,0.04,0.3,0.9),method ="fdr")



#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_occ_svm_seq, auc_seq = auc_results_occ_svm_seq, predictYes = T, seq = T, dataSet = Training_ABI_occupation, outcomeVariable = "outcome_occupation")
#shows significant <0.0001 expected given confidence intervals- good result again
#then adjust all permutation with FDR corrections

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_occ_svm_seq$pred, y=results_occ_svm_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.798 - svm occ


################################################################################################################
#Preprocessing outcome_qol
#
#Training Data Preprocessing #taking out outcomes to not do preprocessing on 
Training_ABI_qol = ABI_outcomes[ ,!(colnames(ABI_outcomes) %in% c("outcome_length_of_stay_weeks","outcome_accommodation","outcome_qolupation","outcome_supervision","outcome_participation","outcome_qol"))]

#Dummy code (not outcomes - required to remain factor) 
dummies = dummyVars(~ ., data = Training_ABI_qol, fullRank = T)#rules for dummy coding
Training_ABI_qol = data.frame(predict(dummies, newdata = Training_ABI_qol))#second data frame using the rules just created

#remove zero and near zero variance columns
nzv_cols = nearZeroVar(Training_ABI_qol)
if(length(nzv_cols) > 0) Training_ABI_qol = Training_ABI_qol[, -nzv_cols]

#Look at correlation - impute just for correlation as not missing at random - we will impute properly during cross-validation
#
preProcessImp = preProcess(Training_ABI_qol, method = c("center","scale","knnImpute")) #knn requires standardisation, default k=5
Training_qol_Imp = data.frame(predict(preProcessImp, Training_ABI_qol))



#Make correlation matrix (Pearson is default)
cor_all_qol = cor(Training_qol_Imp)
pdf("corPlot_qol.pdf", width = 25, height = 25)
corrplot(cor_all_qol, method = "number")
dev.off()

#Remove any columns with >0.7 correlation from training
hc = findCorrelation(cor_all_qol, cutoff=0.7) # choose"cutoff" 
hc = sort(hc)
Training_ABI_qol = Training_ABI_qol[,-c(hc)] #Remove highly correlated columns (hc)

#Add factor outcome back in 
Training_ABI_qol$outcome_qol = ABI_outcomes$outcome_qol

#remove na from outcome - not allowed to impute outcome - leak information from training to test
Training_ABI_qol = Training_ABI_qol[which(!is.na(Training_ABI_qol[, "outcome_qol"])),]

#remove columns with more than 20% missing data default
Training_qol = Training_ABI_qol[, colMeans(is.na(Training_ABI_qol)) <= 0.2] #my study change to .5 for secondary analyses


#

##################################################################################################################################################################
#Model Specification (some done before) & Estimation
#

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 10 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification #mine have repeats to 10 for larger sets >100p's
control = trainControl(method="repeatedcv", number=5, repeats=10, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)#caret does that for you to not leak data across the sets 
preProcess = c("center", "scale","knnImpute") #default k=5

set.seed(987)#make replicable (splits are random)#therefore in publication people will have same results 
ABI_qol_mod = train(outcome_qol ~ ., #Outcome against all predictors
                    data = Training_ABI_qol, #data
                    method ="glmnet", #elastic net #or change to rf etc. caret website has a list of available models and what they're callled
                    metric="ROC", #performance metric - ROC for classification problems
                    tuneLength = 10, #for elastic net, how big a grid of alpha and lambda - 10*10 - only works correctly for selectionFunction = "best" #or for rf how many trees growing
                    preProc = preProcess, #Our preprocessing for each train fold
                    trControl = control, #Our tuning method rules
                    na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Resampling results across tuning parameters
##training performance shouldn't be reported as overoptimistic 
ABI_qol_mod

#plot performance against alpha for each amount of lambda
plot(ABI_qol_mod)
#plot regularisation paths for final model alpha as lambda increases
plot(ABI_qol_mod$finalModel, label = T,xvar = "lambda")
#plot best lambda
abline(v=log(ABI_qol_mod$bestTune$lambda))
#Zoom in
plot(ABI_qol_mod$finalModel,label = T, xvar = "lambda", xlim = c(-2.5,-0.5), ylim = c(-0.5,0.5))
abline(v=log(ABI_qol_mod$bestTune$lambda))

#Look at final coefficients #svm or rf do not get coefficients
coef(ABI_qol_mod$finalModel, ABI_qol_mod$bestTune$lambda)
#Look at odds ratios - no SE provided as estimates biased due to regularisation
exp(coef(ABI_qol_mod$finalModel, ABI_qol_mod$bestTune$lambda))
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_qol_mod)#or ranks by other things depending on algorithm 
#Plot them
varImp(ABI_qol_mod)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()

#biggest predictors for qol anxiety, executive functioning, ad tbi, wais wmi, mpai abilities, multiple trauma

#Try unregularised GLM to compare
set.seed(987)#make replicable (splits are random)
ABI_qol_mod_GLM = train(outcome_qol ~ ., #Outcome against all predictors
                        data = Training_ABI_qol, #data
                        method ="glm", #elastic net
                        metric="ROC", #performance metric - ROC for classification problems
                        preProc = preProcess, #Our preprocessing for each train fold
                        trControl = control, #Our tuning method rules
                        na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias
#ROC 
ABI_qol_mod_GLM
#variable importance 
varImp(ABI_qol_mod)

#Try linear kernel SVM to compare
#SVM do not feature select but work ok with high dimensions
set.seed(987)
ABI_qol_mod_lSVM = train(outcome_qol ~ ., #Outcome against all predictors
                         data = Training_ABI_qol, #data
                         method ="svmLinear2", #SVM linear kernel
                         metric="ROC", #performance metric - ROC for classification problems
                         tuneLength = 10, #how many Costs 
                         preProc = preProcess, #Our preprocessing for each train fold
                         trControl = control, #Our tuning method rules
                         na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Apparent performance? 
ABI_qol_mod_lSVM
plot(ABI_qol_mod_lSVM)

#svm variable importance for interest
ABI_qol_mod_lSVMImp <- varImp(ABI_qol_mod_lSVM, scale = FALSE)
ABI_qol_mod_lSVMImp

#best svm predictors multiple  trauma, other med cond, executive functioning, abilties, dx tbi, alcohol abuse

#Try random forest to compare
#Feature selects
set.seed(987)
ABI_qol_mod_rf = train(outcome_qol ~ ., #Outcome against all predictors
                       data = Training_ABI_qol, #data
                       method ="rf", #random forest
                       metric="ROC", #performance metric - ROC for classification problems
                       tuneLength = 10, #how many mtrys
                       ntree = 500, #how nany trees to grow
                       preProc = preProcess, #Our preprocessing for each train fold
                       trControl = control, #Our tuning method rules
                       na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias


#Apparent performance? - best for qol but all underpowered
ABI_qol_mod_rf
plot(ABI_qol_mod_rf)
#Look at variable importance
#Intuitively, the random shuffling means that, on average, the shuffled variable has no predictive power. 
#This importance is a measure of by how much removing a variable decreases accuracy, 
#and vice versa - by how much including a variable increases accuracy.
varImp(ABI_qol_mod_rf)
#Plot them
varImp(ABI_qol_mod_rf)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()

#best predictors HADS anxiety, abilties, exec functioning, memory
# male gender much lower predictor in comparison to RLR

#
##################################################################################################################################################################
#Now next step is internal validation performance

#Custom method for glmnet only
nestedRepeatedCV = function(dataset, #dataset with outcome
                            outcomeVariable, #outcome variable name String
                            grid=F, #supplying own tune grid?
                            tuneGrid=NULL, #if true supply
                            repeatsOuter=5, #defaults to 5 outer repeats
                            cvOuter=5, #defaults to 5 outer folds
                            control, #supply control to specify how chooses best, and inner cv set up
                            tuneLength = 10, #defaults to 10*10 grid alpha lambda
                            preProcess,
                            method = "glmnet") #what preprocessing on the fly?
{
  resultsOuter = list()
  modsOuter = list()
  for(j in seq(1:repeatsOuter)) 
  {
    #make replicable
    #
    set.seed(j)
    
    #Set up nested loop
    #Outer loop is 5 (default) fold cv repeated 5 (defaults) to evaluate model on left out test data
    #Inner loop is defined by trainControl
    #
    #
    #Outcome ~ variables
    #
    splits <- createFolds(dataset[,outcomeVariable], returnTrain = TRUE, k=cvOuter)
    
    #
    results = lapply(splits, function(x, dat) 
    {
      holdout <- (1:nrow(dat))[-unique(x)]
      data.frame(index = holdout, obs = dat[,outcomeVariable][holdout])
    },
    dat = dataset)
    
    #a vector to hold the models as 5 different ones will be created from the 5 splits
    #
    mods = vector(mode = "list", length = length(splits))
    
    #
    print(paste0("Outer Repeat: ",j,"/5"))
    for(i in seq(along = splits)) 
    {
      in_train <- unique(splits[[i]])
      set.seed(j+1)
      if(grid)
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneGrid = tuneGrid, preProc=preProcess, trControl=control, na.action=na.pass)
      }else
      {
        mod <- train(as.formula(paste(outcomeVariable, "~ .")), data=dataset[in_train, ], method=method, metric="ROC", tuneLength = tuneLength, preProc=preProcess, trControl=control, na.action=na.pass)
        
      }
      results[[i]]$pred = predict(mod, dataset[-in_train, ], type = "prob", na.action = na.pass)
      
      mods[[i]] = mod
      print(paste0("Inner Repeat: ", i,"/5"))
    }
    resultsOuter[[j]] = results
    modsOuter[[j]] = mods
  }
  results_mods = list(results = resultsOuter, mods = modsOuter)  
}

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 20 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification
control = trainControl(method="repeatedcv", number=5, repeats=20, classProbs=TRUE, summaryFunction=twoClassSummary, selectionFunction = "best")

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)
preProcess = c("center", "scale","knnImpute") #default k=5

#Takes a while! 10 mins roughly...
results_mods_qol = nestedRepeatedCV(dataset = Training_ABI_qol,outcomeVariable = "outcome_qol", grid = F, control = control, preProcess = preProcess, tuneLength = 10, method = "glmnet")

results_qol = results_mods_qol$results
mods_qol = results_mods_qol$mods

#Takes a while! 10 mins roughly... for GLM
results_mods_qol_GLM = nestedRepeatedCV(dataset = Training_ABI_qol,outcomeVariable = "outcome_qol", grid = F, control = control, preProcess = preProcess, tuneLength = 10, method = "glm",repeatsOuter = 2,cvOuter = 2)

results_qo_GLM = results_mods_qol_GLM$results
mods_qol_GLM = results_mods_qol_GLM$mods

#Custom method to combine the results in a sequence in order to make a ROC curve for the whole lot
combineResultsInSequence = function(results, predictYes = T)
{
  results_seq = NULL
  for (j in seq(1:length(results)))
  {
    for (i in seq(1:length(results[[j]])))
    {
      if(predictYes)
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$Yes)
      }else
      {
        results_seq$pred = c(results_seq$pred, results[[j]][[i]]$pred$No)
      }
      results_seq$obs = c(results_seq$obs, as.character(results[[j]][[i]]$obs))
    }
  }
  results_seq$obs = as.factor(results_seq$obs)
  return(results_seq)
}

results_qol_seq = combineResultsInSequence(results_qol)

results_qol_seq

#Build a ROC curve object
roc_results_qol_seq = roc(predictor = results_qol_seq$pred, response = results_qol_seq$obs, ci = T, levels=c("No", "Yes"), direction="<")
#save AUC for permutation testing later
auc_results_qol_seq = roc_results_qol_seq$auc
#internal validity of our elastic net model
roc_results_qol_seq 
#AUC 0.728 (95% CI 0.628-0.827) - looks reasonable but need to calculate p-value by permutation testing

#Custom Method for permutation - tell it if it is a sequence result or a normal result (e.g. for external validation)
#provide results, auc, which direction you want to predict (default Y), dataet, outcome variable name
permutationPValue = function(results_seq, auc_seq, seed = 987, predictYes=T, seq=T, dataSet = NULL, outcomeVariable = NULL)
{
  set.seed(seed)
  auc_null = NULL
  #significance level <0.0001
  if (!seq)
  {
    if (is.null(dataSet)|is.null(outcomeVariable))
    {
      print("You need to define a dataset and an outcome variable!")
      pPerm = NULL
    }else
    {
      for(i in seq (1:10001))
      {
        perm = permute(dataSet[,outcomeVariable])
        #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
        #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
        if(predictYes)
        {
          auc_null = c(auc_null, roc(predictor = results_seq$Yes, response = perm, levels=c("No", "Yes"), direction="<")$auc)
        }else
        {
          auc_null = c(auc_null, roc(predictor = results_seq$No, response = perm, levels=c("No", "Yes"), direction=">")$auc)
        }
      }
      pPerm = (1+sum(auc_null >= auc_seq))/10001
    }
  }else
  {
    for(i in seq (1:10001))
    {
      perm = permute(results_seq$obs)
      #set direction explicitly so not biased towards higher roc values (just makes it less likely for things to be significant, however)
      #https://www.rdocumentation.org/packages/pROC/versions/1.15.3/topics/roc
      if(predictYes)
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction="<")$auc)
      }else
      {
        auc_null = c(auc_null, roc(predictor = results_seq$pred, response = perm, levels=c("No", "Yes"), direction=">")$auc)
      }
    }
    pPerm = (1+sum(auc_null >= auc_seq))/10001    
  }
  #get p value by taking proportion of permutated values greater or equal to the actual value
  return(pPerm)
}

permutationPValue(results_seq = results_qol_seq, auc_seq = auc_results_qol_seq, predictYes = T, seq = T, dataSet = Training_ABI_qol, outcomeVariable = "outcome_qol")
#significant <0.0001 expected given confidence intervals!

#https://github.com/nogueirs/JMLR2018
#Method from Nogueirs PhD on predictor stability for feature selection algorithms
#
# run from here until end of df_coefs_Emp = coefEvaluation(mods = mods_qol) as a complete section of code
getStability <- function(X,alpha=0.05) {
  ## the input X is a binary matrix of size M*d where:
  ## M is the number of bootstrap replicates
  ## d is the total number of features
  ## alpha is the level of significance (e.g. if alpha=0.05, we will get 95% confidence intervals)
  ## it's an optional argument and is set to 5% by default
  ### first we compute the stability
  
  M<-nrow(X)
  d<-ncol(X)
  hatPF<-colMeans(X)
  kbar<-sum(hatPF)
  v_rand=(kbar/d)*(1-kbar/d)
  stability<-1-(M/(M-1))*mean(hatPF*(1-hatPF))/v_rand ## this is the stability estimate
  
  ## then we compute the variance of the estimate
  ki<-rowSums(X)
  phi_i<-rep(0,M)
  for(i in 1:M){ 
    phi_i[i]<-(1/v_rand)*((1/d)*sum(X[i,]*hatPF)-(ki[i]*kbar)/d^2-(stability/2)*((2*kbar*ki[i])/d^2-ki[i]/d-kbar/d+1))
  }
  phi_bar=mean(phi_i)
  var_stab=(4/M^2)*sum((phi_i-phi_bar)^2) ## this is the variance of the stability estimate
  
  ## then we calculate lower and upper limits of the confidence intervals
  z<-qnorm(1-alpha/2) # this is the standard normal cumulative inverse at a level 1-alpha/2
  upper<-stability+z*sqrt(var_stab) ## the upper bound of the (1-alpha) confidence interval
  lower<-stability-z*sqrt(var_stab) ## the lower bound of the (1-alpha) confidence interval
  
  return(list("stability"=stability,"variance"=var_stab,"lower"=lower,"upper"=upper))
  
}

#Look at coefficients across 25 best models from nested cv
coefEvaluation = function(mods, isGLM = F)
{
  coefs = NULL
  
  for (j in seq(1:length(mods)))
  {
    for (i in seq(1:length(mods[[j]])))
    {
      if(isGLM)
      {
        coefs = rbind(coefs, coef(mods[[j]][[i]]$finalModel))
      }else
      {
        coefs = c(coefs, coef(mods[[j]][[i]]$finalModel, mods[[j]][[i]]$bestTune$lambda))
      }
    }
  }
  
  lengthC = NULL
  if (isGLM)
  {
    lengthC = length(coefs[1,])
  }
  else
  {
    lengthC = length(coefs[[1]])
  }
  
  #just get numbers
  coefs_extract = NULL
  
  if(isGLM)
  {
    coefs_extract = coefs
  }
  else
  {
    for(i in seq(1:length(coefs)))
    {
      coefs_extract = rbind(coefs_extract, coefs[[i]][1:lengthC])
    }
  }
  
  #get matrix of coefficients presence (1) or absence (0)
  #Presence or absence of predictors across all 14 LOSOCV models
  coefs_presence = NULL
  coefs_presence = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presenceint = coefs_extract[1:length(coefs),1:lengthC]
  coefs_presence[coefs_presence != 0] <- 1
  coefs_presenceint[coefs_presenceint != 0] <- 1
  
  #stability of feature selection http://jmlr.org/papers/volume18/17-514/17-514.pdf
  #stability across final 25 best models
  print(getStability(coefs_presence))
  
  #get rank of coef by importance as in sports ranking
  coefs_rank = NULL
  
  for(i in seq(1:(length(mods)*length(mods[[1]]))))
  {
    #rank absolute value excluding the intercept for each model
    coefs_rank = rbind(coefs_rank, rank(abs(coefs_extract[i,1:lengthC]), ties.method = "min"))
  }
  
  # rank the mean ranks of each column across all models
  coefs_rank_mean = colMeans(coefs_rank)
  
  #Invert order of rank to identify top models
  coefs_order = rank(-coefs_rank_mean)
  
  #Get the column names (not the intercept)
  if(isGLM)
  {
    coef_names = colnames(coefs[,1:lengthC])
  }else
  {
    coef_names = dimnames(coefs[[1]])[[1]][1:lengthC]
  }
  coefs_means = colMeans(coefs_extract)[1:lengthC]
  
  df = data.frame(coef_names, coefs_order, coefs_means, colMeans(coefs_presenceint))
}

df_coefs_qol = coefEvaluation(mods = mods_qol)

# Run coefficient code to here for complete code section

#Gives stability 0.104 which is very low (basically interpreted the same as a correlation coefficient)
#stability lower than 0·4 shows poor agreement between the models, 0·4 to 0·75 shows intermediate to good agreement, and higher than 0·75 shows excellent agreement.
View(df_coefs_qol)
#1 stable coefficients (hads anxiety) selected across all 25 nested best models

#Check calibration slope - no point in looking at intercept as internal validation
val.prob.ci.2(p=results_qol_seq$pred, y=results_qol_seq$obs=="Yes", 
              g=10, #number of deciles...
              logistic.cal = T, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal="blue", lwd.ideal=0.5, dostats = T)
#Slope is 0.218 - far too few observations
#
# 
#not repeated for other algorithms due to significantly overfitting

#########################################################################################################################

##############################################################################################################################
#For outcome_length_of_stay_weeks
#
#
#
#
#Preprocessing outcome_length_of_stay_weeks
#
#Training Data Preprocessing #taking out outcomes to not do preprocessing on 
Training_ABI_LOS = ABI_outcomes[ ,!(colnames(ABI_outcomes) %in% c("outcome_length_of_stay_weeks","outcome_accommodation","outcome_occupation","outcome_supervision","outcome_participation","outcome_qol"))]

#Dummy code (not outcomes - required to remain factor) 
dummies = dummyVars(~ ., data = Training_ABI_LOS, fullRank = T)#rules for dummy coding
Training_ABI_LOS = data.frame(predict(dummies, newdata = Training_ABI_LOS))#second data frame using the rules just created

#remove zero and near zero variance columns
nzv_cols = nearZeroVar(Training_ABI_LOS)
if(length(nzv_cols) > 0) Training_ABI_LOS = Training_ABI_LOS[, -nzv_cols]

#Look at correlation - impute just for correlation as not missing at random - we will impute properly during cross-validation
#
preProcessImp = preProcess(Training_ABI_LOS, method = c("center","scale","knnImpute")) #knn requires standardisation, default k=5
Training_LOS_Imp = data.frame(predict(preProcessImp, Training_ABI_LOS))



#Make correlation matrix (Pearson is default)
cor_all_LOS = cor(Training_LOS_Imp)
pdf("corPlot_LOS.pdf", width = 25, height = 25)
corrplot(cor_all_LOS, method = "number")
dev.off()

#Remove any columns with >0.7 correlation from training
hc = findCorrelation(cor_all_LOS, cutoff=0.7) # choose "cutoff" 
hc = sort(hc)
Training_ABI_LOS = Training_ABI_LOS[,-c(hc)] #Remove highly correlated columns (hc)

#Add factor outcome back in 
Training_ABI_LOS$outcome_length_of_stay_weeks = ABI_outcomes$outcome_length_of_stay_weeks

#remove na from outcome - not allowed to impute outcome - leak information from training to test
Training_ABI_LOS = Training_ABI_LOS[which(!is.na(Training_ABI_LOS[, "outcome_length_of_stay_weeks"])),]

#remove columns with more than 20% missing data default
Training_LOS = Training_ABI_LOS[, colMeans(is.na(Training_ABI_LOS)) <= 0.2] #my study change to .5 for secondary analyses


##################################################################################################################################################################
#Model Specification (some done before) & Estimation for outcome_length_of_stay_weeks
#

#Set and save rules for how to tune your hyperparameters - 
#5 fold cross-validation, repeated 10 times, "best" performance
#probabilities for using ROC, twoClassSummary for binary classification #mine have repeats to 10 for larger sets >100p's

#Set up what you are going to do for preprocessing on the fly (standardisation and imputation need to be done on train folds only, 
#to not leak information to test fold)#caret does that for you to not leak data across the sets 
preProcess = c("center", "scale","knnImpute") #default k=5

cv_5 = trainControl(method = "repeatedcv", number = 5, repeats = 10, selectionFunction = "best")

set.seed(987)#make replicable (splits are random)#therefore in publication people will have same results 

ABI_LOS_mod = train(outcome_length_of_stay_weeks ~ ., #Outcome against all predictors
                    data = Training_ABI_LOS, #data
                    method ="glmnet", #elastic net #or change to rf etc. caret website has a list of available models and what they're callled
                    preProc = preProcess, #Our preprocessing for each train fold
                    trControl = cv_5, #Our tuning method rules
                    na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias

#Resampling results across tuning parameters
#Tuning "apparent" performance RMSE= 50.37  Rsquared=0.06075 #training performance shouldn't be reported as overoptimistic however this is already very poor so no point further evaluating
ABI_LOS_mod


#plot performance against alpha for each amount of lambda
plot(ABI_LOS_mod)
#plot regularisation paths for final model alpha as lambda increases
plot(ABI_LOS_mod$finalModel, label = T,xvar = "lambda")
#plot best lambda
abline(v=log(ABI_LOS_mod$bestTune$lambda))
#Zoom in
plot(ABI_LOS_mod$finalModel,label = T, xvar = "lambda", xlim = c(-2.5,-0.5), ylim = c(-0.5,0.5))
abline(v=log(ABI_LOS_mod$bestTune$lambda))

#Look at final coefficients #svm or rf do not get coefficients
coef(ABI_LOS_mod$finalModel, ABI_LOS_mod$bestTune$lambda)
#Look at odds ratios - no SE provided as estimates biased due to regularisation
exp(coef(ABI_LOS_mod$finalModel, ABI_LOS_mod$bestTune$lambda))
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_LOS_mod)#or ranks by other things depending on algorithm 
#Plot them
varImp(ABI_LOS_mod)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()


#Try unregularised GLM to compare
set.seed(987)#make replicable (splits are random)
ABI_LOS_mod_GLM = train(outcome_length_of_stay_weeks ~ ., #Outcome against all predictors
                        data = Training_ABI_LOS, #data
                        method ="glm", #without elastic net
                        preProc = preProcess, #Our preprocessing for each train fold
                        trControl = cv_5, #Our tuning method rules
                        na.action = na.pass) #don't remove NA values, we are going to impute to increase power and minimise bias
#RMSE 52.76  0.05037Rsquared 0.04 compared to 0.06 for linear regression ML - both so poor
ABI_LOS_mod_GLM
#Variable importance - essential just absolute beta coefficients ranked
varImp(ABI_LOS_mod_GLM)

##################################################################################################################################################################
#Now next step would be internal validation performance however fit so poor does not support hypothesis
