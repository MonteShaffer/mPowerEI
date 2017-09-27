
#' Build variable $healthState = 0,1,2, 3,  5 and $isPD = 0, 0.5, 1  where 0.5 and 3 are NA.
#'
#' @param dframe 
#'
#' @return updated dframe with $healthState and $isPD
#' @export
#'
#' @examples 
#' trainme = codeHealthState(mPower$walking.training);
codeHealthState = function(dframe)
{
  trainme = dframe;
  trainme$healthState = NA; 
  trainme$healthState[mPower$walking.training$medTimepoint=="Immediately before Parkinson medication"] = 0;
  
  trainme$healthState[mPower$walking.training$medTimepoint=="Another time"] = 1;
  
  
  trainme$healthState[mPower$walking.training$medTimepoint=="Just after Parkinson medication (at your best)"] = 2;
  
  trainme$healthState[mPower$walking.training$medTimepoint==""] = 3;
  trainme$healthState[is.na(mPower$walking.training$medTimepoint)] = 3;
  
  trainme$healthState[mPower$walking.training$medTimepoint=="I don't take Parkinson medications"] = 5;
  
  
  trainme$isPD = NA; 
  trainme$isPD[mPower$walking.training$medTimepoint=="Immediately before Parkinson medication"] = 1;
  
  trainme$isPD[mPower$walking.training$medTimepoint=="Another time"] = 1;
  
  
  trainme$isPD[mPower$walking.training$medTimepoint=="Just after Parkinson medication (at your best)"] = 1;
  
  trainme$isPD[mPower$walking.training$medTimepoint==""] = 0.5;
  trainme$isPD[is.na(mPower$walking.training$medTimepoint)] = 0.5;
  
  trainme$isPD[mPower$walking.training$medTimepoint=="I don't take Parkinson medications"] = 0;
  
  trainme;
}


#' Load required libraries for Synapse model prediction 
#' 
#' We assume and  
#' 
#' library(synapseClient);  
#' synapseLogin(); 
#' 
#' have already been called. 
#'
#' @return nothing
#' @export
#'

loadSubmitLibraries = function()
{
  
 
  library(caret);
  library(caretEnsemble);
  library(pROC);
  library(glmnet);
  library(e1071);
  library(randomForest);
  library(kernlab);
  library(nnet);
  library(data.table);
}



#' mPower Challenge scoring
#' 
#' resultme = PD_score_challenge1(submitme);
#' str(resultme$error);
#'
#' @param training_features 
#'
#' @return ensemble_model
#' @export
#'

PD_score_challenge1<-function(training_features){
  
  #manipulate incoming data into dataframe
  training_features<-as.data.frame(training_features)
  recordidname<-names(training_features)[1]
  featurenames<-names(training_features)[-1]
  training_features[,featurenames] <- sapply( training_features[,featurenames], as.numeric )
  
  print("Reading and merging covariates")
  #Read-in and merge covariates
  training_features<-download_merge_covariate(training_features)
  
  print("Summarizing the data")
  #Summarize by Median
  covs_num<-c("age")
  covs_fac<-c("gender")
  groupvariables<-c("healthCode", "medTimepoint", "professional.diagnosis", covs_num, covs_fac) #"age", "gender") #, "appVersion.walktest", "phoneInfo.walktest")
  
  ensemble_model<-NULL
  
  if(any(is.na(training_features[,featurenames]))){
    print("WARNING: Missing values/records not allowed!")
  } else {
    dttrain<-data.table(training_features)
    mediantraining<-dttrain[, lapply(.SD, median, na.rm=TRUE), by=groupvariables, .SDcols=featurenames ]
    mediantraining<-as.data.frame(mediantraining)
    
    print("Fitting the model")
    #Fit model
    ensemble_model<-fit_model(mediantraining, featurenames, covs_num, covs_fac)
  }
  
  return(ensemble_model)
}



#' Covariate data downloaded remotely from syntable
#'
#' @param features 
#'
#' @return mergeddata with covariates (age/gender) from demos$
#' @export
#'

download_merge_covariate<-function(features){
  
  #download correct covariates
  synid<-"syn10233116"
  
  syndemos<-synGet(synid)
  demos<-read.csv(attributes(syndemos)$filePath, header=T, as.is=T)
  
  # Merge Data
  mergeddata<-cbind(demos, features[match(demos$recordId.walktest, features[,1]), -1])
  if(dim(features)[2]==2){
    names(mergeddata)[dim(mergeddata)[2]]<-names(features)[2]
  }
  
  return(mergeddata)
}

#' Ensemble Training
#'
#' @param training 
#' @param featurenames 
#' @param covs_num 
#' @param covs_fac 
#'
#' @return greedy_ensemble  
#' @export
#'

fit_model<-function(training, featurenames, covs_num, covs_fac){
  
  trainoutcome<-training$professional.diagnosis
  trainoutcome<-factor(ifelse(trainoutcome, "PD", "Control"))
  
  trainfeatures<-training[,c(covs_num, covs_fac,featurenames)]
  trainfeatures[,covs_fac]<-sapply(trainfeatures[,covs_fac], as.factor)
  
  print("Run Control")  
  set.seed(1234)
  myControl <- trainControl(  method="boot",
                              number = 50,
                              savePredictions="final",
                              classProbs=TRUE,
                              index=createResample(trainoutcome, 50),
                              summaryFunction=twoClassSummary )
  
  
  tmpmmatrix<-model.matrix( ~ ., trainfeatures)
  tmpmmatrix<-tmpmmatrix[,-1]
  
  print(dim(tmpmmatrix))
  print(length(trainoutcome))
  print(table(trainoutcome))
  
  print("Caret Call")
  model_list <- caretList(
    y=trainoutcome , x=tmpmmatrix,
    trControl=myControl,
    metric = "ROC",
    tuneList=list(glmnet = caretModelSpec(method = "glmnet"), 
                  rf = caretModelSpec(method="rf"), 
                  svmLinear = caretModelSpec(method="svmLinear"), 
                  knn = caretModelSpec(method="knn"), 
                  nnet = caretModelSpec(method = "nnet", trace=F)),
    continue_on_fail = FALSE, preProc = c("center", "scale")
  )
  
  print("Ensemble run")
  greedy_ensemble <- caretEnsemble(
    model_list, 
    metric="ROC",
    trControl=trainControl(
      number=length(model_list),
      summaryFunction=twoClassSummary,
      classProbs=TRUE
    ))
  return(greedy_ensemble)
}


