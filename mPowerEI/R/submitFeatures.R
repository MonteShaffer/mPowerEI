

#' Convert Factor to Number
#'
#' 
#' @param dframerow 
#' @param myK 
#' @param myS
#'
#' @return
#' @export
#'
#' @examples
scoreFactor = function(dframerow,myK,myS)
{
  myV = dframerow[[myK]];
  
  #print(myK);print(myS);print(myV);
  
  if(myS == "healthState")
  {
    if(length(myV) < 1) { return(3.333); }
    if(is.na(myV)) { return(3.333); }
    if(myV =="Immediately before Parkinson medication") { return(0); }
    if(myV =="Another time") { return(1); }
    if(myV =="Just after Parkinson medication (at your best)") { return(2); }
    if(myV =="I don't take Parkinson medications") { return(5); }
    
    return(3);
  } else if(myS == "isPD")
  {
    if(length(myV) < 1) { return(0.666); }
    if(is.na(myV)) { return(0.666); }
    if(myV =="Immediately before Parkinson medication") { return(1); }
    if(myV =="Another time") { return(1); }
    if(myV =="Just after Parkinson medication (at your best)") { return(1); }
    if(myV =="I don't take Parkinson medications") { return(0); }
    
    return(0.5);
  }  else if(myS == "age")
  {
    if(length(myV) < 1) { return(33.333); }
    #if(is.na(myV)) { return(33.333); }
    
    myV = as.numeric(myV);
    if(is.na(myV)) { return(33.222); }
   
    return(myV);
  }  
  else if(myS == "gender")
  {
    if(length(myV) < 1) { return(0.5005); }
    if(is.na(myV)) { return(0.5115); }
    if(myV =="Male") { return(1); }
    if(myV =="Female") { return(0); }
    
    
    return(0.5);
  }   else { } 
  
}

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
  
  # https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
  library("mlbench");
  library("rpart");
  
  library("caTools");
  library("gbm");
  
  
  
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


#' Dampen outliers
#'
#' @param dframe 
#' @param xfeats numeric vector, list of columns to be evaluated
#' @param iCut number, number of IQRs to use to truncate min/max
#'
#' @return updated dframe
#' @export
#'

dampenOutliers = function(dframe,xfeats,iCut=6)
{
  # iCut is number of IQR to truncate to
  pmed = plyr::colwise(median)(dframe[,xfeats]);  
  pmean = plyr::colwise(mean)(dframe[,xfeats]);
  psd = plyr::colwise(sd)(dframe[,xfeats]); 
  pmin = plyr::colwise(min)(dframe[,xfeats]); 
  pmax = plyr::colwise(max)(dframe[,xfeats]); 
  piqr = plyr::colwise(IQR)(dframe[,xfeats]);  
  
  pcutMin = pmed - iCut*piqr;
  pcutMax = pmed + iCut*piqr;
 
  rframe = dframe;
  xlen = length(xfeats);
  xnames = names(pfeats[,xfeats]);
  for(i in 1:xlen)
    {
    xname = xnames[i];
      xval = as.numeric(pcutMin[xname]);
    xidx = xfeats[i];
    
    rframe[[xname]][ rframe[[xname]] < xval  ] = xval; 
      xval = as.numeric(pcutMax[xname]);
    rframe[[xname]][ rframe[[xname]] > xval  ] = xval; 
    
    
    #rframe[,xidx] = ifelse(x < xval, xval, x);
    #rframe[,xidx] = ifelse(x > xval, xval, x);
    }
  
  
  rmed = plyr::colwise(median)(rframe[,xfeats]);  
  rmin = plyr::colwise(min)(rframe[,xfeats]); 
  rmax = plyr::colwise(max)(rframe[,xfeats]); 
  
rframe;  
}


#' Internal Test of Greedy Ensemble with Predictions and Accuracy
#'
#' @param submitme the dataframe with submission, not entire synapseSubmission, $r and $h are required.
#' @param split number, 1-99, what percent of the data will be sampled into train vs test set (e.g., 80 means 80 into train, 20 into 'out of sample' test)
#' @param dv string, the name of the column that will be the dv
#' @param dvv numeric vector, the values of the DV that will be classified, first = 0, second = 5
#' @param dvf string vector, the factors of the DV that will be classified, first = 'PD.off', second = 'nonPD'
#' @param xfeats numeric vector, the column numbers that define the features
#' @param dreplace string, how do we replace duplicates in analysis set, "first" is just the first value, "median" is the median of all values, "none" skips this step...
#'
#' @return list of items from caret, model = , ensemble = , predict = , accuracy = , control = 
#' @export
#'

mPower_test<-function(submitme, dv="healthState", dvv = c(0,5), dvf=c("PD.off","nonPD"), xfeats=1:20, split=80, dreplace="none")
  {
  timerx = NULL; timeru = NULL; timeri = NULL;
  
  tstart = Sys.time();
  # submitme has some noise applied to imputate, so if we re-imputate, we will get slightly different values ...
  # this function samples based on 'split' so a given run will have slightly different values
  # $r and $h are for recordId and healthCode
  
  play = submitme;
  
  # subset based on dvv
  psets = NULL;
  for(dva in dvv)
    {
    print(dva);
    pset = subset(play, play[[dv]] == dva);
    psets = rbind(psets,pset);
    }
  
  play = psets;
  #set.seed(9876); # if you want repeatable
  play = play[sample(nrow(play)),];  # sample by row ... randomize
  
  myC = c();
    myC = c(myC, which(colnames(play)=="h"));
    myC = c(myC, which(colnames(play)=="r"));
    myC = c(myC, which(colnames(play)==dv));
    myC = c(myC, xfeats);
  
  xlen = length(xfeats);
  xpos = 4:(3+xlen);
  
  playN = dim(play)[1];
  playS = floor(split/100 * playN);
  
  ptrain = play[1:playS,myC];           # 14340
  ptest  = play[(1+playS):playN,myC];   #  3585  [0.25]
  
  tend = Sys.time(); timer = tend - tstart; print(timer); flush.console();
    timerx = c(timerx,as.character(timer));
    timeru = c(timeru, attr(timer,"units") );
    timeri = c(timeri, "init");
  
  
  
  tstart = Sys.time();
  if(dreplace != "none")
  {
  # replace duplicates with "median" or merely "first value"
    ptrain = replaceDuplicates(ptrain,xpos,dreplace); # 2354
    ptest = replaceDuplicates(ptest,xpos,dreplace);   # 1218   [0.52]
  }
  tend = Sys.time(); timer = tend - tstart; print(timer); flush.console();
 
    timerx = c(timerx,as.character(timer));
    timeru = c(timeru, attr(timer,"units") );
    timeri = c(timeri, "duplicates");
  
  
  tstart = Sys.time();
  ## fit model
  model.dv = ptrain[,3];
    model.dvf = factor(model.dv,levels=dvv,labels=dvf);
    model.features = ptrain[,xpos];
    model.features.names = names(model.features);
  
  
  print("Run Control"); flush.console(); 
  set.seed(1234)
  model.control <- trainControl(  method="boot",
                              number = 50,
                              savePredictions="final",
                              classProbs=TRUE,
                              index=createResample(model.dvf, 50),
                              summaryFunction=twoClassSummary );
  
  tmpM = model.matrix( ~ ., model.features);  # useful if we have factors - factors to dummies
                                              # we are not allowed to include factors, so?
                                              # [,-1] removes the intercept
  
  print(dim(model.features));
  print(length(model.dv));
  print(table(model.dvf));
  
  tend = Sys.time(); timer = tend - tstart; print(timer);
  
  timerx = c(timerx,as.character(timer));
  timeru = c(timeru, attr(timer,"units") );
  timeri = c(timeri, "control");
  
  
  
  
  tstart = Sys.time();
  print("Caret Call"); flush.console();
  model.list <- caretList(
    y=model.dvf , x=model.features,
    trControl=model.control,
    metric = "ROC",
    tuneList=list(glmnet = caretModelSpec(method = "glmnet"), 
                  rf = caretModelSpec(method="rf"), # random forest
                  svmLinear = caretModelSpec(method="svmLinear"), 
                  knn = caretModelSpec(method="knn"), 
                  nnet = caretModelSpec(method = "nnet", trace=F)),
    continue_on_fail = FALSE, preProc = c("center", "scale")
  )
  
  
  tend = Sys.time(); timer = tend - tstart; print(timer);
  
  timerx = c(timerx,as.character(timer));
  timeru = c(timeru, attr(timer,"units") );
  timeri = c(timeri, "caret");
  
  
  
  tstart = Sys.time();
  
  print("Ensemble run"); flush.console();
  greedy_ensemble <- caretEnsemble(
    model.list, 
    metric="ROC",
    trControl=trainControl(
      number=length(model.list),
      summaryFunction=twoClassSummary,
      classProbs=TRUE
    ))
  
  
  
  
  tend = Sys.time(); timer = tend - tstart; print(timer);
  
  timerx = c(timerx,as.character(timer));
  timeru = c(timeru, attr(timer,"units") );
  timeri = c(timeri, "ensemble");
  
  
  
  
  
  }




#' Replace 'healthCode' duplicates in dataframe
#'
#' @param dframe dataframe
#' @param xpos positions of xfeats that need to be analyzed
#' @param dreplace string, how to replace with "first" value or "median"
#'
#' @return updated dataframe without replicates
#' @export
#'

replaceDuplicates = function(dframe, xpos, dreplace="first")
{
  
  rframe = NULL;
  myHs = NULL;
  # stack approach, we are not going to adjust order .. not efficient
  dlen = dim(dframe)[1];
  
  for(i in 1:dlen)
  {
  #print(paste(i," of ",dlen)); flush.console();  
    drow = dframe[i,];
    dh = as.character(drow$h);
  #print(dh);
    # index 3 is the dv
    dv = as.numeric(drow[3]);
    
    if( length( which(myHs == dh) ) < 1 )
    {
      # not found
      if(dreplace=="first") {
        dreplacerow = drow;
      } else if(dreplace=="median") {
        dsub = subset(dframe,dframe$h==dh);
          dmed = plyr::colwise(median)(dsub[,c(3,xpos)]);  # median of state?
        dreplacerow = drow;
        dreplacerow[xpos] = dmed[-1];
      }
      
    
    rframe = rbind(rframe,dreplacerow);    
    myHs = c(myHs,dh); # added to stack
    }
      
    
  }
  
  
  
  rframe;
  
}



