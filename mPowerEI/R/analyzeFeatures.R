
# do system-wide loops on different analyses
getPedometerFeatures = function()
{
  # we are building for testing, so will use all 79,000 records
  
  myO = paste(localCache,"summaryObjects","",sep="/");
  pedF = paste(myO, paste(synapseProject,"PEDOMETER",sep='-'), ".Rda", sep='');
  
  pframe = data.frame();
  
  records = names(audit$rclist);
    rlen = length(records);
  for(i in 1:rlen)
  {
    print(paste(i," of ",rlen)); flush.console();
    rv = records[i];
    rvObj = getMotionObject(rv);
    
    pfeat = getPedometerFeaturesFromRecord(rvObj);
    pframe = rbind(pframe,pfeat);
    
  }
  
  
}






#' Analyze Features
#'
#' Graphs, Plots, and Play
#' @return nothing
#' @export
#'

analyzeFeatures = function() {}
