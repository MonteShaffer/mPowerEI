
#' Load Synapse Data From Tables Assigned to Variables
#'
#' The config file has default values: synapseTables and synapseVariables should be of the same length
#'
#' @param synapseTablesOverride [optional] string vector to override the config-file synapseTables 
#' @param synapseVariablesOverride [optional] string vector to override the config-file synapseVariables 
#'
#' @return vector of strings on success
#' @export
#'
#' @examples
#' loadSynapse('./mPowerEI/example/config.txt'); loadSynapseData();
#'  
loadSynapseData = function( synapseTablesOverride = NULL, synapseVariablesOverride = NULL)
{
  if (length(synapseTablesOverride) > 0)
  {
    synapseTables = synapseTablesOverride
  }
  
  if (length(synapseVariablesOverride) > 0)
  {
    synapseVariables = synapseVariablesOverride
  }
  
  
  
  
  if (!exists("synapseProject"))
  {
    stop("Missing synapseProject variable - Required for Data Access")
  }
  
  if (!exists("synapseTables"))
  {
    stop("Missing synapseTables variable - Required for Data Access")
  }
  
  if (!exists("synapseVariables"))
  {
    stop("Missing synapseVariables variable - Required for Data Access")
  }
  
  
  
  
  if (length(synapseVariables) !=  length(synapseTables))
  {
    stop("Length mismatch: synapseTables and synapseVariables")
  }
  
  tableCache = paste(localCache,"summaryObjects",sep="/");
    if(!dir.exists(tableCache)) { dir.create(tableCache); }
  
  # store in one object as a list of objects, since get/assign is not working as expected.
  projectObj = list();
  n = length(synapseTables); 
  
  tableFile = paste(tableCache,paste(synapseProject,".Rda",sep=''),sep="/");
  
  if(!file.exists(tableFile))
  {
    msg = paste("Project File [",synapseProject,"] does not exist ... creating");
    print(msg); flush.console();

  for(i in 1:n)
    {
    sv = synapseVariables[i];
    st = synapseTables[i];
    
    msg = paste("Grabbing Data From Table [",st,"]");
    print(msg); flush.console();

    
    temp <- synTableQuery( paste("select * from ",st,sep='') ); 
    
    msg = paste("Data Cached To Variable [",synapseVariables[i],"]");
    print(msg); flush.console();

    
    projectObj[sv] = (temp);
    
   
    
  }
    save(projectObj,file=tableFile);
    
  }
  
  msg = paste("Project File [",synapseProject,"] loading ... ");
  print(msg); flush.console();

  
  
  load(tableFile);
  
  projectList = list();
  for(i in 1:n)
  {
    sv = synapseVariables[i];
    projectList[[sv]] = attr(projectObj[[sv]],"values");
  }
  
    
  return(projectList);
  
}