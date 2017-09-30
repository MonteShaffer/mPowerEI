
#' Load Synapse Library and Login to Access the Data
#'
#' The config file has username, password, and local cache folders
#'
#' @param configfile full path to configuration file with necessary instance variables (username, password, localcache)
#' @param login boolean value (true or false) to process login with 
#' @param rememberMe boolean value (true or false) to keep session alive with Synapse
#' @param synapseUserOverride string to override the config-file username to login to Synapse 
#' @param synapsePassOverride string to override the config-file password to login to Synapse 
#'
#' @return string on success
#' @export
#'
#' @examples
#' loadSynapse('./mPowerEI/example/config.txt');
#' 
#' loadSynapse('./mPowerEI/example/config.txt',login=F);
#' 
#' loadSynapse('./mPowerEI/example/config.txt',login=T,synapseUserOverride='monte.shaffer@gmail.com',synapsePassOverride='myNewPassword');
#' 
loadSynapse = function(configfile, login = TRUE, rememberMe = TRUE, 
    synapseUserOverride = character(0), synapsePassOverride = character(0))
    {
    
    if (!exists("configfile"))
    {
        stop("Missing configfile")
    }
    if (!file.exists(configfile))
    {
        stop(paste0("Required configfile not found: ", configfile))
    }
    source(configfile)
    
    if (!exists("localCache"))
    {
        stop("Missing localCache variable - Required")
    }
    
    if (!exists("synapseCache"))
    {
        stop("Missing synapseCache variable - Required")
    }
    
  
  #library(synapseClient); # should include outside of this function ...
    synapseCacheDir(synapseCache)
    
    
    if (login == T)
    {
        if (!exists("synapseUser"))
        {
            stop("Missing synapseUser variable - Required for Login")
        }
        
        if (!exists("synapsePass"))
        {
            stop("Missing synapsePass variable - Required for Login")
        }
        
        if (length(synapseUserOverride) > 0)
        {
            synapseUser = synapseUserOverride
        }
        
        if (length(synapsePassOverride) > 0)
        {
            synapsePass = synapsePassOverride
        }
        
        synapseLogin(username = synapseUser, password = synapsePass, 
            rememberMe = rememberMe)
        
    }
    return("... Load Synapse ... COMPLETE!")
    
}