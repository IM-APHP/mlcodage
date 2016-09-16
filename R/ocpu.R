##############################################################################################
############## Follows: webserive/ocpu call functions ########################################
##############################################################################################

#' Launcher Web Service Listener
#' 
#' Launch the Web service daemon at the specified port. If the daemon is already listening, it will be stopped and replaced by the new one.
#' Please check that the message "Web interface at: \%url\%" is displayed to ensure everything went ok.
#' The coding Web interface is available at the displayed \%url\% address that you can access via navigator.
#' 
#' @export
#' @param port listening port (default 80).
#' @return FALSE if everything went ok.
#' @examples
#' launchListener(port=8020) # must return FALSE
launchListener <- function (port=80) {

  library(opencpu, quietly=TRUE);
  # Stop then strat to customize port
  opencpu$stop();
  opencpu$start(port);
  message("OpenCPU : ", opencpu$checkstatus());
  message("Listening..\nWeb interface at: ", opencpu$url(), "/library/mlcodage/www/index.html");
  return (FALSE);
}


#' Get Available Models
#' 
#' Returns the list of all available models that can be used for code prediction. The models are stored as 'RData' files within the 'data' folder of the package.
#' If you want to use custom models, please add them manually to the 'data' folder (with extension '.model.RData') then re-build the package.
#' 
#' @export
#' @return character vector with the names of available models.
#' @examples
#' getAvailModels() # returns c("Chirurgie-generale", "Chirurgie-maxillo-faciale", "Chirurgie-plastique-Pr-Revol", "Urologie")
getAvailModels <- function() {
  dat <- as.data.frame(data(package = "mlcodage")$results);
  dat <- dat[ grepl("model", dat$Item), "Item"];
  dat <- as.vector(dat);
  modelNames  <- c();
  for (i in dat) {
    modelName <- gsub("[\\(\\)]", "", regmatches(i, gregexpr("\\(.*?\\)", i))[[1]]);
    modelNames <- c( modelNames, sub("\\..+$", "", modelName) ) ;
  }
  return (modelNames);
}

#' ocpuPredict function
#' 
#' Predicts the probables codes for a text within a service
#' 
# function that takes a text and return JSON prediction : parameters should be checked for errors before calling this fun
ocpuPredict <- function (texte, service, nbCodes) {
  
  # Load 'Libelles' for code labels
  utils::data( list=c("Libelles"), package = "mlcodage" );
  
  # Load prediction model
  utils::data( list=paste(service, ".model", sep=""), package = "mlcodage" );
  
  ### Transform free text
  mtrx <- transform.matrix( tm::DocumentTermMatrix(tm::Corpus(tm::VectorSource(texte)), control=model$dtmParams) , "me");
  
  # check if text is not empty (empty = after parse, no "1" in the matrix)
  if ( rowSums(mtrx) == 0) {
    return ("Error: void (or too short) text !");
  }
  
  # Predict and return raw probabilities
  result <- stats::predict (model, mtrx, probability=TRUE);
  res <- attr (result, "probabilities");
  
  # Pick the "nbCodes" most probable ones
  
  nbCodes <- min(nbCodes, model$nclasses);
  
  ordIndices <- order(res, decreasing=TRUE);
  code <- colnames(res)[ ordIndices[1: nbCodes] ] ;
  prob <- res[ ordIndices[1: nbCodes] ];
  labels <- as.character(Libelles[code,1])
  
  # Build and return json
  json <- jsonlite::toJSON(data.frame(code=code, label=labels, prob=round(prob, 4) ), pretty=TRUE);
  json <- stringr::str_replace_all(json, "\\{", "<br/>{<br/>");
  json <- stringr::str_replace_all(json, "\\}", "<br/>}<br/>");
  
  return(json);
}

#' CCAM Code Prediction
#' 
#' CCAM code prediction function. It is used by OpenCPU Web service for Web-based prediction and can be used directly from R console.
#' 
#' @export
#' @param text medical text to be encoded.
#' @param service service name, from the list of built-in models : \{'urologie', 'chirgen', 'chirplas', 'chirmaxfac'\}. 
#' This parameter is used to pick the right model for code prediction.
#' @param nbCodes the desired number of returned codes (default 10).
#' @return string with JSON data with the ordered list of predicted codes. Each is assigned with code label and probability.
#' If an error occurs, the function returns a string with error description.
#' @examples
#' ccamCode("Résection de tumeurs de la vessie, par endoscopie", "urologie", 5) # returns a JSON string with predicted codes and probabilities.
ccamCode <- function(texte="", service="", nbCodes=10) {
  
  modelNames <- getAvailModels();
  
  if( (texte == "") || (! service %in% modelNames) || (as.numeric(nbCodes) %% 1 != 0) ) {
    return ("Error: incorrect data format ! (possible causes: void text, service is not available, number of codes is not numeric)");
  }
  else {
    return ( ocpuPredict(texte, service, as.numeric(nbCodes)) );
  }
}
