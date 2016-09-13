
#' launchListener function
#' 
#' CCAM Code Prediction daemon launcher through OpenCPU
#' 
#' @export
#' @param port listening port (default 5626). Required.
launchListener <- function (port=80) {

  library(opencpu, quietly=TRUE);
  # Stop then strat to customize port
  opencpu$stop();
  opencpu$start(port);
  message("OpenCPU : ", opencpu$checkstatus());
  message("Listening on : ", opencpu$url());
}


#' getAvailModels function
#' 
#' Return the list of all available models that can be used for prediction
#' 
#' @export
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
#' @export
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

#' ccamCode function
#' 
#' CCAM Code Prediction interface (to be called from mlcodage:: package)
#' 
#' @export
#' @param text discharge summary. Required.
#' @param service the service from urology, surgery, ... etc, used to pick the right model. Required.
#' @param nbCodes number of returned codes (default 10). Required.
ccamCode <- function(texte="", service="", nbCodes=10) {
  
  modelNames <- getAvailModels();
  
  if( (texte == "") || (! service %in% modelNames) || (as.numeric(nbCodes) %% 1 != 0) ) {
    return ("Error: incorrect data format !");
  }
  else {
    return ( ocpuPredict(texte, service, as.numeric(nbCodes)) );
  }
}
