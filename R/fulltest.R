
#' Build Predictive Models from CSV
#' 
#' Build a new code prediction model from CSV data. The function takes two mandatory 2-column dataframes: 'csv' and 'diags'. See below for details.
#' 
#' @export
#' @param csv a data frame with 2 columns: 'NDA' (a unique line identifier), and 'TEXT (free text).
#' @param diags a data frame with 2 columns: 'NDA' (a unique line identifier), and 'DIAG' (alpha-num codes).
#' @param modelType model type from \{"SVM", "NB"\} for Support Vector Machine, and Naive Bayes respectively.
#' @param multiLabMod a string argument to specify how to deal with multilabeled texts. Put "d" if you want to duplicate them (the same text is considered multiple times with one code at each), or any other value to ignore multilabeled texts.
#' @param minCodSiz minimum code size to be reached (numeric). If this argument is set to K, then the codes with less than K texts will be bootstrapped to reach this minimum.
#' @return an object of type "svm" or "nb" (depends on the specified 'modelType' argument.).
#' If you want to use the built model within 'mlcodage' Web service, you must add it to the 'data' folder of the package in the form of an RData object with extension ".model.RData', then re-compile the package.
#' @examples
#' csv = data.frame(NDA=c("T1", "T2"), TEXT=c("text numer one", "text number two"))
#' diags = data.frame(NDA=c("T1", "T2"), DIAG=c("CODE1", "CODE2"))
#' buildFromCsv(csv, diags, modelType="SVM", multiLabMod="!d", minCodSiz=0)
buildFromCsv <- function (csv, diags, modelType="SVM", multiLabMod="!d", minCodSiz=0) {
  # Chek data format
  if (colnames(csv)[1] != "NDA" | colnames(csv)[2] != "TEXT") {
    return ("Error: 'csv' object must have exactly two columns: 'NDA', 'TEXT' !")
  }
  if (colnames(diags)[1] != "NDA" | colnames(diags)[2] != "DIAG") {
    return ("Error: 'diags' object must have exactly two columns: 'NDA', 'DIAG' !")
  }
  if ( length(unique(rownames(csv))) != length(rownames(csv)) || length(unique(rownames(diags))) != length(rownames(diags)) ) {
    return ("Error: 'csv' and 'diags' objects must have unique row names !")
  }
  
  # Put project folder here
  setwd("E:/Code/mlcodage");
  
  # for new data only: save the freq.-wise sorted list of codes (next time, you can restrict on N codes)
  allowedCodes <- sort(table(diags$DIAG), decreasing=TRUE);
  allowedCodes <- allowedCodes[allowedCodes > 0]; # discard non-present codes
  allowedCodes <- names(allowedCodes);
  message(c("#distint codes = ", length(allowedCodes) ));
  
  # Build gold-standard matrix (ground truth)
  cat("Build gold-standard matrix..");
  goldStrd <- buildGoldStrd (csv, diags, allowedCodes);
  cat ("ok\n");
  gc();
  
  # identify 0-labeled and multi-labeled docs
  cat ("\nComputing stats/NDA..");
  statsPerNda <- rowSums(goldStrd);
  cat ("ok\n");
  
  # Discard docs with #labels == 0 (if multiLabelModel != "d" (duplicate), then discard multilabeled docs too)
  inds = c();
  if (multiLabMod != "d") {
    cat ("Discarding docs. with '0' or '>1' labels");
    inds <- which(statsPerNda != 1 );
  } else {
    cat ("Discarding docs. with '0' labels, others are duplicated");
    inds <- which(statsPerNda == 0 );
  }
  
  if (length(inds) > 0) {
    message (paste(length(inds), "documents discarded."));
    csv <- csv [ -c(inds), ];
    goldStrd <- goldStrd [ -c(inds), ];
    statsPerNda <- statsPerNda[ -c(inds) ];
    message(paste(length(csv$NDA), "documents kept."));
  }
  nbUniqDocs <- nrow(csv);
  
  # After removing these NDAs, some codes may be empty (no more docs): update stats
  inds <- names(goldStrd)[ which(colSums(goldStrd)==0) ];
  goldStrd <- goldStrd[ , !(names(goldStrd) %in% inds) ];
  
  # compute stats/code (#docs per code)
  cat ("\nComputing stats/code..");
  statsPerCode <- colSums(goldStrd);
  cat ("ok\n");
  
  # Trandform gold-standard matrix into a vector of codes (some algos need this)
  truthV <- apply(goldStrd, 1, function(x) colnames(goldStrd)[which.max(x)]);
  
  cat ("Data parse..");
  dtmParams <- getDefaultDtmParams();
  dtm <- transform.matrix( DocumentTermMatrix(Corpus(VectorSource(csv$TEXT)), control=dtmParams) , "sparse");
  cat ("ok\n");
  message(c("#tokens = ", ncol(dtm) ));
  
  # duplicate docs for multi-label lrn: each doc with >1 label is duplicated for each of its labels
  if (multiLabMod == "d") {
    cat ("multi-label doc. duplic...");
    newDtm <- duplicMultiLabel(dtm, goldStrd, statsPerNda);
    cat("ok\n");
    message("#new docs. created = ", length(newDtm$gsVec));
    
    dtm <- rbind(dtm, newDtm$mat);
    rownames(dtm) <- make.unique(rownames(dtm));
    goldStrd <- rbind(goldStrd, newDtm$gsMat);
    rownames(goldStrd) <- make.unique(rownames(goldStrd));
    truthV <- c(truthV, newDtm$gsVec);
    rm(newDtm);
  }
  
  # duplicate docs for rare labels: each code with '< minCodSiz' is bootstrapped by duplicating docs with same code
  if (minCodSiz > 1) {
    cat (paste("rare-label doc. duplic. with ", minCodSiz, " docs. each..", sep="") );
    newDtm <- bootstrpRareLabel(dtm, goldStrd, truthV, statsPerCode, minCodSiz);
    cat("ok\n");
    message("#new docs. created = ", length(newDtm$gsVec));
    
    dtm <- rbind(dtm, newDtm$mat);
    rownames(dtm) <- make.unique(rownames(dtm));
    goldStrd <- rbind(goldStrd, newDtm$gsMat)
    rownames(goldStrd) <- make.unique(rownames(goldStrd));
    truthV <- c(truthV, newDtm$gsVec);
    statsPerCode[names(newDtm$statsBis)] = newDtm$statsBis;
    rm(newDtm);
  }
  gc();
  
  nbDocsAll <- nrow(dtm);
  message("#all docs. = ", nbDocsAll);
  
  lrnLabels <- truthV;
  allLabels <- unique(truthV);
  message("ok");
  
  truthV <- factor(truthV);
  lrnLabels <- factor(lrnLabels);
  
  allLabels <- levels(truthV);
  nbLabelsAll <- length(allLabels);
  message("# distinct labels = ", nbLabelsAll);
    
  # transform matrices into the right format (sparse, dense...)
  lrnMtrx = transform.matrix(dtm, modelType);
  
  # Build ML model
  cat ("\nModel build..");
  datB <- Sys.time();
  model <- buildModel (modelType, lrnMtrx, lrnLabels, buildParams[modelType]);
  datE <- Sys.time();
  # save dtmParams and dictionary (tokens) into model object
  dtmParams$dictionary <- colnames(dtm);
  dtmParams$stopwords <- NULL; # not necessary
  dtmParams$bounds <- NULL; # take all the words in the (future) test text
  model$dtmParams <- dtmParams;
  
  cat("[", round(as.numeric(datE-datB)/60, 2) ,"] min. ok\n");
  
  cat("To be used within 'mlcodage' package, the model must be saved into 'data' directory of the package. 'RData' file should contain one sigle object called 'model'.")
  return(model);
}

#' Generate CSV objects from a set of JSON Files
#' 
#' Generate 'csv' and 'diags' objects starting from a floder with a plenty of JSON files.
#' These objects can then be given to function 'buildFromCsv' in order to build a predictive model.
#' 
#' @export
#' @param jsFolder path to your local folder containing JSON files.
#' @param diagsPath path to your local RData file containing diagnostics (codes). The file must contain an object called "Diagnostics" with at least two columns ('NDA', and 'ACTE').
#' @param corpusName choose a name for your corpus.
#' @param serviceName choose a service name from the built-in services: \{'urologie', 'chirgen', 'chirplas', 'chirmaxfac'\}.
#' This argument is used to choose the right UH (Hospital Unit) as well as the right variables from the JSON.
#' If you don't want any restriction regarding UH and VAR, please set serviceName to 'gen'.
#' @return a list of two dataframes: 'csv' and 'diags' to be given separately to function 'buildFromCsv'.
csvFromJSONs <- function (jsFolder, diagsPath, corpusName, serviceName) {
  # MiddleCare variables (different from a service to another)
  fields = list();
  fields[["urologie"]] <- unique(c("VAR463", "VAR468", "VAR467", "VAR484", "VAR653")); # URO-CRO
  fields[["chirgen"]] <- unique(c("VAR91", "VAR90", "VAR539", "VAR5", "VAR6", "VAR275")); # CHIRGEN-CRO
  fields[["chirplas"]] <- unique(c("VAR8", "VAR59", "VAR60", "VAR61", "VAR62", "VAR646")); # CHIRPLAS-CRO
  fields[["chirmaxfac"]] <- unique(c("VAR7", "VAR18")); # MAXILLO-CRO
  fields[["gen"]] <- unique(unlist(fields)); # GENERIC
  fields = fields[[serviceName]];
  if (length(fields) < 1)
    stop("Error: 'fields' is empty, please check your 'serviceName' variable !");
  
  # UH_EXEC of the concernec service
  uhs <- list();
  uhs[["urologie"]] <- unique(c("76819", "76176", "76173", "76175", "76191", "76192", "76505", "76536", "76834", "76129")); # URO-CRO
  uhs[["chirgen"]] <- unique(c("76824", "76267", "76153")); # CHIRGEN-CRO
  uhs[["chirplas"]] <- unique(c("76818", "76267", "76241", "76242", "76216", "76219", "76234")); # CHIRPLAS-CRO
  uhs[["chirmaxfac"]] <- unique(c("76816", "76267")); # MAXILLO-CRO
  uhs[["gen"]] <- unique(unlist(uhs)); # GENERIC
  uhs = uhs[[serviceName]];
  if (length(uhs) < 1)
    stop("Error: 'uhss' is empty, please check your 'serviceName' variable !");
  
  # Load texts either: from PostGresQL (mimic), or JSON files (all files must be in same folder)
  cat("Loading text from: ");
  if ( stringr::str_detect( tolower(corpusName), "mimic")) { # load from PostGresql
    cat ("PostgreSQL db");
    res <- get.ds.icd(NULL, NULL, 0, renewCon=TRUE);
    csv <- data.frame(NDA=paste0(res[["subject_id"]], res[["hadm_id"]]), TEXT=res[["text"]])
    csv <- unique(csv[c("NDA", "TEXT")]);
  } else { # load from disk
    cat ("disk..");
    csv <- loadJsonFolder (jsFolder, fields, docType=NULL, 1)
  }
  cat ("ok\n");
  
  nbDocsAll <- nrow(csv);
  message (nbDocsAll, " retrieved documents.\n");
  
  # Load codes (labels)
  if ( stringr::str_detect( tolower(corpusName), "mimic")) { # from MIMIC data
    diags <- data.frame(NDA=paste0(res[["subject_id"]], res[["hadm_id"]]), DIAG=res[["code"]]);
    csv[["NDA"]] = as.character(csv[["NDA"]]); # diagnostics should be strings
    rm(res);
    gc();
  } else { # from a separate R object provided by Rémi : ONLY ONE LINE SHOULD BE UNCOMMENTED
    #diags <- loadDiags(diagsPath, serviceLabel, diagType, allowedCodes); # un-comment this for CRHs
    diags <- loadActes(diagsPath, uhs, NULL); # un-comment this for CROs
    
    # keep only present NDAs
    diags <- diags[diags$NDA %in% csv$NDA, , ];
  }
  return (list(csv=csv, diags=diags));
}

# Return the default parsing parameters.
getDefaultDtmParams <- function() {
  
  stopWordPath <- paste(getwd(), "/data/sw200.txt", sep=""); # stop word file list (one word /line)
  corpusLanguage <- "french";
  
  # return Global tokenizing params #########################
  dtmParams <- list(
    dictionary = NULL, # A character vector of terms wose will be listed in the result
    bounds = list(local = c(1, Inf), global = c(1, Inf)), # Terms that appear less often in doc than the lower bound bounds$local[1] or more often than the upper bound bounds$local[2] are discarded
    wordLengths = c(2, Inf), # Words shorter than the minimum word length wordLengths[1] or longer than the maximum word length wordLengths[2] are discarded
    weighting = weightBin,
    removePunctuation = rmvPunct,
    removeNumbers = rmvNmbrUnAccent,
    stemming = TRUE,
    stopwords = stemDocument(as.character(read.csv(stopWordPath, stringsAsFactors=FALSE, encoding="UTF-8")[,1]), lang=corpusLanguage),
    tolower = TRUE,
    minTokenSize = 2,
    maxTokenSize = 3,
    tokenize = "words" # can be "MC", "scan", "words" or a custom function
  );
  return(dtmParams);
}