##############################################################################################
############## Follows: text parsing/preprocessing functions #################################
##############################################################################################

# duplicate multi-label documents for learning, return a sparse 'Matrix'
duplicMultiLabel <- function (dtMat, goldStrd, statsPerDoc) {
  message ("multi-label doc. duplication");
  dtMatBis <- Matrix (0, nrow=(length(which(goldStrd==T))-nrow(dtMat)), ncol=ncol(dtMat));
  colnames(dtMatBis) <- colnames(dtMat);
  gsMatBis <- matrix (FALSE, nrow=(length(which(goldStrd==T))-nrow(dtMat)), ncol=ncol(goldStrd));
  colnames(gsMatBis) <- colnames(goldStrd);
  truthVBis <- vector();
  
  cptMatBis <- 1;
  for (d in 1:nrow(dtMat)) {
    if (d %% 500 == 0 || d == nrow(dtMat))
      cat ("\t", d, "done\n")
    if (statsPerDoc[d] > 1) {
      inds <- which (goldStrd[d,] == TRUE) [ -c(1) ]; # except th 1st label (already in dtMat)

      # duplicate document
      for (i in 1:(length(inds)) ) {
        dtMatBis[ cptMatBis, ] <- as.numeric(dtMat[d, ]);
        gsMatBis [ cptMatBis, inds[i] ] <- TRUE;
        cptMatBis <- cptMatBis + 1;
      }
      truthVBis <- c(truthVBis, colnames(goldStrd)[inds]);
    }
  }
  return (list(mat=dtMatBis, gsMat=gsMatBis, gsVec=truthVBis));
}

# duplicate multi-label documents for learning, return a sparse 'Matrix'
bootstrpRareLabel <- function (dtMat, goldStrdMat, truthVec, statsPerCode, minCodSiz) {
  dtMatBis <- Matrix (0, nrow=0, ncol=ncol(dtMat));
  colnames(dtMatBis) <- colnames(dtMat);
  gsMatBis <- matrix (FALSE, nrow=0, ncol=ncol(goldStrdMat));
  colnames(gsMatBis) <- colnames(goldStrdMat);
  truthVBis <- vector();
  
  # which ones are the rare codes ?
  rareCodes <- statsPerCode[statsPerCode < minCodSiz];
  
  for (rareCode in names(rareCodes)) {
    # how many times the code should be bootstrapped
    lackNb <- minCodSiz - rareCodes[rareCode];
    
    # randomly sample duplicate documents
    uniqInds <- which(truthVec==rareCode);
    inds <- sample(uniqInds, lackNb, replace=TRUE);
           
    dtMatBis <- rbind(dtMatBis, dtMat[inds, ]);
    gsMatBis <- rbind(gsMatBis, goldStrdMat[inds, ]);
    truthVBis <- c(truthVBis, rep(rareCode, lackNb))
  }
  rareCodes[1:length(rareCodes)] <- rep(minCodSiz, length(rareCodes));
  # statsBis are to replace old statsPerCode (only on the concerned codes)
  return (list(mat=dtMatBis, gsMat=gsMatBis, gsVec=truthVBis, statsBis=rareCodes));
}

# re-def: remove numbers and un-accentuate text
rmvNmbrUnAccent <- function(x) {
  x <- iconv(x, from="UTF-8", to='ASCII//TRANSLIT');
  return ( tm::stripWhitespace( tm::removeNumbers(x))  );
}

# re-def: remove punctuation marks
rmvPunct <- function(x) {
  return (tm::stripWhitespace( tm::removePunctuation(x, preserve_intra_word_dashes = TRUE) ) );
}

# matrix transform w.r.t. model type 
transform.matrix <- function(dtMatrix, modelType) {
  switch(tolower(modelType),
    "nb"={
     mat <- as.matrix(dtMatrix);
     rownames(mat) <- dimnames(dtMatrix)[[1]];
     colnames(mat) <- dimnames(dtMatrix)[[2]];
     mat[1,] = rep(1, ncol(dtMatrix));# PROVISOIRE : ensure all factors appear while training (add a pseudo doc with all words)
     mat <- apply(mat, 2, factor);
     
     return(mat);
    },
    "me"={
     mat <- as.matrix(dtMatrix);
     rownames(mat) <- dimnames(dtMatrix)[[1]];
     colnames(mat) <- dimnames(dtMatrix)[[2]];
     return (mat);
    },
    "dt"={
      mat <- as.matrix(dtMatrix);
      rownames(mat) <- dimnames(dtMatrix)[[1]];
      colnames(mat) <- dimnames(dtMatrix)[[2]];
      mat[1,] = rep(1, ncol(dtMatrix));# PROVISOIRE : ensure all factors appear while training (add a pseudo doc with all words)
      return(mat);
    },
    "sparse"={
      mat <- Matrix(dtMatrix, ncol=ncol(dtMatrix), nrow=nrow(dtMatrix));
      dimnames(mat)[[1]] <- rownames(dtMatrix);
      dimnames(mat)[[2]] <- colnames(dtMatrix);
      return(mat);
    },
    # default: do nothing
    {
      return (dtMatrix);
    }
  );
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

# random split lrn/tst
random.split <- function (nbRows, lrnRate) {
  border <- as.integer(nbRows * lrnRate);
  set.seed(1000);
  indices <- sample.int(nbRows-1, border-1); # line 1 must always be in lrn 'ocz it contains all-1
  return(c(1, indices + 1));
}

# Load data from a folder (all JSON files)
loadJsonFolder <- function (jsFolder, fields=NULL, docType=NULL, minContentLen=0, idPath=c('NDA','valeur')) {
  csv <- data.frame(matrix(ncol=2));
  names(csv) <- c("NDA", "TEXT");
  for (fileName in list.files(jsFolder, pattern="\\.json$")) {
    d <- loadJsonFile( paste(jsFolder, fileName, sep=""), fields, docType, minContentLen, idPath);
    # Eviter les doublons et les textes vides
    if (is.null(d) || d[1] %in% csv$NDA)
      next;
    csv <- rbind(csv, d)
  }
  
  # Remove default-NA row
  csv <- csv[-c(1),];
  # Set NDA as row names
  rownames(csv) <- csv[,"NDA"];
  return (csv);
}

# Load data from  asingle JSON file
loadJsonFile <- function (path, fields=NULL, docType=NULL, minContentLen=0, idPath=c('NDA','valeur')) {
  jsonData <- jsonlite::fromJSON(path);
  nda <- eval ( parse(text=paste("jsonData$",paste(idPath,collapse='$'), "\n", sep="")));
  # Keep only 9 first digits from NDA
  nda = substr(nda, 1, 9);
  
  # Provisoire §!!!! -- Pattern-based constraint on document type (CRO, CRH, Urologie, etc...)
  if (! is.null(docType))
    if ( ! str_detect( tolower(jsonData$TYPE$valeur), docType) )
      return (NULL);
  
  relevantFields <- fields;
  if (is.null(fields)) {
    dat <-paste(unlist(jsonData),collapse=' ');
  }
  else {
    for (varName in relevantFields) {
      #etiq <- eval ( parse(text=paste("jsonData$", varName, "$etiquette", "\n", sep="")));
      #dat <- paste (dat, etiq, sep="\n\n");
      dat <- eval ( parse(text=paste("jsonData$", varName, "$valeur", "\n", sep="")));
      }
  }
  dat <- paste (dat, gsub("<.*?>", "", dat), sep="; ");
  dat <- gsub("^\\s+|\\s+$", "", dat); # Suppr. les espaces au début et à la fin
  dat <- gsub("\n+|\r+", " ", dat); # Suppr. les sauts de lignes (entre autres pour un stockage CSV efficace)
  if ( nchar(dat) >= minContentLen)
      return (c(nda, dat))
    else
      return (NULL)
}

# Build gold-standard matrix: lines=texts, colums=codes, cell(text,code)=TRUE if text has this code
buildGoldStrd <- function (df.text, df.code, allowedCodes) {
  goldStrd <- as.data.frame(matrix(FALSE, ncol=length(allowedCodes), nrow=length(df.text[,1])) );
  names(goldStrd) <- allowedCodes;
  rownames(goldStrd) <- df.text[, 1];
  for (nda in df.text[,1]) {
    codes <- as.vector( (unique(as.matrix(df.code[df.code[,1]==nda, , ][2]))) );
    goldStrd[nda, codes] <- TRUE;
  }
  return (goldStrd);
}
