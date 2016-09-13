
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

# re-def
rmvNmbrUnAccent <- function(x) {
  
  # !!!!!!! PROVISOIRE "DE-ACCENTUATE"
  x <- iconv(x, from="UTF-8", to='ASCII//TRANSLIT');
  return ( tm::stripWhitespace( tm::removeNumbers(x))  );
}

# re-def
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
    # default
    {
      return (dtMatrix);
    }
  );
}

# remove punct
rmv.punct <- function(x) {
  return (gsub("[^[:alnum:]['-]|'", " ", x));
}

# random split lrn/tst
random.split <- function (nbRows, lrnRate) {
  border <- as.integer(nbRows * lrnRate);
  set.seed(1000);
  indices <- sample.int(nbRows-1, border-1); # line 1 must always be in lrn 'ocz it contains all-1
  return(c(1, indices + 1));
}

# Save a matrix as 'jGibbsLabeledLda' file # 'labels' must be factor or you'll be sorry
writeLlda <- function (matrix, numericLabels, outPath) {
  cat(paste0("Generate labeledLDA input files to: '", outPath, "'"));
  if (nrow(matrix) != length(numericLabels) ) {
    message("Errot in fct. writeLlda : matrices do not have the same size !");
    return(NULL);
  }
  if (! is.character(numericLabels )) {
    message("Errot in fct. writeLlda : gold standard must be character vector !");
    return(NULL);
  }
  # Eliminate empty docs
  emptyIndices <- which(rowSums(matrix)==0);
  reducedLabels <- c();
  if (length(emptyIndices) > 0)
    message("Only ", (nrow(matrix)-length(emptyIndices)), " written, the rest are empty docs !", sep="")
  else
    message("No empty docs. All will be written.");

  # Map factor levels to numerics (startinf from 0 instead of 1)
  if (length(emptyIndices > 0)) {
    reducedMatrix <- matrix[-c(emptyIndices),];
    reducedLabels <- numericLabels[-c(emptyIndices)];
  }
  else {
    reducedMatrix <- matrix;
    reducedLabels <- numericLabels;
  }
  
  # write out
  zz <- file(outPath, "w", encoding = "UTF-8")  # open an output file connection
  for (i in 1:nrow(reducedMatrix)) {
    cat ( paste0( c(paste("[", reducedLabels[i], "]\t", sep=""), colnames(matrix)[which(reducedMatrix[i,]!=0)], "\n"), collapse=" "), file=zz);
  }
  close(zz)
  
  # Create a gZip file
  gzip (outPath, paste(outPath, ".gz", sep=""), temporary=FALSE, overwrite=TRUE, remove=FALSE);
  # Write numeric labels for evaluation
  write.table (reducedLabels, file=paste(outPath, ".labels", sep=""), append=FALSE, quote=FALSE, sep = "", eol = "\n", na = "NA", dec = ".", row.names=FALSE, col.names=paste(paste0("V", 1:ncol(goldStrd)), collapse=" "), fileEncoding="UTF-8");
  rm(reducedMatrix, reducedLabels);
}

# Save a matrix as 'svmlight' file (a.w.a. labels in a separate file)
writeSvmLight <- function (matrix, labels, outPath) {
  # Eliminate empty docs
  emptyIndices <- which(rowSums(matrix)==0);
  numericLabels <- c();
  
  if (length(emptyIndices) > 0) {
    message("Only ", (nrow(matrix)-length(emptyIndices)), " written, the rest are empty docs !", sep="")  
    
    # Map factor levels to numerics (startinf from 0)
    numericLabels <- labels[-c(emptyIndices)];
    
    lengths <- rowSums(matrix[-c(emptyIndices),]);
    write.matrix.csr(matrix[-c(emptyIndices),], file=outPath, y=lengths, fac=TRUE);
  }
  else {
    message("No empty docs. All will be written.");
    
    # Map factor levels to numerics (startinf from 0)
    numericLabels <- labels;
    
    lengths <- rowSums(matrix);
    write.matrix.csr(matrix, file=outPath, y=lengths, fac=TRUE);
  }
  # Write labels
  write.table (numericLabels, file=paste(outPath, ".labels", sep=""), append=FALSE, quote=FALSE, sep = "", eol = "\n", na = "NA", dec = ".", row.names=FALSE, col.names=FALSE, fileEncoding="UTF-8");
  message (paste("Please check 1st line of :", outPath, ". There may be some 0 at the end.", sep=""));
}

# Load data from a folder (all JSON files)
loadJsonFolder <- function (jsFolder, fields, docType, minContentLen) {
  csv <- data.frame(matrix(ncol=2));
  names(csv) <- c("NDA", "TEXT");
  for (fileName in list.files(jsFolder, pattern="\\.json$")) {
    d <- loadJsonFile( paste(jsFolder, fileName, sep=""), fields, docType, minContentLen);
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
loadJsonFile <- function (path, fields, docType, minContentLen) {
  jsonData <- jsonlite::fromJSON(path);
  nda <- eval ( parse(text=paste("jsonData$NDA$valeur", "\n", sep="")));
  # Keep only 9 first digits from NDA
  nda = substr(nda, 1, 9);
  
  # Provisoire §!!!! -- Pattern-based constraint on document type (CRO, CRH, Urologie, etc...)
  if (! is.null(docType))
    if ( ! str_detect( tolower(jsonData$TYPE$valeur), docType) )
      return (NULL);
  
  relevantFields <- fields;
  if (is.null(fields))
    relevantFields <- names(jsonData);
  dat <- "";
  for (varName in relevantFields) {
    #etiq <- eval ( parse(text=paste("jsonData$", varName, "$etiquette", "\n", sep="")));
    #dat <- paste (dat, etiq, sep="\n\n");
    val <- eval ( parse(text=paste("jsonData$", varName, "$valeur", "\n", sep="")));
    dat <- paste (dat, gsub("<.*?>", "", val), sep="; ");
    dat <- gsub("^\\s+|\\s+$", "", dat); # Suppr. les espaces au début et à la fin
    dat <- gsub("\n+|\r+", " ", dat); # Suppr. les sauts de lignes (entre autres pour un stockage CSV efficace)
  }
  if ( nchar(dat) >= minContentLen)
      return (c(nda, dat))
    else
      return (NULL)
}

# Build gold-standard matrix
buildGoldStrd <- function (csv, diags, allowedCodes) {
  goldStrd <- as.data.frame(matrix(FALSE, ncol=length(allowedCodes), nrow=length(csv$NDA)) );
  names(goldStrd) <- allowedCodes;
  rownames(goldStrd) <- csv[,"NDA"];
  for (nda in csv$NDA) {
    codes <- as.vector( (unique(as.matrix(diags[diags$NDA==nda, , ]["DIAG"]))) );
    goldStrd[nda, codes] <- TRUE;
  }
  return (goldStrd);
}

# Create a copy of all files of the corpus restricted on present NDA
copy.corpus <- function (ndas, srcFolder, destFolder) {
  for (nda in ndas) {
    file.copy(paste(srcFolder, nda, ".json", sep=""), paste(destFolder, nda, ".json", sep=""), overwrite=TRUE);
  }
}

