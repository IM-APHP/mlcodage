#' loadPkgs function
#' 
#' Install and/or load all the required packages for mlcodage
#' 
loadPkgs <- function() {
  # install required packages (if not installed)
  requiredPkg <- c("tm","opencpu","e1071", "ROCR", "rpart", "tree", "maxent", "gplots", "PerfMeas", "jsonlite", "gdata", "mldr", "gmum.r", "matrixStats", "SnowballC", "R.utils", "arules", "FSelector", "ggplot2", "RWeka", "stringr", "RPostgreSQL");
  #requiredPkg <- c();# c("tm","opencpu","e1071", "jsonlite", "R.utils");
  
  #   pkgToInstall <- setdiff(requiredPkg, rownames(installed.packages()));
  #   for (package in pkgToInstall) {
  #     if (!require(package, character.only=TRUE)) {
  #       message ("Installing package ", package);
  #       install.packages(package);
  #       library(package, character.only=T);
  #     }
  #   }
  
  message ("Loading required libraries..");
  for (p in requiredPkg)
    suppressWarnings(suppressMessages(library(p, character.only=TRUE, quietly=TRUE)));
}

# Function : Build Association-rule prediction model based on APriori
buildRules <- function (dataMatrix, minSupp, minConf, gdStrd) {
  gdStrd <- apply(gdStrd, 2, as.numeric); # Convert logical to numeric
  mat <- t(t(cbind (dataMatrix, gdStrd)));
  trans <- as(mat, "transactions");
  rules <- apriori(trans, appearance = list(default="lhs",rhs=allLabels), parameter=list(supp=minSupp, conf=minConf, target="rules", minlen=2, maxlen=5));
  return (rules);
}

# Function : Build model
buildModel <- function (modelType, dataMatrix, labels, buildParams) {
  model = NULL;
  switch(tolower(modelType),
         "svm"={
           model <- e1071::svm(x=dataMatrix, y=labels, kernel="linear", probability=TRUE, type="C-classification", scale=TRUE, tolerance=0.1, fitted=FALSE);
           #model <- gmum.r::SVM(as.matrix(dataMatrix), as.vector(labels), class.type="one.versus.one", core="libsvm", kernel=params$kernel, C=1);
         },
         "nb"={
           model <- naiveBayes(x = dataMatrix, y=labels, buildParams, laplace=0.01);
         },
         "me"={
           model <- maxent(dataMatrix, labels);
         },
         "dt"={
           model <- rpart(labels ~ ., data=as.data.frame(dataMatrix), method="class", parms=list(split="information"), control=rpart.control(minsplit=4, cp=0.01)  );
         },
         # default
{
  message ("Model type not recognized !");
}
  );
return (model);
}

# Deploy a ML model (#if thresH < 1 return all classes with thres>thresH, else return exactly thresH classes)
deployModel <- function (modelType, model, dataMatrix, thresH, allPresLabels) {
  res <- NULL;
  switch(tolower(modelType),
    "svm"= {
     res <- deploySvm (model, dataMatrix, thresH, allPresLabels);
    },
    "nb"= {
     res <- deployNb (model, dataMatrix, thresH);
    },
    "me"= {
     res <- deployME (model, dataMatrix, thresH);
    },
    "dt"= {
     res <- deployDT (model, as.data.frame(dataMatrix), thresH);
    },
    # default
    {
      message ("Model type not recognized !");
    }
  );
  return (res);
}

# Function : Deploy SVM model
deploySvm <- function (model, dataMatrix, thresH, allPresLabels) {
  result <- predict(model, dataMatrix, probability=TRUE);
  res <- attr(result, "probabilities");
  # Cas 1 : restituer plusieurs labels qui > seuil thresH
  if (thresH < 1) {
    res[res < thresH] <- 0;
    res[res >= thresH] <- 1;
  }
  # Cas 2 : restituer un nombre précis de labels égal à thresH (1, 2, 3, .....)
  else {
    # Find the "thresH" probable labels
    indices <- integer(thresH);
    for (i in 1:nrow(res)) {
      indices <- order(res[i,], decreasing=TRUE)[1:thresH];
      res[i, ] = rep(0, ncol(res));
      res[i, as.matrix(indices) ] <- 1;
    }
  }
  
  # completude matrix (labels never predicted) : a MUST for SVM (may be because codes are not factors ?)
  newNames <- setdiff(allPresLabels, unlist(attr(attr(result,"probabilities"), "dimnames")[2]));
  complMatrix <- matrix(0, ncol=length(newNames), nrow=nrow(res));
  rownames(complMatrix) <- rownames(res);
  colnames(complMatrix) <- newNames;
  res<- cbind(res,complMatrix);
  return (res);
}

# Function : Deploy NB model
deployNb <- function (model, dataMatrix, thresH) {
  res <- predict(model, dataMatrix, type="raw", laplace=0.01, eps=0.01 );
  # Cas 1 : restituer plusieurs labels qui > seuil thresH
  if (thresH < 1) {
    res[res < thresH] <- 0;
    res[res >= thresH] <- 1;
  }
  # Cas 2 : restituer un nombre précis de labels égal à thresH (1, 2, 3, .....)
  else {
    # Find the "thresH" probable labels
    indices <- integer(thresH);
    for (i in 1:nrow(res)) {
      indices <- order(res[i,], decreasing=TRUE)[1:thresH];
      res[i, ] = rep(0, ncol(res));
      res[i, as.matrix(indices) ] <- 1;
    }
  }
  return (res);
}

# Function : Deploy MaxEnt model
deployME <- function (model, dataMatrix, thresH) {
  res <- predict(model, dataMatrix);
  res <- res[,-c(1)];
  # Cas 1 : restituer plusieurs labels qui > seuil thresH
  if (thresH < 1) {
    res[res < thresH] <- 0;
    res[res >= thresH] <- 1;
  }
  # Cas 2 : restituer un nombre précis de labels égal à thresH (1, 2, 3, .....)
  else {
    # Find the "thresH" probable labels
    indices <- integer(thresH);
    for (i in 1:nrow(res)) {
      indices <- order(res[i,], decreasing=TRUE)[1:thresH];
      res[i, ] = rep(0, ncol(res));
      res[i, as.matrix(indices) ] <- 1;
    }
  }
  return (res);
}

# Function : Deploy DT model
deployDT <- function (model, dataMatrix, thresH) {
  res <- predict(model, dataMatrix);
  # Cas 1 : restituer plusieurs labels qui > seuil thresH
  if (thresH < 1) {
    res[res < thresH] <- 0;
    res[res >= thresH] <- 1;
  }
  # Cas 2 : restituer un nombre précis de labels égal à thresH (1, 2, 3, .....)
  else {
    # Find the "thresH" probable labels
    indices <- integer(thresH);
    for (i in 1:nrow(res)) {
      indices <- order(res[i,], decreasing=TRUE)[1:thresH];
      res[i, ] = rep(0, ncol(res));
      res[i, as.matrix(indices) ] <- 1;
    }
  }
  return (res);
}
