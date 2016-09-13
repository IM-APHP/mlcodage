
# merge the predictions (logical matrices) from two different models
merge.predictions<- function (predMat1, predMat2) {
  if (nrow(predMat1) != nrow(predMat2) || ncol(predMat1) != ncol(predMat2) ) {
    message("Error in fct 'merge.predictions': matrices do not have the same size !");
    return (NULL);
  }
  #
  res <- predMat1;
  for (i in 1:nrow(predMat1)) {
    res[i, ] <- as.numeric( predMat1[i, ] | predMat2[i, ] );
  }
  return (res);
}

# compute element-wise mean of a list of matrices
mean.sd <- function (corpusName, nbRolls, nbFolds, granularity) {
  
  scoreListList <- list();
  for (i in 1:nbFolds) {
    load(paste("C:\\Users\\mdermouche\\Desktop\\R\\", i, corpusName, "-scores.RData", sep=""));
    scoreListList[[paste("scores", i, sep="")]] <- scores;
  }
  algoLabels <- names(scoreListList[[ names(scoreListList)[1] ]]);
  
  meanMat <- NULL;
  sdsMat <- NULL;
  for (scorRoll in names(scoreListList)) {
    message(scorRoll);
    mat <- make.plottable (scoreListList[[scorRoll]], nbRolls, granularity);
    
    if (is.null(meanMat)) {
      meanMat <- mat;
      sdsMat <- apply(mat, c(1,2), as.character);
      row.names(meanMat) <- algoLabels;
      row.names(sdsMat) <- algoLabels;
    } else {
      meanMat <- meanMat + mat;
      # paste sd
      for (rw in 1:length(algoLabels))
        for (cl in 1:nbRolls)
          sdsMat[rw, cl] <- paste(sdsMat[rw, cl], ",", as.character(mat[rw, cl]) );
    }
  }
  meanMat <- meanMat / length( names(scoreListList) );
  # eval sd
  for (rw in 1:length(algoLabels))
    for (cl in 1:nbRolls)
      sdsMat[rw, cl] <- eval(parse(text=paste("sd(c(", as.character(sdsMat[rw, cl]), "));", sep="") ) );
  sdsMat <- apply(sdsMat, c(1,2), as.numeric);
  
  return (list(means=meanMat, sds=sdsMat));
}

# Stat significance from scores (list of lists) [use the 1st lines of mean.sd to get it]
stat.signif <- function (scores, granularity) {
  execs <- attributes(scores)[["names"]];
  nbRolls <- ncol(scores[[1]] [[1]]);
  fullMat <- matrix(0, nrow=0, ncol=nbRolls);
  diffMat <- matrix(0, nrow=0, ncol=(nbRolls-1));
  pvalues <- c(NA);
  
  # make a big matrix
  for (exec in 1:length(execs)) {
    mat <- make.plottable(scores[[exec]], nbRolls, granularity);
    fullMat <- rbind(fullMat, mat);
  }
  # compute differences
  for (i in 1:(nbRolls-1)) {
    fullMat[, i] = fullMat[, i+1] - fullMat[, i];
    if (i > 1 & i < nbRolls)
      pvalues <- c(pvalues, t.test(fullMat[, i-1], fullMat[, i] , paired=TRUE) [["p.value"]] );
  }
  fullMat <- fullMat[, -c(nbRolls)];
  fullMat <- rbind(fullMat, means=colMeans(fullMat) );
  return (rbind(fullMat, pvalue=round(pvalues, 8) ) );
}

# Make plottable object from evaluation scores
make.plottable <- function (scoreList, nbRolls, granularity) {
  mat <- matrix(0, 0, nbRolls);
  
  for (modelNam in names(scoreList)) {
    if( ncol(mat) != length(scoreList[[modelNam]] [granularity,]))
      message("error!");
    mat <- rbind(mat, scoreList[[modelNam]] [granularity,]);
  }
  rownames(mat) <- names(scoreList);
  return(mat);
}

# Un-fuzzify predictions (when more than 1 label is returned)--make returned code equal to true one (if any)
unfuzzify <- function (fuzzyPred, truthMatrix, bestClasses) {
  # check
  if ( ncol(fuzzyPred)!=ncol(truthMatrix) || nrow(fuzzyPred)!=nrow(truthMatrix)) {
    message("Error in fct 'unfuzzify': matrices do not have the same size !");
    return (NULL);
  }
    
  hardPred <- matrix(0, nrow(fuzzyPred), ncol(fuzzyPred));
  rownames(hardPred) <- rownames(fuzzyPred);
  colnames(hardPred) <- colnames(fuzzyPred);
  for (i in 1:nrow(fuzzyPred)) {
    cod <- which(truthMatrix[i,]==1);
    if (fuzzyPred[i,cod] == 1)
      hardPred[i, cod] <- 1
    else
      hardPred[i, bestClasses[i] ] <- 1; # Best (most probable) class returned by the classifier
  }
  return(hardPred);
}

# Draw barplots : F-score per code / roll. takes a matrix of scores in input : returns a df (code,size,scores)
draw.scores.per.class <- function (dataMtrx, scoreMtrx, truthVect, yTitle, rollTitles, colors, outFile) {
  pdf(outFile, width=70, height=23);
  numbRolls <- ncol(scoreMtrx);
  
  par(mar=c(1.47, 4.1, 3.8, 4.1), mfrow=c(numbRolls + 2, 1));
  
  # plot Scores
  for (nroll in 1:numbRolls) {
    scores = sort(table(truthVect), decreasing=T); # initiliaze so to have the same level-order with truthV
    for (cod in names(scores)) 
      scores[cod] <- scoreMtrx[cod, nroll];
    barplot(scores, col=colors[nroll], ylab=yTitle, main=rollTitles[nroll], cex.main=.9, cex.names=.8, las=2, ylim=c(0,1), space=.6, col.main=colors[nroll]);
    grid(NA, 5);
  }
  
  # plot class size
  barplot(sort(table(truthVect),decreasing=T), ylab="#documents", xlab="Codes", main="Code frequency (lrn data)", cex.main=.9, cex.names=.8, las=2, ylim = rev(c(0,max(table(truthVect)))), space=.6);
  
  # plot document size/class
  docSize <- sort(table(truthVect), decreasing=T);
  for (cod in names(docSize)) {
    inds <- which(truthVect==cod);
    if (length(inds) > 1)
      docSize[cod] <- mean(rowSums( apply(as.matrix(dataMtrx)[inds,], 2, as.numeric) ))
    else if (length(inds) == 1)
      docSize[cod] <- sum(as.numeric(dataMtrx[inds,]))
    else
      docSize[cod] <- length(inds);
  }
  barplot(docSize, ylab="Avg. doc. size", xlab="Codes", main="Average document size per code (lrn data)", cex.main=.9, cex.names=.8, las=2, ylim=rev(c(0,max(docSize))), names.arg=NA, space=.6);
  dev.off();
  par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1));
  message("Plot succes: ", outFile);
  # return a dataframe with (code,size,fscores)
  res <- data.frame(cbind(rownames(scoreMtrx), table(truthV), scoreMtrx), row.names=NULL);
  colnames(res) = c("Code", "CodeSize", paste("F", 1:numbRolls, "-score", sep=""));
  return (res);
}

# Draw barplots for N rolls
plot.scores.nrolls2 <- function (mod1, errBars, mainTitle, yLab, colors, makeLegend, makeXlab) {
  mod1 <- t(mod1);
  if (! is.null(errBars))
    errBars <- t(errBars);
  numbRolls <- nrow(mod1);
  rownames(mod1) <- paste0("n=", 1:numbRolls);
  
  yMax <- max(data.frame(mod1));
  yMax <- 1.1;
  
  dat <- as.data.frame(t(mod1));
  xlabels = rep("", ncol(dat));
  if (makeXlab)
    xlabels <- names(dat);
  
  ang <- c(0, 30, -00, -00);#, 30);
  dens <- c(0, 10, 25, 12);#, 25);
  graph <- barplot (as.matrix(dat), cex.axis=.7, cex.names=.7, main=mainTitle, xpd=TRUE, space=c(0.4,1.6), ylim=c(0,yMax), ylab=yLab, names.arg=xlabels, xlab="", cex.lab=0.7, cex.main=1, beside=TRUE, col=1, angle=ang, density=dens);
  if (! is.null(errBars))
    arrows(graph, t(mod1-errBars), graph, t(mod1+errBars), code=3, angle=90, length=0.025);
  if (makeLegend)
    legend("top", colnames(mod1), inset=c(0.0,-0.05), cex=.8, bty="n", horiz=TRUE, angle=ang, density=dens );
  
  #abline(h=c(0.2,0.4,0.6,0.8,1), col="lightgray", lty=2)
  # re-plot pour écraser le grid
  #graph <- barplot (as.matrix(dat), add=T, cex.axis=.7, cex.names=.9, main=mainTitle, xpd=TRUE, width=0.5, space=c(0,1), ylim=c(0,1.0), ylab=yLab, names.arg=xlabels, xlab="", cex.lab=0.7, cex.main=1, beside=TRUE, col=1, angle=ang, density=dens);
  
  #grid(NA, 5); # draw a dahsed line every 10
}

# Draw barplots for N rolls
plot.scores.nrolls <- function (mod1, errBars, mainTitle, yLab, colors, makeLegend, makeXlab) {
  numbRolls <- ncol(mod1);
  
  yMax <- max(data.frame(mod1));
  yMax <- 1;
  
  dat <- as.data.frame(t(mod1));
  xlabels = rep("", ncol(dat));
  if (makeXlab)
    xlabels <- names(dat);
  
  graph <- barplot (as.matrix(dat), main=mainTitle, xpd=TRUE, width=0.5, space=c(0,1), ylim=c(0,1.1), ylab=yLab, names.arg=xlabels, xlab="", cex.lab=0.75, cex.main=1, beside=TRUE, col=colors[1:numbRolls]);
  if (! is.null(errBars))
    arrows(graph, t(mod1-errBars), graph, t(mod1+errBars), code=3, angle=90, length=0.025);
  if (makeLegend)
    legend("top", c(as.character(paste("k=",1:numbRolls,sep=""))), inset=c(0,-0.3), cex=.8, bty="n", horiz=TRUE, fill=colors[1:numbRolls] );
  grid(NA, 11); # draw a dahsed line very 10
}

# Print results on the screen, "measure" in {"F", "R", "P"}
aggreg.result <- function (x, measure) {
  if (! measure %in% c("R", "P", "F")) {
    message("Error : unkown measure type in function aggreg.result !");
    return (NULL);
  }
  mic = list();
  mic$F <- sum(x$per.class[,"F"] * x$per.class[,"Pos."])/sum(x$per.class[,"Pos."]);
  mic$P <- sum(x$per.class[,"P"] * x$per.class[,"Pos."])/sum(x$per.class[,"Pos."]);
  mic$R <- sum(x$per.class[,"R"] * x$per.class[,"Pos."])/sum(x$per.class[,"Pos."]);
  
  return (c(mic[[measure]], as.numeric(x$average[measure])));
}

# Compute performance indicators from classifier result
evaluate <- function (trutMatrix, predMatrix) {
  return (F.measure.single.over.classes(trutMatrix, predMatrix));
}

# Evaluate topic model output
evaluate.tm <- function (truthPath, predPath, orderedClassNames, nroll, multilab) {
  t <- read.csv (truthPath, header=T, sep=" ", stringsAsFactors=FALSE, as.is=TRUE, encoding="UTF-8");
  p <- read.csv (predPath, header=FALSE, sep=" ", stringsAsFactors=FALSE, encoding="UTF-8");
  
  # check
  if (nrow(t)!=nrow(p) || length(orderedClassNames)!=ncol(p)) {
    message("Error in fct 'evaluate.tm': matrices do not have the same size !");
    return (NULL);
  }
  
  truth <- matrix(0, nrow=nrow(p), ncol=ncol(p));
  pred <- matrix(0, nrow=nrow(p), ncol=ncol(p));
  bestClassesV <- rep("", nrow(p));
  
  for (i in 1:nrow(p)) {
    for (r in which( ! is.na(t[i,])))
      truth[i,(t[i,r]+1)] <- 1;
    for (r in 1:nroll)
      pred[i,(p[i,r])+1] <- 1;
    # update bestClass vector
    bestClassesV[i] <- orderedClassNames[ p[i,1] + 1 ];
  }
  colnames(truth) <- orderedClassNames;
  colnames(pred) <- orderedClassNames;
  
  # Un-fuzzify pred matrix ( ! for single label only ) !!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (! multilab)
    pred <- unfuzzify(pred, truth, bestClassesV);
  
  # The following is to create heatmaps (but never returned) ##########
  tv = rep("", nrow(p));
  tp = rep("", nrow(p));
  for (i in 1:nrow(p)) {
    tv[i] = orderedClassNames[ which(truth[i, ] == 1) ];
    tp[i] = orderedClassNames[ which(pred[i, ] == 1) ];
  }
  qq= table(tv, tp);
  ss = matrix("", ncol=5, nrow=ncol(p));
  row.names(ss) <- orderedClassNames;
  for (i in row.names(qq) ) {
    ss[i,] <- orderedClassNames[ order(qq[i,], decreasing=T)[1:5] ];
  }
  # end heatmap creation
  
  x <- evaluate(truth, pred);
  return(x);
}