
# Function : Load diagnostics
loadDiags <- function(diagsPath, srvceLabel, diagType, allowedCodes) {
  load(diagsPath);
  diags <- data.frame(NDA=Diagnostics$NDA, SERVICE=Diagnostics$SERVICE, TYPE=Diagnostics$TYPE, DIAG=Diagnostics$DIAG);
  rm(Diagnostics);
  
  # Ignore codes not from service "serviceLabel"
  if ( !is.null(srvceLabel) )
    diags <- subset(diags, str_detect(tolower(diags$SERVICE), tolower(srvceLabel)));
  
  # diagnostic : in {P, R, A}
  if ( !is.null(diagType) )
    diags <- subset(diags, str_detect(tolower(diags$TYPE), tolower(diagType)));
  
  # Only allowedCodes
  if ( !is.null(allowedCodes) )
    diags <- subset(diags, diags$DIAG %in% allowedCodes);
  
  return ( diags );
}

loadActes <- function(diagsPath, uhExec, allowedCodes) {
  load(diagsPath);
  diags <- data.frame(NDA=Diagnostics$NDA, DIAG=Diagnostics$ACTE, UH=Diagnostics$UH_EXEC);
  rm(Diagnostics);
  
  # restrict on UH_EXEC
  if (! is.null(uhExec))
    diags <- subset(diags, diags$UH %in% uhExec);
  
  # Only allowedCodes
  if ( !is.null(allowedCodes) )
    diags <- subset(diags, diags$DIAG %in% allowedCodes);
  
  diags <- diags[,c("NDA","DIAG")];
  
  diags$NDA <- as.numeric(diags$NDA);
  diags$DIAG <- as.character(diags$DIAG);
  
  return ( diags );
}