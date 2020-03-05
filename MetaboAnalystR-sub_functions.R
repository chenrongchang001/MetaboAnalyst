#'Clean the data matrix
#'@description Function used in higher functinos to clean data matrix
#'@param ndata Input the data to be cleaned
#'@export
#'
CleanDataMatrix <- function(ndata){
  # make sure no costant columns crop up
  varCol <- apply(data.frame(ndata), 2, var, na.rm=T); # getting an error of dim(X) must have a positive length, fixed by data.frame 
  constCol <- (varCol == 0 | is.na(varCol));
  return(ndata[,!constCol, drop=FALSE]); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
}

### Perform miscellaneous tasks
### Perform misc tasks
### Jeff Xia\email{jeff.xia@mcgill.ca}
### McGill University, Canada
###License: GNU GPL (>= 2)


#'Given a data with duplicates, remove duplicates
#'@description Dups is the one with duplicates
#'@param data Input data to remove duplicates
#'@param lvlOpt Set options, default is mean
#'@param quiet Set to quiet, logical, default is T
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
RemoveDuplicates <- function(data, lvlOpt="mean", quiet=T){
  
  all.nms <- rownames(data);
  colnms <- colnames(data);
  dup.inx <- duplicated(all.nms);
  dim.orig  <- dim(data);
  data <- apply(data, 2, as.numeric); # force to be all numeric
  dim(data) <- dim.orig; # keep dimension (will lost when only one item) 
  rownames(data) <- all.nms;
  colnames(data) <- colnms;
  if(sum(dup.inx) > 0){
    uniq.nms <- all.nms[!dup.inx];
    uniq.data <- data[!dup.inx,,drop=F];
    
    dup.nms <- all.nms[dup.inx];
    uniq.dupnms <- unique(dup.nms);
    uniq.duplen <- length(uniq.dupnms);
    
    for(i in 1:uniq.duplen){
      nm <- uniq.dupnms[i];
      hit.inx.all <- which(all.nms == nm);
      hit.inx.uniq <- which(uniq.nms == nm);
      
      # average the whole sub matrix 
      if(lvlOpt == "mean"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, mean, na.rm=T);
      }else if(lvlOpt == "median"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, median, na.rm=T);
      }else if(lvlOpt == "max"){
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, max, na.rm=T);
      }else{ # sum
        uniq.data[hit.inx.uniq, ]<- apply(data[hit.inx.all,,drop=F], 2, sum, na.rm=T);
      }
    }
    AddMsg(paste("A total of ", sum(dup.inx), " of duplicates were replaced by their ", lvlOpt, ".", sep=""));
    return(uniq.data);
  }else{
    AddMsg("All IDs are unique.");
    return(data);
  }
} 

# setup microservice for R function execution
SetupRSclient <- function(user.dir=FALSE, remote=TRUE){
  #library(RSclient);
  load_RSclient();
  rsc <- try(RS.connect(host = "132.216.38.6", port = 6313));
  if(class(rsc) == "try-error") {
    rsc <- RS.connect(); # switch to local
    remote <- FALSE
  }
  
  if(user.dir){
    if(remote){
      dir.name <- strsplit(getwd(), "/")[[1]]; dir.name <- dir.name [length(dir.name)]
      RS.assign(rsc, "my.dir", paste0("/data/Rtmp/", dir.name));
      RS.eval(rsc, dir.create(my.dir));
    } else{
      RS.assign(rsc, "my.dir", getwd()); RS.eval(rsc, setwd(my.dir));
    }
  }
  return(rsc);
}

#'Read data table
#'@description Function to read in a data table. First, it will try to use fread, however, it has issues with 
#'some windows 10 files. In such case, use the slower read.table method.
#'@param fileName Input filename
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

.readDataTable <- function(fileName){
  
  dat <- tryCatch(
    data.table::fread(fileName, header=TRUE, check.names=FALSE, blank.lines.skip=TRUE, data.table=FALSE),
    error=function(e){
      print(e);
      return(.my.slowreaders(fileName));    
    }, 
    warning=function(w){
      print(w);
      return(.my.slowreaders(fileName));
    });
  
  if(any(dim(dat) == 0)){
    dat <- .my.slowreaders(fileName);
  }
  return(dat);
}

.my.slowreaders <- function(fileName){
  print("Using slower file reader ...");
  formatStr <- substr(fileName, nchar(fileName)-2, nchar(fileName))
  if(formatStr == "txt"){
    dat <- try(read.table(fileName, header=TRUE, comment.char = "", check.names=F, as.is=T));
  }else{ # note, read.csv is more than read.table with sep=","
    dat <- try(read.csv(fileName, header=TRUE, comment.char = "", check.names=F, as.is=T));
  }  
  return(dat);
}

#'Permutation
#'@description Perform permutation, options to change number of cores used
#'@param perm.num Numeric, input the number of permutations to perform
#'@param fun Dummy function
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage Perform.permutation(perm.num, fun)
#'@export
#'
Perform.permutation <- function(perm.num, fun){
  
  # for public server, perm.num is not always followed to make sure loop will not continue for very long time
  # before the adventure, see how long it takes for 10 permutations
  # if it is extremely slow (>60 sec) => max 20 (<0.05)
  # if it is very slow (30-60 sec) => max 100 (<0.01)
  
  start.num <- 1; 
  perm.res <- NULL;
  if(.on.public.web & perm.num > 20){
    start.time <- Sys.time();
    perm.res <- lapply(1:10, fun);
    end.time <- Sys.time();
    
    time.taken <- end.time - start.time;
    print(paste("time taken for 10 permutations: ", time.taken));
    
    if(time.taken > 60){
      perm.num <- 20;
    }else if(time.taken > 30){
      perm.num <- 100;
    }
    start.num <- 11;
  }
  print(paste("performing", perm.num, "permutations ..."));
  perm.res <- c(perm.res, lapply(start.num:perm.num, fun));
  return(list(perm.res=perm.res, perm.num = perm.num));
}

#'Unzip .zip files
#'@description Unzips uploaded .zip files, removes the uploaded file, checks for success
#'@param inPath Input the path of the zipped files
#'@param outPath Input the path to directory where the unzipped files will be deposited
#'@param rmFile Logical, input whether or not to remove files. Default set to T
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
UnzipUploadedFile<-function(inPath, outPath, rmFile=T){
  
  a <- tryCatch(
    system(paste("unzip",  "-o", inPath, "-d", outPath), intern=T),
    error=function(e){
      print(e);
      return(unzip(inPath, outPath));    
    }, 
    warning=function(w){
      print(w);
      return(unzip(inPath, outPath));
    });
  
  if(class(a) == "try-error" | !length(a)>0){
    AddErrMsg("Failed to unzip the uploaded files!");
    AddErrMsg("Possible reason: file name contains space or special characters.");
    AddErrMsg("Use only alphabets and numbers, make sure there is no space in your file name.");
    AddErrMsg("For WinZip 12.x, use \"Legacy compression (Zip 2.0 compatible)\"");
    return (0);
  }
  #if(rmFile){
  #  RemoveFile(inPath);
  #}
  return (1);
}

#'Perform data cleaning
#'@description Cleans data and removes -Inf, Inf, NA, negative and 0s.
#'@param bdata Input data to clean
#'@param removeNA Logical, T to remove NAs, F to not. 
#'@param removeNeg Logical, T to remove negative numbers, F to not. 
#'@param removeConst Logical, T to remove samples/features with 0s, F to not. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'

CleanData <-function(bdata, removeNA=T, removeNeg=T, removeConst=T){
  
  if(sum(bdata==Inf, na.rm=TRUE)>0){
    inx <- bdata == Inf;
    bdata[inx] <- NA;
    bdata[inx] <- max(bdata, na.rm=T)*2
  }
  if(sum(bdata==-Inf, na.rm=TRUE)>0){
    inx <- bdata == -Inf;
    bdata[inx] <- NA;
    bdata[inx] <- min(bdata, na.rm=T)/2
  }
  if(removeNA){
    if(sum(is.na(bdata))>0){
      bdata[is.na(bdata)] <- min(bdata, na.rm=T)/2
    }
  }
  if(removeNeg){
    if(sum(as.numeric(bdata<=0)) > 0){
      inx <- bdata <= 0;
      bdata[inx] <- NA;
      bdata[inx] <- min(bdata, na.rm=T)/2
    }
  }
  if(removeConst){
    varCol <- apply(data.frame(bdata), 2, var, na.rm=T); # getting an error of dim(X) must have a positive length, fixed by data.frame
    constCol <- (varCol == 0 | is.na(varCol));
    constNum <- sum(constCol, na.rm=T);
    if(constNum > 0){
      bdata <- data.frame(bdata[,!constCol, drop=FALSE]); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
    }
  }
  bdata;
}

#'Replace infinite numbers
#'@description Replace -Inf, Inf to 99999 and -99999
#'@param bdata Input matrix to clean numbers
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
CleanNumber <-function(bdata){
  if(sum(bdata==Inf)>0){
    inx <- bdata == Inf;
    bdata[inx] <- NA;
    bdata[inx] <- 999999;
  }
  if(sum(bdata==-Inf)>0){
    inx <- bdata == -Inf;
    bdata[inx] <- NA;
    bdata[inx] <- -999999;
  }
  bdata;
}

# only keep alphabets, numbers, ",", "." "_", "-" "/"
# note, this may leads to duplicate names
CleanNames <- function(query, type){
  
  if(type=="sample_name"){
    query <- gsub("[^[:alnum:]./_-]", "", query);
  }else{
    query <- gsub("[^[:alnum:][:space:],'./_-]", "", query)
  }
  return(make.unique(query));
}

#'Remove spaces
#'@description Remove from, within, leading and trailing spaces
#'@param query Input the query to clear
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

ClearStrings<-function(query){
  # kill multiple white space
  query <- gsub(" +"," ",query);
  # remove leading and trailing space
  query<- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", query, perl=TRUE);
  return(query);
}

# Remove HTML tag
PrepareLatex <- function(stringVec){
  stringVec <- gsub("<(.|\n)*?>","",stringVec);
  stringVec <- gsub("%", "\\\\%", stringVec);
  stringVec;
}

#'Determine value label for plotting
#'@description Concentration or intensity data type
#'@param data.type Input concentration or intensity data
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetAbundanceLabel<-function(data.type){
  if(data.type=="conc"){
    return("Concentration");
  }else {
    return("Intensity");
  }
}

#'Determine variable label for plotting
#'@description Determine data type, binned spectra, nmr peak, or ms peak
#'@param data.type Input the data type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetVariableLabel<-function(data.type){
  if(data.type=="conc"){
    return("Compounds");
  }else if(data.type=="specbin"){
    return("Spectra Bins");
  }else if(data.type=="nmrpeak"){
    return("Peaks (ppm)");
  }else if(data.type=="mspeak"){
    return("Peaks (mass)");
  }else{
    return("Peaks(mz/rt)");
  }
}

#'Create Latex table
#'@description generate Latex table
#'@param mat Input matrix
#'@param method Input method to create table
#'@param data.type Input the data type
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

GetSigTable<-function(mat, method, data.type){
  if(!isEmptyMatrix(mat)){ # test if empty
    cap<-"Important features identified by";
    if(nrow(mat)>50){
      smat<-as.matrix(mat[1:50,]); # only print top 50 if too many
      colnames(smat)<-colnames(mat); # make sure column names are also copied
      mat<-smat;
      cap<-"Top 50 features identified by";
    }
    # change the rowname to first column
    col1<-rownames(mat);
    cname<-colnames(mat);
    cname<-c(GetVariableLabel(data.type), cname);
    mat<-cbind(col1, mat);
    rownames(mat)<-NULL;
    colnames(mat)<-cname;
    print(xtable::xtable(mat, caption=paste(cap, method)), caption.placement="top", size="\\scriptsize");
  }else{
    print(paste("No significant features were found using the given threshold for", method));
  }
}

#'Sig table matrix is empty
#'@description Test if a sig table matrix is empty
#'@param mat Matrix to test if empty
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

isEmptyMatrix <- function(mat){
  if(is.null(mat) | length(mat)==0){
    return(TRUE);
  }
  if(nrow(mat)==0 | ncol(mat)==0){
    return(TRUE);
  }
  if(is.na(mat[1,1])){
    return(TRUE);
  }
  return(FALSE);
}

# List of objects
# Improved list of objects
# Jeff Xia\email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)

.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  mSetObj <- .get.mSet(mSetObj);
  print(lapply(mSetObj$dataSet, object.size));
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

#'Extend axis
#'@description Extends the axis range to both ends
#'vec is the values for that axis
#'unit is the width to extend, 10 will increase by 1/10 of the range
#'@param vec Input the vector
#'@param unit Numeric
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetExtendRange<-function(vec, unit=10){
  var.max <- max(vec, na.rm=T);
  var.min <- min(vec, na.rm=T);
  exts <- (var.max - var.min)/unit;
  c(var.min-exts, var.max+exts);
}

getVennCounts <- function(x, include="both") {
  x <- as.matrix(x)
  include <- match.arg(include,c("both","up","down"))
  x <- sign(switch(include,
                   both = abs(x),
                   up = x > 0,
                   down = x < 0
  ))
  nprobes <- nrow(x)
  ncontrasts <- ncol(x)
  names <- colnames(x)
  if(is.null(names)) names <- paste("Group",1:ncontrasts)
  noutcomes <- 2^ncontrasts
  outcomes <- matrix(0,noutcomes,ncontrasts)
  colnames(outcomes) <- names
  for (j in 1:ncontrasts)
    outcomes[,j] <- rep(0:1,times=2^(j-1),each=2^(ncontrasts-j))
  xlist <- list()
  for (i in 1:ncontrasts) xlist[[i]] <- factor(x[,ncontrasts-i+1],levels=c(0,1))
  counts <- as.vector(table(xlist))
  structure(cbind(outcomes,Counts=counts),class="VennCounts")
}

# Perform utilities for MetPa
# borrowed from Hmisc
# Jeff Xia\email{jeff.xia@mcgill.ca}
# McGill University, Canada
# License: GNU GPL (>= 2)
all.numeric <- function (x, what = c("test", "vector"), extras = c(".", "NA")){
  what <- match.arg(what)
  old <- options(warn = -1)
  on.exit(options(old));
  x <- sub("[[:space:]]+$", "", x);
  x <- sub("^[[:space:]]+", "", x);
  inx <- x %in% c("", extras);
  xs <- x[!inx];
  isnum <- !any(is.na(as.numeric(xs)))
  if (what == "test") 
    isnum
  else if (isnum) 
    as.numeric(x)
  else x
}

ClearNumerics <-function(dat.mat){
  dat.mat[is.na(dat.mat)] <- -777;
  dat.mat[dat.mat == Inf] <- -999;
  dat.mat[dat.mat == -Inf] <- -111;
  dat.mat;
}

#'Calculate Pairwise Differences
#'@description Mat are log normalized, diff will be ratio. Used in higher functions. 
#'@param mat Input matrix of data to calculate pair-wise differences.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
CalculatePairwiseDiff <- function(mat){
  f <- function(i, mat) {
    z <- mat[, i-1] - mat[, i:ncol(mat), drop = FALSE]
    colnames(z) <- paste(colnames(mat)[i-1], colnames(z), sep = "/")
    z
  }
  res <- do.call("cbind", sapply(2:ncol(mat), f, mat));
  round(res,5);
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Update graph settings
#'@description Function to update the graph settings.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
# col.vec should already been created
UpdateGraphSettings <- function(mSetObj=NA, colVec, shapeVec){
  mSetObj <- .get.mSet(mSetObj);
  grpnms <- GetGroupNames(mSetObj);
  names(colVec) <- grpnms;
  names(shapeVec) <- grpnms;
  colVec <<- colVec;
  shapeVec <<- shapeVec;
}

GetShapeSchema <- function(mSetObj=NA, show.name, grey.scale){
  mSetObj <- .get.mSet(mSetObj);
  if(exists("shapeVec") && all(shapeVec >= 0)){
    sps <- rep(0, length=length(mSetObj$dataSet$cls));
    clsVec <- as.character(mSetObj$dataSet$cls)
    grpnms <- names(shapeVec);
    for(i in 1:length(grpnms)){
      sps[clsVec == grpnms[i]] <- shapeVec[i];
    }
    shapes <- sps;
  }else{
    if(show.name | grey.scale){
      shapes <- as.numeric(mSetObj$dataSet$cls)+1;
    }else{
      shapes <- rep(19, length(mSetObj$dataSet$cls));
    }
  }
  return(shapes);
}

GetColorSchema <- function(mSetObj=NA, grayscale=F){
  
  mSetObj <- .get.mSet(mSetObj);
  lvs <- levels(mSetObj$dataSet$cls); 
  grp.num <- length(lvs);
  
  if(grayscale){
    dist.cols <- colorRampPalette(c("grey90", "grey30"))(grp.num);
  }else if(exists("colVec") && !any(colVec =="#NA")){
    dist.cols <- colVec;
  }else{
    pal18 <- c("#e6194B", "#3cb44b", "#4363d8", "#42d4f4", "#f032e6", "#ffe119", "#911eb4", "#f58231", "#bfef45",
               "#fabebe", "#469990", "#e6beff", "#9A6324", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#000075");
    
    if(grp.num <= 18){ # update color and respect default
      dist.cols <- pal18[1:grp.num];
    }else{
      dist.cols <- colorRampPalette(pal18)(grp.num);
    }
  }
  
  colors <- vector(mode="character", length=length(mSetObj$dataSet$cls));
  for(i in 1:length(lvs)){
    colors[mSetObj$dataSet$cls == lvs[i]] <- dist.cols[i];
  }
  return (colors);
}

#'Remove folder
#'@description Remove folder
#'@param folderName Input name of folder to remove
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
RemoveFolder<-function(folderName){
  a<-system(paste("rm",  "-r", folderName), intern=T);
  if(!length(a)>0){
    AddErrMsg(paste("Could not remove file -", folderName));
    return (0);
  }
  return(1);
}

#'Remove file
#'@description Remove file
#'@param fileName Input name of file to remove
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
RemoveFile<-function(fileName){
  if(file.exists(fileName)){
    file.remove(fileName);
  }
}

#'Clear folder and memory
#'@description Clear the current folder and objects in memory
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
ClearUserDir<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  # remove physical files
  unlink(dir(), recursive=T);
  mSetObj$dataSet <- mSetObj$analSet <- list();
  res <- .set.mSet(mSetObj);
  cleanMem();
  return(res);
}

# do memory cleaning after removing many objects
cleanMem <- function(n=10) { for (i in 1:n) gc() }

#'Retrieve last command from the Rhistory.R file
#'@description Fetches the last command from the Rhistory.R file
#'@param regexp Retrieve last command from Rhistory file
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetCMD<-function(regexp){
  # store all lines into a list object
  all.lines<-readLines("Rhistory.R");
  
  all.matches<-grep(regexp, all.lines, value=T);
  if(length(all.matches)==0){
    return(NULL);
  }else{
    # only return the last command
    return(all.matches[length(all.matches)]);
  }
}

# Memory functions
ShowMemoryUse <- function(..., n=20) {
  library(pryr);
  sink(); # make sure print to screen
  print(mem_used());
  print(sessionInfo());
  print(.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n));
  print(warnings());
}

#'Perform utilities for cropping images
#'@description Obtain the full path to convert (from imagemagik)
#'for cropping images
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetConvertFullPath<-function(){
  path <- system("which convert", intern=TRUE);
  if((length(path) == 0) && (typeof(path) == "character")){
    print("Could not find convert in the PATH!");
    return("NA");
  }
  return(path);
}

# need to obtain the full path to convert (from imagemagik) for cropping images
GetBashFullPath<-function(){
  path <- system("which bash", intern=TRUE);
  if((length(path) == 0) && (typeof(path) == "character")){
    print("Could not find bash in the PATH!");
    return("NA");
  }
  return(path);
}

#'Converts xset object from XCMS to mSet object for MetaboAnalyst
#'@description This function converts processed raw LC/MS data from XCMS 
#'to a usable data object (mSet) for MetaboAnalyst. The immediate next step following using this 
#'function is to perform a SanityCheck, and then further data processing and analysis can continue.
#'@param xset The name of the xcmsSet object created.
#'@param dataType The type of data, either list (Compound lists), conc (Compound concentration data), 
#'specbin (Binned spectra data), pktable (Peak intensity table), nmrpeak (NMR peak lists), mspeak (MS peak lists), 
#'or msspec (MS spectra data).
#'@param analType Indicate the analysis module to be performed: stat, pathora, pathqea, msetora, msetssp, msetqea, ts, 
#'cmpdmap, smpmap, or pathinteg.
#'@param paired Logical, is data paired (T) or not (F).
#'@param format Specify if samples are paired and in rows (rowp), unpaired and in rows (rowu),
#'in columns and paired (colp), or in columns and unpaired (colu).
#'@param lbl.type Specify the data label type, either discrete (disc) or continuous (cont).
#'@export

XSet2MSet <- function(xset, dataType, analType, paired=F, format, lbl.type){
  
  data <- xcms::groupval(xset, "medret", "into");
  data2 <- rbind(class= as.character(phenoData(xset)$class), data);
  rownames(data2) <- c("group", paste(round(groups(xset)[,"mzmed"], 3), round(groups(xset)[,"rtmed"]/60, 1), sep="/"));
  write.csv(data2, file="PeakTable.csv");
  mSet <- InitDataObjects("dataType", "analType", paired)
  mSet <- Read.TextData(mSet, "PeakTable.csv", "format", "lbl.type")
  print("mSet successfully created...")
  return(.set.mSet(mSetObj));
}

#'Get fisher p-values
#'@param numSigMembers Number of significant members
#'@param numSigAll Number of all significant features
#'@param numMembers Number of members
#'@param numAllMembers Number of all members
#'@export
GetFisherPvalue <- function(numSigMembers, numSigAll, numMembers, numAllMembers){
  z <- cbind(numSigMembers, numSigAll-numSigMembers, numMembers-numSigMembers, numAllMembers-numMembers-numSigAll+numSigMembers);
  z <- lapply(split(z, 1:nrow(z)), matrix, ncol=2);
  z <- lapply(z, fisher.test, alternative = 'greater');
  p.values <- as.numeric(unlist(lapply(z, "[[", "p.value"), use.names=FALSE));
  return(p.values);
}

saveNetworkInSIF <- function(network, name){
  edges <- .graph.sif(network=network, file=name);
  sif.nm <- paste(name, ".sif", sep="");
  if(length(list.edge.attributes(network))!=0){
    edge.nms <- .graph.eda(network=network, file=name, edgelist.names=edges);
    sif.nm <- c(sif.nm, edge.nms);
  }
  if(length(list.vertex.attributes(network))!=0){
    node.nms <- .graph.noa(network=network, file=name);
    sif.nm <- c(sif.nm, node.nms);
  }
  # need to save all sif and associated attribute files into a zip file for download
  zip(paste(name,"_sif",".zip", sep=""), sif.nm);
}

.graph.sif <- function(network, file){
  edgelist.names <- igraph::get.edgelist(network, names=TRUE)
  edgelist.names <- cbind(edgelist.names[,1], rep("pp", length(E(network))), edgelist.names[,2]);
  write.table(edgelist.names, row.names=FALSE, col.names=FALSE, file=paste(file, ".sif", sep=""), sep="\t", quote=FALSE)
  return(edgelist.names) 
}

# internal method to write cytoscape node attribute files
.graph.noa <- function(network, file){
  all.nms <- c();
  attrib <- list.vertex.attributes(network)
  for(i in 1:length(attrib)){
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "character")
    {
      type <- "String"
    }
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "integer")
    {
      type <- "Integer"
    }
    if(is(get.vertex.attribute(network, attrib[i]))[1] == "numeric")
    {
      type <- "Double"
    }
    noa <- cbind(V(network)$name, rep("=", length(V(network))), get.vertex.attribute(network, attrib[i]))
    first.line <- paste(attrib[i], " (class=java.lang.", type, ")", sep="")
    file.nm <- paste(file, "_", attrib[i], ".NA", sep="");
    write(first.line, file=file.nm, ncolumns = 1, append=FALSE, sep=" ")
    write.table(noa, row.names = FALSE, col.names = FALSE, file=file.nm, sep=" ", append=TRUE, quote=FALSE);
    all.nms <- c(all.nms, file.nm);
  }
  return(all.nms);
}

# internal method to write cytoscape edge attribute files
.graph.eda <- function(network, file, edgelist.names){
  all.nms <- c();
  attrib <- list.edge.attributes(network)
  for(i in 1:length(attrib)){
    if(is(get.edge.attribute(network, attrib[i]))[1] == "character")
    {
      type <- "String"
    }
    if(is(get.edge.attribute(network, attrib[i]))[1] == "integer")
    {
      type <- "Integer"
    }
    if(is(get.edge.attribute(network, attrib[i]))[1] == "numeric")
    {
      type <- "Double"
    }
    eda <- cbind(cbind(edgelist.names[,1], rep("(pp)", length(E(network))), edgelist.names[,3]), rep("=", length(E(network))), get.edge.attribute(network, attrib[i]))
    first.line <- paste(attrib[i], " (class=java.lang.", type, ")", sep="");
    file.nm <- paste(file, "_", attrib[i], ".EA", sep="");
    write(first.line, file=file.nm, ncolumns=1, append=FALSE, sep =" ")
    write.table(eda, row.names = FALSE, col.names = FALSE, file=file.nm, sep=" ", append=TRUE, quote=FALSE);
    all.nms <- c(all.nms, file.nm);
  }
  return(all.nms);
}

PlotLoadBoxplot <- function(mSetObj=NA, cmpd){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
  }
  
  cls.lbls <- mSetObj$dataSet$cls;
  y.label <- GetAbundanceLabel(mSetObj$dataSet$type);
  cmpd.name = paste0("Met_", cmpd, ".png")
  
  Cairo::Cairo(file=cmpd.name, width=240, height=400, bg = "transparent", type="png");
  
  col <- unique(GetColorSchema(mSetObj))
  df <- data.frame(conc = mSetObj$dataSet$norm[, cmpd], class = cls.lbls)
  p <- ggplot2::ggplot(df, aes(x=class, y=conc, fill=class)) + geom_boxplot(notch=FALSE, outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1)
  p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
  p <- p + stat_summary(fun.y=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
  p <- p + theme(text = element_text(size=15), plot.margin = margin(t=0.45, r=0.25, b=1.5, l=0.25, "cm"))
  p <- p + scale_fill_manual(values=col) + ggtitle(cmpd) + theme(axis.text.x = element_text(angle=90, hjust=1), axis.text = element_text(size=10))
  p <- p + theme(plot.title = element_text(size = 14, hjust=0.5, face="bold", vjust=2))
  print(p)
  
  dev.off()
}

#'Compute within group and between group sum of squares
#'(BSS/WSS) for each row of a matrix which may have NA
#'@description Columns have labels, x is a numeric vector,
#'cl is consecutive integers
#'@param x Numeric vector
#'@param cl Columns
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

Get.bwss<-function(x, cl){
  K <- max(cl) - min(cl) + 1
  tvar <- var.na(x);
  tn <- sum(!is.na(x));
  wvar <- wn <- numeric(K);
  
  for(i in (1:K)) {
    if(sum(cl == (i + min(cl) - 1)) == 1){
      wvar[i] <- 0;
      wn[i] <- 1;
    }
    
    if(sum(cl == (i + min(cl) - 1)) > 1) {
      wvar[i] <- var.na(x[cl == (i + min(cl) - 1)]);
      wn[i] <- sum(!is.na(x[cl == (i + min(cl) - 1)]));
    }
  }
  
  WSS <- sum.na(wvar * (wn - 1));
  TSS <- tvar * (tn - 1)
  (TSS - WSS)/WSS;
}

sum.na <- function(x,...){
  res <- NA
  tmp <- !(is.na(x) | is.infinite(x))
  if(sum(tmp) > 0)
    res <- sum(x[tmp])
  res
}

var.na <- function(x){
  res <- NA
  tmp <- !(is.na(x) | is.infinite(x))
  if(sum(tmp) > 1){
    res <- var(as.numeric(x[tmp]))
  }
  res
}

end.with <- function(bigTxt, endTxt){
  return(substr(bigTxt, nchar(bigTxt)-nchar(endTxt)+1, nchar(bigTxt)) == endTxt);
}

## fast T-tests/F-tests using genefilter
## It leverages RSclient to perform one-time memory intensive computing
PerformFastUnivTests <- function(data, cls, var.equal=TRUE){
  print("Peforming fast univariate tests ....");
  rsc <- SetupRSclient();
  
  # note, feature in rows for gene expression
  data <- t(as.matrix(data));
  dat.out <- list(data=data, cls=cls, var.equal=var.equal);
  RS.assign(rsc, "dat.in", dat.out); 
  my.fun <- function(){
    if(length(levels(cls)) > 2){
      res <- try(genefilter::rowFtests(dat.in$data, dat.in$cls, var.equal = dat.in$var.equal));
    }else{
      res <- try(genefilter::rowttests(dat.in$data, dat.in$cls));
    }
    if(class(res) == "try-error") {
      res <- cbind(NA, NA);
    }else{
      res <- cbind(res$statistic, res$p.value);
    }
    return(res);
  }
  RS.assign(rsc, my.fun);
  my.res <- RS.eval(rsc, my.fun());
  RS.close(rsc);
  return(my.res);
}


#'Normalization
#'@description This function performs row-wise normalization, transformation, and 
#'scaling of your metabolomic data. 
#'@usage Normalization(mSetObj, rowNorm, transNorm, scaleNorm, ref=NULL, ratio=FALSE, ratioNum=20)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param rowNorm Select the option for row-wise normalization, "QuantileNorm" for Quantile Normalization, 
#'"ProbNormT" for Probabilistic Quotient Normalization without using a reference sample,
#'"ProbNormF" for Probabilistic Quotient Normalization based on a reference sample, 
#'"CompNorm" for Normalization by a reference feature,
#'"SumNorm" for Normalization to constant sum, 
#'"MedianNorm" for Normalization to sample median, and 
#'"SpecNorm" for Normalization by a sample-specific factor.
#'@param transNorm Select option to transform the data, "LogNorm" for Log Normalization,
#'and "CrNorm" for Cubic Root Transformation. 
#'@param scaleNorm Select option for scaling the data, "MeanCenter" for Mean Centering,
#'"AutoNorm" for Autoscaling, "ParetoNorm" for Pareto Scaling, amd "RangeNorm" for Range Scaling.
#'@param ref Input the name of the reference sample or the reference feature, use " " around the name.  
#'@param ratio This option is only for biomarker analysis.
#'@param ratioNum Relevant only for biomarker analysis.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong
#'McGill University, Canada
#'@export
#'
Normalization <- function(mSetObj=NA, rowNorm, transNorm, scaleNorm, ref=NULL, ratio=FALSE, ratioNum=20){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # PreparePrenormData() called already
  data <- mSetObj$dataSet$prenorm;
  cls <- mSetObj$dataSet$prenorm.cls;
  
  # note, setup time factor
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    if(is.null(mSetObj$dataSet$prenorm.facA)){
      nfacA <- mSetObj$dataSet$facA;
      nfacB <- mSetObj$dataSet$facB;
    }else{
      nfacA <- mSetObj$dataSet$prenorm.facA;
      nfacB <- mSetObj$dataSet$prenorm.facB;
    }
    
    mSetObj$dataSet$facA <- nfacA;
    mSetObj$dataSet$facB <- nfacB;
    if(mSetObj$dataSet$design.type =="time" | mSetObj$dataSet$design.type =="time0"){
      # determine time factor and should order first by subject then by each time points
      if(tolower(mSetObj$dataSet$facA.lbl) == "time"){ 
        time.fac <- nfacA;
        exp.fac <- nfacB;
      }else{
        time.fac <- nfacB;
        exp.fac <- nfacA;
      }
      mSetObj$dataSet$time.fac <- time.fac;
      mSetObj$dataSet$exp.fac <- exp.fac;
    }
  }
  
  colNames <- colnames(data);
  rowNames <- rownames(data);
  
  # row-wise normalization
  if(rowNorm=="QuantileNorm"){
    data<-QuantileNormalize(data);
    # this can introduce constant variables if a variable is 
    # at the same rank across all samples (replaced by its average across all)
    
    varCol <- apply(data, 2, var, na.rm=T);
    constCol <- (varCol == 0 | is.na(varCol));
    constNum <- sum(constCol, na.rm=T);
    if(constNum > 0){
      print(paste("After quantile normalization", constNum, "features with a constant value were found and deleted."));
      data <- data[,!constCol, drop=FALSE];
      colNames <- colnames(data);
      rowNames <- rownames(data);
    }
    rownm<-"Quantile Normalization";
  }else if(rowNorm=="GroupPQN"){
    grp.inx <- cls == ref;
    ref.smpl <- apply(data[grp.inx, , drop=FALSE], 2, mean);
    data<-t(apply(data, 1, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization by a reference group";
  }else if(rowNorm=="SamplePQN"){
    ref.smpl <- data[ref, , drop=FALSE];
    data<-t(apply(data, 1, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization by a reference sample";
  }else if(rowNorm=="CompNorm"){
    data<-t(apply(data, 1, CompNorm, ref));
    rownm<-"Normalization by a reference feature";
  }else if(rowNorm=="SumNorm"){
    data<-t(apply(data, 1, SumNorm));
    rownm<-"Normalization to constant sum";
  }else if(rowNorm=="MedianNorm"){
    data<-t(apply(data, 1, MedianNorm));
    rownm<-"Normalization to sample median";
  }else if(rowNorm=="SpecNorm"){
    if(!exists("norm.vec")){
      norm.vec <- rep(1,nrow(data)); # default all same weight vec to prevent error
      print("No sample specific information were given, all set to 1.0");
    }
    rownm<-"Normalization by sample-specific factor";
    data<-data/norm.vec;
  }else{
    # nothing to do
    rownm<-"N/A";
  }
  
  # use apply will lose dimesion info (i.e. row names and colnames)
  rownames(data)<-rowNames;
  colnames(data)<-colNames;
  
  # note: row-normed data is based on biological knowledge, since the previous
  # replacing zero/missing values by half of the min positive (a constant) 
  # now may become different due to different norm factor, which is artificial
  # variance and should be corrected again
  #
  # stopped, this step cause troubles
  # minConc<-round(min(data)/2, 5);
  # data[dataSet$fill.inx]<-minConc;
  
  # if the reference by feature, the feature column should be removed, since it is all 1
  if(rowNorm=="CompNorm" && !is.null(ref)){
    inx<-match(ref, colnames(data));
    data<-data[,-inx, drop=FALSE];
    colNames <- colNames[-inx];
  }
  
  # record row-normed data for fold change analysis (b/c not applicable for mean-centered data)
  mSetObj$dataSet$row.norm <- as.data.frame(CleanData(data, T, T)); #moved below ratio 
  
  # this is for biomarker analysis only (for compound concentration data)
  if(ratio){
    min.val <- min(abs(data[data!=0]))/2;
    norm.data <- log2((data + sqrt(data^2 + min.val))/2);
    transnm<-"Log Normalization";
    ratio.mat <- CalculatePairwiseDiff(norm.data);
    
    fstats <- Get.Fstat(ratio.mat, cls);
    hit.inx <- rank(-fstats) < ratioNum;  # get top n
    
    ratio.mat <- ratio.mat[, hit.inx, drop=FALSE];
    
    data <- cbind(norm.data, ratio.mat);
    
    colNames <- colnames(data);
    rowNames <- rownames(data);
    mSetObj$dataSet$use.ratio <- TRUE;
    mSetObj$dataSet$proc.ratio <- data;
    
  }else{
    mSetObj$dataSet$use.ratio <- FALSE;
    # transformation
    if(transNorm=='LogNorm'){
      min.val <- min(abs(data[data!=0]))/10;
      data<-apply(data, 2, LogNorm, min.val);
      transnm<-"Log Normalization";
    }else if(transNorm=='CrNorm'){
      norm.data <- abs(data)^(1/3);
      norm.data[data<0] <- - norm.data[data<0];
      data <- norm.data;
      transnm<-"Cubic Root Transformation";
    }else{
      transnm<-"N/A";
    }
  }
  
  # scaling
  if(scaleNorm=='MeanCenter'){
    data<-apply(data, 2, MeanCenter);
    scalenm<-"Mean Centering";
  }else if(scaleNorm=='AutoNorm'){
    data<-apply(data, 2, AutoNorm);
    scalenm<-"Autoscaling";
  }else if(scaleNorm=='ParetoNorm'){
    data<-apply(data, 2, ParetoNorm);
    scalenm<-"Pareto Scaling";
  }else if(scaleNorm=='RangeNorm'){
    data<-apply(data, 2, RangeNorm);
    scalenm<-"Range Scaling";
  }else{
    scalenm<-"N/A";
  }
  
  # note after using "apply" function, all the attribute lost, need to add back
  rownames(data)<-rowNames;
  colnames(data)<-colNames;
  
  # need to do some sanity check, for log there may be Inf values introduced
  data <- CleanData(data, T, F);
  
  if(ratio){
    mSetObj$dataSet$ratio <- CleanData(ratio.mat, T, F)
  }
  
  mSetObj$dataSet$norm <- as.data.frame(data);
  mSetObj$dataSet$cls <- cls;
  
  mSetObj$dataSet$rownorm.method <- rownm;
  mSetObj$dataSet$trans.method <- transnm;
  mSetObj$dataSet$scale.method <- scalenm;
  mSetObj$dataSet$combined.method <- FALSE;
  mSetObj$dataSet$norm.all <- NULL; # this is only for biomarker ROC analysis
  processedObj <- list();#for omicsanalyst
  processedObj$name <- "met_t_omicsanalyst.json"
  processedObj$type <- "met.t"
  processedObj$data.proc <- as.matrix(t(mSetObj$dataSet$norm))
  processedObj$feature.nms <- rownames(processedObj$data.proc)
  processedObj$sample.nms <- colnames(processedObj$data.proc)
  meta = data.frame(Condition = mSetObj$dataSet$cls)
  rownames(meta) <-  colnames(processedObj$data.proc)
  processedObj$meta <- meta
  library(RJSONIO)
  sink(processedObj$name);
  cat(toJSON(processedObj));
  sink();
  return(.set.mSet(mSetObj));
}

#'Row-wise Normalization
#'@description Row-wise norm methods, when x is a row.
#'Normalize by a sum of each sample, assume constant sum (1000).
# Return: normalized data.
#'Options for normalize by sum median, reference sample,
#'reference reference (compound), or quantile normalization
#'@param x Input data to normalize
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'@export
#'
SumNorm<-function(x){
  1000*x/sum(x, na.rm=T);
}

# normalize by median
MedianNorm<-function(x){
  x/median(x, na.rm=T);
}

# normalize by a reference sample (probability quotient normalization)
# ref should be the name of the reference sample
ProbNorm<-function(x, ref.smpl){
  x/median(as.numeric(x/ref.smpl), na.rm=T)
}

# normalize by a reference reference (i.e. creatinine)
# ref should be the name of the cmpd
CompNorm<-function(x, ref){
  1000*x/x[ref];
}

# perform quantile normalization on the raw data (can be log transformed later by user)
# https://stat.ethz.ch/pipermail/bioconductor/2005-April/008348.html
QuantileNormalize <- function(data){
  return(t(preprocessCore::normalize.quantiles(t(data), copy=FALSE)));
}

#'Column-wise Normalization
#'@description Column-wise norm methods, when x is a column
#'Options for log, zero mean and unit variance, and
#'several zero mean and variance/SE 
#'@param x Input data
#'@param min.val Input minimum value
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada

# generalize log, tolerant to 0 and negative values
LogNorm<-function(x, min.val){
  log2((x + sqrt(x^2 + min.val^2))/2)
}

# normalize to zero mean and unit variance
AutoNorm<-function(x){
  (x - mean(x))/sd(x, na.rm=T);
}

# normalize to zero mean but variance/SE
ParetoNorm<-function(x){
  (x - mean(x))/sqrt(sd(x, na.rm=T));
}

# normalize to zero mean but variance/SE
MeanCenter<-function(x){
  x - mean(x);
}

# normalize to zero mean but variance/SE
RangeNorm<-function(x){
  if(max(x) == min(x)){
    x;
  }else{
    (x - mean(x))/(max(x)-min(x));
  }
}

#'Two plot summary plot: Feature View of before and after normalization
#'@description For each plot, the top is a box plot, bottom is a density plot
#'@usage PlotNormSummary(mSetObj, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'@export
#'
PlotNormSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 10.5; h <- 12.5;
  }else if(width==0){
    w = 7.2
    h = 9.5
  }else if(width>0){
    w = width
    h = width*1.25
    # w <- 7.2; h <- 9;
  }
  
  mSetObj$imgSet$norm <- imgName
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  layout(matrix(c(1,2,2,2,3,4,4,4), 4, 2, byrow = FALSE))
  
  # since there may be too many compounds, only plot a subsets (50) in box plot
  # but density plot will use all the data
  
  pre.inx<-GetRandomSubsetIndex(ncol(mSetObj$dataSet$proc), sub.num=50);
  namesVec <- colnames(mSetObj$dataSet$proc[,pre.inx, drop=FALSE]);
  
  # only get common ones
  nm.inx <- namesVec %in% colnames(mSetObj$dataSet$norm)
  namesVec <- namesVec[nm.inx];
  pre.inx <- pre.inx[nm.inx];
  
  norm.inx<-match(namesVec, colnames(mSetObj$dataSet$norm));
  namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  
  rangex.pre <- range(mSetObj$dataSet$proc[, pre.inx, drop=FALSE], na.rm=T);
  rangex.norm <- range(mSetObj$dataSet$norm[, norm.inx, drop=FALSE], na.rm=T);
  
  x.label<-GetAbundanceLabel(mSetObj$dataSet$type);
  y.label<-GetVariableLabel(mSetObj$dataSet$type);
  
  # fig 1
  if(anal.type == "roc" & mSetObj$dataSet$roc_cols == 1){
    op<-par(mar=c(4,7,4,0), xaxt="s");
    plot.new()
  }else{
    op<-par(mar=c(4,7,4,0), xaxt="s");
    plot(density(apply(mSetObj$dataSet$proc, 2, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
    mtext("Density", 2, 5);
    mtext("Before Normalization",3, 1)
  }
  
  # fig 2
  op<-par(mar=c(7,7,0,0), xaxt="s");
  boxplot(mSetObj$dataSet$proc[,pre.inx, drop=FALSE], names=namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T, show.names=T);
  mtext(x.label, 1, 5);
  
  # fig 3
  if(anal.type == "roc" & mSetObj$dataSet$roc_cols == 1){
    op<-par(mar=c(4,7,4,2), xaxt="s");
    plot.new()
  }else{
    op<-par(mar=c(4,7,4,2), xaxt="s");
    plot(density(apply(mSetObj$dataSet$norm, 2, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
    mtext("After Normalization",3, 1);
  }
  
  # fig 4
  op<-par(mar=c(7,7,0,2), xaxt="s");
  boxplot(mSetObj$dataSet$norm[,norm.inx, drop=FALSE], names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", horizontal=T, show.names=T);
  mtext(paste("Normalized",x.label),1, 5);
  
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Two plot summary plot: Sample View of before and after normalization
#'@description For each plot, the top is a density plot and the bottom is a box plot.
#'@usage PlotSampleNormSummary(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'@export

PlotSampleNormSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10.5; h <- 12.5;
  }else if(width == 0){
    w <- 7.2;h <- 9.5;
  }else if(width>0){
    w = width
    h = width*1.25
    # w <- 7.2; h <- 9;
  }
  
  mSetObj$imgSet$summary_norm <-imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  layout(matrix(c(1,1,1,2,3,3,3,4), 4, 2, byrow = FALSE))
  
  # since there may be too many samples, only plot a subsets (50) in box plot
  # but density plot will use all the data
  
  pre.inx<-GetRandomSubsetIndex(nrow(mSetObj$dataSet$proc), sub.num=50);
  namesVec <- rownames(mSetObj$dataSet$proc[pre.inx, , drop=FALSE]);
  
  # only get common ones
  nm.inx <- namesVec %in% rownames(mSetObj$dataSet$norm)
  namesVec <- namesVec[nm.inx];
  pre.inx <- pre.inx[nm.inx];
  
  norm.inx<-match(namesVec, rownames(mSetObj$dataSet$norm));
  namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  
  rangex.pre <- range(mSetObj$dataSet$proc[pre.inx, , drop=FALSE], na.rm=T);
  rangex.norm <- range(mSetObj$dataSet$norm[norm.inx, , drop=FALSE], na.rm=T);
  
  x.label<-GetAbundanceLabel(mSetObj$dataSet$type);
  y.label<-"Samples";
  
  # fig 1
  op<-par(mar=c(5.75,8,4,0), xaxt="s");
  boxplot(t(mSetObj$dataSet$proc[pre.inx, , drop=FALSE]), names= namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T);
  mtext("Before Normalization", 3,1)
  
  # fig 2
  op<-par(mar=c(6.5,7,0,0), xaxt="s");
  plot(density(apply(mSetObj$dataSet$proc, 1, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
  mtext(x.label, 1, 4);
  mtext("Density", 2, 5);
  
  # fig 3
  op<-par(mar=c(5.75,8,4,2), xaxt="s");
  boxplot(t(mSetObj$dataSet$norm[norm.inx, , drop=FALSE]), names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", ylab="", horizontal=T);
  mtext("After Normalization", 3, 1);
  
  # fig 4
  op<-par(mar=c(6.5,7,0,2), xaxt="s");
  plot(density(apply(mSetObj$dataSet$norm, 1, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
  mtext(paste("Normalized",x.label),1, 4)
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Update data for filtering
#'@description Function to update the mSetObj after removing features or samples.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
# note: feature.nm.vec, smpl.nm.vec, grp.nm.vec all set up
UpdateData <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  
  #Reset to default
  mSetObj$dataSet$edit <- mSetObj$dataSet$prenorm <- NULL; 
  
  if(is.null(mSetObj$dataSet$filt)){
    data <- mSetObj$dataSet$proc;
    cls <- mSetObj$dataSet$proc.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$proc.facA;
      facB <- mSetObj$dataSet$proc.facB;
    }
  }else{
    data <- mSetObj$dataSet$filt;
    cls <- mSetObj$dataSet$filt.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$filt.facA;
      facB <- mSetObj$dataSet$filt.facB;
    }
  }
  
  # update feature 
  feat.hit.inx <- colnames(data) %in% feature.nm.vec;
  data <- CleanDataMatrix(data[,!feat.hit.inx,drop=FALSE]);
  #AddMsg("Successfully updated the feature items!");
  
  # update samples
  smpl.hit.inx <- rownames(data) %in% smpl.nm.vec;
  data <- CleanDataMatrix(data[!smpl.hit.inx,,drop=FALSE]);
  cls <- as.factor(as.character(cls[!smpl.hit.inx]));
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    facA <- as.factor(as.character(facA[!smpl.hit.inx]));
    facB <- as.factor(as.character(facB[!smpl.hit.inx]));
  }
  #AddMsg("Successfully updated the sample items!");
  
  # update groups
  grp.hit.inx <- cls %in% grp.nm.vec;
  data <- CleanDataMatrix(data[!grp.hit.inx,,drop=FALSE]);
  cls <- droplevels(factor(cls[!grp.hit.inx])); 
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    facA <- droplevels(factor(facA[!grp.hit.inx]));
    facB <- droplevels(factor(facB[!grp.hit.inx]));
  }
  AddMsg("Successfully updated the data!");
  
  # now set to 
  mSetObj$dataSet$edit <- data;
  mSetObj$dataSet$edit.cls <- cls; 
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    mSetObj$dataSet$edit.facA <- facA;
    mSetObj$dataSet$edit.facB <- facB;
  }
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(length(levels(mSetObj$dataSet$edit.cls)));
  }else{
    return(.set.mSet(mSetObj));
  }
}

# should always init (new or overwrite previous prenorm object)
# note in right order that dataSet$edit will always performed using dataSet$filt (if it exists)
# note dataSet$filt can be re-performed after dataSet$edit during analysis
# need to make sure prenorm created using the latest information (based on both)

#'Prepare data for normalization
#'@description Function should always be initialized (new or overwrite previous prenorm object).
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export

PreparePrenormData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.null(mSetObj$dataSet$edit)){
    mydata <- mSetObj$dataSet$edit;
    if(!is.null(mSetObj$dataSet$filt)){
      # some features could be removed
      hit.inx <- colnames(mydata) %in% colnames(mSetObj$dataSet$filt);
      mydata <- mydata[,hit.inx, drop=FALSE];
    }
    mSetObj$dataSet$prenorm <- mydata;
    mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$edit.cls;
    if(substring(mSetObj$dataSet$format,4,5) == "ts"){
      mSetObj$dataSet$prenorm.facA <- mSetObj$dataSet$edit.facA;
      mSetObj$dataSet$prenorm.facB <- mSetObj$dataSet$edit.facB;
    }
  }else if(!is.null(mSetObj$dataSet$filt)){
    mSetObj$dataSet$prenorm <- mSetObj$dataSet$filt;
    mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$filt.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      mSetObj$dataSet$prenorm.facA <- mSetObj$dataSet$filt.facA;
      mSetObj$dataSet$prenorm.facB <- mSetObj$dataSet$filt.facB;
    }
  }else{
    mSetObj$dataSet$prenorm <- mSetObj$dataSet$proc;
    mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$proc.cls;
    if(substring(mSetObj$dataSet$format,4,5) == "ts"){
      mSetObj$dataSet$prenorm.facA <- mSetObj$dataSet$proc.facA;
      mSetObj$dataSet$prenorm.facB <- mSetObj$dataSet$proc.facB;
    }
  }
  .set.mSet(mSetObj)
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

# get the dropdown list for sample normalization view
GetPrenormSmplNms <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$dataSet$prenorm));
}

GetPrenormFeatureNum <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(ncol(mSetObj$dataSet$prenorm));
}

GetPrenormFeatureNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$dataSet$prenorm));
}

ValidateFeatureName<- function(mSetObj=NA, nm){
  mSetObj <- .get.mSet(mSetObj);
  if(nm %in% colnames(mSetObj$dataSet$prenorm)){
    return(1);
  }
  return(0);
}

GetPrenormClsNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(levels(mSetObj$dataSet$prenorm.cls));
}

########## Utility Functions ###############
GetRandomSubsetIndex<-function(total, sub.num = 50){
  if(total < sub.num){
    1:total;
  }else{
    sample(1:total, sub.num);
  }
}

# Test if data require genefilter version 
# if so, then microservice will be used
RequireFastUnivTests <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(ncol(mSetObj$dataSet$norm) < 1000){
    return(FALSE);
  }else{
    return(TRUE);
  }
}

#'Perform PCA analysis
#'@description Perform PCA analysis, obtain variance explained, store item to PCA object
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'@param mSetObj Input name of the created mSet Object
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PCA.Anal <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  pca <- prcomp(mSetObj$dataSet$norm, center=TRUE, scale=F);
  
  # obtain variance explained
  sum.pca <- summary(pca);
  imp.pca <- sum.pca$importance;
  std.pca <- imp.pca[1,]; # standard devietation
  var.pca <- imp.pca[2,]; # variance explained by each PC
  cum.pca <- imp.pca[3,]; # cummulated variance explained
  
  # store the item to the pca object
  mSetObj$analSet$pca<-append(pca, list(std=std.pca, variance=var.pca, cum.var=cum.pca));
  write.csv(signif(mSetObj$analSet$pca$x,5), file="pca_score.csv");
  write.csv(signif(mSetObj$analSet$pca$rotation,5), file="pca_loadings.csv");
  mSetObj$analSet$pca$loading.type <- "all";
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
}

#'Rotate PCA analysis
#'@description Rotate PCA analysis
#'@param mSetObj Input name of the created mSet Object
#'@param axisOpt Input the axis option 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PCA.Flip <- function(mSetObj=NA, axisOpt){
  
  mSetObj <- .get.mSet(mSetObj);
  
  pca<-mSetObj$analSet$pca;
  # store the item to the pca object
  if(axisOpt == "x"){
    pca$x[,1] <- -pca$x[,1];
    pca$rotation[,1] <- -pca$rotation[,1];
  }else if(axisOpt == "y"){
    pca$x[,2] <- -pca$x[,2];
    pca$rotation[,2] <- -pca$rotation[,2];
  }else{ # all
    pca$x <- -pca$x;
    pca$rotation <- -pca$rotation;
  }
  write.csv(signif(pca$x,5), file="pca_score.csv");
  write.csv(signif(pca$rotation,5), file="pca_loadings.csv");
  
  mSetObj$analSet$pca <- pca;
  return(.set.mSet(mSetObj));
}

#'Plot PCA pair summary, format image in png, tiff, pdf, ps, svg
#'@description Rotate PCA analysis
#'@usage PlotPCAPairSummary(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pc.num)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pc.num Numeric, input a number to indicate the number of principal components to display in the pairwise score plot.
#'@export
#'
PlotPCAPairSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pc.num){
  
  mSetObj <- .get.mSet(mSetObj);
  pclabels <- paste("PC", 1:pc.num, "\n", round(100*mSetObj$analSet$pca$variance[1:pc.num],1), "%");
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  
  mSetObj$imgSet$pca.pair <- imgName;
  
  h <- w;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  if(mSetObj$dataSet$cls.type == "disc"){
    pairs(mSetObj$analSet$pca$x[,1:pc.num], col=GetColorSchema(mSetObj), pch=as.numeric(mSetObj$dataSet$cls)+1, labels=pclabels);
  }else{
    pairs(mSetObj$analSet$pca$x[,1:pc.num], labels=pclabels);
  }
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot PCA scree plot
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param scree.num Numeric, input a number to indicate the number of principal components to display in the scree plot.
#'@usage PlotPCAScree(mSetObj=NA, imgName, format="png", dpi=72, width=NA, scree.num)
#'@export
#'
PlotPCAScree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, scree.num){
  
  mSetObj <- .get.mSet(mSetObj);
  
  stds <-mSetObj$analSet$pca$std[1:scree.num];
  pcvars<-mSetObj$analSet$pca$variance[1:scree.num];
  cumvars<-mSetObj$analSet$pca$cum.var[1:scree.num];
  
  ylims <- range(c(pcvars,cumvars));
  extd<-(ylims[2]-ylims[1])/10
  miny<- ifelse(ylims[1]-extd>0, ylims[1]-extd, 0);
  maxy<- ifelse(ylims[2]+extd>1, 1.0, ylims[2]+extd);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*2/3;
  
  mSetObj$imgSet$pca.scree <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,6,3));
  plot(pcvars, type='l', col='blue', main='Scree plot', xlab='PC index', ylab='Variance explained', ylim=c(miny, maxy), axes=F)
  text(pcvars, labels =paste(100*round(pcvars,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  points(pcvars, col='red');
  
  lines(cumvars, type='l', col='green')
  text(cumvars, labels =paste(100*round(cumvars,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  points(cumvars, col='red');
  
  abline(v=1:scree.num, lty=3);
  axis(2);
  axis(1, 1:length(pcvars), 1:length(pcvars));
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Create 2D PCA score plot
#'@description Rotate PCA analysis
#'@usage PlotPCA2DScore(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pcx Specify the principal component on the x-axis
#'@param pcy Specify the principal component on the y-axis
#'@param reg Numeric, input a number between 0 and 1, 0.95 will display the 95 percent confidence regions, and 0 will not.
#'@param show Display sample names, 1 = show names, 0 = do not show names.
#'@param grey.scale Use grey-scale colors, 1 = grey-scale, 0 = not grey-scale.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPCA2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcx, pcy, reg = 0.95, show=1, grey.scale = 0){
  
  mSetObj <- .get.mSet(mSetObj);
  
  xlabel = paste("PC",pcx, "(", round(100*mSetObj$analSet$pca$variance[pcx],1), "%)");
  ylabel = paste("PC",pcy, "(", round(100*mSetObj$analSet$pca$variance[pcy],1), "%)");
  pc1 = mSetObj$analSet$pca$x[, pcx];
  pc2 = mSetObj$analSet$pca$x[, pcy];
  text.lbls<-substr(names(pc1),1,14) # some names may be too long
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pca.score2d <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  op<-par(mar=c(5,5,3,3));
  
  if(mSetObj$dataSet$cls.type == "disc"){
    # obtain ellipse points to the scatter plot for each category
    
    if(mSetObj$dataSet$type.cls.lbl=="integer"){
      cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
    }else{
      cls <- mSetObj$dataSet$cls;
    }
    
    lvs <- levels(cls);
    pts.array <- array(0, dim=c(100,2,length(lvs)));
    for(i in 1:length(lvs)){
      inx <-mSetObj$dataSet$cls == lvs[i];
      groupVar<-var(cbind(pc1[inx],pc2[inx]), na.rm=T);
      groupMean<-cbind(mean(pc1[inx], na.rm=T),mean(pc2[inx], na.rm=T));
      pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
    }
    
    xrg <- range(pc1, pts.array[,1,]);
    yrg <- range(pc2, pts.array[,2,]);
    x.ext<-(xrg[2]-xrg[1])/12;
    y.ext<-(yrg[2]-yrg[1])/12;
    xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
    ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);
    
    cols <- GetColorSchema(mSetObj, grey.scale==1);
    uniq.cols <- unique(cols);
    
    plot(pc1, pc2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot",
         col=cols, pch=as.numeric(mSetObj$dataSet$cls)+1); ## added
    grid(col = "lightgray", lty = "dotted", lwd = 1);
    
    # make sure name and number of the same order DO NOT USE levels, which may be different
    legend.nm <- unique(as.character(sort(cls)));
    ## uniq.cols <- unique(cols);
    
    ## BHAN: when same color is choosen; it makes an error
    if ( length(uniq.cols) > 1 ) {
      names(uniq.cols) <- legend.nm;
    }
    
    # draw ellipse
    for(i in 1:length(lvs)){
      if (length(uniq.cols) > 1) {
        polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.2), border=NA);
      } else {
        polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.2), border=NA);
      }
      if(grey.scale) {
        lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
      }
    }
    
    pchs <- GetShapeSchema(mSetObj, show, grey.scale);
    if(grey.scale) {
      cols <- rep("black", length(cols));
    }
    if(show == 1){
      text(pc1, pc2, label=text.lbls, pos=4, xpd=T, cex=0.75);
      points(pc1, pc2, pch=pchs, col=cols);
    }else{
      if(length(uniq.cols) == 1){
        points(pc1, pc2, pch=pchs, col=cols, cex=1.0);
      }else{
        if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>=0))){
          points(pc1, pc2, pch=pchs, col=adjustcolor(cols, alpha.f = 0.4), cex=1.8);
        }else{
          points(pc1, pc2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2);
        }
      }
    }
    uniq.pchs <- unique(pchs);
    if(grey.scale) {
      uniq.cols <- "black";
    }
    
    if(length(lvs) < 6){
      legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
    }else if (length(lvs) < 10){
      legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols, cex=0.75);
    }else{
      legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols, cex=0.5);
    }
    
  }else{
    plot(pc1, pc2, xlab=xlabel, ylab=ylabel, type='n', main="Scores Plot");
    points(pc1, pc2, pch=15, col="magenta");
    text(pc1, pc2, label=text.lbls, pos=4, col ="blue", xpd=T, cex=0.8);
  }
  par(op);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Create 3D PCA score plot
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage PlotPCA3DScore(mSetObj=NA, imgName, format="json", inx1, inx2, inx3)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param inx3 Numeric, indicate the number of the principal component for the z-axis of the loading plot.
#'@export
#'
PlotPCA3DScore <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  
  pca <-  mSetObj$analSet$pca;
  pca3d <- list();
  pca3d$score$axis <- paste("PC", c(inx1, inx2, inx3), " (", 100*round( mSetObj$analSet$pca$variance[c(inx1, inx2, inx3)], 3), "%)", sep="");
  coords <- data.frame(t(signif(pca$x[,c(inx1, inx2, inx3)], 5)));
  colnames(coords) <- NULL;
  pca3d$score$xyz <- coords;
  pca3d$score$name <- rownames(mSetObj$dataSet$norm);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pca3d$score$facA <- cls;
  
  # now set color for each group
  cols <- unique(GetColorSchema(mSetObj));
  rgbcols <- col2rgb(cols);
  cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
  pca3d$score$colors <- cols;
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- RJSONIO::toJSON(pca3d, .na='null');
  sink(imgName);
  cat(json.obj);
  sink();
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj));
  }
}

#'@export
PlotPCA3DLoading <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  pca = mSetObj$analSet$pca
  coords<-signif(as.matrix(cbind(pca$rotation[,inx1],pca$rotation[,inx2],pca$rotation[,inx3])),5);
  pca3d <- list();
  
  pca3d$loading$axis <- paste("Loading ", c(inx1, inx2, inx3), sep="");
  coords <- data.frame(t(signif(pca$rotation[,1:3], 5)));
  
  colnames(coords) <- NULL; 
  pca3d$loading$xyz <- coords;
  pca3d$loading$name <- rownames(pca$rotation);
  pca3d$loading$entrez <-rownames(pca$rotation); 
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pca3d$cls = cls;
  # see if there is secondary
  
  require(RJSONIO);
  imgName = paste(imgName, ".", format, sep="");
  json.mat <- toJSON(pca3d, .na='null');
  sink(imgName);
  cat(json.mat);
  sink();
  current.msg <<- "Annotated data is now ready for PCA 3D visualization!";
  
  if(.on.public.web){
    return(1);
  }else{
    return(.set.mSet(mSetObj));
  }
  
}

#'Update PCA loadings
#'@description Update the PCA loadings
#'@param mSetObj Input name of the created mSet Object
#'@param plotType Set annotation type, "all" to label all variables and
#'"none" to label no variables.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

UpdatePCA.Loading<- function(mSetObj=NA, plotType){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$pca$loading.type <- plotType;
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
}

#'Plot PCA loadings and also set up the matrix for display
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage PlotPCALoading(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, plotType, lbl.feat=1)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@export
#'
PlotPCALoading <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2){
  
  mSetObj <- .get.mSet(mSetObj);
  
  loadings<-signif(as.matrix(cbind(mSetObj$analSet$pca$rotation[,inx1],mSetObj$analSet$pca$rotation[,inx2])),5);
  ldName1<-paste("Loadings", inx1);
  ldName2<-paste("Loadings", inx2);
  colnames(loadings)<-c(ldName1, ldName2);
  load.x.uniq <- jitter(loadings[,1]);
  names(load.x.uniq) <- rownames(loadings);
  mSetObj$analSet$pca$load.x.uniq <- load.x.uniq;
  mSetObj$analSet$pca$imp.loads<-loadings; # set up the loading matrix
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pca.loading <- imgName;
  plotType <- mSetObj$analSet$pca$loading.type;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  par(mar=c(6,5,2,6));
  plot(loadings[,1],loadings[,2], las=2, xlab=ldName1, ylab=ldName2);
  
  mSetObj$pca.axis.lims <- par("usr"); # x1, x2, y1 ,y2
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  points(loadings[,1],loadings[,2], pch=19, col=adjustcolor("magenta", alpha.f = 0.4));
  
  if(plotType=="all"){
    text(loadings[,1],loadings[,2], labels=substr(rownames(loadings), 1, 16), pos=4, col="blue", xpd=T);
  }else if(plotType == "custom"){
    if(length(mSetObj$custom.cmpds) > 0){
      hit.inx <- colnames(mSetObj$dataSet$norm) %in% mSetObj$custom.cmpds;
      text(loadings[hit.inx,1],loadings[hit.inx,2], labels=rownames(loadings)[hit.inx], pos=4, col="blue", xpd=T);
    }
  }else{
    # do nothing
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
  
}

#'Create PCA Biplot, set xpd = T to plot outside margin
#'@description Rotate PCA analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@usage PlotPCABiplot(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@export
#'
PlotPCABiplot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2){
  
  mSetObj <- .get.mSet(mSetObj);
  choices = c(inx1, inx2);
  scores <- mSetObj$analSet$pca$x;
  lam <- mSetObj$analSet$pca$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pca.biplot<-imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  biplot(t(t(scores[, choices]) / lam), t(t(mSetObj$analSet$pca$rotation[, choices]) * lam), xpd =T, cex=0.9);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'PLS analysis using oscorespls (Orthogonal scores algorithm)
#'so that VIP can be calculated
#'note: the VIP is calculated only after PLSDA-CV is performed
#'to determine the best # of comp. used for VIP
#'@description PLS analysis using oscorespls
#'@param mSetObj Input name of the created mSet Object
#'@param reg Logical
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PLSR.Anal <- function(mSetObj=NA, reg=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  comp.num <- dim(mSetObj$dataSet$norm)[1]-1;
  
  if(comp.num > 8) {
    #need to deal with small number of predictors
    comp.num <- min(dim(mSetObj$dataSet$norm)[2], 8)
  }
  
  if(.on.public.web){
    load_pls()
  }
  
  # note, standardize the cls, to minimize the impact of categorical to numerical impact
  if(reg){
    cls <- scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls <- model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  mSetObj$analSet$plsr <- pls::plsr(cls~datmat, method='oscorespls', ncomp=comp.num);
  mSetObj$analSet$plsr$reg <- reg;
  mSetObj$analSet$plsr$loading.type <- "all";
  mSetObj$custom.cmpds <- c();
  
  write.csv(signif(mSetObj$analSet$plsr$scores,5), row.names=rownames(mSetObj$dataSet$norm), file="plsda_score.csv");
  write.csv(signif(mSetObj$analSet$plsr$loadings,5), file="plsda_loadings.csv");
  return(.set.mSet(mSetObj));
}

#'Plot PLS pairwise summary
#'@description Plot PLS pairwise summary
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pc.num Numeric, indicate the number of principal components
#'@export

PlotPLSPairSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pc.num){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
    
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pls.pair <- imgName;
  
  vars <- round(100*mSetObj$analSet$plsr$Xvar[1:pc.num]/mSetObj$analSet$plsr$Xtotvar,1);
  my.data <- mSetObj$analSet$plsr$scores[,1:pc.num];
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  pclabels <- paste("Component", 1:pc.num, "\n", vars, "%");
  pairs(my.data, col=GetColorSchema(mSetObj), pch=as.numeric(mSetObj$dataSet$cls)+1, labels=pclabels)
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot PLS score plot
#'@description Plot PLS score plot
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param reg Numeric, default is 0.95
#'@param show Show labels, 1 or 0
#'@param grey.scale Numeric, use a grey scale (0) or not (1)
#'@param use.sparse Logical, use a sparse algorithm (T) or not (F)
#'@export
#'
PlotPLS2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0, use.sparse=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$plsr$scores[,inx1];
  lv2 <- mSetObj$analSet$plsr$scores[,inx2];
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$plsr$Xvar[inx1]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$plsr$Xvar[inx2]/mSetObj$analSet$plsr$Xtotvar,1), "%)");
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,3));
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long
  
  # obtain ellipse points to the scatter plot for each category
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  lvs <- levels(cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- mSetObj$dataSet$cls == lvs[i];
    groupVar <- var(cbind(lv1[inx],lv2[inx]), na.rm=T);
    groupMean <- cbind(mean(lv1[inx], na.rm=T),mean(lv2[inx], na.rm=T));
    pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
  }
  
  xrg <- range(lv1, pts.array[,1,]);
  yrg <- range(lv2, pts.array[,2,]);
  x.ext<-(xrg[2]-xrg[1])/12;
  y.ext<-(yrg[2]-yrg[1])/12;
  xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);
  
  ## cols = as.numeric(dataSet$cls)+1;
  cols <- GetColorSchema(mSetObj, grey.scale==1);
  uniq.cols <- unique(cols);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  
  # make sure name and number of the same order DO NOT USE levels, which may be different
  legend.nm <- unique(as.character(sort(cls)));
  ## uniq.cols <- unique(cols);
  
  ## BHAN: when same color is choosen for black/white; it makes an error
  # names(uniq.cols) <- legend.nm;
  if (length(uniq.cols) > 1) {
    names(uniq.cols) <- legend.nm;
  }
  # draw ellipse
  for(i in 1:length(lvs)){
    if ( length(uniq.cols) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.2), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.2), border=NA);
    }
    if(grey.scale) {
      lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
    }
  }
  
  pchs <- GetShapeSchema(mSetObj, show, grey.scale);
  if(grey.scale) {
    cols <- rep("black", length(cols));
  }
  if(show==1){ # display sample name set on
    text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75);
    points(lv1, lv2, pch=pchs, col=cols);
  }else{
    if (length(uniq.cols) == 1) {
      points(lv1, lv2, pch=pchs, col=cols, cex=1.0);
    } else {
      if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>=0))){
        points(lv1, lv2, pch=pchs, col=adjustcolor(cols, alpha.f = 0.4), cex=1.8);
      }else{
        points(lv1, lv2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2);
      }
    }
  }
  
  uniq.pchs <- unique(pchs);
  if(grey.scale) {
    uniq.cols <- "black";
  }
  legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot PLS 3D score plot
#'@description Plot PLS 3D score plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param inx3 Numeric, indicate the number of the principal component for the z-axis of the loading plot.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPLS3DScore <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  
  pls3d <- list();
  pls3d$score$axis <- paste("Component", c(inx1, inx2, inx3), " (", round(100*mSetObj$analSet$plsr$Xvar[c(inx1, inx2, inx3)]/mSetObj$analSet$plsr$Xtotvar, 1), "%)", sep="");
  coords <- data.frame(t(signif(mSetObj$analSet$plsr$score[,c(inx1, inx2, inx3)], 5)));
  colnames(coords) <- NULL;
  pls3d$score$xyz <- coords;
  pls3d$score$name <- rownames(mSetObj$dataSet$norm);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pls3d$score$facA <- cls;
  
  # now set color for each group
  cols <- unique(GetColorSchema(mSetObj));
  rgbcols <- col2rgb(cols);
  cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
  pls3d$score$colors <- cols;
  
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- RJSONIO::toJSON(pls3d, .na='null');
  sink(imgName);
  cat(json.obj);
  sink();
  mSet$imgSet$pls.score3d <- imgName;
  return(.set.mSet(mSetObj));
}

#'@export
PlotPLS3DLoading <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  pls = mSetObj$analSet$plsr
  coords<-signif(as.matrix(cbind(pls$loadings[,inx1],pls$loadings[,inx2],pls$loadings[,inx3])),5);
  pls3d <- list();
  
  pls3d$loading$axis <- paste("Loading ", c(inx1, inx2, inx3), sep="");
  coords <- data.frame(t(signif(pls$loadings[,c(inx1, inx2, inx3)], 5)));
  
  colnames(coords) <- NULL; 
  pls3d$loading$xyz <- coords;
  pls3d$loading$name <- rownames(pls$loadings);
  pls3d$loading$entrez <-rownames(pls$loadings); 
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  pls3d$cls = cls;
  # see if there is secondary
  
  require(RJSONIO);
  imgName = paste(imgName, ".", format, sep="");
  json.mat <- RJSONIO::toJSON(pls3d, .na='null');
  sink(imgName);
  cat(json.mat);
  sink();
  current.msg <<- "Annotated data is now ready for PCA 3D visualization!";
  
  if(.on.public.web){
    return(1);
  }else{
    return(.set.mSet(mSetObj));
  }
  
}

#'Update PLS loadings
#'@description Update the PLS loadings
#'@param mSetObj Input name of the created mSet Object
#'@param plotType Set annotation type, "all" to label all variables and
#'"none" to label no variables.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

UpdatePLS.Loading<- function(mSetObj=NA, plotType){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$plsr$loading.type <- plotType;
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
}


#'Plot PLS loading plot, also set the loading matrix for display
#'@description Plot PLS loading plot, also set the loading matrix for display
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5. The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPLSLoading <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2){
  
  mSetObj <- .get.mSet(mSetObj);
  # named vector
  load1<-mSetObj$analSet$plsr$loadings[,inx1];
  load2<-mSetObj$analSet$plsr$loadings[,inx2];
  loadings = signif(as.matrix(cbind(load1, load2)),5);
  
  ldName1<-paste("Loadings", inx1);
  ldName2<-paste("Loadings", inx2)
  colnames(loadings)<-c(ldName1, ldName2);
  load.x.uniq <- jitter(loadings[,1]);
  names(load.x.uniq) <- rownames(loadings);
  mSetObj$analSet$plsr$load.x.uniq <- load.x.uniq;
  mSetObj$analSet$plsr$imp.loads<-loadings; # set up loading matrix
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pls.loading <- imgName;
  plotType <- mSetObj$analSet$plsr$loading.type;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  par(mar=c(6,4,4,5));
  plot(loadings[,1],loadings[,2], las=2, xlab=ldName1, ylab=ldName2);
  
  mSetObj$pls.axis.lims <- par("usr"); # x1, x2, y1 ,y2
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  points(loadings[,1],loadings[,2], pch=19, col=adjustcolor("magenta", alpha.f = 0.4));
  
  if(plotType=="all"){
    text(loadings[,1],loadings[,2], labels=substr(rownames(loadings), 1, 16), pos=4, col="blue", xpd=T);
  }else if(plotType == "custom"){
    if(length(mSetObj$custom.cmpds) > 0){
      hit.inx <- colnames(mSetObj$dataSet$norm) %in% mSetObj$custom.cmpds;
      text(loadings[hit.inx,1],loadings[hit.inx,2], labels=rownames(loadings)[hit.inx], pos=4, col="blue", xpd=T);
    }
  }else{
    # do nothing
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'PLS-DA classification and feature selection
#'@description PLS-DA classification and feature selection
#'@param mSetObj Input name of the created mSet Object
#'@param methodName Logical, by default set to TRUE
#'@param compNum GetDefaultPLSCVComp()
#'@param choice Input the choice, by default it is Q2
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
PLSDA.CV <- function(mSetObj=NA, methodName="T", compNum=GetDefaultPLSCVComp(mSetObj), choice="Q2"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_caret()
  }
  
  # get classification accuracy using caret
  plsda.cls <- caret::train(mSetObj$dataSet$norm, mSetObj$dataSet$cls, "pls", trControl=caret::trainControl(method=ifelse(methodName == 'L', "LOOCV", 'CV')), tuneLength=compNum);
  
  # note, for regression, use model matrix
  if(mSetObj$analSet$plsr$reg){
    cls<-cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  
  # use the classifical regression to get R2 and Q2 measure
  plsda.reg <- pls::plsr(cls~datmat,method ='oscorespls', ncomp=compNum, validation= ifelse(methodName == 'L', "LOO", 'CV'));
  fit.info <- pls::R2(plsda.reg, estimate = "all")$val[,1,];
  
  # combine accuracy, R2 and Q2
  accu <- plsda.cls$results[,2]
  all.info <- rbind(accu, fit.info[,-1]);
  
  rownames(all.info) <- c("Accuracy", "R2", "Q2");
  
  # default use best number determined by Q2
  if(choice == 'Q2'){
    best.num <- which(all.info[3,] == max(all.info[3,]));
  }else if(choice == "R2"){
    best.num <- which(all.info[2,] == max(all.info[2,]));
  }else{
    best.num <- which(all.info[1,] == max(all.info[1,]));
  }
  
  # get coef. table, this can be error when class is very unbalanced
  coef.mat <- try(caret::varImp(plsda.cls, scale=T)$importance);
  if(class(coef.mat) == "try-error") {
    coef.mat <- NULL;
  }else{
    if(mSetObj$dataSet$cls.num > 2){ # add an average coef for multiple class
      coef.mean <- apply(coef.mat, 1, mean);
      coef.mat <- cbind(coef.mean = coef.mean, coef.mat);
    }
    # rearange in decreasing order, keep as matrix, prevent dimesion dropping if only 1 col
    inx.ord <- order(coef.mat[,1], decreasing=T);
    coef.mat <- data.matrix(coef.mat[inx.ord, ,drop=FALSE]);
    write.csv(signif(coef.mat,5), file="plsda_coef.csv"); # added 27 Jan 2014
  }
  # calculate VIP http://mevik.net/work/software/VIP.R
  pls <- mSetObj$analSet$plsr;
  b <- c(pls$Yloadings)[1:compNum];
  T <- pls$scores[,1:compNum, drop = FALSE]
  SS <- b^2 * colSums(T^2)
  W <- pls$loading.weights[,1:compNum, drop = FALSE]
  Wnorm2 <- colSums(W^2);
  SSW <- sweep(W^2, 2, SS / Wnorm2, "*")
  vips <- sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS));
  if(compNum > 1){
    vip.mat <- as.matrix(t(vips));
  }else{
    vip.mat <- as.matrix(vips);
  }
  colnames(vip.mat) <- paste("Comp.", 1:ncol(vip.mat));
  write.csv(signif(vip.mat,5),file="plsda_vip.csv");
  
  mSetObj$analSet$plsda<-list(best.num=best.num, choice=choice, coef.mat=coef.mat, vip.mat=vip.mat, fit.info=all.info);
  return(.set.mSet(mSetObj));
}

#'Perform PLS-DA permutation
#'@description Perform PLS-DA permutation using training classification accuracy as
#'indicator, for two or multi-groups
#'@param mSetObj Input name of the created mSet Object
#'@param num Numeric, input the number of permutations
#'@param type Type of accuracy, if "accu" indicate prediction accuracy, else "sep" is separation distance
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PLSDA.Permut <- function(mSetObj=NA, num=100, type="accu"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  orig.cls <- cls <- as.numeric(mSetObj$dataSet$cls);
  datmat <- as.matrix(mSetObj$dataSet$norm);
  best.num <- mSetObj$analSet$plsda$best.num;
  
  # dummy is not used, for the purpose to maintain lapply API
  Get.pls.bw <- function(dummy){
    cls <- cls[order(runif(length(cls)))];
    pls <- caret::plsda(datmat, as.factor(cls), ncomp=best.num);
    pred <- predict(pls, datmat);
    Get.bwss(pred, cls);
  }
  
  Get.pls.accu <- function(dummy){
    cls <- cls[order(runif(length(cls)))];
    pls <- caret::plsda(datmat, as.factor(cls), ncomp=best.num);
    pred <- predict(pls, datmat);
    sum(pred == cls)/length(cls);
  }
  
  # first calculate the bw values with original labels
  pls <- caret::plsda(datmat, as.factor(orig.cls), ncomp=best.num);
  pred.orig <- predict(pls, datmat);
  if(type=="accu"){
    perm.type = "prediction accuracy";
    res.orig <- sum(pred.orig == orig.cls)/length(orig.cls);
    res.perm <- Perform.permutation(num, Get.pls.accu);
  }else{
    perm.type = "separation distance";
    res.orig <- Get.bwss(pred.orig, orig.cls);
    res.perm <- Perform.permutation(num, Get.pls.bw);
  }
  # perm.num may be adjusted on public server
  perm.num <- res.perm$perm.num;
  perm.res <- res.perm$perm.res;
  perm.vec <- c(res.orig, unlist(perm.res, use.names=FALSE));
  # check for infinite since with group variance could be zero for perfect classification
  inf.found = TRUE;
  if(sum(is.finite(perm.vec))==length(perm.vec)){
    inf.found = FALSE;
  }else {
    if(sum(is.finite(perm.vec))==0){ # all are infinite, give a random number 10
      perm.vec<-rep(10, length(perm.vec));
    }else{ # if not all inf, replace with the 10 fold of non-inf values
      perm.vec[!is.finite(perm.vec)]<-10*max(perm.vec[is.finite(perm.vec)]);
    }
  }
  
  # calculate the significant p value as the proportion of sampled permutations better than or equal to original one
  # note, the precision is determined by the permutation number i.e. for 100 time, no better than original
  # p value is < 0.01, we can not say it is zero
  better.hits <- sum(perm.vec[-1]>=perm.vec[1]);
  if(better.hits == 0) {
    p <- paste("p < ", 1/perm.num, " (", better.hits, "/", perm.num, ")", sep="");
  }else{
    p <- better.hits/perm.num;
    p <- paste("p = ", signif(p, digits=5), " (", better.hits, "/", perm.num, ")", sep="");
  }
  
  mSetObj$analSet$plsda$permut.p <- p;
  mSetObj$analSet$plsda$permut.inf <- F;
  mSetObj$analSet$plsda$permut.type <- perm.type;
  mSetObj$analSet$plsda$permut <- perm.vec;
  
  msg <- paste("Empirical p value:", p);
  if(.on.public.web){
    .set.mSet(mSetObj)
    return(msg)
  }else{
    print(msg);
    return(.set.mSet(mSetObj));
  }
}

#'Plot PLS important features
#'@description Plot PLS important features, BHan: added bgcolor parameter for B/W color
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param type Indicate the type variables of importance to use, "vip" to use VIp scores, or "type"
#'for coefficients  
#'@param feat.nm Feature name
#'@param feat.num Feature numbers
#'@param color.BW Logical, true to use black and white, or false to not
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPLS.Imp <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, type, feat.nm, feat.num, color.BW=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$pls.imp<-imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  if(type=="vip"){
    mSetObj$analSet$plsda$imp.type <- "vip";
    vips <- mSetObj$analSet$plsda$vip.mat[,feat.nm];
    PlotImpVar(mSetObj, vips, "VIP scores", feat.num, color.BW);
  }else{
    mSetObj$analSet$plsda$imp.type <- "coef";
    data<-mSetObj$analSet$plsda$coef.mat[,feat.nm];
    PlotImpVar(mSetObj, data, "Coefficients", feat.num, color.BW);
  }
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Plot PLS important variables,
#'@description Plot PLS important variables, BHan: added bgcolor parameter for B/W color
#'@param mSetObj Input name of the created mSet Object
#'@param imp.vec Input the vector of important variables
#'@param xlbl Input the x-label
#'@param feat.num Numeric, set the feature numbers, default is set to 15
#'@param color.BW Use black-white for plot (T) or colors (F)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotImpVar <- function(mSetObj=NA, imp.vec, xlbl, feat.num=15, color.BW=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  cls.len <- length(levels(mSetObj$dataSet$cls));
  if(cls.len == 2){
    rt.mrg <- 5;
  }else if(cls.len == 3){
    rt.mrg <- 6;
  }else if(cls.len == 4){
    rt.mrg <- 7;
  }else if(cls.len == 5){
    rt.mrg <- 8;
  }else if(cls.len == 6){
    rt.mrg <- 9;
  }else{
    rt.mrg <- 11;
  }
  op <- par(mar=c(5,7,3,rt.mrg)); # set right side margin with the number of class
  
  if(feat.num <= 0){
    feat.num = 15;
  }
  
  if(feat.num > length(imp.vec)){
    feat.num <- length(imp.vec);
  }
  
  # first get the top subset
  imp.vec <- rev(sort(imp.vec))[1:feat.num];
  
  # reverser the order for display
  imp.vec <- sort(imp.vec);
  
  # as data should already be normalized, use mean/median should be the same
  # mns is a list contains means of all vars at each level
  # conver the list into a matrix with each row contains var averages across different lvls
  mns <- by(mSetObj$dataSet$norm[, names(imp.vec)], mSetObj$dataSet$cls,
            function(x){ # inner function note, by send a subset of dataframe
              apply(x, 2, mean, trim=0.1)
            });
  mns <- t(matrix(unlist(mns), ncol=feat.num, byrow=TRUE));
  
  # vip.nms <-substr(names(imp.vec), 1, 12);
  vip.nms <- substr(names(imp.vec), 1, 14);
  names(imp.vec) <- NULL;
  
  # modified for B/W color
  dotcolor <- ifelse(color.BW, "darkgrey", "blue");
  dotchart(imp.vec, bg=dotcolor, xlab= xlbl, cex=1.3);
  
  mtext(side=2, at=1:feat.num, vip.nms, las=2, line=1)
  
  axis.lims <- par("usr"); # x1, x2, y1 ,y2
  
  # get character width
  shift <- 2*par("cxy")[1];
  lgd.x <- axis.lims[2] + shift;
  
  x <- rep(lgd.x, feat.num);
  y <- 1:feat.num;
  par(xpd=T);
  
  nc <- ncol(mns);
  
  # modified for B/W color
  colorpalette <- ifelse(color.BW, "Greys", "RdYlGn");
  col <- colorRampPalette(RColorBrewer::brewer.pal(10, colorpalette))(nc); # set colors for each class
  if(color.BW) col <- rev(col);
  
  # calculate background
  bg <- matrix("", nrow(mns), nc);
  for (m in 1:nrow(mns)){
    bg[m,] <- (col[nc:1])[rank(mns[m,])];
  }
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  cls.lbl <- levels(cls);
  
  for (n in 1:ncol(mns)){
    points(x,y, bty="n", pch=22, bg=bg[,n], cex=3);
    # now add label
    text(x[1], axis.lims[4], cls.lbl[n], srt=45, adj=c(0.2,0.5));
    # shift x, note, this is good for current size
    x <- x + shift/1.25;
  }
  
  # now add color key, padding with more intermediate colors for contiuous band
  col <- colorRampPalette(RColorBrewer::brewer.pal(25, colorpalette))(50)
  if(color.BW) col <- rev(col);
  
  nc <- length(col);
  x <- rep(x[1] + shift, nc);
  
  shifty <- (axis.lims[4]-axis.lims[3])/3;
  starty <- axis.lims[3] + shifty;
  endy <- axis.lims[3] + 2*shifty;
  y <- seq(from = starty, to = endy, length = nc);
  
  points(x,y, bty="n", pch=15, col=rev(col), cex=2);
  
  text(x[1], endy+shifty/8, "High");
  text(x[1], starty-shifty/8, "Low");
  
  par(op);
}

#'Plot PLS-DA classification performance using different components
#'@description Plot plsda classification performance using different components
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPLS.Classification <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- mSetObj$analSet$plsda$fit.info;
  colnames(res) <- 1:ncol(res);
  best.num <- mSetObj$analSet$plsda$best.num;
  choice <- mSetObj$analSet$plsda$choice;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*5/7;
  
  mSetObj$imgSet$pls.class <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,2,7)); # put legend on the right outside
  barplot(res, beside = TRUE, col = c("lightblue", "mistyrose","lightcyan"), ylim= c(0,1.05), xlab="Number of components", ylab="Performance");
  
  if(choice == "Q2"){
    text((best.num-1)*3 + best.num + 2.5, res[3,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
  }else if(choice == "R2"){
    text((best.num-1)*3 + best.num + 1.5, res[2,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
  }else{
    text((best.num-1)*3 + best.num + 0.5, res[1,best.num]+ 0.02, labels = "*", cex=2.5, col="red");
  }
  
  # calculate the maximum y position, each bar is 1, place one space between the group
  xpos <- ncol(res)*3 + ncol(res) + 1;
  legend(xpos, 1.0, rownames(res), fill = c("lightblue", "mistyrose","lightcyan"), xpd=T);
  dev.off();
  return(.set.mSet(mSetObj));
}


#'Plot PLS-DA classification performance using different components, permutation
#'@description Plot plsda classification performance using different components
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotPLS.Permutation <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  bw.vec <- mSetObj$analSet$plsda$permut;
  len <- length(bw.vec);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$pls.permut <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,2,4));
  hst <- hist(bw.vec, breaks = "FD", freq=T,
              ylab="Frequency", xlab= 'Permutation test statistics', col="lightblue", main="");
  
  # add the indicator using original label
  h <- max(hst$counts)
  arrows(bw.vec[1], h/5, bw.vec[1], 0, col="red", lwd=2);
  text(bw.vec[1], h/3.5, paste('Observed \n statistic \n', mSetObj$analSet$plsda$permut.p), xpd=T);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Perform OPLS-DA
#'@description Orthogonal PLS-DA (from ropls)
#'Add reg (regression i.e. if class order matters)
#'@param mSetObj Input name of the created mSet Object
#'@param reg Logical
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
OPLSR.Anal<-function(mSetObj=NA, reg=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$analSet$opls.reg <- reg;  
  
  # default options for feature labels on splot
  mSetObj$custom.cmpds <- c();
  mSetObj$analSet$oplsda$splot.type <- "all";
  
  if(reg==TRUE){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1); 
  
  if(.on.public.web){
    compiler::loadcmp("../../rscripts/metaboanalystr/stats_opls.Rc");    
  }
  my.res <- perform_opls(datmat, cls, predI=1, permI=0, orthoI=NA, crossvalI=cv.num);
  
  mSetObj$analSet$oplsda <- my.res;
  score.mat <- cbind(mSetObj$analSet$oplsda$scoreMN[,1], mSetObj$analSet$oplsda$orthoScoreMN[,1]);
  colnames(score.mat) <- c("Score (t1)","OrthoScore (to1)");
  write.csv(signif(score.mat,5), row.names=rownames(mSetObj$dataSet$norm), file="oplsda_score.csv");
  load.mat <- cbind(mSetObj$analSet$oplsda$loadingMN[,1], mSetObj$analSet$oplsda$orthoLoadingMN[,1]);
  colnames(load.mat) <- c("Loading (t1)","OrthoLoading (to1)");
  write.csv(signif(load.mat,5), file="oplsda_loadings.csv");
  
  return(.set.mSet(mSetObj));
}

#'Create OPLS-DA score plot
#'@description Orthogonal PLS-DA (from ropls) score plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param reg Numeric
#'@param show Show variable labels, 1 or O
#'@param grey.scale Numeric, indicate grey-scale, 0 for no, and 1 for yes 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotOPLS2DScore<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
    
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$opls.score2d <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,3));
  lv1 <- mSetObj$analSet$oplsda$scoreMN[,1];
  lv2 <- mSetObj$analSet$oplsda$orthoScoreMN[,1];
  xlabel <- paste("T score [1]", "(", round(100*mSetObj$analSet$oplsda$modelDF["p1", "R2X"],1), "%)");
  ylabel <- paste("Orthogonal T score [1]", "(", round(100*mSetObj$analSet$oplsda$modelDF["o1", "R2X"],1), "%)");
  
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long
  
  # obtain ellipse points to the scatter plot for each category
  lvs <- levels(mSetObj$dataSet$cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- mSetObj$dataSet$cls == lvs[i];
    groupVar <- var(cbind(lv1[inx],lv2[inx]), na.rm=T);
    groupMean <- cbind(mean(lv1[inx], na.rm=T),mean(lv2[inx], na.rm=T));
    pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
  }
  
  xrg <- range(lv1, pts.array[,1,]);
  yrg <- range(lv2, pts.array[,2,]);
  x.ext<-(xrg[2]-xrg[1])/12;
  y.ext<-(yrg[2]-yrg[1])/12;
  xlims<-c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims<-c(yrg[1]-y.ext, yrg[2]+y.ext);
  
  ## cols = as.numeric(dataSet$cls)+1;
  cols <- GetColorSchema(mSetObj, grey.scale==1);
  uniq.cols <- unique(cols);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  
  # make sure name and number of the same order DO NOT USE levels, which may be different
  legend.nm <- unique(as.character(mSetObj$dataSet$cls));
  ## uniq.cols <- unique(cols);
  
  if (length(uniq.cols) > 1 ) {
    names(uniq.cols) <- legend.nm;
  }
  # draw ellipse
  for(i in 1:length(lvs)){
    if ( length(uniq.cols) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.2), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.2), border=NA);
    }
    if(grey.scale) {
      lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
    }
  }
  
  pchs <- GetShapeSchema(mSetObj, show, grey.scale);
  if(grey.scale) {
    cols <- rep("black", length(cols));
  }
  if(show==1){ # display sample name set on
    text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75);
    points(lv1, lv2, pch=pchs, col=cols);
  }else{
    if (length(uniq.cols) == 1) {
      points(lv1, lv2, pch=pchs, col=cols, cex=1.0);
    } else {
      if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>=0))){
        points(lv1, lv2, pch=pchs, col=adjustcolor(cols, alpha.f = 0.4), cex=1.8);
      }else{
        points(lv1, lv2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2);
      }
    }
  }
  
  uniq.pchs <- unique(pchs);
  if(grey.scale) {
    uniq.cols <- "black";
  }
  legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Update OPLS loadings
#'@description Update the OPLS loadings
#'@param mSetObj Input name of the created mSet Object
#'@param plotType Set annotation type, "all" to label all variables and
#'"none" to label no variables.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

UpdateOPLS.Splot<- function(mSetObj=NA, plotType){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$oplsda$splot.type <- plotType;
  mSetObj$custom.cmpds <- c();
  return(.set.mSet(mSetObj));
}

#'S-plot for OPLS-DA
#'@description Orthogonal PLS-DA (from ropls) 
#'S-plot for important features from OPLS-DA
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotOPLS.Splot <- function(mSetObj=NA, imgName, plotType="all", format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  s <- as.matrix(mSetObj$dataSet$norm);
  T <- as.matrix(mSetObj$analSet$oplsda$scoreMN)
  p1 <- c()
  for (i in 1:ncol(s)) {
    scov <- cov(s[,i], T)
    p1 <- matrix(c(p1, scov), ncol=1)
  }
  pcorr1 <- c()
  for (i in 1:nrow(p1)) {
    den <- apply(T, 2, sd)*sd(s[,i])
    corr1 <- p1[i,]/den
    pcorr1 <- matrix(c(pcorr1, corr1), ncol=1)
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- h <- 8;
  }else if(width == 0){
    
  }else{
    w <- h <- width;
  }
  
  mSetObj$imgSet$opls.loading<-imgName;
  mSetObj$analSet$oplsda$splot.type <- plotType;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,4,7))
  plot(p1, pcorr1, type="n", xlab="p[1]", ylab ="p(corr)[1]", main = "Feature Importance");
  grid(col="lightgrey", lty=3, lwd = 1);
  points(p1, pcorr1, pch=19, col=adjustcolor("magenta", alpha.f = 0.4));
  opls.axis.lims <- par("usr");
  if(plotType=="all"){
    text(p1, pcorr1, labels=colnames(s), cex=0.8, pos=4, xpd=TRUE, col="blue");
  }else if(plotType == "custom"){
    if(length(mSetObj$custom.cmpds) > 0){
      hit.inx <- colnames(mSetObj$dataSet$norm) %in% mSetObj$custom.cmpds;
      text(p1[hit.inx], pcorr1[hit.inx], labels=colnames(s)[hit.inx], pos=4, xpd=TRUE, col="blue");
    }
  }else{
    # do nothing
  }
  dev.off();
  splot.mat <- cbind(jitter(p1),p1, pcorr1);
  rownames(splot.mat) <- colnames(s); 
  colnames(splot.mat) <- c("jitter", "p[1]","p(corr)[1]");
  write.csv(signif(splot.mat[,2:3],5), file="oplsda_splot.csv"); 
  mSetObj$analSet$oplsda$splot.mat <- splot.mat;
  mSetObj$analSet$oplsda$opls.axis.lims <- opls.axis.lims;   
  return(.set.mSet(mSetObj));
}

#'Plot loading compounds
#'@description Plot loading compounds
#'@param mSetObj Input name of the created mSet Object
#'@param cmpdNm Input the name of the selected compound
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@export
#'
PlotLoadingCmpd<-function(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # need to record the clicked compounds
  mSetObj$custom.cmpds <- c(mSetObj$custom.cmpds, cmpdNm);
  .set.mSet(mSetObj);
  
  return(PlotCmpdView(mSetObj, cmpdNm, format, dpi, width));
}

#'Plot OPLS 
#'@description Plot OPLS 
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@export
#'
PlotOPLS.MDL <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 9;
    
  }else{
    w <- width; 
  }
  h <- w*6/9;
  
  mSetObj$imgSet$opls.class <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  # the model R2Y and Q2Y
  par(mar=c(5,5,4,8)); # put legend on the right outside
  modBarDF <- mSetObj$analSet$oplsda$modelDF[!(rownames(mSetObj$analSet$oplsda$modelDF) %in% c("rot")), ];
  mod.dat <- data.matrix(modBarDF[!rownames(modBarDF)%in% ("sum"), c("R2X", "R2Y", "Q2")]);
  mod.dat <- t(mod.dat);
  bplt <- barplot(mod.dat,beside=TRUE, names.arg = colnames(mod.dat),xlab = "");
  axis(2, lwd.ticks=1);
  barplot(mod.dat,add = TRUE, beside = TRUE, col = c("lightblue", "mistyrose", "lavender"));
  text(x=bplt, y=mod.dat+max(mod.dat)/25, labels=as.character(mod.dat), xpd=TRUE)
  xpos <- nrow(mod.dat)*ncol(mod.dat) + ncol(mod.dat) + 0.5
  ypos <- max(mod.dat)/2;
  legend(xpos, ypos, legend = c("R2X", "R2Y", "Q2"), pch=15, col=c("lightblue", "mistyrose", "lavender"), xpd=T, bty="n");
  dev.off();
  
  write.csv(mod.dat, file="oplsda_model.csv");
  
  return(.set.mSet(mSetObj));
}

#'Perform OPLS-DA permutation
#'@description Orthogonal PLS-DA (from ropls) 
#'perform permutation, using training classification accuracy as
#'indicator, for two or multi-groups
#'@param mSetObj Input name of the created mSet Object
#'@param num Input the number of permutations, default is set to 100.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

OPLSDA.Permut<-function(mSetObj=NA, num=100){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(mSetObj$analSet$opls.reg){
    cls<-scale(as.numeric(mSetObj$dataSet$cls))[,1];
  }else{
    cls<-model.matrix(~mSetObj$dataSet$cls-1);
  }
  
  datmat <- as.matrix(mSetObj$dataSet$norm);
  cv.num <- min(7, dim(mSetObj$dataSet$norm)[1]-1); 
  
  if(.on.public.web){
    compiler::loadcmp("../../rscripts/metaboanalystr/stats_opls.Rc");    
  }
  my.res <- perform_opls(datmat,cls, predI=1, orthoI=NA, permI=num, crossvalI=cv.num);
  
  r.vec <- my.res$suppLs[["permMN"]][, "R2Y(cum)"];
  q.vec <- my.res$suppLs[["permMN"]][, "Q2(cum)"];
  
  # note, actual permutation number may be adjusted in public server
  perm.num <- my.res$suppLs[["permI"]];
  
  mSetObj$analSet$oplsda$perm.res <- list(r.vec=r.vec, q.vec=q.vec, perm.num=perm.num);
  return(.set.mSet(mSetObj));
}

#'Plot OPLS-DA permutation
#'@description Orthogonal PLS-DA (from ropls) 
#'perform permutation, using training classification accuracy as
#'indicator, for two or multi-groups
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotOPLS.Permutation<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  perm.res <- mSetObj$analSet$oplsda$perm.res;
  
  r.vec <- perm.res$r.vec;
  q.vec <- perm.res$q.vec;
  perm.num <- perm.res$perm.num;
  
  better.rhits <- sum(r.vec[-1]>=r.vec[1]);
  
  if(better.rhits == 0) {
    pr <- paste("p < ", 1/perm.num, " (", better.rhits, "/", perm.num, ")", sep="");
  }else{
    p <- better.rhits/perm.num;
    pr <- paste("p = ", signif(p, digits=5), " (", better.rhits, "/", perm.num, ")", sep="");
  }
  better.qhits <- sum(q.vec[-1]>=q.vec[1]);
  if(better.qhits == 0) {
    pq <- paste("p < ", 1/perm.num, " (", better.qhits, "/", perm.num, ")", sep="");
  }else{
    p <- better.qhits/perm.num;
    pq <- paste("p = ", signif(p, digits=5), " (", better.qhits, "/", perm.num, ")", sep="");
  }
  
  rng <- range(c(r.vec, q.vec, 1));
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width; 
  }
  h <- w*6/8;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,2,7));
  rhst <- hist(r.vec[-1], plot=FALSE);
  qhst <- hist(q.vec[-1], plot=FALSE);
  h <- max(c(rhst$counts, qhst$counts))+1;
  bin.size <- min(c(rhst$breaks[2]-rhst$breaks[1], qhst$breaks[2]-qhst$breaks[1]));
  rbins <- seq(min(rhst$breaks),max(rhst$breaks),bin.size);
  qbins <- seq(min(qhst$breaks),max(qhst$breaks),bin.size);
  hist(r.vec[-1], xlim=rng, ylim=c(0, h), breaks=rbins, border=F, ylab="Frequency", xlab= 'Permutations', 
       col=adjustcolor("lightblue", alpha=0.6), main="");
  hist(q.vec[-1], add=TRUE,breaks=qbins, border=F, col=adjustcolor("mistyrose", alpha=0.6));
  
  arrows(r.vec[1], h/3, r.vec[1], 0, length=0.1,angle=30,lwd=2);
  text(r.vec[1], h/2.5, paste('R2Y:', r.vec[1], "\n", pr), xpd=TRUE);
  
  arrows(q.vec[1], h/2, q.vec[1], 0, length=0.1,angle=30,lwd=2);
  text(q.vec[1], h/1.8, paste('Q2:', q.vec[1], "\n", pq), xpd=TRUE);
  
  legend(1, h/3, legend = c("Perm R2Y", "Perm Q2"), pch=15, col=c("lightblue", "mistyrose"), xpd=T, bty="n");
  
  dev.off();
  
  mSetObj$imgSet$opls.permut <- imgName;
  
  msg <- paste("Empirical p-values Q2: ", pq, " and R2Y: ", pr);
  if(.on.public.web){
    .set.mSet(mSetObj)
    return(msg);
  }else{
    print(msg);
    return(.set.mSet(mSetObj));
  }
}

#'Perform SPLS-DA
#'@description Sparse PLS-DA (from mixOmics) 
#'@param mSetObj Input name of the created mSet Object
#'@param comp.num Input the number of computations to run 
#'@param var.num Input the number of variables
#'@param compVarOpt Input the option to perform SPLS-DA
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

SPLSR.Anal <- function(mSetObj=NA, comp.num, var.num, compVarOpt, validOpt="Mfold"){
  
  if(compVarOpt == "same"){
    comp.var.nums <- rep(var.num, comp.num);
  }else{
    if(exists("comp.var.nums") && all(comp.var.nums > 0)){
      comp.var.nums <- ceiling(comp.var.nums);
    }else{
      msg <- c("All values need to be positive integers!");
      return(0);
    }
  }
  
  mSetObj <- .get.mSet(mSetObj);  
  
  # note, standardize the cls, to minimize the impact of categorical to numerical impact
  cls <- scale(as.numeric(mSetObj$dataSet$cls))[,1];
  datmat <- as.matrix(mSetObj$dataSet$norm);
  if(.on.public.web){
    compiler::loadcmp("../../rscripts/metaboanalystr/stats_spls.Rc");    
  }
  my.res <- splsda(datmat, cls, ncomp=comp.num, keepX=comp.var.nums);
  # perform validation
  perf.res <- perf.splsda(my.res, dist= "centroids.dist", validation=validOpt, folds = 5);
  my.res$error.rate <- perf.res$error.rate$overall;
  
  mSetObj$analSet$splsr <- my.res;
  score.mat <- mSetObj$analSet$splsr$variates$X;
  write.csv(signif(score.mat,5), row.names=rownames(mSetObj$dataSet$norm), file="splsda_score.csv");
  load.mat <- score.mat <- mSetObj$analSet$splsr$loadings$X;
  write.csv(signif(load.mat,5), file="splsda_loadings.csv");
  return(.set.mSet(mSetObj));
}

#'Plot SPLS-DA
#'@description Sparse PLS-DA (from mixOmics) pairwise summary plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param pc.num Numeric, indicate the number of principle components
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSPLSPairSummary<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pc.num){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$spls.pair <- imgName;
  
  if(pc.num > mSetObj$analSet$splsr$ncomp){
    pc.num <- mSetObj$analSet$splsr$ncomp;
  }
  vars <- round(100*mSetObj$analSet$splsr$explained_variance$X,1);
  my.data <- mSetObj$analSet$splsr$variates$X[,1:pc.num];
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  pclabels <- paste("Component", 1:pc.num, "\n", vars, "%");
  ellipse::pairs(my.data, col=GetColorSchema(mSetObj), pch=as.numeric(mSetObj$dataSet$cls)+1, labels=pclabels)
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Score Plot SPLS-DA
#'@description Sparse PLS-DA (from mixOmics) score plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param reg Numeric, between 1 and 0
#'@param show Numeric, 1 or 0
#'@param grey.scale Numeric, use grey-scale, 0 for no, and 1 for yes. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSPLS2DScore <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx1, inx2, reg=0.95, show=1, grey.scale=0){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$spls.score2d <- imgName;
  
  lv1 <- mSetObj$analSet$splsr$variates$X[,inx1];
  lv2 <- mSetObj$analSet$splsr$variates$X[,inx2];
  xlabel <- paste("Component", inx1, "(", round(100*mSetObj$analSet$splsr$explained_variance$X[inx1],1), "%)");
  ylabel <- paste("Component", inx2, "(", round(100*mSetObj$analSet$splsr$explained_variance$X[inx2],1), "%)");
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,3));
  text.lbls <- substr(rownames(mSetObj$dataSet$norm),1,12) # some names may be too long
  
  # obtain ellipse points to the scatter plot for each category
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  lvs <- levels(cls);
  pts.array <- array(0, dim=c(100,2,length(lvs)));
  for(i in 1:length(lvs)){
    inx <- mSetObj$dataSet$cls == lvs[i];
    groupVar <- var(cbind(lv1[inx],lv2[inx]), na.rm=T);
    groupMean <- cbind(mean(lv1[inx], na.rm=T),mean(lv2[inx], na.rm=T));
    pts.array[,,i] <- ellipse::ellipse(groupVar, centre = groupMean, level = reg, npoints=100);
  }
  
  xrg <- range(lv1, pts.array[,1,]);
  yrg <- range(lv2, pts.array[,2,]);
  x.ext <- (xrg[2]-xrg[1])/12;
  y.ext <- (yrg[2]-yrg[1])/12;
  xlims <- c(xrg[1]-x.ext, xrg[2]+x.ext);
  ylims <- c(yrg[1]-y.ext, yrg[2]+y.ext);
  
  ## cols = as.numeric(dataSet$cls)+1;
  cols <- GetColorSchema(mSetObj, grey.scale==1);
  uniq.cols <- unique(cols);
  
  plot(lv1, lv2, xlab=xlabel, xlim=xlims, ylim=ylims, ylab=ylabel, type='n', main="Scores Plot");
  grid(col = "lightgray", lty = "dotted", lwd = 1);
  
  # make sure name and number of the same order DO NOT USE levels, which may be different
  legend.nm <- unique(as.character(sort(cls)));
  ## uniq.cols <- unique(cols);
  
  ## BHAN: when same color is choosen for black/white; it makes an error
  # names(uniq.cols) <- legend.nm;
  if (length(uniq.cols) > 1) {
    names(uniq.cols) <- legend.nm;
  }
  # draw ellipse
  for(i in 1:length(lvs)){
    if (length(uniq.cols) > 1) {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols[lvs[i]], alpha=0.25), border=NA);
    } else {
      polygon(pts.array[,,i], col=adjustcolor(uniq.cols, alpha=0.25), border=NA);
    }
    if(grey.scale) {
      lines(pts.array[,,i], col=adjustcolor("black", alpha=0.5), lty=2);
    }
  }
  
  pchs <- GetShapeSchema(mSetObj, show, grey.scale);
  if(grey.scale) {
    cols <- rep("black", length(cols));
  }
  if(show==1){ # display sample name set on
    text(lv1, lv2, label=text.lbls, pos=4, xpd=T, cex=0.75);
    points(lv1, lv2, pch=pchs, col=cols);
  }else{
    if (length(uniq.cols) == 1) {
      points(lv1, lv2, pch=pchs, col=cols, cex=1.0);
    } else {
      if(grey.scale == 1 | (exists("shapeVec") && all(shapeVec>=0))){
        points(lv1, lv2, pch=pchs, col=adjustcolor(cols, alpha.f = 0.4), cex=1.8);
      }else{
        points(lv1, lv2, pch=21, bg=adjustcolor(cols, alpha.f = 0.4), cex=2);
      }
    }
  }
  
  uniq.pchs <- unique(pchs);
  if(grey.scale) {
    uniq.cols <- "black";
  }
  legend("topright", legend = legend.nm, pch=uniq.pchs, col=uniq.cols);
  
  dev.off();
  return(.set.mSet(mSetObj));
}

#'3D SPLS-DA score plot
#'@description Sparse PLS-DA (from mixOmics) 3D score plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param inx1 Numeric, indicate the number of the principal component for the x-axis of the loading plot.
#'@param inx2 Numeric, indicate the number of the principal component for the y-axis of the loading plot.
#'@param inx3 Numeric, indicate the number of the principal component for the z-axis of the loading plot.
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSPLS3DScore <- function(mSetObj=NA, imgName, format="json", inx1=1, inx2=2, inx3=3){
  
  mSetObj <- .get.mSet(mSetObj);
  
  spls3d <- list();
  # need to check if only two components are generated
  if(length(mSetObj$analSet$splsr$explained_variance$X)==2){
    spls3d$score$axis <- paste("Component", c(inx1, inx2), " (", round(100*mSetObj$analSet$splsr$explained_variance$X[c(inx1, inx2)], 1), "%)", sep="");    
    coords <- data.frame(t(signif(mSetObj$analSet$splsr$variates$X[,c(inx1, inx2)], 5)));
    spls3d$score$axis <- c(spls3d$score$axis, "Component3 (NA)");
    coords <- rbind(coords, "comp 3"=rep (0, ncol(coords)));
  }else{
    spls3d$score$axis <- paste("Component", c(inx1, inx2, inx3), " (", round(100*mSetObj$analSet$splsr$explained_variance$X[c(inx1, inx2, inx3)], 1), "%)", sep="");    
    coords <- data.frame(t(signif(mSetObj$analSet$splsr$variates$X[,c(inx1, inx2, inx3)], 5)));
  }
  colnames(coords) <- NULL; 
  spls3d$score$xyz <- coords;
  spls3d$score$name <- rownames(mSetObj$dataSet$norm);
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  spls3d$score$facA <- cls;
  
  # now set color for each group
  cols <- unique(GetColorSchema(mSetObj));
  rgbcols <- col2rgb(cols);
  cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
  spls3d$score$colors <- cols;
  
  imgName = paste(imgName, ".", format, sep="");
  json.obj <- RJSONIO::toJSON(spls3d, .na='null');
  sink(imgName);
  cat(json.obj);
  sink();
  mSetObj$imgSet$spls.score3d <- imgName;
  return(.set.mSet(mSetObj));
}

#'@export
PlotSPLS3DLoading <- function(mSetObj=NA, imgName, format="json", inx1, inx2, inx3){
  mSetObj <- .get.mSet(mSetObj);
  spls = mSetObj$analSet$splsr
  spls3d <- list();
  
  if(length(mSetObj$analSet$splsr$explained_variance$X)==2){
    spls3d$loading$axis <- paste("Loading ", c(inx1, inx2), sep="");    
    coords <- data.frame(t(signif(mSetObj$analSet$splsr$loadings$X[,c(inx1, inx2)], 5)));
    spls3d$loading$axis <- c(spls3d$loading$axis, "Loading 3");
    coords <- rbind(coords, "comp 3"=rep (0, ncol(coords)));
  }else{
    spls3d$loading$axis <- paste("Loading ", c(inx1, inx2, inx3), sep="");    
    coords <- data.frame(t(signif(mSetObj$analSet$splsr$loadings$X[,c(inx1, inx2, inx3)], 5)));
  }
  
  colnames(coords) <- NULL; 
  spls3d$loading$xyz <- coords;
  spls3d$loading$name <- rownames(spls$loadings$X);
  spls3d$loading$entrez <-rownames(spls$loadings$X); 
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  }else{
    cls <- as.character(mSetObj$dataSet$cls);
  }
  
  if(all.numeric(cls)){
    cls <- paste("Group", cls);
  }
  
  spls3d$cls = cls;
  # see if there is secondary
  
  require(RJSONIO);
  imgName = paste(imgName, ".", format, sep="");
  json.mat <- RJSONIO::toJSON(spls3d, .na='null');
  sink(imgName);
  cat(json.mat);
  sink();
  current.msg <<- "Annotated data is now ready for PCA 3D visualization!";
  
  if(.on.public.web){
    return(1);
  }else{
    return(.set.mSet(mSetObj));
  }
  
}


#'Create SPLS-DA loading plot
#'@description Sparse PLS-DA (from mixOmics) loading plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param inx Input the model index
#'@param viewOpt Detailed view "detail" 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSPLSLoading <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, inx, viewOpt="detail"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imp.vec <- abs(mSetObj$analSet$splsr$loadings$X[,inx]);
  imp.vec <- imp.vec[imp.vec > 0];
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
    
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$spls.imp <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  PlotImpVar(mSetObj, imp.vec, paste ("Loadings", inx), 999, FALSE);
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Create SPLS-DA classification plot
#'@description Sparse PLS-DA (from mixOmics) plot of 
#'classification performance using different components
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param validOpt "Mfold"
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import caret
PlotSPLSDA.Classification <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  res <- mSetObj$analSet$splsr$error.rate;
  
  edge <- (max(res)-min(res))/100; # expand y uplimit for text
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$splsda.class <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(res, type='l', xlab='Number of Components', ylab='Error Rate',
       ylim = c(min(res)-5*edge, max(res)+18*edge), axes=F,
       main="Sparse PLS-DA Classification Error Rates")
  text(res, labels = paste(100*round(res,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  axis(2);
  axis(1, 1:length(res), names(res));
  dev.off();
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

# get which number of components give best performance
GetPLSBestTune<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$analSet$plsda$best.num)){
    return(0);
  }
  mSetObj$analSet$plsda$best.num;
}

# obtain VIP score
GetPLSSigMat<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    return(CleanNumber(signif(as.matrix(mSetObj$analSet$plsda$vip.mat),5)));
  }else if(type == "coef"){
    return(CleanNumber(signif(as.matrix(mSetObj$analSet$plsda$coef.mat),5)));
  }else{
    return(CleanNumber(signif(as.matrix(mSetObj$analSet$plsr$imp.loads),5)));
  }
}

GetPLSSigRowNames<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    return(rownames(mSetObj$analSet$plsda$vip.mat));
  }else if(type == "coef"){
    return(rownames(mSetObj$analSet$plsda$coef.mat));
  }else{
    return(rownames(mSetObj$analSet$plsr$imp.loads))
  }
}

GetPLSSigColNames<-function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    return(colnames(mSetObj$analSet$plsda$vip.mat));
  }else if(type == "coef"){
    return(colnames(mSetObj$analSet$plsda$coef.mat));
  }else{
    return(colnames(mSetObj$analSet$plsr$imp.loads));
  }
}

GetPLS_CVRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$plsda$fit.info);
}

GetPLS_CVColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$plsda$fit.info);
}

GetPLS_CVMat<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(signif(mSetObj$analSet$plsda$fit.info, 5));
}

GetMaxPLSPairComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(dim(mSetObj$dataSet$norm)[1]-1, dim(mSetObj$dataSet$norm)[2]));
}

GetMaxPLSCVComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(dim(mSetObj$dataSet$norm)[1]-2, dim(mSetObj$dataSet$norm)[2]));
}

GetDefaultPLSPairComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(5, dim(mSetObj$dataSet$norm)[1]-1, dim(mSetObj$dataSet$norm)[2]));
}

GetDefaultPLSCVComp<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(5, dim(mSetObj$dataSet$norm)[1]-2, dim(mSetObj$dataSet$norm)[2], mSetObj$dataSet$min.grp.size));
}

GetPLSLoadAxesSpec<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$pls.axis.lims;
}

GetPLSLoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$plsr$load.x.uniq);
}

GetPLSLoadCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$plsr$load.x.uniq;
}

GetPLSLoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(cbind(mSetObj$analSet$plsr$load.x.uniq, mSetObj$analSet$plsr$imp.loads[,2]));
}

GetPCALoadAxesSpec <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$pca.axis.lims;
}

GetPCALoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$pca$load.x.uniq);
}

GetPCALoadCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$pca$load.x.uniq;
}

GetPCALoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(cbind(mSetObj$analSet$pca$load.x.uniq, mSetObj$analSet$pca$imp.loads[,2]));
}

#'For plotting PCA, selects max top 9 components
#'@description Rotate PCA analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
GetMaxPCAComp <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(min(9, dim(mSetObj$dataSet$norm)-1));
}

GetOPLSLoadAxesSpec <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$oplsda$opls.axis.lims);
}

GetOPLSLoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$oplsda$splot.mat);
}

GetOPLSLoadColNames <- function(mSetObj=NA){
  return(c("p[1]","p(corr)[1]"));
}

GetOPLSLoadCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$oplsda$splot.mat[,1];
}

GetOPLSLoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(mSetObj$analSet$oplsda$splot.mat[,c(1,3)]);
}

GetDefaultSPLSCVComp <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return (min(5, dim(mSetObj$dataSet$norm)[1]-2, dim(mSetObj$dataSet$norm)[2], mSetObj$dataSet$min.grp.size));
}

GetDefaultSPLSPairComp <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return (min(5, dim(mSetObj$dataSet$norm)[1]-1, dim(mSetObj$dataSet$norm)[2]));
}

GetSPLS_CVRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$splsda$fit.info);
}

GetSPLS_CVColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$splsda$fit.info);
}

GetSPLS_CVMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(signif(mSetObj$analSet$splsda$fit.info, 5));
}

GetSPLSLoadAxesSpec <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$pls.axis.lims;
}

GetSPLSLoadCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$splsr$loadings$X);
}

GetSPLSLoadCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$splsr$load.x.uniq;
}

GetSPLSLoadMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  as.matrix(signif(mSetObj$analSet$splsr$loadings$X, 5));
}

GetSPLSSigColNames <- function(mSetObj=NA, type){
  mSetObj <- .get.mSet(mSetObj);
  if(type == "vip"){
    return (colnames(mSetObj$analSet$splsda$vip.mat));
  }else if(type == "coef"){
    return (colnames(mSetObj$analSet$splsda$coef.mat));
  }else{
    return (colnames(mSetObj$analSet$splsr$loadings$X));
  }
}

#'Fold change analysis, unpaired
#'@description Perform fold change analysis, method can be mean or median
#'@usage FC.Anal.unpaired(mSetObj, fc.thresh=2, cmp.type = 0)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fc.thresh Fold-change threshold, numeric input
#'@param cmp.type Comparison type, 0 for group 1 minus group 2, and 1 for group 
#'1 minus group 2
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
FC.Anal.unpaired <- function(mSetObj=NA, fc.thresh=2, cmp.type = 0){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # make sure threshold is above 1
  fc.thresh = ifelse(fc.thresh>1, fc.thresh, 1/fc.thresh);
  max.thresh = fc.thresh;
  min.thresh = 1/fc.thresh;
  
  res <- GetFC(mSetObj, F, cmp.type);
  fc.all <- res$fc.all;
  fc.log <- res$fc.log;
  
  imp.inx <- fc.all > max.thresh | fc.all < min.thresh;
  sig.mat <- cbind(fc.all[imp.inx, drop=F], fc.log[imp.inx, drop=F]);
  colnames(sig.mat) <- c("Fold Change", "log2(FC)");
  
  # order by absolute log value (since symmetrical in pos and neg)
  inx.ord <- order(abs(sig.mat[,2]), decreasing=T);
  sig.mat <- sig.mat[inx.ord,,drop=F];
  
  fileName <- "fold_change.csv";
  write.csv(sig.mat,file=fileName);
  
  # create a list object to store fc
  mSetObj$analSet$fc<-list (
    paired = FALSE,
    raw.thresh = fc.thresh,
    max.thresh = max.thresh,
    min.thresh = min.thresh,
    fc.all = fc.all, # note a vector
    fc.log = fc.log,
    inx.imp = imp.inx,
    sig.mat = sig.mat
  );
  return(.set.mSet(mSetObj));
}

#'Fold change analysis, paired
#'@description Perform paired fold change analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param fc.thresh Fold-change threshold, numeric input
#'@param percent.thresh Numeric input, from 0 to 1 to indicate the significant count threshold
#'@param cmp.type Comparison type, 0 for group 1 minus group 2, and 1 for group 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
FC.Anal.paired <- function(mSetObj=NA, fc.thresh=2, percent.thresh=0.75, cmp.type=0){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # make sure threshold is above 1
  fc.thresh = ifelse(fc.thresh>1, fc.thresh, 1/fc.thresh);
  max.thresh = fc.thresh;
  min.thresh = 1/fc.thresh;
  
  fc.mat <- GetFC(mSetObj, T, cmp.type);
  
  count.thresh <- round(nrow(mSetObj$dataSet$norm)/2*percent.thresh);
  mat.up <- fc.mat >= log(max.thresh,2);
  mat.down <- fc.mat <= log(min.thresh,2);
  
  count.up <- apply(mat.up, 2, sum);
  count.down <- apply(mat.down, 2, sum);
  fc.all <- rbind(count.up, count.down);
  
  inx.up <- count.up>=count.thresh;
  inx.down <- count.down>=count.thresh;
  
  colnames(fc.all) <- colnames(mSetObj$dataSet$norm);
  rownames(fc.all) <- c("Count (up)", "Count (down)");
  sig.var <- t(fc.all[,(inx.up|inx.down), drop=F]);
  
  # sort sig.var using absolute difference between count(up)-count(down)
  sig.dff <- abs(sig.var[,1]-sig.var[,2])
  inx <- order(sig.dff, decreasing=T);
  sig.var <- sig.var[inx,,drop=F];
  
  fileName <- "fold_change.csv";
  write.csv(signif(sig.var,5),file=fileName);
  
  # create a list object to store fc
  mSetObj$analSet$fc <-list (
    paired = TRUE,
    fc.mat = fc.mat,
    raw.thresh = fc.thresh,
    max.thresh = count.thresh,
    min.thresh = -count.thresh,
    fc.all = fc.all, # note: a 2-row matrix!
    inx.up = inx.up,
    inx.down = inx.down,
    sig.mat = sig.var
  );
  return(.set.mSet(mSetObj));
}

#'Plot fold change 
#'@description Plot fold change analysis
#'@usage PlotFC(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotFC <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$fc <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  par(mar=c(5,5,2,3));
  
  fc = mSetObj$analSet$fc;
  if(fc$paired){
    ylim <- c(-nrow(mSetObj$dataSet$norm)/2, nrow(mSetObj$dataSet$norm)/2);
    xlim <- c(0, ncol(mSetObj$dataSet$norm));
    plot(NULL, xlim=xlim, ylim=ylim, xlab = GetVariableLabel(mSetObj$dataSet$type),
         ylab=paste("Count with FC >=", fc$max.thresh, "or <=", fc$min.thresh));
    for(i in 1:ncol(fc$fc.all)){
      segments(i,0, i, fc$fc.all[1,i], col= ifelse(fc$inx.up[i],"magenta", "darkgrey"),
               lwd= ifelse(fc$inx.up[i], 2, 1));
      segments(i,0, i, -fc$fc.all[2,i], col= ifelse(fc$inx.down[i], "magenta", "darkgrey"),
               lwd= ifelse(fc$inx.down[i], 2, 1));
    }
    abline(h=fc$max.thresh, lty=3);
    abline(h=fc$min.thresh, lty=3);
    abline(h=0, lwd=1);
  }else{
    if(fc$raw.thresh > 0){
      # be symmetrical
      topVal <- max(abs(fc$fc.log));
      ylim <- c(-topVal, topVal);
      plot(fc$fc.log,  ylab="Log2 (FC)", ylim = ylim, xlab = GetVariableLabel(mSetObj$dataSet$type), pch=19, axes=F,
           col= ifelse(fc$inx.imp, "magenta", "darkgrey"));
      axis(2);
      axis(4); # added by Beomsoo
      abline(h=log(fc$max.thresh,2), lty=3);
      abline(h=log(fc$min.thresh,2), lty=3);
      abline(h=0, lwd=1);
    }else{ # plot side by side
      
      dat1 <- mSetObj$dataSet$norm[as.numeric(mSetObj$dataSet$cls) == 1, ];
      dat2 <- mSetObj$dataSet$norm[as.numeric(mSetObj$dataSet$cls) == 2, ];
      
      mns1 <- apply(dat1, 2, mean);
      mn1 <- mean(mns1);
      sd1 <- sd(mns1);
      msd1.top <- mn1 + 2*sd1;
      msd1.low <- mn1 - 2*sd1;
      
      mns2 <- apply(dat2, 2, mean);
      mn2 <- mean(mns2);
      sd2 <- sd(mns2);
      msd2.top <- mn2 + 2*sd2;
      msd2.low <- mn2 - 2*sd2;
      
      ylims <- range(c(mns1, mns2, msd1.top, msd2.top, msd1.low, msd2.low));
      new.mns <- c(mns1, rep(NA, 5), mns2);
      cols <- c(rep("magenta", length(mns1)), rep(NA, 5), rep("blue", length(mns2)));
      pchs <- c(rep(15, length(mns1)), rep(NA, 5), rep(19, length(mns2)));
      plot(new.mns, ylim=ylims, pch = pchs, col = cols, cex = 1.25, axes=F, ylab="");
      axis(2);
      axis(4); # added by Beomsoo
      abline(h=mn1, col="magenta", lty=3, lwd=2);
      abline(h=msd1.low, col="magenta", lty=3, lwd=1);
      abline(h=msd1.top, col="magenta", lty=3, lwd=1);
      abline(h=mn2, col="blue", lty=3, lwd=2);
      abline(h=msd2.low, col="blue", lty=3, lwd=1);
      abline(h=msd2.top, col="blue", lty=3, lwd=1);
      # abline(h=mean(all.mns), col="darkgrey", lty=3);
      axis(1, at=1:length(new.mns), labels=c(1:length(mns1),rep(NA, 5),1:length(mns2)));
    }
  }
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Used by higher functions to calculate fold change 
#'@description Utility method to calculate FC, used in higher function
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param paired Logical, true of false
#'@param cmpType Numeric, 0 or 1
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
GetFC <- function(mSetObj=NA, paired=FALSE, cmpType){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(paired){
    if(mSetObj$dataSet$combined.method){
      data <- mSetObj$dataSet$norm;
    }else{
      data <- log(mSetObj$dataSet$row.norm,2);
    }
    
    G1 <- data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), ]
    G2 <- data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), ]
    
    if(cmpType == 0){
      fc.mat <- G1-G2;
    }else{
      fc.mat <- G2-G1;
    }
    return (fc.mat);
  }else{
    if(mSetObj$dataSet$combined.method){
      data <- mSetObj$dataSet$norm;
      m1 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), ]);
      m2 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), ]);
      
      # create a named matrix of sig vars for display
      if(cmpType == 0){
        fc.log <- signif (m1-m2, 5);
      }else{
        fc.log <- signif (m2-m1, 5);
      }
      fc.all <- signif(2^fc.log, 5);
    }else{
      data <- mSetObj$dataSet$row.norm;
      m1 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]), ]);
      m2 <- colMeans(data[which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]), ]);
      
      # create a named matrix of sig vars for display
      if(cmpType == 0){
        ratio <- m1/m2;
      }else{
        ratio <- m2/m1;
      }
      fc.all <- signif(ratio, 5);
      fc.log <- signif(log2(ratio), 5);
    }
    
    if(mSetObj$dataSet$combined.method){
      names(fc.all) <- names(fc.log) <- colnames(mSetObj$dataSet$norm);  
    }else{
      names(fc.all) <- names(fc.log) <- colnames(mSetObj$dataSet$row.norm) # make even vectors
    }
    return(list(fc.all = fc.all, fc.log = fc.log));
  }
}

#'Perform t-test analysis
#'@description This function is used to perform t-test analysis.
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param nonpar Logical, use a non-parametric test, T or F. False is default. 
#'@param threshp Numeric, enter the adjusted p-value (FDR) cutoff
#'@param paired Logical, is data paired (T) or not (F).
#'@param equal.var Logical, evaluates if the group variance is equal (T) or not (F). 
#'@param all_results Logical, if TRUE, returns T-Test analysis results
#'for all compounds. 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Ttests.Anal <- function(mSetObj=NA, nonpar=F, threshp=0.05, paired=FALSE, equal.var=TRUE, all_results=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web & !nonpar & RequireFastUnivTests(mSetObj)){
    res <- PerformFastUnivTests(mSetObj$dataSet$norm, mSetObj$dataSet$cls, var.equal=equal.var);
  }else{
    res <- GetTtestRes(mSetObj, paired, equal.var, nonpar);
  }
  t.stat <- res[,1];
  p.value <- res[,2];
  names(t.stat) <- names(p.value) <- colnames(mSetObj$dataSet$norm);
  
  p.log <- -log10(p.value);
  fdr.p <- p.adjust(p.value, "fdr");
  
  if(all_results==TRUE){
    
    all.mat <- data.frame(signif(t.stat,5), signif(p.value,5), signif(p.log,5), signif(fdr.p,5));
    
    if(nonpar){
      tt.nm = "Wilcoxon Rank Test";  
      file.nm <- "wilcox_rank_all.csv"
      colnames(all.mat) <- c("V", "p.value", "-log10(p)", "FDR");
    }else{
      tt.nm = "T-Tests";
      file.nm <- "t_test_all.csv";
      colnames(all.mat) <- c("t.stat", "p.value", "-log10(p)", "FDR");
    }
    write.csv(all.mat, file=file.nm);
  }
  
  inx.imp <- fdr.p <= threshp;
  # if there is no sig cmpds, it will be errors, need to improve
  
  AddMsg(paste("A total of", sum(inx.imp), "significant features were found."));
  sig.num <- sum(inx.imp);
  
  if(sig.num > 0){
    sig.t <- t.stat[inx.imp];
    sig.p <- p.value[inx.imp];
    lod<- -log10(sig.p);
    sig.q <-fdr.p[inx.imp];
    
    sig.mat <- cbind(sig.t, sig.p, lod, sig.q);
    colnames(sig.mat) <- c("t.stat", "p.value", "-log10(p)", "FDR");
    ord.inx <- order(sig.p);
    sig.mat <- sig.mat[ord.inx,,drop=F];
    sig.mat <- signif(sig.mat, 5);
    
    if(nonpar){
      tt.nm = "Wilcoxon Rank Test";  
      file.nm <- "wilcox_rank.csv"
      colnames(sig.mat) <- c("V", "p.value", "-log10(p)", "FDR");
    }else{
      tt.nm = "T-Tests";
      file.nm <- "t_test.csv";
      colnames(sig.mat) <- c("t.stat", "p.value", "-log10(p)", "FDR");
    }
    write.csv(sig.mat, file=file.nm);
    
    tt <- list (
      tt.nm = tt.nm,
      sig.nm = file.nm,
      sig.num = sig.num,
      paired = paired,
      raw.thresh = threshp,
      t.score = sort(t.stat),
      p.value = sort(p.value),
      p.log = p.log,
      thresh = -log10(threshp), # only used for plot threshold line
      inx.imp = inx.imp,
      sig.mat = sig.mat
    );
  }else{
    tt <- list (
      sig.num = sig.num,
      paired = paired,
      raw.thresh = threshp,
      t.score = sort(t.stat),
      p.value = sort(p.value),
      p.log = p.log,
      thresh = -log10(threshp), # only used for plot threshold line
      inx.imp = inx.imp
    );
  }
  
  mSetObj$analSet$tt <- tt;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(sig.num);
  }
  return(.set.mSet(mSetObj));
}

#'Plot t-test 
#'@description Plot t-test
#'@usage PlotTT(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotTT <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 8;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/8;
  
  mSetObj$imgSet$tt <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(mSetObj$analSet$tt$p.log, ylab="-log10(p)", xlab=GetVariableLabel(mSetObj$dataSet$type), main=mSetObj$analSet$tt$tt.nm, pch=19,
       col= ifelse(mSetObj$analSet$tt$inx.imp, "magenta", "darkgrey"));
  abline(h=mSetObj$analSet$tt$thresh, lty=3);
  axis(4); 
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Perform Volcano Analysis
#'@description Perform volcano analysis
#'@usage Volcano.Anal(mSetObj=NA, paired=FALSE, fcthresh, cmpType, percent.thresh, nonpar=F, threshp, equal.var=TRUE, pval.type="raw")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param paired Logical, T if data is paired, F if data is not.
#'@param fcthresh Numeric, input the fold change threshold
#'@param cmpType Comparison type, 1 indicates group 1 vs group 2, and 2 indicates group 2 vs group 1
#'@param percent.thresh Only for paired data, numeric, indicate the significant count threshold 
#'@param nonpar Logical, indicate if a non-parametric test should be used (T or F)
#'@param threshp Numeric, indicate the p-value threshold
#'@param equal.var Logical, indicates if the group variance is equal (T) or unequal (F)
#'@param pval.type To indicate raw p-values, use "raw". To indicate FDR-adjusted p-values, use "fdr".  
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Volcano.Anal <- function(mSetObj=NA, paired=FALSE, fcthresh, cmpType, percent.thresh, nonpar=F, threshp, equal.var=TRUE, pval.type="raw"){
  
  mSetObj <- .get.mSet(mSetObj);
  
  #### t-tests
  # check to see if already done by microservice
  if(.on.public.web & !nonpar & RequireFastUnivTests(mSetObj)){
    res <- PerformFastUnivTests(mSetObj$dataSet$norm, mSetObj$dataSet$cls, var.equal=equal.var);
  }else{
    res <- GetTtestRes(mSetObj, paired, equal.var, nonpar);
  }
  p.value <- res[,2];
  if(pval.type == "fdr"){
    p.value <- p.adjust(p.value, "fdr");
  }   
  inx.p <- p.value <= threshp;
  p.log <- -log10(p.value);
  
  ### fold change analysis
  # make sure threshold is above 1
  fcthresh = ifelse(fcthresh>1, fcthresh, 1/fcthresh);
  max.xthresh <- log(fcthresh,2);
  min.xthresh <- log(1/fcthresh,2);
  
  res <- GetFC(mSetObj, paired, cmpType);
  
  # create a named matrix of sig vars for display
  fc.log <- res$fc.log;
  fc.all <- res$fc.all;
  
  inx.up <- fc.log > max.xthresh;
  inx.down <- fc.log < min.xthresh;
  
  if(paired){
    count.thresh<-round(nrow(mSetObj$dataSet$norm)/2*percent.thresh);
    mat.up <- res >= max.xthresh;
    mat.down <- res <= min.xthresh;
    
    count.up <- apply(mat.up, 2, sum);
    count.down <- apply(mat.down, 2, sum);
    fc.all <- rbind(count.up, count.down);
    
    inx.up <- count.up>=count.thresh;
    inx.down <- count.down>=count.thresh;
    
    colnames(fc.all) <- colnames(mSetObj$dataSet$norm);
    rownames(fc.all) <- c("Count (up)", "Count (down)");
    
    # replace the count.thresh for plot
    max.xthresh <- count.thresh;
    min.xthresh <- -count.thresh;
  }
  
  # create named sig table for display
  inx.imp <- (inx.up | inx.down) & inx.p;
  if(paired){ 
    sig.var <- cbind(fc.all[1,][inx.imp,drop=F], fc.all[2,][inx.imp, drop=F], p.value[inx.imp, drop=F], p.log[inx.imp, drop=F]);
    if(pval.type == "fdr"){
      colnames(sig.var)<-c("Counts (up)","Counts (down)", "p.adjusted", "-log10(p)");
    }else{
      colnames(sig.var)<-c("Counts (up)","Counts (down)", "raw.pval", "-log10(p)");
    }
    # first order by count difference, then by log(p)
    dif.count<-abs(sig.var[,1]-sig.var[,2]);
    ord.inx<-order(dif.count, sig.var[,4], decreasing=T);
    sig.var<-sig.var[ord.inx,,drop=F];
    sig.var[,c(3,4)]<-signif(sig.var[,c(3,4)],5);
  }else{
    sig.var <- cbind(fc.all[inx.imp,drop=F], fc.log[inx.imp,drop=F], p.value[inx.imp,drop=F], p.log[inx.imp,drop=F]);
    if(pval.type == "fdr"){
      colnames(sig.var) <- c("FC", "log2(FC)", "p.ajusted", "-log10(p)");
    }else{
      colnames(sig.var) <- c("FC", "log2(FC)", "raw.pval", "-log10(p)");
    }
    # first order by log(p), then by log(FC)
    ord.inx <- order(sig.var[,4], abs(sig.var[,2]), decreasing=T);
    sig.var <- sig.var[ord.inx,,drop=F];
    sig.var <- signif(sig.var,5);
  }
  
  fileName <- "volcano.csv";
  write.csv(signif(sig.var,5), file=fileName);
  volcano <- list (
    raw.threshx = fcthresh,
    raw.threshy = threshp,
    paired = paired,
    max.xthresh = max.xthresh,
    min.xthresh = min.xthresh,
    thresh.y = -log10(threshp),
    fc.all = fc.all,
    fc.log = fc.log,
    fc.log.uniq = jitter(fc.log),
    inx.up = inx.up,
    inx.down = inx.down,
    p.log = p.log,
    inx.p = inx.p,
    sig.mat = sig.var
  );
  mSetObj$analSet$volcano <- volcano;
  return(.set.mSet(mSetObj));
}

#'Create volcano plot
#'@description For labelling interesting points, it is defined by the following rules:
#'need to be signficant (sig.inx) and or 2. top 5 p, or 2. top 5 left, or 3. top 5 right. 
#'@usage PlotVolcano(mSetObj=NA, imgName, plotLbl, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param plotLbl Logical, plot labels, 1 for yes and 0 for no. 
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotVolcano <- function(mSetObj=NA, imgName, plotLbl, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w*6/10;
  
  mSetObj$imgSet$volcano <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mar=c(5,5,3,4));
  vcn <- mSetObj$analSet$volcano;
  MyGray <- rgb(t(col2rgb("black")), alpha=40, maxColorValue=255);
  MyHighlight <- rgb(t(col2rgb("magenta")), alpha=80, maxColorValue=255);
  
  if(vcn$paired){
    xlim<-c(-nrow(mSetObj$dataSet$norm)/2, nrow(mSetObj$dataSet$norm)/2)*1.2;
    
    # merge fc.all two rows into one, bigger one win
    fc.all <- apply(vcn$fc.all, 2, function(x){ if(x[1] > x[2]){return(x[1])}else{return(-x[2])}})
    
    hit.inx <- vcn$inx.p & (vcn$inx.up | vcn$inx.down);
    plot(fc.all, vcn$p.log, xlim=xlim, pch=20, cex=ifelse(hit.inx, 1.2, 0.8),
         col = ifelse(hit.inx, MyHighlight, MyGray),
         xlab="Count of Significant Pairs", ylab="-log10(p)");
    
    sig.upInx <- vcn$inx.p & vcn$inx.up;
    p.topInx <- GetTopInx(vcn$p.log, 5, T) & vcn$inx.up;
    fc.rtInx <- GetTopInx(vcn$fc.all[1,], 5, T);
    lblInx <- p.topInx & sig.upInx & fc.rtInx;
    if(plotLbl & sum(lblInx, na.rm=T) > 0){
      text.lbls<-substr(colnames(mSetObj$dataSet$norm)[lblInx],1,14) # some names may be too long
      text(vcn$fc.all[1,lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=4, col="blue", srt=30, xpd=T, cex=0.8);
    }
    
    sig.dnInx <- vcn$inx.p & vcn$inx.down;
    p.topInx <- GetTopInx(vcn$p.log, 5, T) & vcn$inx.down;
    fc.leftInx <- GetTopInx(vcn$fc.all[2,], 5, T) & vcn$inx.down;
    lblInx <-p.topInx & sig.dnInx & fc.leftInx;
    if(plotLbl & sum(lblInx, na.rm=T) > 0){
      text.lbls<-substr(colnames(mSetObj$dataSet$norm)[lblInx],1,14) # some names may be too long
      text(-vcn$fc.all[2,lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=2, col="blue", srt=-30, xpd=T, cex=0.8);
    }
    
  }else{
    imp.inx<-(vcn$inx.up | vcn$inx.down) & vcn$inx.p;
    plot(vcn$fc.log, vcn$p.log, pch=20, cex=ifelse(imp.inx, 1.2, 0.7),
         col = ifelse(imp.inx, MyHighlight, MyGray),
         xlab="log2 (FC)", ylab="-log10(p)");
    
    sig.inx <- imp.inx;
    p.topInx <- GetTopInx(vcn$p.log, 5, T) & (vcn$inx.down);
    fc.leftInx <- GetTopInx(vcn$fc.log, 5, F);
    lblInx <-  sig.inx & (p.topInx | fc.leftInx);
    if(plotLbl &  sum(lblInx, na.rm=T) > 0){
      text.lbls<-substr(colnames(mSetObj$dataSet$norm)[lblInx],1,14) # some names may be too long
      text(vcn$fc.log[lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=2, col="blue", srt=-30, xpd=T, cex=0.8);
    }
    
    p.topInx <- GetTopInx(vcn$p.log, 5, T) & (vcn$inx.up);
    fc.rtInx <- GetTopInx(vcn$fc.log, 5, T);
    lblInx <- sig.inx & (p.topInx | fc.rtInx);
    if(plotLbl & sum(lblInx, na.rm=T) > 0){
      text.lbls<-substr(colnames(mSetObj$dataSet$norm)[lblInx],1,14) # some names may be too long
      text(vcn$fc.log[lblInx], vcn$p.log[lblInx],labels=text.lbls, pos=4, col="blue", srt=30, xpd=T, cex=0.8);
    }
  }
  
  abline (v = vcn$max.xthresh, lty=3);
  abline (v = vcn$min.xthresh, lty=3);
  abline (h = vcn$thresh.y, lty=3);
  axis(4); # added by Beomsoo
  dev.off();
  return(.set.mSet(mSetObj));
}

#'ANOVA
#'@description Perform anova and only return p values and MSres (for Fisher's LSD)
#'@param x Input the data to perform ANOVA
#'@param cls Input class labels
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
aof <- function(x, cls) {
  aov(x ~ cls);
}


#'Kruskal-Wallis
#'@description Perform  Kruskal-Wallis Test
#'@param x Input data to perform Kruskal-Wallis
#'@param cls Input class labels
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
kwtest <- function(x, cls) {
  kruskal.test(x ~ cls);
}

#'Fisher for ANOVA
#'@description Perform  Fisher LSD for ANOVA, used in higher function 
#'@param aov.obj Input the anova object
#'@param thresh Numeric, input the alpha threshold 
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
FisherLSD <- function(aov.obj, thresh){
  LSD.test(aov.obj,"cls", alpha=thresh)
}

#'Return only the signicant comparison names
#'@description Return only the signicant comparison names, used in higher function 
#'@param tukey Input tukey output
#'@param cut.off Input numeric cut-off
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
parseTukey <- function(tukey, cut.off){
  inx <- tukey$cls[,"p adj"] <= cut.off;
  paste(rownames(tukey$cls)[inx], collapse="; ");
}

#'Return only the signicant comparison names
#'@description Return only the signicant comparison names, used in higher function 
#'@param fisher Input fisher object 
#'@param cut.off Numeric, set cut-off
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
parseFisher <- function(fisher, cut.off){
  inx <- fisher[,"pvalue"] <= cut.off;
  paste(rownames(fisher)[inx], collapse="; ");
}

#'Perform ANOVA analysis
#'@description ANOVA analysis
#'@usage ANOVA.Anal(mSetObj=NA, nonpar=F, thresh=0.05, post.hoc="fisher")
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param nonpar Logical, use a non-parametric test (T) or not (F)
#'@param thresh Numeric, from 0 to 1, indicate the p-value threshold
#'@param post.hoc Input the name of the post-hoc test, "fisher" or "tukey"
#'@param all_results Logical, if TRUE, it will output the ANOVA results for all compounds 
#'with no post-hoc tests performed.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
ANOVA.Anal<-function(mSetObj=NA, nonpar=F, thresh=0.05, post.hoc="fisher", all_results=FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  sig.num <- 0;
  if(nonpar){
    aov.nm <- "Kruskal Wallis Test";
    anova.res <- apply(as.matrix(mSetObj$dataSet$norm), 2, kwtest, cls=mSetObj$dataSet$cls);
    
    #extract all p values
    res <- unlist(lapply(anova.res, function(x) {c(x$statistic, x$p.value)}));
    res <- data.frame(matrix(res, nrow=length(anova.res), byrow=T), stringsAsFactors=FALSE);
    
    fstat <- res[,1];
    p.value <- res[,2];
    
    names(fstat) <- names(p.value) <- colnames(mSetObj$dataSet$norm);
    fdr.p <- p.adjust(p.value, "fdr");
    
    #inx.imp <- p.value <= thresh;
    inx.imp <- fdr.p <= thresh;
    sig.num <- sum(inx.imp);
    
    if(sig.num > 0){ 
      sig.f <- fstat[inx.imp];
      sig.p <- p.value[inx.imp];
      fdr.p <- fdr.p[inx.imp];
      
      sig.mat <- data.frame(signif(sig.f,5), signif(sig.p,5), signif(-log10(sig.p),5), signif(fdr.p,5), 'NA');
      rownames(sig.mat) <- names(sig.p);
      colnames(sig.mat) <- c("chi.squared", "p.value", "-log10(p)", "FDR", "Post-Hoc");
      
      # order the result simultaneously
      ord.inx <- order(sig.p, decreasing = FALSE);
      sig.mat <- sig.mat[ord.inx,,drop=F];
      
      fileName <- "kw_posthoc.csv";
      my.mat <- sig.mat[,1:4];
      colnames(my.mat) <- c("chi_squared", "pval_KW", "-log10(p)", "FDR");
    }
  }else{
    aov.nm <- "One-way ANOVA";
    if(.on.public.web & RequireFastUnivTests(mSetObj)){
      res <- PerformFastUnivTests(mSetObj$dataSet$norm, mSetObj$dataSet$cls);
    }else{
      aov.res <- apply(as.matrix(mSetObj$dataSet$norm), 2, aof, cls=mSetObj$dataSet$cls);
      anova.res <- lapply(aov.res, anova);
      
      #extract all p values
      res <- unlist(lapply(anova.res, function(x) { c(x["F value"][1,], x["Pr(>F)"][1,])}));
      res <- data.frame(matrix(res, nrow=length(aov.res), byrow=T), stringsAsFactors=FALSE);
    }
    fstat <- res[,1];
    p.value <- res[,2];
    names(fstat) <- names(p.value) <- colnames(mSetObj$dataSet$norm);
    
    fdr.p <- p.adjust(p.value, "fdr");
    
    if(all_results==TRUE){
      all.mat <- data.frame(signif(p.value,5), signif(-log10(p.value),5), signif(fdr.p,5));
      rownames(all.mat) <- names(p.value);
      colnames(all.mat) <- c("p.value", "-log10(p)", "FDR");
      write.csv(all.mat, "anova_all_results.csv")
    }
    
    # do post-hoc only for signficant entries
    # inx.imp <- p.value <= thresh;
    inx.imp <- fdr.p <= thresh;
    sig.num <- sum(inx.imp);
    if(sig.num > 0){
      # note aov obj is not avaible using fast version
      # need to recompute using slower version for the sig ones
      if(.on.public.web & RequireFastUnivTests(mSetObj)){
        aov.imp <- apply(as.matrix(mSetObj$dataSet$norm[,inx.imp,drop=FALSE]), 2, aof, cls=mSetObj$dataSet$cls);
      }else{
        aov.imp <- aov.res[inx.imp];
      }
      sig.f <- fstat[inx.imp];
      sig.p <- p.value[inx.imp];
      fdr.p <- fdr.p[inx.imp];
      cmp.res <- NULL;
      post.nm <- NULL;
      if(post.hoc=="tukey"){
        tukey.res<-lapply(aov.imp, TukeyHSD, conf.level=1-thresh);
        cmp.res <- unlist(lapply(tukey.res, parseTukey, cut.off=thresh));
        post.nm = "Tukey's HSD";
      }else{
        fisher.res<-lapply(aov.imp, FisherLSD, thresh);
        cmp.res <- unlist(lapply(fisher.res, parseFisher, cut.off=thresh));
        post.nm = "Fisher's LSD";
      }
      
      # create the result dataframe,
      # note, the last column is string, not double
      
      sig.mat <- data.frame(signif(sig.f,5), signif(sig.p,5), signif(-log10(sig.p),5), signif(fdr.p,5), cmp.res);
      rownames(sig.mat) <- names(sig.p);
      colnames(sig.mat) <- c("f.value", "p.value", "-log10(p)", "FDR", post.nm);
      
      # order the result simultaneously
      ord.inx <- order(sig.p, decreasing = FALSE);
      sig.mat <- sig.mat[ord.inx,,drop=F];
      fileName <- "anova_posthoc.csv";
    }
  }
  
  AddMsg(paste(c("A total of", sum(inx.imp), "significant features were found."), collapse=" "));
  if(sig.num> 0){
    res <- 1;
    write.csv(sig.mat,file=fileName);
    aov<-list (
      aov.nm = aov.nm,
      sig.num = sig.num,
      sig.nm = fileName,
      raw.thresh = thresh,
      thresh = -log10(thresh), # only used for plot threshold line
      p.value = p.value,
      p.log = -log10(p.value),
      inx.imp = inx.imp,
      post.hoc = post.hoc,
      sig.mat = sig.mat
    );
  }else{
    res <- 0;
    aov<-list (
      aov.nm = aov.nm,
      sig.num = sig.num,
      raw.thresh = thresh,
      thresh = -log10(thresh), # only used for plot threshold line
      p.value = p.value,
      p.log = -log10(p.value),
      inx.imp = inx.imp
    );
  }
  mSetObj$analSet$aov <- aov;
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(res);
  }else{
    return(.set.mSet(mSetObj));
  }
}

#'Plot ANOVA 
#'@description Plot ANOVA 
#'@usage PlotANOVA(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotANOVA <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  lod <- mSetObj$analSet$aov$p.log;
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*6/9;
  
  mSetObj$imgSet$anova <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(lod, ylab="-log10(p)", xlab = GetVariableLabel(mSetObj$dataSet$type), main=mSetObj$analSet$aov$aov.nm, type="n");
  red.inx <- which(mSetObj$analSet$aov$inx.imp);
  blue.inx <- which(!mSetObj$analSet$aov$inx.imp);
  points(red.inx, lod[red.inx], bg="red", cex=1.2, pch=21);
  points(blue.inx, lod[blue.inx], bg="green", pch=21);
  abline (h=mSetObj$analSet$aov$thresh, lty=3);
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Plot Compound View 
#'@description Plots a bar-graph of selected compound over groups 
#'@usage PlotCmpdView(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmpdNm Input a name for the compound 
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotCmpdView <- function(mSetObj=NA, cmpdNm, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web){
    load_ggplot()
  }
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    cls <- mSetObj$dataSet$cls;
  }
  
  imgName <- gsub("\\/", "_",  cmpdNm);
  imgName <- paste(imgName, "_dpi", dpi, ".", format, sep="");
  
  my.width <- 200;
  adj.width <- 90*length(levels(cls))+20;
  if(adj.width > my.width){
    my.width <- adj.width;
  }
  
  x <- mSetObj$dataSet$norm[, cmpdNm]
  y <- cls
  df <- data.frame(conc = x, class = y)
  col <- unique(GetColorSchema(mSetObj))
  
  Cairo::Cairo(file = imgName, dpi=dpi, width=my.width, height=325, type=format, bg="transparent");
  
  p <- ggplot2::ggplot(df, aes(x=class, y=conc, fill=class)) + geom_boxplot(notch=FALSE, outlier.shape = NA, outlier.colour=NA) + theme_bw() + geom_jitter(size=1)
  p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
  p <- p + stat_summary(fun.y=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
  p <- p + theme(text = element_text(size=15), plot.margin = margin(t=0.20, r=0.25, b=0.55, l=0.25, "cm"))
  p <- p + scale_fill_manual(values=col) + ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=45, hjust=1))
  p <- p + theme(plot.title = element_text(size = 13, hjust=0.5, face="bold"), axis.text = element_text(size=10))
  print(p)
  dev.off()
  
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Sig Table for Fold-Change Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.FC <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$fc$sig.mat, "fold change analysis", mSetObj$dataSet$type);
}

GetFCSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$fc$sig.mat));
}

GetFCSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$fc$sig.mat);
}

GetFCSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$fc$sig.mat);
}

GetAovSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(as.matrix(mSetObj$analSet$aov$sig.mat[, 1:4])));
}

GetAovSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$aov$sig.mat);
}

GetAovSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$aov$sig.mat[, 1:4]);
}

GetAovPostHocSig <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$aov$sig.mat[,5];
}

#'Sig Table for Anova
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.Anova <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$aov$sig.mat, "One-way ANOVA and post-hoc analysis", mSetObj$dataSet$type);
}

GetAnovaUpMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov$p.log;
  red.inx<- which(mSetObj$analSet$aov$inx.imp);
  if(sum(red.inx) > 0){
    return(as.matrix(cbind(red.inx, lod[red.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetAnovaDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$aov$p.log;
  blue.inx <- which(!mSetObj$analSet$aov$inx.imp);
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(blue.inx, lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetAnovaCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$aov$p.log);
}

GetAnovaCmpdInxs<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(1:length(mSetObj$analSet$aov$p.log));
}

GetAnovaSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$aov$sig.nm;
}

#'Sig Table for T-test Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.TT <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$tt$sig.mat, "t-tests", mSetObj$dataSet$type);
}

#'T-test matrix
#'@description Return a double matrix with 2 columns - p values and lod
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetTTSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$tt$sig.mat));
}

GetTTSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$tt$sig.mat);
}

GetTTSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$tt$sig.mat);
}

GetTtUpMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$tt$p.log;
  red.inx<- which(mSetObj$analSet$tt$inx.imp);
  if(sum(red.inx) > 0){
    return(as.matrix(cbind(red.inx, lod[red.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetTtDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  lod <- mSetObj$analSet$tt$p.log;
  blue.inx <- which(!mSetObj$analSet$tt$inx.imp);
  
  if(sum(blue.inx) > 0){
    return(as.matrix(cbind(blue.inx, lod[blue.inx])));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetTtCmpdInxs <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(1:length(mSetObj$analSet$tt$p.log));
}

GetTtCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$tt$p.log);
}

GetTtestSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$tt$sig.nm;
}

#'Retrieve T-test p-values
#'@description Utility method to get p values
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param paired Default set to FALSE
#'@param equal.var Default set to TRUE
#'@param nonpar Use non-parametric tests, default is set to FALSE
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetTtestRes <- function(mSetObj=NA, paired=FALSE, equal.var=TRUE, nonpar=F){
  
  mSetObj <- .get.mSet(mSetObj);
  inx1 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]);
  inx2 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]);
  univ.test <- function(x){t.test(x[inx1], x[inx2], paired = paired, var.equal = equal.var)};
  if(nonpar){
    univ.test <- function(x){wilcox.test(x[inx1], x[inx2], paired = paired)};
  }
  my.fun <- function(x) {
    tmp <- try(univ.test(x));
    if(class(tmp) == "try-error") {
      return(c(NA, NA));
    }else{
      return(c(tmp$statistic, tmp$p.value));
    }
  }
  res <- apply(as.matrix(mSetObj$dataSet$norm), 2, my.fun);
  return(t(res));
}

#'Utility method to perform the univariate analysis automatically
#'@description The approach is computationally expensive,and fails more often 
#'get around: make it lazy unless users request, otherwise the default t-test will also be affected
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetUnivReport <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  paired <- mSetObj$analSet$tt$paired;
  threshp <- mSetObj$analSet$tt$raw.thresh;
  
  inx1 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[1]);
  inx2 <- which(mSetObj$dataSet$cls==levels(mSetObj$dataSet$cls)[2]);
  
  # output list (mean(sd), mean(sd), p-value, FoldChange, Up/Down) 
  univStat.mat <- apply(as.matrix(mSetObj$dataSet$norm), 2, function(x) {
    
    # normality test for each group
    # ks <- ks.test(x[inx1], x[inx2]); 
    if( var(x[inx1], na.rm=T) == 0 |var(x[inx2], na.rm=T) == 0 ){ # shapiro cannot work when all values are same
      method = "";
    }else{
      sw.g1 <- shapiro.test(x[inx1]); 
      sw.g2 <- shapiro.test(x[inx2]); 
      method <- ifelse( ((sw.g1$p.value <= 0.05) | (sw.g2$p.value <= 0.05)), "(W)","")
    }
    if (method == "(W)") {
      # wilcoxon test
      tmp <- try(wilcox.test(x[inx1], x[inx2], paired = paired));
    } else {
      # t-test
      equal.var <- TRUE;
      if(var(x, na.rm=TRUE) != 0) {
        anal.var <- var.test(x[inx1], x[inx2]);
        equal.var <- ifelse(anal.var$p.value <= 0.05, FALSE, TRUE);
      }
      
      tmp <- try(t.test(x[inx1], x[inx2], paired = paired, var.equal = equal.var));
    }
    if(class(tmp) == "try-error") {
      return(NA);
    }else{            
      mean1 <- mean(x[inx1]);
      mean2 <- mean(x[inx2]);
      sd1 <- sd(x[inx1]);
      sd2 <- sd(x[inx2]);
      p.value <- paste(ifelse(tmp$p.value < 0.0001, "< 0.0001", sprintf("%.4f", tmp$p.value,4))," ", method, sep="");
      p.value.origin <- tmp$p.value;
      foldChange <- mean1 / mean2;
      foldChange <- round(ifelse( foldChange >= 1, foldChange, (-1/foldChange) ), 2);
      upDown <- ifelse(mean1 > mean2, "Up","Down");
      
      univStat <- c(
        meanSD1   = sprintf("%.3f (%.3f)", mean1, sd1),
        meanSD2   = sprintf("%.3f (%.3f)", mean2, sd2),
        p.value = p.value,
        foldChange = foldChange,
        upDown  = upDown,
        p.value.origin = sprintf("%.5f", p.value.origin)
      );
      return(univStat);
    }
  })
  
  univStat.mat <- as.data.frame(t(univStat.mat));
  
  # add FDR/q-value
  q.value <- sprintf("%.4f", p.adjust(p=as.numeric(levels(univStat.mat$p.value.origin))[univStat.mat$p.value.origin], method='fdr'));
  univStat.mat <- cbind(univStat.mat[, c(1,2,3)], q.value, univStat.mat[, c(4,5)], univStat.mat[,6]);
  names(univStat.mat)[1] <- paste("Mean (SD) of ", levels(mSetObj$dataSet$cls)[1], sep='');
  names(univStat.mat)[2] <- paste("Mean (SD) of ", levels(mSetObj$dataSet$cls)[2], sep='');
  names(univStat.mat)[3] <- "p-value";
  names(univStat.mat)[4] <- "q-value (FDR)";
  names(univStat.mat)[5] <- "Fold Change";
  names(univStat.mat)[6] <- paste(levels(mSetObj$dataSet$cls)[1],"/", levels(mSetObj$dataSet$cls)[2], sep='');
  names(univStat.mat)[7] <- "p.value.origin";
  
  univStat.mat <- cbind(Name=rownames(univStat.mat), univStat.mat);
  rownames(univStat.mat) <- NULL
  
  ## generate univariate report file (univAnalReport.csv).
  ## mixed with t-test and wilcoxon test depend on each metabolite's distribution
  univAnal.mat <- univStat.mat;
  note.str <- paste("\n Univariate Analysis Result for each variable/metabolite\n\n",
                    "[NOTE]\n", 
                    "    p-value is calculated with t-test as a default.\n",
                    "    p-value with (W) is calculated by the Wilcoxon Mann Whitney test\n\n\n", sep='');
  
  cat(note.str, file="univAnalReport.csv", append=FALSE);
  write.table(univAnal.mat, file="univAnalReport.csv", append=TRUE, sep=",", row.names=FALSE);
  
  ## generate subset with the threshold (p-value)
  sigones <- which(as.numeric(as.character(univAnal.mat$p.value.origin)) <= threshp);
  
  sigDataSet.orig <- cbind(SampleID=rownames(mSetObj$dataSet$orig), Label=mSetObj$dataSet$cls, mSetObj$dataSet$orig[,c(sigones)])
  sigDataSet.norm <- cbind(SampleID=rownames(mSetObj$dataSet$orig), Label=mSetObj$dataSet$cls, mSetObj$dataSet$norm[,c(sigones)])
  
  write.table(sigDataSet.orig, file=paste("data_subset_orig_p", threshp, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE);
  write.table(sigDataSet.norm, file=paste("data_subset_norm_p", threshp, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE);
}

ContainInfiniteTT<-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(sum(!is.finite(mSetObj$analSet$tt$sig.mat))>0){
    return("true");
  }
  return("false");
}

GetVolcanoDnMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- (vcn$inx.up | vcn$inx.down) & vcn$inx.p;
  blue.inx <- which(!imp.inx);
  
  if(sum(blue.inx)>0){
    xs <- vcn$fc.log.uniq[blue.inx]
    ys <- vcn$p.log[blue.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoUpMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  imp.inx <- (vcn$inx.up | vcn$inx.down) & vcn$inx.p;
  red.inx <- which(imp.inx);
  if(sum(red.inx)>0){
    xs <- vcn$fc.log.uniq[red.inx]
    ys <- vcn$p.log[red.inx];
    return(as.matrix(cbind(xs, ys)));
  }else{
    return(as.matrix(cbind(-1, -1)));
  }
}

GetVolcanoVlMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  limy <- GetExtendRange(vcn$fc.log);
  as.matrix(rbind(c(vcn$min.xthresh, limy[1]), c(vcn$min.xthresh,limy[2])));
}

GetVolcanoVrMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn <- mSetObj$analSet$volcano;
  limy <- GetExtendRange(vcn$fc.log);
  as.matrix(rbind(c(vcn$max.xthresh, limy[1]), c(vcn$max.xthresh,limy[2])));
}

GetVolcanoHlMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  vcn<-mSetObj$analSet$volcano;
  limx <- GetExtendRange(vcn$fc.log);
  as.matrix(rbind(c(limx[1], vcn$thresh.y), c(limx[2],vcn$thresh.y)));
}

GetVolcanoRangeX <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  range(mSetObj$analSet$volcano$fc.log.uniq);
}

GetVolcanoCmpds <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  names(mSetObj$analSet$volcano$fc.log);
}

GetVolcanoCmpdInxs <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$volcano$fc.log.uniq
}

#'Volcano indices
#'@description Get indices of top n largest/smallest number
#'@param vec Vector containing volcano indices
#'@param n Numeric
#'@param dec Logical, default set to TRUE
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetTopInx <- function(vec, n, dec=T){
  inx <- order(vec, decreasing = dec)[1:n];
  # convert to T/F vec
  vec<-rep(F, length=length(vec));
  vec[inx] <- T;
  return (vec);
}

#'Sig table for Volcano Analysis
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetSigTable.Volcano <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  GetSigTable(mSetObj$analSet$volcano$sig.mat, "volcano plot", mSetObj$dataSet$type);
}

GetVolcanoSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(CleanNumber(mSetObj$analSet$volcano$sig.mat));
}

GetVolcanoSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$analSet$volcano$sig.mat));
}

GetVolcanoSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$analSet$volcano$sig.mat));
}

ContainInfiniteVolcano <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  if(sum(!is.finite(mSetObj$analSet$volcano$sig.mat))>0){
    return("true");
  }
  return("false");
}

GetAovSigNum <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$analSet$aov$sig.num);
}


#'Calculate Fisher's Least Significant Difference (LSD)
#'@description Adapted from the 'agricolae' package
#'@param y Input Y
#'@param trt Input trt
#'@param alpha Numeric, default is 0.05
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)

LSD.test <- function(y, trt, alpha = 0.05){
  clase<-c("aov","lm")
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  if("aov"%in%class(y) | "lm"%in%class(y)){
    A<-y$model
    DFerror<-df.residual(y)
    MSerror<-deviance(y)/DFerror
    y<-A[,1]
    ipch<-pmatch(trt,names(A))
    name.t <-names(A)[ipch]
    trt<-A[,ipch]
    name.y <- names(A)[1]
  }
  junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
  means <- tapply.stat(junto[, 1], junto[, 2], stat="mean") #change
  sds <- tapply.stat(junto[, 1], junto[, 2], stat="sd")     #change
  nn <- tapply.stat(junto[, 1], junto[, 2], stat="length")  #change
  std.err <- sds[, 2]/sqrt(nn[, 2])
  Tprob <- qt(1 - alpha/2, DFerror)
  LCL <- means[,2]-Tprob*std.err
  UCL <- means[,2]+Tprob*std.err
  means <- data.frame(means, std.err, replication = nn[, 2], LCL, UCL)
  names(means)[1:2] <- c(name.t, name.y)
  #row.names(means) <- means[, 1]
  ntr <- nrow(means)
  nk <- choose(ntr, 2)
  nr <- unique(nn[, 2])
  
  comb <- combn(ntr, 2)
  nn <- ncol(comb)
  dif <- rep(0, nn)
  LCL1<-dif
  UCL1<-dif
  sig<-NULL
  pvalue <- rep(0, nn)
  for (k in 1:nn) {
    i <- comb[1, k]
    j <- comb[2, k]
    if (means[i, 2] < means[j, 2]){
      comb[1, k]<-j
      comb[2, k]<-i
    }
    dif[k] <- abs(means[i, 2] - means[j, 2])
    sdtdif <- sqrt(MSerror * (1/means[i, 4] + 1/means[j,4]))
    pvalue[k] <- 2 * (1 - pt(dif[k]/sdtdif, DFerror));
    pvalue[k] <- round(pvalue[k],6);
    LCL1[k] <- dif[k] - Tprob*sdtdif
    UCL1[k] <- dif[k] + Tprob*sdtdif
    sig[k]<-" "
    if (pvalue[k] <= 0.001) sig[k]<-"***"
    else  if (pvalue[k] <= 0.01) sig[k]<-"**"
    else  if (pvalue[k] <= 0.05) sig[k]<-"*"
    else  if (pvalue[k] <= 0.1) sig[k]<-"."
  }
  tr.i <- means[comb[1, ],1]
  tr.j <- means[comb[2, ],1]
  output<-data.frame("Difference" = dif, pvalue = pvalue,sig,LCL=LCL1,UCL=UCL1)
  rownames(output)<-paste(tr.i,tr.j,sep=" - ");
  output;
}


tapply.stat <-function (y, x, stat = "mean"){
  cx<-deparse(substitute(x))
  cy<-deparse(substitute(y))
  x<-data.frame(c1=1,x)
  y<-data.frame(v1=1,y)
  nx<-ncol(x)
  ny<-ncol(y)
  namex <- names(x)
  namey <- names(y)
  if (nx==2) namex <- c("c1",cx)
  if (ny==2) namey <- c("v1",cy)
  namexy <- c(namex,namey)
  for(i in 1:nx) {
    x[,i]<-as.character(x[,i])
  }
  z<-NULL
  for(i in 1:nx) {
    z<-paste(z,x[,i],sep="&")
  }
  w<-NULL
  for(i in 1:ny) {
    m <-tapply(y[,i],z,stat)
    m<-as.matrix(m)
    w<-cbind(w,m)
  }
  nw<-nrow(w)
  c<-rownames(w)
  v<-rep("",nw*nx)
  dim(v)<-c(nw,nx)
  for(i in 1:nw) {
    for(j in 1:nx) {
      v[i,j]<-strsplit(c[i],"&")[[1]][j+1]
    }
  }
  rownames(w)<-NULL
  junto<-data.frame(v[,-1],w)
  junto<-junto[,-nx]
  names(junto)<-namexy[c(-1,-(nx+1))]
  return(junto)
}

## fast T-tests/F-tests using genefilter
## It leverages RSclient to perform one-time memory intensive computing
PerformFastUnivTests <- function(data, cls, var.equal=TRUE){
  print("Peforming fast univariate tests ....");
  rsc <- SetupRSclient();
  
  # note, feature in rows for gene expression
  data <- t(as.matrix(data));
  dat.out <- list(data=data, cls=cls, var.equal=var.equal);
  RS.assign(rsc, "dat.in", dat.out); 
  my.fun <- function(){
    if(length(levels(cls)) > 2){
      res <- try(genefilter::rowFtests(dat.in$data, dat.in$cls, var.equal = dat.in$var.equal));
    }else{
      res <- try(genefilter::rowttests(dat.in$data, dat.in$cls));
    }
    if(class(res) == "try-error") {
      res <- cbind(NA, NA);
    }else{
      res <- cbind(res$statistic, res$p.value);
    }
    return(res);
  }
  RS.assign(rsc, my.fun);
  my.res <- RS.eval(rsc, my.fun());
  RS.close(rsc);
  return(my.res);
}

#'Create Heat Map Plot
#'@description Plot a heatmap based on results from t-tests/ANOVA, VIP or randomforest
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param dataOpt Set data options
#'@param scaleOpt Set the image scale
#'@param smplDist Input the sample distance method
#'@param clstDist Input the clustering distance method
#'@param palette Input color palette choice
#'@param viewOpt Set heatmap options, default is set to "detail"
#'@param rowV Default is set to T
#'@param colV Default is set to T
#'@param var.inx Default is set to NA
#'@param border Indicate whether or not to show cell-borders, default is set to T
#'@param grp.ave Logical, default is set to F
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotHeatMap <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, dataOpt, scaleOpt, smplDist, 
                        clstDist, palette, viewOpt="detail", rowV=T, colV=T, var.inx=NA, border=T, grp.ave=F){
  filenm = paste0(imgName, ".json")
  mSetObj <- .get.mSet(mSetObj);
  
  # record the paramters
  mSetObj$analSet$htmap <- list(dist.par=smplDist, clust.par=clstDist);
  
  # set up data set
  if(dataOpt=="norm"){
    my.data <- mSetObj$dataSet$norm;
  }else{
    my.data <- mSetObj$dataSet$prenorm;
  }
  
  if(is.na(var.inx)){
    hc.dat<-as.matrix(my.data);
  }else{
    hc.dat<-as.matrix(my.data[,var.inx]);
  }
  
  colnames(hc.dat) <- substr(colnames(hc.dat),1,18) # some names are too long
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    hc.cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    hc.cls <- mSetObj$dataSet$cls;
  }
  
  if(grp.ave){ # only use group average
    lvs <- levels(hc.cls);
    my.mns <- matrix(ncol=ncol(hc.dat),nrow=length(lvs));
    for(i in 1:length(lvs)){
      inx <-hc.cls == lvs[i];
      my.mns[i,]<- apply(hc.dat[inx, ], 2, mean);
    }
    rownames(my.mns) <- lvs;
    colnames(my.mns) <- colnames(hc.dat);
    hc.dat <- my.mns;
    hc.cls <- as.factor(lvs);
  }
  
  # set up colors for heatmap
  if(palette=="gbr"){
    colors <- colorRampPalette(c("green", "black", "red"), space="rgb")(256);
  }else if(palette == "heat"){
    colors <- heat.colors(256);
  }else if(palette == "topo"){
    colors <- topo.colors(256);
  }else if(palette == "gray"){
    colors <- colorRampPalette(c("grey90", "grey10"), space="rgb")(256);
  }else{
    colors <- rev(colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256));
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    minW <- 630;
    myW <- nrow(hc.dat)*18 + 150;
    
    if(myW < minW){
      myW <- minW;
    }   
    w <- round(myW/72,2);
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- 7.2;
  }
  
  mSetObj$imgSet$heatmap <- imgName;
  
  myH <- ncol(hc.dat)*18 + 150;
  h <- round(myH/72,2);
  
  if(viewOpt == "overview"){
    if(is.na(width)){
      if(w > 9){
        w <- 9;
      }
    }else if(width == 0){
      if(w > 7.2){
        w <- 7.2;
      }
      
    }else{
      w <- 7.2;
    }
    if(h > w){
      h <- w;
    }
    
    mSetObj$imgSet$heatmap <- imgName;
  }
  
  # make the width smaller fro group average
  if(grp.ave){
    w <- nrow(hc.dat)*25 + 300;
    w <- round(w/72,2);
  }
  
  if(border){
    border.col<-"grey60";
  }else{
    border.col <- NA;
  }
  if(format=="pdf"){
    pdf(file = imgName, width=w, height=h, bg="white", onefile=FALSE);
  }else{
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  }
  if(mSetObj$dataSet$cls.type == "disc"){
    annotation <- data.frame(class = hc.cls);
    rownames(annotation) <- rownames(hc.dat); 
    
    # set up color schema for samples
    if(palette == "gray"){
      cols <- GetColorSchema(mSetObj, T);
      uniq.cols <- unique(cols);
    }else{
      cols <- GetColorSchema(mSetObj);
      uniq.cols <- unique(cols);
    }
    
    if(mSetObj$dataSet$type.cls.lbl=="integer"){
      cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
    }else{
      cls <- mSetObj$dataSet$cls;
    }
    
    names(uniq.cols) <- unique(as.character(sort(cls)));
    ann_colors <- list(class= uniq.cols);
    
    pheatmap::pheatmap(t(hc.dat), 
                       annotation=annotation, 
                       fontsize=8, fontsize_row=8, 
                       clustering_distance_rows = smplDist,
                       clustering_distance_cols = smplDist,
                       clustering_method = clstDist, 
                       border_color = border.col,
                       cluster_rows = colV, 
                       cluster_cols = rowV,
                       scale = scaleOpt, 
                       color = colors,
                       annotation_colors = ann_colors);
    dat = t(hc.dat)
    if(scaleOpt == "row"){
      res <- t(apply(dat, 1, function(x){as.numeric(cut(x, breaks=30))}));
    }else{
      res <- t(apply(dat, 2, function(x){as.numeric(cut(x, breaks=30))}));
    }
    colnames(dat) = NULL
    netData <- list(data=res, annotation=annotation, smp.nms = colnames(t(hc.dat)), met.nms = rownames(t(hc.dat)), colors = colors);
    sink(filenm);
    cat(RJSONIO::toJSON(netData));
    sink();
  }else{
    heatmap(hc.dat, Rowv = rowTree, Colv=colTree, col = colors, scale="column");
    dat = t(hc.dat)
    if(scaleOpt == "row"){
      res <- t(apply(dat, 1, function(x){as.numeric(cut(x, breaks=30))}));
    }else{
      res <- t(apply(dat, 2, function(x){as.numeric(cut(x, breaks=30))}));
    }
    colnames(dat) = NULL
    netData <- list(data=res, annotation="NA", smp.nms = colnames(t(hc.dat)), met.nms = rownames(t(hc.dat)), colors = colors);
    sink(filenm);
    cat(RJSONIO::toJSON(netData));
    sink();
  }
  dev.off();
  return(.set.mSet(mSetObj));
}

