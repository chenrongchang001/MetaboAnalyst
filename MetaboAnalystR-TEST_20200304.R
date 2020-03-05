# test the normalisation by median with demo data--------------------------
setwd("F:/Rsutdio_wd")
library(MetaboAnalystR)
mSet<-InitDataObjects("conc", "stat", FALSE)
mSet<-Read.TextData(mSet, "example.csv", "rowu", "disc")
mSet<-SanityCheckData(mSet)
#mSet<-RemoveMissingPercent(mSet, percent=0.5)
mSet<-ImputeVar(mSet, method="colmin")
mSet<-PreparePrenormData(mSet)
mSet<-Normalization_new(mSet, "MedianNorm","NULL", "AutoNorm", ratio=FALSE, ratioNum=20) #Normalization by Median
mSet<-PlotNormSummary(mSet, "norm_0_", "png", 72, width=NA)
mSet<-PlotSampleNormSummary(mSet, "snorm_0_", "png", 72, width=NA)

# volcano plot ------------------------------------------------------------
mSet<-Volcano.Anal(mSet, FALSE, 2.0, 0, 0.75,F, 0.1, TRUE, "raw")
mSet<-PlotVolcano(mSet, "volcano_0_",1, "png", 72, width=NA)

# T test ------------------------------------------------------------------
mSet<-Ttests.Anal(mSet, F, 0.05, FALSE, TRUE, FALSE)
mSet<-PlotTT(mSet, "tt_0_", "png", 72, width=NA)
mSet<-PlotCmpdView(mSet, "12017", "png", 72, width=NA)

# PCA analysis ------------------------------------------------------------
mSet<-PCA.Anal(mSet)
mSet<-PlotPCAPairSummary(mSet, "pca_pair_0_", "png", 72, width=NA, 5)
mSet<-PlotPCAScree(mSet, "pca_scree_0_", "png", 72, width=NA, 5)
mSet<-PlotPCA2DScore(mSet, "pca_score2d_0_", "png", 72, width=NA, 1,2,0.95,0,0)
mSet<-PlotPCALoading(mSet, "pca_loading_0_", "png", 72, width=NA, 1,2)
mSet<-PlotPCABiplot(mSet, "pca_biplot_0_", "png", 72, width=NA, 1,2)
mSet<-PlotPCA3DScoreImg(mSet, "pca_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
mSet<-PlotPCA3DLoading(mSet, "pca_loading3d_0_", "json", 1,2,3)
mSet<-PlotPCA2DScore(mSet, "pca_score2d_1_", "png", 72, width=NA, 1,5,0.95,0,0)
mSet<-PlotPCA2DScore(mSet, "pca_score2d_2_", "png", 72, width=NA, 4,5,0.95,0,0)

# Heatmap -----------------------------------------------------------------
mSet<-PlotHeatMap(mSet, "heatmap_1_", "png", 72, width=NA, "norm", "row", 
                  "euclidean", "ward.D","bwm", "overview", F, T, NA, T, F)

# PLS-DA ------------------------------------------------------------------
mSet<-PLSR.Anal(mSet, reg=TRUE)
mSet<-PlotPLSPairSummary(mSet, "pls_pair_0_", "png", 72, width=NA, 5)
mSet<-PlotPLS2DScore(mSet, "pls_score2d_0_", "png", 72, width=NA, 1,2,0.95,0,0)
mSet<-PlotPLS3DScoreImg(mSet, "pls_score3d_0_", "png", 72, width=NA, 1,2,3, 40)
mSet<-PlotPLSLoading(mSet, "pls_loading_0_", "png", 72, width=NA, 1, 2);
mSet<-PlotPLS3DLoading(mSet, "pls_loading3d_0_", "json", 1,2,3)
mSet<-PLSDA.CV(mSet, "T",5, "Q2")
mSet<-PlotPLS.Classification(mSet, "pls_cv_0_", "png", 72, width=NA)
mSet<-PlotPLS.Imp(mSet, "pls_imp_0_", "png", 72, width=NA, "vip", "Comp. 1", 15,FALSE)

# reform the Normalisation function ---------------------------------------
Normalization_new <-function (mSetObj = NA, rowNorm, transNorm, scaleNorm, ref = NULL, ratio = FALSE, ratioNum = 20) 
{
  mSetObj <- .get.mSet(mSetObj)
  data <- mSetObj$dataSet$prenorm
  cls <- mSetObj$dataSet$prenorm.cls
  if (substring(mSetObj$dataSet$format, 4, 5) == "ts") {
    if (is.null(mSetObj$dataSet$prenorm.facA)) {
      nfacA <- mSetObj$dataSet$facA
      nfacB <- mSetObj$dataSet$facB
    }
    else {
      nfacA <- mSetObj$dataSet$prenorm.facA
      nfacB <- mSetObj$dataSet$prenorm.facB
    }
    mSetObj$dataSet$facA <- nfacA
    mSetObj$dataSet$facB <- nfacB
    if (mSetObj$dataSet$design.type == "time" | mSetObj$dataSet$design.type == 
        "time0") {
      if (tolower(mSetObj$dataSet$facA.lbl) == "time") {
        time.fac <- nfacA
        exp.fac <- nfacB
      }
      else {
        time.fac <- nfacB
        exp.fac <- nfacA
      }
      mSetObj$dataSet$time.fac <- time.fac
      mSetObj$dataSet$exp.fac <- exp.fac
    }
  }
  colNames <- colnames(data)
  rowNames <- rownames(data)
  if (rowNorm == "QuantileNorm") {
    data <- QuantileNormalize(data)
    varCol <- apply(data, 2, var, na.rm = T)
    constCol <- (varCol == 0 | is.na(varCol))
    constNum <- sum(constCol, na.rm = T)
    if (constNum > 0) {
      print(paste("After quantile normalization", 
                  constNum, "features with a constant value were found and deleted."))
      data <- data[, !constCol, drop = FALSE]
      colNames <- colnames(data)
      rowNames <- rownames(data)
    }
    rownm <- "Quantile Normalization"
  }
  else if (rowNorm == "GroupPQN") {
    grp.inx <- cls == ref
    ref.smpl <- apply(data[grp.inx, , drop = FALSE], 2, mean)
    data <- t(apply(data, 1, ProbNorm, ref.smpl))
    rownm <- "Probabilistic Quotient Normalization by a reference group"
  }
  else if (rowNorm == "SamplePQN") {
    ref.smpl <- data[ref, , drop = FALSE]
    data <- t(apply(data, 1, ProbNorm, ref.smpl))
    rownm <- "Probabilistic Quotient Normalization by a reference sample"
  }
  else if (rowNorm == "CompNorm") {
    data <- t(apply(data, 1, CompNorm, ref))
    rownm <- "Normalization by a reference feature"
  }
  else if (rowNorm == "SumNorm") {
    data <- t(apply(data, 1, SumNorm))
    rownm <- "Normalization to constant sum"
  }
  else if (rowNorm == "MedianNorm") {
    data <- apply(t(data), 1, MedianNorm)       #this part has been reformed by crc on 20200304
    rownm <- "Normalization to sample median"
  }
  else if (rowNorm == "SpecNorm") {
    if (!exists("norm.vec")) {
      norm.vec <- rep(1, nrow(data))
      print("No sample specific information were given, all set to 1.0")
    }
    rownm <- "Normalization by sample-specific factor"
    data <- data/norm.vec
  }
  else {
    rownm <- "N/A"
  }
  rownames(data) <- rowNames
  colnames(data) <- colNames
  if (rowNorm == "CompNorm" && !is.null(ref)) {
    inx <- match(ref, colnames(data))
    data <- data[, -inx, drop = FALSE]
    colNames <- colNames[-inx]
  }
  mSetObj$dataSet$row.norm <- as.data.frame(CleanData(data, 
                                                      T, T))
  if (ratio) {
    min.val <- min(abs(data[data != 0]))/2
    norm.data <- log2((data + sqrt(data^2 + min.val))/2)
    transnm <- "Log Normalization"
    ratio.mat <- CalculatePairwiseDiff(norm.data)
    fstats <- Get.Fstat(ratio.mat, cls)
    hit.inx <- rank(-fstats) < ratioNum
    ratio.mat <- ratio.mat[, hit.inx, drop = FALSE]
    data <- cbind(norm.data, ratio.mat)
    colNames <- colnames(data)
    rowNames <- rownames(data)
    mSetObj$dataSet$use.ratio <- TRUE
    mSetObj$dataSet$proc.ratio <- data
  }
  else {
    mSetObj$dataSet$use.ratio <- FALSE
    if (transNorm == "LogNorm") {
      min.val <- min(abs(data[data != 0]))/10
      data <- apply(data, 2, LogNorm, min.val)
      transnm <- "Log Normalization"
    }
    else if (transNorm == "CrNorm") {
      norm.data <- abs(data)^(1/3)
      norm.data[data < 0] <- -norm.data[data < 0]
      data <- norm.data
      transnm <- "Cubic Root Transformation"
    }
    else {
      transnm <- "N/A"
    }
  }
  if (scaleNorm == "MeanCenter") {
    data <- apply(data, 2, MeanCenter)
    scalenm <- "Mean Centering"
  }
  else if (scaleNorm == "AutoNorm") {
    data <- apply(data, 2, AutoNorm)
    scalenm <- "Autoscaling"
  }
  else if (scaleNorm == "ParetoNorm") {
    data <- apply(data, 2, ParetoNorm)
    scalenm <- "Pareto Scaling"
  }
  else if (scaleNorm == "RangeNorm") {
    data <- apply(data, 2, RangeNorm)
    scalenm <- "Range Scaling"
  }
  else {
    scalenm <- "N/A"
  }
  rownames(data) <- rowNames
  colnames(data) <- colNames
  data <- CleanData(data, T, F)
  if (ratio) {
    mSetObj$dataSet$ratio <- CleanData(ratio.mat, T, F)
  }
  mSetObj$dataSet$norm <- as.data.frame(data)
  mSetObj$dataSet$cls <- cls
  mSetObj$dataSet$rownorm.method <- rownm
  mSetObj$dataSet$trans.method <- transnm
  mSetObj$dataSet$scale.method <- scalenm
  mSetObj$dataSet$combined.method <- FALSE
  mSetObj$dataSet$norm.all <- NULL
  return(.set.mSet(mSetObj))
}
