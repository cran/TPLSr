## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(TPLSr)
attach(TPLSdat)

## -----------------------------------------------------------------------------
ACCstorage <- rep(NA, 3)
for (i in 1:3) { # primary cross-validation fold
  test = subj==i; train = !test
  
  # perform nested cross-validation within training data
  cvmdl = TPLS_cv(X[train,],Y[train],subj[train])
  cvstats = evalTuningParam(cvmdl,"Pearson",X[train,],Y[train],1:25,seq(0,1,0.05),run[train])
  
  # fit T-PLS model using all training data based on best tuning parameter
  mdl = TPLS(X[train,],Y[train])
  
  # predict the testing subject
  score = TPLSpredict(mdl,cvstats$compval_best,cvstats$threshval_best,X[test,])
  prediction = 1*(score > 0.5)
  
  # assess performance of prediction
  ACCstorage[i] = mean(prediction==Y[test])
}

mean(ACCstorage) # out-of-sample CV performance

