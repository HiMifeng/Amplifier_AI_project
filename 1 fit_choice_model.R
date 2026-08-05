# Description: Fit HB choice model to get per-respondent utility coefficients.
# Input: raw answer CSVs in data/{exp}/raw answers/
# Output: coefs CSVs in data/{exp}/coefs/

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(MASS)
# library(evd)
library(DescTools)
library(doMC)
library(foreach)
library(conjointr)

set.seed(123)

# Setup R, parameters, paths, etc
# ================================================================================
#exp="energy_policy"
exp= "messaging_app"
project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_folder <- file.path(project_root,"data",exp)
exp_names <- list.files(file.path(data_folder,"raw answers"))
coefs_folder <- file.path(data_folder,"coefs"); dir.create(coefs_folder, recursive = TRUE, showWarnings = FALSE)
source(file.path(project_root,"functions",'0_1 estimation_function.R'))

patch_ChoiceModelR_none()
for (exp_name in exp_names) {

  # Load data
  # ================================================================================
  data <- fread(file.path(data_folder,"raw answers",exp_name), stringsAsFactors=TRUE)
  data <- data[, c(setdiff(names(data), c("none","choice")), "none","choice"),with=FALSE]
  data[, none := factor(none, levels = c(1, 2), labels = c('1','2'))]
  
  if(exp=="energy_policy"){
    # Energy policy
    # =============================================================
    data[, year:=as.factor(year)]
    data[, distance:=as.factor(distance)]
    data[, cost:=as.factor(cost)]
    # Fit
    # ================================================================================
    fit.hb <- FitHB(data = data, options = list(none = FALSE, save = TRUE, keep = 10), mcmc = list(R = 30000,  use = 20000), directory=getwd())#, constraints = constraints)
 
    } else if(exp=="messaging_app"){
    # Fit  
    # ================================================================================
    fit.hb <- FitHB(data = data, options = list(none = FALSE, save = TRUE, keep = 10), mcmc = list(R = 30000,  use = 20000), directory=getwd())#, constraints = constraints)
    
  }
  setnames(fit.hb$betadraw, c("none1", "none2"), c("noneTRUE", "noneFALSE"))
  # coefficient export
  coefs <- fit.hb$betadraw[, lapply(.SD, mean), by=resp.id]
  
  write.csv(coefs, file=file.path(coefs_folder,exp_name), row.names=FALSE)
  
  
}



