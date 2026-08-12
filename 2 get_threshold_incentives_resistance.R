# Description: Compute adoption thresholds, incentives, and resistances from fitted coefs.
# Input: coefs CSVs in data/{exp}/coefs/, profiles/predictors in data/{exp}/conjoint input/
# Output: thresholds/incentives/resistances CSVs in data/{exp}/thresholds|incentives|resistances/

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(MASS)
# library(evd)
library(conjointr)
library(DescTools)
library(doMC)
library(foreach)
library(ggplot2)

# data loading
# ================================================================================

exp="messaging_app"
#exp="energy_policy"

project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_folder <- file.path(project_root,"data",exp)
input_folder<- file.path(project_root,"data",exp,"conjoint input")
dir.create(file.path(data_folder,"thresholds"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(data_folder,"incentives"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(data_folder,"resistances"), recursive = TRUE, showWarnings = FALSE)

# product profiles
profiles.matrix <- fread(file.path(input_folder,"profiles_matrix.csv"))
profiles.matrix <- as.matrix(profiles.matrix[, -"V1", with=FALSE], rownames=profiles.matrix$V1)
# Attributes and levels + additional variables
predictors.map <- fread(file.path(input_folder, "predictors_map.csv"), header=TRUE)


# register parallel backend 
n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# different realization
exp_names <- list.files(file.path(data_folder,"coefs"))
for (exp_name in exp_names){
  
  fit.hb <- fread( file.path(data_folder,"coefs",exp_name))
  #  averaging over the mcmc draws
  fit.hb <- fit.hb[, lapply(.SD, mean), by=resp.id]
  
  # Remove respondents with negative utility of the social signal 
  fit.hb <- fit.hb[adopters >=0]

  # prepare the predictors and product profile (label "adopters" is social signal)
  #===============================================================================

  # The predictors are all except adopters, resp.id and draw
  predictors <- predictors.map[(!grepl("adopters", prepared)) & !prepared %in% c("resp.id", "draw") ]$prepared
  
  # Get the names of the adopters variables
  adopters.variables <- predictors.map[grepl("adopters", prepared)]$prepared
  # Get the adopters values
  adopters.values <- sapply(adopters.variables, function(x) strsplit(x, "adopters")[[1]][2])
  
  # Adjust predictors to match fit.hb column naming
  predictors[predictors=="none"] <- "noneFALSE"

  # Calculate thresholds 
  # ================================================================================
  thresholds <- conjointr::GetThresholds(fit.hb, profiles.matrix, predictors, adopters = list(type="linear", values=adopters.values), constrain.thresholds=TRUE)
  thresholds <- thresholds[, list(frequency=.N), by=c("resp.id", "profile", "threshold")]
  # Write to csv
  write.csv(thresholds, file=file.path(data_folder,"thresholds",exp_name), row.names=FALSE)
  
  # get incentives
  # ================================================================================
  # Get incentives
  incentives <- GetIncentives(data = fit.hb, profiles.matrix = profiles.matrix, predictors = predictors)
  incentives <- incentives[, list(frequency=.N), by=c("resp.id", "profile", "incentive")]
  write.csv(incentives, file=file.path(data_folder,"incentives",exp_name), row.names=FALSE)
  
 
  # Get resistances
  # ================================================================================
  resistances = GetResistance(fit.hb, profiles.matrix)
  setnames(resistances, old = "product", new="profile")
  # Export 
  # ================================================================================
  write.csv(resistances, file=file.path(data_folder,"resistances",exp_name), row.names=FALSE)



}

# deregister parallel backend
parallel::stopCluster(cl)
foreach::registerDoSEQ()


