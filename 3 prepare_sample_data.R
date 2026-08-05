# Description: Combine coefs, thresholds, incentives, resistances, and profiles into one sample object.
# Input: coefs/thresholds/incentives/resistances CSVs, data/{exp}/conjoint input/ profiles
# Output: prepared_sample .rda files in data/{exp}/prepared_sample/
library(dplyr)
library(data.table)

# for energy policy - sampled product profile
exp="energy_policy"
#exp="messaging_app"

# data loading
# ================================================================================
project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_folder <- file.path(project_root,"data",exp)
input_folder<- file.path(project_root,"data",exp,"conjoint input")
dir.create(file.path(data_folder,"prepared_sample"), recursive = TRUE, showWarnings = FALSE)


# different realization
# ================================================================================
exp_names <- list.files(file.path(data_folder,"coefs"))
# combine all needed data
for (exp_name in exp_names){
  
  # product profiles
  profiles.matrix <- fread(file.path(input_folder,"profiles_matrix.csv"))
  profiles.matrix <- as.matrix(profiles.matrix[, -"V1", with=FALSE], rownames=profiles.matrix$V1)

  if(exp=="energy_policy"){
    # sample product profiles
    sampled_profiles <-fread(file.path(input_folder,"sampled_profiles.csv"))
    profiles_matrix <- profiles.matrix[rownames(profiles.matrix)%in% sampled_profiles$profile,]

    # get sampled thresholds and incentives,resistances
    thresholds<-fread(file.path(data_folder,"thresholds",exp_name))
    incentives<-fread(file.path(data_folder,"incentives",exp_name))
    resistances<-fread(file.path(data_folder,"resistances",exp_name))
    thresholds <- thresholds[profile %in% sampled_profiles$profile,]
    incentives <- incentives[profile %in% sampled_profiles$profile,]
    resistances<- resistances[profile %in% sampled_profiles$profile,]
  }else if(exp=="messaging_app"){
    # get full product profiles
    profiles_matrix <- profiles.matrix
    # get thresholds and incentives
    thresholds<-fread(file.path(data_folder,"thresholds",exp_name))
    incentives<-fread(file.path(data_folder,"incentives",exp_name))
    resistances<-fread(file.path(data_folder,"resistances",exp_name))
  }
  
  # get coefs
  coefs <- fread(file.path(data_folder,"coefs",exp_name))
  # none (effect coding): noneTRUE is always noneTRUE (required by the diffusion pkg)
  # remove: adopters-> social_signal
  setnames(coefs, old = "adopters",new="social_signal")
  profiles_matrix <- profiles_matrix[, !colnames(profiles_matrix) %in% "adopters", drop = FALSE]
  
  coefs$resp.id <- as.character(coefs$resp.id)
  thresholds$resp.id <- as.character(thresholds$resp.id)
  incentives$resp.id <- as.character(incentives$resp.id)
  resistances$resp.id <- as.character(resistances$resp.id)
  
  # exclude id with negative social influence
  # energy policy: 6 real respondents have social_signal < 0 (277 -> 271)
  # messaging app: 0 real respondents have social_signal < 0 (284 -> 284)
  ids_negt <- coefs[social_signal<0,resp.id]
  print(paste0(exp_name," negative id number:",length(ids_negt)))
  coefs<-coefs[!resp.id%in%ids_negt,]
  thresholds<-thresholds[!resp.id%in%ids_negt,]
  incentives<-incentives[!resp.id%in%ids_negt,]
  resistances<-resistances[!resp.id%in%ids_negt,]
  
  # combine all data above
  sample <- c()
  sample$coefs <-coefs %>% as.data.table()
  sample$thresholds <-thresholds %>% as.data.table()
  sample$incentives <- incentives %>% as.data.table()
  sample$resistances <- resistances %>% as.data.table()
  sample$profiles_matrix <- profiles_matrix

  # save the combined data 
  exp_name <- paste0(sub("\\.csv$", "", exp_name),".rda")
  save(sample, file=file.path(data_folder,"prepared_sample",exp_name))

}







