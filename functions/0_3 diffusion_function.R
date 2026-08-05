# Description: Helpers for the diffusion run - mix real/AI respondents, append results to CSV, flatten named lists into columns.
# Input: prepared_sample coefs/thresholds/incentives/resistances lists
# Output: mixed sample list / appended CSV / named vector

library(data.table)
library(dplyr)

# mix ai and real participant
mix_sample <- function(n, real_sample, ai_sample, ai_percent=0.1){
  # count the sampled number
  n_ai = round(n*ai_percent,digits = 0)
  n_real = n-n_ai
  # get the whole resp.ids
  real_resp_ids <- real_sample$coefs$resp.id
  ai_resp_ids <- ai_sample$coefs$resp.id
  # sample from real and ai
  if(n_real>0){
    sampled_real <- data.table(mix_id=paste0("mix",1:n_real),
                               numeric_mix_id=1:n_real,
                               resp.id=sample(x=real_resp_ids, size=n_real, replace = TRUE)%>%as.character())
  }else{
    sampled_real <- data.table(mix_id = character(0), numeric_mix_id = integer(0), resp.id = character(0))
  }
  if(n_ai>0){
  sampled_ai <- data.table(mix_id=paste0("mix",(n_real+1):n),
                            numeric_mix_id=(n_real+1):n,
                            resp.id=sample(x=ai_resp_ids, size=n_ai, replace = TRUE)%>%as.character() )
  }else{
    sampled_ai <- data.table(mix_id = character(0), numeric_mix_id = integer(0), resp.id = character(0))
  }
  # combine the sampled ids
  mix_sample<-list()
  mix_sample$coefs <- rbind(merge(sampled_real,real_sample$coefs, by = 'resp.id', all.x = TRUE, allow.cartesian = TRUE),
                            merge(sampled_ai,ai_sample$coefs, by = 'resp.id', all.x = TRUE, allow.cartesian = TRUE) )
  mix_sample$thresholds <-  rbind(merge(sampled_real,real_sample$thresholds, by = 'resp.id', all.x = TRUE, allow.cartesian = TRUE),
                                  merge(sampled_ai,ai_sample$thresholds, by = 'resp.id', all.x = TRUE, allow.cartesian = TRUE) )
  mix_sample$incentives <-  rbind(merge(sampled_real,real_sample$incentives, by = 'resp.id', all.x = TRUE, allow.cartesian = TRUE),
                                  merge(sampled_ai,ai_sample$incentives, by = 'resp.id', all.x = TRUE, allow.cartesian = TRUE) )
  mix_sample$resistances <- rbind(merge(sampled_real,real_sample$resistances, by = 'resp.id', all.x = TRUE, allow.cartesian = TRUE),
                                  merge(sampled_ai,ai_sample$resistances, by = 'resp.id', all.x = TRUE, allow.cartesian = TRUE) )
  if(!identical(real_sample$profiles_matrix,ai_sample$profiles_matrix)){
    stop(print("profiles_matrix between real_sample and ai_sample is not identical"))
  }
  mix_sample$profiles_matrix <- real_sample$profiles_matrix

  # sort based on numeric_mix_id
  setorder( mix_sample$coefs, numeric_mix_id)
  setorder( mix_sample$thresholds, numeric_mix_id)
  setorder( mix_sample$incentives, numeric_mix_id)
  setorder( mix_sample$resistances, numeric_mix_id)
  # remove unnecessary columns
  mix_sample$coefs[,numeric_mix_id:=NULL]
  mix_sample$thresholds[,numeric_mix_id:=NULL]
  mix_sample$incentives[,numeric_mix_id:=NULL]
  mix_sample$resistances[,numeric_mix_id:=NULL]


  return(mix_sample)
}

# optionally append the data into existing csv
append2csv <- function(folder_path, filename, new_data) {
  file_path <- file.path(folder_path, filename)
  # Check if the CSV file exists: append if exist
  if (file.exists(file_path)) {
    existing_data <- fread(file_path)
    # update old value of stats if needed
    #existing_data <- existing_data[!file %in% new_data$file,]
    # combine
    full_data <- rbindlist(list(existing_data, new_data))
  } else {
    full_data <- new_data
  }
  fwrite(full_data, file = file_path)
}

# flatten a named list into a vector with "<prefix>_<name>" element names
suffix_named <- function(x, prefix) setNames(x, paste0(prefix, "_", names(x)))
