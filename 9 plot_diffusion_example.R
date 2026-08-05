# Description: Simulate network diffusion (mixed real/AI respondents, one seeding strategy) over
#   a single AddHealth network, for one AI model, one product profile, and 6 AI-proportion values.
#   Minimal example derived from "4 run_diffusion.R" -- see that script for the full sweep over
#   all networks/models/profiles/seeding strategies.
# Input: prepared_sample .rda files, data/network_addhealth/ network CSVs + sampled_nets.rda
# Output: diffusion_example.csv in data/{exp}/diffusion/, plus a cumulative-adopters-by-timestep
#   plot (diffusion_example_cumulative_adopters.png) built from that CSV

library(igraph)
library(data.table)
library(dplyr)
library(moments)
library(diffuNet)
library(FinancialMath)
library(ids)
library(ggplot2)
set.seed(123)

# setting
# ================================================================================
exp="messaging_app"
#exp="energy_policy"

project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)

source(file.path(project_root,"functions",'0_2 seeding_function.R'))
source(file.path(project_root,"functions",'0_3 diffusion_function.R'))

data_folder <- file.path(project_root,"data",exp)

output_folder <- file.path(project_root,"data")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# get prepared sample files
sample_data_folder <-  paste0(file.path(data_folder, "prepared_sample"))
sample_files <-list.files(sample_data_folder)
real_sample_file <- grep("real",sample_files,value=TRUE)
ai_sample_file <- "app_gpt3.5_temp1.rda"

# load net file -- smallest of the 18 sampled networks (25 nodes), only network used here
net_folder <- file.path(project_root,"data","network_addhealth","AddHealth_Networks_Largest_Components")
net_file <- "addhealth_net_75.csv"

r_seeds = 0.01
mapping_time = 1
ai_percents =c(0,0.2,0.4,0.6,0.8,1)

# load empirical net
#===============================================================
gmat <- fread(file.path(net_folder,net_file)) %>% as.matrix()
rownames(gmat)<-colnames(gmat)
g<-graph_from_adjacency_matrix(gmat,mode="undirected")
n <- nrow(gmat)
n_seeds <- ceiling(n*r_seeds)
ids <- colnames(gmat)

#---------data loading and preprocessing------------
#===============================================================
# load sample
load(file = file.path(sample_data_folder, real_sample_file))
real_sample <- copy(sample)
load(file = file.path(sample_data_folder, ai_sample_file))
ai_sample <- copy(sample)

# sampling from real participants and ai
for (ai_percent in ai_percents){

  mixed_sample <- mix_sample(n, real_sample, ai_sample, ai_percent = ai_percent)
  mixed_ids <- mixed_sample$coefs$mix_id

  # record net_sample_id
  net_sample_id <- ids::uuid(1,drop_hyphens = TRUE,use_time = TRUE)

  # ----------------mapping---------------------
  mapping <- sample(1:n, n)

  # record mapping dictionary
  mapping_dic <- data.table(node_id = ids, mix_id = mixed_ids[mapping])
  # get mapped_coefs
  coefs <- merge(mapping_dic, mixed_sample$coefs, by = 'mix_id', all.x = TRUE, allow.cartesian = TRUE)# key_x might duplicate, key_y is unique
  coefs$mix_id <- NULL; coefs$resp.id <- NULL;
  coefs <- coefs %>% setnames(old='node_id', new='resp.id') %>% .sort_ids(sort_on = ids, get="datatable")
  # get profiles_matrix
  profiles_matrix <- mixed_sample$profiles_matrix
  # single product profile for this example
  profile_name <- "web|simple|low|multi_person"

  product_profile <- t(profiles_matrix[profile_name,]) %>% as.data.table()
  product_profile$profile <- profile_name
  setcolorder(product_profile, c("profile", names(product_profile)[!names(product_profile) %in% "profile"]))
  product_profile$profile <- 1

  # find seeds for the seeding strategy
  #--------------select top nodes------------------
  print("start seed set selection")
  simple_centralities_df <- get_simple_centralities(g)

  # one entry per seeding strategy -- add/remove a strategy by editing only this list
  top_ids_list <- list(
    degree      = ids[find_seeds_index(n_seeds, simple_centralities_df[['degree']])]
  )
  seed_sets <- lapply(top_ids_list, seedIds2dt, ids = ids, profile_name = profile_name)

  #--------------run diffusion------------------
  print("running diffusions")
  results <- lapply(seed_sets, function(seeds) {
    diffuNet::run_diffusion(g = gmat, n_product = 1, seeds = seeds, decisionOpportunity = TRUE,
                             exposureCon = TRUE, deviation = FALSE, t_max = NULL,
                             stop_on_stable_state = 2, social_value = "ratio",
                             decision_function = utility_based_decision,
                             product_profile = product_profile, coefs)
  })

  new_adopters_flow_list <- lapply(results, function(r) {
    flow <- c(sum(r$adoption_matrix[, 1]), r$new_adopters_flow[1, ])
    paste(flow, collapse = ",")
  })
  accumulated_adopters_flow_list <- lapply(results, function(r) {
    paste(colSums(r$adoption_matrix), collapse = ",")
  })

  # flatten each named list 
  strategy_cols <- c(
    suffix_named(new_adopters_flow_list, "new_adopters_flow"),
    suffix_named(accumulated_adopters_flow_list, "accumulated_adopters_flow")
  )

  rs <- data.table(net_file=net_file,n=n,
                   real_sample_file=real_sample_file,
                   ai_sample_file=ai_sample_file,
                   net_sample_id=net_sample_id,
                   ai_percent=ai_percent,
                   mapping_times =mapping_time
  )
  rs <- cbind(rs, as.data.table(strategy_cols))

  append2csv(output_folder, filename='diffusion_example.csv', rs)
}

