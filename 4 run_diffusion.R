# Description: Simulate network diffusion (mixed real/AI respondents, multiple seeding strategies) over AddHealth networks.
# Input: prepared_sample .rda files, data/network_addhealth/ network CSVs + sampled_nets.rda
# Output: diffusion result CSV in data/{exp}/diffusion/

library(igraph)
library(data.table)
library(dplyr)
library(moments)
library(ggplot2)
library(cowplot)
library(tidyr)
library(ggsignif)
#remotes::install_github("HiMifeng/diffuNet", force = TRUE)
library(diffuNet)
library(FinancialMath)
library(grid)
library(gridExtra)
library(ids)
set.seed(123)

# setting
# ================================================================================
#exp="messaging_app"
exp="energy_policy"

project_root <- dirname(rstudioapi::getActiveDocumentContext()$path)

source(file.path(project_root,"functions",'0_2 seeding_function.R'))
source(file.path(project_root,"functions",'0_3 diffusion_function.R'))

data_folder <- file.path(project_root,"data",exp)

output_folder <- file.path(data_folder,"diffusion")
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# get prepared sample files
sample_data_folder <-  paste0(file.path(data_folder, "prepared_sample"))
sample_files <-list.files(sample_data_folder)
real_sample_file <- grep("real",sample_files,value=TRUE)
# exclude the "sm_"-prefixed SM prompt-variant samples (MatchedSet/ShuffledOrder/Compressed/NoMemory) -- diffusion only uses the main-text samples
ai_sample_files <- grep("^(?!sm_).*temp1.*$", sample_files, value = TRUE, perl = TRUE)


# load net files
net_folder <- file.path(project_root,"data","network_addhealth","AddHealth_Networks_Largest_Components")
net_files_all <- list.files(net_folder)
# filter net_files
load(file.path(project_root,"data","network_addhealth","sampled_nets.rda"))
sampled_nets <- sampled_nets %>% unlist() %>% unname()

r_seeds = 0.01
sampling_times=1
mapping_times = 1
ai_percents =c(0,0.2,0.4,0.6,0.8,1)
for (net_file in  sampled_nets){

  # load empirical net
  #===============================================================
  gmat <- fread(file.path(net_folder,net_file)) %>% as.matrix()
  rownames(gmat)<-colnames(gmat)
  g<-graph_from_adjacency_matrix(gmat,mode="undirected")
  n <- nrow(gmat)
  n_seeds <- ceiling(n*r_seeds)
  ids <- colnames(gmat)

  for(ai_sample_file in ai_sample_files){
    ai_sample_name <- sub("\\.rda$", "", ai_sample_file)
    real_sample_name <- sub("\\.rda$", "", real_sample_file)
    timestamp <- date()

    # data loading and preprocessing
    #===============================================================
    # load sample
    load(file = file.path(sample_data_folder, real_sample_file))
    real_sample <- copy(sample)
    load(file = file.path(sample_data_folder, ai_sample_file))
    ai_sample <- copy(sample)

    for (ai_percent in ai_percents){
      for (samping_time in 1: sampling_times){
        # sampling from real participants and ai
        mixed_sample <- mix_sample(n, real_sample, ai_sample, ai_percent = ai_percent) # replace=TRUE is set inside mix_sample() because n_net might be larger than n_sample 
        mixed_ids <- mixed_sample$coefs$mix_id
        # record net_sample_id
        net_sample_id <- ids::uuid(1,drop_hyphens = TRUE,use_time = TRUE)

        for (mapping_time in 1:mapping_times) {

          #--------------------- mapping-----------------------------------
          mapping <- sample(1:n, n)
          
          # record mapping dictionary
          mapping_dic <- data.table(node_id = ids, mix_id = mixed_ids[mapping])

          # get mapped_coefs
          coefs <- merge(mapping_dic, mixed_sample$coefs, by = 'mix_id', all.x = TRUE, allow.cartesian = TRUE)# key_x might duplicate, key_y is unique
          coefs$mix_id <- NULL; coefs$resp.id <- NULL;
          # use node_id as the index for the diffusion package (required to rename as resp.id)
          coefs <- coefs %>% setnames(old='node_id', new='resp.id') %>% .sort_ids(sort_on = ids, get="datatable")

          # get mapped_thresholds; label id with node_id instead of initial resp.id
          thresholds<-merge(mapping_dic, mixed_sample$thresholds, by = 'mix_id', all.x = TRUE, allow.cartesian = TRUE)#
          thresholds$mix_id<-NULL; thresholds$resp.id <- NULL;
          thresholds <- thresholds %>% setnames(old='node_id', new='resp.id')

          thresholds_mean <- mean(thresholds$threshold)
          thresholds_var <- var(thresholds$threshold)
          thresholds_skewness <- skewness(thresholds$threshold)
          thresholds_kurtosis <- kurtosis(thresholds$threshold)
          thresholds_value0 <- sum(thresholds$threshold<=0)/nrow(thresholds)
          thresholds_value1 <- sum(thresholds$threshold>=1)/nrow(thresholds)

          # get mapped_incentives (cost=0 when incentive<0); label id with node_id instead of initial resp.id
          incentives <- getCost(mixed_sample$incentives)
          incentives <- merge(mapping_dic, incentives, by = 'mix_id', all.x = TRUE, allow.cartesian = TRUE) # key_x might duplicate, key_y is unique
          incentives$mix_id <- NULL; incentives$resp.id <- NULL;
          incentives <- incentives %>% setnames(old='node_id', new='resp.id')

          incentives_mean <- mean(incentives$incentive)
          incentives_var <- var(incentives$incentive)
          incentives_skewness <- skewness(incentives$incentive)
          incentives_kurtosis <- kurtosis(incentives$incentive)
          incentives_value0 <- sum(incentives$incentive<=0)/nrow(incentives)

          # get profiles_matrix
          profiles_matrix <- mixed_sample$profiles_matrix
          profile_names <- rownames(profiles_matrix)
          n_product<- length(profile_names)

          # ---------------------run each time for only one product--------------
          for (p in 1:n_product){
            profile_name <- profile_names[p]
            product_profile <- t(profiles_matrix[profile_name,]) %>% as.data.table()
            product_profile$profile <- profile_name
            setcolorder(product_profile, c("profile", names(product_profile)[!names(product_profile) %in% "profile"]))
            product_profile$profile <- 1

            gt_thresholds_p <- thresholds[profile==profile_name, c("resp.id","threshold")] %>%as.data.table()
            gt_thresholds_p <- .sort_ids(gt_thresholds_p,id_colnames = "resp.id",sort_on = ids)

            thresholds_p_mean <- mean(gt_thresholds_p)
            thresholds_p_var <- var(gt_thresholds_p)[1]
            thresholds_p_skewness <- skewness(gt_thresholds_p)
            thresholds_p_kurtosis <- kurtosis(gt_thresholds_p)
            thresholds_p_value0 <- sum(gt_thresholds_p<=0)/n
            thresholds_p_value1 <- sum(gt_thresholds_p>=1)/n

            # get incentives_p
            gt_incentives_p <- incentives[profile==profile_name, c("resp.id","incentive")] %>%as.data.table()
            gt_incentives_p <- .sort_ids(gt_incentives_p,id_colnames = "resp.id",sort_on = ids)

            incentives_p_mean <- mean(gt_incentives_p)
            incentives_p_var <- var(gt_incentives_p)[1]
            incentives_p_skewness <- skewness(gt_incentives_p)
            incentives_p_kurtosis <- kurtosis(gt_incentives_p)
            incentives_p_value0 <- sum(gt_incentives_p==0)/n

            # find seeds for different seeding strategies
            #--------------select top nodes------------------
            print("start seed set selection")
            #store centrality and behavioral information
            simple_centralities_df <- get_simple_centralities(g)
            simple_behavioral_df <- get_simple_behavioral(g, gt_thresholds_p)

            # one entry per seeding strategy -- add/remove a strategy by editing only this list
            top_ids_list <- list(
              degree      = ids[find_seeds_index(n_seeds, simple_centralities_df[['degree']])],
                low_th      = ids[find_seeds_index(n_seeds, -simple_behavioral_df[['thresholds']])],
              neigh_susc  = ids[find_seeds_index(n_seeds, simple_behavioral_df[['neigh_susc']])],
              rand        = sample(ids, size = n_seeds, replace = FALSE)
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

            # per-strategy summary metrics
            max_t_list <- lapply(results, function(r) ncol(r$adoption_matrix) - 1)
            profit_list <- lapply(results, function(r) r$num_adopters[["p1"]])
            NoSelfAdopters_list <- lapply(results, function(r) get_NoSelfAdopters(ids, coefs, product_profile, r$adoption_matrix))
            strategy_cols <- c(
              suffix_named(max_t_list, "max_t"),
              suffix_named(profit_list, "profit"),
              suffix_named(NoSelfAdopters_list, "NoSelfAdopters")
            )

            rs <- data.table(net_file=net_file,n=n,
                             real_sample_file=real_sample_file,
                             ai_sample_file=ai_sample_file,
                             net_sample_id=net_sample_id,
                             ai_percent=ai_percent,
                             mapping_times =mapping_time,

                             thresholds_mean=thresholds_mean,
                             thresholds_var = thresholds_var,
                             thresholds_skewness =thresholds_skewness,
                             thresholds_kurtosis = thresholds_kurtosis,
                             thresholds_value0=thresholds_value0,
                             thresholds_value1=thresholds_value1,

                             incentives_mean=incentives_mean,
                             incentives_var = incentives_var,
                             incentives_skewness =incentives_skewness,
                             incentives_kurtosis = incentives_kurtosis,
                             incentives_value0 = incentives_value0,

                             n_seeds=n_seeds,
                             n_product=1, profile_name=profile_name,
                             thresholds_p_mean=thresholds_p_mean,
                             thresholds_p_var = thresholds_p_var,
                             thresholds_p_skewness =thresholds_p_skewness,
                             thresholds_p_kurtosis = thresholds_p_kurtosis,
                             thresholds_p_value0 =thresholds_p_value0,
                             thresholds_p_value1 =thresholds_p_value1,

                             incentives_p_mean=incentives_p_mean,
                             incentives_p_var = incentives_p_var,
                             incentives_p_skewness =incentives_p_skewness,
                             incentives_p_kurtosis = incentives_p_kurtosis,
                             incentives_p_value0 =incentives_p_value0
            )
            rs <- cbind(rs, as.data.table(strategy_cols))

            append2csv(output_folder, filename=paste0(exp,'_diffusion_result.csv'), rs)
          }
        }

      }
    }
  }
}


