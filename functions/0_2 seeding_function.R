# Description: Seed-selection helpers - centrality/behavioral scoring, seed picking, incentive cost adjustment.
# Input: igraph graph, coefs/thresholds data.table
# Output: seed id vectors / data.table

library(data.table)

.sort_ids <- function(data, id_colnames= "resp.id",sort_on = resp.ids, get="matrix"){
  data = data %>% as.data.table()

  #id_factor_col = data[, .SD, .SDcols = id_colnames][[1]] %>% factor(levels = sort_on)
  id_factor_col = data[, ..id_colnames][[1]] %>% factor(levels = sort_on)
  #id_factor_col = data[, (id_colnames)] %>% factor(levels = sort_on)
  data[, id_factor := id_factor_col]
  data <- data[order(id_factor)]
  data[, id_factor := NULL];
  data[,(id_colnames) :=NULL]
  if(get=="matrix"){
    rs <- data %>% as.matrix()
    rownames(rs) <- sort_on
  }else if(get=="datatable"){
    data[,(id_colnames) :=sort_on]
    rs <- data
  }
  return(rs)
}


get_simple_centralities<-function(g){
  centrality_df<-data.table(resp.id=V(g)$name,
                            degree = as.numeric(igraph::degree(g)),
                            betweenness = as.numeric(igraph::betweenness(g)),
                            closeness = as.numeric(igraph::closeness(g)),
                            eigen = as.numeric(eigen_centrality(g)$vector))
  return(centrality_df)
}


neighborhood.susceptibility<-function(g,thresholds){
  n<-vcount(g);
  all_degrees <- degree(g, mode = "all")
  # neighsusc vector to store how many susceptiable neighbors one node has
  neighsusc<-replicate(n,0)
  for (i in 1:n){
    neigh_ids <- neighbors(g, i) %>% unname()
    # if neighbor's threshold <= 1/(focal neighbor's degree)
    condition_met <- (thresholds[neigh_ids] <= 1 / all_degrees[neigh_ids])
    neighsusc[i] <- sum(condition_met)

  }
  return(neighsusc)
}

get_simple_behavioral<-function(g, thresholds){
  ids<-V(g)$name
  centrality_df<-data.table(
    resp.id=ids,
    thresholds=thresholds%>%as.vector(),
    neigh_susc=neighborhood.susceptibility(g, thresholds) %>% as.numeric()
  )
  return(centrality_df)
}


find_seeds_index<-function(n, to_rank){
  if(n>length(to_rank)){stop("number of seeds exceeded the allowed maximum")}
  r<-base::rank(to_rank, ties.method = 'random')
  seeds_index <- which(r > length(to_rank)-n )

  return(seeds_index)
}

seedIds2dt <- function(seeded_ids, ids, profile_name){
  seeded_ids= seeded_ids %>% as.character()
  rs= data.table(resp.id=ids %>% as.character(),
                 product=ifelse(ids %in% seeded_ids, 1,0))
  setnames(rs, old = "product",new=profile_name)
  return(rs)
}

getCost <- function(data){
  # Set epsilon
  epsilon=0.
  
  # Adjust incentive
  data[incentive>=0, incentive:=incentive + epsilon]
  data[incentive<0, incentive:=epsilon]
  
  # Return
 return(data) 
}

get_NoSelfAdopters <- function(ids, coefs, product_profile, adoption_matrix ){
  # compute the self-adopters with social_signal=0
  social_signal_dt = data.table(resp.id=ids,
                                social_signal=0,
                                t="t0",
                                profile=1)
  decision_social0 <- diffuNet::utility_based_decision(social_signal =social_signal_dt, product_profile, coefs, error.var.scale=0)
  decision_social0 <- decision_social0[ids]  # sort based on ids
  
  # get the final adopters from the diffusion result
  adopted_list <- adoption_matrix[,ncol(adoption_matrix)][ids]  # sort based on ids
  
  # compare the diffusion result and self_adopters
  NoSelfAdopters=sum(adopted_list-decision_social0==1)
  return(NoSelfAdopters)
}
