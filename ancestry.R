simulate_pedigree <- function(n_founders = 35, n_generations = 10, offspring_per_pair = 3) {
  set.seed(123)  # reproducibility
  id_counter <- 1
  
  # initialize storage list
  pedigree_list <- vector("list", n_generations + 1)
  
  # Generation 0: Founders
  founders <- data.frame(
    id = generate_unique_ids(n_founders),
    mother_id = NA_character_,
    father_id = NA_character_,
    sex = sample(c("M", "F"), n_founders, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  ids <- founders$id
  
  pedigree_list[[1]] <- founders
  parents <- founders
  
  # progress bar
  pb <- txtProgressBar(min = 0, max = n_generations, style = 3)
  
  for (gen in 1:n_generations) {
    males <- parents[parents$sex == "M", ]
    females <- parents[parents$sex == "F", ]
    
    n_pairs <- min(nrow(males), nrow(females))
    if (n_pairs == 0) {
      setTxtProgressBar(pb, gen)
      break
    }
    
    pairs <- data.frame(
      mother = females$id[sample(1:n_pairs)],
      father = males$id[sample(1:n_pairs)],
      stringsAsFactors = FALSE
    )
    
    offspring <- lapply(1:n_pairs, function(i) {
      data.frame(
        id = generate_unique_ids(offspring_per_pair,existing = ids),
        mother_id = rep(pairs$mother[i], offspring_per_pair),
        father_id = rep(pairs$father[i], offspring_per_pair),
        sex = sample(c("M", "F"), offspring_per_pair, replace = TRUE),
        stringsAsFactors = FALSE
      )
    })
    
    
    offspring_df <- do.call(rbind, offspring)
    
    ids<-c(ids,offspring_df$id)
    
    pedigree_list[[gen + 1]] <- offspring_df
    parents <- offspring_df
    
    setTxtProgressBar(pb, gen)
  }
  
  close(pb)
  
  pedigree <- do.call(rbind, pedigree_list)
  return(pedigree)
}

get_ancestors <- function(ped_df, individual) {
  ancestors <- c()
  current <- individual
  
  while (length(current) > 0) {
    moms <- ped_df$mother_id[ped_df$id %in% current]
    dads <- ped_df$father_id[ped_df$id %in% current]
    parents <- unique(na.omit(c(moms, dads)))
    ancestors <- unique(c(ancestors, parents))
    current <- parents
  }
  
  return(ancestors)
}

# Get average off-diagonal kinship
mean_kin <- function(mat) {
  vals <- mat[lower.tri(mat)]
  mean(vals)
}

library(kinship2)

ped_df<-simulate_pedigree(n_generations = 10,n_founders = 45)
ped <- with(ped_df, pedigree(id = id, momid = mother_id, dadid = father_id, sex = ifelse(sex == "M", 1, 2)))
kin <- kinship(ped)

# Find terminal generation (no offspring)
last_gen_ids <- setdiff(ped_df$id, c(ped_df$mother_id, ped_df$father_id))

# Subset kinship matrix
K <- kin[last_gen_ids, last_gen_ids]
rm(kin)

foo<-function(K,thr,ped_df,tol=1e-3){
require(dplyr)
resu<-numeric()
resu_list<-list()
for(tt in thr){
K2<- abs(K-tt)
# which kinship is closer to target

which_min <- which(K2 < tol, arr.ind = TRUE)  # avoid diagonal (self-kinship)
inds<-c(rownames(K2)[which_min[,1]],colnames(K2)[which_min[,2]])%>%unique


trimmed<-sapply(inds,function(i){get_ancestors(ped_df = ped_df,individual = i)})%>%
  unlist%>%
  c(inds)%>%
  unique()

ped_df2<-ped_df%>%filter(id%in%trimmed)

ped <- with(ped_df2, pedigree(id = id, momid = mother_id, dadid = father_id, sex = ifelse(sex == "M", 1, 2)))

last_gen_ids <- setdiff(ped_df2$id, c(ped_df2$mother_id, ped_df2$father_id))

kin <- kinship(ped)

resu<-c(resu,mean_kin(kin[last_gen_ids,last_gen_ids]))
}         
df<-data.frame(thr=thr,av=resu,inds=paste0(trimmed,collapse=","),no_last_gen=length(last_gen_ids))
     
return(df)  # See how close to 0.1
       
}
# x<-foo(K=K,thr=.1365,ped_df = ped_df)

x<-foo(K=K,thr=seq(from=.09,to=.21,by=0.001),ped_df = ped_df,tol=1e-2)

z<-x%>%mutate(y=abs(av-0.1))%>%
  arrange(y)

z%>%select(thr,y,no_last_gen)%>%head

selected<-stringr::str_split(z$inds[1],",")[[1]]


pedigree_selected<-ped_df%>%
  dplyr::filter(id%in%selected)%>%
  dplyr::mutate(last_gen=id%in%setdiff(id, c(mother_id, father_id)))

sum(pedigree_selected$last_gen)
