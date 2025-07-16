#--------------------------------------------#
# Function: generate_related_founders
#--------------------------------------------#
# Simulates a population of founder individuals that are 
# full siblings from a limited number of shared parent pairs,
# in order to achieve a target average pairwise kinship (K) 
# of approximately 0.1.
#
# Args:
#   n_founders:       Total number of founders to generate
#   n_family_pairs:   Number of unrelated dam–sire pairs to generate founders from
#   target_K:         Unused directly, but here for reference/future tuning
#   id_prefix:        Prefix used to name founder IDs
#   seed:             Random seed for reproducibility
#
# Returns:
#   A list with:
#     - founders: tibble of founder individuals and their parents
#     - ancestors: tibble of sire and dam individuals (unrelated)
#     - pedigree_df: full pedigree including ancestors and founders
#     - kinship_matrix: kinship matrix for all individuals
#     - mean_kinship: average pairwise K among founders
#--------------------------------------------#
generate_related_founders <- function(n_founders = 100,
                                      n_family_pairs = 10,
                                      target_K = 0.1,
                                      id_prefix = "F",
                                      seed = NULL) {
  require(dplyr)
  require(kinship2)
  
  # Ensure reproducibility
  if(!is.null(seed)){set.seed(seed)}
  
  #----------------------------------#
  # 1. Create Unique Sire and Dam IDs
  #----------------------------------#
  sire_ids <- paste0("S", seq_len(n_family_pairs))  # e.g., S1 to S10
  dam_ids  <- paste0("D", seq_len(n_family_pairs))  # e.g., D1 to D10
  
  # Create unrelated male ancestors (sires)
  sires <- tibble(
    id = sire_ids,
    sex = "M",
    mother_id = NA_character_,
    father_id = NA_character_
  )
  
  # Create unrelated female ancestors (dams)
  dams <- tibble(
    id = dam_ids,
    sex = "F",
    mother_id = NA_character_,
    father_id = NA_character_
  )
  
  # Combine all ancestors into a single dataframe
  ancestors <- bind_rows(sires, dams)
  
  #--------------------------------------------------#
  # 2. Allocate Founder Individuals Across Families
  #--------------------------------------------------#
  # Evenly distribute founders across the n_family_pairs
  founders_per_family <- rep(floor(n_founders / n_family_pairs), n_family_pairs)
  
  # Add any leftover individuals due to rounding
  remainder <- n_founders - sum(founders_per_family)
  founders_per_family[1:remainder] <- founders_per_family[1:remainder] + 1
  
  #--------------------------------------------------#
  # 3. Generate Founders from Each Parent Pair
  #--------------------------------------------------#
  founders <- purrr::map2_dfr(
    sire_ids,
    dam_ids,
    function(sire, dam) {
      # How many offspring this pair should produce
      n_offspring <- founders_per_family[which(sire_ids == sire)]
      
      # Create full-sib founders
      tibble(
        id = paste0(id_prefix, "_", sire, "_", seq_len(n_offspring)),  # e.g., F_S1_1
        sex = sample(c("M", "F"), n_offspring, replace = TRUE),        # Random sex
        mother_id = dam,
        father_id = sire
      )
    }
  )
  
  #--------------------------------------------------#
  # 4. Assemble Pedigree Table for Kinship Calculation
  #--------------------------------------------------#
  all_indivs <- bind_rows(ancestors, founders) %>%
    mutate(sex_code = ifelse(sex == "M", 1, 2))  # Required coding for kinship2
  
  #--------------------------------------------------#
  # 5. Create Pedigree Object and Kinship Matrix
  #--------------------------------------------------#
  ped <- with(all_indivs, pedigree(id = id, momid = mother_id, dadid = father_id, sex = sex_code))
  K <- kinship(ped)
  
  #--------------------------------------------------#
  # 6. Calculate Mean Pairwise Kinship Among Founders
  #--------------------------------------------------#
  founder_ids <- founders$id
  K_founders <- K[founder_ids, founder_ids]              # Subset to founders only
  mean_K <- mean(K_founders[lower.tri(K_founders)])      # Mean of lower triangle (exclude self-kinship)
  
  message(sprintf("Mean pairwise kinship among founders: %.3f", mean_K))
  
  #--------------------------------------------------#
  # 7. Return Structured Output
  #--------------------------------------------------#
  return(list(
    founders = founders,
    ancestors = ancestors,
    pedigree_df = all_indivs,
    kinship_matrix = K,
    mean_kinship = mean_K
  ))
}

simulate_pedigree_generations <- function(n_generations = 3,
                                          founders_n = 100,
                                          offspring_n_per_gen = 3,
                                          n_family_pairs = 10,
                                          id_prefix = "ID",
                                          seed = NULL) {
  require(dplyr)
  require(purrr)
  require(kinship2)
  
  if (!is.null(seed)) set.seed(seed)
  
  all_indivs <- list()  # List to hold individuals from each generation
  
  #------------------------------#
  # 1. Create Founders (Gen 0)
  #------------------------------#
  sire_ids <- paste0("S", seq_len(n_family_pairs))
  dam_ids  <- paste0("D", seq_len(n_family_pairs))
  
  sires <- tibble(
    id = sire_ids,
    sex = "M",
    mother_id = NA,
    father_id = NA,
    generation = 0
  )
  
  dams <- tibble(
    id = dam_ids,
    sex = "F",
    mother_id = NA,
    father_id = NA,
    generation = 0
  )
  
  ancestors <- bind_rows(sires, dams)
  
  founders_per_family <- rep(floor(founders_n / n_family_pairs), n_family_pairs)
  remainder <- founders_n - sum(founders_per_family)
  founders_per_family[1:remainder] <- founders_per_family[1:remainder] + 1
  
  founders <- map2_dfr(
    sire_ids, dam_ids,
    function(sire, dam) {
      n_offspring <- founders_per_family[which(sire_ids == sire)]
      tibble(
        id = paste0(id_prefix, "_G1_", sire, "_", seq_len(n_offspring)),
        sex = sample(c("M", "F"), n_offspring, replace = TRUE),
        mother_id = dam,
        father_id = sire,
        generation = 1
      )
    }
  )
  
  all_indivs[[1]] <- bind_rows(ancestors, founders)
  
  #------------------------------#
  # 2. Loop Over Generations
  #------------------------------#
  current_gen <- founders
  for (g in 2:n_generations) {
    # Split current_gen into males and females
    males <- current_gen %>% filter(sex == "M")
    females <- current_gen %>% filter(sex == "F")
    
    n_pairs <- min(nrow(males), nrow(females), n_family_pairs)
    if (n_pairs == 0) break  # Can't breed
    
    # Randomly assign pairs
    mating_pairs <- tibble(
      sire = sample(males$id, n_pairs),
      dam = sample(females$id, n_pairs)
    )
    
    # Determine offspring per pair
    offspring_per_pair <- rep(floor(offspring_n_per_gen / n_pairs), n_pairs)
    extra <- offspring_n_per_gen - sum(offspring_per_pair)
    if (extra > 0) offspring_per_pair[1:extra] <- offspring_per_pair[1:extra] + 1
    
    # Generate offspring
    gen_offspring <- pmap_dfr(
      list(mating_pairs$sire, mating_pairs$dam, offspring_per_pair),
      function(sire, dam, n_off) {
        tibble(
          id = paste0(id_prefix, "_G", g, "_", sire, "_", seq_len(n_off)),
          sex = sample(c("M", "F"), n_off, replace = TRUE),
          mother_id = dam,
          father_id = sire,
          generation = g
        )
      }
    )
    
    current_gen <- gen_offspring
    all_indivs[[g]] <- gen_offspring
  }
  
  #------------------------------#
  # 3. Combine and Calculate Kinship
  #------------------------------#
  full_pedigree <- bind_rows(all_indivs) %>%
    mutate(sex_code = ifelse(sex == "M", 1, 2))
  
  ped <- with(full_pedigree, pedigree(id = id, momid = mother_id, dadid = father_id, sex = sex_code))
  K <- kinship(ped)
  
  # Optionally calculate mean kinship among last generation
  last_gen_ids <- current_gen$id
  mean_K_last <- mean(K[last_gen_ids, last_gen_ids][lower.tri(K[last_gen_ids, last_gen_ids])])
  message(sprintf("Mean pairwise kinship in generation %d: %.3f", n_generations, mean_K_last))
  
  return(list(
    pedigree_df = full_pedigree,
    kinship_matrix = K,
    mean_kinship_last_gen = mean_K_last
  ))
}




test<-expand.grid(n_generations=2:4,founders_n=c(50,100),offspring_n_per_gen=c(50),n_family_pairs=sum(StartN))

meanK<-numeric()

result_list<-list()
for( i in 1:nrow(test)){

  result_list[[i]] <- simulate_pedigree_generations(n_generations = test$n_generations[i], 
                                        founders_n = test$founders_n[i],
                                        offspring_n_per_gen = test$offspring_n_per_gen[i],
                                        n_family_pairs = test$n_family_pairs[i],
                                        seed = 101)
  meanK[i]<-result_list[[i]]$mean_kinship_last_gen

  }
test$meanK<-meanK

last_gen_ids<-result_list[[5]]$pedigree_df%>%filter(generation==max(generation))%>%pull(id)
result_list[[5]]$kinship_matrix[]








result <- simulate_pedigree_generations(n_generations = 3, 
                                        founders_n = 50,
                                        offspring_n_per_gen = 8,
                                        n_family_pairs = 6,
                                        seed = 101)

head(result$pedigree_df)

result$mean_kinship_last_gen























no_ancestors_eval<-1:20
target_K<-.1
result_list<-lapply(no_ancestors_eval,function(n){generate_related_founders(n_founders = sum(StartN)/2,n_family_pairs = n,target_K = target_K)})

data.frame(meanK=sapply(result_list,function(x){x$mean_kinship}),
no_ancestors=no_ancestors_eval)%>%
  dplyr::mutate(err=abs(meanK-target_K))


generate_related_founders(n_founders = 14, target_K = 0.1)

founders <- result$founders
K <- result$kinship_matrix

# Check inbreeding of each founder (F = 2k_ii - 1)
inb <- 2 * diag(K)[founders$id] - 1
head(inb)











library(kinship2)

# Create founders with assumed pairwise kinship of 0.1 (conceptual; not explicitly encoded)
founders <- data.frame(
  id = paste0("F", 1:10),
  dadid = NA,
  momid = NA,
  sex = rep(c("male", "female"), length.out = 10)
)

# Create F1 individuals as offspring from different founder pairs
f1 <- data.frame(
  id = paste0("I", 1:10),
  dadid = founders$id[seq(1, 10, by = 2)],   # F1, F3, F5, F7, F9
  momid = founders$id[seq(2, 10, by = 2)],   # F2, F4, F6, F8, F10
  sex = rep(c("male", "female"), length.out = 10)
)

# Combine all individuals into one pedigree
ped_df <- rbind(founders, f1)

# Convert sex to numeric code as required by kinship2 (1 = male, 2 = female)
ped_df$sex_code <- ifelse(ped_df$sex == "male", 1, 2)

# Create pedigree object
ped <- with(ped_df, pedigree(id = id, dadid = dadid, momid = momid, sex = sex_code))

# Compute kinship matrix
K <- kinship(ped)

# Extract F1 individual IDs
f1_ids <- f1$id

# Get mean kinship between distinct F1 pairs
kin_f1 <- K[f1_ids, f1_ids]
mean_kin_f1 <- mean(kin_f1[lower.tri(kin_f1)])

# Expected inbreeding coefficient of offspring from F1 pair
expected_Fi <- 2 * mean_kin_f1

cat("Expected Fi from two random F1 individuals:", round(expected_Fi, 4), "\n")

