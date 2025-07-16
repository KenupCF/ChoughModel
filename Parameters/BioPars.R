

model_pars$bio$inherent<-list()
model_pars$bio$gene<-list()

model_pars$bio$gen$starting_inbreeding<-0.1
model_pars$bio$gen$founder_kinship<-0.1


if(model_pars$sim$parametric_uncertainty){
  model_pars$priors$diploid_eq<-data.frame(min=3,max=15,dist="unif")
}else{
  model_pars$priors$diploid_eq<-data.frame(min=7,max=7,dist="unif")
}

subpops<-model_pars$bio$subpops<-LETTERS[1:5]

NoAgeClasses<-3

NoSubPops<-5

StartN<-c(28,14,19,10,47)
sum(StartN)


SubPopK<-c(100,60,90,90,150)

model_pars$bio$carr_capac_df<-data.frame(subpop=subpops,C=SubPopK)


dispersalMat <- matrix(c(
  35, 15, 25, 0, 25,
  20, 50, 10, 5, 15,
  0, 10, 50, 15, 25,
  0, 0, 0, 50, 50,
  10, 5, 15, 5, 65
), nrow = 5, byrow = TRUE)/100

rownames(dispersalMat) <- colnames(dispersalMat) <- subpops

model_pars$bio$dispersalMat<-dispersalMat

cor_env_repro_surv <- 0
cor_env_among_pops <- 1


## Temporary mock demographic parameters
surv<-.75
fec<-2

#### Genetics

startK<-0.1
startF_p<-0.1

diploidLethalEquivalents<-data.frame(min=3,max=15,dist="unif")

##  Reproduction

failures <- c(35, 35, 32, 40, 45)/100
p_nest_success<- 1 - failures
names(p_nest_success) <- rownames(dispersalMat)

# # Transpose the matrix to make sites rows
nest_success_df <- as.data.frame(p_nest_success)
nest_success_df$subpop <- rownames(nest_success_df)

model_pars$bio$inherent$age_first_breed <- 3
model_pars$bio$inherent$age_last_breed <-	17
model_pars$bio$inherent$max_age <-	17
model_pars$bio$inherent$max_brood_p_year <-	1
model_pars$bio$inherent$max_prog_per_brood <- 5
model_pars$bio$inherent$sex_ratio <-	0.5
model_pars$bio$inherent$prop_fem_breed<-1
model_pars$bio$inherent$prop_mal_breed<-1
model_pars$bio$inherent$av_clutch_size<-4

model_pars$bio$inherent$age_structure <- c(
  0.285663936, 0.183075095, 0.132970019, 0.098160761, 0.074208916,
  0.048653033, 0.038884003, 0.031301492, 0.027974704, 0.018496084,
  0.013611135, 0.009851368, 0.007206196, 0.007697649, 0.007863388,
  0.007421634, 0.006960587
)




#### Mortality handling
{
# Mean values matrix
mortality_mean <- matrix(
  c(
    0.31, 0.25, 0.16, 0.29, 0.29,  # First year survival probability
    26,   26,   26,   26,   26,    # Second year mortality rate
    20,   20,   20,   27,   27     # Adult mortality rate
  ),
  nrow = 3,
  byrow = TRUE
)
  
  mortality_mean[-1,]<-mortality_mean[-1,]/100
  mortality_mean[1,]<-1-mortality_mean[1,]
  
  # Standard deviation matrix
  mortality_sd <- matrix(
    c(
      0.15, 0.15, 0.15, 0.15, 0.15,  # First year survival probability
      17,   17,   17,   17,   17,    # Second year mortality rate
      4,    4,    4,    4,    4      # Adult mortality rate
    ),
    nrow = 3,
    byrow = TRUE
  )
  mortality_sd[-1,]<-mortality_sd[-1,]/100
  
  # Set row and column names
  rownames(mortality_mean) <- rownames(mortality_sd) <- c("m1", "m2", "mad")
  colnames(mortality_mean) <- colnames(mortality_sd) <- c("A", "B", "C", "D", "E")
  
  
  # # Transpose the matrix to make sites rows
  mort_df <- as.data.frame(t(mortality_mean))
  mort_df$subpop <- rownames(mort_df)
  
  # # Transpose the matrix to make sites rows
  mort_sd_df <- as.data.frame(t(mortality_sd))
  mort_sd_df$subpop <- rownames(mort_sd_df)
  
  # Step 1: pivot to long format
  mort_long <- mort_df %>%
    pivot_longer(cols = starts_with("m"), 
                 names_to = "age_class_raw", 
                 values_to = "mortality")
  
  # Step 2: create proper age_class label
  mort_long <- mort_long %>%
    dplyr::mutate(age_class = recode(age_class_raw,
                                     "m1" = "1",
                                     "m2" = "2",
                                     "mad" = "ad")) %>%
    dplyr::select(subpop, age_class, mortality)
  
  # Step 1: pivot to long format
  mort_long_sd <- mort_sd_df %>%
    pivot_longer(cols = starts_with("m"), 
                 names_to = "age_class_raw", 
                 values_to = "mortality_sd")
  
  # Step 2: create proper age_class label
  mort_long_sd <- mort_long_sd %>%
    dplyr::mutate(age_class = recode(age_class_raw,
                                     "m1" = "1",
                                     "m2" = "2",
                                     "mad" = "ad")) %>%
    dplyr::select(subpop, age_class, mortality_sd)
  
  
  survival_df<-left_join(mort_long,mort_long_sd)%>%
    dplyr::mutate(survival=1-mortality)%>%
    dplyr::mutate(
      mortality_logit = logit(mortality),
      mortality_logit_sd = ((1 / (mortality * (1 - mortality)))^2)*(mortality_sd^2))%>%
    dplyr::mutate(
      mean=mortality_logit,
      sd=mortality_logit_sd,
      par=paste(subpop,age_class,sep="_"),dist="norm")%>%
    dplyr::mutate(mean=-mean)%>%
    dplyr::select(mean,sd,dist,par)
  
  survival_sd_df<-survival_df%>%
    dplyr::mutate(mean=sd,sd=0,par=paste0("sd_",par))
  
  
  survival_df<-survival_df%>%
    dplyr::mutate(sd=0)
  
  
  
  ### create distribution descriptions for each age class / subpopulation
  dists<-split(survival_df,survival_df$par)
  names(dists)<-paste("surv",survival_df$par)
  
  dists_sd<-split(survival_sd_df,survival_sd_df$par)
  names(dists_sd)<-paste("surv",survival_sd_df$par)
  
  for(d in seq_along(dists)) {
    
    # Force local evaluation of mean and sd
    dist_mean <- dists[[d]]$mean
    dist_sd <- dists[[d]]$sd
    dist_name <- names(dists)[d]
    
    dist_sd_mean <- dists_sd[[d]]$mean
    dist_sd_sd   <- dists_sd[[d]]$sd
    dist_sd_name <- names(dists_sd)[d]
    
    qFUN[[dist_name]] <- local({
      m <- dist_mean
      s <- dist_sd
      function(x) {
        qnorm(p = x, mean = m, sd = s)
      }
    })
    
    qFUN[[dist_sd_name]] <- local({
      m <- dist_sd_mean
      s <- dist_sd_sd
      function(x) {
        qnorm(p = x, mean = m, sd = s)
      }
    })
    
  }
}

#### Nesting success handling
{
  model_pars$bio$nest_succ_df<-data.frame(B0=logit(p_nest_success),
                                          subpop=names(p_nest_success))
}

#### Brood size handling
{
  # Define subpop labels
  subpops <- LETTERS[1:5]
  
  brood_size_mean <- c(2.6, 2.8, 2.9, 2.5, 2.6)
  
  brood_size_sd <- c(0.9, 1.3, 1.0, 1.1, 1.2)
  
  # Step 1: Create mean and SD data frames
  brood_df <- data.frame(
    subpop = subpops,
    mean = brood_size_mean,
    sd = brood_size_sd
  )
  
  # Step 2: Convert to log-normal distribution parameters using the delta method
  # For log-normal: log(mean^2 / sqrt(sd^2 + mean^2)) and sqrt(log(1 + (sd^2 / mean^2)))
  brood_df <- brood_df %>%
    mutate(
      sd = sqrt(log(1 + (sd^2 / mean^2))),
      mean = log(mean),
    par = paste0("brood_", subpop),
    dist = "norm"
  ) %>%
  dplyr::select(mean, sd, dist, par)


brood_sd_df<-brood_df%>%
  dplyr::mutate(mean=sd,sd=0,par=paste0("sd_",par))

brood_df<-brood_df%>%
  dplyr::mutate(sd=0)

# Step 3: Create named list of distribution rows
brood_dists <- split(brood_df, brood_df$par)
brood_sd_dists <- split(brood_sd_df, brood_sd_df$par)

# Step 4: Add quantile functions to qFUN
for (d in seq_along(brood_dists)) {
  
  # Force local evaluation of mean and sd
  dist_mean <- brood_dists[[d]]$mean
  dist_sd <- brood_dists[[d]]$sd
  dist_name <- names(brood_dists)[d]
  
  dist_mean_sd <- brood_sd_dists[[d]]$mean
  dist_sd_sd   <- brood_sd_dists[[d]]$sd
  dist_name_sd <- names(brood_sd_dists)[d]
  
  
  qFUN[[dist_name]] <- local({
    m <- dist_mean
    s <- dist_sd
    function(x) {
      qnorm(p = x, mean = m, sd = s)
    }
  })
  
  qFUN[[dist_name_sd]] <- local({
    m <- dist_mean_sd
    s <- dist_sd_sd
    function(x) {
      qnorm(p = x, mean = m, sd = s)
    }
  })
}
}






