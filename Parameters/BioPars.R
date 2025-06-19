
NoAgeClasses<-3

NoSubPops<-5

StartN<-c(28,14,19,10,47)

SubPopK<-c(100,60,90,90,150)


dispersalMat <- matrix(c(
  35, 15, 25, 0, 25,
  20, 50, 10, 5, 15,
  0, 10, 50, 15, 25,
  0, 0, 0, 50, 50,
  10, 5, 15, 5, 65
), nrow = 5, byrow = TRUE)/100

rownames(dispersalMat) <- colnames(dispersalMat) <- c("A", "B", "C", "D", "E")

cor_env_repro_surv <- 0
cor_env_among_pops <- 1

brood_failures_pct <- c(A = 35, B = 35, C = 32, D = 40, E = 45)

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


brood_size_mean <- c(2.6, 2.8, 2.9, 2.5, 2.6)
  
brood_size_sd <- c(0.9, 1.3, 1.0, 1.1, 1.2)



age_firt_breed <- 3
age_last_breed <-	17
max_age <-	17
max_brood_p_year <-	1
max_prog_per_brood <- 5
sex_ratio <-	0.5
prop_fem_breed<-1
prop_mal_breed<-1


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

# Set row and column names
rownames(mortality_mean) <- rownames(mortality_sd) <- c("m1", "m2", "mad")
colnames(mortality_mean) <- colnames(mortality_sd) <- c("A", "B", "C", "D", "E")


# # Transpose the matrix to make sites rows
mort_df <- as.data.frame(t(mortality_mean))
mort_df$subpop <- rownames(mort_df)
str(mort_df)
# # Pivot longer and rename age classes
mort_long <- mort_df %>%
  pivot_longer(cols = c(m1, m2, mad), names_to = "age_class", values_to = "mortality") %>%
  mutate(age = case_when(
    age_class == "m1" ~ list(1),
    age_class == "m2" ~ list(2),
    age_class == "mad" ~ list(3:17)
  )) %>%
  unnest(age) %>%
  mutate(survival=1-mortality)%>%
  select(subpop, age, survival)

# # mort_long-
#   
#   
# 
# 
# 
