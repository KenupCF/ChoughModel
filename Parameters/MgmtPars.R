
model_pars$mgmt$supp_feed_opts<-c(
    # "No",
    "Current","Provisional")
# model_pars$mgmt$supp_feed_opts<-c("Current")


# Set prior distributions for model parameters using a template
model_pars$priors$Q_acc_period             <- qrunif_template
model_pars$priors$Q_surv_cor               <- qrunif_template
model_pars$priors$Q_nest_abandonment       <- qrunif_template
model_pars$priors$Q_habitat_effect_size    <- qrunif_template

model_pars$priors$Q_bl_surv                <- qrunif_template
model_pars$priors$Q_bl_brood               <- qrunif_template
model_pars$priors$Q_bl_surv_sd             <- qrunif_template
model_pars$priors$Q_bl_brood_sd            <- qrunif_template
model_pars$priors$Q_bl_nest                <- qrunif_template

model_pars$priors$Q_improved_foraging      <- qrunif_template
model_pars$priors$Q_improved_foraging_year <- qrunif_template

# Define management timing for release seasons
model_pars$mgmt$release_year_cont <- data.frame(
  release_time = c(NA, "Summer", "Winter"),
  yr_duration  = c(1, 1-(2/12), 1-(8/12))
)

# Define supplementary feeding strategies
model_pars$mgmt$supp_feeding_master <- list(
  "No" = data.frame(
    sf          = 0,
    subpop      = subpops,
    StopAfterIF = FALSE
  ),
  "Current" = data.frame(
    sf          = c(1, 1, 1, 0, 0),
    subpop      = subpops,
    StopAfterIF = FALSE
  ),
  "Provisional" = data.frame(
    sf          = c(1, 1, 1, 0, 0),
    subpop      = subpops,
    StopAfterIF = TRUE
  )
)


release_size_vec<-seq(from=4,to=24,by=2)
egg_swap_vec<-seq(from=5,to=25,by=5)

# Define release strategies using expand.grid

rel_strat_00 <- expand.grid(
  release_size     = 0,
  age_release      = NA,
  origin           = NA_character_,
  habituation      = NA,
  release_time     = NA_character_,
  release_meth     = NA_character_,
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 0,wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_01 <- expand.grid(
  release_size     = 0,
  age_release      = NA,
  origin           = NA_character_,
  habituation      = NA,
  release_time     = NA_character_,
  release_meth     = NA_character_,
  noEggsReleased   = egg_swap_vec,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)
rel_strat_01a <- expand.grid(
  release_size     = 0,
  age_release      = NA,
  origin           = NA_character_,
  habituation      = NA,
  release_time     = NA_character_,
  release_meth     = NA_character_,
  noEggsReleased   = egg_swap_vec,nest_aband_allowed=FALSE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)
rel_strat_02 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 1,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Summer",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_03 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 1,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_04 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 1,
  origin           = "Wild",
  habituation      = 1,
  release_time     = "Summer",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_05 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 1,
  origin           = "Wild",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_06 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 2,
  origin           = "Captive",
  habituation      = 0,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_07 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 2,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
  )

rel_strat_08 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 2,
  origin           = "Wild",
  habituation      = 0,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_09 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 2,
  origin           = "Wild",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_10 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 3,
  origin           = "Captive",
  habituation      = 0,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_11 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 3,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strat_12 <- expand.grid(
  release_size     = release_size_vec,
  age_release      = 3,
  origin           = "Wild",
  habituation      = 0,
  release_time     = "Winter",
  release_meth    = "Immediate",
  noEggsReleased   = 0,nest_aband_allowed=TRUE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)

rel_strats_test_size<-plyr::rbind.fill(rel_strat_01,rel_strat_01a,rel_strat_02,rel_strat_03,rel_strat_04,rel_strat_05,rel_strat_06,
                        rel_strat_07,rel_strat_08,rel_strat_09,rel_strat_10,rel_strat_11,rel_strat_12)%>%
  dplyr::mutate(r=1:n(),scot_heritage=0)

#### temporary to just get one of each release type
rel_strats_test_final<-rel_strats_test_size%>%
  plyr::rbind.fill(rel_strat_00)%>%
  dplyr::mutate(release_size=case_when(release_size>0~NA,
                                       TRUE~release_size),
                noEggsReleased=case_when(noEggsReleased>0~NA,
                                         TRUE~noEggsReleased))%>%
  dplyr::filter(!duplicated(data.frame(release_size,noEggsReleased,
                                       age_release,origin,habituation,release_time,release_meth,
                                       nest_aband_allowed,release_years,wait_for_habitat,subpop,backup_subpop)))%>%
  dplyr::mutate(wait_for_habitat=NULL)%>%
  dplyr::mutate(noReleases=(release_size+noEggsReleased)==0)%>%
  dplyr::mutate(noReleases=case_when(is.na(noReleases)~FALSE,TRUE~noReleases))%>%
  merge(data.frame(wait_for_habitat=c(F,T)))%>%
  dplyr::filter(!(noReleases& wait_for_habitat))%>%
  dplyr::mutate(r=1:n(),scot_heritage=0)
write.csv(rel_strats_test_final,"release_strategy_template.csv")

# Test strategy for sensitivity or debugging
rel_strat_test <- expand.grid(
  release_size     = 24,
  age_release      = 2,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 25,nest_aband_allowed=FALSE,
  release_years    = 5, wait_for_habitat=FALSE,
  subpop           = "A",backup_subpop="C"
)%>%
  dplyr::mutate(r=1:n(),scot_heritage=0)

rel_strat_no_release<-rel_strat_00%>%
  dplyr::mutate(r=1:n(),scot_heritage=0)


###  decide which release strategies to run 
# 
model_pars$mgmt$release_schedule_master<-rel_strats_test_final
# model_pars$mgmt$release_schedule_master<-rel_strat_test
# model_pars$mgmt$release_schedule_master<-rel_strat_no_release

