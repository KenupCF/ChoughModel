# Set prior distributions for model parameters using a template
model_pars$priors$Q_acc_period             <- qrunif_template
model_pars$priors$Q_surv_cor               <- qrunif_template
model_pars$priors$Q_nest_abandonment       <- qrunif_template
model_pars$priors$Q_habitat_effect_size    <- qrunif_template

model_pars$priors$Q_bl_surv                <- qrunif_template
model_pars$priors$Q_bl_brood               <- qrunif_template
model_pars$priors$Q_bl_nest                <- qrunif_template

model_pars$priors$Q_improved_foraging      <- qrunif_template
model_pars$priors$Q_improved_foraging_year <- qrunif_template

# Define management timing for release seasons
model_pars$mgmt$release_year_cont <- data.frame(
  release_time = c(NA, "Summer", "Winter"),
  yr_duration  = c(1, 1, 1 / 2)
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

# Define release strategies using expand.grid
rel_strat_01 <- expand.grid(
  release_size     = 0,
  age_release      = NA,
  origin           = NA_character_,
  habituation      = NA,
  release_time     = NA_character_,
  release_meth    = NA_character_,
  noEggsReleased   = 1:20,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_02 <- expand.grid(
  release_size     = 1:20,
  age_release      = 1,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Summer",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_03 <- expand.grid(
  release_size     = 1:20,
  age_release      = 1,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_04 <- expand.grid(
  release_size     = 1:20,
  age_release      = 1,
  origin           = "Wild",
  habituation      = 1,
  release_time     = "Summer",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_05 <- expand.grid(
  release_size     = 1:20,
  age_release      = 1,
  origin           = "Wild",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_06 <- expand.grid(
  release_size     = 1:20,
  age_release      = 2,
  origin           = "Captive",
  habituation      = 0,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_07 <- expand.grid(
  release_size     = 1:20,
  age_release      = 2,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
  )

rel_strat_08 <- expand.grid(
  release_size     = 1:20,
  age_release      = 2,
  origin           = "Wild",
  habituation      = 0,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_09 <- expand.grid(
  release_size     = 1:20,
  age_release      = 2,
  origin           = "Wild",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_10 <- expand.grid(
  release_size     = 1:20,
  age_release      = 3,
  origin           = "Captive",
  habituation      = 0,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_11 <- expand.grid(
  release_size     = 1:20,
  age_release      = 3,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strat_12 <- expand.grid(
  release_size     = 1:20,
  age_release      = 3,
  origin           = "Wild",
  habituation      = 0,
  release_time     = "Winter",
  release_meth    = "Immediate",
  noEggsReleased   = 0,
  release_years    = 5,
  subpop           = "A"
)

rel_strats<-plyr::rbind.fill(rel_strat_01,rel_strat_02,rel_strat_03,rel_strat_04,rel_strat_05,rel_strat_06,
                        rel_strat_07,rel_strat_08,rel_strat_09,rel_strat_10,rel_strat_11,rel_strat_12)%>%
  dplyr::mutate(r=1:n())

# Test strategy for sensitivity or debugging
rel_strat_test <- expand.grid(
  release_size     = 5,
  age_release      = 2,
  origin           = "Captive",
  habituation      = 1,
  release_time     = "Winter",
  release_meth    = "Staged",
  noEggsReleased   = 5,
  release_years    = 5,
  subpop           = "A"
)%>%
  dplyr::mutate(r=1:n())

model_pars$mgmt$release_schedule_master<-rel_strat_test

