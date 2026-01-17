library(targets)
library(tarchetypes)
library(here)

here::i_am("_targets.R")

tar_option_set(
  packages = c(
   "here",
   "tidyverse",
   "marquee",
   "patchwork",
   "readxl",
   "mgcv",
   "gamm4",
   "glmmTMB",
   "tinyplot",
   "cchunts",
   "hgEnergyGrowth",
   "metaDigitise",
   "hagenutils"
  )#,
  # controller = crew::crew_controller_local(workers = 5, seconds_idle = 60)
) 

tar_source(
  files = c("R/koster2020.R", "R/misc.R")
)

list(

# Lifecourse parameter grid -----------------------------------------------

  # tar_target(
  #   name = params1,
  #   command = list(
  #     group = 'avg',
  #     afb = 20,
  #     max_age = 80,
  #     menopause_age = c(40, 80),
  #     age_gap = c(5, 10),
  #     alpha_m = c(0.25, 0.5, 0.75),
  #     alpha_f = c(0.25, 0.5, 0.75),
  #     b1_m = seq(0.3, 0.6, 0.1),
  #     b1_f = seq(0.3, 0.6, 0.1),
  #     TEE_prop_m = seq(1, 2.4, 0.2),
  #     TEE_prop_f = seq(0.3, 1.8, 0.3)
  #   )
  # ),
  # tar_target(
  #   name = params1grid,
  #   command = expand.grid(params1) |> dplyr::filter(TEE_prop_m + TEE_prop_f >= 2.4)
  # ),
  # tar_target(
  #   name = params1out,
  #   command = pmap(params1grid, hg_lifecourse, .progress = F)
  # ),
  tar_target(
    name = params2,
    command = list(
      group = 'avg',
      afb = 20,
      max_age = 80,
      menopause_age = c(38, 80),
      age_gap = c(5, 10),
      alpha_m = c(0.25, 0.5, 0.75),
      alpha_f = c(0.25, 0.5, 0.75),
      b1_m = c(0.15, 0.25, 0.40),
      b1_f = c(0.15, 0.25, 0.40),
      age50_m = c(10, 15, 20),
      age50_f = c(10, 15, 20),
      TEE_prop_m = seq(1, 2.6, 0.4),
      TEE_prop_f = seq(0.4, 1.8, 0.2)
    )
  ),
  tar_target(
    name = params2grid,
    command = expand.grid(params2) |> 
      dplyr::filter(
        # Values from Kraft et al. 2020, accounting for some numeric double weirdness
        TEE_prop_m + TEE_prop_f >= 1.599 & TEE_prop_m + TEE_prop_f <= 3.401,
        
        # Fast, medium, and slow skill acquisition
        (age50_f == params2$age50_f[1] & b1_f == params2$b1_f[3]) | (age50_f == params2$age50_f[2] & b1_f == params2$b1_f[2]) | (age50_f == params2$age50_f[3] & b1_f == params2$b1_f[1]),
        (age50_m == params2$age50_m[1] & b1_m == params2$b1_m[3]) | (age50_m == params2$age50_m[2] & b1_m == params2$b1_m[2]) | (age50_m == params2$age50_m[3] & b1_m == params2$b1_m[1]),
      )
  ),
  tar_target(
    name = params2out,
    command = pmap(params2grid, hg_lifecourse, .progress = F)
  ),
  tar_target(
    name = params3,
    command = list_assign(params2, menopause_age = c(32, 38, 44, 80))
  ),
  tar_target(
    name = params3grid,
    command = expand.grid(params3) |> 
      dplyr::filter(
        # Values from Kraft et al. 2020, accounting for some numeric double weirdness
        TEE_prop_m + TEE_prop_f >= 1.599 & TEE_prop_m + TEE_prop_f <= 3.401,
      
        # Fast, medium, and slow skill acquisition
        (age50_f == params2$age50_f[1] & b1_f == params2$b1_f[3]) | (age50_f == params2$age50_f[2] & b1_f == params2$b1_f[2]) | (age50_f == params2$age50_f[3] & b1_f == params2$b1_f[1]),
        (age50_m == params2$age50_m[1] & b1_m == params2$b1_m[3]) | (age50_m == params2$age50_m[2] & b1_m == params2$b1_m[2]) | (age50_m == params2$age50_m[3] & b1_m == params2$b1_m[1]),
      )
  ),
  tar_target(
    name = params3out,
    command = pmap(params3grid, hg_lifecourse, .progress = F)
  ),
  tar_quarto(
    name = menopause_paper,
    path = "menopause_paper.qmd"
  )
)
