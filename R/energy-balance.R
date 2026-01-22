# Family energy balance ---------------------------------------------------

#+ include=F

AFB <- params2$afb
max_age <- params2$max_age
AOM <- min(params2$menopause_age)

out_mean <- function(d){
  d |> 
    summarise(
      menopause_age = mean(menopause_age),
      fertility = mean(fertility),
      # fertility2 = mean(fertility2),
      family_size = mean(family_size),
      mean_family_consumption = mean(family_consumption),
      mean_wife_consumption = mean(wifeTEE),
      mean_husband_consumption = mean(husbandTEE),
      mean_total_child_consumption = mean(total_child_consumption),
      mean_production = mean(family_production),
      mean_wife_production = mean(wife_production),
      mean_husband_production = mean(husband_production),
      mean_total_child_production = mean(total_child_production),
      mean_energybalance = mean(energy_balance),
      resident_children = mean(resident_children),
      wife_survival = mean(wife_survival),
      husband_survival = mean(husband_survival),
      .by = c(Menopause, age_gap, wife_age)
    )
}

params2grid$ParamSet <- 1:nrow(params2grid)
params2grid$ParamSet2 <- rep(1:(nrow(params2grid)/2), each = 2)

outdf <- 
  list_rbind(params2out, names_to = "ParamSet") |> 
  left_join(params2grid) |>
  mutate(
    Menopause = ifelse(menopause_age < max_age, str_glue("Menopause (age {menopause_age})"), "No Menopause"),
    Menopause = factor(Menopause, levels = sort(unique(Menopause), decreasing = T)),
    Parent_production = TEE_prop_f + TEE_prop_m
  ) |> 
  mutate(
    cumsumEB = cumsum(energy_balance),
    .by = ParamSet
  )

outdfsplit <- split(outdf, outdf$menopause_age)
outdfsplit_menopause <- outdfsplit[[1]]
outdfsplit_none <- outdfsplit[[2]]

EBthreshold <- 500
menopause_param_space <-
  outdfsplit_menopause |> 
  dplyr::filter(
    abs(mean(energy_balance)) < EBthreshold,
    .by = ParamSet
  )

nomenopause_match <- outdfsplit_none[outdfsplit_none$ParamSet2 %in% unique(menopause_param_space$ParamSet2),]

lifecourse <- bind_rows(menopause_param_space, nomenopause_match)
lifecourse_mean <- out_mean(lifecourse)

# Average over age_gap (since little difference)
lifecourse_mean2 <- lifecourse_mean |> summarise(across(everything(), mean), .by = c(Menopause, wife_age))

# Menopause condition only
lifecourse_mean3 <- lifecourse_mean2[lifecourse_mean2$Menopause == "Menopause (age 38)",]

# No menopause condition only
lifecourse_mean4 <- lifecourse_mean2[lifecourse_mean2$Menopause != "Menopause (age 38)",]

# Early age energy balance and transitions

earlyEB <-
  lifecourse |> 
  dplyr::filter(wife_age < AOM) |> 
  summarise(
    change = list(signchange(energy_balance, age = wife_age)),
    change = set_names(change, ParamSet[1]),
    .by = ParamSet
  ) |> 
  pull(change) |> 
  list_rbind(names_to = "ParamSet")

d20 <- 
  menopause_param_space |> 
  dplyr::filter(wife_age == 20) |> 
  mutate(across(age_gap:TEE_prop_f, as.ordered))
d20$group <- NULL

AOMi <- AOM - AFB + 1
eb_pos <- 
  menopause_param_space |> 
  dplyr::filter(b1_f == 0.25 & b1_m == 0.25) |>
  summarise(
    pre_pos = sum(energy_balance[1:AOMi][energy_balance[1:AOMi] > 0]),
    pre_n = sum(energy_balance[1:AOMi] > 0),
    # pre_neg = sum(energy_balance[1:AOM][energy_balance[1:AOM] < 0]),
    post_pos = sum(energy_balance[(AOMi+1):length(energy_balance)][energy_balance[(AOMi+1):length(energy_balance)] > 0]),
    post_n = sum(energy_balance[(AOMi+1):length(energy_balance)] > 0),
    # post_neg = sum(energy_balance[(AOM+1):length(energy_balance)][energy_balance[(AOM+1):length(energy_balance)] < 0]),
    early_prop = pre_pos/(pre_pos + post_pos),
    .by = ParamSet
  )
100*round(mean(eb_pos$early_prop, na.rm = T), 2)


# Multiple ages of menopause ----------------------------------------------

params3grid$ParamSet <- 1:nrow(params3grid)

outdf3 <- 
  list_rbind(params3out, names_to = "ParamSet") |> 
  left_join(params3grid) |>
  mutate(
    Menopause = str_glue("Menopause (age {menopause_age})"),
    Menopause = factor(Menopause, levels = sort(unique(Menopause), decreasing = F)),
    Parent_production = TEE_prop_f + TEE_prop_m
  ) |> 
  mutate(
    cumsumEB = cumsum(energy_balance),
    .by = ParamSet
  )

outdf3mean <- out_mean(outdf3)

# Total fertility for multiple ages of menopause
fertility_mean <- map_dbl(params3$menopause_age, \(ma) max(hg_lifecourse(menopause_age = ma)$fertility2)) |> round(digits = 1)