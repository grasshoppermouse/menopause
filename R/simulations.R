
# Functions ---------------------------------------------------------------

binary_colors <- viridisLite::magma(11)[c(4,8)]

param_constraints <- function(param_grid, params){
  param_grid |> 
    dplyr::filter(
      # Values from Kraft et al. 2020, accounting for some numeric double weirdness
      TEE_prop_m + TEE_prop_f >= 2 & TEE_prop_m + TEE_prop_f <= 3.401,
      
      # Fast, medium, and slow skill acquisition
      (age50_f == params$age50_f[1] & b1_f == params$b1_f[3]) | (age50_f == params$age50_f[2] & b1_f == params$b1_f[2]) | (age50_f == params$age50_f[3] & b1_f == params$b1_f[1]),
      (age50_m == params$age50_m[1] & b1_m == params$b1_m[3]) | (age50_m == params$age50_m[2] & b1_m == params$b1_m[2]) | (age50_m == params$age50_m[3] & b1_m == params$b1_m[1]),
    )
}

effectsizes <- function(d, outcome, param, max_age = 40){
  v <- as.character(sort(unique(d[[param]])))
  e <- tapply(d[[outcome]], d[[param]], mean) |> diff()
  tibble(
    param = rep(param, length(v)),
    param_values = v,
    param_index = paste0("v", 1:length(v)),
    effects = c(0, unname(e))
  )
}

# Main params -------------------------------------------------------------

demographic_params0 <- list(
  e0_f = 35,
  e0_m = 30,
  SRB = 1.05,
  group = 'avg',
  afb = 20,
  max_age = 80
)

demographic_params1 <- list(
  IBI = c(3, 4),
  alb = c(30, 38, 50, 60)
)

demographic_params <- c(demographic_params0, demographic_params1)

skill_params <- list(
  alpha_m = c(0.25, 0.5, 0.75),
  alpha_f = c(0.25, 0.5, 0.75),
  b1_m = c(0.15, 0.25, 0.40),
  b1_f = c(0.15, 0.25, 0.40),
  age50_m = c(10, 15, 20),
  age50_f = c(10, 15, 20)
)

production_params <- list(
  # These values derived from Kraft et al. 2020
  TEE_prop_m = seq(1, 2.2, 0.4),
  TEE_prop_f = seq(0.4, 1.6, 0.4)
)

params0 <- c(skill_params, demographic_params0, production_params)
params <- c(skill_params, demographic_params, production_params)

param_grid0 <- 
  expand.grid(params0) |> 
  param_constraints(params) |> 
  mutate(ParamSet = cur_group_rows())
  
param_grid <- 
  expand.grid(params) |> 
  param_constraints(params) |> 
  mutate(
    ParamSet = cur_group_rows(),
    Menopause = str_glue("ALB: {alb}")
  )

# After first running simulations, which take a few minutes,
# comment out these 4 lines for repeat runs, if desired
param_grid0$pop_sims <- pmap(param_grid0, hg_lifecourse2, .progress = F)
param_grid$family_sims <- pmap(param_grid, hg_lifecourse, .progress = F)
save(param_grid0, file = "param_grid0.rds")
save(param_grid, file = "param_grid.rds")

load("param_grid0.rds")
load("param_grid.rds")

fam_grid <-
  param_grid |> 
  unnest(family_sims) |> 
  mutate(
    TEE_prop = round(TEE_prop_f + TEE_prop_m, 1),
    child_prod_con_ratio = ifelse(total_child_consumption == 0, NA, total_child_production / total_child_consumption)
  )

child_grid <-
  fam_grid |> 
  summarise(
    girlTEE = mean(girlTEE),
    boyTEE = mean(boyTEE),
    child_TEE = mean(c(girlTEE, boyTEE)),
    girl_production = mean(girl_production),
    boy_production = mean(boy_production),
    child_production = mean(c(girl_production, boy_production)),
    girl_prodconratio = mean(girl_production/girlTEE),
    boy_prodconratio = mean(boy_production/boyTEE),
    child_prodconratio = mean((girl_production + boy_production) / (girlTEE + boyTEE)),
    .by = c(ParamSet, child_age)
  )


child_grid_long <-
  child_grid |>
  dplyr::select(ParamSet, child_age, girl_prodconratio, boy_prodconratio) |> 
  pivot_longer(c(girl_prodconratio, boy_prodconratio), names_to = "Sex", values_to = "Ratio") |> 
  mutate(
    Sex = ifelse(Sex == "girl_prodconratio", "Girl", "Boy")
  )


child_grid2 <-
  child_grid |> 
  summarise(
    across(-ParamSet, mean),
    .by = child_age
  )

child_grid_long2 <-
  child_grid2 |> 
  pivot_longer(
    c(girl_prodconratio, boy_prodconratio), 
    names_to = "Sex", 
    values_to = "Ratio", 
  ) |> 
  mutate(
    Sex = ifelse(Sex == "girl_prodconratio", "Girl", "Boy")
  )

LCmean <-
  fam_grid |> 
  dplyr::select(
    Menopause,
    # alb,
    IBI, 
    wife_age,
    parents,
    family_size,
    resident_children,
    energy_balance,
    cumulativeEB,
    fertility2,
    total_girl_consumption,
    total_boy_consumption,
    total_child_consumption,
    total_girl_production,
    total_boy_production,
    total_child_production,
    wifeTEE,
    husbandTEE,
    wife_production,
    husband_production
  ) |>
  summarise(
    across(everything(), list(mean = mean2, min = min, max = max)),
    .by = c(Menopause, IBI, wife_age)
  )

# Average over IBI
LCmean2 <-
  LCmean |> 
  summarise(
    across(everything(), mean2),
    .by = c(Menopause, wife_age)
  )

plot_family_size <- 
  ggplot(LCmean2, aes(wife_age, family_size_mean, colour = Menopause)) +
  geom_line(linewidth = 1.5) +
  # geom_text(data = margin_vals[margin_vals$IBI == 3,], aes(x = I(1), y = y, label = lab, hjust = 0)) +
  scale_color_viridis_d("", option = "A", end = 0.8, direction = -1) +
  ylim(0, NA) +
  labs(title = "Family model", x = "Wife age (years)", y = "Family size") +
  theme_minimal(15) +
  theme(legend.position = "top")
plot_family_size

margin_vals <-
  LCmean2 |> 
  summarise(
    y = cumulativeEB_mean[61],
    TF = round(max(fertility2_mean), 1),
    lab = str_glue("{Menopause}, TF: {TF}")[1],
    .by = c(Menopause)
  )

plot_EB_fam <- 
  ggplot(LCmean2, aes(wife_age, cumulativeEB_mean, colour = Menopause)) +
  geom_line(linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_text(data = margin_vals, aes(x = I(1), y = y, label = lab, hjust = 0)) +
  scale_color_viridis_d(option = "A", end = 0.8, direction = -1) +
  labs(x = "Wife age (years)", y = "Cumulative energy balance\n(kcals/family/day)") +
  theme_minimal(15) +
  coord_cartesian(xlim = c(20, 80), clip = "off") +
  theme(legend.position = "none", plot.margin = margin(0.5, 4.5, 0.5, 1, "cm"))
plot_EB_fam

plot_family_model <- plot_family_size / plot_EB_fam + plot_layout(axes = "collect")
plot_family_model

# Average over wife age
LCmean3 <-
  LCmean |> 
  summarise(
    maxkids = max(resident_children_max),
    agemaxkidsindex = which.max(resident_children_max),
    agemaxkids = wife_age[agemaxkidsindex],
    maxfamilysize = max(family_size_max),
    total_fertility = max(fertility2_max),
    zerokidsindex = signchange0(resident_children_mean > 0),
    zerokidsage = wife_age[zerokidsindex],
    posEBindex = signchange0(energy_balance_mean[wife_age > 25] > 0)[1], # omit early transient EB > 0 for IBI = 4
    posEBage = wife_age[wife_age > 25][posEBindex],
    across(-c(wife_age, maxkids:posEBage, matches("min$|max$")), mean2),
    .by = c(Menopause, IBI)
  )

tbl_stats <-
  LCmean3 |> 
  dplyr::select(
    IBI,
    ALB = Menopause,
    `Mean dependents` = resident_children_mean,
    `Maximum dependents` = maxkids,
    `Mean family size` = family_size_mean,
    `Maximum family size` = maxfamilysize,
    `Total fertility` = total_fertility,
    `Mean dependent consumption (kcals/day)` = total_child_consumption_mean,
    `Mean dependent production (kcals/day)` = total_child_production_mean,
    `Mean family energy balance (kcals/day)` = energy_balance_mean,
    `Age maximum dependents` = agemaxkids,
    `Age positive energy balance` = posEBage,
    `Age zero dependents` = zerokidsage
  ) |>
  mutate(
    ALB = str_remove(ALB, "ALB: "),
    across(-ALB,\(v) round(v, 1)),
    across(matches("kcals"), \(v) round(v))
  ) |>
  arrange(IBI, ALB) |> 
  rename(
    `Age of last birth (ALB)` = ALB,
    `Interbirth interval (IBI)` = IBI
  ) |> 
  t() |> 
  as_tibble(rownames = "Variable", .name_repair = "unique") |> 
  tt(colnames = F) |> 
  style_tt(i = 1:2, bold = T) |> 
  style_tt(i = 1:11, j = 6, line = 'l', line_color = 'lightgrey') |> 
  style_tt(i = 2, j = 1:9, line = 'b') |> 
  style_tt(j = 1, align = 'l')|> 
  style_tt(fontsize = 0.7)

tbl_stats

LCmean3b <-
  LCmean3 |> 
  mutate(
    across(-c(Menopause, IBI), \(v) round(v, 1)),
    across(-c(Menopause, IBI), as.list),
    across(-c(Menopause, IBI), \(v) set_names(v, nm = str_glue("ALB{str_remove(Menopause, 'ALB: ')}IBI{IBI}")))
  )
  
# lc_grid <-
#   expand_grid(ALB = params$alb, IBI = params$IBI) |> 
#   mutate(
#     lifecourse = map2(ALB, IBI, \(alb, ibi) hg_lifecourse(alb = alb, IBI = ibi, afb = 20)),
#     zerokidsindex = map_dbl(lifecourse, \(lc) signchange0(lc$resident_children > 0)),
#     zerokidsage = map2(lifecourse, zerokidsindex, \(lc, zerokidsindex) lc$wife_age[zerokidsindex]),
#     mean_kids = map(lifecourse, \(lc) mean(lc$resident_children)),
#     max_kids = map(lifecourse, \(lc) max(lc$resident_children)),
#     mean_consumption = map(lifecourse, \(lc) mean(lc$total_child_consumption)),
#     max_consumption = map(lifecourse, \(lc) max(lc$total_child_consumption)),
#     across(lifecourse:max_consumption, \(v) set_names(v, nm = str_glue("IBI{IBI}ALB{ALB}"))),
#     IBI = paste("IBI:", IBI),
#     ALB = paste("ALB:", ALB),
#   )

fam_grid2 <-
  fam_grid |> 
  dplyr::filter(
    wife_age %in% seq(20, 80, 5),
    TEE_prop %in% c(2.2, 2.6, 3),
    alb %in% params$alb[2:4]
  ) |> 
  mutate(
    wife_age = factor(wife_age),
    wife_age = fct_rev(wife_age)
  )

fam_grid_sum <-
  fam_grid2 |> 
  summarise(
    meanEB = mean(energy_balance),
    .by = c(Menopause, TEE_prop, wife_age)
  )

plot_ridgeline <- 
  ggplot(fam_grid2, aes(energy_balance, wife_age)) + 
  geom_density_ridges() + 
  geom_point(data = fam_grid_sum, aes(meanEB, wife_age), colour = "blue") +
  geom_vline(xintercept = 0) + 
  labs(x = "Family energy balance (kcals/day)", y = "Wife age (years)") +
  facet_grid(Menopause~TEE_prop) +
  theme_minimal(15) +
  theme(strip.text.y = element_text(angle = 0))
plot_ridgeline

# Average lifecourse ------------------------------------------------------

member_dict <- c(
  total_girl_ = "Girls",
  total_boy_ = "Boys",
  wife = "Wife",
  wife_ = "Wife",
  husband = "Husband",
  husband_ = "Husband"
)

type_dict <- c(
  consumption_mean = "Consumption",
  production_mean = "Production",
  TEE_mean = "Consumption"
)

LCprod_con <-
  LCmean2 |>
  pivot_longer(
    c(
      total_girl_consumption_mean, 
      total_boy_consumption_mean,
      total_girl_production_mean,
      total_boy_production_mean,
      wifeTEE_mean,
      husbandTEE_mean,
      wife_production_mean,
      husband_production_mean
    ), 
    names_to = 'Member', 
    values_to = 'kcals'
  ) |>
  separate_wider_regex(Member, c(Member = "husband_|wife_|total_girl_|total_boy_|wife|husband", Type = ".*")) |> 
  mutate(
    Member = member_dict[Member],
    Type = type_dict[Type]
  )

plot_energyProdCon <-
  ggplot(LCprod_con, aes(wife_age, kcals, fill = Member)) + 
  geom_col() +
  scale_fill_viridis_d(option = "A", begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "Wife age (years)", y = "kcals/day") +
  # ylim(0, 10500) +
  facet_grid(Type~Menopause) +
  theme_minimal(15) +
  theme(strip.text.y = element_text(angle = 0), legend.position = "top", legend.title = element_blank())
plot_energyProdCon

plot_EBbalance <-
  ggplot(LCmean, aes(wife_age, energy_balance_mean)) +
  geom_smooth(span = 0.5, se=F) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "Wife age (years)", y = "kcals/day") +
  facet_grid("Balance"~Menopause) +
  theme_minimal(15) +
  theme(strip.text.x = element_blank(), strip.text.y = element_text(angle = 0))
plot_EBbalance

plot_ProdConBal <- plot_energyProdCon / plot_EBbalance + plot_layout(axes = "collect", heights = c(2,1))

# Population stats --------------------------------------------------------

pop_stats <-
  param_grid0 |> 
  mutate(
    TEE_prop = round(TEE_prop_f + TEE_prop_m, 1),
    b1 = round(b1_f + b1_m, 2),
    totalEB = map_dbl(pop_sims, \(d) mean(d$energy_balance)),
    youngEB = map_dbl(pop_sims, \(d) mean(d$energy_balance[d$age <= 40])),
    totalEB2 = map_dbl(pop_sims, \(d) mean(d$energy_balance_adult_prod)),
    ratio = map_dbl(pop_sims, \(d) mean((d$production_f[d$age < 20] + d$production_m[d$age < 20]) / (d$TEE_f[d$age < 20] + d$TEE_m[d$age < 20]), na.rm = T))
  )

param_effects <-
  map(names(skill_params), \(p) effectsizes(pop_stats, "youngEB", p)) |> 
  list_rbind() |>
  dplyr::filter(!str_detect(param, "age"))

plot_effectsizes <-
  ggplot(param_effects[param_effects$effects != 0,], aes(param, effects, fill = fct_rev(param_index), label = param_values)) + 
  geom_col() +
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white") +
  geom_text(data = param_effects[param_effects$effects == 0,], aes(param, y = -5, label = param_values), size = 4) +
  scale_fill_grey() + 
  # ylim(-300, 700) +
  labs(x = "\nParameter", y = "Change in energy balance (kcals)") +
  theme_minimal(15) +
  theme(legend.position = 'none')
plot_effectsizes

plot_EB_pop <- 
  ggplot(pop_stats, aes(TEE_prop, totalEB)) + 
  geom_boxplot(aes(group = TEE_prop), colour = binary_colors[1]) + 
  geom_boxplot(aes(TEE_prop, totalEB2, group = TEE_prop), colour = binary_colors[2]) +
  geom_point(data = HG_means2, aes(y = -450), colour = binary_colors[2])  +
  geom_text(data = HG_means2, aes(label = Population, y  = -400), size = 4) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  annotate("text", label = "With juvenile\nproduction", x = 3.75, y = 150, hjust = 0, colour = binary_colors[1]) +
  annotate("text", label = "Without juvenile\nproduction", x = 3.75, y = -150, hjust = 0, colour = binary_colors[2]) +
  scale_x_continuous(breaks = seq(1.8, 3.4, 0.4)) +
  scale_y_continuous(breaks = seq(-500, 500, 100)) +
  labs(title = "Population model", x = "Daily adult production as proportion of adult TEE", y = "Mean energy balance\n(kcals/person/day)") +
  coord_cartesian(xlim = c(1.6, 3.6), clip = "off") +
  theme_minimal(13) +
  theme(plot.margin = margin(0.5, 5, 0.5, 1, "cm"))
# plot_EB_pop

plot_EB_pop_fam <- plot_EB_pop / plot_EB_fam
# ggsave("Figures/plot_EB_pop_fam.xsvg") # Many manual edits. Don't run!


# ALB x IBI stats ---------------------------------------------------------------

lc_grid <-
  expand_grid(ALB = params$alb, IBI = params$IBI) |> 
  mutate(
    lifecourse = map2(ALB, IBI, \(alb, ibi) hg_lifecourse(alb = alb, IBI = ibi, afb = 20)),
    lifecourse = map(lifecourse, \(lc) {lc$parents <- lc$wife_survival + lc$husband_survival; lc}),
    zerokidsindex = map_dbl(lifecourse, \(lc) signchange0(lc$resident_children > 0)),
    zerokidsage = map2(lifecourse, zerokidsindex, \(lc, zerokidsindex) lc$wife_age[zerokidsindex]),
    mean_kids = map(lifecourse, \(lc) mean(lc$resident_children)),
    max_kids = map(lifecourse, \(lc) max(lc$resident_children)),
    mean_consumption = map(lifecourse, \(lc) mean(lc$total_child_consumption)),
    max_consumption = map(lifecourse, \(lc) max(lc$total_child_consumption)),
    across(lifecourse:max_consumption, \(v) set_names(v, nm = str_glue("IBI{IBI}ALB{ALB}"))),
    IBI = paste("IBI:", IBI),
    ALB = paste("ALB:", ALB),
  )

plot_alb_stats <- 
  ggplot(lc_grid, aes(unlist(mean_kids), unlist(mean_consumption), color = factor(ALB))) + 
    geom_point(size = 4) + 
    geom_text(aes(label = ALB), position = position_nudge(x = -0.3)) +
    scale_color_viridis_d(option = "C", end = 0.8, guide = guide_none()) +
    xlim(0, NA) +
    ylim(0, NA) +
    labs(x = "Average number of dependents across the adult lifecourse", y = "Average daily energy consumption\nof all dependents (kcals)") +
    facet_wrap(~IBI) +
   theme_bw(15)
plot_alb_stats

# Mortality ---------------------------------------------------------------

parent_loss <- function(e0_f, e0_m, IBI, afb, alb, age_gap){
  d <- hg_lifecourse(afb = afb, e0_f = e0_f, e0_m = e0_m, alb = alb, IBI = IBI)
  husband_ages_index <- (afb + age_gap + 1):(81 + age_gap)
  tibble(
    wife_age = d$wife_age,
    wife_survival = d$wife_survival,
    husband_survival = lifetable("avg", sex = 1, e0 = e0_m, SRB = 1.05)$lx[husband_ages_index] / d$wife_num[1],
    parents = wife_survival + husband_survival,
    spouse_ratio = husband_survival / wife_survival,
    dependents = d$resident_children,
    dependent_ratio = dependents / parents,
    # mean_dependent_age = map_dbl(d$wife_age, \(age) hgEnergyGrowth:::sum_resident_children(age, 20, d$births, d$child_age)) / dependents / 2
  )
}

mortality_grid <-
  expand.grid(demographic_params[c("e0_f", "e0_m", "IBI", "afb", "alb")]) |> 
  mutate(age_gap = 0)
mortality_grid$out <- pmap(mortality_grid, parent_loss, .progress = T)

mortality_grid2 <-
  mortality_grid |> 
  unnest(out) |> 
  mutate(
    IBI = paste("IBI:", IBI),
    ALB = paste("ALB:", alb)
  )

plot_dependent_ratio <-
  ggplot(mortality_grid2, aes(wife_age, dependent_ratio, colour = ALB, group = ALB)) + 
  geom_line(linewidth = 1.5) + 
  scale_colour_viridis_d(option = "A", begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "Wife age (years)", y = "Dependents / Parent ratio") +
  facet_wrap(~IBI) +
  theme_minimal(15)

mortality_grid3 <-
  expand_grid(e0_f = seq(25, 45, 5), e0_m = seq(25, 45, 5), IBI = c(3, 4), afb = 20, alb = c(38, 60), age_gap = c(0, 2, 5, 10)) |> 
  dplyr::filter(e0_f > e0_m, abs(e0_f - e0_m) <= 10)
mortality_grid3$parent_loss <- pmap(mortality_grid3, parent_loss, .progress = T)
mortality_grid3$ParamSet <- 1:nrow(mortality_grid3)

parent_loss2 <- function(d){
  one_parent = signchange(d$parents <= 1, age = d$wife_age)
  tibble(
    oneparentage = one_parent$age[1],
    oneparentindex = one_parent$index[1],
    
    # Interpolate before and after the sign change
    wife_surv = 100*round((d$wife_survival[oneparentindex - 1] + d$wife_survival[oneparentindex]) / 2, 2),
    husband_surv = 100*round((d$husband_survival[oneparentindex - 1] + d$husband_survival[oneparentindex]) / 2, 2),
    resid_kid_surv = round((d$dependents[oneparentindex - 1] + d$dependents[oneparentindex]) / 2, 2)
  )
}

mortality_grid3b <-
  mortality_grid3 |>
  mutate(
    alb0 = alb,
    IBI0 = IBI,
    alb = paste(alb, "years"),
    IBI = paste("IBI: ", IBI),
    Gap = paste("Gap:", age_gap, "years"),
    Gap = fct_reorder(Gap, age_gap),
    oneparent = map(parent_loss, parent_loss2)
  ) |> 
  unnest(oneparent) |> 
  mutate(
    wife_surv2 = paste0(wife_surv, "%"),
    husband_surv2 = paste0(husband_surv, "%"),
    across(oneparentage:resid_kid_surv, as.list),
    across(oneparentage:resid_kid_surv, \(v) set_names(v, nm = str_glue("IBI{IBI0}ALB{alb0}Gap{age_gap}")))
  )

plot_wife_survival <-
  ggplot(mortality_grid3b, aes(e0_f, e0_m, fill = unlist(wife_surv))) + 
  geom_tile() + 
  geom_text(aes(label = wife_surv2), color = "white") + 
  scale_fill_viridis_c(name = "Wife survival", option = "A", end = 0.9, label = scales::percent_format(scale = 1)) +
  facet_wrap(~Gap) +
  theme_bw(15)

mortality_grid3c <- dplyr::filter(mortality_grid3b, e0_f == 35, e0_m == 30)

plot_oneparent <-
  ggplot(mortality_grid3c, aes(unlist(oneparentage), unlist(resid_kid_surv), colour = alb)) + 
  geom_point() + 
  geom_text(aes(label = wife_surv2), position = position_nudge(x = -0.5), show.legend = F) +
  xlim(48, 54) +
  ylim(0, 3.5) +
  scale_color_binary() +
  guides(colour = guide_legend(title = "ALB", reverse = T)) +
  labs(x = "Age of wife when one parent remains alive", y = "Dependents") +
  facet_grid(Gap~IBI) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
# plot_oneparent

# Brideservice ------------------------------------------------------------

brideservice <-
  fam_grid |>
  dplyr::filter(wife_age >=20, wife_age <= 25) |>
  mutate(
    age = ordered(husband_age),
    Male = husband_production - husbandTEE,
    Female = wife_production - wifeTEE,
  ) |> 
  dplyr::select(age, Female, Male) |> 
  pivot_longer(-age, names_to = "Sex", values_to = "Balance")

brideservice_mean <-
  brideservice |> 
  summarise(
    mean_balance = mean(Balance),
    .by = c(Sex, age)
  )

plot_brideservice <-
  ggplot(brideservice, aes(Balance, age)) +
  geom_density_ridges() +
  geom_point(data = brideservice_mean, aes(mean_balance, age)) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1000, colour = "red") +
  labs(y = "Young adult age (years)", x = "Production - consumption (kcals/day)") +
  facet_wrap(~Sex) +
  theme_bw(15)
plot_brideservice


# Self-sufficient ---------------------------------------------------------

production_params2 <- list(
  TEE_prop_m = seq(1.8, 2.2, 0.1),
  TEE_prop_f = seq(1, 1.6, 0.1)
)

params_ss <- c(skill_params, demographic_params, production_params2)
param_grid_ss <- expand.grid(params_ss) |> 
  param_constraints(params) |> 
  mutate(ParamSet = cur_group_rows())

# Code takes a minute or two to run
# After running, comment out these two lines for repeat runs
param_grid_ss$family_sims <- pmap(param_grid_ss, hg_lifecourse)
save(param_grid_ss, file = "param_grid_ss.rds")

load("param_grid_ss.rds")

fam_grid_ss <-
  param_grid_ss |> 
  unnest(family_sims) |> 
  mutate(
    TEE_prop = round(TEE_prop_f + TEE_prop_m, 1)
  )

fam_grid_ss_sum <-
  fam_grid_ss |>
  mutate(
    IBI = factor(IBI)
  ) |> 
  summarise(
    pos = all(energy_balance > 0),
    meanEB = mean(energy_balance),
    .by = c(IBI, TEE_prop, ParamSet)
  ) |> 
  summarise(
    pos_prop = sum(pos) / n(),
    meanmeanEB = mean(meanEB),
    .by = c(IBI, TEE_prop)
  )

plot_selfsufficient <-
  ggplot(fam_grid_ss_sum, aes(TEE_prop, pos_prop, colour = IBI)) + 
  geom_line(linewidth = 1.5) +
  geom_point(data = HG_means2, aes(TEE_prop, y = -0.05, colour = NULL), show.legend = F) +
  geom_text(data = HG_means2, aes(label = Population, y  = -0.1, colour = NULL), size = 4, show.legend = F) +
  annotate("segment", x = 1.5, xend = 3.0, y = 0, yend = 0, colour = "grey", linewidth = 1.5) +
  scale_color_binary() + 
  # scale_color_viridis_d(option = "A", end = 0.9) +
  guides(colour = guide_legend(reverse = T)) + 
  ylim(-0.1, 1) +
  labs(x = "Joint productivity (TEE_prop)", y = "Self-sufficient proportion of parameter space") +
  theme_minimal(15)
# plot_selfsufficient
