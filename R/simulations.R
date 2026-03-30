library(tidyverse)
library(patchwork)
library(ggridges)
library(hgEnergyGrowth)
library(hagenutils)

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

demographic_params <- list(
  e0_f = 35,
  e0_m = 30,
  SRB = 1.05,
  group = 'avg',
  afb = 20,
  IBI = c(3, 4),
  max_age = 80,
  menopause_age = seq(30, 60, 10)
)

skill_params <- list(
  alpha_m = c(0.25, 0.5, 0.75),
  alpha_f = c(0.25, 0.5, 0.75),
  b1_m = c(0.15, 0.25, 0.40),
  b1_f = c(0.15, 0.25, 0.40),
  age50_m = c(10, 15, 20),
  age50_f = c(10, 15, 20),
  
  # These values derived from Kraft et al. 2020
  TEE_prop_m = seq(1, 2.2, 0.4),
  TEE_prop_f = seq(0.4, 1.6, 0.4)
)

params <- c(skill_params, demographic_params)

param_grid <- 
  expand.grid(params) |> 
  param_constraints(params) |> 
  mutate(
    ParamSet = cur_group_rows(),
    Menopause = str_glue("ALB: {menopause_age}")
  )

# param_grid$family_sims <- pmap(param_grid, hg_lifecourse, .progress = T)
# param_grid$pop_sims <- pmap(param_grid, hg_lifecourse2, .progress = T)
# save(param_grid, file = "param_grid.rds")

load("param_grid.rds")

param_grid2 <-
  param_grid |> 
  mutate(
    family_sims = map(
      family_sims, 
      \(d){
        d |> 
          mutate(
            energy_balance2 = wife_production + husband_production - family_consumption,
            cumulativeEB = cumsum(energy_balance)
          )
      }
    )
  )

fam_grid <-
  param_grid2 |> 
  unnest(family_sims) |> 
  mutate(
    TEE_prop = round(TEE_prop_f + TEE_prop_m, 1)
  )

EB_ALB <-
  fam_grid |> 
  # dplyr::filter(IBI == 4) |> 
  summarise(
    meanEB = mean(energy_balance),
    mean_cumulative = mean(cumulativeEB),
    TF = mean(fertility2),
    mean_family_size = mean(family_size),
    .by = c(Menopause, wife_age)
  )

plot_family_size <- 
  ggplot(EB_ALB, aes(wife_age, mean_family_size, colour = Menopause)) +
  geom_line(linewidth = 1.5) +
  # geom_text(data = margin_vals[margin_vals$IBI == 3,], aes(x = I(1), y = y, label = lab, hjust = 0)) +
  scale_color_viridis_d("", option = "A", end = 0.8, direction = -1) +
  ylim(0, NA) +
  labs(title = "Family model", x = "Wife age (years)", y = "Family size") +
  theme_minimal(15) +
  theme(legend.position = "top")

margin_vals <-
  EB_ALB |> 
  summarise(
    y = mean_cumulative[61],
    TF = round(max(TF), 1),
    lab = str_glue("{Menopause}, TF: {TF}")[1],
    .by = c(Menopause)
  )

plot_EB_fam <- 
  ggplot(EB_ALB, aes(wife_age, mean_cumulative, colour = Menopause)) +
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

fam_grid2 <-
  fam_grid |> 
  dplyr::filter(
    wife_age %in% seq(20, 80, 5),
    TEE_prop %in% c(2.2, 2.6, 3),
    menopause_age %in% c(40,50,60)
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

# Example lifecourse ------------------------------------------------------

nm_dict <- c(
  meanenergy_balance = "Energy balance",
  meanwife = "Wife", 
  meanhusband = "Husband",
  meangirl = "Girls",
  meanboy = "Boys",
  meantotal_child_production = "All children",
  meanwife = "Wife", 
  meanhusband = "Husband", 
  meangirl = "Girls",
  meanboy = "Boys",
  meantotal_child_consumption = "All children"
)

LCmean <-
  param_grid2 |> 
  # dplyr::filter(IBI == 3) |> 
  unnest(family_sims) |> 
  mutate(
    TEE_prop = round(TEE_prop_f + TEE_prop_m, 1)
  ) |> 
  summarise(
    meanenergy_balance = mean(energy_balance),
    
    meangirl_consumption = mean(total_girl_consumption),
    meanboy_consumption = mean(total_boy_consumption),
    meangirl_production = mean(total_girl_production),
    meanboy_production = mean(total_boy_production),
    
    meanwife_consumption = mean(wifeTEE),
    meanhusband_consumption = mean(husbandTEE),
    meanwife_production = mean(wife_production),
    meanhusband_production = mean(husband_production),

    .by = c(Menopause, wife_age)
  )  

LCprod_con <-
  LCmean |>
  pivot_longer(
    c(meangirl_consumption:meanhusband_production), 
    names_to = 'Member', 
    values_to = 'Value'
  ) |>
  separate(Member, into = c("Member", "Type"), sep = "_") |> 
  mutate(
    Member = nm_dict[Member],
    Type = str_to_title(Type)
  )

plot_energyProdCon <-
  ggplot(LCprod_con, aes(wife_age, Value, fill = Member)) + 
  geom_col() +
  scale_fill_viridis_d(option = "A", begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "Wife age (years)", y = "kcals/day") +
  # ylim(0, 10500) +
  facet_grid(Type~Menopause) +
  theme_minimal(15) +
  theme(strip.text.y = element_text(angle = 0), legend.position = "top", legend.title = element_blank())
plot_energyProdCon

plot_EBbalance <-
  ggplot(LCmean, aes(wife_age, meanenergy_balance)) +
  geom_smooth(span = 0.5, se=F) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "Wife age (years)", y = "kcals/day") +
  facet_grid("Balance"~Menopause) +
  theme_minimal(15) +
  theme(strip.text.x = element_blank(), strip.text.y = element_text(angle = 0))

plot_ProdConBal <- plot_energyProdCon / plot_EBbalance + plot_layout(axes = "collect", heights = c(2,1)) 

# Population stats --------------------------------------------------------

pop_stats <-
  param_grid |> 
  mutate(
    TEE_prop = round(TEE_prop_f + TEE_prop_m, 1),
    b1 = round(b1_f + b1_m, 2),
    totalEB = map_dbl(pop_sims, \(d) mean(d$energy_balance) / 2),
    youngEB = map_dbl(pop_sims, \(d) mean(d$energy_balance[d$age <= 40])),
    totalEB2 = map_dbl(pop_sims, \(d) mean(d$energy_balance_adult_prod) / 2)
  )

param_effects <-
  map(names(skill_params), \(p) effectsizes(pop_stats, "youngEB", p)) |> 
  list_rbind() |>
  # mutate(
  #   param2 = str_replace(param, "_prop_f", "<sub>prop,f</sub>"),
  #   param2 = str_replace(param2, "_prop_m", "~prop,m~"),
  #   param2 = str_replace(param2, "_f", "~f~"),
  #   param2 = str_replace(param2, "_m", "~m~"),
  # ) |> 
  dplyr::filter(!str_detect(param, "age"))

plot_effectsizes <-
  ggplot(param_effects[param_effects$effects != 0,], aes(param, effects, fill = fct_rev(param_index), label = param_values)) + 
  geom_col() +
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white") +
  geom_text(data = param_effects[param_effects$effects == 0,], aes(param, y = -20, label = param_values), size = 4) +
  scale_fill_grey() + 
  # ylim(-300, 700) +
  labs(x = "\nParameter", y = "Change in energy balance (kcals)") +
  theme_minimal(15) +
  theme(legend.position = 'none')
plot_effectsizes

HG_means2 <- 
  HG_means |> 
  dplyr::select(-Tf_mean) |>  
  pivot_wider(names_from = "Sex", values_from = "Ea_mean") |> 
  mutate(
    Population = str_to_title(Population),
    Total = Male + Female, 
    TEE_prop = Total / 2300
  ) |> 
  arrange(Total)

plot_EB_pop <- 
  ggplot(pop_stats, aes(TEE_prop, totalEB)) + 
  geom_boxplot(aes(group = TEE_prop), colour = binary_colors[1]) + 
  geom_boxplot(aes(TEE_prop, totalEB2, group = TEE_prop), colour = binary_colors[2]) +
  geom_point(data = HG_means2, aes(y = -280), colour = binary_colors[2])  +
  geom_text(data = HG_means2, aes(label = Population, y  = -250), size = 4) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  annotate("text", label = "With juvenile\nproduction", x = 3.75, y = 50, hjust = 0, colour = binary_colors[1]) +
  annotate("text", label = "Without juvenile\nproduction", x = 3.75, y = -250, hjust = 0, colour = binary_colors[2]) +
  scale_x_continuous(breaks = seq(1.8, 3.4, 0.4)) +
  scale_y_continuous(breaks = seq(-250, 250, 50)) +
  labs(title = "Population model", x = "Daily adult production as proportion of adult TEE", y = "Mean energy balance\n(kcals/person/day)") +
  coord_cartesian(xlim = c(1.6, 3.6), clip = "off") +
  theme_minimal(13) +
  theme(plot.margin = margin(0.5, 5, 0.5, 1, "cm"))
plot_EB_pop

plot_EB_pop_fam <- plot_EB_pop / plot_EB_fam
# ggsave("Figures/plot_EB_pop_fam.xsvg") # Many manual edits. Don't run!


# ALB x IBI stats ---------------------------------------------------------------

alb_grid <-
  expand_grid(ALB = params$menopause_age, IBI = params$IBI) |> 
  mutate(
    lifecourse = map2(ALB, IBI, \(alb, ibi) hg_lifecourse(menopause_age = alb, IBI = ibi, afb = 20)),
    mean_dependents = map_dbl(lifecourse, \(lc) mean((lc$resident_girls + lc$resident_boys))),
    mean_kcals = map_dbl(lifecourse, \(lc) mean((lc$total_girl_consumption + lc$total_boy_consumption))),
    IBI = paste("IBI:", IBI),
    ALB = paste("ALB:", ALB)
  )

plot_mean_dependents <-
  ggplot(alb_grid, aes(ALB, mean_dependents, colour = IBI)) +
  geom_line(linewidth = 1.5) +
  scale_color_binary() +
  ylim(0, NA) +
  labs(x = "Age of Last Birth (years)", y = "Average number of dependents") +
  theme_bw(15)

plot_mean_energy <-
  ggplot(alb_grid, aes(ALB, mean_kcals, colour = IBI)) +
  geom_line(linewidth = 1.5) +
  scale_color_binary() +
  ylim(0, NA) +
  labs(x = "Age of Last Birth (years)", y = "Average daily energy consumption\nof all dependents (kcals)") +
  theme_bw(15)

# plot_alb_stats <- plot_mean_dependents / plot_mean_energy + plot_layout(axes = "collect", guides = "collect")

plot_alb_stats <- 
  ggplot(alb_grid, aes(mean_dependents, mean_kcals, color = factor(ALB))) + 
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

# wives0 <- lifetable("avg", 0, e0 = 35)$lx[21:81]
# wives <- wives0 / wives0[1]
# husbands <- lifetable("avg", 1, e0 = 30)$lx[21:81] / wives0[1]
# parents <- wives + husbands

parent_loss <- function(e0_f, e0_m, IBI, afb, alb, age_gap){
  d <- hg_lifecourse(afb = afb, e0_f = e0_f, e0_m = e0_m, menopause_age = alb, IBI = IBI)
  husband_ages <- (afb + age_gap + 1):(81 + age_gap)
  tibble(
    wife_age = d$wife_age,
    wife_survival = d$wife_survival,
    husband_survival = 1.05 * lifetable("avg", sex = 1, e0 = e0_m)$lx[husband_ages] / d$wife_num[1],
    parents = wife_survival + husband_survival,
    spouse_ratio = husband_survival / wife_survival,
    dependents = d$resident_children,
    dependent_ratio = dependents / parents,
    # mean_dependent_age = map_dbl(d$wife_age, \(age) hgEnergyGrowth:::sum_resident_children(age, 20, d$births, d$child_age)) / dependents / 2
  )
}

mortality_grid <-
  expand_grid(e0_f = 35, e0_m = 30, IBI = c(3, 4), afb = 20, alb = seq(30, 60, 10), age_gap = 0)
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
  labs(x = "Wife age (years)", y = "Dependent / Parent ratio") +
  facet_wrap(~IBI) +
  theme_minimal(15)

mortality_grid3 <-
  expand_grid(e0_f = seq(25, 45, 5), e0_m = seq(25, 45, 5), IBI = c(3, 4), afb = 20, alb = c(40, 60), age_gap = c(0, 2, 5, 10)) |> 
  dplyr::filter(e0_f > e0_m, abs(e0_f - e0_m) <= 10)
mortality_grid3$out <- pmap(mortality_grid3, parent_loss, .progress = T)
mortality_grid3$ParamSet <- 1:nrow(mortality_grid3)

# mortality_grid4 <-
#   mortality_grid3 |> 
#   unnest(out) |> 
#   mutate(age_gap = factor(age_gap))
# 
# plot_spouse_ratio <-
#   ggplot(mortality_grid4, aes(wife_age, spouse_ratio, colour = age_gap, group = ParamSet)) + 
#   geom_line(linewidth = 1) + 
#   scale_colour_viridis_d(option = "A", begin = 0.2, end = 0.9, direction = -1) +
#   ylim(0, 1) +
#   labs(x = "Wife age (years)", y = "Husband / Wife ratio") +
#   facet_grid(e0_f~e0_m) +
#   theme_linedraw(15)
# plot_spouse_ratio

parent_loss2 <- function(d){
  one_parent = signchange(d$parents <= 1, age = d$wife_age)
  tibble(
    oneparentage = one_parent$age[1],
    oneparentindex = one_parent$index[1],
    
    wife_surv = 100*round(d$wife_survival[oneparentindex], 2),
    husband_surv = 100*round(d$husband_survival[oneparentindex], 2),
    
    resid_kid_surv = round(d$dependents[oneparentindex], 2)
  )
}

mortality_grid3b <-
  mortality_grid3 |>
  dplyr::filter(e0_f == 35, e0_m == 30) |> 
  mutate(
    alb = paste(alb, "years"),
    IBI = paste("IBI: ", IBI),
    Gap = paste("Gap:", age_gap, "years"),
    Gap = fct_reorder(Gap, age_gap),
    oneparent = map(out, parent_loss2)
  ) |> 
  unnest(oneparent) |> 
  mutate(
    wife_surv = paste0(wife_surv, "%"),
  )

plot_oneparent <-
  ggplot(mortality_grid3b, aes(oneparentage, resid_kid_surv, colour = alb)) + 
  geom_point() + 
  geom_text(aes(label = wife_surv), position = position_nudge(x = -0.5), show.legend = F) +
  xlim(48, 54) +
  ylim(0, 3.5) +
  scale_color_binary() +
  guides(colour = guide_legend(title = "ALB", reverse = T)) +
  labs(x = "Age of wife when one parent remains alive", y = "Dependents") +
  facet_grid(Gap~IBI) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
# plot_oneparent

# plot_dependent_age_ratio <-
#   ggplot(mortality_grid2, aes(wife_age, mean_dependent_age, colour = ALB, group = ALB)) + 
#   geom_line(linewidth = 1.5) + 
#   scale_colour_viridis_d(option = "A", begin = 0.2, end = 0.9, direction = -1) +
#   labs(x = "Wife age (years)", y = "Mean age of dependents") +
#   facet_wrap(~IBI) +
#   theme_minimal(15)
# plot_dependent_age_ratio

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
