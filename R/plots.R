
# Post-reproduction representation ----------------------------------------

wood <-
  read_csv(here("data", "wood2023.csv")) |> 
  mutate(
    PrR_max = max(PrR),
    x0 = min(PrR),
    .by = Species
  ) |> 
  mutate(
    Species = str_replace_all(Species, "\\*", ""), # Temp remove markdown formatting
    Species = fct_reorder(Species, PrR_max)
  )

plot_PrR <-
  ggplot(wood, aes(PrR, Species)) +
  geom_segment(aes(x = x0, xend = PrR_max)) +
  geom_point(size = 3) +
  annotate(GeomMarquee, label = "*Orcinus orca*", x = 0.53, y = 13.8, lineheight = 0.8, hjust = 1) +
  annotate(GeomMarquee, label = "*Globicephala macrorhynchus*", x = 0.26, y = 13.8, lineheight = 0.8) +
  annotate("text", label = "Plantation\nslaves", x = 0.3, y = 11.0, lineheight = 0.8, size = 4) +
  annotate("text", label = "Hunter\ngatherers", x = 0.435, y = 11.0, lineheight = 0.8, size = 4) +
  annotate("text", label = "Historical\nSweden", x = 0.48, y = 11.0, lineheight = 0.8, size = 4, hjust = 0) +
  labs(x = "Post-reproductive representation (PrR)", y = "") +
  theme_minimal(15) + theme(axis.text.y = element_marquee()) +
  coord_cartesian(ylim = c(0, 14), clip = 'off')
plot_PrR


# Lifetables --------------------------------------------------------------

LTS <- bind_rows(
  Kung_Both = lifetable("kung", sex = 0),
  Ache_Female = lifetable("ache", sex = 0),
  Ache_Male = lifetable("ache", sex = 1),
  Hadza_Female = lifetable("hadza", sex = 0),
  Hadza_Male = lifetable("hadza", sex = 1),
  UN_Female = lifetable("avg", sex = 0, e0 = 35),
  UN_Male = lifetable("avg", sex = 1, e0 = 30),
  .id = "Group"
) |> 
  mutate(x = ifelse(is.na(x), Age, x)) |>
  dplyr::select(x, lx, Group) |> 
  separate_wider_delim(Group, delim = "_", names = c("Group", "Sex"))

plot_lifetables <-
  ggplot(LTS[LTS$x <= 75,], aes(x, lx, colour = Sex)) + 
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "A", end = 0.8) +
  # scale_color_scico_d(palette = 'managua') +
  guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
  labs(x = "Age (years)", y = "Survival") +
  facet_wrap(~Group, ncol = 2) +
  theme_bw(15) +
  theme(legend.position = 'top', legend.title = element_blank())
# plot_lifetables

# Growth ------------------------------------------------------------------

wt_params <- 
  expand_grid(group = c("ache", "hadza", "kung", "avg"), age = 0:80, sex = c(0, 1), pregnant = 0)|> 
  arrange(sex, group, age)

# IBI = 3 years (no menopause or fertility decline)
wt_params$pregnant <- c(rep(c(rep(0, 18), rep(c(0, 0, 1), 21))[1:81], 4), rep(0, 324))

wt_params$Weight <- pmap_dbl(wt_params, hg_weight)
wt_params <-
  wt_params |> 
  mutate(
    TEE = TEE2(age, sex, group, pregnant),
    Sex = ifelse(sex == 0, "Female", "Male"),
    group = str_to_title(group),
    group = str_replace(group, "Avg", "Average"),
    group = str_replace(group, "Kung", "!Kung")
  ) 

plot_growth <-
  ggplot(wt_params[wt_params$age <= 75,], aes(age, Weight, colour = group)) +
  geom_line(linewidth = 1) +
  # scico::scale_color_scico_d(palette = "roma") +
  scale_color_viridis_d(option="H", end = 0.9) +
  guides(colour = guide_legend("", position = "top", override.aes = list(linewidth = 2))) +
  ylim(0, NA) +
  labs(x = "Age (years)", y = "Weight (kg)") +
  facet_wrap(~Sex) +
  theme_minimal(15)
# plot_growth

# TEE ---------------------------------------------------------------------

plot_TEE <-
  ggplot(wt_params, aes(age, TEE, colour = group)) +
  geom_line(linewidth = 1) +
  # scico::scale_color_scico_d(palette = "roma") +
  scale_color_viridis_d(option="H", end = 0.9) +
  guides(colour = guide_legend("", position = "top", override.aes = list(linewidth = 2))) +
  ylim(0, NA) +
  labs(x = "Age (years)", y = "TEE (kcals/day)") +
  facet_wrap(~Sex) +
  theme_minimal(15) +
  theme(strip.text = element_blank())
# plot_TEE

# Productivity ------------------------------------------------------------


## Kraft et al 2020 ------------------------------------------------------

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

HG_means3 <-
  HG_means2 |>
  mutate(
    Population = fct_reorder(Population, Total)
  ) |> 
  pivot_longer(Male:Total, names_to = "Sex", values_to = "kcals")

pKraft1 <- 
  ggplot(HG_means3, aes(kcals, y = 0, colour = Sex, shape = Sex)) + 
  geom_boxplot(notch = F, show.legend = F) + 
  geom_point(show.legend = F) +
  scale_color_viridis_d(option = "B", begin = 0.2, end = 0.8) +
  # guides(colour = guide_legend(title = element_blank()), shape = guide_legend(title = element_blank())) +
  xlim(0, 8000) +
  facet_wrap(~Sex, ncol = 1) +
  ggplot2::theme_void() +
  theme(
    strip.text = element_blank()
  )
pKraft1

TEEavg_f <- mean(TEE2(20:60, 0, 'avg'))
TEEavg_m <- mean(TEE2(20:60, 1, 'avg'))
TEEavg <- mean(c(TEEavg_f, TEEavg_m))

pKraft2 <- 
  ggplot(HG_means3, aes(kcals, Population, colour = Sex, shape = Sex)) + 
  annotate("rect", xmin = TEEavg_f, xmax = TEEavg_m, ymin = 1, ymax = 10.5, fill = 'grey', alpha = 0.2) + 
  geom_vline(xintercept = TEEavg_f + TEEavg_m, linetype = 'dotted', linewidth = 1) + 
  geom_point(size = 4) +
  scale_x_continuous(limits = c(0, 8000), sec.axis = sec_axis(~ . / TEEavg, breaks = seq(0, 3.5, 0.5))) +
  scale_color_viridis_d(option = "B", begin = 0.2, end = 0.8) +
  guides(colour = guide_legend(title = NULL), shape = guide_legend(title = NULL)) +
  # xlim(0, 8000) +
  xlab('Energy production (kcals/day)') +
  coord_cartesian(clip = 'off') +
  theme_minimal(20) +
  theme(
    axis.title.y = element_blank()
  )
# pKraft2

plot_kraft_energy <- pKraft1 + pKraft2 + plot_layout(ncol = 1, heights = c(1,5), guides = "collect") & theme(legend.position = "top")
# plot_kraft_energy

## Skill ontogeny ----------------------------------------------------------

prod_params <- tibble(
  b1 = c(0.30, 0.2, 0.15),
  age50 = c(10, 15, 20)
)

skill_ontogeny <- 
  map2(prod_params$b1, prod_params$age50, \(b1, age50) tibble(age = 0:80, skill = hg_skill(age = 0:80, b1 = b1, age50 = age50, b2 = -0.15))) |> 
  list_rbind(names_to = "Skill\nontogeny") |> 
  mutate(
    `Skill\nontogeny` = case_when(
      `Skill\nontogeny` == 1 ~ "Fast",
      `Skill\nontogeny` == 2 ~ "Medium",
      `Skill\nontogeny` == 3 ~ "Slow"
    )
  )

plot_skill_ontogeny <- 
  ggplot(skill_ontogeny, aes(age, skill, colour = `Skill\nontogeny`)) +
  geom_line(linewidth = 2) +
  scale_color_viridis_d(option = "A", end = 0.9) +
  labs(x = "Age (years)", y = "Skill") +
  theme_minimal(15) +
  theme(legend.position = "right")
# plot_skill_ontogeny

hadza_adult_TEE <- mean(c(TEE2(20:60, NA, 'hadza')))
out <- fit_hadza_kid_productivity(hadza_kids_total)
hkp <- tibble(
  age = 5:14,
  kcals = hadza_adult_TEE*hg_productivity(5:14, NA, TEE_prop = out$par[1], alpha = out$par[2], b1 = out$par[3], age50 = out$par[4])
)

plot_hadza_kids <-
  ggplot(hadza_kids_total, aes(age, kcals)) +
  geom_point() +
  # geom_line(data = hkp, alpha = 0.5) +
  # geom_textline(data = hkp, alpha = 0.5, vjust = -0.5, hjust = 0.85, label = "Fitted productivity curve") +
  geom_line(data = hkp, alpha = 0.5) +
  geom_hline(yintercept = TEE2(30, 1, "hadza"), linetype = 'dotted', colour = "black") +
  geom_hline(yintercept = TEE2(30, 0, "hadza"), linetype = 'dotted', colour = "black") +
  geom_hline(yintercept = TEE2(10, NA, "hadza"), linetype = 'dotted', colour = "black") +
  annotate("text", x = 5, y = TEE2(30, 1, "hadza") + 150, label = "Adult male Hadza TEE", size = 3, hjust = 0, alpha = 0.75) +
  annotate("text", x = 5, y = TEE2(30, 0, "hadza") + 150, label = "Adult female Hadza TEE", size = 3, hjust = 0, alpha = 0.75) +
  annotate("text", x = 5, y = TEE2(10, NA, "hadza") - 150, label = "Child Hadza (age 10) TEE", size = 3, hjust = 0, alpha = 0.75) +
  scale_x_continuous(breaks = 5:15) +
  scale_y_continuous(breaks = seq(0, 7000, 1000), sec.axis = sec_axis(~ . / TEEavg, name = "TEE prop", breaks = seq(0, 3, 0.5))) +
  labs(x = "Age (years)", y = "Productivity (kcals/day)") +
  theme_classic(15)
# plot_hadza_kids

# Ratio of fertile females to adult males ---------------------------------

fertile_ratio <- function(e0_f, e0_m, SRB = 1.05, ALB = 40, max_age = 60){
  
  mx_m <- lt_col_interpolate('mx', e0_m, 'Male')
  mx_f <- lt_col_interpolate('mx', e0_f, 'Female')
  
  ax_m <- lt_col_interpolate('ax', e0_m, 'Male')
  ax_f <- lt_col_interpolate('ax', e0_f, 'Female')
  
  lt_m <- LT_from_mx(mx = mx_m, ax = ax_m, sex = "Male", SRB = SRB)
  lt_f <- LT_from_mx(mx = mx_f, ax = ax_f, sex = "Female", SRB = SRB)  
  # return(lt_f) # For debugging
  # return(
  #   lt_m$lx[lt_m$x == 55] / lt_f$lx[lt_f$x == 45]
  #   )
  
  # Total fertile females
  n_women_fertile <- sum(map_dbl(18:ALB, \(age) lt_f$lx[lt_f$x == age]))
  
  # Total fertile males
  n_men_fertile <- sum(map_dbl(18:max_age, \(age) lt_m$lx[lt_m$x == age]))
  
  n_women_fertile / n_men_fertile
  
}

# Compute ratio of fertile females to adult males
fertile_params <- expand_grid(e0_f = 25:70, e0_m = 25:70)
fertile_params$ratio <- map2_dbl(fertile_params$e0_f, fertile_params$e0_m, fertile_ratio, .progress = T)

plot_fertile_ratio <- 
  fertile_params |> 
  dplyr::filter(e0_f <= 45, e0_m <= 45) |> 
  ggplot(aes(e0_f, e0_m, fill = ratio)) + 
  geom_raster() + 
  geom_textabline(intercept = 0, slope = 1, colour = 'yellow', label = 'Equal life expectancy', hjust = 0.8) +
  geom_textabline(intercept = -5, slope = 1, colour = 'yellow', label = '5 year difference', hjust = 0.8) +
  geom_textcontour(aes(z = ratio, label = after_stat(scales::number(level, accuracy = 0.01))), colour = 'white', alpha = 0.75, hjust = 0.35) +
  scale_fill_scico(palette = 'vik', midpoint = 1, direction = 1, limits = c(0.30, 1.1)) +
  guides(fill = guide_colorbar(title = 'Ratio', reverse = T)) +
  labs(
    title = "Ratio of fertile women (18-40) to adult men (18-60)",
    x = expression("Female life expectancy at birth (e"[0]*")"),
    y = expression("Male life expectancy at birth (e"[0]*")")
  ) +
  
  annotate("point", x = 35, y = 30, colour = 'white', fill = "red", size = 4, pch = 21) +
  annotate("text", x = 35.25, y = 29.5, label = "Current study", hjust = 0, colour = "white") +
  coord_fixed() +
  theme_minimal(15) + theme(plot.subtitle = element_text(size = 11))
plot_fertile_ratio

