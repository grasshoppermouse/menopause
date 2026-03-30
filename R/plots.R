

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
plot_growth

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
plot_TEE

# Productivity ------------------------------------------------------------

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
plot_skill_ontogeny

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
  annotate("text", x = 5, y = TEE2(30, 1, "hadza") + 150, label = "Male Hadza TEE", size = 3, hjust = 0, alpha = 0.75) +
  annotate("text", x = 5, y = TEE2(30, 0, "hadza") + 150, label = "Female Hadza TEE", size = 3, hjust = 0, alpha = 0.75) +
  annotate("text", x = 5, y = TEE2(10, NA, "hadza") - 150, label = "Child Hadza (age 10) TEE", size = 3, hjust = 0, alpha = 0.75) +
  scale_x_continuous(breaks = 5:15) +
  labs(x = "Age (years)", y = "Productivity (kcals)") +
  theme_classic(15)
plot_hadza_kids

# dk2020 <- tibble(
#   Age = 0:80,
#   Productivity = koster2020skill(Age)
# )
# 
# plot_koster2020 <-
#   ggplot(dk2020, aes(Age, Productivity)) +
#   geom_line() +
#   labs(x = "Age (years)", y = "Skill") +
#   theme_minimal(15)
# plot_koster2020
# 
# kaplan2000male <- png::readPNG("Figures/kaplan2000male.png", native = T)
# kaplan2000female <- png::readPNG("Figures/kaplan2000female.png", native = T)
# schniter2015 <- png::readPNG(here("Figures/schniter2015.png"), native = T)
# 
# plot_skills <-
#   wrap_elements(kaplan2000male) + kaplan2000female +
#   plot_hadza_kids + schniter2015 + 
#   plot_koster2020 + plot_skill_ontogeny + 
#   plot_annotation(tag_levels = "A") + plot_layout(ncol = 2)
# plot_skills

# Parameter table plots ---------------------------------------------------

plot_param_matrix <- function(d1, d2, p1, p2, digits = 2) {
  p1v1 <- d1[[p1]]
  p2v1 <- d1[[p2]]
  p1v2 <- d2[[p1]]
  p2v2 <- d2[[p2]]
  num1 = length(p1v1)
  num2 = length(p2v2)
  tbl1 <- table(p1v1, p2v1)
  tbl2 <- table(p1v2, p2v2)
  expected <- (num1/num2) * tbl2
  out <- (tbl1 - expected)/expected
  suppressMessages(
    hagenheat(round(out, digits), seriation_method = "Identity", display_values = T) +
      scico::scale_fill_scico(palette = "vik", midpoint = 0, limits = c(-1, 1.5)) +
      guides(fill = guide_colorbar(title = "Deviation")) +
      xlab(p2) + 
      ylab(p1)
  )
}

plot_EB_param_matrix <- function(d, p1, p2, indices, limits, digits = 2){
  out <- 
    d |>
    slice(indices, .by = ParamSet) |> 
    mutate(across(all_of(c(p1, p2)), as.ordered)) |> 
    summarise(mean_EB = mean(energy_balance), .by = all_of(c(p1, p2)))
  ggplot(out, aes(.data[[p1]], .data[[p2]], fill = mean_EB)) +
    geom_tile() +
    geom_text(aes(label = round(mean_EB, 0)), size = 3, color = "white") +
    scico::scale_fill_scico(palette = "bam", midpoint = 0, limits = limits) +
    guides(fill = guide_colorbar(title = "Mean energy balance")) +
    theme_minimal(10)
}
