
plot_energybalance <- function(d, dmean, alpha = 0.01){
  
  colrs <- viridis(5, option = "H")
  fntsize <- 2.9
  
  d <- rename(d, `Parent\nproduction` = Parent_production)
  
  p1 <- 
    ggplot(dmean, aes(x = wife_age, group = age_gap)) + 
    geom_line(aes(y = family_size), colour = colrs[1]) +
    geom_line(aes(y = resident_children), colour = colrs[2]) +
    geom_line(aes(y = wife_survival + husband_survival), colour = colrs[3]) +
    geom_line(aes(y = wife_survival), colour = colrs[4]) +
    geom_line(aes(y = husband_survival), colour = colrs[5]) +
    
    annotate("text", label = "Total family size",  x = 60, y = 5.5, hjust = 0, size = fntsize, colour = colrs[1]) +
    annotate("text", label = "Resident children",  x = 60, y = 5.1, hjust = 0, size = fntsize, colour = colrs[2]) +
    annotate("text", label = "Surviving parents",  x = 60, y = 4.7, hjust = 0, size = fntsize, colour = colrs[3]) +
    annotate("text", label = "Surviving wives",    x = 60, y = 4.3, hjust = 0, size = fntsize, colour = colrs[4]) +
    annotate("text", label = "Surviving husbands", x = 60, y = 3.9, hjust = 0, size = fntsize, colour = colrs[5]) +
    
    labs(x = "Wife age (years)", y = "Family\nnumbers") +
    facet_wrap(~Menopause) +
    theme_linedraw(15) +
    theme(axis.title.y = element_text(angle = 0, hjust = 1))
  
  p2 <-
    ggplot(dmean, aes(wife_age, mean_family_consumption, group = age_gap)) + 
    geom_line() +
    labs(x = "Wife age (years)", y = "Daily family energy\nconsumption (kcals)") +
    facet_wrap(~Menopause) + 
    theme_linedraw(15) +
    theme(
      axis.title.y = element_text(angle = 0, hjust = 1),
      strip.text.x = element_blank()
    )
  
  p3 <-
    ggplot(d, aes(wife_age, family_production, colour = `Parent\nproduction`, group = ParamSet)) + 
    geom_line(alpha = alpha) +
    geom_line(data = dmean, aes(wife_age, mean_production, group = age_gap), colour = 'lightblue') +
    scale_color_viridis_c(option = "A") +
    # guides(colour = guide_legend(title = "Parent production")) +
    labs(x = "Wife age (years)", y = "Daily family energy\nproduction (kcals)") +
    facet_wrap(~Menopause) + 
    theme_linedraw(15) +
    theme(
      axis.title.y = element_text(angle = 0, hjust = 1),
      strip.text.x = element_blank()
    )
  
  p4 <-
    ggplot(d, aes(wife_age, energy_balance, colour = `Parent\nproduction`, group = ParamSet)) + 
    geom_line(alpha = alpha) +
    geom_line(data = dmean, aes(wife_age, mean_energybalance, group = age_gap), colour = 'lightblue') +
    geom_hline(yintercept = 0, colour = 'red') +
    scale_color_viridis_c(option = "A") +
    # guides(colour = guide_legend(title = "Parent production")) +
    labs(x = "Wife age (years)", y = "Daily family energy\nbalance (kcals)") +
    facet_wrap(~Menopause) + 
    theme_linedraw(15) +
    theme(
      axis.title.y = element_text(angle = 0, hjust = 1),
      strip.text.x = element_blank()
    )
  
  p1 + p2 + wrap_plots(p3 / p4 +  plot_layout(axes = "collect_x", guides = "collect"))+
    plot_layout(ncol = 1, axes = "collect_x", heights = c(1, 1, 2.5)) + 
    plot_annotation(tag_levels = "A") & 
    theme(plot.tag.position = "topright")
}


# Family energy production over the lifecourse in menopause condition

nm_dict <- c(
  mean_wife_production = "Wife", 
  mean_husband_production = "Husband", 
  mean_total_child_production = "All children",
  mean_wife_consumption = "Wife", 
  mean_husband_consumption = "Husband", 
  mean_total_child_consumption = "All children"
)

energyProd <- 
  lifecourse_mean2 |> 
  pivot_longer(
    c(mean_wife_production, mean_husband_production, mean_total_child_production), 
    names_to = 'Member', 
    values_to = 'production'
  ) |> 
  mutate(
    Member = nm_dict[Member]
  )

plot_energyProd <-
  ggplot(energyProd, aes(wife_age, production, fill = Member)) + 
  geom_col() +
  scale_fill_viridis_d(option = "A", begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "Wife age (years)", y = "Production (kcals)") +
  ylim(0, 10500) +
  facet_wrap(~Menopause) +
  theme_minimal(15) +
  theme(strip.text = element_blank())
plot_energyProd

energyConsumers <- 
  lifecourse_mean2 |> 
  pivot_longer(
    c(mean_wife_consumption, mean_husband_consumption, mean_total_child_consumption), 
    names_to = 'Member', 
    values_to = 'consumption'
  ) |> 
  mutate(
    Member = nm_dict[Member]
  )

plot_energyConsumers <-
  ggplot(energyConsumers, aes(wife_age, consumption, fill = Member)) + 
  geom_col() +
  scale_fill_viridis_d(option = "A", begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "Wife age (years)", y = "Consumption (kcals)") +
  ylim(0, 10500) +
  facet_wrap(~Menopause) +
  theme_minimal(15)
# plot_energyConsumers

# p <- plot_energyConsumers + plot_energyProd + 
#   plot_layout(guides = 'collect', axes = 'collect', ncol = 1) & 
#   theme(legend.position = 'top', legend.title = element_blank())

plot_kraft_energy <- png::readPNG("Figures/plot_kraft_energy.png", native = T, info = T)

plot_consumption_production <- 
  wrap_elements(full = plot_kraft_energy) + 
  wrap_plots(
    plot_energyConsumers + plot_energyProd + 
      plot_layout(guides = 'collect', axes = 'collect', ncol = 1)
  ) +
  plot_layout(ncol = 1, heights = c(3,2)) +
  plot_annotation(tag_levels = "A") & 
  theme(legend.position = 'top', legend.title = element_blank())
# plot_consumption_production

# Positive balance without menopause --------------------------------------

# Joint productivity <= 2.8 adult male TEE
outdf2.8 <-
  outdf |> 
  dplyr::filter(TEE_prop_f == 1, TEE_prop_m > 1, alpha_m == 0.5, alpha_f == 0.5, age_gap == 5, menopause_age == 80) |> 
  mutate(
    b1_m = factor(b1_m, levels = (unique(b1_m))),
    b1_f = ordered(b1_f),
    alpha_m = ordered(alpha_m),
    age_gap = ordered(age_gap),
  )

plot2.8 <-
  ggplot(outdf2.8, aes(wife_age, energy_balance, colour = b1_f, group = ParamSet)) +
  geom_line(alpha = 1) +
  geom_hline(yintercept = 0, colour = 'red') +
  scale_colour_viridis_d(option = "A", end = 0.8) +
  guides(colour = guide_legend("b1 (female)", reverse = T, override.aes = list(linewidth = 2))) +
  labs(x = "Wife age (years)", y = "Energy balance (kcals)") +
  facet_grid(b1_m ~ TEE_prop_m) +
  theme_bw(15) +
  theme(strip.text.y = element_text(angle = 0))
plot2.8

# Multiple ages of menopause ----------------------------------------------

plot_outdf3 <-
  ggplot(outdf3, aes(wife_age, energy_balance, colour = `Parent_production`, group = ParamSet)) + 
  geom_line(alpha = 0.01) +
  geom_line(data = outdf3mean, aes(wife_age, mean_energybalance, group = age_gap), colour = 'lightblue') +
  geom_hline(yintercept = 0, colour = 'red') +
  scale_color_viridis_c(option = "A") +
  guides(colour = guide_colorbar(title = "Parent\nproduction")) +
  labs(x = "Wife age (years)", y = "Daily family energy\nbalance (kcals)") +
  facet_wrap(~Menopause) + 
  theme_bw(15)

clrs <- viridisLite::turbo(4)

plot_outdf3mean <-
  ggplot(outdf3mean, aes(wife_age, mean_energybalance, colour = factor(menopause_age))) + 
  geom_line(alpha = 1) +
  geom_hline(yintercept = 0, colour = 'red') +
  
  annotate("text", label = fertility_mean[1], x = 32, y = -500, hjust = 0, colour = clrs[1]) +
  annotate("text", label = fertility_mean[2], x = 38, y = -500, hjust = 0, colour = clrs[2]) +
  annotate("text", label = fertility_mean[3], x = 44, y = -500, hjust = 0, colour = clrs[3]) +
  annotate("text", label = fertility_mean[4], x = 73, y = -500, hjust = 0, colour = clrs[4]) +
  
  scale_color_viridis_d(option = "H") +
  guides(colour = guide_legend(title = "Menopause age", position = "top", override.aes = list(linewidth = 2))) +
  labs(x = "Wife age (years)", y = "Daily family energy\nbalance (kcals)") +
  theme_bw(15)
plot_outdf3mean

# Energy balance with and without juvenile production ---------------------

# Unrestricted parameter space, menopause condition

nokids <- function(d){
  nokids_df <- 
    d |> 
    pivot_longer(c(energy_balance, energy_balance2), names_to = "KidProd", values_to = 'energy_balance') |> 
    mutate(
      KidProd = ifelse(KidProd == "energy_balance", "With juvenile production", "Without juvenile production")
    )
  
  nokids_mean <-
    d |> 
    out_mean() |> 
    pivot_longer(c(mean_energybalance, mean_energybalance2), names_to = "KidProd", values_to = 'energy_balance') |> 
    mutate(
      KidProd = ifelse(KidProd == "mean_energybalance", "With juvenile production", "Without juvenile production")
    )
  
  ggplot(nokids_df, aes(wife_age, energy_balance, group = ParamSet)) + 
    geom_line(alpha = 0.01) +
    geom_line(data = nokids_mean, group = NA, colour = 'lightblue') +
    geom_hline(yintercept = 0, colour = "red") +
    labs(x = "Wife age (years)", y = "Energy balance (kcals)") +
    facet_wrap(~KidProd) + 
    theme_minimal(15)
}

plot_nokids <- nokids(outdfsplit_menopause)
