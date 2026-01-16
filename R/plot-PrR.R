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
