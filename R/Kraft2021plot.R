
p1 <- ggplot(HG_means, aes(value, y = 0, colour = name, shape = name)) + 
  geom_boxplot(notch = F) + 
  geom_point() +
  scale_color_viridis_d(option = "B", begin = 0.2, end = 0.8) +
  guides(color = guide_none(), shape = guide_none()) +
  # geom_text_repel(aes(label = Population)) +
  xlim(0, 8000) +
  facet_wrap(~name, ncol = 1) +
  theme_void() +
  theme(
    strip.text = element_blank()
    )

p2 <- 
  ggplot(HG_means, aes(value, Population2, colour = name, shape = name)) + 
  geom_point(alpha = 0) + # Hack\
  # Hadza female to male
  # From Pontzer et al. (2015) https://onlinelibrary.wiley.com/doi/full/10.1002/ajhb.22711
  annotate("rect", xmin = 1877, xmax = 2649, ymin = 1, ymax = 10.5, fill = 'grey', alpha = 0.2) + 

  # annotate("rect", xmin = 4124, xmax = 5804, ymin = 1, ymax = 10.5, fill = 'grey', alpha = 0.2) + # Two humans +/- 1 SD
  geom_vline(xintercept = 1877 + 2649, linetype = 'dotted', linewidth = 1) + # 1 male + 1 female Hadza
  
  # annotate("rect", xmin = 1983, xmax = 2979, ymin = 1, ymax = 10.5, fill = 'lightblue', alpha = 0.2) + # chimp +/- 1 SD
  # geom_vline(xintercept = 1576, linetype = 'dotted', linewidth = 1) + # chimp
  
  annotate("rect", xmin = 1478, xmax = 1675, ymin = 1, ymax = 10.5, fill = 'grey', alpha = 0.2) + # chimp female to male
  # geom_vline(xintercept = 1478, linetype = 'dotted', linewidth = 1) + # female chimps from Kraft et al.
  # geom_vline(xintercept = 1675, linetype = 'dotted', linewidth = 1) + # male chimps from Kraft et al.
  geom_point(size = 4) +
  # annotate("text", x = 2200, y = 9.5, label = 'Total energy expenditure\nChimpanzee & human range', hjust = 1) +
  scale_color_viridis_d(option = "B", begin = 0.2, end = 0.8) +
  guides(colour = guide_legend(title = element_blank()), shape = guide_legend(title = element_blank())) +
  xlim(0, 8000) +
  xlab('Energy production (kcals/day)') +
  # labs(caption = "Daily per capita adult energy production by contemporary hunter-gatherer females and males. Boxplots in the top panel indicate the distributions of female and male production depicted in the bottom panel. Vertical dotted lines indicate chimpanzee (left) and human (right) average total energy expenditure (TEE). Data from Kraft et al. (2021) and Pontzer et al. (2014).") +
  coord_cartesian(clip = 'off') +
  theme_minimal(20) +
  theme(
    axis.title.y = element_blank(),
    # plot.caption = marquee::element_marquee(size = 8, width = 50)
  )

plot_kraft_energy <- p1 + p2 + plot_layout(ncol = 1, heights = c(1,5))
# plot_energy
# ggsave("Kraft2021/plot_energy.pdf", plot_energy, width = 11, height = 5)
# ggsave("Kraft2021/plot_energy.svg", plot_energy, width = 11, height = 5)

# summarise(
#   d_means,
#   Ea_mean = mean(Ea_mean, na.rm = T),
#   Tf_mean = mean(Tf_mean, na.rm = T),
#   .by = Sex
# )
