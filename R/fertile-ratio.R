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
  annotate("text", x = 35, y = 31, label = "Current study", hjust = 0, colour = "white") +
  coord_fixed() +
  theme_minimal(15) + theme(plot.subtitle = element_text(size = 11))
plot_fertile_ratio
