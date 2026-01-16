
plot_energybalance <- function(d, dmean, alpha = 0.01){
  
  colrs <- viridis(5, option = "H")
  fntsize <- 3.7
  
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
    ggplot(dmean, aes(wife_age, family_consumption, group = age_gap)) + 
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
  
  p1 + p2 + p3 + p4 + 
    plot_layout(ncol = 1, axes = "collect_x", guides = "collect") + 
    plot_annotation(tag_levels = "A") & 
    theme(plot.tag.position = "topright")
}


# plot_childproduction2 <-
#   ggplot(outdf[outdf$child_age<18,], aes(child_age, total_child_production, group = ParamSet)) + 
#   geom_line(alpha = 0.01) + 
#   geom_line(aes(y = total_child_consumption), colour = "red") +
#   geom_smooth(group = 1) +
#   labs(x = "Oldest child age (years)", y = "Total child consumption/production (kcals)") +
#   theme_minimal(15)