# My fit to Koster et al. 2020 data

# Prepare data from cchunts
# units are kg meat per trip

get_koster2020 <- function(){
  make_joint() %>%
    rename(age = age_dist_1) |> 
    mutate(
      uniqueid = factor(paste(society_id, forager_id)),
      society = factor(society),
      independent = group_type == "Independent"
    ) %>% 
    rowwise() %>% 
    mutate(
      mixed = length(unique(na.omit(c_across(contains('sex'))))) > 1,
      group_size = length(na.omit(c_across(matches('a_\\d_id')))) + 1
    ) %>% 
    ungroup() #|> 
  #dplyr::filter(sex == "M", pooled == 0, age_type != "Uniform", !society %in% c("Nielsen", "Marks"))
}

fit_koster2020 <- function(d){
  glmmTMB(
    harvest ~ independent + 
      s(age) + 
      (independent | society / uniqueid), 
    family = ziGamma(link="log"), 
    ziformula = ~ independent + s(age) + (independent | society / uniqueid), 
    REML = T, 
    data = d
  )
}
