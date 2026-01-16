# Fit hg_productivity to hadza kid data -----------------------------------

fit_hadza_kid_productivity <- function(d){
  hadza_adult_TEE <- mean(c(TEE2(20:60, NA, 'hadza'))) # avg of male and female
  
  fn <- function(params){
    sum( (d$kcals - hadza_adult_TEE*hg_productivity(d$age, NA, TEE_prop = params[1], alpha = params[2], b1 = params[3], age50 = params[4], group = 'hadza'))^2)
  }
  
  optim(c(1, 0.5, 0.30, 10), fn)
}
