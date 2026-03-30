
# Misc functions

signchange0 <- function(v){
  chng <- c(0, diff(sign(v)))
  which(chng != 0)
}

signchange <- function(v, age){
  chng <- c(0, diff(sign(v)))
  loc <- which(chng != 0)
  tibble(index = loc, age = age[loc], direction = sign(chng)[loc])
}

pct_diff <- function(v, digits = 2){
  round(table(v)/length(v), digits)
}

# Fit hg_productivity to hadza kid data -----------------------------------

fit_hadza_kid_productivity <- function(d){
  hadza_adult_TEE <- mean(c(TEE2(20:60, NA, 'hadza'))) # avg of male and female
  
  fn <- function(params){
    sum( (d$kcals - hadza_adult_TEE*hg_productivity(d$age, NA, TEE_prop = params[1], alpha = params[2], b1 = params[3], age50 = params[4], group = 'hadza'))^2)
  }
  
  optim(c(1, 0.5, 0.30, 10), fn)
}
