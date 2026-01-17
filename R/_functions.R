
# Misc functions

signchange0 <- function(v){
  chng <- c(0, diff(sign(v)))
  which(chng != 0)
}

signchange <- function(v, age = AFB:80){
  chng <- c(0, diff(sign(v)))
  loc <- which(chng != 0)
  tibble(index = loc, age = age[loc], direction = sign(chng)[loc])
}

pct_diff <- function(v, digits = 2){
  round(table(v)/length(v), digits)
}
