library(tidyverse)

# This is the "single value strategy" meaning, we work on only one data point.

# Table 1 : Cancer Deaths per age and sex, Canada, 2009
dat <- read_csv("data/table_1.csv")

# Add a total row = male + female
dat$total = rowSums(dat[, c(2, 3)])

# R uses the IEEE 754 recommendation of "go to the even digit"
# but SAS uses "ties away from zero" comes from http://alandgraf.blogspot.com/2012/06/rounding-in-r.html
sas_round = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

round_even_5 = function(nums) { round(nums / 5) * 5 }
round_away_5 = function(nums) { sas_round(nums / 5, 0) * 5 }
ceil5 = function(nums) { ceiling(nums / 5) * 5 }
floor5 = function(nums) { floor(nums / 5) * 5 }
round_rand_5 = function(nums) {
  shouldCeil = rbernoulli(1, p = (nums %% 5) / 5)
  mapply(function(x1, x2) { ifelse(x2, ceil5(x1), floor5(x1)) }, nums, shouldCeil)
}

# Classical Rounding
cat("Each cell is rounded to the nearest multiple of 5, including margins.")

cat("Classical Rounding using 'even digit' rule")
dat %>% mutate_if(is.numeric, round_even_5)

cat("Classical Rounding using 'away from zero' rule")
dat %>% mutate_if(is.numeric, round_away_5)

# Random Rounding
set.seed(0xD15EA5E)

cat("Each cell is rounded up with a prob. equald to how close it is to the next multiple of 5, including margins.")
dat %>% mutate_if(is.numeric, round_rand_5)

# Controlled Rounding
cat("I'm not implementing it, since we don't need it for the moment. (and it would require constraints programming)")