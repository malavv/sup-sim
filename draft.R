library(tidyverse)

source("utils.R")

# This is the "single value strategy" meaning, we work on only one data point.

# Table 1 : Cancer Deaths per age and sex, Canada, 2009
dat <- read_csv("data/table_1.csv")

# Add a total row = male + female
dat$total = rowSums(dat[, c(2, 3)])

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