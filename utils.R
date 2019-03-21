# R uses the IEEE 754 recommendation of "go to the even digit"
# but SAS uses "ties away from zero" comes from http://alandgraf.blogspot.com/2012/06/rounding-in-r.html
sas_round = function(x, n) {
  posneg = sign(x)
  z = abs(x) * 10 ^ n
  z = z + 0.5
  z = trunc(z)
  z = z / 10 ^ n
  z * posneg
}

round_even_5 = function(nums) {
  round(nums / 5) * 5
}

round_away_5 = function(nums) {
  sas_round(nums / 5, 0) * 5
}

ceil5 = function(nums) {
  ceiling(nums / 5) * 5
}

floor5 = function(nums) {
  floor(nums / 5) * 5
}

round_rand_5 = function(nums) {
  shouldCeil = rbernoulli(1, p = (nums %% 5) / 5)
  mapply(function(x1, x2) {
    ifelse(x2, ceil5(x1), floor5(x1))
  }, nums, shouldCeil)
}