# mysoln is a list to store answers

mysoln = list()

install.packages("FinCal")
library("FinCal")

#Function to calculate bond price
calculate_bond_price <- function(maturity, parvalue, couponrate, freq = 1, spotrates){
  
  bond_price <- 0
  for (i in seq(1, maturity/freq, 1)){
    bond_price <- bond_price + pv(spotrates[i], i, fv = parvalue * couponrate, 0, 0)
  }
  bond_price <- bond_price + pv(spotrates[i], maturity/freq, fv = parvalue, 0, 0)
}

#Bond Valuation Function
bond_valuation <- function(i, cf, t = seq(along = cf)){
  sum(cf / (1 + i)^t)
}

#Function to calculate YTM
calculate_ytm <- function(cf) {
  uniroot(bond_valuation, c(0,1), cf = cf)$root
}

#Function to calculate forward rate
calculate_fwd_rate <- function(r1, r2, t1, t){
  
  (((1 + r2)^(t1+t)/(1 + r1)^t1)^(1/t)) - 1
  
}

calculate_price_change <- function(price1, price2){
  ((price2-price1) * 100)/price1
}

# 1
spotrates <- c(0.01, 0.02, 0.03)

# a. Calculating bond price and YTM:

price <- calculate_bond_price(3, -100, 0.05, 1, spotrates)
cf <- c(-price, 5, 5, 105)
ytm <- calculate_ytm(cf)

#b. Calculating forward rates: Year 1 - Year 2, Year 2 - Year 3, Year 1 - Year 3

f1_1 <- calculate_fwd_rate(0.01, 0.02, 1, 1)
f2_1 <- calculate_fwd_rate(0.02, 0.03, 2, 1)
f1_2 <- calculate_fwd_rate(0.01, 0.03, 1, 2)

#c. Guaranteed 3 year return can be achieved in the following ways:
#   Invest in one year bond at one year rate and after one year at 2 year forward rate for two years
#   Invest in two year bond at two year rate and after two years at 1 year forward rate for one year
#   Invest in one year bond at one year rate and after one year at 1 year forward rate for one year and again after 1 year at 1 year forward rate for one year

# In all the above cases, the guaranteed return for three years is 3%

return.3yr <- 0.03

# save down your final answers for part a, b, and c
a = c(price, ytm)
b = c(f1_1, f2_1, f1_2) #increase/decrease vector depending on how many forward rates you can calculate
c = return.3yr

# add answers to list for "Q2"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2
# Answered a and c in pdf
#b. Calculating 2 year forward rate one year from now: 
fwd_1_2 <- calculate_fwd_rate(0.01, 0.05, 1, 2)
b = fwd_1_2
mysoln[["Q2"]] = list(b=b)

# 3

# save down your final answers for part b,c,d,and e (a and f in PDF writeup)
#a = "Answered in PDF Write Up"
#b. Computing bond prices for bonds A, B, C, D:

cf_ac_b <- rep(0.01, 20)
cf_bd_b <- rep(0.01, 60)

price_a <- calculate_bond_price(10, -100, 0.005, 0.5, cf_ac_b)

price_b <- calculate_bond_price(30, -100, 0.005, 0.5, cf_bd_b)

price_c <- calculate_bond_price(10, -100, 0.03, 0.5, cf_ac_b)

price_d <- calculate_bond_price(30, -100, 0.03, 0.5, cf_bd_b)

b = c(price_a,price_b,price_c,price_d) #(Bond A, Bond B, Bond C, Bond D)

#c. Computing bond prices and change in bond's price when yield is changed from 2% to 2.5%

cf_ac_c <- rep(0.0125, 20)
cf_bd_c <- rep(0.0125, 60)

bp_a <- calculate_bond_price(10, -100, 0.005, 0.5, cf_ac_c)

bp_b <- calculate_bond_price(30, -100, 0.005, 0.5, cf_bd_c)

bp_c <- calculate_bond_price(10, -100, 0.03, 0.5, cf_ac_c)

bp_d <- calculate_bond_price(30, -100, 0.03, 0.5, cf_bd_c)

change_a <- calculate_price_change(price_a, bp_a)
change_b <- calculate_price_change(price_b, bp_b)
change_c <- calculate_price_change(price_c, bp_c)
change_d <- calculate_price_change(price_d, bp_d)

c.prices = c(bp_a,bp_b,bp_c,bp_d) #(Bond A, Bond B, Bond C, Bond D)
c.changes = c(change_a,change_b,change_c,change_d) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form

#Note that positive change here indicates increase in bond prices.

#c. Computing bond prices and change in bond's price when yield is changed from 2% to 1.5%

cf_ac_d <- rep(0.0075, 20)
cf_bd_d <- rep(0.0075, 60)

bp_a <- calculate_bond_price(10, -100, 0.005, 0.5, cf_ac_d)

bp_b <- calculate_bond_price(30, -100, 0.005, 0.5, cf_bd_d)

bp_c <- calculate_bond_price(10, -100, 0.03, 0.5, cf_ac_d)

bp_d <- calculate_bond_price(30, -100, 0.03, 0.5, cf_bd_d)

change_a <- calculate_price_change(price_a, bp_a)
change_b <- calculate_price_change(price_b, bp_b)
change_c <- calculate_price_change(price_c, bp_c)
change_d <- calculate_price_change(price_d, bp_d)

d.prices = c(bp_a,bp_b,bp_c,bp_d) #(Bond A, Bond B, Bond C, Bond D)
d.changes = c(change_a,change_b,change_c,change_d) #(Bond A % Chg, Bond B % Chg, Bond C % Chg, Bond D % Chg) in decimal form

#Note that negative change here indicates decrease in bond prices.

# add answers to list for "Q2"
mysoln[["Q3"]] = list(#a=a, put in PDF writeup only
  b=b, 
  c.pric = c.prices, 
  c.chg = c.changes, #changes are percent changes in decimal
  d.pric = d.prices, 
  d.chg = d.changes) #changes are percent changes in decimal




# return my solution
mysoln