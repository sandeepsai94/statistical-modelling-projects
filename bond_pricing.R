# Bond Pricing 
# mysoln is a list to store answers

mysoln = list()

install.packages("FinCal")
library("FinCal")

#Function to calculate bond price
calculate_bond_price <- function(maturity, parvalue, couponrate, freq = 1, spotrates){
  
  bond_price <- 0
  for (i in seq(1, maturity/freq, 1)){
    bond_price <- bond_price + pv(spotrates[i], i * freq, fv = parvalue * couponrate, 0, 0)
  }
  bond_price <- bond_price + pv(spotrates[i], maturity, fv = parvalue, 0, 0)
}

#Function to calculate spot rate
calculate_spot_rate <- function(payment, bondprice, freq=1, n){
  
  spot_rate <- ((payment/bondprice)^(1/n) - 1 ) * freq #Multiplying by frequency to calculate APR
  
}

# 1

spotrates <- c(0.02, 0.025, 0.03, 0.035)

#a. Price of Zero coupon bond with 3 years to maturity: 

price_a <- calculate_bond_price(3, -100, 0, 1, spotrates)

#b. Price of bond with coupon rate 1% and 2 years to maturity:
price_b <- calculate_bond_price(2, -100, 0.01, 1, spotrates)

#c. Price of bond with coupon rate 13% and 4 years to maturity:
price_c <- calculate_bond_price(4, -100, 0.13, 1, spotrates)

# save down your final answers for part a, b, and c
a = price_a
b = price_b 
c = price_c 

# add answers to list for "Q1"
mysoln[["Q1"]] = list(a=a, b=b, c=c)

# 2

#Calculating 6 month spot-rate (Expressed as APR with semiannual compunding)
r.6month <- calculate_spot_rate(101.5, 98.98, freq = 2, 1)

#Calculating 1 year spot-rate (Expressed as APR with semiannual compunding)
r.1yr <- calculate_spot_rate(102, 98.59 - (2/(1+r.6month/2)), freq = 2, 2)

# save down your final answers
a = c(r.6month, r.1yr) #in decimal form

# add answers to list for "Q2"
mysoln[["Q2"]] = list(a=a)

# 3
# To determine the possibility of arbitrage, we calculate spot rates for bonds A and B and calculate bond price of C based on spot rates of A & B

r_1yr <- calculate_spot_rate(100, 98.238, 1, 1)

r_2yr <- calculate_spot_rate(102, 99.438 - (2/(1+r_1yr)), freq = 1, 2)

# Calculating bond price of C based on above spot rates
spotrate <- c(r_1yr, r_2yr)

bond_price_C <- calculate_bond_price(2, -100, 0.03, 1, spotrate)


# Clearly Bond C is underpriced as calculated value of bond C is 101.37 while current trading price is 100.37
# Hence, the strategy to make profit out of arbitrage is to sell bonds A and B and buy bonds of C