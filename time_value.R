# mysoln is a list to store your answers


mysoln <- list()

#Throughout this code, cash inflows are assumed to be positive and cash outflows to be negative

#Using FinCal package for computation
install.packages("FinCal")
library(FinCal)

# 1
#Computing Future Value for (a)
fv_a <- fv(0.12, 2, -100000, 0, 0)

#Computing Future Value for (b)
ear_b <- ear(0.12, 4)

fv_b <- fv(ear_b, 2, -100000, 0, 0)

#Computing Future Value for (c)

ear_c <- ear(0.12, 12)

fv_c <- fv(ear_c, 2, -100000, 0, 0)

# answers
a <- fv_a
b <- fv_b
c <- fv_c

# add to list (start at the second element - the first element is a vectopr of names)
mysoln[["Q1"]] <- c(a, b, c)

# 2

# Computing Charlotte's net worth based on Bunny's proposition - part (a)
  
eff_rate <- ear(0.12, 2) # Computing effective rate for two years taking annual discount rate as 6%
pv_a <- pv.annuity(eff_rate,10,100000,0)

# Computing annuity for Charlotte's net worth to be $1.5 million - part (b)

pmt <- pmt(eff_rate, 10, -1500000, 0, 0)

# answers
a <- pv_a
b <- pmt
mysoln[["Q2"]] <- c(a, b)

# 3

# Computing future value of the savings made for 35 years

earnings <- 250000 
yearly_savings <- 250000 * 0.25

fv_savings <- fv(0.02, 35, 0, -yearly_savings, 0)

# Computing the amount that can be consumed in each retirement year

yearly_consumption <- pmt(0.02, 25, fv_savings, 0, 0)
# answers
a <- fv_savings
b <- yearly_consumption
mysoln[["Q3"]] <- c(a, b)

# 4

# Computing EAR for 3% monthly APR

ear_mortgage <- ear(0.03, 12)

#Computing monthly payment for the mortgage

interest_per_month <- 0.03/12

monthly_payment <- pmt(interest_per_month, 30*12, 400000, 0, 0)

#Computing the balance payment immediately after 20th monthly payment

pv_20_month <- pv(interest_per_month,20,0,monthly_payment,0)
mortgage_amount <- 400000
pv_balance <- mortgage_amount - pv_20_month

balance_20_month <- fv(interest_per_month, 20, pv_balance,0, 0) #Future value of pv_balance after 20 months
# answers
a <- round(ear_mortgage, digits = 4)
b <- round(monthly_payment, digits = 0)
c <- round(balance_20_month, digits = 0)

mysoln[["Q4"]] <- c(a, b, c)

# 5

# Forming a project with cashflows to get no IRR - all cash flows in million $

cf_1 <- c(4.5, 4, 4, 4, 4)

irr(cf_1) 
 
# Forming a project with cashflows to get multiple IRRs - all cash flows in million $

cf_2 <- c(1, -4.5, -4.5, -4.5, -4.5, 10)

irr(cf_2)
irr2(cf_2) #Multiple IRRs exist!

# answers: change the numbers or extend the vector as needed
a <- cf_1
b <- cf_2
mysoln[["Q5"]] <- list(a = a, b = b)

#return my solution

mysoln

