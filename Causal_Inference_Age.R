# Clear the working space
rm(list = ls())

#Set working directory

setwd('/Users/lakshmesrada42/Downloads/')  

#getwd()

### Load the packages (all must have been installed)

library(tidyverse)
library(doBy)
library(foreign)
library(knitr)
library(lmtest)
library(readstata13)
library(sandwich)
library(stargazer)
library(AER)
library(gdata)
library(wooldridge)
library(openintro)

cse=function(reg){
  rob=sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)}
  
  
  
### Data section: loading data
  
data("Medicaid1986")
medi = as.data.frame(Medicaid1986)
  
#structure of dataset
str(Medicaid1986)
  
#checking for null values
colSums(is.na(medi)) 
  
#encoding dummy variables
medi$female = ifelse(medi$gender == 'female', 1, 0)
medi$old = ifelse(medi$age >= 55, 1, 0)
medi$ssi = ifelse(medi$program == 'ssi', 1,0)
  

# descriptive statistics of variables of interest
stargazer(medi[c("visits", "age", "health1", "access", "income")], type="text", digits=2, summary.stat=c("n", "mean", "median", "sd","min","max"), title="Descriptive Statistics", flip=FALSE, covariate.labels=c("visits", "age", "health1", "access", "income"))
  
stargazer(medi[c("children", "female","ssi")], type="text", digits=2, summary.stat=c("n", "mean", "median", "sd","min","max"), title="Descriptive Statistics", flip=FALSE, covariate.labels=c("children", "gender", "program type"))
  
  
## Graphs
  
# histograms to check distribution of age and visits
ggplot(medi) + geom_histogram(aes(x=age), col="darkturquoise",binwidth = 2) + 
    labs(title = "age", x="age" )
  
ggplot(medi) + geom_histogram(aes(x=visits), col="darkturquoise",binwidth = 2) + 
    labs(title = "visits", x="visits" )
  
  
# Scatterplot of number of hospital visits vs. age 
ggplot(medi, aes(x=age, y=visits)) + geom_point(col="deeppink1") + 
    labs(title = "Visits vs Age", x = "age", y = "visits") +
    stat_smooth(method=lm, col = "darkturquoise", se=FALSE)
  
  
#creating subset without outlier
medi_small = subset(medi, visits < 40)
  
medi_small$age_scaled <- scale(medi_small$age)
  
  
## Regression Table 1
  
#r0 -> regression of visits on age
r0=lm(visits ~ age, data = medi)
  
#r1 -> regression of visits on binary variable 'old'
r1=lm(visits ~ old, data = medi)
  
#r1 -> regression of visits on old + health1
r2=lm(visits ~ old+health1, data = medi)
  
#r3 -> regression of visits on old + health1 + children
r3=lm(visits ~ old+health1+children, data = medi)
  
#r4 -> regression of visits on old + health1 + children + program
r4=lm(visits ~ old+health1+children+program, data = medi)
  
#r5 -> regression without outliers in visits
r5 =lm(visits ~ old+health1+children+program, data = medi_small)
  
#r6 -> regression of visits on old + health1 + children + program + quadratic term of age
r6=lm(visits ~ age+health1+children+program+I(age_scaled^2), data = medi_small)
  
#r7 -> regression of visits on binary variable old + health1 + children + program + age^2 + cubic term of age
r7=lm(visits ~ age+health1+children+program+I(age_scaled^2)+I(age_scaled^3), data = medi_small)
  

#Display results   
stargazer(r0,r1,r2,r3,r4,r5,r6,r7, se=list(cse(r0),cse(r1), cse(r2), cse(r3), cse(r4), cse(r5), cse(r6),cse(r7)),
            title="variables on visits", type="text",
            star.cutoffs = NA, df=FALSE, digits=3)
  
  
## Regression Table 2

r7=lm(visits ~ old+health1+children+program+I(children^2), data = medi_small)
r8=lm(visits ~ old+health1+children+program+access, data = medi_small)
r9=lm(visits ~ old+health1+children+program+access+female, data = medi_small)
r10=lm(visits ~ old+health1+children+program+income, data = medi_small)
r11=lm(visits ~ old+health1+children+program+access+female+female*income, data = medi_small)
  
  stargazer(r7,r8,r9,r10,r11, se=list(cse(r7),cse(r8), cse(r9), cse(r10), cse(r11)),
            title="variables on visits", type="text",
            star.cutoffs = NA, df=FALSE, digits=3)
  
  
### Additional Exploration Regression Table
  
#maybe add age^2
  medi_young = subset(medi_small, age < 55)
  medi_old = subset(medi_small, age >= 55)
  
  
#Regressions - additional
r1=lm(visits ~ age+health1+children, data = medi_young)
  
r2=lm(visits ~ age+health1+children, data = medi_old)
r3=lm(visits ~ age+health1+children+program, data = medi_old)
  
stargazer(r1,r2,r3, se=list(cse(r1), cse(r2), cse(r3)),
            title="age subsets", type="text",
            star.cutoffs = NA, df=FALSE, digits=3)
  
  