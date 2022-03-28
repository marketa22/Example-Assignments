#Assignment 6 Part 1   Marketa Hlavon
#Consider 10,000 SNPs. For a population of 100 cases and 100 controls, generate a set of 10,000 SNPs, of which 10 "Disease SNPs" have an odds ratio of 1.5, and the rest are randomly distributed.
#OR: (cases-disease/cases-healthy) / (control-disease/control-healthy) = 1.5  
#Assume SNPs that only have two alleles (can call them "1" and "0").
#Randomly distribute SNPs you can just pick them randomly (for both cases and controls) from a large population of "1"s and "0"s.
#For the 10 Disease SNPs, generate a population that has an odds ratio of 1.5 (i.e., you have "1"s more often than "0"s), and pick the cases from them, whereas you pick the controls from the normal population.

createPop = function(pop){
  cases =  matrix(data = NA, nrow = pop, ncol = 10000)
  cntrls = matrix(data = NA, nrow = pop, ncol = 10000)
  
  test = c(0,1)
  i = 1
  while (i <= nrow(cntrls)){
    m1 = sample(test, 10000, replace = TRUE) #most ot the dataset is created randomly
    m2 = sample(test, 10000, replace = TRUE)
    cntrls[i,]=m1
    cases[i,]=m2
    i = i+1
  }
  
  #create 10 SNPs that have a OR of 1.5
  i = 1
  prob_pick <- ifelse(test == 0, 0.67, 1)
  while (i <= 10){
    m1 = sample(test, pop, replace = TRUE, prob = prob_pick)
    cases[,i]=m1
    i = i+1
  }
  
  #Perform fisher's exact
  i=1
  p_values = c() 
  for(col in 1:ncol(cases)){
    cases1= sum(cases[,i])
    cases0 = pop-cases1
    cntr1=sum(cntrls[,i])
    cntr0=pop-cntr1
    
    Table_Frame = as.table(rbind(c(cases0, cases1), c(cntr0, cntr1)))
    dimnames(Table_Frame) = list(DataC = c("Cases","Control"), SNP = c("0","1"))
    p_values = append(p_values, fisher.test(Table_Frame)$p.value)

    i = i+1
  }
  p_values = p.adjust(p_values, method="fdr")
  return(p_values)
}

#Perform fisher's exact tests on all of them and correct their p-values using FDR.
#Do any of your 10 genotypes that you know are associated with the genotype show significant p-values?

p_values = createPop(100)

numbSig = function(p_values){
  significant = 0
  j = 1
  while(j<=10){
    if(p_values[j]<=0.05){
      significant = significant +1
    }
    j= j+1
  }
  return(significant)
}

significant = numbSig(p_values)
print(paste0("The number of significant p-vales associated with the 10 genotypes is: ", significant))

#Repeat 10 times and estimate the power.
print("Repeating 10 times")
i = 1
sum = 0
while(i<=10){
  print(paste0("Try number: ", i))
  p_values = createPop(100)
  significant = numbSig(p_values)
  print(paste0("The number of significant p-vales associated with the 10 genotypes is: ", significant))
  sum = sum + significant
  i = i+1
}

cases =  matrix(data = NA, nrow = 100, ncol = 10000)
cntrls = matrix(data = NA, nrow = 100, ncol = 10000)

test = c(0,1)
i = 1
while (i <= nrow(cntrls)){
  m1 = sample(test, 10000, replace = TRUE) #most ot the dataset is created randomly
  m2 = sample(test, 10000, replace = TRUE)
  cntrls[i,]=m1
  cases[i,]=m2
  i = i+1
}

i = 1
prob_pick <- ifelse(test == 0, 0.67, 1)
while (i <= 10){
  m1 = sample(test, 100, replace = TRUE, prob = prob_pick)
  cases[,i]=m1
  i = i+1
}

library(pwr)
mean1 = mean(cases[,1:10])
mean2 = mean(cntrls[,1:10])
sd1 = sd(cases[,1:10])
sd2 = sd(cntrls[,1:10])
power = pwr.t.test(n = 100, d = (mean1-mean2)/(sd1+sd2), type="two.sample",alternative="two.sided")$power

print(paste0("An estimated power with 100 individuals using the pwr package is: ", round(power, digits = 3)))
print(paste0("An estimated power with 100 individuals based on the results is: ", sum/100))

# Increase the population 10-fold and repeat (just once, it will take a while to calculate).
# What is your power now?
p_values = createPop(1000)
significant = numbSig(p_values)
print(paste0("The number of significant p-vales associated with the 10 genotypes when the population 10-fold is: ", significant))

cases =  matrix(data = NA, nrow = 1000, ncol = 10000)
cntrls = matrix(data = NA, nrow = 1000, ncol = 10000)

i = 1
while (i <= nrow(cntrls)){
  m1 = sample(test, 10000, replace = TRUE) #most ot the dataset is created randomly
  m2 = sample(test, 10000, replace = TRUE)
  cntrls[i,]=m1
  cases[i,]=m2
  i = i+1
}

i = 1
prob_pick <- ifelse(test == 0, 0.67, 1)
while (i <= 10){
  m1 = sample(test, 1000, replace = TRUE, prob = prob_pick)
  cases[,i]=m1
  i = i+1
}

mean1 = mean(cases[,1:10])
mean2 = mean(cntrls[,1:10])
sd1 = sd(cases[,1:10])
sd2 = sd(cntrls[,1:10])
power = pwr.t.test(n = 1000, d = (mean1-mean2)/(sd1+sd2), type="two.sample",alternative="two.sided")$power

print(paste0("An estimated power with 1000 individuals using the pwr package is: ", round(power, digits = 3)))
print(paste0("An estimated power with 1000 individuals based on the result is: ", significant/10))

# What sample size is required to do a genome-wide study (100 times the number of tests)?
print("The sample size would need to be larger than 1000 in order to do a genome-wide study,")
print("both becuase you would want the power to be atleast 0.8, which was not seen above")
print("and since it will be around 100 times larger you would need a more significant p value.")

number = pwr.t.test(d = (mean1-mean2)/(sd1+sd2),sig.level = 0.0005, power = 0.8, type="two.sample",alternative="two.sided")$n
print("Using the pwr function, in order to have a power of 0.8 on a study that is 100 times the number of tests the estimated sample size required is:")
print(number)

  