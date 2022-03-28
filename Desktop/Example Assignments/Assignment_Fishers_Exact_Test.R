#Assignment 6 Part 1   Marketa Hlavon
#Write our own program to perform a fisher's exact test.
#Assume that we have a sample of teenagers (male/female) that are studying or not studying
  
#           Men   	  Women  	  Row-total
#Studying	  3	          10	      13
#Not-stud  	11	        3	        14
#Col total	14	        13	      27

# Calculate the odds ratio of the table above.
SM = 3
SW = 10
NM = 11
NW = 3

OR2orig = ((SW/SM)/(NW/NM))

print(paste0("Odds ratio before randomization by hand ", round(OR2orig, digits = 3)))

#Construct a contingency matrix in R, perform the fisher test using the built-in function
Table_Frame = as.table(rbind(c(SW, SM), c(NW, NM)))
dimnames(Table_Frame) = list(Studying = c("Studying", "Not-Studying"), Gender = c("Women", "Men"))
p_value = fisher.test(Table_Frame)$p.value
OR_fish = fisher.test(Table_Frame)$estimate
print(paste0("Fisher's test p value: ", round(p_value, digits = 3)))
print(paste0("Fisher's test Odds ratio: ", round(OR_fish, digits = 3)))

#What we will do is construct a "total population" of Men and Women, put all 27 people in that above matrix into one population. Label the studying ones "1" and non-studying ones "0".
#For now, we will forget about their gender, so we have 27 people labeled as 1/0.
total_population = c(rep.int(1,13),rep.int(0,14)) #13 studying, 14 nonstudying

#We can now randomly pick an appropriate number of people from that population that we will label as "Men". Do the same for "Women". 
num_M = 14
num_W = 13

male = sample(total_population, num_M, replace = FALSE)
n = sum(male)
n2 = 13-n
female = c(rep(1,n2), rep(0,num_W-n2))

#Now, we can check how many "Men"/"Women" of our random sample are studying and how many aren't and can calculate an Odds ratio in our randomized sample.
sig3 = female[]==1
sig4 = female[]==0

SM = n
NM = n2
SW = length(female[sig3])
NW = length(female[sig4])

OR2 = ((SW/SM)/(NW/NM))

print(paste0("Odds ratio after first randomization ", OR2))

# Do this for 10000 iterations. Plot a histogram of the log of the odds ratios. What shape is it?
odds_ratio = c()

i = 1
while (i<=10000){
  total_population = c(rep.int(1,13),rep.int(0,14)) #13 studying, 14 nonstudying
  male = sample(total_population, num_M, replace = FALSE)
  n = sum(male)
  n2 = 13-n
  female = c(rep(1,n2), rep(0,num_W-n2))
  sig3 = female[]==1
  sig4 = female[]==0
  
  SM = n
  NM = n2
  SW = length(female[sig3])
  NW = length(female[sig4])
  
  OR2 = ((SW/SM)/(NW/NM))
  odds_ratio = append(odds_ratio, OR2)
  
  i= i+1
}

hist(log10(odds_ratio))
print("The shape of the histogram of the log of the odds ratios is normally distributed (bell-shaped)")

#To find whether the association between men and women is significant, check what fraction of cases the "randomized" Odds ratio exceeds the actual Odds ratio calculated in 1).
#This fraction should be within a factor of 3 or so of the p-value given by the built-in R fisher.test.

numb_Odds = 0
i = 1
while(i<=length(odds_ratio)){
  if(odds_ratio[i]>OR_fish){
    numb_Odds=numb_Odds+1
  }
  i= i+1
}
print(paste0("The number of randomized Odds ratio exceeding the actual Odds ratio calculated: ", numb_Odds))

fraction = numb_Odds/10000
print(paste0("Fraction: ", fraction))
print(paste0("Compared to the p_value: ", round(p_value, digits = 3)))

if(p_value<(fraction*3)|p_value>(fraction/3)){
  print("The fraction is within a factor of 3 of the p-value given by the R fisher-test")
}


  