#Assignment 8   Marketa Hlavon

#Load the data:
library(lasso2)
data(Prostate)

#Investigate the rationale for a separate testing and training dataset.
#Use a subset (30 datapoints) of the prostate dataset and split it into test and training data equally:
N = 30
sample.rows = sample(1:nrow(Prostate), N) 
Prostate_small = Prostate[sample.rows, ] # sample of 30
Prostate_train = Prostate_small[1:(N/2), ] # half of the sample
Prostate_test = Prostate_small[(N/2+1):N, ] # other half of the sample

#Fit a model with all 8 predictor variables to predict lpsa on the training data.
#Then use the model to predict on the training data and the testing data.
lm.all1 = lm(lpsa~., Prostate_train)
pred1 = predict.lm(lm.all1, Prostate_train)
pred2 = predict.lm(lm.all1, Prostate_test)
numbers1 = rownames(Prostate_train)
numbers2 = rownames(Prostate_test)
expected1 = Prostate[numbers1,9]
expected2 = Prostate[numbers2,9]
compare1 = mean((pred1-expected1)^2)
compare2 = mean((pred2-expected2)^2)

#What do you observe? On which dataset is the error larger, why do you think that is?
print(paste0("The prediction based on the training dataset had a much smaller error of: ", round(compare1, digits = 3), " than the testing data, which was an error of: ", round(compare2, digits = 3), "."))
print(c("This was expected as the training dataset was used to create the model used,",
"as well using all 8 predictor variables probably caused over fitting to the training model",
"which is why the error was significantly higher for the testing data than the training data"))

#Explore the relationship between sample size and generalization error. 
#Test all values of N between 20 and 97 using the above procedure. For each
#iteration draw a random sample of size N exactly 10 times, compute the testing
#and training error and then average them.
#Record and plot the ratio of test error to training error as a function of N.
N = 20
i = 1
testing_error = c()
training_error = c()
averages_test = c()
averages_train = c()
N_20to97 = c(20:97)

while(N<=97){
  while(i<=10){
    sample.rows = sample(1:nrow(Prostate), N) 
    Prostate_small = Prostate[sample.rows, ] # sample of 30
    Prostate_train = Prostate_small[1:(N/2), ] # half of the sample
    Prostate_test = Prostate_small[(N/2+1):N, ] # other half of the sample
    lm.all1 = lm(lpsa~., Prostate_train)
    pred1 = predict.lm(lm.all1, Prostate_train)
    pred2 = predict.lm(lm.all1, Prostate_test)
    numbers1 = rownames(Prostate_train)
    numbers2 = rownames(Prostate_test)
    expected1 = Prostate[numbers1,9]
    expected2 = Prostate[numbers2,9]
    compare1 = mean((pred1-expected1)^2)
    compare2 = mean((pred2-expected2)^2)
    training_error = append(training_error, compare1)
    testing_error = append(testing_error, compare2)
    i=i+1
  }
  averages_test = append(averages_test, mean(testing_error))
  averages_train = append(averages_train, mean(training_error))
  testing_error = c()
  training_error = c()
  i=1
  N = N+1
}

plot(N_20to97, (averages_test/averages_train), xlab = "Size of N", ylab = "Ratio of test error to training error", main = "Ratio of Test Error to Training Error as a Function of N")

#Use cross-validation to fit an optimal model to the data.
#Split the data into testing and training datasets using a 1:3 split. Randomly select 1/4 of the rows for the test data and use the rest for training.
N = round(nrow(Prostate)/4) # a quarter
sample.rows = sample(1:nrow(Prostate), 97)
first_quarter = sample.rows[1:N]
Prostate_test = Prostate[first_quarter, ] # test 1/4 of the applicants
sample.rows = sample.rows[(N+1):nrow(Prostate)]

#Next, split the remaining data (the 3/4) into a validation dataset and a training dataset.
#Use a 1:3 split, therefore randomly select 1/4 of the remaining columns from the training data
#for the validation data and use the rest for the training dataset. You should now have 24 datapoints
#in your testing data, 18 in your validation data and 55 in your training set.

N = round(length(sample.rows)/4)
first_quarter = sample.rows[1:N]
Prostate_validation = Prostate[first_quarter, ] # test 1/4 of the remainding applicants
remainder = sample.rows[(N+1):length(sample.rows)]# remainder of 55
Prostate_training = Prostate[remainder,]

#Fit 4 different models to the data which all aim to predict lpsa:
#i. Model 1: Use only the lcavol variable
#ii. Model 2: Use only the lcavol, the lweight and the svi variables
#iii. Model 3: Use all 8 eight available variables
#iv. Model 4: Replace the lcavol from Model 3 with a 4th degree polynomial in the lcavol variable (use the poly function)

lm.1 = lm(lpsa~lcavol, Prostate_training) #Model 1
lm.2 = lm(lpsa~lcavol+lweight+svi, Prostate_training) #Model 2
lm.3 = lm(lpsa~., Prostate_training) #Model 3
lm.4 = lm(lpsa~lweight+age+lbph+svi+lcp+gleason+pgg45+poly(lcavol,4), Prostate_training) #Model 4

#Fit these four models on the training data and then apply them to predict on both the training and the validation data.
#Compute the mean squared error for each model and data set (training and validation):
#MSE = 1/N from i=1 to N (e)^2 = 1/N from i=1 to N (y - y hat)^2

MSE = c()

MSEoutput = function(model_number, MSE){
  pred1 = predict.lm(model_number, Prostate_training)
  pred2 = predict.lm(model_number, Prostate_validation)
  numbers1 = rownames(Prostate_training)
  numbers2 = rownames(Prostate_validation)
  expected1 = Prostate[numbers1,9]
  expected2 = Prostate[numbers2,9]
  MSE1 = mean((pred1-expected1)^2)
  MSE2 = mean((pred2-expected2)^2)
  MSE = append(MSE, MSE1)
  MSE = append(MSE, MSE2)
  return(MSE)
}

MSE = MSEoutput(lm.1, MSE)#Model 1
MSE = MSEoutput(lm.2, MSE)#Model 2
MSE = MSEoutput(lm.3, MSE)#Model 3
MSE = MSEoutput(lm.4, MSE)#Model 4

#What do you notice when looking at the MSE? Is there a trend in the MSE as the model becomes more complex?
#Is it the same trend for both data sets (training and validation)?
print(c("The MSE for my validation usually creates a V shape, increasing more for more complex models, usually having the lowest point on Model 2.",
"On the otherhand the MSE decreases for my training data as the model becomes more complicated."))
#If so, can you explain the trend?
print(c("This occurs because as the model becomes more complex it is explaining the training set better however it starts to overfit for the training data, ",
"therfore when the validation data is tested on the overfitted model, it fits worse (higher MSE) than on a less overfitted model"))

#Based on your observations, which model would you recommend to use?
print("Looking for the lowest MSE when testing the validation data:") 

vector_val = c(MSE[2],MSE[4],MSE[6],MSE[8])
min = which.min(vector_val)
if(min == 1){
  print("Based on the way the data was distributed, Model 1 was recommended")
}else if(min == 2){
  print("Based on the way the data was distributed, Model 2 was recommended")
}else if(min == 3){
  print("Based on the way the data was distributed, Model 3 was recommended")
}else{
  print("Based on the way the data was distributed, Model 4 was recommended")
}

print("I will evaluate Model 2 on the testing dataset")

#Evaluate the model that you chose on your testing dataset - what MSE do you get?
pred2 = predict.lm(lm.2, Prostate_test)
numbers2 = rownames(Prostate_test)
expected2 = Prostate[numbers2,9]
MSE2 = mean((pred2-expected2)^2)

print(paste0("I get a MSE of: ", MSE2))

#Use the non-parametric bootstrap on the Prostate data to estimate the variance in our estimate and fit.

# First consider a model predicting lpsa from lcavol.
# First create a model using all N data points. Create a scatter plot from all N datapoints using the
# two variables. Then compute 5 bootstrap estimates of the regression line (draw N samples with replacement).
# Plot these 5 estimates in the same plot.
lm.prob2 = lm(lpsa~lcavol, Prostate) #Model 1
plot(Prostate$lcavol, Prostate$lpsa, xlab = "(log) Cancer Volume (lcavol)", ylab = "(log) Prostate Specific Antigen (lpsa)", main = "Scatterplot of (log) Prostate Specific Antigen vs (log) Cancer Volume \nwith 5 Bootstrap Estimates of the Regression Line", cex.main = 0.9)
one = Prostate[sample(row(Prostate), N, replace = T),]
two = Prostate[sample(row(Prostate), N, replace = T),]
three = Prostate[sample(row(Prostate), N, replace = T),]
four = Prostate[sample(row(Prostate), N, replace = T),]
five = Prostate[sample(row(Prostate), N, replace = T),]

lm.model_one = lm(lpsa~lcavol, one)
lm.model_two = lm(lpsa~lcavol, two)
lm.model_three = lm(lpsa~lcavol, three)
lm.model_four = lm(lpsa~lcavol, four)
lm.model_five = lm(lpsa~lcavol, five)

lines(one$lcavol, predict(lm.model_one), type="l", col="blue")
lines(two$lcavol, predict(lm.model_two), type="l", col="red")
lines(three$lcavol, predict(lm.model_three), type="l", col="green")
lines(four$lcavol, predict(lm.model_four), type="l", col="magenta")
lines(five$lcavol, predict(lm.model_five), type="l", col="yellow")

#Interested in the accuracy of the bhat0 and bhat1 estimates that we have computed in the model.
#Create 1000 bootstrap samples and estimate bhat0 and bhat1 in each.
bhat0 = c()
bhat1 = c()
Rsq = c()
N = 97
for(i in 1:1000){
  sample.beta = Prostate[sample(row(Prostate), N, replace = T),]
  lm.model_sample = lm(lpsa~lcavol, sample.beta)
  summary_beta = summary(lm.model_sample)
  bhat0 = append(bhat0, summary_beta$coefficients[1,1])
  bhat1 = append(bhat1, summary_beta$coefficients[2,1])
  Rsq = append(Rsq, summary_beta$r.squared)
}

# Estimate the 95% confidence interval for bhat0 and bhat1 from the bootstrap samples.
# Estimate the 95% conffidence interval for the R2.
bb = quantile(bhat0, probs = seq(0.025, 1))
bt = quantile(bhat0, probs = seq(0.975, 1))
bb2 = quantile(bhat1, probs = seq(0.025, 1))
bt2 = quantile(bhat1, probs = seq(0.975, 1))
r2b = quantile(Rsq, probs = seq(0.025, 1))
r2t = quantile(Rsq, probs = seq(0.975, 1))

print(paste0("The 95% confidense interval for Bhat 0 based on bootstrap is between ", round(bb, digits = 3), " and ", round(bt, digits = 3), "."))
print(paste0("The 95% confidense interval for Bhat 1 based on bootstrap is between ", round(bb2, digits = 3), " and ", round(bt2, digits = 3), "."))
print(paste0("The 95% confidense interval for R-squared based on bootstrap is between", round(r2b, digits = 3), " and ", round(r2t, digits = 3), "."))
              
sum_model1 = summary(lm.prob2)
est0 = sum_model1$coefficients[1,1]
est1 = sum_model1$coefficients[2,1]
sd0 = sum_model1$coefficients[1,2]
sd1 = sum_model1$coefficients[2,2]
top0_2 = est0 + (1.96*(sd0))
bot0_2 = est0 - (1.96*(sd0))
top1_2 = est1 + (1.96*(sd1))
bot1_2 = est1 - (1.96*(sd1))

print(paste0("The 95% confidense interval for Bhat 0 based on the summary is between ", round(bot0_2, digits = 3), " and ", round(top0_2, digits = 3), "."))
print(paste0("The 95% confidense interval for Bhat 1 based on the summary is between ", round(bot1_2, digits = 3), " and ", round(top1_2, digits = 3), "."))

# Compute analytical estimates for the 95% confidence interval using the estimates and standard deviations from the lm summary
# How do the two estimates compare?
print(c("The estimates for the 95% conffidence interval for Bhat 0 and Bhat 1 are very similar: ",
"they usually only differentiate from the second decimal spot onwards"))
print("Looking at the second decimal point, bootstrap tends to have a slightly wider 95% conffidence interval than summary")
print("Comparison side by side Bhat 0:")
cases0 =  matrix(data = c(bb, bt, bot0_2, top0_2), nrow = 2, ncol = 2, dimnames = list(c("From (2.5%)", "To (97.5%)"), c("Bootstrap", "Summary")))
print(cases0)
print("Comparison side by side Bhat 1:")
cases1 =  matrix(data = c(bb2, bt2, bot1_2, top1_2), nrow = 2, ncol = 2, dimnames = list(c("From (2.5%)", "To (97.5%)"), c("Bootstrap", "Summary")))
print(cases1)

#Now we would like to make an estimate of our prediction conffidence on new patients.
#Assume we have three patients, one with lcavol = -1, one with lcavol = 1 and one with lcavol = 3.
#Compute 1000 bootstrap estimates and predict lpsa for each 
patient1 = c()
patient2 = c()
patient3 = c()

lcavol.1 = data.frame(lcavol = c(-1))
lcavol.2 = data.frame(lcavol = c(1))
lcavol.3 = data.frame(lcavol = c(3))

for(i in 1:1000){
  sample.new = Prostate[sample(row(Prostate), N, replace = T),]
  lm.model_sample = lm(lpsa~lcavol, sample.new)
  p1 = predict(lm.model_sample, lcavol.1)
  p2 = predict(lm.model_sample, lcavol.2)
  p3 = predict(lm.model_sample, lcavol.3)
  patient1 = append(patient1, p1)
  patient2 = append(patient2, p2)
  patient3 = append(patient3, p3)
}

# Compute the mean and standard deviation for the estimates. 
print(paste0("The patient with an lcavol of -1 had a mean lpsa of: ", round(mean(patient1), digits = 3), " with a standard deviation of: ", round(sd(patient1), digits = 3), "."))
print(paste0("The patient with an lcavol of 1 had a mean lpsa of: ", round(mean(patient2), digits = 3), " with a standard deviation of: ", round(sd(patient2), digits = 3), "."))
print(paste0("The patient with an lcavol of 3 had a mean lpsa of:" , round(mean(patient3), digits = 3), " with a standard deviation of: ", round(sd(patient3), digits = 3), "."))

# How close are the means to the predictions from the model on the full data?
lm.full = lm(lpsa~lcavol, Prostate)
p1f = predict(lm.full, lcavol.1)
p2f = predict(lm.full, lcavol.2)
p3f = predict(lm.full, lcavol.3)
print("Comparison of Patien 1 , 2 and 3 results side by side:")
cases =  matrix(data = c(mean(patient1), mean(patient2), mean(patient3), p1f, p2f, p3f), nrow = 3, ncol = 2, dimnames = list(c("Patient 1", "Patient 2", "Patient 3"), c("Bootstrap", "Full data")))
print(cases)
print(c("The means of the bootstrap method were very similar to the predictions from the model using the full data." ,
"The number only varied starting from the second or third decimal."))

#Use all predictors to predict lpsa and estimate the mean and variance of the R2 of the full model.
lm.full = lm(lpsa~., Prostate)
Rsq = c()

for(i in 1:1000){
  sample.beta = Prostate[sample(row(Prostate), N, replace = T),]
  lm.model_sample = lm(lpsa~., sample.beta)
  summary_beta = summary(lm.model_sample)
  Rsq = append(Rsq, summary_beta$r.squared)
}

r2b = quantile(Rsq, probs = seq(0.025, 1))
r2t = quantile(Rsq, probs = seq(0.975, 1))

print(paste0("The 95% confidense interval for R-squared for lpsa based on all predictors is: ", round(r2b, digits = 3), " and ", round(r2t, digits = 3), "."))
print(paste0("Mean R-squared is: ", round(mean(Rsq), digits = 3), " with a variance of: ", round(var(Rsq), digits = 3), "."))

#Use exploratory data analysis to investigate data created from a subset of HapMap Phase II dataset consisting of 270 individuals.
#You can read in the data as follows:
pop = read.table("population.csv")
colnames(pop) <- c("identifier")
snps = read.table("snps.mat")

#Now perform a principal component analysis (PCA) on the data using the prcomp command (center, do not scale).
#How much of the variance is explained by the first two principal components (PC)? How many PC do you need to explain 50% of the variance?
pca.pop = prcomp(snps)
sum_pop = summary(pca.pop)
print("The variance explained by the first two PCAs is 19.4% of the variance")
print("I require 57 PCAs in order to explain 50% of the variance")

#Now investigate the data graphically.
library(devtools)
install_github("vqv/ggbiplot") #only required teh first time
library(ggbiplot)
#Plot the first two PC using ggbiplot with var.axes = F. Try to interpret the plot.
#Explain your reasoning.
print(ggbiplot(pca.pop, var.axes = F))
print("Based on this plot, from the first two PCAs, the data can be broken down into three distinct groups")
print("The graph of the data split on the first two PCAs had three groups with no overlap")
print("PCA1 and PCA2 could each alone divide these 3 groups into distinct goups")

print(ggbiplot(pca.pop, var.axes = F, groups = pop[,1]))

#Now plot additional PC, can you interpret PC 3 and 4? What about PC 5 and 6?
print(ggbiplot(pca.pop, var.axes = F, choices = c(3,4), groups = pop[,1]) )
print(ggbiplot(pca.pop, var.axes = F, choices = c(5,6), groups = pop[,1]) )
print("Both PCS 3 and 4, or PCS 5 and 6 do not split any of the populations into distinct groups.")

