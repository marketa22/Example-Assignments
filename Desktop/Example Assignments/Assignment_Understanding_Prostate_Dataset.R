#Assignment 7 Part 1   Marketa Hlavon

# Use the Prostate cancer dataset, load the data:
library(lasso2)
data(Prostate)

#Exploratory data analysis, first glimpse of data/trying to understand the data better.

#Use the pairs, the dim, the summary and the cor function to learn about your data.
#How many data points are there in total? What do you notice about the svi variable?
pairs_P = pairs(Prostate) #learning about data
dim_P = dim(Prostate)
summary_Prostate = summary(Prostate)
cor1 = cor(Prostate)

print(paste0(dim_P[1], " by ", dim_P[2], " data points, in total ", dim_P[1]*dim_P[2], " data points.")) #results from dim
print("SVI (seminal vesicle invasion) is always 0 or 1, because it is a discreate variable meaning that it is either present, or it is not.")
 
#What is the maximum correlation between any of the variables? Between which two variables does it occur?
#Why do you think that is?
a = 1
b = 1
c = 1
d = 1
max = 0
names = c("lcavol","lweight","age","lbph","svi","lcp","gleason","pgg45")

while((a<=8) && (b<=8)){
 if((cor1[a,b]>max) && cor1[a,b]!=1){
   max = cor1[a,b]
   c = a
   d = b
 }
  if(a==8){
    a=0
    b = b+1
  }
  a = a+1
}

print(paste0("The maximum correlation is: ", round(max, digits = 3)))
print(paste0("This is between: ", names[c], " and ", names[d]))
print("Their correlation is high due to them both providing information about the gleason score gleason is simply the the Gleason score, while pgg45 is the percentage Gleason scores 4 or 5.")

#Some of our variables have been transformed. Which transformation was applied?
#For lpsa, check whether the raw values or the transformed values are closer to a normal distribution.
print("The log was taken of cancer volume (lcavol), prostate weight (lweight), benign prostatic hyperplasia amount (lbph), capsular penetration (lcp) and prostate specific antigen (lpsa) data.")

print("QQ plot of transformed lpsa data")
qqnorm(Prostate$lpsa, ylab = "log prostate specific antigen levels")
print("QQ plot of raw lpsa data")
qqnorm(exp((Prostate$lpsa)), ylab = "Raw Prostate Specific Antigen Levels")
print("Based on the QQ plot results the log (transformed) prostate specific antigen levels have a closer to normal distribution")

#Our goal is to see whether we can predict the amount of PSA in the blood using our predictor variables.
#Create a scatterplot using only the lcavol and the lpsa variable. What is their correlation? Make a second plot using the raw values of both variables.
#Compare the correlation between raw values and transformed values, compare the plots between raw values and transformed values.
#Which do you think is better for prediction?
plot(Prostate$lcavol, Prostate$lpsa, xlab = "log cancer volume", ylab = "log psa") #Create a scatterplot using only the lcavol and the lpsa variable
print(paste0("The correlation of lcavol and lpsa is: ", round(cor(Prostate$lcavol, Prostate$lpsa), digits = 3)))

plot((exp(Prostate$lcavol)), (exp(Prostate$lpsa)), xlab = "cancer volume", ylab = "psa") #plot using the raw values
print(paste0("Compared to the correlation of the raw data for cancer volume and psa of: ", round(cor((exp(Prostate$lcavol)), (exp(Prostate$lpsa))), digits = 3)))

print(c("The corroloation between lcavol and lpsa in the scatterplot is linear, this corrolation is not seen using their raw values.",
"The corrleation values of the transformed data was much higher than that of the raw data.",
"Based on the difference in correlation values and the plots the transformed values show a much greater correlation, and would be better for prediction."))

#Create a linear model using lm, predicting lpsa from lweight. Read out from the summary report the probability that B1 = 0.
#The two variables have a correlation of 0.35.
#To confirm that the p-values are accurate, perform a simulation where you compute the correlation on a randomized dataset and count how often you achieve a correlation as extreme as 0.35.
#Report your empirical p-value, how close is it to the p-value reported by lm?

lm.lpsa = lm(lpsa~lweight, Prostate)
sum_lpsa = summary(lm.lpsa)
p_value = sum_lpsa$coefficients[2,4]
print(paste0("The probability that B1=0 (from summary report) is: ", round(p_value, digits = 6)))
corr = 0.35
run_sample = 100000
vector_lpsa = Prostate$lpsa
vector_lweight = Prostate$lweight
vector_mix_lpsa = c()
cor_vec = c()

i=1
while(i<=run_sample){
  vector_mix_lpsa = sample(vector_lpsa, length(vector_lpsa), replace = FALSE)
  corl = cor(vector_mix_lpsa, vector_lweight)
  cor_vec = append(cor_vec, corl)
  i = i + 1
}

numb_Odds = 0
i = 1
while(i<=length(cor_vec)){
  if(cor_vec[i]>corr){
    numb_Odds=numb_Odds+1
  }else if(cor_vec[i]<(-corr)){
    numb_Odds=numb_Odds+1
  }
  i= i+1
}

impirical_value = numb_Odds/run_sample
print(paste0("The number of times a correlation of >0.35 or <-0.35 is: ", numb_Odds))
print(paste0("My impirical p-value would be: ", numb_Odds/run_sample))
print(paste0("The original p-value was: ", round(sum_lpsa$coefficients[2,4], digits = 6)))
print(paste0("These are ", round(100*(abs(impirical_value-p_value)/p_value), digits = 1), " percent different."))

#Create a linear model predicting lpsa from lcavol.
#Create a scatterplot from the two variables and add the prediction as a line.
#How good is the linear model? Print out the R2 value and the p-value of the lcavol predictor variable.
lm.lpsa = lm(lpsa~lcavol, Prostate) #linear model
plot(lpsa~lcavol, Prostate) #scatterplot
lines(Prostate$lcavol, predict(lm.lpsa), col = "red") #add prediction line
sum2 = summary(lm.lpsa)
p_value = 2* pt(abs(sum2$coefficients[2,3]), 95, lower.tail = FALSE) #2 makes it two tailed
print(paste0("R-squared value is: ", round(sum2$r.squared, digits = 3)))
print(paste0("Adjusted lcavol p-value: ", signif(p_value, digits = 4)))

#Look at an alternative method to find B0 and B1 from our univariate regression between lpsa and lcavol.
#Use a a brute force grid search approach: trying out every possible value of B0 and B1 and recording the parameter values at the minimum of the objective function.

#Write a program that loops over a range of values for parameters of B0 and B1,
#computes the RSS for the set of parameters and stores the best RSS and the associated set of parameters.

y = c(Prostate$lpsa)
x = c(Prostate$lcavol)
v3 = c()
v4 = c()
v5 = c()
min = 1000000 # set arbitrarilty high for comparison
B0 = NULL # start as null
B1 = NULL
for (beta_0 in seq(0, 3, 0.1)){
  for (beta_1 in seq(0, 2, 0.1)){
      v3 = beta_0 + beta_1*x
      v4 = y - v3
      v5 = v4^2
      total = sum(v5)
      if(total<min){
        min = total
        B0 = beta_0
        B1 = beta_1
      }
  }
}

print(paste0("Minimum RSS is: ", round(min, digits = 3)))
print(paste0("Value of B0: ", B0))
print(paste0("Value of B1: ", B1))

#How much better will your solution become if you use a step size of 0.01 instead of 0.1?
#Record the minimal RSS value and the optimal values for B that you find.

min2 = 1000000 #set aribitrarily high for comparison
B0 = NULL
B1 = NULL
for (beta_0 in seq(0, 3, 0.01)){
  for (beta_1 in seq(0, 2, 0.01)){
      v3 = beta_0 + beta_1*x
      v4 = y - v3
      v5 = v4^2
      total = sum(v5)
      if(total<min2){
        min2 = total
        B0 = beta_0
        B1 = beta_1
      }
  }
}

print(paste0("Minimum RSS is: ", min2))
print(paste0("The difference is: ", min-min2))
print(paste0("Value of B0: ", B0))
print(paste0("Value of B1: ", B1))

#The loop above takes M2 iterations to complete where M is the number of steps.
#If we wanted to estimate B0, B1 and B2 for a 3dimensional problem, how many steps would it take?
#How many steps would it take for p parameters?
print("For a 3-dimentional proplem it would take M^3 steps, or M^p steps for p parameters")

#Use matrix algebra to re-compute what lm has been computing for us.
#Ensure we understand the inner workings of lm.

#First we will set up our matrices X and vectors ~x and ~y:
x = Prostate$lcavol
y = Prostate$lpsa
X = cbind(rep(1, length(Prostate$lpsa) ), x)
N = nrow(Prostate)

#Compute b-hat
B_hat = solve(t(X) %*% X, t(X) %*% y)
print(paste0("Beta hat: ", round(B_hat, digits = 3)))

#Compute zj for the intercept and the slope, using this formula for the variance estimator:
#o^2 = (1/N-2) from i = 1 to N (yi-B0-xi x B1)^2
#This is the unbiased estimator with the term in the sum representing the squared RSS.
#In R you can get access to the residuals themselves with residuals(lm.model) assuming that the lm.model variable contains your model.

resid_data = residuals(lm.lpsa) #get the residuals
sigma2 = (sum(resid_data^2))/95 #compute sigma
bottom = ((sigma2*(solve(t(X) %*% X)))^(1/2)) #compute the bottom of the equation (not included)
zj1 = B_hat[1]/bottom[1,1] #compute both zjs by dividing the appropriate Beta hat by the bottom
zj2 = B_hat[2]/bottom[2,2]
print(paste0("The zj for the intercept is: ", round(zj1, digits = 3)))
print(paste0("The zj for the slope is: ", round(zj2, digits = 3)))

#Use the zj to compute p-values under the t distribution with 2 degrees of freedom.
p_value2 = 2* pt(abs(zj1), 95, lower.tail = FALSE)
p_value3 = 2* pt(abs(zj2), 95, lower.tail = FALSE)
print(paste0("From the z value, the p-value for the intercept is: ", signif(p_value2, digits = 3)))
print(paste0("From the z value, The p-value for the slope is: ", signif(p_value3, digits = 3)))

#Look at the full model.
#Create a linear regression model that uses all 8 variables to predict lpsa and analyze the result:
lm.lpsa2 = lm(lpsa ~ . , Prostate)

#Create the diagnostic plots for the models, is there any cause for concern?
plots = plot(lm.lpsa2)
print(c("Looking at the diagostic plots I see no cause for concern:",
"the residuals vs fitted plot show linear relationship,",
"the QQ plot shows that the residuals are  for the most part normally distributed,",
"the scale-location plot shows that the residuals are spread equally along the range of predictors,",
"and the residuals vs leverage plot shows no outliers."))

#For how many & which variables in the full model can we reject the null hypothesis Bj = 0?
summary_dist = summary(lm.lpsa2)
count = 0
i = 1
while(i<=9){
  if((summary_dist$coefficients[i,4])<0.05){
    count = count +1
    print(paste0("We can reject the null hypothesis for: ", names[i-1]))
    print(paste0("With a p value: ", signif(summary_dist$coefficients[i,4], digits = 3)))
  }
  i = i+1
}
print(paste0("In total this makes the number of variables: ", count))

#Is the full multivariate model with all 8 variables statistically significantly better compared to the original univariate model?
#Use an appropriate statistical test to compare the two models.
#Based on the test, would you recommend using the multivariate model or the univariate model?
print(paste0("The univariate model has a R-squared value of: ", round(sum2$r.squared, digits = 3), " compared to the R-squared value in the multivariate model: ", round(summary_dist$r.squared, digits = 3), "."))
print("Since the multivariate model has a higher R-squared value this signifies that the multivariate model can explain the variation in the model better.")
print("The ANOVA test can be used to look at whether this increase in explanation is statistically significant.")

anova_test = anova(lm.lpsa,lm.lpsa2)
print("The results of the ANOVA test to look at variation:")
print(anova_test)

print(c("The p-value for the ANOVA test is <0.05, and is therefore taken as significant",
"Meaning that the multivariate model can significantly better explain the model",
"As such, I would recommend using the multivariate model",
"However, based on the results when looking at the summary of the multivariate model",
"I would only keep the variables that are statistically significant (will be done later on)"))

#Find an optimal model using regularization. The previous question probably has too many variables.
#Use the function glmnet from the glmnetUtils library. a = 0 will corresponds to ridge regression and a = 1 corresponds to lasso while any value in between is a mixture of the two models.
#Compare its coeficients using the coef function in R.

# How do the coefficients compare to the full model computed before? What are the differences?

library(glmnet)
lam = 0
a = 0
coefficients1 = coef(glmnet(data.matrix(Prostate[1:8]),data.matrix(Prostate[9]), alpha = a, lambda = lam))
estimates = summary_dist$coefficients[,1] 
print("The coefficients computed before: ", estimates)
print(estimates)
print("The coefficients with lam = 0 and a = 0: ")
print(coefficients1)
print("Two of the coefficients increase slightly (lweight and gleason), while the others decreased by a little bit")
print("The differences are: ")
print(coefficients1-estimates)
print(c("As you can see, the differences are very small, and should be considered no change.",
"No change would be expected when lambda = 0, as it signifies there no weight was assigned to reducing the penalty."))

#How do the coefficients compare to the full model computed before? What are the differences?
lam = 0.8
a = 0
coefficients2 = coef(glmnet(data.matrix(Prostate[1:8]),data.matrix(Prostate[9]), alpha = a, lambda = lam))
print("All the changes seen were moving estimates closer to zero (positive estimates decreasing, negative ones increasing)")
print("The differences were: ")
print(coefficients2 - estimates)
print("The ridge regression led to shrinkage of the parameters, the ones with the largest values lcavol, lweight, and svi saw the greates amount of shrinkage.")

#Explain what effect the lasso had on the variables, which variables did it select?
#Would you have selected the same variables based on the linear model?
lam = 0.2
a = 1
coefficients3 = coef(glmnet(data.matrix(Prostate[1:8]),data.matrix(Prostate[9]), alpha = a, lambda = lam))

print("Lasso allowed the 'influence' of variables that were less significant to go to zero (become zero).")
print("It selected lcavol, lweight and svi, all of which were selected using the linear model (significant p values).")
print("As such, yes, I would have selected the same variables using the lasso and the linear model.")

#Create a new linear model with lm which only contains the variables selected by the lasso and compare it to the full multivariate model.
#Use an appropriate statistical test to compare the two models. Based on the test, would you recommend using multivariate model or model selected by the lasso?
lm.lpsa3 = lm(lpsa ~ lcavol+lweight+svi , Prostate)
print("The results of the ANOVA test comparing the three variant model to the multivariate model: ")
print(anova(lm.lpsa3, lm.lpsa2))
print(c("The p-value is >0.05: the diffrence between the two models in not statistically significant meaning that the multivariate model does not significantly describe the data better.",
"The RSS of the multivariate model is lower, however since the the p-value was not significant we favour the simpler model- I would recomment using the model selected by the lasso."))

#Is there a difference between your own model created using lm and the one selected by the lasso?
print("The differences in the models are the coefficients assigned to the three variables")
print("The differences are: ")
print(summary(lm.lpsa3)$coefficients[,1])
print(coefficients3[c(1,2,3,6),1])
print("As you can see there were some significant changes to the coefficients selected by lasso, lasso causes a decrease in all the coefficients.")
  
#Calculating the melting temperature of DNA is a fundamental practical problem for sequencing analysis.
#We have provided the dataset online so you can read it directly from the file raw_data_accuracy_benchmark.fix.txt into an R dataframe.

#We suspect that the melting temperature is mainly affected by the GC content. Therefore, create
#a univariate model where you try to predict the dependent variable Tm_exp as a function of GC
#content (X.GC). How good is your model? Is GC content predictive of melting temperature?
fileContents = read.table("raw_data_accuracy_benchmark.fix.txt", header=T)
lm.TM = lm(fileContents$Tm_exp~fileContents$X.CG)
TM_GC = summary(lm.TM)
print("When you look at whether GC content is predictive of melting temperature through creating a model, the summary provides a R-squared of 0.6122, meaning that about 61% of the variance is predicted to be based on the GC content.")
print("This has a significant p-value of 2.2e-16. Both the B values were also significant.")

#Now create a multivariate model with four variables, namely GC content, length, salt concentration and oligo concentration.
lm.four = lm(fileContents$Tm_exp~fileContents$X.CG+fileContents$Length+fileContents$X.salt.+fileContents$X.oligo.)

#Create the diagnostic plots
print(plot(lm.four))

#How much better than the univariate model is the new model in explaining the data?
univ = summary(lm.four)
print("The new model is significantly better at explaining the data this can be seen when looking at the R-squared values")
print(paste0("The univariate model has a R-squared value of: ", round(TM_GC$r.squared, digits = 3), "."))
print(paste0("While the new model has a R-squared value of: ", round(univ$r.squared, digits = 3), "."))
print(paste0("This means that the new model can explain the variance by an increase of: ", round(((univ$r.squared-TM_GC$r.squared)*100), digits = 2), " percent."))

#Perform an appropriate statistical test to determine if the multivariate model is better than the singlevariate model.
#What is the result of the test?
anova_2 = anova(lm.TM, lm.four)
print("The anova results of the comparing the two models:")
print(anova_2)
print("As you can see the multivariate model has a lower RSS, and is significantly explains the data better which is seen in the very small p-value, which is <0.05.")

#Improve the model using a technique called feature engineering. Add the logarithm of the oligo length to the model.
#In R, you can simply add an additional column to the data which contains the new value.
#Does the fit improve perform a statistical test? Would you recommend extending to model with this new feature?
oligo_log = log10(fileContents$Length)
lm.oligo = lm(fileContents$Tm_exp~fileContents$X.CG+fileContents$Length+fileContents$X.salt.+fileContents$X.oligo.+oligo_log)
sum_oligo= summary(lm.oligo)  
anova_3 = anova(lm.four, lm.oligo)
print("The ANOVA results from adding the logarithm of the oligo length to the model")
print(anova_3)
print("As you can see, the new model with the logarithm of the oligo length is significantly (p<0.05) than the model without it, as such I would recommend adding it to the model.")
print("Other than the p-value, you can see that the model is better with this added value.")
print(paste0("By the lower RSS value, and the slight increase in R-squared in the summary: ", round(sum_oligo$r.squared, digits = 3), " from ", round(univ$r.squared, digits = 3), "."))
print("I would recommend adding it to the model since the result of the ANOVA model showed that the model is significantly better with it.")
  