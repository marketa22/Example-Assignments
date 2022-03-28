#Assignment 9   Marketa Hlavon

df = read.csv("raw_data_accuracy_benchmark.fix.txt", sep="\t") # Read in the melting temperature dataset from Assignment 7
#Split dataset into test and training. Split the data based on the sequence so that no sequences overlap between them:
log_line = log(df$Length)
df[,16] = log_line
colnames(df)[16] <- "log_length" # adding the log length

N = length(unique(df$Sequence))
test_seq = sample(unique(df$Sequence), N/5) #test data is 1/5 of all data
df_test = subset(df, Sequence %in% test_seq)
df_train = subset(df, !(Sequence %in% test_seq))

#Compute a linear model to predict the melting temperature Tm_exp from five predictor variables, GC content, length, salt concentration, oligo concentration and the logarithm of the length.
#Compute the MSE of the  linear model on the test and on the training dataset. Based on the linear model, which variables are important for the prediction?
lm.meltTm = lm(Tm_exp~X.CG+Length+X.salt.+X.oligo.+log(Length), df_train)
MSE1 = mean((predict.lm(lm.meltTm, df_train)-df_train$Tm_exp)^2)
MSE2 = mean((predict.lm(lm.meltTm, df_test)-df_test$Tm_exp)^2)
print(paste0("The MSE of the the training dataset: ", round(MSE1, digits = 3)))
print(paste0("The MSE of the the testing dataset: ", round(MSE2, digits = 3)))
print(summary(lm.meltTm)$coefficients)
print(c("All of the 5 variables were 'important' for the linear model (all had significant p-values)",
"The least significant of the 5 was Length, even though the logarith of the legth had high significance",
"Based on the p-values the most significant were GC content, followed by salt concentraion"))

#Investigate both the length and the logarithm of the length as predictors. Create a scatter plot of the two variables, what do you notice?
#Compute the MSE of the linear model on the test data for
#(a) a linear model that contains the length but not the logarithm of the length
#(b) a linear model that contains the logarithm of the length but not the length
#(c) a linear model that contains both
# Which performs best?
plot(df$Length, ylab = "Length", main = "Length vs Index number")
plot(log(df$Length), ylab = "Logarithm of Length", main = "log(Length) vs Index number")
print(c("As you can see, the two plots look similar, with the difference being the values on the y axis.",
"The logarithm of the length allow the values to be with '1', and therefor make the data seem less discrete.",
"Since the logarithm of the data fits better, this signifies that the large change in values for length",
"do not match the effect they are having on the melting temperature, making it a better variable in the linear model."))

print("When you plot them against each other:")
plot(df$Length,df$log_length, xlab = "Length", ylab = "log Length")
print("As you can see, this plots a straight line")

lm.meltTm1 = lm(Tm_exp~X.CG+Length+X.salt.+X.oligo., df_train) #a
lm.meltTm2 = lm(Tm_exp~X.CG+log_length+X.salt.+X.oligo., df_train) #b
lm.meltTm3 = lm(Tm_exp~X.CG+Length+X.salt.+X.oligo.+log_length, df_train) #c
MSE2a = mean((predict.lm(lm.meltTm1, df_test)-df_test$Tm_exp)^2)
MSE2b = mean((predict.lm(lm.meltTm2, df_test)-df_test$Tm_exp)^2)
MSE2c = mean((predict.lm(lm.meltTm3, df_test)-df_test$Tm_exp)^2)
print(paste0("The MSE with just the length (not logarith of length) of the the testing dataset: ", round(MSE2a, digits = 3)))
print(paste0("The MSE with just the logarith of length (not legth) of the the testing dataset :", round(MSE2b, digits = 3)))
print(paste0("The MSE with both: ", round(MSE2c, digits = 3)))
print("The one with both performs the best (as seen with the lowest MSE).")

#Use random forests for prediction, train a RF on the three models from the previous question.
library(randomForest)
rf.1=randomForest(Tm_exp~X.CG+Length+X.salt.+X.oligo., df_train)
rf.2=randomForest(Tm_exp~X.CG+X.salt.+X.oligo.+log_length, df_train)
rf.3=randomForest(Tm_exp~X.CG+Length+X.salt.+X.oligo.+log_length, df_train)
MSE3a = mean((predict(rf.1, df_test)-df_test$Tm_exp)^2)
MSE3b = mean((predict(rf.2, df_test)-df_test$Tm_exp)^2)
MSE3c = mean((predict(rf.3, df_test)-df_test$Tm_exp)^2)

#Do you see the same results? Which models work best?
print("The MSE with just the length (not logarith of length) of the the testing dataset:")
print(paste0("The MSE with just the length (not logarith of length) of the the testing dataset: ", round(MSE3a, digits = 3)))
print(paste0("The MSE with just the logarith of length (not legth) of the the testing dataset :", round(MSE3b, digits = 3)))
print(paste0("The MSE with both: ", round(MSE3c, digits = 3)))
print("The one with only log of the length performs the best (as seen with the lowest MSE most of the time).")

#Compare the variable importance plots of the two models with 4 variables. What do you notice?
varImpPlot(rf.1)
varImpPlot(rf.2)
print("When comparing the variable imprortance plots, for both models the most important variable is GC content followed by salt concentration, and the least contributing one in oligo concentraion.")
print("Length or log length is in third place in model 1 and 2)".)

#Compare the variable importance plots with the output of the linear model.
#Which variables are important for the linear model, which for the RF?
one = summary(lm.meltTm1)
two = summary(lm.meltTm2)
print(c("For models 1 and 2 the important (significant) variables are the same when looking at thelinear models as looking",
"at the variable importance plot- all of the variables are more significant than oligo concentration, which is the least important.",
"As mentioned under linear model and random forrest, GC content followed by salt concentration are the most important."))

#Use an artificial neural network to predict melting temperature.
library(nnet)

#Use the same number of neurons in the hidden layers as input variables.
#Train an ANN for the same three parameter sets as in the other questions.
nnet1 = nnet(Tm_exp~X.CG+Length+X.salt.+X.oligo.,df_train, size = 4, linout=TRUE)
nnet2 = nnet(Tm_exp~X.CG+log_length+X.salt.+X.oligo., df_train, size = 4, linout=TRUE)
nnet3 = nnet(Tm_exp~X.CG+log_length+X.salt.+X.oligo.+Length, df_train, size = 5, linout=TRUE)
MSE31a = mean((predict(nnet1, df_test)-df_test$Tm_exp)^2)
MSE31b = mean((predict(nnet2, df_test)-df_test$Tm_exp)^2)
MSE31c = mean((predict(nnet3, df_test)-df_test$Tm_exp)^2)
MSE3a = mean((predict(nnet1, df_train)-df_train$Tm_exp)^2)
MSE3b = mean((predict(nnet2, df_train)-df_train$Tm_exp)^2)
MSE3c = mean((predict(nnet3, df_train)-df_train$Tm_exp)^2)

print(paste0("The MSE with just the length (not logarith of length) of the the training and testing dataset: ", round(MSE3a, digits = 3), ", ", round(MSE31a, digits = 3), "."))
print(paste0("The MSE with just the logarith of length (not legth) of the the training and testing dataset: ", round(MSE3b, digits = 3), ", ", round(MSE31b, digits = 3), "."))
print(paste0("The MSE with both for training and testing: " , round(MSE3c, digits = 3), ", ", round(MSE31c, digits = 3), "."))

#Compute the MSE on test and the training data. What do you notice when you train the same three models? Do you see a difference? 
print("The training MSE tends to be slightly lower than the testing MSE. This trend is the same for all three models.")
print("No model seems better than the other as the values tend to differ a lot each time the neural networks are created (as seen below).")

#When you repeat the training, does the model performance vary?
nnet1 = nnet(Tm_exp~X.CG+Length+X.salt.+X.oligo.,df_train, size = 4, linout=TRUE)
nnet2 = nnet(Tm_exp~X.CG+log_length+X.salt.+X.oligo., df_train, size = 4, linout=TRUE)
nnet3 = nnet(Tm_exp~X.CG+log_length+X.salt.+X.oligo.+Length, df_train, size = 5, linout=TRUE)
MSE3a = mean((predict(nnet1, df_train)-df_train$Tm_exp)^2)
MSE3b = mean((predict(nnet2, df_train)-df_train$Tm_exp)^2)
MSE3c = mean((predict(nnet3, df_train)-df_train$Tm_exp)^2)

print("When you repeat the training:")
print(paste0("The MSE with just the length (not logarith of length) of the the training dataset: ", round(MSE3a, digits = 3)))
print(paste0("The MSE with just the logarith of length (not legth) of the the training dataset: ", round(MSE3b, digits = 3)))
print(paste0("The MSE with both for training: ", round(MSE3c, digits = 3)))

print("As you can see, the MSE training values can be very different.")

#ANN tend to be more difficult to train than other models. Use the parameters: size=25, decay=1, maxit=5000, linout=TRUE for new ANN
#Compute the MSE on the test and the training data.  What do you observe?
#Difference in the MSE, difference between the test and training data MSE - why do you think that is?
nnet1 = nnet(Tm_exp~X.CG+Length+X.salt.+X.oligo.,df_train, size=25, decay=1, maxit=5000, linout=TRUE)
nnet2 = nnet(Tm_exp~X.CG+log_length+X.salt.+X.oligo., df_train, size=25, decay=1, maxit=5000, linout=TRUE)
nnet3 = nnet(Tm_exp~X.CG+log_length+X.salt.+X.oligo.+Length, df_train, size=25, decay=1, maxit=5000, linout=TRUE)
MSE31a = mean((predict(nnet1, df_test)-df_test$Tm_exp)^2)
MSE31b = mean((predict(nnet2, df_test)-df_test$Tm_exp)^2)
MSE31c = mean((predict(nnet3, df_test)-df_test$Tm_exp)^2)
MSE3a = mean((predict(nnet1, df_train)-df_train$Tm_exp)^2)
MSE3b = mean((predict(nnet2, df_train)-df_train$Tm_exp)^2)
MSE3c = mean((predict(nnet3, df_train)-df_train$Tm_exp)^2)

print(paste0("The MSE with just the length (not logarith of length) of the the training and testing dataset: ", round(MSE3a, digits = 3), ", ", round(MSE31a, digits = 3)))
print(paste0("The MSE with just the logarith of length (not legth) of the the training and testing dataset: " , round(MSE3b, digits = 3), ", ", round(MSE31b, digits = 3)))
print(paste0("The MSE with both for training and testing: ", round(MSE3c, digits = 3), ", ", round(MSE31c, digits = 3)))

print(c("The testing MSE tends to be slightly higher than the training MSE but not by too much, which means that this model seems fit well.",
"The MSE values are now much lower all around than before adding in the new parametrs.",
"These trends are the same for all three models.",
"No model seems significantly better than the others as the MSE values tend to be similar"))

#Would you recommend ANN over RF for this problem? Explain your reasoning.
print("I would recommend ANN using the more advanced parameters as the MSE values for both training and testing were significantly lower than using RF (running each time these are significantly lower).")

#HPLC is used to separate complex mixture of analytes including peptides.
#Each compound has a compound-specific retention time. We will predict the retention time of short peptides cleaved by the enzyme trypsin.
df_test = read.csv("peptides.test.csv", header=T, stringsAsFactors=F)
df_train = read.csv("peptides.train.csv", header=T, stringsAsFactors=F)

#Dataset only has peptide sequences and retention time. We have to compute predictor variables first.
#We will first use the length of the sequence as a predictor.

#Compute the length of the peptide sequence, plot the length against the retention time on the training data.
vector_length = apply(as.matrix(df_train$peptide), 1, FUN = nchar)
plot(vector_length, df_train$rt, xlab = "Peptide length", ylab = "Retention Time", main = "Peptide Length against the Retention Time \non the Training Data")
df_train[,3] = vector_length
colnames(df_train)[3] <- "Length"

vector_length = apply(as.matrix(df_test$peptide), 1, FUN = nchar)
df_test[,3] = vector_length
colnames(df_test)[3] <- "Length"

#Use the length as a predictor variable in a linear model. Is the length a statistically signifficant predictor for retention time?
lm.2 = lm(df_train$rt~vector_length)
print("The length is a statistically significant predictor for retention time")
print("This can be seen in the summary of the linear model:")
print(summary(lm.2)$coefficients)

#Compute the MSE for a model that uses the mean of the retention time as a predictor and compare it to test data MSE of your linear model that uses the length. 
#What are the two MSE values? Which one is better?
print(paste0("Using the naive model the MSE value is: ", round((mean((mean(df_train$rt)-df_test$rt)^2)), digits = 3)))
print(paste0("Using the linear model the MSE value is: ", round((mean((predict(lm.2,df_test)-df_test$rt)^2)), digits = 3)))

#How good is your current prediction with the linear model? Explain your reasoning why you think the model is good/bad.
print("The linear model is not very good, since its MSE value is not much better than just using the mean value")

#Engineer a set of 20 features that contain the percentage of each amino acid in the sequence.
names_aa = c("A","C","D","E","F","G","H","I","K","L","M","N","P","Q","R","S","T","V","W","Y")
dataf2 = data.frame(matrix(0, ncol = length(names_aa), nrow = nrow(df_train)))
colnames(dataf2) = names_aa
i = 1
while(i<=nrow(df_train)){
  j = 1
  while(j<=df_train[i,3]){
    pep = df_train[i,1]
    if(substr(pep,j,j)=="A"){
      dataf2[i,1] = dataf2[i,1] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="C"){
      dataf2[i,2] = dataf2[i,2] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="D"){
      dataf2[i,3] = dataf2[i,3] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="E"){
      dataf2[i,4] = dataf2[i,4] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="F"){
      dataf2[i,5] = dataf2[i,5] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="G"){
      dataf2[i,6] = dataf2[i,6] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="H"){
      dataf2[i,7] = dataf2[i,7] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="I"){
      dataf2[i,8] = dataf2[i,8] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="K"){
      dataf2[i,9] = dataf2[i,9] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="L"){
      dataf2[i,10] = dataf2[i,10] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="M"){
      dataf2[i,11] = dataf2[i,11] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="N"){
      dataf2[i,12] = dataf2[i,12] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="P"){
      dataf2[i,13] = dataf2[i,13] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="Q"){
      dataf2[i,14] = dataf2[i,14] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="R"){
      dataf2[i,15] = dataf2[i,15] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="S"){
      dataf2[i,16] = dataf2[i,16] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="T"){
      dataf2[i,17] = dataf2[i,17] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="V"){
      dataf2[i,18] = dataf2[i,18] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="W"){
      dataf2[i,19] = dataf2[i,19] + (1/df_train[i,3]) 
    }else if(substr(pep,j,j)=="Y"){
      dataf2[i,20] = dataf2[i,20] + (1/df_train[i,3]) 
    }
    j=j+1
  }
  i=i+1
}

df_train[,4:23] = dataf2

dataf2 = data.frame(matrix(0, ncol = length(names_aa), nrow = nrow(df_test)))
colnames(dataf2) = names_aa
i = 1
while(i<=nrow(df_test)){
  j = 1
  while(j<=df_test[i,3]){
    pep = df_test[i,1]
    if(substr(pep,j,j)=="A"){
      dataf2[i,1] = dataf2[i,1] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="C"){
      dataf2[i,2] = dataf2[i,2] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="D"){
      dataf2[i,3] = dataf2[i,3] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="E"){
      dataf2[i,4] = dataf2[i,4] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="F"){
      dataf2[i,5] = dataf2[i,5] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="G"){
      dataf2[i,6] = dataf2[i,6] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="H"){
      dataf2[i,7] = dataf2[i,7] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="I"){
      dataf2[i,8] = dataf2[i,8] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="K"){
      dataf2[i,9] = dataf2[i,9] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="L"){
      dataf2[i,10] = dataf2[i,10] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="M"){
      dataf2[i,11] = dataf2[i,11] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="N"){
      dataf2[i,12] = dataf2[i,12] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="P"){
      dataf2[i,13] = dataf2[i,13] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="Q"){
      dataf2[i,14] = dataf2[i,14] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="R"){
      dataf2[i,15] = dataf2[i,15] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="S"){
      dataf2[i,16] = dataf2[i,16] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="T"){
      dataf2[i,17] = dataf2[i,17] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="V"){
      dataf2[i,18] = dataf2[i,18] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="W"){
      dataf2[i,19] = dataf2[i,19] + (1/df_test[i,3]) 
    }else if(substr(pep,j,j)=="Y"){
      dataf2[i,20] = dataf2[i,20] + (1/df_test[i,3]) 
    }
    j=j+1
  }
  i=i+1
}

df_test[,4:23] = dataf2

#We now have 21 features for prediction available (the 20 amino acids and the length).
#To ensure all students can complete the exercise, you can load the finished tables as follows:
df_test = read.csv("peptides.f.test.csv", header=T, stringsAsFactors=F)
df_train = read.csv("peptides.f.train.csv", header=T, stringsAsFactors=F)

#Now train a new linear model with all 21 features and compute the MSE
lm.20 = lm(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train) #removed V

#Did the MSE become smaller with more predictors? What is its value?
MSE3a = mean((predict(lm.20, df_train)-df_train$rt)^2)
MSELM = mean((predict(lm.20, df_test)-df_test$rt)^2)

print(paste0("The MSE using the the training dataset: ", round(MSE3a, digits = 3)))
print(paste0("The MSE using the the testing dataset: ", round(MSELM, digits = 3)))

# Did the MSE become smaller with more predictors?
print("The MSE values are now smaller using only the length as a predictor")

#Plot the predicted RT values against the true RT values in the test dataset.
#Do you think your prediction has improved compared to the previous model using only length?
plot(predict(lm.20, df_test), df_test$rt, xlab = "Predicted test RT results", ylab = "RT test results", main = "predicted RT values against the true RT values in the test dataset")
print("Yes, I think the prediction has improved over just using the length as seen by the lower MSE values")

#Which features are important for retention time? Can you explain these results, do they make sense to you?
#Note that HPLC separation is based on hydrophobicity and that the HPLC was run under acidic conditions.
print("As seen in the model summary, the length, and all amino acids other than valine and tyrosine were significant")
print("The expectation would be that hydrophobic amino acids increase the RT while hydrophilic amino acids decrease RT, both having a significant effect.")
dataf3 = summary(lm.20)$coefficients
dataf3 = dataf3[order(dataf3[,1]),]
print("Printout of the ordered estimates:")
print(dataf3[,1])
print("As you can see, the hydrophilic (basic and amidic) amino acids (arginine, histidine, lysine, asparagine, glutamine) corollated with decreased retention times,")
print("while hydrophobic amino acids (tryptophan, leucine, isoleucine and methionine) corrollated with increased retention times.")

#Train a RF regression model to predict retention time.
#Plot the predicted RT values against the true RT values in the test dataset.
rf.20 = randomForest(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train)
MSE3a = mean((predict(rf.20, df_train)-df_train$rt)^2)
MSERF = mean((predict(rf.20, df_test)-df_test$rt)^2)

print(paste0("The MSE using the the training dataset: ", round(MSE3a, digits = 3)))
print(paste0("The MSE using the the testing dataset: ", round(MSERF, digits = 3)))

#Plot the predicted RT values against the true RT values in the test dataset
plot(predict(rf.20, df_test), df_test$rt, xlab = "Predicted test RT results", ylab = "RT test results", main = "predicted RT values against the true RT values in the test dataset")

#How does the MSE on the test data compare to the linear model?
print("The MSE values tend to be slightly higher for the random forest model than the linear model on the testing dataset.")
print("The difference seen between the training dataset and testing MSE are significant using the random forrest model, which means it is probably overfitting to the training data.")

#Train an artificial neural network model to predict retention time.

#Train different ANN models with different number of neurons in the hidden layer 1->5
nnet1 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 1, linout=TRUE)
nnet2 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 2, linout=TRUE)
nnet3 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 3, linout=TRUE)
nnet4 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 4, linout=TRUE)
nnet5 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 5, linout=TRUE)

MSE1 = mean((predict(nnet1, df_test)-df_test$rt)^2)
MSE2 = mean((predict(nnet2, df_test)-df_test$rt)^2)
MSE3 = mean((predict(nnet3, df_test)-df_test$rt)^2)
MSE4 = mean((predict(nnet4, df_test)-df_test$rt)^2)
MSE5 = mean((predict(nnet5, df_test)-df_test$rt)^2)

print("MSE results using 1 through 5 hidden layers:")
cases0 =  matrix(data = c("1", "2", "3", "4", "5",MSE1, MSE2, MSE3, MSE4, MSE5), nrow = 5, ncol = 2, dimnames = list(c("1", "2", "3", "4", "5"),c("Size", "MSE")))
print(cases0)

#Try to add a weight decay parameter of 0.1, does this improve the quality of your models? Does it improve robustness?
nnet1 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 1, linout=TRUE, decay=0.1)
nnet2 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 2, linout=TRUE, decay=0.1)
nnet3 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 3, linout=TRUE, decay=0.1)
nnet4 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 4, linout=TRUE, decay=0.1)
nnet5 = nnet(rt~length+ A+ C+ D+ E+ df_train[,8]+ G+ H+ I+ K+ L+ M+ N+ P+ Q+ R+ S+ df_train[,20]+ W+ Y,df_train, size = 5, linout=TRUE, decay=0.1)

MSE1 = mean((predict(nnet1, df_test)-df_test$rt)^2)
MSE2 = mean((predict(nnet2, df_test)-df_test$rt)^2)
MSE3 = mean((predict(nnet3, df_test)-df_test$rt)^2)
MSE4 = mean((predict(nnet4, df_test)-df_test$rt)^2)
MSE5 = mean((predict(nnet5, df_test)-df_test$rt)^2)

print("MSE results using 1 through 5 hidden layers adding decay:")
cases0 =  matrix(data = c("1", "2", "3", "4", "5",MSE1, MSE2, MSE3, MSE4, MSE5), nrow = 5, ncol = 2, dimnames = list(c("1", "2", "3", "4", "5"),c("Size", "MSE")))
print(cases0)
print("It does improve my robustness, the MSE values decrease throughout.")

#Select a final ANN model among those you tested and report its performance.
#How does its performance compare to linear models and RF?
v = c(MSE1,MSE2, MSE3, MSE4, MSE5)
MSEANN = min(v)
print("Using the smallest MSE value when adding the decay parameter, here are the results when comparing the linear model, random forrest and ANN MSEs:")
cases0 =  matrix(data = c(MSELM, MSERF, MSEANN), nrow = 3, ncol = 1, dimnames = list(c("LM", "RF", "ANN"),c("MSE values")))
print(cases0)
print("As you can see, the ANN MSE is lower that the LM or RF MSE values, which means it is doing well as a model.")
  
  