#Testing your understanding of how to create a volcano plot and heat map
#Marketa Hlavon
#Answers are on a seperate document Answers_TestinUnderstanding.R

#Volcano Plot practice
#You will need volcano3.txt from google drive and ggplot2 installed to complete

#Run before you start, remember to set your working directory
library(ggplot2)

#read the data and check the format of the data
#select the required information (log2FC, gene symble, and q.value)
y <- read.table("volcano3.txt", header=TRUE, stringsAsFactors=FALSE, sep = "\t")
vol_data <- data.frame(logFC=y$logFC, gene_name=y$SYMBOL, adjP = y$q.value)

#selecting significant data q value < 0.05 AND |logFC| > 1
vol_data$sig <- "not-significant"
vol_data$sig[which((vol_data$adjP < 0.05) & (vol_data$logFC > 1))] <- "up"
vol_data$sig[which((vol_data$adjP < 0.05) & (vol_data$logFC < -1))] <-  "down"

#plotting graph with colours of choice
p <- ggplot(data = vol_data,aes(x=logFC, y=-1*log10(adjP), color = sig)) + geom_point() +
  scale_color_manual(values =c("#2f5688","#BBBBBB","#CC0000"))
p


# Question 1
# Difficulty: Easy 
# Label the volcano plot so that the x axis says "Fold change", y axis says "Adjusted P value"
# and the title of the plot is "Improved Volcano Plot", and plot it


# Question 2 
# Difficulty: Easy
# Set the x-limit to -6 and 6, y-limit to 0 and 4, add a light dotted line (?linetype) through
# the x intercept at 0, and plot it


# Question 3
# Difficulty: Hard
# Change the shape and size of the significantly top 3 up and top 3 down regulated genes and
# plot it



######################################################################################

#Heatmap practice

#You will need heatmap2.txt from google drive and pheatmap installed to complete

#Run before you start, remember to set your working directory
library(pheatmap)

#read the data and check the format of the data
#make it as a matrix, because pheatmap requires a matrix as the input
heatmap_table <- read.table("heatmap2.txt", header = T, stringsAsFactors = FALSE, row.names=1)
graph1 <- as.matrix(heatmap_table)
pheatmap(graph1)



# Question 1
# Difficulty: Easy
# Make the border colour white



# Question 2
# Difficulty: Medium
# Simplify the sample names to "A1", "B1" etc, increase the sample name fontsize to 15,
# and change the label so that it is horizontal (easier to read)



# Question 3
# Difficulty: Medium
# Change the clustering method of the rows to median/WPGMC linkage, add a gap half-way
# through the columns and save the heatmap to the filename "HeatmapTest.png"

