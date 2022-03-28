#Answers to: Testing your understanding of how to create a volcano plot and heat map
#Marketa Hlavon

#Answers to Volcano Plot practice

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

p <- p + labs(x="Fold change",y="Adjusted P value", title = "Improved Volcano Plot")
p

# Question 2 
# Difficulty: Easy
# Set the x-limit to -6 and 6, y-limit to 0 and 4, add a light dotted line (?linetype) through
# the x intercept at 0, and plot it

p <- p + xlim(-6,6) + ylim(0,4) + geom_vline(xintercept=0, linetype=3)
p

# Question 3
# Difficulty: Hard
# Change the shape and size of the significantly top 3 up and top 3 down regulated genes and
# plot it

vol_data <- vol_data[order(vol_data$adjP),]
up_gene <- head(vol_data$gene_name[which(vol_data$sig == "up")],3)
down_gene <- head(vol_data$gene_name[which(vol_data$sig == "down")],3)
top3 <- c(as.character(up_gene), as.character(down_gene))
vol_data$topGenes = "NOT"
vol_data$topGenes[match(top3, vol_data$gene_name)] <- "TOP"
p <- p + geom_point(aes(shape = vol_data$topGenes)) + geom_point(aes(size = vol_data$topGenes))
p


######################################################################################

#Answers to Heatmap practice

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

pheatmap(graph1, border_color = "white")

# Question 2
# Difficulty: Medium
# Simplify the sample names to "A1", "B1" etc, increase the sample name fontsize to 15,
# and change the label so that it is horizontal (easier to read)

col_names = c("A2", "B2", "C2", "A1", "B1", "C1")
colnames(graph1) = col_names
pheatmap(graph1,  fontsize_col = 15, angle_col = 0)


# Question 3
# Difficulty: Medium
# Change the clustering method of the rows to median/WPGMC linkage, add a gap half-way
# through the columns and save the heatmap to the filename "HeatmapTest.png"

# Hint 1: a gap can not be added to a column or row that has clustering
# Hint 2: details about clustering can be found under hclust
pheatmap(graph1, clustering_method = "median", cluster_cols = FALSE, gaps_col = c(3), filename = "HeatmapTest.png")



