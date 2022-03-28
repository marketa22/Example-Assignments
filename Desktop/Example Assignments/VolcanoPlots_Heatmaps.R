#Marketa Hlavon
#This file was created for a video to teach someone how to make a volcano plot and a heatmap
#Examples and answers for practice included seperately

# Volcano Plot

setwd("~/Desktop") #set your working directory

library(ggplot2) #ggplot package

#read the data and check the format of the data
y <- read.table("volcano3.txt", header=TRUE, stringsAsFactors=FALSE, sep = "\t")

#select the required information
#for volcano plot, we need the log2FC, gene symble, and q.value
vol_data <- data.frame(logFC=y$logFC, gene_name=y$SYMBOL, adjP = y$q.value)

?ggplot() #information about the packages
?geom_point()
ggplot(data = vol_data,aes(x=logFC, y=-1*log10(adjP))) + geom_point()

#Making the plot look better

vol_data$sig <- "not-significant" #Show the significant expressed genes

#Use Which() to select the rows that have q value < 0.05 AND |logFC| > 1
vol_data$sig[which((vol_data$adjP < 0.05) & (vol_data$logFC > 1))] <- "up"
vol_data$sig[which((vol_data$adjP < 0.05) & (vol_data$logFC < -1))] <-  "down"

#Plot it again
ggplot(data = vol_data,aes(x=logFC, y=-1*log10(adjP), color = sig)) + geom_point()

#Color codes (Hex color code https://www.color-hex.com/)
p <- ggplot(data = vol_data,aes(x=logFC, y=-1*log10(adjP), color = sig)) + 
  geom_point() + scale_color_manual(values =c("#2f5688","#BBBBBB","#CC0000"))
p

#Add Titles and Axis labels
#labs()
p <- p + labs(x="log2(FoldChange)",y="-log10(FDR)", title = "Volcano plot")
p

#Add the limits, the intercept lines help to see the cutoff
p <- p + xlim(-5,5) + geom_hline(yintercept=-log10(0.05),linetype=4) + 
  geom_vline(xintercept=c(-1,1),linetype=4)
p

#Lable the top5 significantly expressed genes, add a new column called label
vol_data$label <- ""

#Sort the p value, find the top 5 up and down regulated genes
vol_data <- vol_data[order(vol_data$adjP),]
up_gene <- head(vol_data$gene_name[which(vol_data$sig == "up")],5)
down_gene <- head(vol_data$gene_name[which(vol_data$sig == "down")],5)

#Store the 10 genes in a vector, label them in the data frame
top5 <- c(as.character(up_gene), as.character(down_gene))
vol_data$label[match(top5, vol_data$gene_name)] <- top5
head(vol_data)

#Use geom_text() to label the dots
p <- p + geom_text(aes(label=vol_data$label))
p

#Theme() allows you to customize the non-data components of your plots
?theme()
p <- p + theme(panel.background = element_blank(), axis.title=element_text(size=10), 
               plot.title = element_text(hjust=0.5))
p

######################################################################################


#Heatmap

library(pheatmap) #pheatmap package

?pheatmap

#Read the data and check the format of the data
#Row.names = 1 take the column number of the data file from which to take the row names
heatmap_table <- read.table("heatmap2.txt", header = T, 
                            stringsAsFactors = FALSE, row.names=1)
head(heatmap_table)

#Make it as a matrix, because pheatmap requires a matrix as the input
graph1 <- as.matrix(heatmap_table)

pheatmap(graph1)

#Change the heatmap colour
color_key <- c("#3300CC", "#3399FF", "white","#FF3333", "#CC0000")
pheatmap(graph1, color = color_key, border_color = NA)

#Add the colour gradiant
pheatmap(graph1, color = colorRampPalette(color_key)(50), border_color = NA)

#cluster_row = FALSE, cluster for the genes
#cluster_col = FALSE, cluster for the samples
#the default for these two arguments are TRUE
pheatmap(graph1, color = colorRampPalette(color_key)(50), border_color = NA, 
         cluster_row = FALSE, cluster_col = FALSE)

#Labeling
colnames(graph1)

#Label the sample
#First create a dataframe with one column
sample_label = data.frame(Group = rep(c("Case", "Control"), c(3, 3)))
rownames(sample_label) <- colnames(graph1)
sample_label

#Label the columns using annotation_col
pheatmap(graph1, color = colorRampPalette(color_key)(50), border_color = NA, 
         annotation_col = sample_label)

#Label the rows
#ASSUME the first 12 genes in the table are replication-related
#The rest are transcription-related
row_label <- data.frame(Type = rep(c("Replication", "Transcription"), c(12, 36)))
rownames(row_label) <- rownames(graph1)

pheatmap(graph1, color = colorRampPalette(color_key)(50), border_color = NA, 
         annotation_col = sample_label, annotation_row = row_label)

#Figure title and font size
pheatmap(graph1, color = colorRampPalette(color_key)(50), border_color = NA, 
         annotation_col = sample_label, annotation_row = row_label,
         main = "Example heatmap", fontsize_row = 5)

#For more information to customize the plot, please look at the documentation of pheatmap







