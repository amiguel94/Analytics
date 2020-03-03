#working directory
setwd("~/Desktop/MONASH/Data visualization and analytics unit - ETF5922/Analytics")

#packages
library(tidyverse)
library(arules)
library(ggrepel)
library(rpart)
library(rpart.plot)
library(precrec)
library(ade4)
library(ggdendro)

utilities <- as_tibble(read.csv("Utilities.csv"))
utility <- utilities %>% 
  column_to_rownames(var = 'Company') # set row names to the utilities column

utilities.norm <-sapply(utility, scale)# normalize input variables

row.names(utilities.norm) <- row.names(utility)# add row names: utilities
d.norm <- dist(utilities.norm[,c(6,8)],# compute normalized distance based on Sales and Fuel Cost
          method = "euclidean")

hc1 <- hclust(d.norm, method = "single")
ggdendrogram(hc1,
             theme_dendro = F) +
  ylab("Height") + 
  xlab('Utilities Companies') + labs(title = "Single Method")

hc2 <- hclust(d.norm, method = "average")
ggdendrogram(hc2,
             theme_dendro = F) +
  ylab("Height") + 
  xlab('Utilities Companies') + labs(title = "Average Method")

memb <- cutree(hc1, k = 6)
memb2 <- cutree(hc2, k = 6)

#validating clusters
#we notice that both methods (single and average linkage) identify {NY} and {San Diego} as singleton clusters
#Also, both dendrograms imply that a reasonable number of clusters in this dataset is four.
#One insight that can be derived from the average linkage clustering is that clusters
#tend to group geographically

# set labels as cluster membership and utility name
row.names(utilities.norm) <- paste(memb2, ": ", row.names(utility), sep = "")
# plot heatmap
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.norm), Colv = NA, hclustfun = hclust,
        col = rev(paste("grey",1:99,sep="")))

#------------k-means--------

# run kmeans algorithm
# load and preprocess data
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]
# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df)
# run kmeans algorithm
km <- kmeans(utilities.df.norm, 6)
# show cluster membership
km$cluster #San Diego as a singleton cluster
km$centers # cluster 5 is distiguished by low fixed charge and RoR, and high Demand-growth
