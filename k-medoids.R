
#################################################################################
###                         K - medoids Clustering                             ##
#################################################################################

dataset <- read.csv("dataset.csv", stringsAsFactors = FALSE)

kmedoids_ds <- dataset
kmedoids_ds$year <- NULL
kmedoids_ds$track.name <- NULL
kmedoids_ds$playlist_name <- NULL

apply(X = kmedoids_ds[,],
      MARGIN = 2,
      FUN = shapiro.test)

#Variables are not normally distributed

kmedoids.st <- apply(X = kmedoids_ds,
                     MARGIN = 2,
                     FUN = function(x) scale(x, center = median(x), scale =
                                               IQR(x)))
#Since apply() f. returns a list, we will convert dataset to a data frame

kmedoids.st <- as.data.frame(kmedoids.st)

#End of scaling data
summary(kmedoids.st)

#Estimating the optimal number of clusters

#To create a beautiful graph of the clusters generated with the pam() function, will use
#the factoextra package.

#Install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)

#To estimate the optimal number of clusters, we will use the average silhouette method

#The average silhouette measures the quality of a clustering.

fviz_nbclust(kmedoids.st, pam, method = "silhouette")+  theme_classic()
#from the plot the suggested number is 6

#We have decided to go with 5 clusters
pam.res <- pam(kmedoids.st, 5)

#Glimpse into 5k model
print(pam.res)
pam.res$medoids
pam.res$clusinfo

#Visualizing PAM clusters
# P.C.A

fviz_cluster(pam.res,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             geom = "point")

## Source:
## https://rpkgs.datanovia.com/factoextra/reference/fviz_cluster.html

#Dive into real data and look into spreading of genres in created clusters
table(dataset$playlist_name, pam.res$cluster)

#plot(dataset$danceability, dataset$track.popularity, col = pam.res$cluster)

# function that provides summary statistics about clusters
summary.stats <- function(feature.set, clusters, cl.num) {
  sum.stats <- aggregate(x = feature.set,
                         by = list(clusters),
                         FUN = function(x) {
                           m <- median(x, na.rm = T)
                           Q1 <- quantile(x, 0.25, na.rm = T)
                           Q3 <- quantile(x, 0.75, na.rm = T)
                           paste(round(m, digits = 2), " (",
                                 round(Q1, digits = 2), ", ",round(Q3, digits = 2), ")", sep = "")
                         })
  sum.stat.df <- data.frame(cluster = sum.stats[,1],
                            freq = as.vector(table(clusters)),
                            sum.stats[,-1])
  sum.stats.transpose <- t( as.matrix(sum.stat.df) )
  sum.stats.transpose <- as.data.frame(sum.stats.transpose)
  attributes <- rownames(sum.stats.transpose)
  sum.stats.transpose <- as.data.frame( cbind(attributes, sum.stats.transpose) )
  colnames(sum.stats.transpose) <- c( "attributes", rep("Median(Q1,Q3)", cl.num) )
  rownames(sum.stats.transpose) <- NULL
  sum.stats.transpose
}

sum.stats <- summary.stats(feature.set = kmedoids_ds, 
                           clusters = pam.res$cluster,
                           cl.num = 5)
sum.stats

#Plot cluster centers
#points(pam.res$medoids[, c('valence', 'track.popularity')], col=1:4, pch = 5, cex = 2) 

#Silhouette quality 

silh <- silhouette(pam.res$clustering,  dist(kmedoids.st))
#fviz_silhouette(silh)
plot(silh,col=1:10,border=NA)
summary(silh)

# PREUZETO: https://rpubs.com/mert-camur/580886
# jos jedan clanak 
#https://www.datanovia.com/en/blog/clustering-example-4-steps-you-should-know/

#Dvodimenzionalna vizualizacija

kmedoids.st$clusters <- factor(pam.res$clustering)

#Plot the clusters along with their centroids
ggplot(data=kmedoids.st, aes(x=energy, y=track.popularity, colour=clusters)) +
  geom_point() +
  xlab("energija") +
  ylab("popularnost pesme") +
  ggtitle("Uporedjivanje varijabli") +
  geom_point(data=as.data.frame(pam.res$medoids),
             colour="black",size=4, shape=17 )

kmedoids.st$clusters <- NULL

#literatura: skripta sa vezbi