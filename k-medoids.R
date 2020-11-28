str(dataset)

kmedoids_ds <- dataset

kmedoids_ds$year <- NULL
kmedoids_ds$track.name <- NULL
kmedoids_ds$playlist_name <- NULL

str(kmedoids_ds)

apply(X = kmedoids_ds[,],
      MARGIN = 2,
      FUN = shapiro.test)

#Variables are not normally distributed. Thus, we wont standardize using mean and SD, and for the other variables, we’ll use median and IQR.
#To do the scaling, we will use the scale function (from the base package).

#we will do scalin on nonnormal distributed variables
#apply the scalling function to each column

kmedoids.st <- apply(X = kmedoids_ds,
                     MARGIN = 2,
                     FUN = function(x) scale(x, center = median(x), scale =
                                               IQR(x)))
#since apply() f. returns a list, convert it to a data frame
kmedoids.st <- as.data.frame(kmedoids.st)


#end of scaling data
summary(kmedoids.st)

#Estimating the optimal number of clusters

#To create a beautiful graph of the clusters generated with the pam() function, will use
#the factoextra package.
#1. Installing required packages:

# install.packages(c("cluster", "factoextra"))

library(cluster)
library(factoextra)

#To estimate the optimal number of clusters, we’ll use the average silhouette method

#The average silhouette measures the quality of a clustering. A high average silhouette
#width indicates a good clustering. 

?fviz_nbclust

fviz_nbclust(kmedoids.st, pam, method = "silhouette")+  theme_classic()

# from the plot the suggested number is 6

pam.res <- pam(kmedoids.st, 5)
print(pam.res)

#if you want to add the point classifications to the original data, use this:
dd <- cbind(dataset, cluster = pam.res$cluster)
head(dd, n = 5)

pam.res$medoids

pam.res$clusinfo

#Visualizing PAM clusters
# P.C.A

fviz_cluster(pam.res,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic(), geom = "point")

fviz_cluster(pam.res,
             repel = TRUE, # Avoid label overplotting (slow)
             palette = "Set2",
             ggtheme = theme_classic(), 
             geom = "point")

## Izvor za lep grafik:
## https://rpkgs.datanovia.com/factoextra/reference/fviz_cluster.html

table(dataset$playlist_name, pam.res$cluster)

#plot(dataset$danceability, dataset$track.popularity, col = pam.res$cluster)

## function that provides summary statistics about clusters
summary.stats <- function(feature.set, clusters, cl.num) {
  sum.stats <- aggregate(x = feature.set,
                         by = list(clusters),
                         FUN = function(x) {
                           m <- mean(x, na.rm = T)
                           sd <- sqrt(var(x, na.rm = T))
                           paste(round(m, digits = 2), " (",
                                 round(sd, digits = 2), ")", sep = "")
                         })
  sum.stat.df <- data.frame(cluster = sum.stats[,1],
                            freq = as.vector(table(clusters)),
                            sum.stats[,-1])
  sum.stats.transpose <- t( as.matrix(sum.stat.df) )
  sum.stats.transpose <- as.data.frame(sum.stats.transpose)
  attributes <- rownames(sum.stats.transpose)
  sum.stats.transpose <- as.data.frame( cbind(attributes, sum.stats.transpose) )
  colnames(sum.stats.transpose) <- c( "attributes", rep("Mean (SD)", cl.num) )
  rownames(sum.stats.transpose) <- NULL
  sum.stats.transpose
}

sum.stats <- summary.stats(feature.set = kmedoids_ds, 
                           clusters = pam.res$cluster,
                           cl.num = 5)
sum.stats


points(pam.res$medoids[, c('valence', 'track.popularity')], col=1:4, pch = 5, cex = 2) #plot cluster centers

#silueta 

silueta<-silhouette(pam.res$clustering, dist(kmedoids.st))

fviz_silhouette(silueta)

summary(silueta)

# PREUZETO: https://rpubs.com/mert-camur/580886
# jos jedan clanak 
#https://www.datanovia.com/en/blog/clustering-example-4-steps-you-should-know/

#dvodimenzionalna vizualizacija


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

