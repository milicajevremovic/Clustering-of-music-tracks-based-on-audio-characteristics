
##########################################################################
###                              DBSCAN                                ###
##########################################################################

#install.packages("fpc")
library('fpc')
#install.packages("dbscan")
library('dbscan')

data <- read.csv('dataset.csv')
df <- data
df$year <- NULL
df$track.name <- NULL
df$playlist_name <- NULL

summary(df)

df <- apply(X = df,
            MARGIN = 2,
            FUN = function(x) scale(x, center = median(x), scale = IQR(x)))
df <- as.data.frame(df)

# Compute DBSCAN using fpc package

# optimal paramethers
dbscan::kNNdistplot(df, k =  5) 
abline(h = 1.4, lty = 2)

set.seed(123)
db <- fpc::dbscan(df, eps = 1.29, MinPts = 50)
#120 3 clust
#grouping results
print(db)

#library("factoextra")
#1
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point")
#2
fviz_cluster(db, df, geom = "point")

fviz_cluster(db, data = df,
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
              geom = "point")

which(db$cluster == 0)

# Setting row names for better visualisation
row.names(df) <- data$track.name

summary(df)

df$cluster <- db$cluster

cluster1 <- df[which(df$cluster == 1),] 
cluster2 <- df[which(df$cluster == 2),] 
cluster3 <- df[which(df$cluster == 3),] 
cluster4 <- df[which(df$cluster == 4),] 

cluster1 <- cluster1[, -10]
cluster2 <- cluster2[, -10]
cluster3 <- cluster3[, -10]
cluster4 <- cluster4[, -10]

#PCA visualisation of clusters

# Source:
# https://rpkgs.datanovia.com/factoextra/reference/fviz_pca.html

library("FactoMineR")
res.pca <- prcomp(cluster1,  scale = TRUE)

# 1. Control automatically the color of individuals 
# using the "cos2" or the contributions "contrib"
# cos2 = the quality of the individuals on the factor map
# 2. To keep only point or text use geom = "point" or geom = "text".
# 3. Change themes using ggtheme: http://www.sthda.com/english/wiki/ggplot2-themes
fviz_pca_ind(res.pca, col.ind="cos2", #geom = "point",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07" ))


# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#E61E0E"),
             ggtheme = theme_minimal())

ggplot(cluster2, aes(x = Long,y = Lat, label = rownames(Eurodf))) +
  geom_point(color = "blue") + geom_text(hjust = 0.5, vjust = -0.5)

# We have to introduce this variable for horizontal bar genre plot by cluster
df$genre <- as.factor(data$playlist_name)

# How genres are actually distributed in clusters?
ggplot(data = df, aes(y = cluster)) +
  geom_bar(aes(fill = genre)) +
  ggtitle("Расподела жанровских плејлисти по кластерима") +
  theme(plot.title = element_text(hjust = 0.5))

# Source:
# https://towardsdatascience.com/how-to-use-and-visualize-k-means-clustering-in-r-19264374a53c


# Measuring clustering quality - Silhouette

library(cluster)

df$genre <- NULL
df$cluster <- NULL

#r Remove zero cluster - outliers, and also remove observations in df which match zero cluster

db$cluster <- db$cluster[which(db$cluster != 0)]
df <- df[-(which(db$cluster == 0)),]

silh <- silhouette(db$cluster, dist(df))
#fviz_silhouette(silh)
plot(silh,col=1:8,border=NA)
summary(silh)
