
##########################################################################
###                 Agglomerative Hierarchical Clustering               ##
##########################################################################

## Data ##
dataset <- read.csv("dataset.csv", stringsAsFactors = FALSE)
dataset <- dataset[-c(1294,1035),]
str(dataset)
hclust_ds <- dataset
hclust_ds$year <- NULL
hclust_ds$track.name <- NULL
hclust_ds$playlist_name <- NULL
## Scaling ##
summary(hclust_ds)
hclust_ds.st <- apply(X = hclust_ds,
                     MARGIN = 2,
                     FUN = function(x) scale(x, center = median(x), scale =
                                               IQR(x)))
hclust_ds.st <- as.data.frame(hclust_ds.st)

# Setting row names for better visualization
row.names(hclust_ds.st) <- dataset$track.name

## Choosing optimal linkage method ##

# Dissimilarity matrix
d0=dist(hclust_ds.st)


############ HEATMAP
library("factoextra")

fviz_dist(dist(df), show_labels = FALSE)+
  labs(title = "Fvz")

library(cluster)
# Hierarchical Agglomerative Clustering with various linkage methods
h1=hclust(d0,method='average')
h2=hclust(d0,method='complete')
h3=hclust(d0,method='ward.D')
h4=hclust(d0,method='single')
h5=hclust(d0,method='ward.D2')
h6=hclust(d0,method='mcquitty')
h7=hclust(d0,method='median')
h8=hclust(d0,method='centroid')

# Cophenetic Distances for each linkage
c1=cophenetic(h1)
c2=cophenetic(h2)
c3=cophenetic(h3)
c4=cophenetic(h4)
c5=cophenetic(h5)
c6=cophenetic(h6)
c7=cophenetic(h7)
c8=cophenetic(h8)

# Correlations
#the correlation coeficient shows that using a different linkage method creates a tree that represents the original distances slightly better.
cor(d0,c1) # 0.7434738
cor(d0,c2) # 0.5654911
cor(d0,c3) # 0.5921817
cor(d0,c4) # 0.3440507
cor(d0,c5) # 0.6023202
cor(d0,c6) # 0.6937402
cor(d0,c7) # 0.4187819
cor(d0,c8) # 0.6537135


########################################## KRUZNA I PARTITIVNA VIZUALIZACIJA NUMERA #######################################

library("factoextra")

# Create a plot of the whole dendrogram, and extract the dendrogram data
dend_plot <- fviz_dend(h3, k = 5, # Cut in five groups
                       cex = 0.6, # label size
                       k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#C542f5"),
                       show_labels = FALSE
)
dend_data <- attr(dend_plot, "dendrogram")

# Cut the dendrogram at height h = 10
dend_cuts <- cut(dend_data, h = 50)

# Plot subtree 1
fviz_dend(dend_cuts$lower[[1]],  main = "Subtree 1")
# Plot subtree 2
fviz_dend(dend_cuts$lower[[9]], main = "Subtree 2")

# 4 и 5 su zuti klaster
#6 crveni
#7 i 8 ljubicasti

#komplet kruzno stablo
fviz_dend(h1, cex = 0.2, k = 5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#C542f5"), type = "circular")

## Raspodela plejlisti po zanrovima
# Cluster distribution
grp <- cutree(h1, k = 5)
table(grp)
table(dataset$playlist_name, grp)

hclust_ds$genre <- dataset$playlist_name
hclust_ds$cluster <- grp

## horizontalni bar graf
ggplot(data = hclust_ds, aes(y = cluster)) +
  geom_bar(aes(fill = genre)) +
  ggtitle("Count of Clusters by Genre") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette plot

silh <- silhouette(grp, dist(hclust_ds.st))
plot(silh,col=1:8,border=NA)
summary(silh)


# prikaz dendograma po bojama i preseku
fviz_dend(h3, k = 5, 
          cex = 0.1, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#C542f5"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

# prelaz iz densograma u PCA
fviz_cluster(list(data = hclust_ds.st, cluster = grp),
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#C542f5"),
  
             #ggtheme = theme_classic(),
             geom = "point")


plot(list(data = hclust_ds.st, cluster = grp), choice = "3D.map")


#find optimal number of clusters for better cluster quality
fviz_nbclust(hclust_ds.st, FUN = hcut, method = "silhouette")
# suggested number of clusters is 4 


##SUMMARY: 



#odlican materijal za evaluaciju:
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7952