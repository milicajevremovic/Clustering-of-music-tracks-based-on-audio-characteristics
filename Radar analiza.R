# Cluster Radar Chart

#This helps to make the chart more clear and readable.
ds_norm <- cbind(dataset[1], apply(dataset[-c(1,10,12)],2,scale))

cluster_centers <- as.data.frame(pam.res$medoids)
cluster <- c("Кластер 1", "Кластер 2", "Кластер 3", "Кластер 4", "Кластер 5")
cluster_centers <- cbind(cluster, cluster_centers)

#install.packages('tidyr')
library(tidyr)
radarDF_5 <- gather(cluster_centers, key=Attribute, value=Score, -cluster) %>%
  spread(key=cluster, value=Score)

# Change the colours according to clusters
colMatrix = matrix(c(c(153,50,204), c(0,255,0), c(251,114,1), c(33,205,255), c(255,33,156)), nrow = 3)

# Chart

#install.packages('radarchart')
library(radarchart)

chartJSRadar(scores = radarDF_5, scaleStartValue = -3, maxScale = 3, showToolTipLabel = TRUE, colMatrix = colMatrix)

# preuzeto: https://www.chartjs.org/
# preuzeto: https://github.com/khanhnamle1994/spotify-artists-analysis/blob/master/Data-Visualization.R
# preuzeto: https://www.freecodecamp.org/news/spotifys-this-is-playlists-the-ultimate-song-analysis-for-50-mainstream-artists-491882081819/