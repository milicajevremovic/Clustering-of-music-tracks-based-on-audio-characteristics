#+eval=FALSE

library(tidyverse)

library(plotly)
library(corrplot)
library(factoextra)
library(plyr)
library(RColorBrewer)

library(knitr)

#################################### E.D.A. ####################################################################
# Analyzing data distribution of the audio features
library(funModeling)

spotify_hist <- dataset[,-c(1,10)]
plot_num(spotify_hist) 

#################################### K-means clustering ########################################################

plejlista <- as.data.frame(dataset)

apply(X = plejlista[, -c(1,10,12)], 
      MARGIN = 2,
      FUN = function(x) length(boxplot.stats(x)$out))

#Winsorzing

boxplot(plejlista$energy, xlab='Energy')

sort(boxplot.stats(plejlista$energy)$out)

quantile(plejlista$energy, probs = c(seq(0, 0.1, 0.025)))

new.min <- as.numeric(quantile(plejlista$energy, probs = 0.025))

plejlista$energy[plejlista$energy < new.min] <- new.min


boxplot(plejlista$loudness, xlab='Loudness')

sort(boxplot.stats(plejlista$loudness)$out)

quantile(plejlista$loudness, probs = c(seq(0, 0.1, 0.025)))

new.min <- as.numeric(quantile(plejlista$loudness, probs = 0.075))

plejlista$loudness[plejlista$loudness < new.min] <- new.min


boxplot(plejlista$speechiness, xlab='Speechiness') 

sort(boxplot.stats(plejlista$speechiness)$out)

quantile(plejlista$speechiness, probs = c(seq(0.8, 1, 0.025)))

new.max <- as.numeric(quantile(plejlista$speechiness, probs = 0.875))

plejlista$speechiness[plejlista$speechiness >  new.max] <- new.max


boxplot(plejlista$acousticness, xlab='Acousticness')

sort(boxplot.stats(plejlista$acousticness)$out)

quantile(plejlista$acousticness, probs = c(seq(0.8, 1, 0.025)))

new.max <- as.numeric(quantile(plejlista$acousticness, probs = 0.95))

plejlista$acousticness[plejlista$acousticness > new.max] <- new.max


boxplot(plejlista$liveness, xlab='Liveness')

sort(boxplot.stats(plejlista$liveness)$out)

quantile(plejlista$liveness, probs = c(seq(0.8, 1, 0.025)))

new.max <- as.numeric(quantile(plejlista$liveness, probs = 0.925))

plejlista$liveness[plejlista$liveness > new.max] <- new.max


#Skaliranje

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) - min(feature, na.rm = T)))
}

#za k-means eliminisemo faktor playlist_name, eliminisemo varijablu year koja nam nije potrebna
#cuvamo sredjenu plejlistu za dalje i kreiramo specijalnu k-means plejlistu
kmeans_pl <- plejlista

str(kmeans_pl)

kmeans_pl$playlist_name <-NULL
kmeans_pl$track.name <-NULL
kmeans_pl$year <-NULL

summary(kmeans_pl)

kmeans_pl.norm <- as.data.frame(apply(kmeans_pl[,], 2, normalize.feature))

#normalizovani k-means dataset
summary(kmeans_pl.norm)

#Elbow metoda
#ne pogadjamo k, vec odmah koristimo Elbow metod za optimalnu vrednost k
set.seed(3108)

eval.metrics.9var <- data.frame()

# testiramo kmeans za svaku K vrednost u opsegu 2:8, tacnije kalkulisemo podatke za lakat metodu
for (k in 2:8) {
  set.seed(3108)
  km.res <- kmeans(x=kmeans_pl.norm, centers=k, iter.max=20, nstart = 1000)
  # combine cluster number and the error measure, write to dataframe
  eval.metrics.9var <- rbind(eval.metrics.9var,
                             c(k, km.res$tot.withinss, km.res$betweenss/km.res$totss))
}

names(eval.metrics.9var) <- c("cluster", "tot.within.ss", "ratio")
eval.metrics.9var

#vizualizujemo lakat metodu
library(ggplot2)

ggplot(data=eval.metrics.9var, aes(x=cluster, y=tot.within.ss)) +
  geom_line() +
  ggtitle("Reduction in error for different values of K\n") +
  xlab("\nClusters") +
  ylab("Total Within Cluster Sum of Squares\n") +
  scale_x_continuous(breaks=seq(from=0, to=8, by=1))
#KOMENTAR: naocigled se vidi da je u cetvorci veci prelom

#f-ja za racunanje razlike izmedju dve uzastopne vrednosti
compute.difference <- function(values) {
  dif <- vector(mode = "numeric", length = length(values))
  dif[1] <- NA
  for(i in 1:(length(values)-1)) {
    dif[i+1] <- abs(values[i+1] - values[i])
  }
  dif
}

#izracunavanje "ratio-difference" za razlicite k vrednosti, od 2. do 8.
data.frame(k=c(2:8),
           tot.within.ss.delta=compute.difference(eval.metrics.9var$tot.within.ss),
           ration.delta=compute.difference(eval.metrics.9var$ratio))

#KOMENTAR: Najlepsa metrika je za k = 4
# tot.within.ss.delta  ration.delta
#            58.37909    0.10253520

#drugi nacin prikaza alternativnog lakta, iz knjige: Practical Guide To Cluster Analysis in R, str. 40
library(factoextra)

#wss - within sum od square
fviz_nbclust(kmeans_pl.norm, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

#KOMENTAR: kao i prethodni pokazatelji, potvrdjuje da je 4 idealan broj klastera
# ------------------------ KMEANS MODEL SA K=4 --------------------------

set.seed(3108)
kmeans_pl.norm.4k <- kmeans(x=kmeans_pl.norm, centers=4, iter.max=20, nstart = 1000)

#cuvamo claster vrednost u novoj faktor varijabli zarad vizuelizacije klastera
kmeans_pl.norm$cluster <- factor(kmeans_pl.norm.4k$cluster)
#Plot the 4-cluster solution

#Plot the clusters along with their centroids
ggplot(data=kmeans_pl.norm, aes(x=energy, y=track.popularity, colour=cluster)) +
  geom_point() +
  xlab("energija") +
  ylab("popularnost pesme") +
  ggtitle("Uporedjivanje varijabli") +
  geom_point(data=as.data.frame(kmeans_pl.norm.4k$centers),
             colour="black",size=4, shape=17)
#posto smo iskoristili za potrebu gglplota, sada uklanjamo faktor varijablu cluster iz normalizovanog dataseta
kmeans_pl.norm$cluster <- NULL

## KOMENTAR NA GRAFIK:
# Ovde sam napravila klaster 3 (crveni) Koji je karakteristican po tome da su u njemu pesme 
# koje su sa velikom energijom, a tome sledi i veca popularnost pesme
# Dok klaster 3 je "sve i svasta"
# Pesme nisu popularne
# Znaci nisu komercijalne
# Ali se meni svidjaju jer su jakog ritma, energicne, glasne
# Samo nisu "za siru javnost tacnije nisu se probile"


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

# examine cluster centers
sum.stats <- summary.stats(feature.set = kmeans_pl[,], #retail.data[,c(2:7)],
                           clusters = kmeans_pl.norm.4k$cluster,
                           cl.num = 4)
sum.stats


# sum.stats
#attributes     Mean (SD)     Mean (SD)     Mean (SD)    Mean (SD)
#1           cluster             1             2             3            4
#2              freq           459           389           305          305
#3      danceability   0.61 (0.13)   0.52 (0.12)    0.71 (0.1)  0.68 (0.14)
#4            energy    0.7 (0.17)   0.19 (0.12)   0.76 (0.17)   0.8 (0.14)
#5          loudness  -7.42 (3.27) -16.75 (3.37) -10.31 (2.89) -4.83 (2.36)
#6       speechiness   0.05 (0.02)   0.04 (0.02)   0.06 (0.03)  0.14 (0.02)
#7      acousticness   0.19 (0.21)   0.86 (0.14)   0.09 (0.16)  0.09 (0.13)
#8  instrumentalness   0.03 (0.09)   0.65 (0.33)   0.86 (0.11)  0.03 (0.12)
#9          liveness   0.16 (0.09)   0.13 (0.05)   0.12 (0.06)    0.2 (0.1)
#10          valence   0.57 (0.24)   0.32 (0.19)   0.22 (0.22)  0.49 (0.22)
#11 track.popularity 55.36 (25.58) 31.45 (20.18) 16.43 (14.08)   50 (20.63)

## ====> KOMENTAR STATISTIKA:

# klaster 1 sadrzi najvise popularne pesme, komercijalne, sto obrazlaze i to da su vesme veoma takodje i energicne i za djuskanje
# klaster 1 ima najvecu valencu, dakle: Valence: “A measure from 0.0 to 1.0 describing the musical positiveness 
# conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric),
# while tracks with low valence sound more negative (e.g. sad, depressed, angry)” znaci, tehno, klaster 3 nam bas zvuci depresivno, ljuto itd.
# klaster 4 je klaster sa cas 0.1 speechiness, sto znaci da su pune govora, takodje su i najenergicnije
# klaster 3 i 2 imaju nisku energiju, znaci da zvuce tuznije, manje srecno, manje euforicno
# klaster 3 je prepun instrumentala, ali ima bas malo akusticnosti
# klaster 2 isto je pun instrumentala, s tim sto se u klasteru 3 vise igra i ima veoma vise energije, a klaster 2 ima najvise akusticnosti (100% jazz!)

#plotovanje klastera

fviz_cluster(kmeans_pl.norm.4k, data = kmeans_pl.norm,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal(),
             geom = "point"
)

#interesantno

fviz_cluster(kmeans_pl.norm.4k, data = kmeans_pl.norm,
             repel = TRUE, 
             palette = "Set2",
             ggtheme = theme_classic(), 
             geom = "point")
