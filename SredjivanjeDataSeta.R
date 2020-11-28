#'За учитавање плејлисти које ћемо користити у овом раду, користићемо пакет spotifyr.
#učitavanje spotifyr paketa u okruženje

remotes::install_github("charlie86/spotifyr")
library(spotifyr)

#postavljanje clidentIDa

clientID <- 'f2a1c03c4a89453a9a994f0a97593fe9'

#postavljanje clientServer

clientSecret <- '0b53cc55d0ef4508a8938e8973a75f79'

#dodeljivanje dokaza identifikacije sistemskom okruzenju

Sys.setenv(SPOTIFY_CLIENT_ID = clientID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = clientSecret)

#'Након што смо поставили свој клијет ин и тајни кључ, можемо да приступимо токену.
#'Потребан нам је важећи приступни токен услуге Spotify Accounts који је резултат извршавања функције get_spotify_access_token() пакета spotifyr.
#pribavljane tokena

access_token <- get_spotify_access_token()

#'Функција која ће нам требати за преузимање одређених персоназизованих плејлисти је get_playlist_audio_features(playlist_username, playlist_uris)
#'Поребно је да наведемо uri(Uniform Resource Identifier) корисника од ког желимо да преузмемо плејлисту и uri плејлисте.Uri се не може пронаћи у опцијама плејлисте 
#'путем веб апликације, већ једино путем десктоп апликације, путањом Share/Copy Spotify Uri.

playlist_username <- 'Spotify'
playlist_uris <- '37i9dQZF1DWXRqgorJj26U'

Rock_Classics <- get_playlist_audio_features(playlist_username, playlist_uris)

#preuzimanje plejlisti

pl_RnB <- get_playlist_audio_features(12122044555, '4lZRKvOQKJwvK2fm7Z1n8i')
pl_Techno <- get_playlist_audio_features('acemasterrace', '59KuQSm27IfRylpXxz9KrM') 
pl_HipHop <-get_playlist_audio_features('antyg123', '655aKiu3xtToGcIrs4lquB')  
pl_HappyHits <- get_playlist_audio_features(playlist_username, '37i9dQZF1DXdPec7aLTmlC')
pl_Energic <- get_playlist_audio_features('51rbktbfuqw4yisgc8jgkcx1z', '0kzCZrsdFlQdOug0Dm3sTb')
pl_BeautifulVoices <- get_playlist_audio_features(playlist_username, '37i9dQZF1DX6z20IXmBjWI')
pl_CoffeTableJazz <-get_playlist_audio_features(playlist_username, '37i9dQZF1DWVqfgj8NZEp1')
pl_Rock<- get_playlist_audio_features(playlist_username, '37i9dQZF1DX1X7WV84927n') 

pl_Energic$playlist_name <- 'Pop'
pl_BeautifulVoices$playlist_name <-'Neo Soul'
pl_RnB$playlist_name<- 'R&B'
pl_HipHop$playlist_name <- 'Hip Hop'
pl_CoffeTableJazz$playlist_name<- 'Jazz'
pl_Techno$playlist_name <- 'Electronica'
pl_HappyHits$playlist_name <- 'Top Charts 2020'
pl_Rock$playlist_name <-'Rock'

mixed.playlists <- rbind(pl_Energic,
                         pl_HappyHits,
                         pl_RnB,
                         pl_BeautifulVoices,
                         pl_HipHop,
                         pl_CoffeTableJazz,
                         pl_Techno,
                         pl_Rock)

#kreiranje novog dataseta sa kljucnim varijablama

mixed.playlists.selected.features <- select(mixed.playlists, c(playlist_name, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, track.name, track.popularity,track.album.release_date))

#provera NA

colSums(is.na(mixed.playlists.selected.features))

dataset <- mixed.playlists.selected.features
dataset <- as.data.frame(dataset)
str(dataset)
#' Креирали смо датафрејм од 12 варијабли са 3 карактер варијабле. Пошто смо претходно учитали dplyr библиотеку, која служи за ефикасно манипулисање подацима, такође ћемо користити pipe operator - %>%. 
#' Сложени оператор додељивања %<>% користи се за ажурирање вредности тако што се прво пребацује у један или више израза, а затим додељује резултат. 
#' Стога, трансформисаћемо карактер варијаблу playlist_name у фактор, а затим ћемо из track.album.release_date из датума извући само годину издања и затим пребацити у целобројни тип податка.
library(dplyr)
#transformisanje karakter varijable u faktorsku, s obzirom da se radi o nekoliko naziva plejlisti
dataset <- dataset  %>% 
  mutate(playlist_name = as.factor(playlist_name))

#izvlacenje godine izdanja pesme iz karaktera datuma
dataset$year <-substr(dataset$track.album.release_date, start = 1, stop = 4)

#transformisanje godine kao karaktera u numericku vrednost
dataset <- dataset  %>% 
  mutate(year = as.numeric(year))

#ukljanjanje nepotrebne varijable
dataset$track.album.release_date <- NULL
#'Након уклањања непотребних варијабли и трансформисању у одговарајуће типове податка погодне за анализу, добили смо коначан скуп података за каснију манипулацију.
# spremljen zdrav dataset

str(dataset)

boxplot(dataset$speechiness, xlab='Speechiness') 

sort(boxplot.stats(dataset$speechiness)$out)

dataset$speechiness[dataset$speechiness >  0.218 ] <-  0.218 


boxplot(dataset$liveness, xlab='Liveness')

sort(boxplot.stats(dataset$liveness)$out)

dataset$liveness[dataset$liveness > 0.394  ] <- 0.394  

#cuvanje u CSV
write.csv(dataset,"C:\\Users\\Milica\\Desktop\\projekat 2\\data.set.csv", row.names = FALSE)
