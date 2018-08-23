library(rsconnect)
library(tidyverse)
library(leaflet)
library(wordcloud)
library(DT)
library(ISOweek)
library(zoo)


# DATASET -----------------------------------------------------------------

# vector
datVect <- read.csv("aegypti_albopictus.csv") %>% 
  select(VECTOR,Y,X,YEAR,COUNTRY)
summary(datVect)
# Brazil, Dominican Republic, Indonesia, Singapore, Sri Lanka, Malaysia, Mexico, Viet Nam
datMap <- filter(datVect, COUNTRY=="Indonesia")
pal <- colorFactor(palette = c('red', 'blue'),
                   domain = datMap$VECTOR)


# MAP ---------------------------------------------------------------------

leaflet() %>% addTiles() %>% 
  addCircles(data = datMap, lat = ~ Y, lng = ~ X, color=~pal(VECTOR), fill = FALSE)


# Twitter -----------------------------------------------------------------

d <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/datWC.csv"))
datTweets <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/datTweets.csv"))

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 15,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

leaflet(datTweets) %>% addTiles() %>%
  addCircleMarkers(radius = ~n, fillOpacity = 0.3)

map("world", fill=FALSE, col="black", bg="white", ylim=c(-60, 90), mar=c(0,0,0,0))
points(datTweets$longitude, datTweets$latitude, col="red", pch=21, cex=datTweets$n/10)

# news -----------------------------------------------------------------

newsLOC <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/newsLOC.csv"))
newsLOC$X <- NULL
colnames(newsLOC)[which(names(newsLOC) == "news_date")] <- "date"
colnames(newsLOC)[which(names(newsLOC) == "news_title")] <- "title"
datatable(newsLOC)

newsINT <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/newsINT.csv"))
newsINT$X <- NULL
colnames(newsINT)[which(names(newsINT) == "news_date2")] <- "date"
colnames(newsINT)[which(names(newsINT) == "news_title2")] <- "title"
datatable(newsINT)


# GTrends -----------------------------------------------------------------

load(url("http://globalminers8973.cloudapp.net:3838/d4t4/Gplot.RData"))
datG <- res$interest_over_time
datGden <- subset(datG, keyword=="dengue")
datGzik <- subset(datG, keyword=="zika")
datGchi <- subset(datG, keyword=="chikungunya")

datGtot <- merge(datGden,datGzik, by="date")
datGtot <- merge(datGtot,datGchi, by="date")

plot(datGtot$date,datGtot$hits.x, ylim=c(0,100), type="l", col="red", xlab="t", ylab="hits")
points(datGtot$date, datGtot$hits.y, type="l", col="orange")
points(datGtot$date, datGtot$hits, type="l", col="blue")

legend("topleft",
       legend=c("dengue","zika", "chikungunya"),
       pch=15, col=c("red","orange","blue"), bty="n", pt.lwd=4)


# MOVE --------------------------------------------------------------------

library("threejs")
earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
#globejs(img=earth, bg="white")

datTweetsORI <- read.csv(url("http://globalminers8973.cloudapp.net:3838/d4t4/datTweetsORI.csv"))
datTweetsORI <- drop_na(datTweetsORI, c(lon,lat))
globejs(bg="white", lat=datTweetsORI$lat, long=datTweetsORI$lon)

##

library("maps")
map("world", fill=FALSE, col="black", bg="white", ylim=c(-60, 90), mar=c(0,0,0,0))
points(datTweetsORI$lon, datTweetsORI$lat, col="red", pch=3, cex=.3)


# WEATHER -----------------------------------------------------------------

library("rnoaa")
# station_data <- ghcnd_stations()
# save(station_data, file = "datStation.RData")
load("datStation.RData") 

lat_lon_df <- data.frame(id="study_area", name="study_area", 
                         latitude=-7.797, longitude=110.370) #-7.797068 #110.370529

# Get all stations within 150 kilometers
meteo_stat <- meteo_nearby_stations(lat_lon_df = lat_lon_df, station_data = station_data,
                                    radius = 100, var = c("PRCP","TMIN","TMAX"))

meteo_stat <- meteo_stat$study_area

#library(leaflet)
leaflet() %>% addTiles() %>%
  addCircleMarkers(data=lat_lon_df, ~longitude, ~latitude, radius = 5, color = "red", label = ~as.character(id)) %>%
  addCircleMarkers(data=meteo_stat, ~longitude, ~latitude, radius = 5, color = "blue", popup = ~as.character(id), label = ~as.character(name))

# map <- get_map(location = c(lon = 20.304, lat = 63.820), zoom = 8)
# ggmap(map) +
#   geom_point(aes(x = longitude, y = latitude),
#              data = meteo_stat, col = "red", size = 1) + 
#   geom_text(aes(x = longitude, y = latitude, label = name), 
#             data = meteo_stat, vjust = -0.5, hjust = 0, size = 4, col = "red") +
#   geom_point(aes(x = longitude, y = latitude),
#              data = lat_lon_df, col = "darkorange2", size = 4, shape = 4) +
#   geom_text(aes(x = longitude, y = latitude, label = id), 
#             data = lat_lon_df, vjust = -0.5, hjust = 0, size = 4, col = "darkorange2")

meteo_dat <- meteo_pull_monitors(meteo_stat$id,
                                 date_min = "2001-01-01",
                                 date_max = "2010-12-25") %>%
  rename(day = date, location = id)

##

meteo_plot <- meteo_dat %>%
  select(-tmax, -tmin) %>%
  tidyr::gather(parameter, value, tavg:prcp)

ggplot(meteo_plot) +
  geom_line(aes(x = day, y = value, col = location)) +
  facet_grid(parameter ~ ., scales = "free_y") + theme_bw()

##
meteo_datS <- meteo_pull_monitors("IDM00096839",
                                  date_min = "2001-01-01",
                                  date_max = "2010-12-25") %>%
  rename(day = date, location = id)

meteo_datS <- select(meteo_datS, day,prcp,tavg) %>% mutate(tavg=(tavg/10))
meteo_datS$YM <- as.yearmon(meteo_datS$day)

meteo_datS <- select(meteo_datS, YM,prcp,tavg) %>% group_by(YM) %>%
  summarise(tavg=round(mean(tavg, na.rm=T),2),
            prcp=(sum(prcp, na.rm=T)))

