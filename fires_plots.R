library(tidyverse)
library(sf)
library(geojsonsf)
require(scales)
library(magrittr)

##############################


sf::sf_use_s2(FALSE)
russia2 <- geojson_sf("fires/boundary-polygon-land-lvl4.geojson", crs = "WGS84")
russia2 <- russia2[-c(16),]

points <- read_csv("fires/firepoints_inside.csv")
points %<>% filter(inside == 1)


points_pivot <- points %>% 
  group_by(type_name, year) %>% 
  count()

bp <- ggplot(data = points_pivot,
             aes(x = year, y = n, fill = type_name))+
  geom_bar(stat = "identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Dark2")+
  xlab("Год")+
  ylab("Количество возгораний")+
  scale_y_continuous(labels = comma)+
  labs(fill = "Тип пожара")

firepoints <- data.frame(dt = points$dt,
                         type_name = points$type_name,
                         type_id = points$type_id,
                         year = points$year,
                         lon = points$lon,
                         lat = points$lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


russia2$pt_count <- lengths(st_intersects(russia2, firepoints))
russia2$count_uncontrolled <- lengths(st_intersects(russia2, firepoints %>% 
                                                      filter(type_id == 1)))
russia2$count_controlled <- lengths(st_intersects(russia2, firepoints%>% 
                                                    filter(type_id == 5)))
russia2$count_forest <- lengths(st_intersects(russia2, firepoints%>% 
                                                filter(type_id == 3)))
russia2$count_nature <- lengths(st_intersects(russia2, firepoints%>% 
                                                filter(type_id == 4)))
russia2$count_peat <- lengths(st_intersects(russia2, firepoints%>% 
                                              filter(type_id == 2)))

russia2$space <- st_area(russia2)
russia2$uncontrolled_per_km <- as.numeric(russia2$count_uncontrolled/russia2$space*1000000)*100
russia2$controlled_per_km <- as.numeric(russia2$count_controlled/russia2$space*1000000)*100
russia2$forest_per_km <- as.numeric(russia2$count_forest/russia2$space*1000000)*100
russia2$nature_per_km <- as.numeric(russia2$count_nature/russia2$space*1000000)*100
russia2$peat_per_km <- as.numeric(russia2$count_peat/russia2$space*1000000)*100

firepoints1 <- firepoints %>% filter(type_id == 1)
firepoints2 <- firepoints %>% filter(type_id == 2)
firepoints3 <- firepoints %>% filter(type_id == 3)
firepoints4 <- firepoints %>% filter(type_id == 4)
firepoints5 <- firepoints %>% filter(type_id == 5)

unp_l <- russia2 %>% arrange(desc(uncontrolled_per_km)) %>% slice(1:5)
cp_l <- russia2 %>% arrange(desc(controlled_per_km)) %>% slice(1:5)
np_l <- russia2 %>% arrange(desc(nature_per_km)) %>% slice(1:5)
fp_l <- russia2 %>% arrange(desc(forest_per_km)) %>% slice(1:5)
pp_l <- russia2 %>% arrange(desc(peat_per_km)) %>% slice(1:5)

#####fill and points###############

unp <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = uncontrolled_per_km))+
  geom_sf(data = firepoints1,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  ggrepel::geom_label_repel(
    data = unp_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1)+
  theme_void()+
  ggtitle("Неконтролируемый пал", 
          subtitle = "В среднем с 2012 по 2021 год, одна точка – координаты одного возгорания")+
  labs(fill = 'Неконтролируемый пал на тыс. кв. км в год'
  )+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))



cp <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = controlled_per_km))+
  geom_sf(data = firepoints5,
          size=0.01,
          alpha= 0.1,
          color = "darkred")+
  ggrepel::geom_label_repel(
    data = cp_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1
  )+
  theme_void()+
  ggtitle("Контролируемый пал", 
          subtitle = "В среднем с 2012 по 2021 год, одна точка – координаты одного возгорания")+
  labs(fill = 'Контролируемый пал на тыс. кв. км')+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


fp <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = forest_per_km))+
  geom_sf(data = firepoints3,
          size=0.01,
          alpha= 0.1,
          color = "darkred")+
  ggrepel::geom_label_repel(
    data = fp_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1
                       
  )+
  ggtitle("Лесные пожары", 
          subtitle = "В среднем с 2012 по 2021 год, одна точка – координаты одного возгорания")+
  theme_void()+
  labs(fill = 'Лесные пожары на тыс. кв. км')+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))



np <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = nature_per_km))+
  geom_sf(data = firepoints %>% filter(type_id == 4),
          #aes(x=lon, y=lat),
          size=0.01,
          alpha= 0.1,
          color = "darkred")+
  ggrepel::geom_label_repel(
    data = np_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1
  )+
  ggtitle("Природные пожары", 
          subtitle = "В среднем с 2012 по 2021 год, одна точка – координаты одного возгорания")+
  theme_void()+
  labs(fill = 'Природные пожары на тыс. кв. км')+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


pp <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = peat_per_km))+
  geom_sf(data = firepoints %>% filter(type_id == 2),
          #aes(x=lon, y=lat),
          size=0.001,
          alpha= 0.1,
          color = "darkred")+
  ggrepel::geom_label_repel(
    data = pp_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1#, #labels = comma,
                       #breaks = c(0.01, 0.02, 0.03, 0.04)
  )+
  ggtitle("Торфяные пожары", 
          subtitle = "В среднем с 2012 по 2021 год, одна точка – координаты одного возгорания")+
  theme_void()+
  labs(fill = 'Торфяные пожары на тыс. кв. км')+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


##################fill no points#####################

unp_np <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = uncontrolled_per_km))+
  ggrepel::geom_label_repel(
    data = unp_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1)+
  theme_void()+
  ggtitle("Неконтролируемый пал", 
          subtitle = "В среднем с 2012 по 2021 год")+
  labs(fill = 'Неконтролируемый пал на тыс. кв. км в год'
  )+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))



cp_np <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = controlled_per_km))+
  ggrepel::geom_label_repel(
    data = cp_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1
  )+
  theme_void()+
  ggtitle("Контролируемый пал", 
          subtitle = "В среднем с 2012 по 2021 год")+
  labs(fill = 'Контролируемый пал на тыс. кв. км')+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


fp_np <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = forest_per_km))+
  ggrepel::geom_label_repel(
    data = fp_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1
                       
  )+
  ggtitle("Лесные пожары", 
          subtitle = "В среднем с 2012 по 2021 год")+
  theme_void()+
  labs(fill = 'Лесные пожары на тыс. кв. км')+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))



np_np <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = nature_per_km))+
  ggrepel::geom_label_repel(
    data = np_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1
  )+
  ggtitle("Природные пожары", 
          subtitle = "В среднем с 2012 по 2021 год")+
  theme_void()+
  labs(fill = 'Природные пожары на тыс. кв. км')+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


pp_np <- ggplot() +
  geom_sf(data = russia2,
          aes(fill = peat_per_km))+
  ggrepel::geom_label_repel(
    data = pp_l,
    aes(label = NAME_RU, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    box.padding = 1
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  scale_fill_distiller(palette = "Reds", direction = 1
  )+
  ggtitle("Торфяные пожары", 
          subtitle = "В среднем с 2012 по 2021 год")+
  theme_void()+
  labs(fill = 'Торфяные пожары на тыс. кв. км')+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))

##################points and lines##########################

quantile(points[points$type_id==1,]$lat, probs = c(0.8))

slon <- seq(20, 174, 1)
elon <- seq(21, 175, 1)
slat <- rep(56.25122, 155)
elat <- rep(56.25122, 155)

unp_lonline <- data.frame(slon, elon, slat, elat)

ls <- apply(unp_lonline, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls = Reduce(c, ls)


unp_line <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints1,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Неконтролируемый пал", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 80% возгораний за 10 лет начались ниже серой линии")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


quantile(points[points$type_id==1,]$lat)

slon <- seq(20, 174, 1)
elon <- seq(21, 175, 1)
elon_l <- seq(21, 165, 1)
slon_l <- seq(20, 164, 1)
slat_l <- rep(50.08364, 145)
elat_l <- rep(50.08364, 145)

unp_lonline_l <- data.frame(slon_l, elon_l, slat_l, elat_l)

ls_l <- apply(unp_lonline_l, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_l = Reduce(c, ls_l)

slat_u <- rep(55.92320, 155)
elat_u <- rep(55.92320, 155)

unp_lonline_u <- data.frame(slon, elon, slat_u, elat_u)

ls_u <- apply(unp_lonline_u, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_u = Reduce(c, ls_u)



unp_lines <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints1,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls_l,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  geom_sf(data = ls_u,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Неконтролируемый пал", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 50% возгораний за 10 лет начались между линиями")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))

quantile(points[points$type_id==5,]$lat, probs = c(0.8, 0.9))

slon <- seq(20, 179, 1)
elon <- seq(21, 180, 1)
slat <- rep(61.69575, 160)
elat <- rep(61.69575, 160)

unp_lonline <- data.frame(slon, elon, slat, elat)

ls <- apply(unp_lonline, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls = Reduce(c, ls)


cp_line <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints5,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Контролируемый пал", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 80% возгораний за 10 лет начались ниже серой линии")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


quantile(points[points$type_id==5,]$lat)

slon <- seq(20, 174, 1)
elon <- seq(21, 175, 1)
elon_l <- seq(21, 165, 1)
slon_l <- seq(20, 164, 1)
slat_l <- rep(51.49790, 145)
elat_l <- rep(51.49790, 145)

unp_lonline_l <- data.frame(slon_l, elon_l, slat_l, elat_l)

ls_l <- apply(unp_lonline_l, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_l = Reduce(c, ls_l)

slat_u <- rep(60.75951, 155)
elat_u <- rep(60.75951, 155)

unp_lonline_u <- data.frame(slon, elon, slat_u, elat_u)

ls_u <- apply(unp_lonline_u, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_u = Reduce(c, ls_u)


cp_lines <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints5,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls_l,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  geom_sf(data = ls_u,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Контролируемый пал", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 50% возгораний за 10 лет начались между линиями")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))

quantile(points[points$type_id==2,]$lat, probs = c(0.8, 0.9))

slon <- seq(20, 179, 1)
elon <- seq(21, 180, 1)
slat <- rep(65.35348, 160)
elat <- rep(65.35348, 160)

unp_lonline <- data.frame(slon, elon, slat, elat)

ls <- apply(unp_lonline, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls = Reduce(c, ls)

pp_line <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints2,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Торфяные пожары", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 80% возгораний за 10 лет начались ниже серой линии")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


quantile(points[points$type_id==2,]$lat)

slon <- seq(20, 179, 1)
elon <- seq(21, 180, 1)
elon_l <- seq(21, 165, 1)
slon_l <- seq(20, 164, 1)
slat_l <- rep(56.48753, 145)
elat_l <- rep(56.48753, 145)

unp_lonline_l <- data.frame(slon_l, elon_l, slat_l, elat_l)

ls_l <- apply(unp_lonline_l, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_l = Reduce(c, ls_l)

slat_u <- rep(62.61973 , 160)
elat_u <- rep(62.61973 , 160)

unp_lonline_u <- data.frame(slon, elon, slat_u, elat_u)

ls_u <- apply(unp_lonline_u, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_u = Reduce(c, ls_u)


pp_lines <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints2,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls_l,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  geom_sf(data = ls_u,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Торфяные пожары", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 50% возгораний за 10 лет начались между линиями")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))

quantile(points[points$type_id==3,]$lat, probs = c(0.8, 0.9))

slon <- seq(20, 179, 1)
elon <- seq(21, 180, 1)
slat <- rep(64.57379, 160)
elat <- rep(64.57379, 160)

unp_lonline <- data.frame(slon, elon, slat, elat)

ls <- apply(unp_lonline, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls = Reduce(c, ls)



fp_line <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints3,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Лесные пожары", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 80% возгораний за 10 лет начались ниже серой линии")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))

quantile(points[points$type_id==3,]$lon,
         probs = c(0, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 1))

slon <- rep(106.21387, 38)
elon <- rep(106.21387, 38)
slat <- seq(47, 84)
elat <- seq(48, 85)

unp_lonline <- data.frame(slon, elon, slat, elat)

ls <- apply(unp_lonline, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls = Reduce(c, ls)

fp_vline <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints3,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Лесные пожары", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 80% возгораний за 10 лет начались восточнее серой линии"
  )+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))



quantile(points[points$type_id==3,]$lat)

slon <- seq(20, 174, 1)
elon <- seq(21, 175, 1)
elon_l <- seq(21, 165, 1)
slon_l <- seq(20, 164, 1)
slat_l <- rep(57.23339, 145)
elat_l <- rep(57.23339, 145)

unp_lonline_l <- data.frame(slon_l, elon_l, slat_l, elat_l)

ls_l <- apply(unp_lonline_l, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_l = Reduce(c, ls_l)

slat_u <- rep(64.00535, 155)
elat_u <- rep(64.00535, 155)

unp_lonline_u <- data.frame(slon, elon, slat_u, elat_u)

ls_u <- apply(unp_lonline_u, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_u = Reduce(c, ls_u)


fp_lines <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints3,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls_l,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  geom_sf(data = ls_u,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Лесные пожары", 
          subtitle = 
  "Одна точка – координаты одного возгорания, 50% возгораний за 10 лет начались между линиями")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


quantile(points[points$type_id==4,]$lat, probs = c(0.8, 0.9))

slon <- seq(20, 179, 1)
elon <- seq(21, 180, 1)
slat <- rep(60.71855, 160)
elat <- rep(60.71855, 160)

unp_lonline <- data.frame(slon, elon, slat, elat)

ls <- apply(unp_lonline, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls = Reduce(c, ls)



np_line <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints5,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Природные пожары", 
          subtitle = 
  "Одна точка – координаты одного возгорания, 80% возгораний за 10 лет начались ниже серой линии")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))


quantile(points[points$type_id==4,]$lat)

quantile(points[points$type_id==4,]$lat, probs = c(0, 0.1, 0.15, 0.2, 0.25, 0.6, 0.65, 0.7, 0.75))

slon <- seq(20, 174, 1)
elon <- seq(21, 175, 1)
elon_l <- seq(26, 160, 1)
slon_l <- seq(25, 159, 1)
slat_l <- rep(48.11124, 135)
elat_l <- rep(48.11124, 135)

unp_lonline_l <- data.frame(slon_l, elon_l, slat_l, elat_l)

ls_l <- apply(unp_lonline_l, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_l = Reduce(c, ls_l)

slat_u <- rep(55.83093, 155)
elat_u <- rep(55.83093, 155)

unp_lonline_u <- data.frame(slon, elon, slat_u, elat_u)

ls_u <- apply(unp_lonline_u, 1, function(x) 
{
  v <- as.numeric(x)
  m <- matrix(v, nrow = 2)
  return(st_sfc(st_linestring(m), crs = 4326))
})
ls_u = Reduce(c, ls_u)


np_lines <- ggplot() +
  geom_sf(data = russia2,
          fill = "gray99")+
  geom_sf(data = firepoints5,
          size=0.01,
          alpha= 0.1,
          color = "darkred"
  )+
  geom_sf(data = ls_l,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  geom_sf(data = ls_u,
          lwd = 1.25,
          linetype = "42",
          color = "gray50"
  )+
  coord_sf(
    crs="+proj=aea +lat_1=50 +lat_2=70 +lat_0=56 +lon_0=100 +x_0=0 +y_0=0 +ellps=WGS84 
    +datum=WGS84 +units=m +no_defs")+
  theme_void()+
  ggtitle("Природные пожары", 
          subtitle = 
 "Одна точка – координаты одного возгорания, 50% возгораний за 10 лет начались между линиями")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.25,
                                  face = "bold",
                                  size = 20),
        plot.subtitle = element_text(hjust = 0.2875))











