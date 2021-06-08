library(tidyverse)
library(sf)
library(cartogram)
library(maps)

sf_tpe <-
  st_read(dsn = "data/Lab10/109年12月行政區人口統計_村里_臺北市_SHP/", layer = "109年12月行政區人口統計_村里", quiet = T) %>%
  mutate(across(where(is.character), ~iconv(., from = "BIG5", to = "UTF8"))) %>%
  rename_with(~str_to_lower(.), everything()) %>% 
  mutate(across(where(is.double), ~if_else(is.na(.),as.double(0),.))) %>%
  st_set_crs(3826) %>% st_transform(4326) %>% filter(str_detect(county, "臺北市")) %>%
  mutate(village = if_else(str_detect(village,"糖"),"糖廍里",village))

sf_taiwan <-
  st_read(dsn = "data/Lab10/直轄市縣市界線/", layer = "COUNTY_MOI_1090820", quiet = T) %>%
  rename_with(~str_to_lower(.), everything()) %>% st_transform(4326)

sf_taiwan_simplify <- sf_taiwan %>% st_transform(3826) %>% st_simplify(dTolerance = 100) %>% st_transform(4326)

sf_taiwan_simplify %>% st_transform(4326) %>% st_bbox()
sf_taiwan_simplify %>% st_transform(4326) %>% #st_crop(xmin = 120, xmax = 122, ymin = 22, ymax = 25.5) %>%
  ggplot() + geom_sf()

sf_taiwan_simplify %>% st_transform(4326) %>% #st_crop(xmin = 120, xmax = 122, ymin = 22, ymax = 25.5) %>%
  st_transform(3826) %>%
  ggplot() + geom_sf()

sf_taiwan_simplify %>% st_bbox()


sf_taiwan_simplify %>% ggplot() + geom_sf()
sf_taiwan_simplify %>% st_transform(3826) %>% ggplot() + geom_sf()

sf_mrt_tpe <-
  st_read(dsn = "data/Lab10/臺北都會區大眾捷運系統車站點位圖/", layer = "TpeMrtStations_TWD97_Unicode", quiet = T) %>%
  rename_with(~str_to_lower(.), everything()) %>% st_transform(4326)

sf_tpe %>% mutate(p_density = p_cnt/(as.double(st_area(.))/1000000)) %>%
  ggplot() + geom_sf(aes(fill = p_density), color = NA) + 
  geom_sf(data = sf_mrt_tpe, size = 0.5, color = "black") +
  scale_fill_gradient(low = "white", high = "purple")

sf_mrt_tpe %>% ggplot() + geom_sf()

df_vote_tpe <- readxl::read_excel("data/Lab10/總統-各投票所得票明細及概況(Excel檔)/總統-A05-3-候選人得票數一覽表-各村里(臺北市).xls", range = "A5:M474")

df_vote_tpe_clean <- df_vote_tpe %>% `colnames<-`(c("town", "village", "vote_sung", "vote_han", "vote_tsai", "vote_effect", "vote_neffect", "vote_total", "vote_nvote", "vote_sent", "vote_left", "vote_qual", "vote_per")) %>%
  fill(town, .direction = "down") %>%
  filter(!is.na(village)) %>% 
  mutate(across(matches("vote_"), ~ as.double(str_remove_all(., ",")))) %>%
  mutate(town = str_remove_all(town, "　"), village = str_remove_all(village, "　")) %>%
  mutate(vote_per = vote_per/100) %>%
  mutate(vote_per_tsai = vote_tsai/vote_effect, 
         vote_per_han = vote_han/vote_effect,
         vote_per_sung = vote_sung/vote_effect)

df_vote_taiwan <- readxl::read_excel("data/Lab10/總統-各投票所得票明細及概況(Excel檔)/總統-A05-1-候選人得票數一覽表(中　央).xls", range = "A5:L28")

df_vote_taiwan_clean <- df_vote_taiwan %>% `colnames<-`(c("county", "vote_sung", "vote_han", "vote_tsai", "vote_effect", "vote_neffect", "vote_total", "vote_nvote", "vote_sent", "vote_left", "vote_qual", "vote_per")) %>%
  filter(str_detect(county, "縣|市")) %>% 
  mutate(across(matches("vote_"), ~ as.double(str_remove_all(., ",")))) %>%
  mutate(county = str_remove_all(county, "　")) %>%
  mutate(vote_per = vote_per/100) %>%
  mutate(vote_per_tsai = vote_tsai/vote_effect, 
         vote_per_han = vote_han/vote_effect,
         vote_per_sung = vote_sung/vote_effect)

# R time: Choropleth
sf_tpe_vote <- sf_tpe %>% left_join(df_vote_tpe_clean %>% select(1:2, matches("tsai|han|sung|vote_qual")), by = c("town", "village"))
sf_taiwan_vote <- sf_taiwan_simplify %>% 
  st_crop(xmin = 120, xmax = 122, ymin = 22, ymax = 25.5) %>% select(county = countyname, countyeng) %>% left_join(df_vote_taiwan_clean %>% select(1, matches("tsai|han|sung|vote_qual")), by = c("county"))

plot_tsai <- sf_tpe_vote %>% ggplot() + geom_sf(aes(fill = vote_per_tsai), color = NA) +
  scale_fill_gradient(low = "white", high = "#1B9431", limits=c(0,0.8)) +
  ggthemes::theme_map() +
  theme(legend.position = "bottom")

plot_han <- sf_tpe_vote %>% ggplot() + geom_sf(aes(fill = vote_per_han), color = NA) +
  scale_fill_gradient(low = "white", high = "#000095", limits=c(0,0.8)) + 
  ggthemes::theme_map() +
  theme(legend.position = "bottom")

gridExtra::grid.arrange(plot_tsai, plot_han,nrow=1)

sf_taiwan_vote %>% ggplot() + geom_sf(aes(fill = vote_per_tsai), color = NA) +
  scale_fill_gradient(low = "white", high = "#1B9431", limits=c(0,0.8)) +
  ggthemes::theme_map() +
  theme(legend.position = "bottom")

sf_taiwan_vote %>% ggplot() + geom_sf(aes(fill = vote_per_han), color = NA) +
  scale_fill_gradient(low = "white", high = "#000095", limits=c(0,0.8)) + 
  ggthemes::theme_map() +
  theme(legend.position = "bottom")

gridExtra::grid.arrange(plot_tsai, plot_han,nrow=1)


## Dot Distribution Map/Bubble Map
library(maps)
### get data
df_city_taiwan <- world.cities %>% 
  filter(country.etc=="Taiwan") %>% as_tibble()

df_city_taiwan %>% head(3)

### plotting: each point equals a city
sf_taiwan_simplify %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot() +
  geom_sf(size = 0.2) +
  geom_point(data=df_city_taiwan, aes(x=long, y=lat)) +
  theme_void()

### plotting: city population size matters
sf_taiwan_simplify %>%
  st_crop(xmin=119,xmax=123,ymin=20,ymax=26) %>%
  ggplot() +
  geom_sf(size = 0.2) +
  geom_point(data=df_city_taiwan, aes(x=long, y=lat, size = pop)) +
  theme_void()

## Cartogram
### use cartogram package

sf_tpe_vote_cartogram <- cartogram_cont(sf_tpe_vote %>% st_transform(3826), "vote_qual", itermax=5)
sf_taiwan_vote_cartogram <- cartogram_cont(sf_taiwan_vote %>% st_transform(3826), "vote_qual", itermax=5)

### sf_tpe_vote_cartogram
sf_tpe_vote_cartogram %>% ggplot() + geom_sf(aes(fill = vote_per_tsai)) +
  scale_fill_gradient(low = "white", high = "#1B9431", limits=c(0,0.8)) +
  theme(legend.position = "bottom")

sf_taiwan_vote_cartogram %>% ggplot() + geom_sf(aes(fill = vote_per_tsai)) +
  scale_fill_gradient(low = "white", high = "#1B9431", limits=c(0,0.8)) +
  theme(legend.position = "bottom")

