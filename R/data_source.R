# OpenFlights Data
# Tutorial: https://franciscorequena.com/post/exploring-world-airline-network/
# This code is completely from the tutorial -------------------------------

# Libraries ---------------------------------------------------------------
install.packages("pacman")

pacman::p_load(tidygraph, maps, ggraph, igraph, tidyverse, patchwork, ggthemes, lubridate, hms)

# Data --------------------------------------------------------------------

worldmap <- borders("world", colour="#efede1", fill="#efede1") 

df <- read.csv('https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat',header = FALSE,
               stringsAsFactors = FALSE,
               col.names = c('airline', 'airline_id', 'src', 'src_id', 'dest', 'dest_id', 'codeshare','stops',  'equip'))[,c(3,5)]

# Data with airport information

# https://openflights.org/data.html#airport

df2 <- read.csv('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat',header = FALSE,
                stringsAsFactors = FALSE)[,c(2,3,5,7,8,12)]

colnames(df2) <- c('name','city', 'code', 'lat', 'long', 'location')

# Clean data

df_airport <- df2 %>% 
  filter(!str_detect(code, fixed("\\N"))) %>%
  filter(!str_detect(location, fixed("\\N"))) %>%
  as_tibble()

tmp_loc <- str_split(df_airport$location, '/')
df_airport$location <- map_chr(tmp_loc, function(x) x[[1]])
df_airport <- df_airport %>% mutate(location = as.factor(location))

df_routes <-
  df %>% 
  filter(!str_detect(src, fixed("\\N")) & !str_detect(dest, fixed("\\N"))) %>%
  filter(!src == dest) %>%
  group_by(src, dest) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  as_tibble()

# Convert dataframe (df_routes) to tbl_graph object (df_graph)

df_graph <- as_tbl_graph(df_routes,directed = FALSE) %>% activate(edges) %>%
  filter(!edge_is_multiple()) %>% activate(nodes) %>%
  mutate(n_degree = centrality_degree(),
         betweenness = centrality_betweenness(),
         community = group_walktrap(),
         n_triangles = local_triangles(),
         clust = local_transitivity()) %>%
  left_join(df_airport, by = c('name' = 'code')) %>%
  filter(!is.na(lat) & !is.na(long))

df_nodes <- df_graph %>% activate(nodes) %>% as_tibble()

df_routes_def <- df_routes %>% 
  left_join(df_airport, by = c('src' = 'code')) %>%
  rename(long_src = long, lat_src = lat) %>%
  left_join(df_airport, by = c('dest' = 'code')) %>%
  rename(long_dest = long, lat_dest = lat) %>%
  left_join(df_nodes, by = c('src' = 'name')) %>%
  select(-lat, -long)

write_csv(df_routes_def, here::here("data", "df_routes_def.csv"))
