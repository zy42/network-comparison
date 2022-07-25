library(tidyverse)
library(igraph)
library(stringr)
library(rvest)
library(LSAfun)
library(dbplyr)
library(lsa)

load("baroni.rda")


sw.dat <- read.csv("2018_strengthR123.csv")
bar_edges <- readRDS("bar_100_df.rds")



#lowercase

sw.dat$cue <- tolower(sw.dat$cue)
sw.dat$response <- tolower(sw.dat$response)


##keep sw responses with R123 >1 
sw.dat <- filter(sw.dat,sw.dat$R123 >1)
sw_edges <- data.frame(word1 = sw.dat$cue,word2 = sw.dat$response, weight= sw.dat$R123.Strength) ##309261 edges

#filter away cues not present in baroni

na_cues = read.csv("NA_cues.csv")

sw_edges <- filter(sw_edges, !word1 %in% na_cues$cues) #308744 edges

length(unique(sw_edges$word1)) #9164 unique cues

##filter cue == response self loop in sw network: 

sw_edges <- filter(sw_edges,word1 !=word2) ##169 self loops

##remove duplicate edges in sw network: 

sw.removed1 <- sw_edges[duplicated(sw_edges),] #38 removed (exact duplicate (EACH cell content is exactly the same), after upper case transformed to lower)
sw_edges <- unique(sw_edges) ##308537 edges remaining


##no. of duplicates that differ in direction


sw_duplicates <- sw_edges %>%  #duplicates that differ in directions: 23935 edges
  group_by(grp = paste(pmax(word1, word2), pmin(word1, word2), sep = "_")) %>%
  arrange(desc(weight)) %>% 
  # retain the pair with stronger R123
  summarise(n = n())%>% 
  filter(n >1)



##directed sw construction

directed_sw.g <- graph_from_data_frame(d = sw_edges,directed = TRUE)
is_weighted(directed_sw.g)
saveRDS(directed_sw.g, file = "dir_sw.rds")

#filter duplicates 
bar.removed <- bar_edges[duplicated(bar_edges),] #0 exact duplicates

##filter away self-loops
bar_edges <- filter(bar_edges,word1 !=word2) ##0 self loops

bar_duplicates <- bar_edges %>%  #pairs that differ in directions: 22690 edges
  group_by(grp = paste(pmax(word1, word2), pmin(word1, word2), sep = "_")) %>%
  arrange(desc(weight)) %>% 
  # retain the pair with stronger R123
  summarise(n = n())%>% 
  filter(n >1)

##directed bar_50 construction



##construct Baroni 50 network with the same total no. of edges = 284 721 edges
bar_edges <-   
  # retain top 50 edges of each cue
  slice_head(bar_edges %>% group_by(grp = word1) %>%
               arrange(desc(weight)), n=50) %>% 
  ungroup()%>%
  dplyr::select(-grp)

### each cue appears at least once

bar_edges_save1 <- bar_edges %>%
  group_by(grp = word1) %>%
  arrange(desc(weight)) %>% 
  # retain the pair with stronger R123
  slice(n = 1) %>% 
  ungroup()%>%
  dplyr::select(-grp)

bar_edges <- rbind(bar_edges_save1,setdiff(bar_edges,bar_edges_save1) %>% 
                         top_n(n= nrow(sw_edges) - nrow(bar_edges_save1), wt =weight))

bar_edges <- bar_edges[order(bar_edges$word1),] #sort by alphabetical order of word1






length(unique(bar_edges$word1)) #9164
length(unique(sw_edges$word1)) #9164 


directed_bar50.g<- graph_from_data_frame(d = bar_edges,directed = TRUE)
saveRDS(directed_bar50.g, file = "dir_bar50.rds")




