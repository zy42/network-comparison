library(tidyverse)
library(igraph)
library(stringr)
library(rvest)
library(LSAfun)
library(dbplyr)
library(lsa)
load("baroni.rda")


sw.dat <- read.csv("2018_strengthR123.csv")



#lowercase

sw.dat$cue <- tolower(sw.dat$cue)
sw.dat$response <- tolower(sw.dat$response)


##keep sw responses with R123 >1 
sw.dat <- filter(sw.dat,sw.dat$R123 >1)
sw_edges <- data.frame(word1 = sw.dat$cue,word2 = sw.dat$response, weight= sw.dat$R123.Strength) ##309261 edges

#no. of cue words

cues = unique(sw.dat$cue) ##9181 unique cues



#create bar_100.nw : 100 neighbors for each cue for pruning


bar_edges <- data.frame(matrix(nrow =0,ncol =3))
colnames(bar_edges) = c("word1","word2","weight")


bar_edges <- readRDS("bar_100_df.rds")


for (i in 1:length(cues)){
  w1 = cues[i]
  w1_vec = rep(w1,each =100)
  nns <- neighbors(w1,101,baroni)[-1] #first neighbor is the word itself, exclude first neighbor
  if (is.na(nns[1])){  #cue word is not in CBOW DSM
    w2_vector <- rep(NA,100)
    sim_vector <- rep(NA,100)
  }
  else{
    w2_vector <- names(nns)
    sim_vector <- unname(nns)
  }
  combined_vector <- cbind(word1 =w1_vec,word2=w2_vector,weight=sim_vector)
  
  bar_edges <- rbind(bar_edges, combined_vector)
}
 

saveRDS(bar_edges,"bar_100_df.rds")

bar_edges$weight = as.numeric(bar_edges$weight)


##filter cues not present in baroni DSM

min(bar_edges$weight)
bar_na_id <- which(is.na(bar_edges$weight))#17 cues removed

na_cues <- unique(bar_edges$word1[bar_na_id])
write_csv(data.frame(cues =na_cues),"NA_cues.csv")

#updated bar_edges (remove cues not present in baroni dsm)

na_cues = read.csv("NA_cues.csv")
bar_edges <- filter(bar_edges,!word1 %in% na_cues$cues)#458200 obs vs 459050 obs
sw_edges <- filter(sw_edges, !sw_edges$word1 %in% na_cues$cues) #308744 obs vs 309261 obs
write.csv(sw_edges,"sw_edges.csv")


cues = unique(sw_edges$word1) ##9164 unique cues, omitting cues not present in bar



##remove duplicate edges in sw network: 


sw.removed1 <- sw_edges[duplicated(sw_edges),] #38 removed (exact duplicate, after upper case transformed to lower)
sw_edges2 <- unique(sw_edges) ##308706 edges remaining


sw.big <- sw_edges2

sw_edges <- sw_edges2 %>%  #23985 duplicates that differ in directions: 284 721 edges reamined
  group_by(grp = paste(pmax(word1, word2), pmin(word1, word2), sep = "_")) %>%
  arrange( desc(weight) ) %>% 
  # retain the pair with stronger R123
  slice(1) %>% 
  ungroup()%>%
  select(-grp)

write.csv(sw_edges,"sw_edges.csv")

sw_edges <- read.csv("sw_edges.csv")[2:4]

sw_edges.summary <- sw_edges %>% group_by(word1) %>% summarise(n = n()) %>% ungroup()
max(sw_edges.summary$n) #max no. of edges in sw network:53
min(sw_edges.summary$n) #max no. of edges in sw network:10

#undirected SW network construction

sw.g <- graph_from_data_frame(d = sw_edges,directed = FALSE)
is_weighted(sw.g)
saveRDS(sw.g, file = "sw.rds")



##construct Baroni 100 network with the same total no. of edges = 284 721 edges

#filter duplicates 
bar.removed <- bar_edges[duplicated(bar_edges),] #0 duplicates


bar_duplicates <- bar_edges %>%  #duplicates that differ in directions: 22690 edges
  group_by(grp = paste(pmax(word1, word2), pmin(word1, word2), sep = "_")) %>%
  arrange(desc(weight)) %>% 
  # retain the pair with stronger R123
  summarise(n = n())%>% 
  filter(n >1)


##select top 284 721 edges from bar_edges, with the condition that each cue appears at least once
##at least one edge for each distinct cue
bar_edges_save1 <- bar_edges %>%
  group_by(grp = word1) %>%
  arrange(desc(weight)) %>% 
  # retain the pair with stronger R123
  slice(n = 1) %>% 
  ungroup()%>%
  select(-grp)

bar_edges_100 <- rbind(bar_edges_save1,setdiff(bar_edges,bar_edges_save1) %>% 
                      top_n(n= nrow(sw_edges) - nrow(bar_edges_save1), wt =weight))

bar_edges_100 <- bar_edges_100[order(bar_edges_100$word1),] #sort by alphabetical order of word1


length(unique(bar_edges_100$word1)) #9164
length(unique(sw_edges$word1)) #9164 


#undirected network construction


bar_100.g<- graph_from_data_frame(d = bar_edges_100,directed = FALSE)
nrow(as_data_frame(simplify(bar_100.g))) ##10774
saveRDS(bar_100.g, file = "bar_100.rds")


########
##select top 284 721 edges from bar_edges, no constraints
bar_edges_100_free <- bar_edges %>%
  top_n(n= nrow(sw_edges), wt =weight)


length(unique(bar_edges_100_free$word1)) #8903
length(unique(sw_edges$word1)) #9164 


bar_100_free.g<- graph_from_data_frame(d = bar_edges_100_free,directed = FALSE)

nrow(as_data_frame(simplify(bar_100_free.g))) ##10780
saveRDS(bar_100_free.g, file = "bar_100_free.rds")


##construct Baroni 50 network with the same total no. of edges = 284 721 edges
bar_edges_50 <-   
  # retain top 50
  slice_head(bar_edges %>% group_by(grp = word1) %>%
               arrange(desc(weight)), n=50) %>% 
  ungroup()%>%
  select(-grp)


##select top 284 721 edges from bar_edges, with the condition that each cue appears at least once
##at least one edge for each distinct cue


bar_edges_50 <- rbind(bar_edges_save1,setdiff(bar_edges_50,bar_edges_save1) %>% 
                         top_n(n= nrow(sw_edges) - nrow(bar_edges_save1), wt =weight))

bar_edges_50 <- bar_edges_50[order(bar_edges_50$word1),] #sort by alphabetical order of word1


length(unique(bar_edges_50$word1)) #9164
length(unique(sw_edges$word1)) #9164 


#undirected network construction


bar_50.g<- graph_from_data_frame(d = bar_edges_50,directed = FALSE)
nrow(sw_edges) - nrow(as_data_frame(simplify(bar_50.g))) ##no. of duplicates (loop) = unique edges: 11806
saveRDS(bar_50.g, file = "bar_50.rds")


########
##select top 284 721 edges from bar_edges, no constraints
bar_edges_50_free <- slice_head(bar_edges %>%
                group_by(grp = word1) %>%
  arrange(desc(weight)), n=50) %>% 
  ungroup()%>%
  select(-grp) %>%
  top_n(n= nrow(sw_edges), wt =weight)



length(unique(bar_edges_50_free$word1)) #9087
length(unique(sw_edges$word1)) #9164 


bar_50_free.g<- graph_from_data_frame(d = bar_edges_50_free,directed = FALSE)

nrow(as_data_frame(simplify(bar_50_free.g))) ##11809
saveRDS(bar_50_free.g, file = "bar_50_free.rds")
