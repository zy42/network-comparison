library(tidyverse)
library(igraph)
library(olsrr)
library(Hmisc)
library(ggcorrplot)


sw.g <- readRDS("sw.rds")


bar_50.g <- readRDS("bar_50.rds")
bar_50_free.g <- readRDS("bar_50_free.rds")

bar_100.g <- readRDS("bar_100.rds")
bar_100_free.g <- readRDS("bar_100_free.rds")



priming.dat <- read.csv("ldt_baseline.csv")[,-c(4,5,6,8)]

priming.dat= na.omit(priming.dat)


priming.dat$Shortest_path_sw <- NA
priming.dat$Shortest_path_bar50 <- NA
priming.dat$Shortest_path_bar50_f <- NA
priming.dat$Shortest_path_bar100 <- NA
priming.dat$Shortest_path_bar100_f <- NA


priming.dat$All_shortest_paths_sw <- NA
priming.dat$All_shortest_paths_bar50 <- NA
priming.dat$All_shortest_paths_bar50_f <- NA
priming.dat$All_shortest_paths_bar100 <- NA
priming.dat$All_shortest_paths_bar100_f <- NA


#2 types of errors for shortest paths

length(get.shortest.paths(bar_50.g,"art","grand")$vpath[[1]]) #no path: length  = 1
length(get.shortest.paths(bar_50.g,"abdomen","tear")$vpath[[1]]) #word not in nw

c("art","grand") %in% V(bar_50.g)$name # TRUE TRUE

#2 types of errors for  number of all shortest paths
length(get.all.shortest.paths(bar_50.g,"art","grand")$res) #no path: length  = 0
length(get.all.shortest.paths(bar_50.g,"abdomen","tear")$res) #word not in nw


for (i in 1:nrow(priming.dat)){
  w1 <- priming.dat$prime[i]
  w2 <- priming.dat$target[i]
  
  path_sw =ifelse(all(c(w1,w2) %in% V(sw.g)$name),length(get.shortest.paths(sw.g,w1,w2)$vpath[[1]]),NA)
  priming.dat$Shortest_path_sw[i] = path_sw
  
  path_bar50 =ifelse(all(c(w1,w2) %in% V(bar_50.g)$name),length(get.shortest.paths(bar_50.g,w1,w2)$vpath[[1]]),NA)
  priming.dat$Shortest_path_bar50[i] = path_bar50
  
  path_bar50_f =ifelse(all(c(w1,w2) %in% V(bar_50_free.g)$name),length(get.shortest.paths(bar_50_free.g,w1,w2)$vpath[[1]]),NA)
  priming.dat$Shortest_path_bar50_f[i] = path_bar50_f
  
  path_bar100 =ifelse(all(c(w1,w2) %in% V(bar_100.g)$name),length(get.shortest.paths(bar_100.g,w1,w2)$vpath[[1]]),NA)
  priming.dat$Shortest_path_bar100[i] = path_bar100
  
  path_bar100_f =ifelse(all(c(w1,w2) %in% V(bar_100_free.g)$name),length(get.shortest.paths(bar_100_free.g,w1,w2)$vpath[[1]]),NA)
  priming.dat$Shortest_path_bar100_f[i] = path_bar100_f
  
  
  all_path_sw =ifelse(all(c(w1,w2) %in% V(sw.g)$name),length(get.all.shortest.paths(sw.g,w1,w2)$res),NA)
  all_path_bar50 = ifelse(all(c(w1,w2) %in% V(bar_50.g)$name),length(get.all.shortest.paths(bar_50.g,w1,w2)$res),NA)
  all_path_bar50_f = ifelse(all(c(w1,w2) %in% V(bar_50_free.g)$name),length(get.all.shortest.paths(bar_50_free.g,w1,w2)$res),NA)
  all_path_bar100 = ifelse(all(c(w1,w2) %in% V(bar_100.g)$name),length(get.all.shortest.paths(bar_100.g,w1,w2)$res),NA)
  all_path_bar100_f = ifelse(all(c(w1,w2) %in% V(bar_100_free.g)$name),length(get.all.shortest.paths(bar_100_free.g,w1,w2)$res),NA)
  
  
  priming.dat$All_shortest_paths_sw[i] = all_path_sw
  priming.dat$All_shortest_paths_bar50[i] = all_path_bar50
  priming.dat$All_shortest_paths_bar50_f[i] = all_path_bar50_f
  priming.dat$All_shortest_paths_bar100[i] = all_path_bar100
  priming.dat$All_shortest_paths_bar100_f[i] = all_path_bar100_f
}




write.csv(priming.dat,"priming_paths.csv")

##Baroni part


priming.dat <- read.csv("priming_paths.csv")


##scatterplot 
ggplot(priming.dat[c(3,14:23)]%>%pivot_longer(cols = colnames(priming.dat[c(14:23)]),
              names_to = "Type", values_to = "Length",values_drop_na = TRUE)
       ,aes(y= Ztarget.RT,x= Length))+
  geom_point(size=0.5)+
  xlim(0, 20)+
  facet_wrap(~Type,ncol= 2)

##scatterpot of outlier points

ggplot(priming.dat[c(3,14:23)]%>%pivot_longer(cols = colnames(priming.dat[c(14:23)]),
                                              names_to = "Type", values_to = "Length",values_drop_na = TRUE)
       ,aes(y= Ztarget.RT,x= Length))+
  geom_point()+
  xlim(20, 100)+
  facet_wrap(~Type)

corr1 <- cor(na.omit(priming.dat[c("Ztarget.RT",
                           "All_shortest_paths_sw", "Shortest_path_sw",
                           "All_shortest_paths_bar50", "Shortest_path_bar50",
                           "All_shortest_paths_bar50_f", "Shortest_path_bar50_f",
                           "All_shortest_paths_bar100", "Shortest_path_bar100",
                           "All_shortest_paths_bar100_f", "Shortest_path_bar100_f")]))

##corr plot of path lengths

ggcorrplot(corr1,hc.order = TRUE,
           type = "lower",
           outline.color = "white",lab= TRUE,lab_size= 3)



##data split 
paths_sw <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                               "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                            "All_shortest_paths_sw", "Shortest_path_sw")]

paths_bar50 <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                          "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                          "All_shortest_paths_bar50", "Shortest_path_bar50")]
paths_bar50_f <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                              "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                              "All_shortest_paths_bar50_f", "Shortest_path_bar50_f")]

paths_bar100 <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                              "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                              "All_shortest_paths_bar100", "Shortest_path_bar100")]
paths_bar100_f <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                               "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                               "All_shortest_paths_bar100_f", "Shortest_path_bar100_f")]



##AIC
