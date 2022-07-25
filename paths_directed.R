
library(igraph)

priming.dat <- read.csv("priming_paths.csv")
dir_sw.g =readRDS( "dir_sw.rds")
dir_bar_50.g =readRDS( "dir_bar_50.rds")



priming.dat <- read.csv("ldt_baseline.csv")[,-c(4,5,6,8)]

priming.dat= na.omit(priming.dat)




#2 types of errors for shortest paths

length(get.shortest.paths(dir_bar_50.g,"art","grand")$vpath[[1]]) #Error Couldn't reach some vertices;no path: length  = 1
length(get.shortest.paths(dir_bar_50.g,"abdomen","tear")$vpath[[1]]) #Error Invalid vertex names;  word not in nw

c("art","grand") %in% V(dir_bar_50.g)$name # TRUE TRUE

#2 types of errors for  number of all shortest paths
length(get.all.shortest.paths(dir_bar_50.g,"art","grand")$res) #Error Couldn't reach some vertices; no path: length  = 0
length(get.all.shortest.paths(dir_bar_50.g,"abdomen","tear")$res) #Error Invalid vertex names; word not in nw



##Path lengths

priming.dat$Shortest_path_dir_sw_in <- NA
priming.dat$Shortest_path_dir_bar50_in <- NA
priming.dat$Shortest_path_dir_sw_out <- NA
priming.dat$Shortest_path_dir_bar50_out <- NA

priming.dat$All_shortest_paths_dir_sw_in <- NA
priming.dat$All_shortest_paths_dir_bar50_in <- NA
priming.dat$All_shortest_paths_dir_sw_out <- NA
priming.dat$All_shortest_paths_dir_bar50_out <- NA


for (i in 1:nrow(priming.dat)){
  w1 <- priming.dat$prime[i]
  w2 <- priming.dat$target[i]
  
  path_sw_in =ifelse(all(c(w1,w2) %in% V(dir_sw.g)$name),length(get.shortest.paths(dir_sw.g,w1,w2,mode ="in")$vpath[[1]]),NA)
  priming.dat$Shortest_path_dir_sw_in[i] = path_sw_in
  
  path_bar50_in =ifelse(all(c(w1,w2) %in% V(dir_bar_50.g)$name),length(get.shortest.paths(dir_bar_50.g,w1,w2,mode = "in")$vpath[[1]]),NA)
  priming.dat$Shortest_path_dir_bar50_in[i] = path_bar50_in
  
  path_sw_out =ifelse(all(c(w1,w2) %in% V(dir_sw.g)$name),length(get.shortest.paths(dir_sw.g,w1,w2,mode ="out")$vpath[[1]]),NA)
  priming.dat$Shortest_path_dir_sw_out[i] = path_sw_out
  
  path_bar50_out =ifelse(all(c(w1,w2) %in% V(dir_bar_50.g)$name),length(get.shortest.paths(dir_bar_50.g,w1,w2,mode = "out")$vpath[[1]]),NA)
  priming.dat$Shortest_path_dir_bar50_out[i] = path_bar50_out
  
  all_path_sw_in =ifelse(all(c(w1,w2) %in% V(dir_sw.g)$name),length(get.all.shortest.paths(dir_sw.g,w1,w2,mode = "in")$res),NA)
  all_path_bar50_in = ifelse(all(c(w1,w2) %in% V(dir_bar_50.g)$name),length(get.all.shortest.paths(dir_bar_50.g,w1,w2,mode= "in")$res),NA)
  
  all_path_sw_out =ifelse(all(c(w1,w2) %in% V(dir_sw.g)$name),length(get.all.shortest.paths(dir_sw.g,w1,w2,mode = "out")$res),NA)
  all_path_bar50_out = ifelse(all(c(w1,w2) %in% V(dir_bar_50.g)$name),length(get.all.shortest.paths(dir_bar_50.g,w1,w2,mode= "out")$res),NA)
  
  
  priming.dat$All_shortest_paths_dir_sw_in[i] = all_path_sw_in
  priming.dat$All_shortest_paths_dir_bar50_in[i] = all_path_bar50_in
  priming.dat$All_shortest_paths_dir_sw_out[i] = all_path_sw_out
  priming.dat$All_shortest_paths_dir_bar50_out[i] = all_path_bar50_out

}

 write.csv(priming.dat,"priming_paths_dir.csv",row.names=FALSE)

 priming.dat <- read.csv("priming_paths_dir.csv")
 
 #lm1 <- lm(Ztarget.RT~ (.)^2,priming.dat)
 
 corr = cor(na.omit(priming.dat[c(3,14:21)]))
 
 ggcorrplot(corr,hc.order = TRUE,
            type = "lower",
            outline.color = "white",lab= TRUE,lab_size= 3)
 
 
   
 paths_dir_sw <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                           "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                           "All_shortest_paths_dir_sw_in", "Shortest_path_dir_sw_in","All_shortest_paths_dir_sw_out", "Shortest_path_dir_sw_out")]
 
 paths_dir_bar50 <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                              "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                              "All_shortest_paths_dir_bar50_in", "Shortest_path_dir_bar50_in","All_shortest_paths_dir_bar50_out", "Shortest_path_dir_bar50_out")] 

 
 ##r between Ztarget.RT with All_shortest_paths_in, Shortest_path_in, All_shortest_paths_out, Shortest_path_out
 
 rcorr(as.matrix(paths_dir_sw)) #0.05,0.03,0.02,-0.02 n =1100
 rcorr(as.matrix(paths_dir_bar50)) #-0.02,0.04,-0.08,-0.01 n =1022
