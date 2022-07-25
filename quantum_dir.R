
library(igraph)
library(tidyverse)
dir_sw.g =readRDS( "dir_sw.rds")
dir_bar50.g =readRDS( "dir_bar50.rds")



priming.dat <- read.csv("ldt_baseline.csv")[,-c(4,5,6,8)]

priming.dat= na.omit(priming.dat)


if_edge <- function(target,associate,networkobj,direction){
  if (direction == "ti"){
    path = c(target,associate)
  }
  else if (direction == "it"){
    path = c(associate,target)
  }
  output= tryCatch(
    E(dir_sw.g,P =path,directed =TRUE)$weight,error = function(e) NA)
  return(output)
}



get_associates <- function(target,networkobj){
  if (!target %in% V(networkobj)$name){
    output = data.frame(source = target,associate =NA,S_ti=NA,S_it=NA)
  } else {
  edges_from <- E(networkobj)[from(target)]
  edges_to <- E(networkobj)[to(target)]
  if (length(edges_from) > 0){
    edges_from = sapply(strsplit(attr(edges_from,"vnames"),"\\|"),"[[",2)
  } else{
    edges_from = NA
  }
  if (length(edges_to) > 0) {
   edges_to = sapply(strsplit(attr(edges_to,"vnames"),"\\|"),"[[",1) 
  } else {
    edges_to = NA
  }
  associates = unique(na.omit(c(edges_from,edges_to)))
  
  output <- data.frame(source = rep(target,length(associates)),associate = associates)
  
  associate_ti =  apply(output,1,function(x) if_edge(x[1],x[2],networkobj,"ti")) #target to associate
  associate_it =  apply(output,1,function(x) if_edge(x[1],x[2],networkobj,"it")) #target to associate
  output$S_ti = associate_ti
  output$S_it = associate_it
  }
  return(output)
}



get_connectivity <- function(associate.dat,networkobj){
  if (all(is.na(associate.dat$associate))){
    output = data.frame(source= associate.dat$source,assoc1=NA,assoc2=NA,S_ij=NA)
  } else{
  temp <- expand.grid(assoc1= associate.dat$associate,assoc2= associate.dat$associate)
  temp <- filter(temp,temp$assoc1 != temp$assoc2)
  S_ij <- apply(temp,1,function(x) if_edge(x[1],x[2],networkobj,"ti"))
  output = cbind(source = rep(associate.dat$source[1],nrow(temp)),temp,S_ij)
  }
  return(na.omit(output))
}




priming_words <- unique(c(priming.dat$prime,priming.dat$target))

strength.dat <- data.frame(matrix(nrow =0,ncol =4))
colnames(strength.dat) = c("source","associate","S_ti","S_it")
connection.dat <- data.frame(matrix(nrow =0,ncol =4))
colnames(connection.dat) = c("source","assoc1","assoc2","S_ij")

networkobj = dir_sw.g 

##extremely long run time

for (i in 1:length(priming_words)){
  source = priming_words[i]
  associate.dat <- get_associates(source,networkobj)
  strength.dat <- rbind(strength.dat,associate.dat) ##associates in both directions
  connect.dat <- get_connectivity(associate.dat,dir_sw.g) ##connections within associates
  if (nrow(connect.dat) >0){
  connection.dat <- rbind(connection.dat,connect.dat)
  }
}



write.csv(strength.dat,"strength_sw.csv",row.names=FALSE)
write.csv(connection.dat,"connectivity_sw.csv",row.names=FALSE)


##repeat for dir_bar_50
networkobj = dir_bar_50.g 

for (i in 1:length(priming_words)){
  source = priming_words[i]
  associate.dat <- get_associates(source,networkobj)
  strength.dat <- rbind(strength.dat,associate.dat)
  connect.dat <- get_connectivity(associate.dat,dir_sw.g)
  if (nrow(connect.dat) >0){
    connection.dat <- rbind(connection.dat,connect.dat)
  }
}
write.csv(strength.dat,"strength_bar50.csv",row.names=FALSE)
write.csv(connection.dat,"connectivity_bar50.csv",row.names=FALSE)



