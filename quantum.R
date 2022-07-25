library(tidyverse)
library(network)
library(igraph)

priming.dat <- read.csv("priming_paths.csv")
sw.g <- readRDS("sw.rds")
bar_50.g <- readRDS("bar_50.rds")
bar_50_free.g <- readRDS("bar_50_free.rds")
bar_100.g <- readRDS("bar_100.rds")
bar_100_free.g <- readRDS("bar_100_free.rds")


get_associates <- function(target,networkobj){
  edges <- E(networkobj)[adj(target)]
  edge_list=strsplit( attr(edges,"vnames"),"\\|")
  associate_set =sapply(edge_list,"[[",2)
  return(associate_set)
}


if_edge <- function(w1,w2,networkobj){
  output= tryCatch(length(E(networkobj,P =c(w1,w2))),
                   error = function(e) NA)
  return(output)
}


get_size <- function(target,networkobj){
  associates <- get_associates(target,networkobj)
  temp <- expand.grid(w1= associates,w2= associates)
  temp2 <- apply(temp,1,function(x) if_edge(x[1],x[2],networkobj))
  return(c(setsize =length(associates), links = sum(na.omit(temp2))))
}

##sw 

priming.dat$prime_links_sw <- NA
priming.dat$prime_setsize_sw  <- NA
priming.dat$target_links_sw <- NA
priming.dat$target_setsize_sw <- NA

##caution: run time is really long, ~ 1-5 min per iteration 
for (i in 1:nrow(priming.dat)) {
  prime = priming.dat$prime[i]
  target = priming.dat$target[i]
  
  if (prime %in% names(V(sw.g))){
    prime.sw = get_size(prime,sw.g)
  }else{
    prime.sw = c("links" =NA,"setsize" = NA)
  }
  
  if (target %in% names(V(sw.g))){
    target.sw = get_size(target,sw.g)
  } else {
    target.sw = c("links" =NA,"setsize" = NA)
    
  }
  priming.dat$prime_links_sw[i] = unname(prime.sw["links"])
  priming.dat$prime_setsize_sw[i] = unname(prime.sw["setsize"])
  priming.dat$target_links_sw[i] = unname(target.sw["links"])
  priming.dat$target_setsize_sw[i] = unname(target.sw["setsize"])
  
  
}

##bar_50

priming.dat$prime_links_bar50 <- NA
priming.dat$prime_setsize_bar50  <- NA
priming.dat$target_links_bar50 <- NA
priming.dat$target_setsize_bar50 <- NA


for (i in 1:nrow(priming.dat)) {
  prime = priming.dat$prime[i]
  target = priming.dat$target[i]
  
  if (prime %in% names(V(bar_50.g))){
    prime.bar50 = get_size(prime,bar_50.g)
  }else{
    prime.bar50 = c("links" =NA,"setsize" = NA)
  }
  
  if (target %in% names(V(bar_50.g))){
    target.bar50 = get_size(target,bar_50.g)
  } else {
    target.bar50 = c("links" =NA,"setsize" = NA)
    
  }
  priming.dat$prime_links_bar50[i] = unname(prime.bar50["links"])
  priming.dat$prime_setsize_bar50[i] = unname(prime.bar50["setsize"])
  priming.dat$target_links_bar50[i] = unname(target.bar50["links"])
  priming.dat$target_setsize_bar50[i] = unname(target.bar50["setsize"])
  
  
}

##bar_50_f
priming.dat$prime_links_bar50_f <- NA
priming.dat$prime_setsize_bar50_f  <- NA
priming.dat$target_links_bar50_f <- NA
priming.dat$target_setsize_bar50_f <- NA


for (i in 1:nrow(priming.dat)) {
  prime = priming.dat$prime[i]
  target = priming.dat$target[i]
  
  if (prime %in% names(V(bar_50_free.g))){
    prime.bar50_f = get_size(prime,bar_50_free.g)
  }else{
    prime.bar50_f = c("links" =NA,"setsize" = NA)
  }
  
  if (target %in% names(V(bar_50_free.g))){
    target.bar50_f = get_size(target,bar_50_free.g)
  } else {
    target.bar50_f = c("links" =NA,"setsize" = NA)
    
  }
  priming.dat$prime_links_bar50_f[i] = unname(prime.bar50_f["links"])
  priming.dat$prime_setsize_bar50_f[i] = unname(prime.bar50_f["setsize"])
  priming.dat$target_links_bar50_f[i] = unname(target.bar50_f["links"])
  priming.dat$target_setsize_bar50_f[i] = unname(target.bar50_f["setsize"])
}


##bar_100

priming.dat$prime_links_bar100 <- NA
priming.dat$prime_setsize_bar100  <- NA
priming.dat$target_links_bar100 <- NA
priming.dat$target_setsize_bar100 <- NA



for (i in 1:nrow(priming.dat)) {
  prime = priming.dat$prime[i]
  target = priming.dat$target[i]
  if (prime %in% names(V(bar_100.g))){
    prime.bar100 = get_size(prime,bar_100.g)
  }else{
    prime.bar100 = c("links" =NA,"setsize" = NA)
  }
  
  if (target %in% names(V(bar_100.g))){
    target.bar100 = get_size(target,bar_100.g)
  } else {
    target.bar100 = c("links" =NA,"setsize" = NA)
    
  }
  
  priming.dat$prime_links_bar100[i] = unname(prime.bar100["links"])
  priming.dat$prime_setsize_bar100[i] = unname(prime.bar100["setsize"])
  priming.dat$target_links_bar100[i] = unname(target.bar100["links"])
  priming.dat$target_setsize_bar100[i] = unname(target.bar100["setsize"])
}




##bar_100_f


priming.dat$prime_links_bar100_f <- NA
priming.dat$prime_setsize_bar100_f  <- NA
priming.dat$target_links_bar100_f <- NA
priming.dat$target_setsize_bar100_f <- NA



for (i in 1:nrow(priming.dat)) {
  prime = priming.dat$prime[i]
  target = priming.dat$target[i]
  if (prime %in% names(V(bar_100_free.g))){
    prime.bar100_f = get_size(prime,bar_100_free.g)
  }else{
    prime.bar100_f = c("links" =NA,"setsize" = NA)
  }
  
  if (target %in% names(V(bar_100_free.g))){
    target.bar100_f = get_size(target,bar_100_free.g)
  } else {
    target.bar100_f = c("links" =NA,"setsize" = NA)
    
  }
  
  priming.dat$prime_links_bar100_f[i] = unname(prime.bar100_f["links"])
  priming.dat$prime_setsize_bar100_f[i] = unname(prime.bar100_f["setsize"])
  priming.dat$target_links_bar100_f[i] = unname(target.bar100_f["links"])
  priming.dat$target_setsize_bar100_f[i] = unname(target.bar100_f["setsize"])
}


write.csv(priming.dat,"priming_full.csv",row.names = FALSE)

priming.dat <- read.csv("priming_full.csv")



library(MASS)


quantum_sw <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                          "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                          "prime_links_sw", "prime_setsize_sw","target_links_sw","target_setsize_sw")]

quantum_bar50 <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                               "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                             "prime_links_bar50", "prime_setsize_bar50","target_links_bar50","target_setsize_bar50")]
quantum_bar50_f <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                               "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                               "prime_links_bar50_f", "prime_setsize_bar50_f","target_links_bar50_f","target_setsize_bar50_f")]

quantum_bar100 <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                              "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                              "prime_links_bar100", "prime_setsize_bar100","target_links_bar100","target_setsize_bar100")]
quantum_bar100_f <- priming.dat[c("Ztarget.RT","prime_Length","target_Length","prime_Log_Freq_HAL","target_Log_Freq_HAL",
                                "prime_LgSUBTLWF","target_LgSUBTLWF","prime_Semantic_Neighborhood_Density","target_Semantic_Neighborhood_Density",
                                "prime_links_bar100_f", "prime_setsize_bar100_f","target_links_bar100_f","target_setsize_bar100_f")]



qsw <- cor(quantum_sw)

ggcorrplot(qsw,hc.order = TRUE,
           type = "lower",
           outline.color = "white",lab= TRUE,lab_size= 2)


##baroni versions
ggcorrplot(cor(na.omit(quantum_bar100_f[c(1,10,11,12,13)])),hc.order = TRUE,
           type = "lower",
           outline.color = "white",lab= TRUE,lab_size= 3)





##AIC 

fit.sw_1 <- lm(Ztarget.RT ~1,quantum_sw)
fit.bar50_1 <- lm(Ztarget.RT ~1,na.omit(quantum_bar50))
fit.sw_2 <- lm(Ztarget.RT ~ prime_Length+target_Length+prime_Log_Freq_HAL+target_Log_Freq_HAL+
               prime_LgSUBTLWF+target_LgSUBTLWF+prime_Semantic_Neighborhood_Density+target_Semantic_Neighborhood_Density+
               prime_links_sw+prime_setsize_sw+target_links_sw+target_setsize_sw,quantum_sw)
fit.bar50_2 <- lm(Ztarget.RT ~prime_Length+target_Length+prime_Log_Freq_HAL+target_Log_Freq_HAL+
                    prime_LgSUBTLWF+target_LgSUBTLWF+prime_Semantic_Neighborhood_Density+target_Semantic_Neighborhood_Density+
                    prime_links_bar50+prime_setsize_bar50+target_links_bar50+target_setsize_bar50,quantum_bar50)

step_sw.F <- stepAIC(fit.sw_1,scope=list(upper=~prime_Length+target_Length+prime_Log_Freq_HAL+target_Log_Freq_HAL+
                                          prime_LgSUBTLWF+target_LgSUBTLWF+prime_Semantic_Neighborhood_Density+target_Semantic_Neighborhood_Density+
                                          prime_links_sw+prime_setsize_sw+target_links_sw+target_setsize_sw,
                                               lower=~1),direction="both")
step_bar50.F <- stepAIC(fit.bar50_1,scope=list(upper=~prime_Length+target_Length+prime_Log_Freq_HAL+target_Log_Freq_HAL+
                                              prime_LgSUBTLWF+target_LgSUBTLWF+prime_Semantic_Neighborhood_Density+target_Semantic_Neighborhood_Density+
                                              prime_links_bar50+prime_setsize_bar50+target_links_bar50+target_setsize_bar50,
                                         lower=~1),direction="both")


summary(step_sw.F) ## Multiple R-squared:  0.317,	Adjusted R-squared:  0.3133 

## lm(formula = Ztarget.RT ~ target_LgSUBTLWF + target_Semantic_Neighborhood_Density + 
## target_Length + target_setsize_sw + prime_LgSUBTLWF + target_links_sw, 
## data = quantum_sw)

summary(step_bar50.F)  ##Multiple R-squared:  0.3138,	Adjusted R-squared:  0.309 

## lm(formula = Ztarget.RT ~ target_LgSUBTLWF + target_Semantic_Neighborhood_Density + 
## target_Length + prime_setsize_bar50 + prime_LgSUBTLWF + target_links_bar50 + 
##  prime_Length, data = na.omit(quantum_bar50)) 
