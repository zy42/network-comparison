library(tidyverse)

library(igraph)


sw.g <- readRDS("sw.rds")


bar_50.g <- readRDS("bar_50.rds")

transitivity(sw.g,type = "global",vids = NULL,isolates = c("NaN", "zero"))  #0.06630624

transitivity(bar_50.g,type = "global",vids = NULL,isolates = c("NaN", "zero"))  #0.06029599

t_sw <- transitivity(sw.g,type = "weighted",vids = NULL,isolates = c("NaN", "zero"))  

t_sw <- t_sw[is.finite(t_sw)]
summary(t_sw) 
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##0.00000 0.09312 0.18168 0.27343 0.32301 3.00000 

mean(t_sw) ## 0.2734316

t_bar50 <- transitivity(bar_50.g,type = "weighted",vids = NULL,isolates = c("NaN", "zero"))  

t_bar50 <- t_bar50[is.finite(t_bar50)]
summary(t_bar50)

##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##0.0000  0.0000  0.1545  0.4890  0.6976  2.0000 

mean(t_bar50) ##0.4889902


##Macro metrics: degree, stress, eigenvector
summary(unname(degree(sw.g)))

##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##1.00    1.00    2.00   18.47   32.00  940.00 

summary(unname(degree(bar_50.g)))

##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 1.000   1.000   2.000   5.318   4.000 105.000 


##Degree

summary(degree(sw.g))
summary(degree(bar_50.g))

edge_density(sw.g,loops =TRUE) # 0.0005991626
edge_density(sw.g,loops =FALSE) # 0.0005991626


edge_density(bar_50.g,loops =TRUE) # 4.966252e-05
edge_density(bar_50.g,loops =FALSE) # 4.966344e-05
