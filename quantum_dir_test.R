library(tidyverse)
library(ggcorrplot)
library(MASS)

priming.dat <- read.csv("ldt_baseline.csv")[,-c(4,5,6,8)]

priming.dat= na.omit(priming.dat)

strength_sw <- read.csv("strength_sw.csv")
strength_bar<- read.csv("strength_bar50.csv")

connect_sw <- read.csv("connectivity_sw.csv")
connect_bar<- read.csv("connectivity_bar50.csv")



###model 1 spreading activation: ease of recall =  sum(S_ti*S_it) + sum(S_ti*S_ij*S_jt), t = source, i = associate1, j = associate2
##2 associates: assoc1 must be directed from the source, assoc2 must be directed to target; assoc1 and assoc2 must be connected



###model 2 spooky action: ease of recall = sum(S_ti) + sum(S_it) + sum(S_ij), t = source, i = associate1, j = associate2



priming.dat$prime_m1_sw = NA
priming.dat$prime_m2_sw = NA
priming.dat$prime_m1_bar = NA
priming.dat$prime_m2_bar= NA

priming.dat$target_m1_sw = NA
priming.dat$target_m2_sw = NA
priming.dat$target_m1_bar = NA
priming.dat$target_m2_bar= NA

##primes
for (i in 1: nrow(priming.dat)){
  s = priming.dat$prime[i]
  d1_sw = filter(strength_sw,source== s)
  d1_bar = filter(strength_bar,source== s)
  assoc1_sw = filter(d1_sw,!(is.na(S_ti)))$associate
  assoc2_sw = filter(d1_sw,!(is.na(S_it)))$associate
  assoc1_bar = filter(d1_bar,!(is.na(S_ti)))$associate
  assoc2_bar = filter(d1_bar,!(is.na(S_it)))$associate
  m1d2_sw = filter(connect_sw,source== s & assoc1 %in% assoc1_sw & assoc2 %in% assoc2_sw)
  m1d2_bar = filter(connect_bar,source== s & assoc1 %in% assoc1_bar & assoc2 %in% assoc2_bar)
  m2d2_sw = filter(connect_sw,source== s)
  m2d2_bar = filter(connect_bar,source== s)
  
  priming.dat$prime_m1_sw[i] = sum(na.omit(d1_sw$S_ti*d1_sw$S_it)) + sum(m1d2_sw$S_ti*m1d2_sw$S_ij*m1d2_sw$S_jt)
  priming.dat$prime_m2_sw[i] = sum(na.omit(d1_sw$S_ti))+sum(na.omit(d1_sw$S_it))+sum(na.omit(m2d2_sw$S_ij))
  priming.dat$prime_m1_bar[i] = sum(na.omit(d1_bar$S_ti*d1_bar$S_it)) + sum(m1d2_bar$S_ti*m1d2_bar$S_ij*m1d2_bar$S_jt)
  priming.dat$prime_m2_bar[i]= sum(na.omit(d1_bar$S_ti))+sum(na.omit(d1_bar$S_it))+sum(na.omit(m2d2_bar$S_ij))

}


##targets
for (i in 1: nrow(priming.dat)){
  s = priming.dat$target[i]
  d1_sw = filter(strength_sw,source== s)
  d1_bar = filter(strength_bar,source== s)
  assoc1_sw = filter(d1_sw,!(is.na(S_ti)))$associate
  assoc2_sw = filter(d1_sw,!(is.na(S_it)))$associate
  assoc1_bar = filter(d1_bar,!(is.na(S_ti)))$associate
  assoc2_bar = filter(d1_bar,!(is.na(S_it)))$associate
  m1d2_sw = filter(connect_sw,source== s & assoc1 %in% assoc1_sw & assoc2 %in% assoc2_sw)
  m1d2_bar = filter(connect_bar,source== s & assoc1 %in% assoc1_bar & assoc2 %in% assoc2_bar)
  m2d2_sw = filter(connect_sw,source== s)
  m2d2_bar = filter(connect_bar,source== s)
  
  priming.dat$target_m1_sw[i] = sum(na.omit(d1_sw$S_ti*d1_sw$S_it)) + sum(m1d2_sw$S_ti*m1d2_sw$S_ij*m1d2_sw$S_jt)
  priming.dat$target_m2_sw[i] = sum(na.omit(d1_sw$S_ti))+sum(na.omit(d1_sw$S_it))+sum(na.omit(m2d2_sw$S_ij))
  priming.dat$target_m1_bar[i] = sum(na.omit(d1_bar$S_ti*d1_bar$S_it)) + sum(m1d2_bar$S_ti*m1d2_bar$S_ij*m1d2_bar$S_jt)
  priming.dat$target_m2_bar[i] = sum(na.omit(d1_bar$S_ti))+sum(na.omit(d1_bar$S_it))+sum(na.omit(m2d2_bar$S_ij))
  
}


write.csv(priming.dat,"priming_quantum_dir.csv")

priming_dir <- read.csv("priming_quantum_dir.csv")

corr <- cor(na.omit(priming_dir[c(3,14:21)]))



ggcorrplot(corr,hc.order = TRUE,
           type = "lower",
           outline.color = "white",lab= TRUE,lab_size= 3)



###AIC




fit1 <- lm(Ztarget.RT ~1,priming.dat[3:21])
fit2 <- lm(Ztarget.RT ~ prime_Length+target_Length+prime_Log_Freq_HAL+target_Log_Freq_HAL+
                 prime_LgSUBTLWF+target_LgSUBTLWF+prime_Semantic_Neighborhood_Density+target_Semantic_Neighborhood_Density+
             prime_m1_sw+prime_m2_sw+prime_m1_bar+prime_m2_bar+
             target_m1_sw+target_m2_sw+target_m1_bar+target_m2_bar
             ,priming.dat[3:21])



step.F <- stepAIC(fit1,scope=list(upper=~prime_Length+target_Length+prime_Log_Freq_HAL+target_Log_Freq_HAL+
                                        prime_LgSUBTLWF+target_LgSUBTLWF+
                                        target_m1_sw+target_m2_sw+target_m1_bar+target_m2_bar+
                                    prime_Semantic_Neighborhood_Density+target_Semantic_Neighborhood_Density,
                                         lower=~1),direction="both")

summary(step.F)

