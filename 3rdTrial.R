library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)
library(effsize)
library(bestNormalize)
library(rstatix)
library(plyr)

result <- read_csv("/Users/arininurrohmah/Documents/file/GreenLab/Analytics/3rdTrial/3rdTrial.csv")
# results <- spec(result)
# results
result

result <- result %>% 
  mutate_at(c('EC', 'PLT'), as.numeric)

result <- result %>% 
  mutate_at(c('Treatments'), as.factor)
            
result


#filtering
withAdsEC <- select(result, website, Treatments, EC) %>% 
  filter(Treatments == 'withads')
withAdsEC

withoutAdsEC <- select(result, website, Treatments, EC) %>% 
  filter(Treatments == 'withoutads')
withoutAdsEC

withAdsPLT<- select(result, website, Treatments, PLT) %>% 
  filter(Treatments == 'withads')
withAdsPLT

withoutAdsPLT<- select(result, website, Treatments, PLT) %>% 
  filter(Treatments == 'withoutads')
withoutAdsPLT

resultEC<- select(result, website, Treatments, EC)
resultEC

resultPLT<- select(result, website, Treatments, PLT)
resultPLT



# mean(x, na.rm = TRUE)
#mean 53.7999
ECwithAds <- mean(withAdsEC$EC, na.rm = TRUE)
ECwithAds

#mean 53.9973
ECwithoutAds <- mean(withoutAdsEC$EC, na.rm = TRUE)
ECwithoutAds

#mean 6895.818
PLTwithAds <- mean(withAdsPLT$PLT, na.rm = TRUE)
PLTwithAds

#mean 6267.204
PLTwithoutAds <- mean(withoutAdsPLT$PLT, na.rm = TRUE)
PLTwithoutAds

#filter tables
withAdsEC
withoutAdsEC
withAdsPLT
withoutAdsPLT

#all trial numeric cols + avg
numb_colsAdsEC <- colnames(withAdsEC)[unlist(lapply(withAdsEC, is.numeric))]
numb_colsNoAdsEC <- colnames(withoutAdsEC)[unlist(lapply(withoutAdsEC, is.numeric))]
numb_colsAdsPLT <- colnames(withAdsPLT)[unlist(lapply(withAdsPLT, is.numeric))]
numb_colsNoAdsPLT <- colnames(withoutAdsPLT)[unlist(lapply(withoutAdsPLT, is.numeric))]

#hist
par(mfrow=c(2,2))
hist(withAdsEC$EC, main='EC With Ads')
hist(withoutAdsEC$EC, main='EC Without Ads')
hist(withAdsPLT$PLT, main='PLT With Ads')
hist(withoutAdsPLT$PLT, main='PLT Without Ads')


# #hist per all plot
# par(mfrow=c(2,2))
# mapply(hist, withAdsEC[numb_colsAdsEC], main=paste('trial with ads', numb_colsAdsEC), xlab=numb_colsAdsEC)
# mapply(hist, withoutAdsEC[numb_colsNoAdsEC], main=paste('trial without ads', numb_colsNoAdsEC), xlab=numb_colsNoAdsEC)
# mapply(hist, withAdsPLT[numb_colsAdsPLT], main=paste('trial with ads', numb_colsAdsPLT), xlab=numb_colsAdsPLT)
# mapply(hist, withoutAdsPLT[numb_colsNoAdsPLT], main=paste('trial without ads', numb_colsNoAdsPLT), xlab=numb_colsNoAdsPLT)


#correcting the plot
#withAdsEC
ECwithAds <- withAdsEC %>% 
  mutate(EC_PresentAds = EC,
         EC_PresentAds_log = log(EC),
         EC_PresentAds_sqrt = sqrt(EC),
         EC_PresentAds_reciprocal = 1/EC)

par(mfrow=c(2,2))
ECwithAds_Plot <- c('EC_PresentAds','EC_PresentAds_log','EC_PresentAds_sqrt','EC_PresentAds_reciprocal')
mapply(hist, ECwithAds[ECwithAds_Plot], main=paste(ECwithAds_Plot), xlab=ECwithAds_Plot)

#withoutAdsEC
ECwithoutAds <- withoutAdsEC %>% 
  mutate(EC_AbsentAds = EC,
         EC_AbsentAds_log = log(EC),
         EC_AbsentAds_sqrt = sqrt(EC),
         EC_AbsentAds_reciprocal = 1/EC)

par(mfrow=c(2,2))
ECwithoutAds_Plot <- c('EC_AbsentAds','EC_AbsentAds_log','EC_AbsentAds_sqrt','EC_AbsentAds_reciprocal')
mapply(hist, ECwithoutAds[ECwithoutAds_Plot], main=paste(ECwithoutAds_Plot), xlab=ECwithoutAds_Plot)

#withAdsPLT
PLTwithAds <- withAdsPLT %>% 
  mutate(PLT_PresentAds = PLT,
         PLT_PresentAds_log = log(PLT),
         PLT_PresentAds_sqrt = sqrt(PLT),
         PLT_PresentAds_reciprocal = 1/PLT)

par(mfrow=c(2,2))
PLTwithAds_Plot <- c('PLT_PresentAds','PLT_PresentAds_log','PLT_PresentAds_sqrt','PLT_PresentAds_reciprocal')
mapply(hist, PLTwithAds[PLTwithAds_Plot], main=paste(PLTwithAds_Plot), xlab=PLTwithAds_Plot)


##withoutAdsPLT
PLTwithoutAds <- withoutAdsPLT %>% 
  mutate(PLT_AbsentAds = PLT,
         PLT_AbsentAds_log = log(PLT),
         PLT_AbsentAds_sqrt = sqrt(PLT),
         PLT_AbsentAds_reciprocal = 1/PLT)

par(mfrow=c(2,2))
PLTwithoutAds_Plot <- c('PLT_AbsentAds','PLT_AbsentAds_log','PLT_AbsentAds_sqrt','PLT_AbsentAds_reciprocal')
mapply(hist, PLTwithoutAds[PLTwithoutAds_Plot], main=paste(PLTwithoutAds_Plot), xlab=PLTwithoutAds_Plot)


#check normality
check_normality <- function(data){
  plot(density(data, na.rm = TRUE))
  car::qqPlot(data)
  #  qqnorm(data)
  shapiro.test(data)
}

#withAdsEC
#saphiro W = 0.61089, p-value = 6.963e-15
par(mfrow=c(1,2))
withAdsEC$EC %>% 
  check_normality

#saphiro W = 0.72936, p-value = 2.758e-12
par(mfrow=c(1,2))
withoutAdsEC$EC %>% 
  check_normality

#saphiro W = 0.5616, p-value = 1.232e-15
par(mfrow=c(1,2))
withAdsPLT$PLT %>% 
  check_normality

#saphiro W = 0.59467, p-value = 4.09e-15
par(mfrow=c(1,2))
withoutAdsPLT$PLT %>% 
  check_normality


#check normality log
#saphiro W = 0.66034, p-value = 7.041e-14
par(mfrow=c(1,2))
ECwithAds$ECwithAds_log %>% 
  check_normality

#saphiro 0.75612, p-value = 1.363e-11
ECwithoutAds$ECwithoutAds_log %>% 
  check_normality

#saphiro W = 0.92024, p-value = 1.741e-05
PLTwithAds$PLTwithAds_log %>% 
  check_normality

#saphiro W = 0.92534, p-value = 2.987e-05
PLTwithoutAds$PLTwithoutAds_log %>% 
  check_normality


#check normality sqrt
#saphiro W = 0.63618, p-value = 2.211e-14
par(mfrow=c(1,2))
ECwithAds$ECwithAds_sqrt %>% 
  check_normality

#saphiro W = 0.74288, p-value = 6.097e-12
ECwithoutAds$ECwithoutAds_sqrt %>% 
  check_normality

#saphiro W = 0.74318, p-value = 8.453e-12
PLTwithAds$PLTwithAds_sqrt %>% 
  check_normality

#saphiro W = 0.76592, p-value = 2.927e-11
PLTwithoutAds$PLTwithoutAds_sqrt %>% 
  check_normality


#check normality reciprocal
#saphiro W = 0.70495, p-value = 7.044e-13
par(mfrow=c(1,2))
ECwithAds$ECwithAds_reciprocal %>% 
  check_normality

#saphiro W = 0.78163, p-value = 7.015e-11
ECwithoutAds$ECwithoutAds_reciprocal %>% 
  check_normality

#saphiro W = 0.86873, p-value = 7.883e-08
PLTwithAds$PLTwithAds_reciprocal %>% 
  check_normality

#saphiro W = 0.79615, p-value = 2.175e-10
PLTwithoutAds$PLTwithoutAds_reciprocal %>% 
  check_normality



#best normalize > EC with ads > can be done
best_norm_withAdsEC <- bestNormalize(withAdsEC$EC)
best_norm_withAdsEC

#best normalize > EC without ads > can be done
best_norm_withoutAdsEC <- bestNormalize(withoutAdsEC$EC)
best_norm_withoutAdsEC

#best normalize > PLT with ads > can be done
best_norm_withAdsPLT <- bestNormalize(withAdsPLT$PLT)
best_norm_withAdsPLT

#best normalize > PLT with ads > can be done
best_norm_withoutAdsPLT <- bestNormalize(withoutAdsPLT$PLT)
best_norm_withoutAdsPLT


#comparing all avg in data result
#53.7999
MeanwithAdsEC <- mean(withAdsEC$EC, na.rm = TRUE)
MeanwithAdsEC

#53.9973
MeanwithoutAdsEC <- mean(withoutAdsEC$EC, na.rm = TRUE)
MeanwithoutAdsEC

#6267.204
MeanwithAdsPLT <- mean(withAdsPLT$PLT, na.rm = TRUE)
MeanwithAdsPLT

#6895.818
MeanwithoutAdsPLT <- mean(withoutAdsPLT$PLT, na.rm = TRUE)
MeanwithoutAdsPLT


#t.test
#mean EC with ads = 53.7999 
withAdsEC$EC %>% 
  t.test(withAdsEC$EC, data=., na.rm = TRUE)

#mean EC without ads = 53.9973  
withoutAdsEC$EC %>% 
  t.test(withoutAdsEC$EC, data=., na.rm = TRUE)

#mean PLT with ads = 6267.204 
withAdsPLT$PLT %>% 
  t.test(withAdsPLT$PLT, data=., na.rm = TRUE)

#mean PLT without ads = 6895.818 
withoutAdsPLT$PLT %>% 
  t.test(withoutAdsPLT$PLT, data=., na.rm = TRUE)

#mean EC with ads = 53.7999   mean EC without ads = 53.9973
resultEC %>% 
  t.test(EC ~ Treatments, data=.)

#mean PLT with ads = 6829.582   mean EC without ads = 6872.548
resultPLT %>% 
  t.test(PLT ~ Treatments, data=.)


#asign in aov
resEC.aov <- resultEC %>% 
  aov(EC ~ Treatments, data=.)


resPLT.aov <- resultPLT %>% 
  aov(PLT ~ Treatments, data=.)

#summary
summary(resEC.aov)
summary(resPLT.aov)


#try non parametric test kruskal
res.kruskalEC <- resultEC %>% 
  kruskal.test(EC ~ Treatments, data=.)

res.kruskalPLT <- resultPLT %>% 
  kruskal.test(PLT ~ Treatments, data=.)

#Kruskal-Wallis chi-squared = 0.95319, df = 1, p-value = 0.3289
res.kruskalEC
#Kruskal-Wallis chi-squared = 0.13683, df = 1, p-value = 0.7115
res.kruskalPLT

#try non parametric test wilcox
res.wilcoxEC <- resultEC %>% 
  wilcox.test(EC ~ Treatments, data=.)

res.wilcoxPLT <- resultPLT %>% 
  wilcox.test(PLT ~ Treatments, data=.)

#W = 4600.5, p-value = 0.3295
res.wilcoxEC
#W = 4999, p-value = 0.7124
res.wilcoxPLT



#data visualization
library(ggplot2)
resultEC
resultPLT

#EC
EC <- ggplot(resultEC, aes(x=Treatments, y=EC, fill=Treatments))+
  theme_bw()+
  xlab('Energy Consumption') + ylab('Joules (J)') +
  #ylim(c(40,100)) +
  geom_violin(trim=FALSE, alpha=.5) +
  geom_boxplot(width=.3,outlier.size = .5)+
  stat_summary(fun=mean, color='black', geom='point',
               shape=5, size=1)

EC


#PLT
PLT <- ggplot(resultPLT, aes(x=Treatments, y=PLT, fill=Treatments))+
  theme_bw()+
  xlab('Page Load Time') + ylab('milliseconds (ms)') +
  #ylim(c(40,100)) +
  geom_violin(trim=FALSE, alpha=.5) +
  geom_boxplot(width=.3,outlier.size = .5)+
  stat_summary(fun=mean, color='black', geom='point',
               shape=5, size=1)

PLT



resultEC %>% 
  group_by(Treatments) %>% 
  summarize(mean_EC = mean(EC, na.rm = TRUE),
            sd_EC = sd(EC, na.rm = TRUE),
            median_EC = median(EC, na.rm = TRUE),
            min_EC = min(EC, na.rm = TRUE),
            max_EC = max(EC, na.rm = TRUE),
            quantile_EC = quantile(EC, na.rm = TRUE),
  )


resultPLT %>% 
  group_by(Treatments) %>% 
  summarize(mean_PLT = mean(PLT, na.rm = TRUE),
            sd_PLT = sd(PLT, na.rm = TRUE),
            median_PLT = median(PLT, na.rm = TRUE),
            min_PLT = min(PLT, na.rm = TRUE),
            max_PLT = max(PLT, na.rm = TRUE),
            quantile_PLT = quantile(PLT, na.rm = TRUE),
  )

EC


levels(resultEC$Treatments) <- c("present ads", "absent ads")
levels(resultPLT$Treatments) <- c("present ads", "absent ads")

resultEC$Treatments <- recode(resultEC$Treatments, withoutads = 'absence of ads', 
                              withads = 'presence of ads')


#EC
EC <- ggplot(resultEC, aes(x=Treatments, y=EC, fill=Treatments))+
  theme_bw()+
  xlab('Energy Consumption') + ylab('Joules (J)') +
  #ylim(c(40,100)) +
  #geom_violin(trim=FALSE, alpha=.5) +
  geom_boxplot(width=.3,outlier.size = 3)+
  theme(legend.position="none")+
  stat_summary(fun=mean, color='black', geom='point',
               shape=5, size=7)

EC


#PLT
PLT <- ggplot(resultPLT, aes(x=Treatments, y=PLT, fill=Treatments))+
  theme_bw()+
  xlab('Page Load Time') + ylab('milliseconds (ms)') +
  #ylim(c(40,100)) +
  #geom_violin(trim=FALSE, alpha=.5) +
  geom_boxplot(width=.3,outlier.size = 3)+
  theme(legend.position="none")+
  stat_summary(fun=mean, color='black', geom='point',
               shape=5, size=7)

PLT





