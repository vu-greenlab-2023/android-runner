library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)
library(effsize)
library(bestNormalize)
library(rstatix)


result <- read_csv("/Users/arininurrohmah/Documents/file/GreenLab/Analytics/Measurement_Results - Sheet3.csv")
# results <- spec(result)
# results
result
result <- result %>% 
  mutate_at(c('factor'), as.factor)

result

result <- result %>% 
  mutate_at(c('PLT1','PLT2','PLT3','PLT4','PLT5','PLT6','PLT7','PLT8','PLT9','PLT10'), as.numeric)

result <- result %>% 
  mutate_at(c('EC1','EC2','EC3','EC4','EC5','EC6','EC7','EC8','EC9','EC10'), as.numeric)
result

result$factor %>% 
  levels()


withAds <- result %>% 
  filter(factor == 'with_ads')

#withAds



# withAdsEC <- result %>% 
#   select(result, EC1,EC2,EC3,EC4,EC5,EC6,EC7,EC8,EC9,EC10,) %>%
#   filter(factor == 'with_ads') 
  

withAdsEC <- select(result, website, factor, EC1,EC2,EC3,EC4,EC5,EC6,EC7,EC8,EC9,EC10,) %>% 
  filter(factor == 'with_ads')

withAdsEC

withoutAdsEC <- select(result, website, factor, EC1,EC2,EC3,EC4,EC5,EC6,EC7,EC8,EC9,EC10,) %>% 
  filter(factor == 'without_ads')

withoutAdsEC

withAdsPLT<- select(result, website, factor, PLT1,PLT2,PLT3,PLT4,PLT5,PLT6,PLT7,PLT8,PLT9,PLT10,) %>% 
  filter(factor == 'with_ads')

withAdsPLT

withoutAdsPLT<- select(result, website, factor, PLT1,PLT2,PLT3,PLT4,PLT5,PLT6,PLT7,PLT8,PLT9,PLT10,) %>% 
  filter(factor == 'without_ads')

withoutAdsPLT

# mean(x, na.rm = TRUE)

withAdsEC <- withAdsEC %>% rowwise() %>%
  mutate(avg = mean(c_across(EC1:EC10), na.rm = TRUE))

withoutAdsEC <- withoutAdsEC %>% rowwise() %>%
  mutate(avg = mean(c_across(EC1:EC10), na.rm = TRUE))

withAdsPLT <- withAdsPLT %>% rowwise() %>%
  mutate(avg = mean(c_across(PLT1:PLT10), na.rm = TRUE))

withoutAdsPLT <- withoutAdsPLT %>% rowwise() %>%
  mutate(avg = mean(c_across(PLT1:PLT10), na.rm = TRUE))

#filter tables
withAdsEC
withoutAdsEC
withAdsPLT
withoutAdsPLT

# result <- result %>% rowwise() %>%
#   mutate(avg = mean(c_across(PLT1:EC10), na.rm = TRUE))
# result
# numb_cols <- colnames(result)[unlist(lapply(result, is.numeric))]
# numb_cols

#all trial numeric cols + avg
numb_colsAdsEC <- colnames(withAdsEC)[unlist(lapply(withAdsEC, is.numeric))]
numb_colsNoAdsEC <- colnames(withoutAdsEC)[unlist(lapply(withoutAdsEC, is.numeric))]
numb_colsAdsPLT <- colnames(withAdsPLT)[unlist(lapply(withAdsPLT, is.numeric))]
numb_colsNoAdsPLT <- colnames(withoutAdsPLT)[unlist(lapply(withoutAdsPLT, is.numeric))]

#hist(result$avg, main='Average of all')
par(mfrow=c(2,2))
hist(withAdsEC$avg, main='avg EC With Ads')
hist(withoutAdsEC$avg, main='avg EC Without Ads')
hist(withAdsPLT$avg, main='avg PLT With Ads')
hist(withoutAdsPLT$avg, main='avg PLT Without Ads')

#hist per all plot
par(mfrow=c(3,4))
mapply(hist, withAdsEC[numb_colsAdsEC], main=paste('trial with ads', numb_colsAdsEC), xlab=numb_colsAdsEC)
mapply(hist, withoutAdsEC[numb_colsNoAdsEC], main=paste('trial without ads', numb_colsNoAdsEC), xlab=numb_colsNoAdsEC)
mapply(hist, withAdsPLT[numb_colsAdsPLT], main=paste('trial with ads', numb_colsAdsPLT), xlab=numb_colsAdsPLT)
mapply(hist, withoutAdsPLT[numb_colsNoAdsPLT], main=paste('trial without ads', numb_colsNoAdsPLT), xlab=numb_colsNoAdsPLT)

#correcting the plot (Avg)
#withAdsEC
withAdsEC <- withAdsEC %>% 
  mutate(avg_log = log(avg),
         avg_sqrt = sqrt(avg),
         avg_reciprocal = 1/avg)

par(mfrow=c(2,2))
withAdsEC_avgPlot <- c('avg','avg_log','avg_sqrt','avg_reciprocal')
mapply(hist, withAdsEC[withAdsEC_avgPlot], main=paste('Avg EC with ads', withAdsEC_avgPlot), xlab=withAdsEC_avgPlot)

#withoutAdsEC
withoutAdsEC <- withoutAdsEC %>% 
  mutate(avg_log = log(avg),
         avg_sqrt = sqrt(avg),
         avg_reciprocal = 1/avg)

par(mfrow=c(2,2))
withoutAdsEC_avgPlot <- c('avg','avg_log','avg_sqrt','avg_reciprocal')
mapply(hist, withoutAdsEC[withoutAdsEC_avgPlot], main=paste('Avg EC without ads', withoutAdsEC_avgPlot), xlab=withoutAdsEC_avgPlot)

#withAdsPLT
withAdsPLT <- withAdsPLT %>% 
  mutate(avg_log = log(avg),
         avg_sqrt = sqrt(avg),
         avg_reciprocal = 1/avg)

par(mfrow=c(2,2))
withAdsPLT_avgPlot <- c('avg','avg_log','avg_sqrt','avg_reciprocal')
mapply(hist, withAdsPLT[withAdsPLT_avgPlot], main=paste('Avg PLT with ads', withAdsPLT_avgPlot), xlab=withAdsPLT_avgPlot)

##withoutAdsPLT
withoutAdsPLT <- withoutAdsPLT %>% 
  mutate(avg_log = log(avg),
         avg_sqrt = sqrt(avg),
         avg_reciprocal = 1/avg)

par(mfrow=c(2,2))
withoutAdsPLT_avgPlot <- c('avg','avg_log','avg_sqrt','avg_reciprocal')
mapply(hist, withoutAdsPLT[withoutAdsPLT_avgPlot], main=paste('Avg PLT without ads', withoutAdsPLT_avgPlot), xlab=withoutAdsPLT_avgPlot)


#check normality
check_normality <- function(data){
  plot(density(data))
  car::qqPlot(data)
  #  qqnorm(data)
  shapiro.test(data)
}

#withAdsEC
#saphiro too low
par(mfrow=c(1,2))
withAdsEC$avg %>% 
  check_normality

#saphiro too low
withAdsEC$avg_log %>% 
  check_normality

#saphiro too low
withAdsEC$avg_sqrt %>% 
  check_normality

#saphiro too low
withAdsEC$avg_reciprocal %>% 
  check_normality



#withoutAdsEC
#saphiro too low
withoutAdsEC$avg %>% 
  check_normality

#saphiro too low
withoutAdsEC$avg_log %>% 
  check_normality

#saphiro too low
withoutAdsEC$avg_sqrt %>% 
  check_normality

#saphiro too low
withoutAdsEC$avg_reciprocal %>% 
  check_normality



#withAdsPLT
withAdsPLT$avg %>% 
  check_normality

withAdsPLT$avg_log %>% 
  check_normality

withAdsPLT$avg_sqrt %>% 
  check_normality

withAdsPLT$avg_reciprocal %>% 
  check_normality


#withoutAdsPLT
withoutAdsPLT$avg %>% 
  check_normality

withoutAdsPLT$avg_log %>% 
  check_normality

withoutAdsPLT$avg_sqrt %>% 
  check_normality

withoutAdsPLT$avg_reciprocal %>% 
  check_normality


# withAdsEC
# withoutAdsEC
# withAdsPLT
# withoutAdsPLT

#best normalize > cant be done
best_norm_withAdsEC <- bestNormalize(withAdsEC$avg)



#comparing all avg in data result
result

resultsAllEC <- select(result, website, factor, EC1,EC2,EC3,EC4,EC5,EC6,EC7,EC8,EC9,EC10)
resultsAllPLT<- select(result, website, factor, PLT1,PLT2,PLT3,PLT4,PLT5,PLT6,PLT7,PLT8,PLT9,PLT10)

resultsAllEC
resultsAllPLT

resultsAllECMean <- resultsAllEC %>% rowwise() %>%
  mutate(avg = mean(c_across(EC1:EC10), na.rm = TRUE))

resultsAllPLTMean <- resultsAllPLT %>% rowwise() %>%
  mutate(avg = mean(c_across(PLT1:PLT10), na.rm = TRUE))

resultsAllECMean
resultsAllPLTMean


#mean EC with ads = 53.7999   mean EC without ads = 53.9973
resultsAllECMean %>% 
  t.test(avg ~ factor, data=.)

#mean PLT with ads = 6829.582   mean EC without ads = 6872.548
resultsAllPLTMean %>% 
  t.test(avg ~ factor, data=.)


#asign in aov
resEC.aov <- resultsAllECMean %>% 
  aov(avg ~ factor, data=.)


resPLT.aov <- resultsAllPLTMean %>% 
  aov(avg ~ factor, data=.)

#summary
summary(resEC.aov)
summary(resPLT.aov)


#try non parametric test kruskal
res.kruskalEC <- resultsAllECMean %>% 
  kruskal.test(avg ~ factor, data=.)

res.kruskalPLT <- resultsAllPLTMean %>% 
  kruskal.test(avg ~ factor, data=.)

res.kruskalEC
res.kruskalPLT

#try non parametric test wilcox
res.wilcoxEC <- resultsAllECMean %>% 
  wilcox.test(avg ~ factor, data=.)

res.wilcoxPLT <- resultsAllPLTMean %>% 
  wilcox.test(avg ~ factor, data=.)

res.wilcoxEC
res.wilcoxPLT


#data visualization
library(ggplot2)
resultsAllEC
resultsAllPLT

resultsAllECMean
resultsAllPLTMean

#EC
ECMean <- ggplot(resultsAllECMean, aes(x=factor, y=avg, fill=factor))+
  theme_bw()+
  xlab('Energy Consumption') + ylab('Joules (J)') +
  #ylim(c(40,100)) +
  geom_violin(trim=FALSE, alpha=.5) +
  geom_boxplot(width=.3,outlier.size = .5)+
  stat_summary(fun=mean, color='black', geom='point',
               shape=5, size=1)

ECMean


#PLT
PLTMean <- ggplot(resultsAllPLTMean, aes(x=factor, y=avg, fill=factor))+
  theme_bw()+
  xlab('Page Load Time') + ylab('milliseconds (ms)') +
  #ylim(c(40,100)) +
  geom_violin(trim=FALSE, alpha=.5) +
  geom_boxplot(width=.3,outlier.size = .5)+
  stat_summary(fun=mean, color='black', geom='point',
               shape=5, size=1)

PLTMean











































