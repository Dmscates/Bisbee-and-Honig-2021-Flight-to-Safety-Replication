# File name: figure4.R
# In: 
#   - replication_data.RData
# Out: 
#   - /Figures/figure4.pdf

require(lme4)
require(lfe)
require(MatchIt)
require(WeightIt)
require(tjbal)
require(optmatch)
require(stargazer)
require(cobalt)
require(tidyverse)
require(ggridges)
rm(list = ls())
gc()
####################################################################################################################### Loading data
load('../Data/replication_data.RData')









####################################################################################################################### Loading functions
source('C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\code\\helper_functions.R')





# Figure 4
finalDat$post <- ifelse(finalDat$date > as.Date('2020-03-10'),1,0)
finalDat$treat <- ifelse(finalDat$DMA_March17Cases > 1,1,0)
summary(tmp <- lmer(as.formula(paste0("pcttw_sanders ~ treat*post + (1|DMA_CODE)")),finalDat))$coefficients
toplot <- data.frame(interaction_plot_continuous(tmp,num_points = 2,pointsplot = T))


pdf('../Figures/figure4.pdf',width = 7,height = 5)
toplot %>%
  mutate(x = factor(ifelse(x_2 == 0,'Pre','Post'),levels = c('Pre','Post'))) %>%
  ggplot(aes(x = x,y = delta_1))+ 
  geom_point() + 
  geom_errorbar(aes(ymin = lb,ymax = ub),width= .1) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  xlab("Period") + ylab("Marginal Effect of Exposure") + 
  theme_ridges()
dev.off()
