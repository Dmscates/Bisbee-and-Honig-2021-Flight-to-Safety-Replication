# File name: figure8_SIfigure19.R
# In: 
#   - /Data/france_data.RData: Data on 2020 municipal elections in France, linked with local-level cases and deaths
# Out: 
#   - /Figures/figure8.pdf
#   - /Figures/SI_figure19.pdf


require(lfe)
require(ggridges)
require(tidyverse)
require(gridExtra)
rm(list =ls())
gc()
####################################################################################################################### Loading functions
source('C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\code\\helper_functions.R')


load('C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Data\\france_data.RData')

# Figure 8
p1 <- toanal %>%
  mutate(mainstream = ifelse(mainstream == 0,'Non-Mainstream','Mainstream'),
         period = factor(ifelse(date == as.Date('2020-03-15'),'March Elections','June Elections'),
                         levels = c('March Elections','June Elections'))) %>%
  group_by(mainstream,period) %>%
  summarise(votes = mean(votes)) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x = period,y = votes,fill = factor(mainstream))) + 
  stat_summary(fun.y = mean,geom = "bar",position = position_dodge(width = .9),
               size = 3,alpha = .7) + 
  scale_fill_manual(name = '',values = c('Mainstream' = 'grey80','Non-Mainstream' = 'grey30')) + 
  theme_ridges() + xlab('Period') + ylab('Aggregate Vote Share')

p2 <- toanal %>%
  mutate(mainstream = ifelse(mainstream == 0,'Non-Mainstream','Mainstream'),
         period = factor(ifelse(date == as.Date('2020-03-15'),'March Elections','June Elections'),
                         levels = c('March Elections','June Elections')),
         logcases = log(deaths+1)) %>%
  filter(!is.na(period)) %>%
  ggplot(aes(x = logcases,y = votes,color = mainstream)) + 
  geom_point(alpha = .7,size = 2) + 
  geom_smooth(method = 'lm') +
  scale_color_manual(name = "",values = c('Mainstream' = 'grey80',"Non-Mainstream" = "grey30")) + 
  theme_ridges() + xlab("Deaths (logged)") + ylab('Aggregate Vote Share')

pdf('../figures/figure8.pdf',width = 7,height = 4)
grid.arrange(p1 + theme(legend.position = 'bottom'),
             p2 + theme(legend.position = 'bottom'),ncol = 2)
dev.off()



# SI Figure 19
toanal$post <- ifelse(toanal$date == '2020-03-15',0,1)

toanal$antiEst <- (toanal$mainstream - 1)^2
summary(mod1 <- felm(votes ~ antiEst*post | dpt_code | 0 | dpt_code,toanal))
summary(mod2 <- felm(votes ~ antiEst*log(deaths+1) | post + dpt_code | 0 | dpt_code,toanal))

toplot <- data.frame(interaction_plot_continuous(mod1,plot = T,pointsplot = T,colr = 'black',alph = 200,num_points = 2,
                            xlabel = "Election Wave",ylabel = 'Marginal Coefficient of Mainstream')) %>%
  mutate(type = 'Temporal')

toplot <- bind_rows(toplot,data.frame(interaction_plot_continuous(mod2,plot = T,pointsplot = T,
                                                                  colr = 'black',alph = 200,num_points = 20,
                                                 xlabel = "Election Wave",ylabel = 'Marginal Coefficient of Mainstream')) %>%
  mutate(type = 'Geographic'))



p1 <- toplot %>%
  filter(type == 'Temporal') %>%
  mutate(x = factor(ifelse(x_2 == 0,'March Elections','June Elections'),levels = c('March Elections','June Elections'))) %>%
  ggplot(aes(x = x,y = delta_1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lb,ymax = ub),width = .1) + 
  geom_hline(yintercept = 0,linetype = 'dashed') +
  ylab('Margnial Coefficient on Non-Mainstream') + xlab('') + 
  theme_ridges() + 
  ggtitle('Temporal Variation')
p2 <- toplot %>%
  filter(type == 'Geographic') %>%
  ggplot(aes(x = x_2,y = delta_1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lb,ymax = ub),width = .1) + 
  geom_hline(yintercept = 0,linetype = 'dashed') +
  ylab('Margnial Coefficient on Non-Mainstream') + xlab('Logged Deaths') + 
  theme_ridges() + 
  ggtitle('Geographic Variation')

pdf('../Figures/SI_figure19.pdf',width = 8,height = 4)
grid.arrange(p1,p2,ncol = 2)
dev.off()
