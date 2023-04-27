# File name: SIfigure11_SIfigure12_SIfigure15_SIfigure16.R
# In: 
#   - nationscape_prepped.RData: Nationscape survey data merged with NYT covid data, aggregated to the congressional district level
# Out: 
#   - /Figures/SI_figure11.pdf
#   - /Figures/SI_figure12.pdf
#   - /Figures/SI_figure15.pdf
#   - /Figures/SI_figure16.pdf



rm(list = ls())
gc()
require(tidyverse)
require(stargazer)
require(lubridate)
require(ggridges)
require(lfe)



####################################################################################################################### Loading data
load("C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Data\\nationscape_data.RData")




####################################################################################################################### Loading functions
source('C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\code\\helper_functions.R')



# SI Figure 11
pdf('../Figures/SI_figure11.pdf',width = 7,height = 5)
forAnal %>%
  filter(dateMo < as.Date('2020-07-01') & dateMo > as.Date('2019-07-01')) %>%
  group_by(stcd,dateMo,exposedMean) %>%
  summarise(ideoM = mean(economy_better,na.rm=T)) %>%
  ggplot(aes(x = ideoM,y = fct_rev(factor(dateMo)),group = paste(dateMo,exposedMean),
             fill = paste(dateMo,exposedMean))) +
  geom_density_ridges(alpha = .6,color = 'white',quantile_lines = T,quantiles = 2) + 
  scale_fill_cyclical(
    breaks = c("2019-08-01 0", "2019-08-01 1"),
    labels = c(`2019-08-01 0` = "Insulated", `2019-08-01 1` = "Exposed"),
    values = rev(c("#000000", "#b0b0b0", "#000000", "#b0b0b0")),
    name = "", guide = "legend"
  ) + 
  theme_ridges() + 
  xlab("Economic Evaluation") + 
  ylab("") + 
  scale_x_continuous(limits = c(1,3),breaks = 1:3,labels = c('Improved','The Same','Worse'))
dev.off()

# SI Figure 12
forAnal %>%
  filter(dateMo < as.Date('2020-07-01') & dateMo > as.Date('2019-07-01')) %>%
  group_by(stcd,dateWk,dateMo,exposedMean) %>%
  summarise(econ = mean(economy_better,na.rm=T)) %>%
  mutate(post = ifelse(dateMo > as.Date('2020-02-01'),1,0)) -> forDID

summary(mEcon <- felm(scale(econ) ~ exposedMean*post | 0 | 0 | stcd,forDID %>% filter(dateMo < as.Date('2020-04-01'))))
toplot <- data.frame(interaction_plot_continuous(mEcon,pointsplot = T,num_points = 2,plot = T))

pdf('../Figures/SI_figure12.pdf',width = 7,height = 4)
toplot %>%
  mutate(x_2 = factor(ifelse(x_2 == 0,'Pre-March','March'),levels = c('Pre-March','March'))) %>%
  ggplot(aes(x = x_2,y = delta_1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lb,ymax = ub),width = .1) +
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  theme_ridges() +
  xlab('Period') + ylab('Effect of Covid Exposure')
dev.off()



# SI Figure 15
(pSanders <- forAnal %>%
  mutate(sanders = (cand_favorability_sanders - 5)*-1) %>% #ifelse(cand_favorability_sanders < 3,1,0)) %>%
  filter(dateMo < as.Date('2020-05-01') & dateMo > as.Date('2019-07-01')) %>%
  group_by(stcd,dateMo,exposedMean) %>%
  summarise(ideoM = mean(sanders,na.rm=T)) %>%
  ggplot(aes(x = ideoM,y = fct_rev(factor(dateMo)),group = paste(dateMo,exposedMean),
             fill = paste(dateMo,exposedMean))) +
  geom_density_ridges(alpha = .6,color = 'white',quantile_lines = T,quantiles = 2) + 
  scale_fill_cyclical(
    breaks = c("2019-08-01 0", "2019-08-01 1"),
    labels = c(`2019-08-01 0` = "Insulated", `2019-08-01 1` = "Exposed"),
    values = rev(c("#000000", "#b0b0b0", "#000000", "#b0b0b0")),
    name = "", guide = "legend"
  ) + 
  theme_ridges() + 
  xlab("Sanders Approval") + 
  ylab("") + 
  scale_x_continuous(breaks = 1:4,labels = c('Very\nunfavorable','Somewhat\nunfavorable','Somewhat\nfavorable','Very\nfavorable')))

pdf('../Figures/SI_figure15.pdf',width = 7,height = 5)
print(pSanders)
dev.off()

# SI Figure 16
pdf('../Figures/SI_figure16.pdf',width = 8,height = 5)
forAnal %>%
  mutate(sanders = (cand_favorability_sanders - 5)*-1) %>% #ifelse(cand_favorability_sanders < 3,1,0)) %>%
  filter(dateWk %in% as.Date(c('2020-02-02','2020-02-09','2020-02-16','2020-02-23','2020-03-01','2020-03-08','2020-03-15','2020-03-22','2020-03-29'))) %>%
  group_by(stcd,dateWk) %>%
  summarise(sanders = mean(sanders,na.rm=T),
            cases = log(mean(casesSum,na.rm=T)+1)) %>%
  ggplot(aes(x = cases,y = sanders)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(~dateWk) + 
  theme_ridges() + 
  xlab('Logged Cases') + ylab('Sanders Approval')
dev.off()
