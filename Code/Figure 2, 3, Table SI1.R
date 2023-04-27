# File name: figure2_figure3_SItable1.R
# In: 
#   - replication_data.RData
# Out: 
#   - /Figures/figure2.pdf
#   - /Figures/figure3.pdf
#   - /Tables/SI-table1.tex

require(lme4)
require(lfe)
require(MatchIt)
require(WeightIt)
require(tjbal)
#install.packages("tjbal")
#install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
#devtools::install_github('chadhazlett/kbal')
#devtools::install_github('xuyiqing/tjbal')
require(optmatch)
require(stargazer)
require(cobalt)
require(tidyverse)
require(sf)
#install.packages("sf")
require(leaflet)
#install.packages("leaflet")
require(ggridges)
#install.packages("ggridges")
require(ggthemes)
install.packages("ggthemes")
require(gridExtra)  

rm(list = ls())
gc()
########################################################################################## Loading data
load('../Data/replication_data.RData')


# Figure 2: Map (compressed to PNG in manuscript)
toplot <- maps$county %>% filter(!state %in% c("Alaska","Hawaii"))
toplot$DMA.CODE[which(toplot$state == 'Texas' & toplot$name %in% c('Throckmorton','Knox'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Texas' & toplot$name %in% c('Baylor'))])
toplot$DMA.CODE[which(toplot$state == 'Texas' & toplot$name %in% c('Montague'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Texas' & toplot$name %in% c('Wise'))])
toplot$DMA.CODE[which(toplot$state == 'Colorado' & toplot$name %in% c('Cheyenne'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Colorado' & toplot$name %in% c('Kiowa'))])
toplot$DMA.CODE[which(toplot$state == 'Nebraska' & toplot$name %in% c('Hooker','Arthur','Blaine'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Nebraska' & toplot$name %in% c('Thomas'))])
toplot$DMA.CODE[which(toplot$state == 'Wyoming' & toplot$name %in% c('Johnson'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Wyoming' & toplot$name %in% c('Natrona'))])
toplot$DMA.CODE[which(toplot$state == 'Iowa' & toplot$name %in% c('Emmet'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Minnesota' & toplot$name %in% c('Martin'))])
toplot$DMA.CODE[which(toplot$state == 'Minnesota' & toplot$name %in% c('Nicollet'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Minnesota' & toplot$name %in% c('Sibley'))])
toplot$DMA.CODE[which(toplot$state == 'Illinois' & toplot$name %in% c('Edgar'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Illinois' & toplot$name %in% c('Clark'))])
toplot$DMA.CODE[which(toplot$state == 'Kentucky' & toplot$name %in% c('Adair','Cumberland'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Kentucky' & toplot$name %in% c('Metcalfe'))])
toplot$DMA.CODE[which(toplot$state == 'Ohio' & toplot$name %in% c('Athens'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Ohio' & toplot$name %in% c('Meigs'))])
toplot$DMA.CODE[which(toplot$state == 'Ohio' & toplot$name %in% c('Guernsey','Noble'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Ohio' & toplot$name %in% c('Belmont'))])
toplot$DMA.CODE[which(toplot$state == 'Arkansas' & toplot$name %in% c('Nevada'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Arkansas' & toplot$name %in% c('Columbia'))])
toplot$DMA.CODE[which(toplot$state == 'Tennessee' & toplot$name %in% c('Gibson','Chester'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Tennessee' & toplot$name %in% c('McNairy'))])
toplot$DMA.CODE[which(toplot$state == 'South Carolina' & toplot$name %in% c('Saluda'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'South Carolina' & toplot$name %in% c('Edgefield'))])
toplot$DMA.CODE[which(toplot$state == 'California' & toplot$name %in% c('Modoc'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'California' & toplot$name %in% c('Siskiyou'))])
toplot$DMA.CODE[which(toplot$state == 'Arkansas' & toplot$name %in% c('Johnson'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Arkansas' & toplot$name %in% c('Pope'))])
toplot$DMA.CODE[which(toplot$state == 'Colorado' & toplot$name %in% c('Custer'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Colorado' & toplot$name %in% c('Saguache'))])
toplot$DMA.CODE[which(toplot$state == 'Wyoming' & toplot$name %in% c('Lincoln'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Wyoming' & toplot$name %in% c('Teton'))])
toplot$DMA.CODE[which(toplot$state == 'Nebraska' & toplot$name %in% c('Banner'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Nebraska' & toplot$name %in% c('Morrill'))])
toplot$DMA.CODE[which(toplot$state == 'South Dakota' & toplot$name %in% c('Jones'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'South Dakota' & toplot$name %in% c('Lyman'))])
toplot$DMA.CODE[which(toplot$state == 'South Dakota' & toplot$name %in% c('Dewey','Campbell'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'South Dakota' & toplot$name %in% c('Corson'))])
toplot$DMA.CODE[which(toplot$state == 'Iowa' & toplot$name %in% c('Adams'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Iowa' & toplot$name %in% c('Montgomery'))])
toplot$DMA.CODE[which(toplot$state == 'Missouri' & toplot$name %in% c('Mercer'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Iowa' & toplot$name %in% c('Decatur'))])
toplot$DMA.CODE[which(toplot$state == 'Missouri' & toplot$name %in% c('Phelps'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Missouri' & toplot$name %in% c('Pulaski'))])
toplot$DMA.CODE[which(toplot$state == 'Missouri' & toplot$name %in% c('Reynolds'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Missouri' & toplot$name %in% c('Wayne'))])
toplot$DMA.CODE[which(toplot$state == 'Oklahoma' & toplot$name %in% c('Ottawa'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Kansas' & toplot$name %in% c('Cherokee'))])
toplot$DMA.CODE[which(toplot$state == 'Kentucky' & toplot$name %in% c('Magoffin'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Kentucky' & toplot$name %in% c('Johnson'))])
toplot$DMA.CODE[which(toplot$state == 'Pennsylvania' & toplot$name %in% c('Franklin'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Pennsylvania' & toplot$name %in% c('Fulton'))])
toplot$DMA.CODE[which(toplot$state == 'Ohio' & toplot$name %in% c('Auglaize'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Ohio' & toplot$name %in% c('Shelby'))])
toplot$DMA.CODE[which(toplot$state == 'Georgia' & toplot$name %in% c('Taylor'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Georgia' & toplot$name %in% c('Marion'))])
toplot$DMA.CODE[which(toplot$state == 'Illinois' & toplot$name %in% c('Cass'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Illinois' & toplot$name %in% c('Brown'))])
toplot$DMA.CODE[which(toplot$state == 'Tennessee' & toplot$name %in% c('Wayne'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Alabama' & toplot$name %in% c('Lauderdale'))])
toplot$DMA.CODE[which(toplot$state == 'Texas' & toplot$name %in% c('Red River','Camp'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Texas' & toplot$name %in% c('Titus'))])
toplot$DMA.CODE[which(toplot$state == 'Nebraska' & toplot$name %in% c('Butler','Pawnee'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Nebraska' & toplot$name %in% c('Johnson'))])
toplot$DMA.CODE[which(toplot$state == 'New Mexico' & toplot$name %in% c('Lea'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Texas' & toplot$name %in% c('Winkler'))])
toplot$DMA.CODE[which(toplot$state == 'Kansas' & toplot$name %in% c('Morton'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Oklahoma' & toplot$name %in% c('Texas'))])
toplot$DMA.CODE[which(toplot$state == 'Georgia' & toplot$name %in% c('Seminole'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'Georgia' & toplot$name %in% c('Early'))])
toplot$DMA.CODE[which(toplot$state == 'Virginia' & toplot$name %in% c('Grayson'))] <- unique(toplot$DMA.CODE[which(toplot$state == 'North Carolina' & toplot$name %in% c('Alleghany'))])

as.data.frame(toplot) %>% ungroup() %>% select(state,DMA.CODE) %>% distinct() %>%
  group_by(DMA.CODE) %>% summarise(n=n()) -> multiStateDMA

toplot %>% left_join(multiStateDMA %>% mutate(DMA.CODE,multiState = (n > 1)+0) %>% select(-n)) -> toplot



toplot$treated <- ifelse(toplot$state %in% c('Iowa','New Hampshire','South Carolina','Nevada',
                                             'Alabama','Arkansas','California','Colorado','Maine',
                                             'Massachusetts','Minnesota','North Carolina','Oklahoma',
                                             'Tennessee','Texas','Utah','Vermont','Virginia'),0,1)
comparisons <- as.data.frame(toplot) %>% select(DMA.CODE,multiState,stab,treated) %>%
  filter(multiState == 1) %>%
  group_by(DMA.CODE) %>%
  summarise(IDvar = mean(treated)) %>%
  filter(IDvar > 0 & IDvar < 1)


pdf('../Figures/figure2.pdf',width = 16,height = 12)
toplot %>%
  left_join(comparisons) %>%
  mutate(IDvar = ifelse(is.na(IDvar),0,IDvar)) %>%
  mutate(treated2 = ifelse(treated == 1 & IDvar > 0,4,
                           ifelse(treated == 0 & IDvar > 0,3,
                                  ifelse(treated == 1 & IDvar == 0,2,1)))) %>%
  mutate(treated2 = ifelse(IDvar > 0,treated2,'drop')) %>%
  ggplot() + 
  geom_sf(aes(fill = factor(treated2),color = treated2),size = .1) + 
  scale_fill_manual(name = 'Primary Date',
                    values = c('4' = '#1e1e1e','3' = '#878787',
                               '2' = 'white','1' = 'white'),
                    breaks = c('4','3'),
                    labels = c('4' = 'Post-Outbreak','3' = 'Pre-Outbreak')) + 
  scale_color_manual(guide = F,values = c('4' = 'white','3' = 'white','drop' = 'grey80'),na.translate = F) +
  geom_sf(data = maps$DMA,color = 'black',fill = NA) +
  theme_map()
dev.off()



# Figure 3
p1 <- finalDat %>%
  mutate(treat = ifelse(county_March17Cases > 0,1,0),
         post = ifelse(date > as.Date('2020-03-10'),'Post March 10','Pre March 17')) %>%
  group_by(treat,post) %>%
  summarise(outcome = mean(pcttw_sanders,na.rm=T),
            sd = sd(pcttw_sanders,na.rm=T)) %>%
  ggplot(aes(x = factor(post,levels = c("Pre March 17","Post March 10")),y = outcome,
             fill = factor(treat))) + 
  stat_summary(fun.y = mean,geom = "bar",position = position_dodge(width = .9),
               size = 3,alpha = .7) + 
  scale_fill_manual(name = '',values = c('0' = 'grey80','1' = 'grey30'),
                    labels = c('0' = 'Insulated on March 17th','1' = 'Exposed on March 17th')) + 
  theme_ridges() + xlab('Period') + ylab('Sanders Two-Way Vote Share')

p2 <- finalDat %>%
  mutate(post = ifelse(date > as.Date('2020-03-10'),'Post','Pre'),
         logcases = log(county_March17Cases+1)) %>%
  ggplot(aes(x = logcases,y = pcttw_sanders,color = factor(post,levels = c("Pre","Post")),size = turnout_20,weight = log(turnout_20+1))) + 
  geom_point(alpha = .6) + 
  geom_smooth(method = 'lm') +
  scale_color_manual(name = "Period",values = c('Post' = 'grey30',"Pre" = "grey70")) + 
  scale_size_continuous(guide = F) + 
  theme_ridges() + xlab("March 17 Cases (logged)") + ylab('Sanders Two-Way Vote Share')


pdf('../Figures/figure3.pdf',width = 7,height = 5)
grid.arrange(p1 + theme(legend.position = 'bottom'),
             p2 + theme(legend.position = 'bottom'),ncol = 2)
dev.off()



# SI Table 1
covariates <- c("sc_CTY_LTHS","sc_CTY_CollUp",'caucus_switch','caucus',
                "sc_CTY_LT30yo","sc_CTY_60Up",
                "sc_CTY_Below_poverty_level_AGE_18_64","sc_CTY_Female_hher_no_husbandhh",
                "sc_CTY_Unem_rate_pop_16_over","sc_CTY_Labor_Force_Part_Rate_pop_16_over",
                "sc_CTY_Manufactur","sc_CTY_Md_inc_hhs",
                "sc_CTY_POPPCT_RURAL","sc_CTY_Speak_only_English","sc_CTY_White","sc_CTY_Black_or_African_American",
                "ln_CTY_tot_pop","sc_turnout_pct_20")

maps$county %>% left_join(multiStateDMA %>% mutate(DMA.CODE,multiState = (n > 1)+0) %>% select(-n)) -> maps$county
as.data.frame(maps$county) %>% ungroup() %>% select(FIPS,multiState) %>% 
  distinct() %>%
  left_join(finalDat,by = c("FIPS" = "stcou")) %>%
  select(multiState,gsub("sc_|ln_","",covariates),pct_sanders16) -> tocompare

zeroOne <- function(x) {
  return(x > 0 & x < 1)
}

div100 <- function(x) {
  return(x/100)
}
tocompare %>% 
  mutate(turnout_pct_20 = turnout_pct_20*100) %>%
  gather(variable,value,-multiState) %>%
  filter(complete.cases(.)) %>%
  group_by(multiState,variable) %>%
  summarise(value = list(value)) %>%
  spread(multiState,value,sep = "_") %>% 
  group_by(variable) %>%
  mutate(p_value = round(t.test(unlist(multiState_0), unlist(multiState_1))$p.value,2),
         t_value = round(t.test(unlist(multiState_0), unlist(multiState_1))$statistic,1),
         avg_0 = round(mean(unlist(multiState_0),na.rm=T),1),
         avg_1 = round(mean(unlist(multiState_1),na.rm=T),1),
         sd_0 = round(sd(unlist(multiState_0),na.rm=T),1),
         sd_1 = round(sd(unlist(multiState_1),na.rm=T),1)) %>%
  select(variable,matches("avg|sd"),p_value,t_value) %>%
  arrange(t_value) -> dma_fe_balance
dma_fe_balance$variable <- gsub("_"," ",gsub("CTY_","",dma_fe_balance$variable))
stargazer(data.frame(dma_fe_balance %>% select(-matches("sd_"))),summary = F,digits = 1,out = '../Tables/SI-table1.tex')
