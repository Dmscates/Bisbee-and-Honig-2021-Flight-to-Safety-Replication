# File name: figure7_SIfigure2_SIfigure17.R
# In: 
#   - /Data/gtrends_data.RData: DMA-level data on google searches for "coronavirus" linked with DMA-level cases and deaths
#   - /Data/mobility_data.RData: county-level data on physical mobility linked with county-level cases and deaths
#   - /Data/pew/biden_sanders_ideo_feb_march_PEW.csv: Pew survey data on public's placement of Sanders and Biden on an ideological scale
# Out: 
#   - /Figures/figure7.pdf
#   - /Figures/SI_figure2.pdf
#   - /Figures/SI_figure17.pdf

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

# Figure 7
load("C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Data\\gtrends_data.RData")

coefs <- NULL
for(day in as.character(unique(gtrends_cases$date))[5:95]) {
  tmp <- summary(lmer(hits ~ log(cases+1) + (1|stab),gtrends_cases %>% filter(date == as.Date(day))))$coefficients
  if(nrow(tmp) == 1) { next }
  txttmp <- unique(trimws(gsub("coronavirus|the","",queries[[day]]$value)))
  if(any(txttmp == "")) {
    txttmp <- txttmp[-which(txttmp == "")]
  }
  txttmp <- txttmp[1:3]
  if(any(txttmp == "DGSGsymptoms")) {
    txttmp <- ifelse(txttmp == "symptoms",'"symptoms"',paste0('phantom("',txttmp,'")'))
    txttmp <- paste(txttmp,collapse = " ")
    txttmp <- gsub(' "symptoms"',' * "symptoms"',txttmp)
    txttmp <- gsub('"symptoms" ','"symptoms" * ',txttmp)
    txttmp <- gsub('"\\) phantom\\("', ' ',txttmp)
  } else {
    txttmp <- paste(txttmp,collapse = " ")
  }
  tq <- txttmp
  
  coefs <- bind_rows(coefs,data.frame(t(tmp[2,]),date = as.Date(day),topQ = tq,stringsAsFactors = F))
}

pdf("../figures/figure7.pdf",width = 7,height = 5)
coefs %>%
  ggplot(aes(x = date,y = Estimate)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = Estimate - 2*Std..Error,ymax = Estimate + 2*Std..Error)) + 
  theme_bw() + 
  geom_hline(yintercept = 0) + 
  theme_bw() +
  geom_vline(xintercept = as.Date(c("2020-02-03","2020-02-11","2020-02-22","2020-02-29",
                                    "2020-03-03","2020-03-10","2020-03-17")),linetype = "dashed",alpha = .3) + 
  annotate(geom = "text",label = c("IA","NH","NV","SC","SupTues","Mar 10th","AZ, FL, IL"),
           x = as.Date(c("2020-02-03","2020-02-11","2020-02-22","2020-02-29",
                         "2020-03-03","2020-03-10","2020-03-17")),y = -Inf,angle = 90,vjust = 1,hjust = 0) + 
  theme_ridges() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlab("Date") + ylab("Coefficient on Exposure") + 
  annotate(geom = "text",label = coefs$topQ,y = coefs$Estimate + 2*coefs$Std..Error + 1,x = coefs$date,angle = 90,
           parse = F,hjust = 0,vjust = .3,size = 2,color = "grey30") + 
  ylim(c(-10,45))
dev.off()



# SI Figure 2
load("C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Data\\mobility_data.RData")


dma_coefs <- cty_coefs <- NULL
for(week in as.character(unique(mobility_cases$weeks))[-13]) {
  tmp <- summary(felm(raw ~ log(cases+1) | stab | 0 | stab,mobility_cases %>% filter(weeks == as.Date(week))))$coefficients[1,1:3]
  names(tmp) <- c("Estimate","Std..Error","tstat")
  cty_coefs <- bind_rows(cty_coefs,
                         data.frame(t(tmp),date = as.Date(week),stringsAsFactors = F))
}

pdf("../figures/SI_figure2.pdf",width= 7,height = 5)
cty_coefs %>%
  ggplot(aes(x = date,y = Estimate)) + 
  geom_point(position = position_dodge(width = 4)) + 
  geom_errorbar(aes(ymin = Estimate - 2*Std..Error,
                    ymax = Estimate + 2*Std..Error),width = 1) + 
  geom_hline(yintercept = 0) + 
  theme_bw()  +
  geom_vline(xintercept = as.Date(c("2020-02-03","2020-02-11","2020-02-22","2020-02-29",
                                    "2020-03-03","2020-03-10","2020-03-17")),linetype = "dashed",alpha = .5) + 
  annotate(geom = "text",label = c("IA","NH","NV","SC","SupTues","Mar 10th","AZ, FL, IL"),
           x = as.Date(c("2020-02-03","2020-02-11","2020-02-22","2020-02-29",
                         "2020-03-03","2020-03-10","2020-03-17")),y = Inf,angle = 90,vjust = 1,hjust = 1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlab("Date") + ylab("Coefficient on Exposure") + 
  theme_ridges()
dev.off()







# SI Figure 17
toplot <- read.csv("C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Data\\pew\\biden_sanders_ideo_feb_march_PEW.csv",stringsAsFactors = F)

pdf('../figures/SI_figure17.pdf',width = 8,height = 4)
toplot %>%
  mutate_at(vars(matches('\\.|Refused|Moderate')),function(x) as.numeric(gsub('%','',x))) %>%
  gather(key,value,-Group,-Candidate) %>%
  filter(!grepl('Refused|Not\\.sure',key)) %>% 
  mutate(key = factor(gsub('\\.','\n',key),levels = c('Very\nliberal','Mostly\nliberal','Slightly\nliberal',
                                                     'Moderate','Slightly\nconservative','Mostly\nconservative','Very\nconservative'))) %>%
  filter(grepl('liberal dem',tolower(Group))) %>%
  ggplot(aes(x = key,y = value,group = Candidate,fill = Candidate)) + 
  geom_bar(stat = 'identity',position= position_dodge(.8),alpha = .6) + 
  ylab('% Agreeing') + xlab('Candidate Ideology') + 
  scale_fill_manual(name = '',values = c('Biden' = 'grey40','Sanders' = 'grey70')) + 
  theme_ridges() + 
  theme(legend.position = 'bottom')
dev.off()
