# File name: table3_SIfigure18.R
# In: 
#   - /Data/primary_data.RData: Primary election results for the house of representatives, linked with district-level cases and deaths
# Out: 
#   - /Tables/table3.tex
#   - /Figures/SI_figure18.pdf

require(lme4)
require(lfe)
require(MatchIt)
require(WeightIt)
require(tjbal)
require(optmatch)
require(stargazer)
require(cobalt)
require(tidyverse)
require(stargazer)
require(gridExtra)
require(ggridges)
require(ggrepel)

rm(list = ls())
gc()

setwd("C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Data\\replication_data.RData")
load('./Data/primary_data.RData')

# Table 3
summary(mVScaseEXT <- felm(voteSh ~ extreme*log(casesSum+1) | stcd | 0 | stcd,toAnal))
summary(mVScaseJST <- felm(voteSh ~ justice*log(casesSum+1) | stcd | 0 | stcd,toAnal))
summary(mVScaseTEA <- felm(voteSh ~ tea*log(casesSum+1) | stcd | 0 | stcd,toAnal))

summary(mVSdthEXT <- felm(voteSh ~ extreme*log(deathsSum+1) | stcd | 0 | stcd,toAnal))
summary(mVSdthJST <- felm(voteSh ~ justice*log(deathsSum+1) | stcd | 0 | stcd,toAnal))
summary(mVSdthTEA <- felm(voteSh ~ tea*log(deathsSum+1) | stcd | 0 | stcd,toAnal))

summary(mVTcaseEXT <- felm(log(votes+1) ~ extreme*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,toAnal %>% filter(votes != 0)))
summary(mVTcaseJST <- felm(log(votes+1) ~ justice*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,toAnal %>% filter(votes != 0)))
summary(mVTcaseTEA <- felm(log(votes+1) ~ tea*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,toAnal %>% filter(votes != 0)))

summary(mVTdthEXT <- felm(log(votes+1) ~ extreme*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,toAnal %>% filter(votes != 0)))
summary(mVTdthJST <- felm(log(votes+1) ~ justice*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,toAnal %>% filter(votes != 0)))
summary(mVTdthTEA <- felm(log(votes+1) ~ tea*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,toAnal %>% filter(votes != 0)))

regs <- list()
for(m in objects(pattern = '^mVScase')) {
  regs[[m]] <- get(m)
  rownames(regs[[m]]$coefficients) <- rownames(regs[[m]]$beta) <- gsub(':',' X ',
                                                                       gsub('tea|justice|extreme','Anti-Est.',
                                        gsub('log\\(deathsSum \\+ 1\\)','Deaths (ln)',
                                             gsub('log\\(casesSum \\+ 1\\)','Cases (ln)',
                                                  gsub('log\\(totVotes \\+ 1\\)','Total Votes (ln)',rownames(regs[[m]]$coefficients))))))
}

stargazer(regs,keep.stat = c('n','rsq'),
          star.char = c('\\dag','*','**','***'),
          star.cutoffs = c(.1,.05,.01,.001),
          add.lines = list(c('District FE','Y','Y','Y','Y')), out = '../Tables/table3.tex')

# SI Figure 18
toplot <- toAnal %>%
  filter(date < as.Date('2020-11-01'),
         votes > 0)

toplot$lab <- NA
for(d in unique(toplot$date)) {
  toplot$lab[which(toplot$date == d)] <- paste(unique(toplot$stab[which(toplot$date == d)]),collapse = '\n')
}



p1 <- toplot %>%
  ggplot(aes(x = date,y = voteSh,color = factor(extreme),size = casesSum,weight = log(casesSum+1),
             alpha = factor(extreme),
             label = lab)) + 
  geom_jitter(width = 0) + 
  geom_smooth(method = 'lm') + 
  geom_text(data = toplot %>%
              group_by(date) %>%
              slice(1) %>%
              filter(!stab %in% c('HI','TN')) %>%
              ungroup() %>%
              mutate(voteSh = 1.01,
                     date = date - 3),
            size = 2.5,hjust = 0,vjust = 0,color = 'black',alpha = .8) + 
  theme_ridges() + 
  scale_color_manual(name = 'Type',values = c(`0` = 'grey60',`1` = 'black'),label = c('Mainstream','Extreme')) + 
  scale_alpha_manual(guide = F,values = c(`0` = .15,`1` = .6)) + 
  scale_size_continuous(guide = F) + 
  xlab('Date') + ylab('Vote Share') + 
  scale_y_continuous(breaks = seq(0,1,by = .25),limits = c(0,1.25)) +
  theme(legend.position = 'bottom')


p2 <- toplot %>%
  ggplot(aes(x = log(casesSum+1),y = voteSh,color = factor(extreme))) + 
  geom_jitter(alpha = .2,width = .2) + 
  geom_smooth(method = 'lm') + 
  theme_ridges() + 
  scale_color_manual(name = 'Type',values = c(`0` = 'grey60',`1` = 'black'),label = c('Mainstream','Extreme')) + 
  scale_alpha_manual(guide = F,values = c(`0` = .15,`1` = .6)) + 
  scale_size_continuous(guide = F) + 
  xlab('Logged Cases') + ylab('Vote Share') + 
  scale_y_continuous(breaks = seq(0,1,by = .25),limits = c(0,1.25)) +
  theme(legend.position = 'bottom')

pdf('./Figures/SI_figure18.pdf',width = 12,height = 8)
grid.arrange(p1,p2,ncol = 2)
dev.off()
