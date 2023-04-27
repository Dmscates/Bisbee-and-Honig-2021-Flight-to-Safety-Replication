
load('../I4RBisbeeHonig/camdidates.csv')

#need to add in load candidates final dataset command 
extreme <- candidates$`Extreme_ new`
justice <- candidates$justice
tea <- candidates$tea
trump <- candidates$trump
RWeither <- candidates$RW_either
RWboth <- candidates$RW_both

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

#Table 3 - same measures, updated
#Cases + vote share 
summary(mVScaseEXT <- felm(voteSh ~ extreme*log(casesSum+1) | stcd | 0 | stcd,candidates))
summary(mVScaseJST <- felm(voteSh ~ justice*log(casesSum+1) | stcd | 0 | stcd,candidates))
summary(mVScaseTEA <- felm(voteSh ~ tea*log(casesSum+1) | stcd | 0 | stcd,candidates))

#Deaths + Vote Share
summary(mVSdthEXT <- felm(voteSh ~ extreme*log(deathsSum+1) | stcd | 0 | stcd,candidates))
summary(mVSdthJST <- felm(voteSh ~ justice*log(deathsSum+1) | stcd | 0 | stcd,candidates))
summary(mVSdthTEA <- felm(voteSh ~ tea*log(deathsSum+1) | stcd | 0 | stcd, candidates))

#Cases + logged Vote Totals
summary(mVTcaseEXT <- felm(log(votes+1) ~ extreme*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTcaseJST <- felm(log(votes+1) ~ justice*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTcaseTEA <- felm(log(votes+1) ~ tea*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))

#Deaths + logged Vote Totals
summary(mVTdthEXT <- felm(log(votes+1) ~ extreme*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTdthJST <- felm(log(votes+1) ~ justice*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTdthTEA <- felm(log(votes+1) ~ tea*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))


regs1 <- list()
for(m in objects(pattern = '^mVScase')) {
  regs1[[m]] <- get(m)
  rownames(regs1[[m]]$coefficients) <- rownames(regs1[[m]]$beta) <- gsub(':',' X ',
                                                                       gsub('tea|justice|extreme','Anti-Est.',
                                                                            gsub('log\\(deathsSum \\+ 1\\)','Deaths (ln)',
                                                                                 gsub('log\\(casesSum \\+ 1\\)','Cases (ln)',
                                                                                      gsub('log\\(totVotes \\+ 1\\)','Total Votes (ln)',rownames(regs1[[m]]$coefficients))))))
}

stargazer(regs1,keep.stat = c('n','rsq'),
          star.char = c('\\dag','*','**','***'),
          star.cutoffs = c(.1,.05,.01,.001),
          add.lines = list(c('District FE','Y','Y','Y','Y')), out = '../Tables/table3.tex')


# Table 2 - new measures, updated data

#Cases + vote share
summary(mVScaseEXT <- felm(voteSh ~ extreme*log(casesSum+1) | stcd | 0 | stcd,candidates))
summary(mVScaseJST <- felm(voteSh ~ justice*log(casesSum+1) | stcd | 0 | stcd,candidates))
summary(mVScaseTEA <- felm(voteSh ~ tea*log(casesSum+1) | stcd | 0 | stcd,candidates))
summary(mVScaseTRUMP <- felm(voteSh ~ trump*log(casesSum+1) | stcd | 0 | stcd,candidates))
summary(mVScaseRWeither <- felm(voteSh ~ RW_either*log(casesSum+1) | stcd | 0 | stcd,candidates))
summary(mVScaseRWboth <- felm(voteSh ~ RW_both*log(casesSum+1) | stcd | 0 | stcd,candidates))

#Deaths + Vote Share
summary(mVSdthEXT <- felm(voteSh ~ extreme*log(deathsSum+1) | stcd | 0 | stcd,candidates))
summary(mVSdthJST <- felm(voteSh ~ justice*log(deathsSum+1) | stcd | 0 | stcd,candidates))
summary(mVSdthTEA <- felm(voteSh ~ tea*log(deathsSum+1) | stcd | 0 | stcd, candidates))
summary(mVSdthTRUMP <- felm(voteSh ~ trump*log(deathsSum+1) | stcd | 0 | stcd, candidates))
summary(mVSdthRWeither <- felm(voteSh ~ RW_either*log(deathsSum+1) | stcd | 0 | stcd, candidates))
summary(mVSdthRWboth <- felm(voteSh ~ RW_both*log(deathsSum+1) | stcd | 0 | stcd, candidates))

#Cases + logged Vote Totals
summary(mVTcaseEXT <- felm(log(votes+1) ~ extreme*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTcaseJST <- felm(log(votes+1) ~ justice*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTcaseTEA <- felm(log(votes+1) ~ tea*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTcaseTRUMP <- felm(log(votes+1) ~ trump*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTcaseRWeither <- felm(log(votes+1) ~ RW_either*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTcaseRWboth <- felm(log(votes+1) ~ RW_both*log(casesSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))

#Deaths + logged Vote Totals
summary(mVTdthEXT <- felm(log(votes+1) ~ extreme*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTdthJST <- felm(log(votes+1) ~ justice*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTdthTEA <- felm(log(votes+1) ~ tea*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTdthTRUMP <- felm(log(votes+1) ~ trump*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTdthRWeither <- felm(log(votes+1) ~ RW_either*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))
summary(mVTdthRWboth <- felm(log(votes+1) ~ RW_both*log(deathsSum+1) + log(totVotes+1) | stcd | 0 | stcd,candidates %>% filter(votes != 0)))

regs <- list()
for(m in objects(pattern = '^mVScase')) {
  regs[[m]] <- get(m)
  rownames(regs[[m]]$coefficients) <- rownames(regs[[m]]$beta) <- gsub(':',' X ',
                                                                       gsub('tea|justice|extreme|trump|RW_either|RW_both','Anti-Est.',
                                                                            gsub('log\\(deathsSum \\+ 1\\)','Deaths (ln)',
                                                                                 gsub('log\\(casesSum \\+ 1\\)','Cases (ln)',
                                                                                      gsub('log\\(totVotes \\+ 1\\)','Total Votes (ln)',rownames(regs[[m]]$coefficients))))))
}

stargazer(regs,keep.stat = c('n','rsq'),
          star.char = c('\\dag','*','**','***'),
          star.cutoffs = c(.1,.05,.01,.001),
          add.lines = list(c('District FE','Y','Y','Y','Y')), out = '../Tables/table3.tex')
stargazer(regs)
