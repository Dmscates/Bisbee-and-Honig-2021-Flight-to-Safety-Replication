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


load('../Data/primary_data.RData')

# Dem subset
summary(mVScaseEXTold.D <- felm(voteSh ~ extreme*log(casesSum+1) | stcd | 0 | stcd,toAnal, subset=(party =="DEM")))
summary(mVScaseEXTold.G <- felm(voteSh ~ extreme*log(casesSum+1) | stcd | 0 | stcd,toAnal, subset=(party =="GOP")))


regsG1 <- list()
for(m in objects(pattern = '^mVScaseEXTold')) {
  regsG1[[m]] <- get(m)
  rownames(regsG1[[m]]$coefficients) <- rownames(regsG1[[m]]$beta) <- gsub(':',' X ',
                                                                       gsub('extreme','Anti-Est.',
                                                                            gsub('log\\(deathsSum \\+ 1\\)','Deaths (ln)',
                                                                                 gsub('log\\(casesSum \\+ 1\\)','Cases (ln)',
                                                                                      gsub('voteSh','VoteShare',rownames(regsG1[[m]]$coefficients))))))
}

stargazer(regsG1,keep.stat = c('n','rsq'),
          star.char = c('\\dag','*','**','***'),
          star.cutoffs = c(.1,.05,.01,.001),
          add.lines = list(c('District FE','Y','Y','Y','Y')), out = '../Tables/table3.tex')

#R subset
regsG2 <- list()
for(m in objects(pattern = '^mVScaseEXT.GOPold')) {
  regsG2[[m]] <- get(m)
  rownames(regsG2[[m]]$coefficients) <- rownames(regsG2[[m]]$beta) <- gsub(':',' X ',
                                                                           gsub('tea|extreme|trump|RW_either|RW_both','Anti-Est.',
                                                                                gsub('log\\(deathsSum \\+ 1\\)','Deaths (ln)',
                                                                                     gsub('log\\(casesSum \\+ 1\\)','Cases (ln)',
                                                                                          gsub('voteSh','VoteShare',rownames(regsG2[[m]]$coefficients))))))
}
stargazer(regsG2,keep.stat = c('n','rsq'),
          star.char = c('\\dag','*','**','***'),
          star.cutoffs = c(.1,.05,.01,.001),
          add.lines = list(c('District FE','Y','Y','Y','Y')), out = '../Tables/table3.tex')
