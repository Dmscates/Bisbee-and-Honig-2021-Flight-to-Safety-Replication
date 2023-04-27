# File name: table1_SItable3_SItable5_SItable4_SIfigure13_SIfigure14.R
# In: 
#   - replication_data.RData
# Out: 
#   - /Tables/table1.tex
#   - /Tables/SI-table3.tex
#   - /Figures/SI_figure13.pdf
#   - /Tables/SI-table4.tex
#   - /Tables/SI-table5.tex
#   - /Figures/SI_figure14.pdf

require(lme4)
#install.packages("lme4")
require(lfe)
#install.packages("lfe")
require(MatchIt)
#install.packages("MatchIt")
require(WeightIt)
#install.packages("WeightIt")
require(tjbal)
#install.packages("tjbal")
#install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
#devtools::install_github('chadhazlett/kbal')
#devtools::install_github('xuyiqing/tjbal')
require(optmatch)
#install.packages("optmatch")
require(stargazer)
require(cobalt)
require(tidyverse)
require(CBPS)
#install.packages("CBPS")
require(optweight)
#install.packages("optweight")

rm(list = ls())
gc()
setwd("C://Users//danie//Dropbox//Replication//dataverse_files//Replication//Daniel Reproduction")
####################################################################################################################### Loading data
load("C:/Users/danie/Dropbox/Replication/dataverse_files/Replication/Data/replication_data.RData")


####################################################################################################################### Loading functions
source("C://Users//danie//Dropbox//Replication//dataverse_files//Replication//code//helper_functions.R")





####################################################################################################################### Preparing variables
Y <- c('pct_sanders16',paste0(c("pct_","VAP_","pcttw_"),"sanders"))
D <- unlist(lapply(c("sc_","ln_"),function(x) paste0(x,paste0(c("county_","DMA_","state_"),"cases"))))
FE <- c("0","DMA_CODE","date")
covariates <- c("sc_CTY_LTHS","sc_CTY_CollUp",
                "sc_CTY_LT30yo","sc_CTY_60Up",
                "sc_CTY_Below_poverty_level_AGE_18_64","sc_CTY_Female_hher_no_husbandhh",
                "sc_CTY_Unem_rate_pop_16_over","sc_CTY_Labor_Force_Part_Rate_pop_16_over",
                "sc_CTY_Manufactur","sc_CTY_Md_inc_hhs",
                "sc_CTY_POPPCT_RURAL","sc_CTY_Speak_only_English","sc_CTY_White","sc_CTY_Black_or_African_American",
                "ln_CTY_tot_pop","sc_turnout_pct_20",'caucus_switch','caucus')



# Function for generating matched data + weighted data
wgtMatchFun <- function(dat,formMatch,formWgt,matchMethod = 'nearest',wgtMethod = 'cbps') {
  m.out <- matchit(formula = formula(formMatch),
                   data = dat,
                   method = matchMethod,
                   distance = "mahalanobis")
  
  m.data <- match.data(m.out)
  W.out <- weightit(formula = formula(formWgt),
                    data = dat, 
                    estimand = "ATT", 
                    method = wgtMethod)
  
  # Balance tables
  balt.pre <- bal.tab(formula(formWgt),
                      data = dat, 
                      estimand = "ATT",
                      m.threshold = .05)
  
  balt.post <- bal.tab(W.out, m.threshold = .05, disp.v.ratio = TRUE)
  unm <- data.frame(Covs = gsub('_$','',rownames(balt.pre$Balance)),
                    balt.pre$Balance %>% 
                      mutate(Diff_Unm = round(Diff.Un,2),
                             Bal_Test_Unm = M.Threshold.Un)) %>%
    dplyr::select(Covs,Diff_Unm,Bal_Test_Unm)
  
  match <- data.frame(Covs = rownames(balt.post$Balance),
                      balt.post$Balance %>% mutate(Diff_Match = round(Diff.Adj,2),
                                                   Bal_Test_Match = M.Threshold)) %>%
    dplyr::select(Covs,Diff_Match,Bal_Test_Match)
  
  
  balTab <- match %>% left_join(unm) %>% dplyr::select(Covs,Diff_Unm,Bal_Test_Unm,Diff_Match,Bal_Test_Match)
  balPlot <- qqprep(x = m.out)
  
 return(list(balTab = balTab,
             m.data = m.data,
             W.out = W.out,
             dat = dat,
             balPlot = balPlot))
}


set.seed(123)
finalDat$treatBin <- ifelse(finalDat$DMA_cases > 1,1,0)

# Full Sample Regressions: Nearest neighbor + CBPS
summary(mFullRaw <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                         paste(c(covariates,"pcttw_sanders16"),collapse = " + "),
                         " | DMA_CODE | 0 | DMA_CODE")),
                         finalDat))


foranal.weight <- finalDat %>% 
  dplyr::select(pcttw_sanders,DMA_cases,treatBin,DMA_CODE,date,stab,covariates,pcttw_sanders16) %>% 
  filter(complete.cases(.))

fullPrepped <- wgtMatchFun(dat = foranal.weight,
                           formMatch = paste0('treatBin ~ ',paste(covariates,collapse = " + ")),
                           formWgt = paste0('treatBin ~ ',paste(covariates,collapse = " + ")),
                           matchMethod = 'nearest',
                           wgtMethod = 'cbps')

summary(mFullMatch <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                        paste(c(covariates,"pcttw_sanders16"),collapse = " + "),
                                        " | DMA_CODE | 0 | DMA_CODE")),
                           fullPrepped$m.data,weights = fullPrepped$m.data$weights))


summary(mFullWgt <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                             paste(c(covariates,"pcttw_sanders16"),collapse = " + "),
                                           " | DMA_CODE | 0 | DMA_CODE")),
                         fullPrepped$dat,weights = fullPrepped$W.out$weights))


# March + April Regressions
foranal.weight <- finalDat %>% 
  dplyr::select(pcttw_sanders,DMA_cases,treatBin,
         DMA_CODE,date,stab,covariates,
         pcttw_sanders16) %>% 
  filter(complete.cases(.),
         date >= as.Date('2020-03-01'))

covariates <- covariates[-which(covariates == 'caucus')]

marPrepped <- wgtMatchFun(dat = foranal.weight,
                          formMatch = paste0('treatBin ~ ',paste(covariates,collapse = " + ")),
                          formWgt = paste0('treatBin ~ ',paste(covariates,collapse = " + ")),
                          matchMethod = 'nearest',
                          wgtMethod = 'cbps')

summary(mMarRaw <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                           paste(c(covariates,"pcttw_sanders16"),collapse = " + ")," | DMA_CODE | 0 | DMA_CODE")),
                         finalDat %>% filter(date >= as.Date("2020-03-01"))))

summary(mMarMatch <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                             paste(c(covariates,"pcttw_sanders16"),collapse = " + ")," | DMA_CODE | 0 | DMA_CODE")),
                          marPrepped$m.data,weights = marPrepped$m.data$weights))


summary(mMarWgt <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                           paste(c(covariates,"pcttw_sanders16"),collapse = " + ")," | DMA_CODE | 0 | DMA_CODE")),
                        marPrepped$dat,weights = marPrepped$W.out$weights))


# Table 1
covars <- rownames(mFullRaw$coefficients)
labs <- gsub('Treatbin','Exposure Dummy',gsub('Turnout pct 20','Turnout 2020',gsub('Pcttw sanders16','Sanders 2016',gsub('Caucus$','Caucus dummy',gsub("Lths","LTHS",gsub("Collup","Coll. Up",gsub("Md inc HH","Med HH Inc",gsub("Sc ","",gsub(" or african american|band$","",Hmisc::capitalize(trimws(gsub("manufactur","manufacturing",gsub("hher|hhs","HH",gsub("labor force part rate","LFPR",gsub("bachelor s","bachelor's",gsub("age 18 64|pop 16 over|hh$|poppct","",gsub("\\_"," ",gsub("^sc_|^.*cty\\_","",tolower(covars)))))))))))))))))))

prepTable <- function(tab,keeps = c('Exposure Dummy','Turnout 2020','Sanders 2016',
                                    'Caucus dummy','Caucus switch'),tops = 1:14,bottoms = (length(tab) - 8):length(tab)) {
  inds <- NULL
  for(keep in keeps) {
    tmpInds <- which(grepl(keep,tab))
    for(i in tmpInds) {
      inds <- c(inds,i:(i+2))
    }
  }
  inds <- unique(c(tops,inds,bottoms))
  
  return(tab[inds])
}

tex <- stargazer(mFullRaw,mFullMatch,mFullWgt,
          mMarRaw,mMarMatch,mMarWgt,
          keep.stat = c("n","rsq"),
          star.cutoffs = c(.1,.05,.01,.001),
          star.char = c('\\dag','*','**','***'),
          covariate.labels = labs)

cat(paste(prepTable(tex,keeps = c('Exposure Dummy','Turnout 2020','Sanders 2016',
                            'Caucus switch','Caucus dummy')),
          collapse = '\n'),file = "C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Daniel Reproduction\\table1.tex")



# SI Table 3
stargazer(marPrepped$balTab,summary = F,out = '../Tables/SI-table3.tex')



# SI Figure 13
toplot <- fullPrepped$balPlot$toplot
# rr <- marPrepped$balPlot$rr

dist2d <- function(x,y) {
  m <- cbind(c(-1,-1),c(x,y))
  abs(det(m))/sqrt(sum(c(-1,-1)^2))
}
vdist2d <- Vectorize(dist2d,vectorize.args = c("x","y"))
toplot$type <- as.character(toplot$type)
toplot$cov <- as.character(toplot$cov)
toplot <- toplot %>% bind_rows(toplot %>% group_by(cov) %>% summarise(x = min(x,y),y = min(x,y)),
                               toplot %>% group_by(cov) %>% summarise(x = max(x,y),y = max(x,y)))


toplot$cov <- Hmisc::capitalize(tolower(trimws(gsub("Manufactur","Manufacturing",gsub("Labor Force Part Rate","LFPR",gsub("hher|hhs","HH",gsub("Bachelor s","Bachelor's",gsub("_"," ",gsub("CTY_|AGE_18_64|pop_16_over|hh$|POPPCT","",toplot$cov)))))))))
toplot %>% group_by(type,cov) %>%
  summarise(eucdist = mean(vdist2d(x = x,y= y))) %>%
  filter(!is.na(type)) -> sum.euc



pdf("../SI_figure13.pdf",width = 7,height = 7)
toplot %>%
  filter(!grepl('caucus',tolower(cov))) %>%
  ggplot(aes(x = x,y = y,color = factor(type,levels = c("Raw","Matched")),
             text = paste0(type,"\nEuc Dist = ",round(vdist2d(x = x,y = y),2)))) +
  geom_point(alpha = .3,size = .5) +
  geom_abline() +
  geom_abline(aes(intercept = (rrlb - rrub) * 0.1,slope = 1),linetype = "dashed") +
  geom_abline(aes(intercept = -(rrlb - rrub) * 0.1,slope = 1),linetype = "dashed") +
  theme_bw() +
  xlab("Control") + ylab("Treated") +
  theme(legend.position = "bottom") +
  facet_wrap(~cov,scales = "free") +
  scale_color_discrete("",na.translate = F) +
  guides(color = guide_legend(override.aes = list(size = 5,alpha = 1)))
dev.off()








# SI Tables 4-5 and Figure 14
# Full Sample Regressions: CEM + optweights
summary(mFullRaw <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                           paste(c(covariates,"pcttw_sanders16"),collapse = " + "),
                                           " | DMA_CODE | 0 | DMA_CODE")),
                         finalDat))


foranal.weight <- finalDat %>% dplyr::select(pcttw_sanders,DMA_cases,treatBin,DMA_CODE,date,stab,covariates,pcttw_sanders16) %>% filter(complete.cases(.))
fullPrepped <- wgtMatchFun(dat = foranal.weight,
                           formMatch = paste0('treatBin ~ ',paste(covariates[c(4,2,10,12,7,13)],collapse = " + ")),
                           formWgt = paste0('treatBin ~ ',paste(covariates,collapse = " + ")),
                           matchMethod = 'cem',
                           wgtMethod = 'optweight')

summary(mFullMatch <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                             paste(c(covariates,"pcttw_sanders16"),collapse = " + "),
                                             " | DMA_CODE | 0 | DMA_CODE")),
                           fullPrepped$m.data,weights = fullPrepped$m.data$weights))


summary(mFullWgt <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                           paste(c(covariates,"pcttw_sanders16"),collapse = " + "),
                                           " | DMA_CODE | 0 | DMA_CODE")),
                         fullPrepped$dat,weights = fullPrepped$W.out$weights))


# March + April Regressions
foranal.weight <- finalDat %>% 
  dplyr::select(pcttw_sanders,DMA_cases,treatBin,
         DMA_CODE,date,stab,covariates,
         pcttw_sanders16) %>% 
  filter(complete.cases(.),
         date >= as.Date('2020-03-01'))

covs <- covariates[-which(grepl('caucus',covariates))]

marPrepped <- wgtMatchFun(dat = foranal.weight,
                          formMatch = paste0('treatBin ~ ',paste(covs[c(4,2,10,12,7,13)],collapse = " + ")), # CEM fails with the full set of covariates. See SI page 13-15 for discussion.
                          formWgt = paste0('treatBin ~ ',paste(covs,collapse = " + ")),
                          matchMethod = 'cem',
                          wgtMethod = 'optweight')

summary(mMarRaw <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                          paste(c(covs,"pcttw_sanders16"),collapse = " + "),
                                          " | DMA_CODE | 0 | DMA_CODE")),
                        finalDat %>% filter(date >= as.Date("2020-03-01"))))

summary(mMarMatch <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                            paste(c(covs,"pcttw_sanders16"),collapse = " + "),
                                            " | DMA_CODE | 0 | DMA_CODE")),
                          marPrepped$m.data,weights = marPrepped$m.data$weights))


summary(mMarWgt <- felm(as.formula(paste0("scale(pcttw_sanders) ~ treatBin + ",
                                          paste(c(covs,"pcttw_sanders16"),collapse = " + "),
                                          " | DMA_CODE | 0 | DMA_CODE")),
                        marPrepped$dat,weights = marPrepped$W.out$weights))


# SI Table 4
stargazer(fullPrepped$balTab,summary = F,out = '../Tables/SI-table4.tex')


# SI Table 5
covars <- rownames(mFullRaw$coefficients)
labs <- gsub('Treatbin','Exposure Dummy',gsub('Turnout pct 20','Turnout 2020',gsub('Pcttw sander16','Sanders 2016',gsub('Caucus$','Caucus dummy',gsub("Lths","LTHS",gsub("Collup","Coll. Up",gsub("Md inc HH","Med HH Inc",gsub("Sc ","",gsub(" or african american|band$","",Hmisc::capitalize(trimws(gsub("manufactur","manufacturing",gsub("hher|hhs","HH",gsub("labor force part rate","LFPR",gsub("bachelor s","bachelor's",gsub("age 18 64|pop 16 over|hh$|poppct","",gsub("\\_"," ",gsub("^sc_|^.*cty\\_","",tolower(covars)))))))))))))))))))

stargazer(mFullRaw,mFullMatch,mFullWgt,
                 mMarRaw,mMarMatch,mMarWgt,
                 keep.stat = c("n","rsq"),
                 star.cutoffs = c(.1,.05,.01,.001),
                 star.char = c('\\dag','*','**','***'),
                 covariate.labels = labs,out = '../Tables/SI-table5.tex')




# SI Figure 14
toplot <- fullPrepped$balPlot$toplot

dist2d <- function(x,y) {
  m <- cbind(c(-1,-1),c(x,y))
  abs(det(m))/sqrt(sum(c(-1,-1)^2))
}
vdist2d <- Vectorize(dist2d,vectorize.args = c("x","y"))
toplot$type <- as.character(toplot$type)
toplot$cov <- as.character(toplot$cov)
toplot <- toplot %>% bind_rows(toplot %>% group_by(cov) %>% summarise(x = min(x,y),y = min(x,y)),
                               toplot %>% group_by(cov) %>% summarise(x = max(x,y),y = max(x,y)))


toplot$cov <- Hmisc::capitalize(tolower(trimws(gsub("Manufactur","Manufacturing",gsub("Labor Force Part Rate","LFPR",gsub("hher|hhs","HH",gsub("Bachelor s","Bachelor's",gsub("_"," ",gsub("CTY_|AGE_18_64|pop_16_over|hh$|POPPCT","",toplot$cov)))))))))
toplot %>% group_by(type,cov) %>%
  summarise(eucdist = mean(vdist2d(x = x,y= y))) %>%
  filter(!is.na(type)) -> sum.euc



pdf('../Figures/SI_figure14.pdf',width = 7,height = 5)
toplot %>%
  filter(!grepl('caucus',tolower(cov))) %>%
  ggplot(aes(x = x,y = y,color = factor(type,levels = c("Raw","Matched")),text = paste0(type,"\nEuc Dist = ",round(vdist2d(x = x,y = y),2)))) +
  geom_point(alpha = .3,size = .5) +
  geom_abline() +
  geom_abline(aes(intercept = (rrlb - rrub) * 0.1,slope = 1),linetype = "dashed") +
  geom_abline(aes(intercept = -(rrlb - rrub) * 0.1,slope = 1),linetype = "dashed") +
  theme_bw() +
  xlab("Control") + ylab("Treated") +
  theme(legend.position = "bottom") +
  facet_wrap(~cov,scales = "free") +
  scale_color_discrete("",na.translate = F) +
  guides(color = guide_legend(override.aes = list(size = 5,alpha = 1)))
dev.off()
