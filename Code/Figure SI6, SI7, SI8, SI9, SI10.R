# File name: SIfigure6_SIfigure7_SIfigure8_SIfigure9_SIfigure10.R
# In: 
#   - replication_data.RData
# Out:
#   - /Figures/SI_figure6.pdf
#   - /Figures/SI_figure7.pdf
#   - /Figures/SI_figure8.pdf
#   - /Figures/SI_figure9.pdf
#   - /Figures/SI_figure10.pdf

require(lme4)
require(lfe)
require(MatchIt)
require(WeightIt)
require(tjbal)
require(optmatch)
require(stargazer)
require(cobalt)
require(tidyverse)
require(gridExtra)
require(ggridges)
require(ggrepel)

rm(list = ls())
gc()




####################################################################################################################### Loading data
load("C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Data\\replication_data.RData")





####################################################################################################################### Loading functions
source('C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\code\\helper_functions.R')







####################################################################################################################### Preparing variables
Y <- paste0(c("pct_","VAP_","pcttw_"),"sanders")
D <- unlist(lapply(c("sc_","ln_"),function(x) paste0(x,paste0(c("county_","DMA_","state_"),"cases"))))
FE <- c("0","DMA_CODE","date")
covariates <- c("sc_CTY_LTHS","sc_CTY_CollUp",'caucus_switch','caucus',
                "sc_CTY_LT30yo","sc_CTY_60Up",
                "sc_CTY_Below_poverty_level_AGE_18_64","sc_CTY_Female_hher_no_husbandhh",
                "sc_CTY_Unem_rate_pop_16_over","sc_CTY_Labor_Force_Part_Rate_pop_16_over",
                "sc_CTY_Manufactur","sc_CTY_Md_inc_hhs",
                "sc_CTY_POPPCT_RURAL","sc_CTY_Speak_only_English","sc_CTY_White","sc_CTY_Black_or_African_American",
                "ln_CTY_tot_pop","sc_turnout_pct_20")






####################################################################################################################### Looping across specifications
finalDat$sc_turnout_pct_chg <- scale(finalDat$turnout_pct_chg)
finalDat$sc_VAP_turnout_20 <- scale(finalDat$VAP_turnout_20)

Y <- c("sc_turnout_pct_20","sc_turnout_pct_chg","sc_VAP_turnout_20")
resTurnout <- list()
for(y in Y) {
  for(d in D) {
    for(fe in FE) {
      for(m in c("sc_CTY_LT30yo","sc_CTY_60Up","sc_CTY_Unem_rate_pop_16_over","sc_CTY_Old_age_dep_ratio","sc_CTY_POPPCT_RURAL","sc_CTY_Manufactur","sc_CTY_LTHS","sc_CTY_CollUp")) {
        resTurnout[[y]][[d]][[fe]][[m]]$felm$coefs <- summary(tmp <- felm(as.formula(paste0(y," ~ ",d,"*",m," + ",
                                                                                            paste(c(covariates)[-which(c(covariates) %in% c("sc_CTY_LT30yo","sc_CTY_60Up","sc_turnout_pct_20"))],collapse = "+")," | ",fe," | 0 | ",fe)),finalDat))$coefficients
        resTurnout[[y]][[d]][[fe]][[m]]$felm$mfx <- interaction_plot_continuous(tmp)
        if(fe != "0") {
          resTurnout[[y]][[d]][[fe]][[m]]$lmer$coefs <- summary(tmp <- lmer(as.formula(paste0(y," ~ ",d,"*",m," + ",
                                                                                              paste(c(covariates)[-which(c(covariates) %in% c("sc_CTY_LT30yo","sc_CTY_60Up","sc_turnout_pct_20"))],collapse = "+")," + (1| ",fe,")")),finalDat))$coefficients
          resTurnout[[y]][[d]][[fe]][[m]]$lmer$mfx <- interaction_plot_continuous(tmp)
          
        }
        
      }
    }
  }
}


p1 <- data.frame(resTurnout$sc_turnout_pct_20$sc_DMA_cases$DMA_CODE$sc_CTY_60Up$felm$mfx) %>%
  ggplot(aes(x = x_2,y = delta_1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lb,ymax = ub),width = .1) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  xlab(expression(Younger %<->% Older)) +
  ylab('MFX of Exposure on Turnout') + 
  theme_ridges() + 
  ggtitle(label = '',subtitle = '% of county 60 years or older')

p2 <- data.frame(resTurnout$sc_turnout_pct_20$sc_DMA_cases$DMA_CODE$sc_CTY_LT30yo$felm$mfx) %>%
  ggplot(aes(x = x_2,y = delta_1)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lb,ymax = ub),width = .1) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  xlab(expression(Older %<->% Younger)) +
  ylab('MFX of Exposure on Turnout') + 
  theme_ridges() + 
  ggtitle(label = '',subtitle = '% of county 30 years or younger')


# SI Figure 6: Turnout and age
pdf('../Figures/SI_figure6.pdf',width = 8,height = 5)
grid.arrange(p1,p2,ncol = 2)
dev.off()



# SI Figure 7
pdf('../Figures/SI_figure7.pdf',width = 8,height = 5)
finalDat %>%
  group_by(date,stab) %>%
  summarise(pct_sanders16 = mean(pct_sanders16,na.rm=T),
            turnout_pct_20 = mean(turnout_pct_20*100,na.rm=T)) %>%
  ggplot(aes(x = date,y = turnout_pct_20,size = pct_sanders16,label = stab)) + 
  geom_point(alpha = .3) +
  geom_text_repel(size = 4) + 
  theme_ridges() + 
  xlab('Date') + ylab('Turnout 2020 (%)') +
  scale_size(name = 'Sanders 2016 Vote Share (%)') + 
  theme(legend.position = 'bottom')
dev.off()


# SI Figure 8
p1 <- finalDat %>%
  filter(date %in% as.Date(c('2020-03-03',
                             '2020-03-10',
                             '2020-03-17'))) %>%
  mutate(turnout_pct_20 = turnout_pct_20*100) %>%
  ggplot(aes(x = pct_sanders16,y = turnout_pct_20)) + 
  geom_point() + 
  geom_smooth() + 
  theme_ridges() + 
  xlab('Sanders 2016 Vote Share (%)') + 
  ylab('Turnout 2020 (%)') + ylim(c(0,50)) + 
  ggtitle(label = '',subtitle = 'March Data')
  
p2 <- finalDat %>%
  filter(date < as.Date('2020-03-03')) %>%
  mutate(turnout_pct_20 = turnout_pct_20*100) %>%
  ggplot(aes(x = pct_sanders16,y = turnout_pct_20)) + 
  geom_point() + 
  geom_smooth() + 
  theme_ridges() + 
  xlab('Sanders 2016 Vote Share (%)') + 
  ylab('Turnout 2020 (%)') + ylim(c(0,50)) + 
  ggtitle(label = '',subtitle = 'Pre-March Data')

pdf('../Figures/SI_figure8.pdf',width = 9,height = 5)
grid.arrange(p2,p1,ncol = 2)
dev.off()




# SI Figure 9
finalDat$post <- ifelse(finalDat$date > as.Date('2020-02-29'),1,0)
toplot <- data.frame(summary(felm(as.formula(paste0("turnout_pct_20 ~ scale(pct_sanders16)*factor(date) + ",
                                                    paste(covariates[c(1:2,5:17)],collapse = '+'),'| 0 | 0 | 0')),finalDat))$coefficients)
colnames(toplot) <- c('est','se','tstat','pval')
toplot$vars <- rownames(toplot)
rownames(toplot) <- NULL


toplot %>%
  filter(grepl(':factor',vars)) %>%
  mutate(date = as.Date(gsub('scale.*?date\\)','',vars))) %>%
  ggplot(aes(x = date,y = est)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = est - 2*se,ymax = est+2*se)) + 
  geom_hline(yintercept = 0,linetype = 'dashed')


finalDat$dateTmp <- as.Date(ifelse(finalDat$Date %in% as.Date(c('2020-02-11','2020-02-22')),as.Date('2020-02-22'),finalDat$Date),origin = '1970-01-01')
toplot <- NULL
for(d in unique(finalDat$Date)) {
  if(nrow(finalDat %>% filter(Date == d)) < 50) {
    covs <- covariates[c(2,6,9,15)]
  } else {
    covs <- covariates[c(2,6,9,15)]
  }
  tmp <- data.frame(t(summary(felm(as.formula(paste0("scale(turnout_pct_20) ~ scale(pct_sanders16) + ",
                                                     paste(covs,collapse = '+'),'| 0 | 0 | 0')),finalDat %>%
                                     filter(Date == d)))$coefficients[2,]))
  colnames(tmp) <- c('est','se','tstat','pval')
  tmp$date <- as.Date(d,origin = '1970-01-01')
  toplot <- bind_rows(toplot,tmp)
}


labs <- finalDat %>%
  select(stab,Date) %>% distinct()
toplot$lab <- NA
for(d in unique(toplot$date)) {
  toplot$lab[which(toplot$date == d)] <- paste(unique(labs$stab[which(labs$Date == d)])[order(unique(labs$stab[which(labs$Date == d)]))],collapse = '\n')
}


pdf('../Figures/SI_figure9.pdf',width = 8,height = 5)
toplot %>%
  filter(date < as.Date('2020-04-15')) %>%
  ggplot(aes(x = date,y = est,label = lab,color = ifelse(lab == 'SC','SC','black'))) + 
  geom_point() + 
  geom_errorbar(aes(ymin = est - 2*se,ymax = est+2*se),width = .5) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  theme_ridges() + 
  xlab('Election Date') + ylab('Relationship between Sanders 2016 and Turnout') + 
  scale_color_manual(values = c('SC' = 'red','black' = 'black')) +
  geom_text_repel(size = 3,hjust = 0,vjust = .5,alpha = .8) + 
  theme(legend.position = 'none')
dev.off()














# SI Figure 10
set.seed(123)
Y <- c('pct_sanders16')

D <- unlist(lapply(c("sc_","ln_"),function(x) paste0(x,paste0(c("county_","DMA_","state_"),"cases"))))
FE <- c("0","DMA_CODE","stab")
covariates <- c("sc_CTY_LTHS","sc_CTY_CollUp",'caucus_switch','caucus',
                "sc_CTY_LT30yo","sc_CTY_60Up",
                "sc_CTY_Below_poverty_level_AGE_18_64","sc_CTY_Female_hher_no_husbandhh",
                "sc_CTY_Unem_rate_pop_16_over","sc_CTY_Labor_Force_Part_Rate_pop_16_over",
                "sc_CTY_Manufactur","sc_CTY_Md_inc_hhs",
                "sc_CTY_POPPCT_RURAL","sc_CTY_Speak_only_English",
                "sc_CTY_White","sc_CTY_Black_or_African_American",
                "ln_CTY_tot_pop","sc_turnout_pct_20")


resRegs <- resBal <- resTabs <- list()
for(y in Y) {
  for(d in D[c(1:3)]) {
    
    for(fe in FE) {
      stars <- list()
      for(dateThresh in c("2020-01-01","2020-03-01")) {
        if(dateThresh == '2020-03-01') {
          covs <- covariates[-which(covariates == 'caucus')]
        } else {
          covs <- covariates
        }
        if(!is.null(resRegs[[y]][[d]][[fe]][[dateThresh]]$basic$felm$cont)) { next }
        finalDat$treatBin <- ifelse(finalDat[[gsub("sc_|ln_","",d)]] > 1,1,0)
        foranal.weight <- finalDat %>% select(y,d,treatBin,gsub("sc_|ln_","",d),DMA_CODE,date,stab,covs) %>% 
          filter(complete.cases(.),date >= as.Date(dateThresh))
        
        m.out <- matchit(formula = as.formula(paste("treatBin ~ ",paste(covs,collapse = " + "))),
                         data = foranal.weight,
                         method = "nearest",
                         distance = "mahalanobis")
        
        m.data <- match.data(m.out)
        W.out <- weightit(formula(paste0("treatBin ~ ",paste(covs,collapse = " + "))),
                          data = foranal.weight, estimand = "ATT", method = "cbps")
        
        # Balance tables
        balt.pre <- bal.tab(formula(paste0("treatBin ~ ",paste(covs,collapse = " + "))),
                            data = foranal.weight, estimand = "ATT",m.threshold = .05)
        
        balt.post <- bal.tab(W.out, m.threshold = .05, disp.v.ratio = TRUE)
        unm <- data.frame(Covs = rownames(balt.pre$Balance),
                          balt.pre$Balance %>% mutate(Diff_Unm = round(Diff.Un,2),
                                                      Bal_Test_Unm = M.Threshold.Un)) %>%
          select(Covs,Diff_Unm,Bal_Test_Unm)
        
        match <- data.frame(Covs = rownames(balt.post$Balance),
                            balt.post$Balance %>% mutate(Diff_Match = round(Diff.Adj,2),
                                                         Bal_Test_Match = M.Threshold)) %>%
          select(Covs,Diff_Match,Bal_Test_Match)
        
        
        # Basic
        resRegs[[y]][[d]][[fe]][[dateThresh]]$basic$felm$cont <- summary(stars$cont[[paste0(dateThresh,"_1")]] <- felm(as.formula(paste0("scale(",y,") ~ ",d," + ",paste(c(covs),collapse = " + ")," | ",fe," | 0 | ",fe)),finalDat %>% filter(date >= as.Date(dateThresh))))$coefficients
        resRegs[[y]][[d]][[fe]][[dateThresh]]$basic$felm$bin <- summary(stars$bin[[paste0(dateThresh,"_1")]] <- felm(as.formula(paste0("scale(",y,") ~ treatBin + ",paste(c(covs),collapse = " + ")," | ",fe," | 0 | ",fe)),finalDat %>% filter(date >= as.Date(dateThresh))))$coefficients
        
        # Matching
        resRegs[[y]][[d]][[fe]][[dateThresh]]$matching$felm$cont <- summary(stars$cont[[paste0(dateThresh,"_2")]] <- felm(as.formula(paste0("scale(",y,") ~ ",d," + ",paste(c(covs),collapse = " + ")," | ",fe," | 0 | ",fe)),m.data,weights = m.data$weights))$coefficients
        resRegs[[y]][[d]][[fe]][[dateThresh]]$matching$felm$bin <- summary(stars$bin[[paste0(dateThresh,"_2")]] <- felm(as.formula(paste0("scale(",y,") ~ treatBin + ",paste(c(covs),collapse = " + ")," | ",fe," | 0 | ",fe)),m.data,weights = m.data$weights))$coefficients
        
        # Weighting
        resRegs[[y]][[d]][[fe]][[dateThresh]]$weighting$felm$cont <- summary(stars$cont[[paste0(dateThresh,"_3")]] <- felm(as.formula(paste0("scale(",y,") ~ ",d," + ",paste(c(covs),collapse = " + ")," | ",fe," | 0 | ",fe)),foranal.weight,weights = W.out$weights))$coefficients
        resRegs[[y]][[d]][[fe]][[dateThresh]]$weighting$felm$bin <- summary(stars$bin[[paste0(dateThresh,"_3")]] <- felm(as.formula(paste0("scale(",y,") ~ treatBin + ",paste(c(covs),collapse = " + ")," | ",fe," | 0 | ",fe)),foranal.weight,weights = W.out$weights))$coefficients
        
        if(fe != "0") {
          resRegs[[y]][[d]][[fe]][[dateThresh]]$basic$lmer <- summary(lmer(as.formula(paste0("scale(",y,") ~ ",d," + ",paste(c(covs),collapse = " + ")," + (1| ",fe,")")),finalDat %>% filter(date >= as.Date(dateThresh))))$coefficients
          resRegs[[y]][[d]][[fe]][[dateThresh]]$matching$lmer <- summary(lmer(as.formula(paste0("scale(",y,") ~ ",d," + ",paste(c(covs),collapse = " + ")," + (1| ",fe,")")),m.data,weights = m.data$weights))$coefficients
          resRegs[[y]][[d]][[fe]][[dateThresh]]$weighting$lmer <- summary(lmer(as.formula(paste0("scale(",y,") ~ ",d," + ",paste(c(covs),collapse = " + ")," + (1| ",fe,")")),foranal.weight,weights = W.out$weights))$coefficients
        }
      }
    }
    cat(y,"\n")
  }
}


toplot <- NULL
for(fe in names(resRegs$pct_sanders16$sc_DMA_cases)) {
  for(mod in c('basic','matching','weighting')) {
    for(meas in c('bin','cont')) {
      tmp <- resRegs$pct_sanders16$sc_DMA_cases[[fe]]$`2020-01-01`[[mod]]$felm[[meas]][2,]
      if(is.null(tmp)) { next }
      tmp <- data.frame(t(tmp))
      colnames(tmp) <- c('est','se','tstat','pval')
      tmp$fe <- fe
      tmp$mod <- mod
      tmp$meas <- meas
      toplot <- bind_rows(toplot,tmp)
    }
  }
}


pdf('../Figures/SI_figure10.pdf',width = 8,height = 5)
toplot %>%
  filter(meas != 'cont') %>%
  mutate(fe = factor(ifelse(fe == 0,'None',
                     ifelse(fe == 'DMA_CODE','DMA','State')),levels = c('None','DMA','State'))) %>%
  ggplot(aes(x = fe,y = est,color = mod)) + 
  geom_point(position = position_dodge(width = .2)) + 
  geom_errorbar(aes(ymin = est - 2*se,ymax = est+2*se),width = .1,
                position = position_dodge(width = .2)) + 
  geom_hline(yintercept = 0,linetype = 'dashed') + 
  scale_color_manual(name = 'Model',values = c('basic' = 'grey70',
                                               'matching' = 'grey40',
                                               'weighting' = 'black')) +
  xlab('Fixed Effect') + ylab('Effect of Covid Exposure on Bernie 2016') + 
  theme_ridges()
dev.off()
