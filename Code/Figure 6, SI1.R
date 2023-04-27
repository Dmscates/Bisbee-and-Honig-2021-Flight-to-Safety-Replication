# File name: figure6_SIfigure1.R
# In: 
#   - replication_data.RData
# Out: 
#   - /Data/Results/tjbalWgtsNEW.RData (saving time re-estimating the weights)
#   - /Figures/figure6.pdf
#   - /Figures/SI_figure1.pdf

require(lme4)
require(lfe)
require(MatchIt)
require(WeightIt)
require(rlang)
require(tjbal)
#install.packages("tjbal")
#install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
#devtools::install_github('chadhazlett/kbal')
#devtools::install_github('xuyiqing/tjbal')
#install.packages("rlang")
require(optmatch)
require(stargazer)
require(cobalt)
require(tidyverse)
require(ggridges)


rm(list = ls())
gc()
setwd("C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication")
####################################################################################################################### Loading data
load("C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\Data\\replication_data.RData")


####################################################################################################################### Loading functions
source('C:\\Users\\danie\\Dropbox\\Replication\\dataverse_files\\Replication\\code\\helper_functions.R')





####################################################################################################################### Preparing variables
Y <- paste0(c("pct_","VAP_","pcttw_"),"sanders")
D <- unlist(lapply(c("sc_","ln_"),function(x) paste0(x,paste0(c("county_","DMA_","state_"),"cases"))))
FE <- c("0","DMA_CODE","date")
covariates <- c("sc_CTY_LTHS","sc_CTY_CollUp",
                "sc_CTY_LT30yo","sc_CTY_60Up",
                "sc_CTY_Below_poverty_level_AGE_18_64","sc_CTY_Female_hher_no_husbandhh",
                "sc_CTY_Unem_rate_pop_16_over","sc_CTY_Labor_Force_Part_Rate_pop_16_over",
                "sc_CTY_Manufactur","sc_CTY_Md_inc_hhs",
                "sc_CTY_POPPCT_RURAL","sc_CTY_Speak_only_English","sc_CTY_White","sc_CTY_Black_or_African_American",
                "ln_CTY_tot_pop","sc_turnout_pct_20")




# SI Figure 1
set.seed(123)
treatInd <- "March17Cases"
vd <- '2020-03-17'
d <- 'ln_DMA_cases'
est <- 'meanfirst'
treatInd <- paste0("DMA_",treatInd)
forTjbalAnal$treatGroup <- ifelse(forTjbalAnal$voteDate == vd & forTjbalAnal[[treatInd]] > 0,1,0)
forTjbalAnal$treat <- ifelse(forTjbalAnal$treatGroup == 1 & forTjbalAnal$date > as.Date("2020-04-17"),1,0)

inds <- which(forTjbalAnal$treatGroup == 1)
inds <- unique(forTjbalAnal$stcou[inds])
ctrlInds <- unique(forTjbalAnal$stcou[-c(which(forTjbalAnal$stcou %in% inds),which(forTjbalAnal$voteDate >= as.Date(vd)))])

tmpTjbalAnal <- forTjbalAnal %>% filter(stcou %in% c(inds,ctrlInds))
tjout <- try(tjbal(data = as.data.frame(tmpTjbalAnal),Y = d,D = "treat",X = unique(c(covariates,"pct_sanders16")),
                   index = c("stcou","dateNum"),estimator = est,demean = T,vce = "fixed"))


toplot <- as.data.frame(tjout$Y.bar)
toplot$date <- seq.Date(from = as.Date("2020-01-23"),to = as.Date("2020-05-01"),length.out = nrow(tjout$Y.bar))
getwd()
pdf("../Figures/SI_figure1.pdf")
toplot %>%
  gather(type,value,-date) %>%
  ggplot(aes(x = date,y = value,color = type,size = type,linetype = type,alpha = type)) + 
  geom_line() + 
  geom_vline(xintercept = as.Date(unique(finalDat$Date)[which(as.Date(unique(finalDat$Date)) < as.Date("2020-03-17"))]),linetype = "dashed",alpha = .6) + 
  geom_vline(xintercept = as.Date("2020-03-17"),size = 1.2) + 
  xlab("Date") + ylab("Number of Cases (logged)") + 
  annotate(geom = "text",label = c("Iowa Caucuses","New Hampshire","Nevada","South Carolina","Super Tuesday","March 10","March 17"),
           x = as.Date(c("2020-02-03","2020-02-11","2020-02-22","2020-02-29",
                         "2020-03-03","2020-03-10","2020-03-17")),y = Inf,angle = 90,vjust = 1,hjust = 1,size = 3.5,color = "grey30") + 
  annotate(geom = "text",label = c("Treated","Weighted","Control"),
           y = c(7.8,7.5,6.6),x = as.Date("2020-05-02"),angle = 0,
           parse = F,hjust = 0,vjust = .3,size = 3.5,color = "grey30") +
  scale_linetype_manual(values = c("solid","dotted","solid")) + 
  scale_size_manual(values = c(1.2,1.2,2.5)) +
  scale_color_manual(values = c("red","black","black")) + 
  scale_alpha_manual(values = c(1,1,.3)) + 
  xlim(as.Date("2020-01-23"),as.Date("2020-05-10")) + theme_ridges() + 
  theme(legend.position = "none") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()



# Figure 6 
set.seed(123)
tjBalWgts <- list()
for(vd in c("2020-03-03","2020-03-10","2020-03-17")) {
  for(d in c("county_cases")) {
    for(est in c("kernel")) {
      
      if(vd == "2020-03-03") {
        treatInd <- "March3Cases"
      } else if(vd == "2020-03-10") {
        treatInd <- "March10Cases"
      } else {
        treatInd <- "March17Cases"
      }
      if(grepl("county",d)) {
        treatInd <- paste0("county_",treatInd)
      } else {
        treatInd <- paste0("DMA_",treatInd)
      }
      
      forTjbalAnal$treatGroup <- ifelse(forTjbalAnal$voteDate == vd & forTjbalAnal[[treatInd]] > 0,1,0)
      forTjbalAnal$treat <- ifelse(forTjbalAnal$treatGroup == 1 & forTjbalAnal$date > as.Date("2020-04-17"),1,0)
      
      inds <- which(forTjbalAnal$treatGroup == 1)
      inds <- unique(forTjbalAnal$stcou[inds])
      ctrlInds <- unique(forTjbalAnal$stcou[-c(which(forTjbalAnal$stcou %in% inds),which(forTjbalAnal$voteDate >= as.Date(vd)))])
      
      ests <- finalDat %>% select(stcou)
      for(i in 1:length(inds)) {
        tmpTjbalAnal <- forTjbalAnal %>% filter(stcou %in% c(inds[-i],ctrlInds))
        sink <- capture.output(tjout <- try(tjbal(data = as.data.frame(tmpTjbalAnal),Y = d,D = "treat",
                           X = unique(c(covariates,"pct_sanders16")),seed = 123,
                           index = c("stcou","dateNum"),estimator = est,demean = T,vce = "fixed")))
        
        if(class(tjout) == "try-error") { next }
        tjout$data.wide$w <- tjout$w
        ests <- ests %>% left_join(tjout$data.wide %>% select(stcou = unit,w),by = 'stcou')
        colnames(ests)[which(colnames(ests) == "w")] <- paste0("w",i)
        cat('.')
      }
      cat('\n')
      
      tjBalWgts[[vd]][[d]][[est]]$jacknife <- ests
      tmpTjbalAnal <- forTjbalAnal %>% filter(stcou %in% c(inds,ctrlInds))
      sink <- capture.output(tjout <- try(tjbal(data = as.data.frame(tmpTjbalAnal),Y = d,D = "treat",X = unique(c(covariates,"pct_sanders16")),
                         index = c("stcou","dateNum"),estimator = est,demean = T,seed = 123,
                         vce = "fixed")))
      
      if(class(tjout) == "try-error") { next }
      tjout$data.wide$w <- tjout$w
      tjBalWgts[[vd]][[d]][[est]]$full <- tjout$data.wide %>% select(stcou = unit,w)
    }
  }
}

save(tjBalWgts,file = "../Data/Results/tjbalWgtsNEW.RData")
load("../Data/Results/tjbalWgtsNEW.RData")









resTjbal <- list()
for(y in Y) {
  for(geo in c("county_","DMA_")) {
    for(d in c("March3Cases","March10Cases","March17Cases")) {
      if(d == "March3Cases") {
        int <- "2020-03-03"
      } else if(d == "March10Cases") {
        int <- "2020-03-10"
      } else {
        int <- "2020-03-17"
      }
      for(balCases in names(tjBalWgts[[int]])) {
        for(est in names(tjBalWgts[[int]][[balCases]])) {
          
          if(is.null(tjBalWgts[[int]][[balCases]][[est]]$full)) { next }
          
          finalDat %>% left_join(tjBalWgts[[int]][[balCases]][[est]]$full) %>% 
            left_join(tjBalWgts[[int]][[balCases]][[est]]$jacknife) %>% filter(!is.na(w)) -> tmpAnal
          tmpAnal$post <- ifelse(tmpAnal$date == as.Date(int),1,0)
          tmpAnal$treatGroup <- ifelse(tmpAnal[[paste0(geo,d)]] > 0,1,0)
          tmpAnal$treat <- ifelse(tmpAnal$post == 1 & tmpAnal$treatGroup == 1,1,0)
          tmpAnal %>% select(post,treatGroup,treat,date)
          
          ests <- NULL
          for(i in colnames(tmpAnal %>% select(matches("^w\\d")))) {
            ests <- c(ests,sum(tmpAnal[[y]]*tmpAnal[[i]],na.rm=T))
          }
          resTjbal[[y]][[geo]][[d]][[balCases]][[est]]$bootstrapped <- ests
          resTjbal[[y]][[geo]][[d]][[balCases]][[est]]$lmCont <- summary(lm(as.formula(paste0(y," ~ ",balCases)),tmpAnal,weights = abs(tmpAnal$w)))$coefficients
          resTjbal[[y]][[geo]][[d]][[balCases]][[est]]$lmBin <- summary(lm(as.formula(paste0(y," ~ treat")),tmpAnal,weights = abs(tmpAnal$w)))$coefficients
        }
      }
    }
  }
}


toplot <- NULL
for(period in names(resTjbal$pcttw_sanders$DMA_)) {
  toplot <- bind_rows(toplot,
                      data.frame(bs = resTjbal$pcttw_sanders$county_[[period]]$county_cases$kernel$bootstrapped,
             period = period))
}




# Figure 6: 
pdf('../Figures/figure6.pdf',width = 7,height = 5)
toplot %>%
  mutate(period = factor(gsub('Cases','',
                       gsub('March','March ',period)),levels = rev(c('March 3','March 10','March 17')))) %>%
  ggplot(aes(x = bs,y = period)) + 
  geom_density_ridges(alpha = .7,color = 'black') + 
  theme_ridges() + 
  geom_vline(xintercept = 0,linetype = 'dashed') + 
  xlab('ATT of Exposure on Sanders Support') +  ylab('Outbreak Date')
dev.off()