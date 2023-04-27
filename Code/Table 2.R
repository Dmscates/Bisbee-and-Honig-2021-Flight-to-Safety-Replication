# File name: table2.R
# In: 
#   - /Data/survey_experiment_data.RData
# Out: 
#   - /Tables/table2.tex

rm(list = ls())
gc()
require(tidyverse)

load('../Data/survey_experiment_data.RData')
# Basic test: Does pessimistic treatment increase support for mainstream candidate?
summary(main <- lm(scale(y) ~ z + factor(polAge) + factor(polOcc) + factor(polPlat) + 
             factor(age) + factor(sex) + factor(hisp) + white + black + asian + factor(pid),
             foranal %>%
               filter(duration > 180) %>%
               mutate(sex = factor(sex,levels = c("Neither male nor female describes me accurately OR prefer not to say","Female","Male")),
                      polPlat = factor(polPlat,levels = c("none","hlth","educ")),
                      pid = factor(pid,levels = c("Strong Democrat","Democrat","Lean Democrat","Independent","Lean Republican","Republican","Strong Republican")),
                      covid_self = factor(covid_self,levels = c("No","Maybe","Yes")),
                      covid_pers = factor(covid_pers,levels = c("No","Maybe","Yes")))))

summary(biv <- lm(yBin ~ z,
                   foranal %>%
                     filter(duration > 0) %>%
                     mutate(sex = factor(sex,levels = c("Neither male nor female describes me accurately OR prefer not to say","Female","Male")),
                            polPlat = factor(polPlat,levels = c("none","hlth","educ")),
                            pid = factor(pid,levels = c("Strong Democrat","Democrat","Lean Democrat","Independent","Lean Republican","Republican","Strong Republican")),
                            covid_self = factor(covid_self,levels = c("No","Maybe","Yes")),
                            covid_pers = factor(covid_pers,levels = c("No","Maybe","Yes")))))

summary(ind <- lm(yBin ~ z + 
                    factor(age) + factor(sex) + factor(hisp) + white + black + asian + factor(pid),
                  foranal %>%
                    filter(duration > 0) %>%
                    mutate(sex = factor(sex,levels = c("Neither male nor female describes me accurately OR prefer not to say","Female","Male")),
                           polPlat = factor(polPlat,levels = c("none","hlth","educ")),
                           pid = factor(pid,levels = c("Strong Democrat","Democrat","Lean Democrat","Independent","Lean Republican","Republican","Strong Republican")),
                           covid_self = factor(covid_self,levels = c("No","Maybe","Yes")),
                           covid_pers = factor(covid_pers,levels = c("No","Maybe","Yes")))))

summary(indcov <- lm(yBin ~ z + 
                    factor(age) + factor(sex) + factor(hisp) + white + black + asian + factor(pid),
                  foranal %>%
                    filter(duration > 0) %>%
                    mutate(sex = factor(sex,levels = c("Neither male nor female describes me accurately OR prefer not to say","Female","Male")),
                           polPlat = factor(polPlat,levels = c("none","hlth","educ")),
                           pid = factor(pid,levels = c("Strong Democrat","Democrat","Lean Democrat","Independent","Lean Republican","Republican","Strong Republican")),
                           covid_self = factor(covid_self,levels = c("No","Maybe","Yes")),
                           covid_pers = factor(covid_pers,levels = c("No","Maybe","Yes")))))

summary(full <- lm(yBin ~ z + 
                       factor(age) + factor(sex) + factor(hisp) + white + black + asian + factor(pid) + 
                     factor(polAge) + factor(polOcc) + factor(polPlat),
                     foranal %>%
                       filter(duration > 0) %>%
                       mutate(sex = factor(sex,levels = c("Neither male nor female describes me accurately OR prefer not to say","Female","Male")),
                              polPlat = factor(polPlat,levels = c("none","hlth","educ")),
                              pid = factor(pid,levels = c("Strong Democrat","Democrat","Lean Democrat","Independent","Lean Republican","Republican","Strong Republican")),
                              covid_self = factor(covid_self,levels = c("No","Maybe","Yes")),
                              covid_pers = factor(covid_pers,levels = c("No","Maybe","Yes")))))



summary(attentive <- lm(yBin ~ z + 
                     factor(age) + factor(sex) + factor(hisp) + white + black + asian + factor(pid) + 
                     factor(polAge) + factor(polOcc) + factor(polPlat),
                   foranal %>%
                     filter(duration > 150) %>%
                     mutate(sex = factor(sex,levels = c("Neither male nor female describes me accurately OR prefer not to say","Female","Male")),
                            polPlat = factor(polPlat,levels = c("none","hlth","educ")),
                            pid = factor(pid,levels = c("Strong Democrat","Democrat","Lean Democrat","Independent","Lean Republican","Republican","Strong Republican")),
                            covid_self = factor(covid_self,levels = c("No","Maybe","Yes")),
                            covid_pers = factor(covid_pers,levels = c("No","Maybe","Yes")))))
require(stargazer)
stargazer(biv,ind,full,attentive,keep.stat = c("n","rsq"),keep = "pid|covid|z",
          covariate.labels = c("Anxiety Prime","Democrat","Lean Dem","Independent","Lean GOP","Republican","Strong GOP"),
          star.char = c('\\dag','*','**','***'),
          star.cutoffs = c(.1,.05,.01,.001),
          out = "../Tables/table2.tex")


