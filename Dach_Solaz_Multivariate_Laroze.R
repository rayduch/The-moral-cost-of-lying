

library(foreign)
library(ggplot2)
library(lme4)
library(readstata13)
library(betareg)
library(stargazer)
library(RColorBrewer)
library(rms)
theme_set(theme_bw())
library(plyr)


rm(list=ls())

setwd("C:/Users/André Laroze/Dropbox/CESS-Santiago/Archive/Tax Compliance Experiments/Rep Material/Why-we-Cheat-2016-master/Replication/Laroze rep package")
#fig.path <- "Laroze rep package"

#-----------------------------
# Data Management
#----------------------------

data <- read.dta("additional_data/base.dta")
shock.data <- read.dta13("additional_data/shock.dta")
shock.data <- shock.data[shock.data$session >24, ]
redis.data <- read.dta('additional_data/redistribution.dta')

non.fixed<-read.dta13('additional_data/MasterfileOxford2016e.dta')
non.fixed<-subset(non.fixed, auditrate==0)
non.fixed$percevaded <- (non.fixed$profitret - non.fixed$declared)/non.fixed$profitret


data$HighTax  <- as.numeric(apply(data[,c("T20","T30", "T40")],1,sum)>0)
shock.data$HighTax  <- as.numeric(apply(shock.data[,c("T20","T30", "T40")],1,sum)>0)
redis.data$HighTax  <- as.numeric(apply(redis.data[,c("T20","T30", "T40")],1,sum)>0)


data$percevaded.t <- data$percevaded*.999+.0005
shock.data$percevaded.t <- shock.data$percevaded*.999+.0005
redis.data$percevaded.t <- redis.data$percevaded*.999+.0005
non.fixed$percevaded.t <- non.fixed$percevaded*.999+.0005


data$cost_comply <- (data$taxrate/100) * data$profitret
shock.data$cost_comply <- (shock.data$taxrate/100) * shock.data$profitret
redis.data$cost_comply <- (redis.data$taxrate/100) * redis.data$profitret
non.fixed$cost_comply<-(non.fixed$taxrate/100)*non.fixed$profitret

shock.data$receive_int <- with(shock.data, receiveshock*ncorrectret)

sub.data.1 <- subset(data,difsalaries == 0)
sub.data.2 <- subset(data,difsalaries == 1)

#table(data$HighTax)


### Creating one general df
kv<-c("sujeto", "grupo","cheat", "ncorrectret", "cost_comply", "percevaded", "percevaded.t", "safechoices", "period", "offerdg")

base<-sub.data.1[, kv]
base$receiveshock<-NA
base$highsalary<-NA
base$treatment<-"Baseline"


status<-sub.data.2[, c(kv, "highsalary")]
status$receiveshock<-NA
status$treatment<-"Low Status"
status$treatment[status$highsalary==1]<-"High Status"

shock<-shock.data[, c(kv, "receiveshock")]
shock$highsalary<-NA
shock$treatment<-"No Shock"
shock$treatment[shock$receiveshock==1]<-"Receive Shock"

red<-redis.data[, kv]
red$receiveshock<-NA
red$highsalary<-NA
red$treatment<-"Redistribute"

mydf<-rbind(base, status, shock, red)

rm(base, status, shock, red)

####################################
## model estimation, table 3 output
####################################

m.bs <- lrm(formula = cheat ~ ncorrectret + cost_comply + offerdg,# + ideology + gender + age_subject, 
         data=sub.data.1, x=T, y=T)
m.bs.cl <- robcov(m.bs, sub.data.1$sujeto)

m.bs.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + offerdg,# + ideology + gender + age_subject, 
            data=sub.data.1, family = binomial(link = "logit"))
m.bs.aic<-round(m.bs.2$aic, 2)
#m.bs.bic<-extractAIC(m.bs, k = log(n)) # for BIC


m.st.full <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                    highsalary + HighsalaryAdd + offerdg,
                  data=sub.data.2, x=T, y=T)
m.st.full.cl <- robcov(m.st.full, sub.data.2$sujeto)
m.st.full.2 <- glm(formula = cheat ~ ncorrectret + cost_comply +
                     highsalary + HighsalaryAdd + offerdg,
              data=sub.data.2, 
              family = binomial(link = "logit"))
m.st.full.aic<-round(m.st.full.2$aic, 2)




m.sh.full <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                    receiveshock + receive_int + offerdg,
                  data=shock.data, x=T, y=T)
m.sh.full.cl <- robcov(m.sh.full, shock.data$sujeto)
m.sh.full.2 <- glm(formula = cheat ~ ncorrectret + cost_comply +
                     receiveshock + receive_int + offerdg,
                   data=shock.data, 
                   family = binomial(link = "logit"))
m.sh.full.aic<-round(m.sh.full.2$aic, 2)



m.red <- lrm( formula = cheat ~ ncorrectret + cost_comply + offerdg,
              data=redis.data, x=T, y=T)
m.red.cl <- robcov(m.red, redis.data$sujeto)
m.red.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + offerdg,
               data=redis.data,
               family = binomial(link = "logit"))
m.red.aic<-round(m.red.2$aic, 2)




m.all <- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
             data=mydf, x=T, y=T)
m.all.cl <- robcov(m.all, mydf$sujeto)
m.all.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + offerdg,# + ideology + gender + age_subject, 
               data=mydf,
               family = binomial(link = "logit"))
m.all.aic<-round(m.all.2$aic, 2)


m.all.ols <- ols(formula = percevaded ~ncorrectret + cost_comply + treatment*ncorrectret + offerdg,
                 data=mydf, x=T, y=T)
m.all.ols.cl <- robcov(m.all.ols, mydf$sujeto)
m.all.ols.2 <- lm(formula = percevaded ~ncorrectret + cost_comply + treatment*ncorrectret + offerdg,
                 data=mydf)

m.all.r2<-summary(m.all.ols.2)$r.squared
m.all.r2<-round(m.all.r2, 2)
m.all.adj.r2<- summary(m.all.ols.2)$adj.r.squared



m.inter <- lrm( formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret + offerdg,
              data=mydf, x=T, y=T)
m.inter.cl <- robcov(m.inter, mydf$sujeto)
m.inter.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret + offerdg,
               data=mydf,
               family = binomial(link = "logit"))
m.inter.aic<-round(m.inter.2$aic, 2)




m.nf <- lrm( formula = cheat ~ ncorrectret + cost_comply + offerdg,
              data=non.fixed, x=T, y=T)
m.nf.cl <- robcov(m.nf, non.fixed$sujeto)
m.nf.2 <- glm(formula = cheat ~ ncorrectret + cost_comply,
              data=non.fixed,
                 family = binomial(link = "logit"))
m.nf.aic<-round(m.nf.2$aic, 2)


#logit cheat ncorrectret cost_comply receive receive_inter high_wage low_wage high_inter low_inter redistribution redistribute_inter, vce(cluster sujeto)



stargazer(m.inter.cl, m.bs.cl,  m.st.full.cl, m.sh.full.cl, m.red.cl, m.nf.cl, m.all.ols.cl,
          dep.var.labels.include = F,
          keep.stat = c("n"),
          add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
                         c("R squared", "", "", "", "", "", "", m.all.r2 )),
          model.numbers = T,
          dep.var.caption = "",
          #star.char = c("", "", ""),
          omit.table.layout = "n",
          type="text")



stargazer(m.inter.cl, m.bs.cl,  m.st.full.cl, m.sh.full.cl, m.red.cl, m.nf.cl, m.all.ols.cl,
          dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Gains from Cheating",
                             "High Status", "Low Status", "No Shock", "Receive Shock", "Redistribute",
                             "High Status","\\# of Additions X High Status",
                             "Receive Shock","\\# of Additions X Receive Shock",
                             "DG Offer",
                             "\\# of Additions X High Status", " \\# of Additions X Low Status", " \\# of Additions X No Shock", "\\# of Additions X Receive Shock", "\\# of Additions X Redistribute",
                            
                             "Constant"),
          keep.stat = c("n"),
          add.lines=list(c("AIC", m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic, "" ),
                         c("R squared", "", "", "", "", "", "", m.all.r2 )),
          model.numbers = T,
          dep.var.caption = "",
          star.char = c("", "", ""),
          omit.table.layout = "n",
          out="table3_25July2017.tex")

#############################################
#### Robustness test fixed effect model shock
#############################################

m.clogit<-clogit(formula = cheat ~ ncorrectret + cost_comply +
            receiveshock + receive_int+ strata(sujeto),
          data=shock.data )

m.fe.logit<-glm(formula = cheat ~ ncorrectret + cost_comply +
            receiveshock + receive_int + factor(sujeto),
          data=shock.data, family = binomial(link = "logit") )

stargazer(m.fe.logit, m.clogit, keep =  c("ncorrectret", "cost_comply" ,
                                            "receiveshock" , "receive_int" ),
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "Receive Shock","# of Additions X Receive Shock",
                             "Constant"),
          add.lines=list(c("Estimation", "Subject FE", "Conditional Logit")),
          out="table_appendix_fe_29Mar2017.tex")

######################################
#### Period 1 models, Table 6 Appendix
######################################

m.bs.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=sub.data.1[ sub.data.1$period==1, ], x=T, y=T)
m.bs.1.cl <- robcov(m.bs.1, sub.data.1[ sub.data.1$period==1, ]$sujeto)
m.bs.1.2 <- glm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
                data=sub.data.1[ sub.data.1$period==1, ],
              family = binomial(link = "logit"))
m.bs.1.aic<-round(m.bs.1.2$aic, 2)



m.st.full.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                      highsalary + HighsalaryAdd ,
                    data=sub.data.2[ sub.data.2$period==1, ], x=T, y=T)
m.st.full.1.cl<- robcov(m.st.full.1, sub.data.2[ sub.data.2$period==1, ]$sujeto)
m.st.full.1.2 <- glm(formula = cheat ~ ncorrectret + cost_comply +
                       highsalary + HighsalaryAdd ,
                     data=sub.data.2[ sub.data.2$period==1, ],
                family = binomial(link = "logit"))
m.st.full.1.aic<-round(m.st.full.1.2$aic, 2)




m.sh.full.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                      receiveshock + receive_int,
                    data=shock.data[ shock.data$period==1, ], x=T, y=T)
m.sh.full.1.cl<- robcov(m.sh.full.1, shock.data[ shock.data$period==1, ]$sujeto)
m.sh.full.1.2 <- glm(formula = cheat ~ ncorrectret + cost_comply +
                       receiveshock + receive_int,
                     data=shock.data[ shock.data$period==1, ],
                     family = binomial(link = "logit"))
m.sh.full.1.aic<-round(m.sh.full.1.2$aic, 2)




m.red.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply,
                data=redis.data[ redis.data$period == 1, ], x=T, y=T)
m.red.1.cl<- robcov(m.red.1, redis.data[ redis.data$period == 1, ]$sujeto)
m.red.1.2 <- glm(formula = cheat ~ ncorrectret + cost_comply,
                 data=redis.data[ redis.data$period == 1, ],
                     family = binomial(link = "logit"))
m.red.1.aic<-round(m.red.1.2$aic, 2)




m.all.1 <- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
             data=mydf[ mydf$period == 1, ], x=T, y=T)
m.all.1.cl <- robcov(m.all.1, mydf[ mydf$period == 1, ]$sujeto)
m.all.1.2 <- glm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
                 data=mydf[ mydf$period == 1, ],
                     family = binomial(link = "logit"))
m.all.1.aic<-round(m.all.1.2$aic, 2)



m.inter.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret,
                data=mydf[ mydf$period == 1, ], x=T, y=T)
m.inter.1.cl <- robcov(m.inter.1, mydf[ mydf$period == 1, ]$sujeto)
m.inter.1.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret,
                   data=mydf[ mydf$period == 1, ],
                     family = binomial(link = "logit"))
m.inter.1.aic<-round(m.inter.1.2$aic, 2)




m.nf.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply,
             data=non.fixed[ non.fixed$period == 1, ], x=T, y=T)
m.nf.1.cl <- robcov(m.nf.1, non.fixed[ non.fixed$period == 1, ]$sujeto)
m.nf.1.2 <- glm(formula = cheat ~ ncorrectret + cost_comply,
                data=non.fixed[ non.fixed$period == 1, ],
                     family = binomial(link = "logit"))
m.nf.1.aic<-round(m.nf.1.2$aic, 2)



stargazer(m.all.1.cl,m.inter.1.cl, m.bs.1.cl,  m.st.full.1.cl, m.sh.full.1.cl, m.red.1.cl, m.nf.1.cl, 
          add.lines=list(c("AIC", m.all.1.aic, m.inter.1.aic, m.bs.1.aic, m.st.full.1.aic, m.sh.full.1.aic, m.red.1.aic, m.nf.1.aic )),
          dep.var.labels.include = F, keep.stat = c("n"),
          model.numbers          = F,
          dep.var.caption = "",
          star.char = c("", "", ""),
          omit.table.layout = "n",
          type="text")


stargazer(m.all.1.cl,m.inter.1.cl, m.bs.1.cl,  m.st.full.1.cl, m.sh.full.1.cl, m.red.1.cl, m.nf.1.cl, 
          add.lines=list(c("AIC", m.all.1.aic, m.inter.1.aic, m.bs.1.aic, m.st.full.1.aic, m.sh.full.1.aic, m.red.1.aic, m.nf.1.aic )),
          dep.var.labels.include = F, keep.stat = c("n"),
          model.numbers          = F,
          dep.var.caption = "",
          star.char = c("", "", ""),
          omit.table.layout = "n",
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Status", "Low Status", "No Shock", "Receive Shock", "Redistribute",
                             "\\# of Additions X High Status", " \\# of Additions X Low Status", 
                             " \\# of Additions X No Shock", "\\# of Additions X Receive Shock", "\\# of Additions X Redistribute",
                             "High Status","# of Additions X High Status",
                             "Receive Shock","# of Additions X Receive Shock",
                             "Constant"),
          out="table_firstround_08nov2016.tex")


#####################################
## model evasion, table 5 Appendix
#####################################

m.bs <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
                 data=sub.data.1)
m.bs.aic<-round(AIC(m.bs), 2)

m.st.full <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply +
                        highsalary + HighsalaryAdd ,
                      data=sub.data.2)
m.st.full.aic<-round(AIC(m.st.full), 2)

m.sh.full <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply +
                        receiveshock + receive_int,
                      data=shock.data)
m.sh.full.aic<-round(AIC(m.sh.full), 2)


m.red <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,
                  data=redis.data)
m.red.aic<-round(AIC(m.red), 2)


m.all <- betareg( formula = percevaded.t~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=mydf)
m.all.aic<-round(AIC(m.all), 2)


m.inter <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply + treatment*ncorrectret,
                  data=mydf)
m.inter.aic<-round(AIC(m.inter), 2)


m.nf <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,
               data=non.fixed)
m.nf.aic<-round(AIC(m.nf),2)



stargazer(m.all, m.inter, m.bs,  m.st.full, m.sh.full, m.red, m.nf,
          add.lines=list(c("AIC", m.all.aic, m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic )),
          dep.var.labels.include = F, keep.stat = c("n"),
          model.numbers = F,
          dep.var.caption = "",
          star.char = c("", "", ""), 
          omit.table.layout = "n",
          type="text")



stargazer(m.all, m.inter, m.bs,  m.st.full, m.sh.full, m.red, m.nf,  
          dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Status", "Low Status", "No Shock", "Receive Shock", "Redistribute",
                             "\\# of Additions X High Status", " \\# of Additions X Low Status", " \\# of Additions X No Shock", "\\# of Additions X Receive Shock", "\\# of Additions X Redistribute",
                             "High Status","# of Additions X High Status",
                             "Receive Shock","# of Additions X Receive Shock",
                             "Constant"),
          keep.stat = c("n"),
          add.lines=list(c("AIC", m.all.aic, m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic )),
          model.numbers = T,
          dep.var.caption = "",
          star.char = c("", "", ""),
          omit.table.layout = "n",
          out="table_app_09nov2016.tex")


#######################################         
## model safe choices, table 7 Appendix
#######################################

m.bs <- lrm(formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
            data=sub.data.1, x=T, y=T)
m.bs.cl <- robcov(m.bs, sub.data.1$sujeto)

m.bs.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
              data=sub.data.1, family = binomial(link = "logit"))
m.bs.aic<-round(m.bs.2$aic, 2)
#m.bs.bic<-extractAIC(m.bs, k = log(n)) # for BIC

          
          

m.st.full <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                    highsalary + HighsalaryAdd + safechoices,
                  data=sub.data.2, x=T, y=T)
m.st.full.cl <- robcov(m.st.full, sub.data.2$sujeto)
m.st.full.2 <- glm(formula = cheat ~ ncorrectret + cost_comply +
                     highsalary + HighsalaryAdd + safechoices,
                   data=sub.data.2, 
                   family = binomial(link = "logit"))
m.st.full.aic<-round(m.st.full.2$aic, 2)




m.sh.full <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                    receiveshock + receive_int + safechoices,
                  data=shock.data, x=T, y=T)
m.sh.full.cl <- robcov(m.sh.full, shock.data$sujeto)
m.sh.full.2 <- glm(formula = cheat ~ ncorrectret + cost_comply +
                     receiveshock + receive_int + safechoices,
                   data=shock.data, 
                   family = binomial(link = "logit"))
m.sh.full.aic<-round(m.sh.full.2$aic, 2)



m.red <- lrm( formula = cheat ~ ncorrectret + cost_comply + safechoices,
              data=redis.data, x=T, y=T)
m.red.cl <- robcov(m.red, redis.data$sujeto)
m.red.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + safechoices,
               data=redis.data,
               family = binomial(link = "logit"))
m.red.aic<-round(m.red.2$aic, 2)




m.all <- lrm(formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
             data=mydf, x=T, y=T)
m.all.cl <- robcov(m.all, mydf$sujeto)
m.all.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
               data=mydf,
               family = binomial(link = "logit"))
m.all.aic<-round(m.all.2$aic, 2)




m.inter <- lrm( formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret + safechoices,
                data=mydf, x=T, y=T)
m.inter.cl <- robcov(m.inter, mydf$sujeto)
m.inter.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret + safechoices,
                 data=mydf,
                 family = binomial(link = "logit"))
m.inter.aic<-round(m.inter.2$aic, 2)




m.nf <- lrm( formula = cheat ~ ncorrectret + cost_comply + safechoices,
             data=non.fixed, x=T, y=T)
m.nf.cl <- robcov(m.nf, non.fixed$sujeto)
m.nf.2 <- glm(formula = cheat ~ ncorrectret + cost_comply + safechoices,
              data=non.fixed,
              family = binomial(link = "logit"))
m.nf.aic<-round(m.nf.2$aic, 2)


stargazer(m.all.cl, m.inter.cl, m.bs.cl,  m.st.full.cl, m.sh.full.cl, m.red.cl, m.nf.cl,  dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Status", "Low Status", "No Shock", "Receive Shock", "Redistribute",
                             "High Status","# of Additions X High Status",
                             "Receive Shock","# of Additions X Receive Shock",
                             "Safe choices",
                             "\\# of Additions X High Status", " \\# of Additions X Low Status", 
                             " \\# of Additions X No Shock", "\\# of Additions X Receive Shock", 
                             "\\# of Additions X Redistribute",
                             "Constant"),
          keep.stat = c("n"),
          add.lines=list(c("AIC", m.all.aic, m.inter.aic, m.bs.aic, m.st.full.aic, m.sh.full.aic, m.red.aic, m.nf.aic )),
          model.numbers = T,
          dep.var.caption = "",
          star.char = c("", "", ""),
          omit.table.layout = "n",
          out="table_safechoice_08nov2016.tex")


    
          


###########################
### Data Prep for Figure 4
###########################

### Baseline Data  #### Mistake in the code submitted to journal (safehoices, should not have been included)
m.bs <- lrm( formula = cheat ~ ncorrectret + cost_comply, #+ safechoices,# + ideology + gender + age_subject, 
             data=sub.data.1, x=T, y=T)
m.bs.cl<- robcov(m.bs, sub.data.1$sujeto)

df <- data.frame(predict(m.bs.cl, type = "lp", se.fit=TRUE))
cheat<-sub.data.1$cheat
correct<-sub.data.1$ncorrectret
cost<-sub.data.1$cost_comply
highsalary<-sub.data.1$highsalary
HighTax<-sub.data.1$HighTax  

df<-cbind(df, cheat, correct, cost, HighTax)

df <- within(df, {
  PredictedProb <- plogis(linear.predictors)
  LL <- plogis(linear.predictors - (1.96 * se.fit))
  UL <- plogis(linear.predictors + (1.96 * se.fit))
})

df<-subset(df, correct==7 | correct==12 | correct==17)

df<-ddply(df, .(correct, HighTax), summarize,  linear.predictors=mean(linear.predictors),
          se.fit=mean(se.fit), UL=mean(UL), LL=mean(LL), PredictedProb=mean(PredictedProb))
df$Treatment<-"Baseline"
df$highsalary<-NA
df$receiveshock<-NA
baseline<-df




### status Data
m.st <- lrm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
             data=sub.data.2, x=T, y=T)
m.st.cl <- robcov(m.st, sub.data.2$sujeto)

df <- data.frame(predict(m.st.cl, type = "lp", se.fit=TRUE))
cheat<-sub.data.2$cheat
correct<-sub.data.2$ncorrectret
cost<-sub.data.2$cost_comply
HighTax<-sub.data.1$HighTax
highsalary<-sub.data.2$highsalary

df<-cbind(df, cheat, correct, cost, highsalary, HighTax)

df <- within(df, {
  PredictedProb <- plogis(linear.predictors)
  LL <- plogis(linear.predictors - (1.96 * se.fit))
  UL <- plogis(linear.predictors + (1.96 * se.fit))
})

df<-subset(df, correct==7 | correct==12 | correct==17)

df<-ddply(df, .(correct, highsalary, HighTax), summarize,  linear.predictors=mean(linear.predictors),
          se.fit=mean(se.fit), UL=mean(UL), LL=mean(LL), PredictedProb=mean(PredictedProb))
df$Treatment<-"Low Status"
df$Treatment[df$highsalary==1]<-"High Status"
df$receiveshock<-NA
status<-df




### Shock Data 
m.sh <- lrm( formula = cheat ~ ncorrectret + cost_comply,
             data=shock.data, x=T, y=T)
m.sh.cl <- robcov(m.sh, shock.data$sujeto)

df <- data.frame(predict(m.sh.cl, type = "lp", se.fit=TRUE))
cheat<-shock.data$cheat
correct<-shock.data$ncorrectret
cost<-shock.data$cost_comply
HighTax<-shock.data$HighTax
receiveshock<-shock.data$receiveshock

df<-cbind(df, cheat, correct, cost, HighTax, receiveshock)

df <- within(df, {
  PredictedProb <- plogis(linear.predictors)
  LL <- plogis(linear.predictors - (1.96 * se.fit))
  UL <- plogis(linear.predictors + (1.96 * se.fit))
})

df<-subset(df, correct==7 | correct==12 | correct==17)

df<-ddply(df, .(correct, receiveshock, HighTax), summarize,  linear.predictors=mean(linear.predictors),
          se.fit=mean(se.fit), UL=mean(UL), LL=mean(LL), PredictedProb=mean(PredictedProb))
df$Treatment<-"No Shock"
df$Treatment[df$receiveshock==1]<-"Receive Shock"
df$highsalary<-NA
shock<-df




### Redistribute Data 
m.red <- lrm( formula = cheat ~ ncorrectret + cost_comply,
              data=redis.data, x=T, y=T)
m.red.cl <- robcov(m.red, redis.data$sujeto)

df <- data.frame(predict(m.red.cl, type = "lp", se.fit=TRUE))
cheat<-redis.data$cheat
correct<-redis.data$ncorrectret
cost<-redis.data$cost_comply
HighTax<-redis.data$HighTax

df<-cbind(df, cheat, correct, cost, HighTax)

df <- within(df, {
  PredictedProb <- plogis(linear.predictors)
  LL <- plogis(linear.predictors - (1.96 * se.fit))
  UL <- plogis(linear.predictors + (1.96 * se.fit))
})

df<-subset(df, correct==7 | correct==12 | correct==17)

df<-ddply(df, .(correct, HighTax), summarize,  linear.predictors=mean(linear.predictors),
          se.fit=mean(se.fit), UL=mean(UL), LL=mean(LL), PredictedProb=mean(PredictedProb))
df$Treatment<-"Redistribute"
df$highsalary<-NA
df$receiveshock<-NA
red<-df


##########################
#Figure 4, Treatment Plots
##########################

data.plot<-rbind(status, baseline, shock, red)
data.plot$HighTax_lb<-"Low Tax"
data.plot$HighTax_lb[data.plot$HighTax==1]<-"High Tax"



g<-ggplot(data.plot, aes(x = correct, y = PredictedProb , color = Treatment) ) +
  ylab("")  + xlab("Number of correct responses") + ylab("Predicted Probabilities") +
  scale_x_continuous(breaks=c(7,12,17),
                     labels=c("7", "12", "17")) +
  geom_point(size = 4)  + #ylim(c(-0.7, 0.1)) +
  geom_errorbar(aes(ymax = UL, ymin = LL)) + ylim(0, 1)+
  facet_grid( Treatment ~ HighTax_lb) + guides(color=FALSE)
g
ggsave(filename= "treatment_v_tax.pdf", height=10, width=10)


############################################
##Percent evaded, period -  figure 6 appendix
############################################
df<-ddply(mydf, .(treatment, period, grupo), summarize,  
          percevaded=mean(percevaded))

g<-ggplot(df, aes(x = period, y = percevaded*100, color = treatment))  + 
  geom_line(size=1) + ylim(0,100) + ylab("Percent Evaded") + xlab("Period") + 
facet_wrap( ~ grupo, ncol = 8) + scale_color_discrete(name="Treatment")
g 
  
ggsave(filename= "percevad_period_treatment.pdf", height=10, width=10)
