

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

setwd("C:/Users/Denise Laroze/Dropbox/Denise/Cheating/Why-we-Cheat-2016-master/Replication/Laroze rep package")
#fig.path <- "Laroze rep package"



data <- read.dta("base.dta")
shock.data <- read.dta13("shock.dta")
shock.data <- shock.data[shock.data$session >24, ]
redis.data <- read.dta('redistribution.dta')

non.fixed<-read.dta13('MasterfileOxford2016e.dta')
non.fixed<-subset(non.fixed, auditrate==0)
non.fixed$percevaded <- (non.fixed$profitret - non.fixed$declared)/non.fixed$profitret


data$HighTax  <- as.numeric(apply(data[,c("T20","T30")],1,sum)>0)
shock.data$HighTax  <- as.numeric(apply(shock.data[,c("T20","T30")],1,sum)>0)
redis.data$HighTax  <- as.numeric(apply(redis.data[,c("T20","T30")],1,sum)>0)

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

table(data$HighTax)

### Creating one general df
kv<-c("sujeto", "grupo","cheat", "ncorrectret", "cost_comply", "percevaded", "percevaded.t", "safechoices", "period")

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

m.bs <- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
         data=sub.data.1, x=T, y=T)
m.bs.cl <- robcov(m.bs, sub.data.1$sujeto)


m.st <- lrm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
             data=sub.data.2, x=T, y=T)
m.st.cl <- robcov(m.st, sub.data.2$sujeto)


m.st.full <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                    highsalary + HighsalaryAdd ,
                  data=sub.data.2, x=T, y=T)
m.st.full.cl <- robcov(m.st.full, sub.data.2$sujeto)


m.sh <- lrm( formula = cheat ~ ncorrectret + cost_comply,
             data=shock.data, x=T, y=T)
m.sh.cl <- robcov(m.sh, shock.data$sujeto)


m.sh.full <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                    receiveshock + receive_int,
                  data=shock.data, x=T, y=T)
m.sh.full.cl <- robcov(m.sh.full, shock.data$sujeto)


m.red <- lrm( formula = cheat ~ ncorrectret + cost_comply,
              data=redis.data, x=T, y=T)
m.red.cl <- robcov(m.red, redis.data$sujeto)


m.all <- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
             data=mydf, x=T, y=T)
m.all.cl <- robcov(m.all, mydf$sujeto)


m.inter <- lrm( formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret,
              data=mydf, x=T, y=T)
m.inter.cl <- robcov(m.inter, mydf$sujeto)

m.nf <- lrm( formula = cheat ~ ncorrectret + cost_comply,
              data=non.fixed, x=T, y=T)
m.nf.cl <- robcov(m.nf, non.fixed$sujeto)


#logit cheat ncorrectret cost_comply receive receive_inter high_wage low_wage high_inter low_inter redistribution redistribute_inter, vce(cluster sujeto)



stargazer(m.all.cl, m.inter.cl, m.bs.cl,  m.st.full.cl, m.sh.full.cl, m.red.cl,  dep.var.labels.include = F,type="text")

stargazer(m.all.cl, m.inter.cl, m.bs.cl,  m.st.full.cl, m.sh.full.cl, m.red.cl, m.nf.cl
          , dep.var.labels.include = F,type="text")


stargazer(m.all.cl, m.inter.cl, m.bs.cl,  m.st.full.cl, m.sh.full.cl, m.red.cl, m.nf.cl,  dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Status", "Low Status", "No Shock", "Receive Shock", "Redistribute",
                             "\\# of Additions X High Status", " \\# of Additions X Low Status", " \\# of Additions X No Shock", "\\# of Additions X Receive Shock", "\\# of Additions X Redistribute",
                             "High Status","# of Additions X High Status",
                             "Receive Shock","# of Additions X Receive Shock",
                             "Constant"),out="table3_17Oct2016.tex")




######################################
#### Period 1 models, Table 6 Appendix
######################################

m.bs.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=sub.data.1[ sub.data.1$period==1, ], x=T, y=T)
m.bs.1.cl <- robcov(m.bs.1, sub.data.1[ sub.data.1$period==1, ]$sujeto)


m.st.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=sub.data.2[ sub.data.2$period==1, ], x=T, y=T)
m.st.1.cl<- robcov(m.st.1, sub.data.2[ sub.data.2$period==1, ]$sujeto)

m.st.full.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                      highsalary + HighsalaryAdd ,
                    data=sub.data.2[ sub.data.2$period==1, ], x=T, y=T)
m.st.full.1.cl<- robcov(m.st.full.1, sub.data.2[ sub.data.2$period==1, ]$sujeto)


m.sh.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply,
               data=shock.data[ shock.data$period==1, ], x=T, y=T)
m.sh.1.cl<- robcov(m.sh.1, shock.data[ shock.data$period==1, ]$sujeto)

m.sh.full.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                      receiveshock + receive_int,
                    data=shock.data[ shock.data$period==1, ], x=T, y=T)
m.sh.full.1.cl<- robcov(m.sh.full.1, shock.data[ shock.data$period==1, ]$sujeto)

m.red.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply,
                data=redis.data[ redis.data$period == 1, ], x=T, y=T)
m.red.1.cl<- robcov(m.red.1, redis.data[ redis.data$period == 1, ]$sujeto)



m.all.1 <- lrm(formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
             data=mydf[ mydf$period == 1, ], x=T, y=T)
m.all.1.cl <- robcov(m.all.1, mydf[ mydf$period == 1, ]$sujeto)

m.inter.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret,
                data=mydf[ mydf$period == 1, ], x=T, y=T)
m.inter.1.cl <- robcov(m.inter.1, mydf[ mydf$period == 1, ]$sujeto)

m.nf.1 <- lrm( formula = cheat ~ ncorrectret + cost_comply,
             data=non.fixed[ non.fixed$period == 1, ], x=T, y=T)
m.nf.1.cl <- robcov(m.nf.1, non.fixed[ non.fixed$period == 1, ]$sujeto)

stargazer(m.all.1.cl,m.inter.1, m.bs.1.cl,  m.st.full.1.cl, m.sh.full.1.cl, m.red.1.cl, m.nf.1.cl)

stargazer(m.all.1.cl,m.inter.1, m.bs.1.cl,  m.st.full.1.cl, m.sh.full.1.cl, m.red.1.cl, m.nf.1.cl, 
          dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Status", "Low Status", "No Shock", "Receive Shock", "Redistribute",
                             "\\# of Additions X High Status", " \\# of Additions X Low Status", 
                             " \\# of Additions X No Shock", "\\# of Additions X Receive Shock", "\\# of Additions X Redistribute",
                             "High Status","# of Additions X High Status",
                             "Receive Shock","# of Additions X Receive Shock",
                             "Constant"),out="table_firstround_17oct2016.tex")


#####################################
## model evasion, table 5 Appendix
#####################################

m.bs <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
                 data=sub.data.1,  x=T, y=T)

m.st <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
                 data=sub.data.2)
m.st.full <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply +
                        highsalary + HighsalaryAdd ,
                      data=sub.data.2)
m.sh <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,
                 data=shock.data)
m.sh.full <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply +
                        receiveshock + receive_int,
                      data=shock.data)
m.red <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,
                  data=redis.data)


m.all <- betareg( formula = percevaded.t~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=mydf)


m.inter <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply + treatment*ncorrectret,
                  data=mydf)

m.nf <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,
               data=non.fixed)


stargazer(m.all, m.inter, m.bs, m.st.full, m.sh.full, m.red, m.nf, dep.var.labels.include = F,type="text")
stargazer(m.all, m.inter, m.bs, m.st.full, m.sh.full, m.red, m.nf,  dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Status", "Low Status", "No Shock", "Receive Shock", "Redistribute",
                             "\\# of Additions X High Status", " \\# of Additions X Low Status", " \\# of Additions X No Shock", "\\# of Additions X Receive Shock", "\\# of Additions X Redistribute",
                             "High Status","# of Additions X High Status",
                             "Receive Shock","# of Additions X Receive Shock",
                             "Constant"),out="table_app_26Oct2016.tex")


#####################################
## model safe choices, table 7 Appendix
#####################################

m.bs <- lrm( formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
             data=sub.data.1, x=T, y=T)
m.bs.cl<- robcov(m.bs, sub.data.1$sujeto)

m.st <- lrm( formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
             data=sub.data.2, x=T, y=T)
m.st.cl<- robcov(m.st, sub.data.2$sujeto)

m.st.full <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                    highsalary + HighsalaryAdd + safechoices,
                  data=sub.data.2,  x=T, y=T)
m.st.full.cl<- robcov(m.st.full, sub.data.2$sujeto)

m.sh <- lrm( formula = cheat ~ ncorrectret + cost_comply + safechoices,
             data=shock.data, x=T, y=T)
m.sh.cl<- robcov(m.sh, shock.data$sujeto)

m.sh.full <- lrm( formula = cheat ~ ncorrectret + cost_comply +
                    receiveshock + receive_int + safechoices,
                  data=shock.data, x=T, y=T)
m.sh.full.cl<- robcov(m.sh.full, shock.data$sujeto)

m.red <- lrm( formula = cheat ~ ncorrectret + cost_comply + safechoices,
              data=redis.data, x=T, y=T)
m.red.cl<- robcov(m.red, redis.data$sujeto)

m.all <- lrm(formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
             data=mydf, x=T, y=T)
m.all.cl <- robcov(m.all, mydf$sujeto)


m.inter <- lrm( formula = cheat ~ ncorrectret + cost_comply + treatment*ncorrectret+ safechoices,
                data=mydf, x=T, y=T)
m.inter.cl <- robcov(m.inter, mydf$sujeto)

m.nf <- lrm( formula = cheat ~ ncorrectret + cost_comply+ safechoices,
             data=non.fixed, x=T, y=T)
m.nf.cl <- robcov(m.nf, non.fixed$sujeto)



stargazer(m.all.cl, m.inter.cl, m.bs.cl, m.st.full.cl,  m.sh.full.cl, m.red.cl, m.nf.cl, dep.var.labels.include = F,type="text",
          out="table_safechoice_oct2016.tex")

stargazer(m.all.cl, m.inter.cl, m.bs.cl, m.st.full.cl,  m.sh.full.cl, m.red.cl, m.nf.cl, dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Status", "Low Status", "No Shock", "Receive Shock", "Redistribute",
                             "High Status","# of Additions X High Status",
                             "Receive Shock","# of Additions X Receive Shock",
                             "Safe choices",
                             "\\# of Additions X High Status", " \\# of Additions X Low Status", 
                             " \\# of Additions X No Shock", "\\# of Additions X Receive Shock", 
                             "\\# of Additions X Redistribute",
                             "Constant"),out="table_safechoice_17oct2016.tex")



#######################
### Data Prep for Figure 4
#######################

### Baseline Data
m.bs <- lrm( formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
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
ggsave(filename= "treatment_v_tax.pdf", path=fig.path, height=10, width=10)


#########################
##Percent evaded, period
#########################
df<-ddply(mydf, .(treatment, period, grupo), summarize,  
          percevaded=mean(percevaded))



g<-ggplot(df, aes(x = period, y = percevaded*100, color = treatment))  + 
  geom_line(size=1) + ylim(0,100) + ylab("Percent Evaded") + xlab("Period") + 
facet_wrap( ~ grupo, ncol = 8) + scale_color_discrete(name="Treatment")
g 
  
ggsave(filename= "percevad_period_treatment.pdf", height=10, width=10)
