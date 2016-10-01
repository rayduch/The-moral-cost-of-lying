setwd("~/Dropbox/Tax Compliance Experiments/Data/Replication")
fig.path <- "~/Dropbox/taxes_ray/Text"
rm(list=ls())
library(foreign)
library(Zelig)
library(ZeligMultilevel)
library(ggplot2)
library(lme4)
library(readstata13)
library(betareg)
library(stargazer)
library(RColorBrewer)
theme_set(theme_bw())

data <- read.dta("additional_data/base.dta")
shock.data <- read.dta13("additional_data/shock.dta")
shock.data <- shock.data[shock.data$session >24, ]
redis.data <- read.dta('additional_data/redistribution.dta')


data$HighTax  <- as.numeric(apply(data[,c("T20","T30")],1,sum)>0)
shock.data$HighTax  <- as.numeric(apply(shock.data[,c("T20","T30")],1,sum)>0)
redis.data$HighTax  <- as.numeric(apply(redis.data[,c("T20","T30")],1,sum)>0)

data$percevaded.t <- data$percevaded*.999+.0005
shock.data$percevaded.t <- shock.data$percevaded*.999+.0005
redis.data$percevaded.t <- redis.data$percevaded*.999+.0005

data$cost_comply <- (data$taxrate/100) * data$profitret
shock.data$cost_comply <- (shock.data$taxrate/100) * shock.data$profitret
redis.data$cost_comply <- (redis.data$taxrate/100) * redis.data$profitret

shock.data$receive_int <- with(shock.data, receiveshock*ncorrectret)

sub.data.1 <- subset(data,difsalaries == 0)
sub.data.2 <- subset(data,difsalaries == 1)



table(data$HighTax)

## model estimation, table output
m.bs.1 <- glm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=sub.data.1[ sub.data.1$period==1, ], family = binomial(link="probit" ))
m.bs <- glm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
             data=sub.data.1, family = binomial(link="probit" ))
m.st <- glm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
             data=sub.data.2, family = binomial(link="probit" ))
m.st.full <- glm( formula = cheat ~ ncorrectret + cost_comply +
                    highsalary + HighsalaryAdd ,
                  data=sub.data.2, family = binomial(link="probit" ))
m.st.1 <- glm( formula = cheat ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
               data=sub.data.2[ sub.data.2$period==1, ], family = binomial(link="probit" ))
m.st.full.1 <- glm( formula = cheat ~ ncorrectret + cost_comply +
                      highsalary + HighsalaryAdd ,
                    data=sub.data.2[ sub.data.2$period==1, ], family = binomial(link="probit" ))


m.sh <- glm( formula = cheat ~ ncorrectret + cost_comply,
             data=shock.data, family = binomial(link="probit" ))
m.sh.full <- glm( formula = cheat ~ ncorrectret + cost_comply +
                    receiveshock + receive_int,
                  data=shock.data, family = binomial(link="probit" ))
m.sh.1 <- glm( formula = cheat ~ ncorrectret + cost_comply,
               data=shock.data[ shock.data$period==1, ], family = binomial(link="probit" ))
m.sh.full.1 <- glm( formula = cheat ~ ncorrectret + cost_comply +
                      receiveshock + receive_int,
                    data=shock.data[ shock.data$period==1, ], family = binomial(link="probit" ))

m.red <- glm( formula = cheat ~ ncorrectret + cost_comply,
              data=redis.data, family = binomial(link="probit" ))
m.red.1 <- glm( formula = cheat ~ ncorrectret + cost_comply,
                data=redis.data[ redis.data$period == 1, ], family = binomial(link="probit" ))
stargazer(m.bs, m.st, m.st.full, m.sh, m.sh.full, m.red, dep.var.labels.include = F,type="text")
stargazer(m.bs, m.st, m.st.full, m.sh, m.sh.full, m.red, dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Salary","High Salary X Additions",
                             "Receive Shock","Receive Shock X Additions",
                             "Constant"),out="table2_nov2015.tex")

stargazer(m.bs.1, m.st.1, m.st.full.1, m.sh.1, m.sh.full.1, m.red.1, dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Salary","High Salary X Additions",
                             "Receive Shock","Receive Shock X Additions",
                             "Constant"),out="table_firstround_nov2015.tex")
# 

m.bs <- betareg( formula = percevaded.t ~ ncorrectret + cost_comply,# + ideology + gender + age_subject, 
                 data=sub.data.1)
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
stargazer(m.bs, m.st, m.st.full, m.sh, m.sh.full, m.red, dep.var.labels.include = F,type="text")
stargazer(m.bs, m.st, m.st.full, m.sh, m.sh.full, m.red, dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Salary","High Salary X Additions",
                             "Receive Shock","Receive Shock X Additions",
                             "Constant"),out="table_app_nov2015.tex")



## model estimation, table output
m.bs <- glm( formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
             data=sub.data.1, family = binomial(link="probit" ))
m.st <- glm( formula = cheat ~ ncorrectret + cost_comply + safechoices,# + ideology + gender + age_subject, 
             data=sub.data.2, family = binomial(link="probit" ))
m.st.full <- glm( formula = cheat ~ ncorrectret + cost_comply +
                    highsalary + HighsalaryAdd + safechoices,
                  data=sub.data.2, family = binomial(link="probit" ))
m.sh <- glm( formula = cheat ~ ncorrectret + cost_comply + safechoices,
             data=shock.data, family = binomial(link="probit" ))
m.sh.full <- glm( formula = cheat ~ ncorrectret + cost_comply +
                    receiveshock + receive_int + safechoices,
                  data=shock.data, family = binomial(link="probit" ))
m.red <- glm( formula = cheat ~ ncorrectret + cost_comply + safechoices,
              data=redis.data, family = binomial(link="probit" ))
stargazer(m.bs, m.st, m.st.full, m.sh, m.sh.full, m.red, dep.var.labels.include = F,type="text")
stargazer(m.bs, m.st, m.st.full, m.sh, m.sh.full, m.red, dep.var.labels.include = F,
          covariate.labels=c("\\# of Additions", "Cost of Compliance",
                             "High Salary","High Salary X Additions",
                             "Receive Shock","Receive Shock X Additions", "Safe Choice",
                             "Constant"),out="table2_safechoice_nov2015.tex")




## substantive effect plot (High Tax)
fig.path <- "figs"

for (tx.rate in c(10, 20 ,30)) {
  z.out.bs <- zelig( formula = cheat ~  ncorrectret + ncorrectret * taxrate - taxrate,
                     data=sub.data.1, model = "probit" ) ## use probit instead
  summary(z.out.bs)
  m.bs.plot <- NULL
  for(i in 5:20){
    x <- setx(z.out.bs,ncorrectret=i, taxrate = tx.rate)#,gender=1)
    s.out <- sim(z.out.bs, x=x)
    #print(summary(s.out))
    #print(s.out$stats[[1]])
    m.bs.plot <- rbind(m.bs.plot, c(i,s.out$stats[[1]][3:5]))
  }
  m.bs.plot <- data.frame(m.bs.plot)
  names(m.bs.plot) <- c('ncorrectret','median','ll','ul')
  ggplot(m.bs.plot,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll))+geom_pointrange()
  #ggsave("Model1.pdf",width=4,height=5)
  m.bs.plot$Model <- "Baseline"
  
  z.out.st <- zelig( formula = cheat ~ ncorrectret + ncorrectret * taxrate - taxrate +
                       highsalary * ncorrectret,
                     data=sub.data.2, model = "probit")
  summary(z.out.st)
  m.st.plot.high <- NULL
  for(i in 5:20){
    x <- setx(z.out.st,ncorrectret=i, taxrate = tx.rate, highsalary=1)
    s.out <- sim(z.out.st, x=x)
    #print(s.out$stats[[1]])
    m.st.plot.high <- rbind(m.st.plot.high, c(i,s.out$stats[[1]][3:5]))
  }
  m.st.plot.high <- data.frame(m.st.plot.high)
  names(m.st.plot.high) <- c('ncorrectret','median','ll','ul')
  ggplot(m.st.plot.high,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll))+geom_pointrange()
  #ggsave("Model3.high.pdf",width=4,height=5)
  
  m.st.plot.low <- NULL
  for(i in 5:20){
    x <- setx(z.out.st,ncorrectret = i, taxrate = tx.rate, highsalary=0)
    s.out <- sim(z.out.st, x=x)
    #print(s.out$stats[[1]])
    m.st.plot.low <- rbind(m.st.plot.low, c(i,s.out$stats[[1]][3:5]))
    
  }
  m.st.plot.low <- data.frame(m.st.plot.low)
  names(m.st.plot.low) <- c('ncorrectret','median','ll','ul')
  ggplot(m.st.plot.low,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll))+geom_pointrange()
  
  m.st.plot.high$Model <- "High Status"
  m.st.plot.low$Model <- "Low Status"
  
  plot.total <- rbind(m.bs.plot,m.st.plot.high,m.st.plot.low)
  ggplot(plot.total,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll,colour=plot.total$Model))+
    geom_pointrange(position=position_dodge(width=0.3, height=0))+
    theme_bw()+ylim(c(0,1))+xlab("Number of RET Correct Answers") + ylab("Predicted Probablity")+
    scale_colour_discrete(guide = guide_legend(title = "")) + theme(legend.position='bottom')
  ggsave(sprintf("Predictions_Status_TaxRate%s.pdf", tx.rate), width=4, height=5, path = fig.path)
  
  
  ##
  z.out.sh<- zelig( formula = cheat  ~ ncorrectret + ncorrectret * taxrate - taxrate +
                      receiveshock + receiveshock * ncorrectret, 
                    data=shock.data, model="probit")
  
  
  ##
  m.sh.plot.high <- NULL
  for(i in 5:20){
    x <- setx(z.out.sh,ncorrectret=i, taxrate = tx.rate,receiveshock=1)
    s.out <- sim(z.out.sh, x=x)
    #print(s.out$stats[[1]])
    m.sh.plot.high <- rbind(m.sh.plot.high, c(i,s.out$stats[[1]][3:5]))
  }
  m.sh.plot.high <- data.frame(m.sh.plot.high)
  names(m.sh.plot.high) <- c('ncorrectret','median','ll','ul')
  ggplot(m.sh.plot.high,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll))+geom_pointrange()
  #ggsave("Model3.high.pdf",width=4,height=5)
  
  m.sh.plot.low <- NULL
  for(i in 5:20){
    x <- setx(z.out.sh,ncorrectret=i, taxrate = tx.rate,receiveshock=0)
    s.out <- sim(z.out.sh, x=x)
    #print(s.out$stats[[1]])
    m.sh.plot.low <- rbind(m.sh.plot.low, c(i,s.out$stats[[1]][3:5]))
    
  }
  m.sh.plot.low <- data.frame(m.sh.plot.low)
  names(m.sh.plot.low) <- c('ncorrectret','median','ll','ul')
  ggplot(m.sh.plot.low,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll))+geom_pointrange()
  
  m.sh.plot.high$Model <- "Receive Shock"
  m.sh.plot.low$Model <- "No Shock"
  
  plot.total <- rbind(m.bs.plot,m.sh.plot.high,m.sh.plot.low)
  plot.total$Model <- factor(plot.total$Model,levels=c("Baseline","Receive Shock","No Shock"))
  ggplot(plot.total,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll,colour=plot.total$Model))+
    geom_pointrange(position=position_dodge(width=0.3, height=0))+
    theme_bw()+ylim(c(0,1))+xlab("Number of RET Correct Answers") + ylab("Predicted Probablity")+
    scale_colour_discrete(guide = guide_legend(title = "")) + theme(legend.position='bottom')
  ggsave(sprintf("Predictions_Shock_TaxRate%s.pdf", tx.rate), width=4, height=5, path = fig.path)
  
  
  
  
  ##
  z.out.red<- zelig( formula = cheat ~ ncorrectret + ncorrectret * taxrate - taxrate, 
                     data=redis.data, model="probit")
  
  
  ##
  m.red.plot <- NULL
  for(i in 5:20){
    x <- setx(z.out.red,ncorrectret=i, taxrate = tx.rate)
    s.out <- sim(z.out.red, x=x)
    #print(s.out$stats[[1]])
    m.red.plot <- rbind(m.red.plot, c(i,s.out$stats[[1]][3:5]))
  }
  m.red.plot <- data.frame(m.red.plot)
  names(m.red.plot) <- c('ncorrectret','median','ll','ul')
  ggplot(m.red.plot,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll))+geom_pointrange()
  #ggsave("Model3.high.pdf",width=4,height=5)
  
  m.red.plot$Model <- "Redistribute"
  
  plot.total <- rbind(m.bs.plot,m.red.plot)
  plot.total$Model <- factor(plot.total$Model,levels=c("Baseline","Redistribute"))
  ggplot(plot.total,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll,colour=plot.total$Model))+ 
    geom_pointrange(position=position_dodge(width=0.3, height=0))+
    #  geom_pointrange(position=position_dodge(width=0.3, height=0))+
    theme_bw()+ylim(c(0,1))+xlab("Number of RET Correct Answers") + ylab("Predicted Probablity")+
    scale_colour_discrete(guide = guide_legend(title = "")) + theme(legend.position='bottom')
  ggsave(sprintf("Predictions_Redistribute_TaxRate%s.pdf", tx.rate), width=4, height=5, path = fig.path)
  
  ## new plot (15-Feb-2016)
  top.panel <- rbind(m.bs.plot,m.st.plot.high,m.st.plot.low)
  top.panel$panel <- "Status"
  mid.panel <- rbind(m.bs.plot,m.sh.plot.high,m.sh.plot.low)
  mid.panel$panel <- "Shock"
  bottom.panel <- rbind(m.bs.plot,m.red.plot)
  bottom.panel$panel <- "Redistribution"
  plot.total <- rbind(top.panel, mid.panel, bottom.panel)
  
  plot.total$tx.rate <- tx.rate
  assign(sprintf("Predictions_%s", tx.rate), plot.total)
  plot.total$panel <- factor(plot.total$panel,levels=c("Status", "Shock","Redistribution"))
  plot.total$Model <- factor(plot.total$Model,
                             levels=c("Baseline", "High Status","Low Status",
                                      "Receive Shock", "No Shock", "Redistribute"))
 
  
  ggplot(plot.total,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll,colour=plot.total$Model))+ 
    geom_pointrange(position=position_dodge(width=0.5, height=0))+
    #  geom_pointrange(position=position_dodge(width=0.3, height=0))+
    facet_grid(panel ~ .)+
    theme_bw()+ylim(c(0,1))+xlab("Number of RET Correct Answers") + ylab("Predicted Probablity")+
    scale_colour_manual(guide = guide_legend(title = ""), values = brewer.pal(6, "Dark2")) + theme(legend.position='right') 
  ggsave(sprintf("Predictions_TaxRate%s.pdf", tx.rate), width=7, height=9, path = fig.path)
  print(sprintf("Predictions_TaxRate%s.pdf", tx.rate))
  
  
  
}


plot.total <- rbind(Predictions_10, Predictions_30)
plot.total$panel <- factor(plot.total$panel,levels=c("Status", "Shock","Redistribution"))
plot.total$Model <- factor(plot.total$Model,
                           levels=c("Baseline", "High Status","Low Status",
                                    "Receive Shock", "No Shock", "Redistribute"))
plot.total$tx.rate <- factor(plot.total$tx.rate, labels = c("Low Cost (10% Tax)", "High Cost (30% Tax)"))
ggplot(plot.total,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll,colour=Model))+ 
  geom_pointrange(position=position_dodge(width=0.5, height=0))+
  #  geom_pointrange(position=position_dodge(width=0.3, height=0))+
  facet_grid(panel ~ tx.rate)+
  theme_bw()+ylim(c(0,1))+xlab("Number of RET Correct Answers") + ylab("Predicted Probablity")+
  scale_colour_manual(guide = guide_legend(title = ""), values = brewer.pal(6, "Dark2")) + theme(legend.position='right') 
ggsave(sprintf("Predictions_fig5.pdf", tx.rate), width=9, height=9, path = fig.path)

## only baseline

plot.total <- plot.total[ plot.total$Model == "Baseline" & plot.total$panel == "Status",]
ggplot(plot.total,aes(x=ncorrectret,y=median,ymax=ul,ymin=ll,colour=tx.rate))+ 
  geom_pointrange(position=position_dodge(width=0.5, height=0))+
  #  geom_pointrange(position=position_dodge(width=0.3, height=0))+
  #facet_grid(panel ~ tx.rate)+
  theme_bw()+ylim(c(0,1))+xlab("Number of RET Correct Answers") + ylab("Predicted Probablity")+
  scale_colour_manual(guide = guide_legend(title = ""), values = brewer.pal(6, "Dark2")) + theme(legend.position='bottom') 
ggsave(sprintf("Predictions_fig5_onlybaseline.pdf", tx.rate), width=6, height=6, path = fig.path) 



library(texreg)
model1 <- glm(formula = cheat ~  ncorrectret + ncorrectret * taxrate,
             data=sub.data.1, family=binomial(link="probit"))
texreg(model1, digits =3)
