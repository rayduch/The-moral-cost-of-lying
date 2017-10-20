
##########################################################
### Who Cheats?
### Duch and Solaz
### Code for figures
### Code written by Denise Laroze deniselaroze@gmail.com
##########################################################


library(foreign)
library(ggplot2)
theme_set(theme_bw())
library(reshape)
library(scales)
library(Rmisc)
library(plyr)


rm(list=ls())

setwd("C:/Users/André Laroze/Dropbox/CESS-Santiago/Archive/Tax Compliance Experiments/Rep Material/Laroze rep package")



#############################
## Data Frames and management
##############################

#### Main data frame with sessions up to 2016
#### 2017 data is included in separate file as 
#### orginal is too large for the xlsx format stability

dat <- read.dta("MasterfileOxfordChile_20160506_b.dta")

dat<-subset(dat, chile==0 | is.na(chile)) # Only UK data
dat$treatment <- NA
dat$treatment[dat$session %in% 1:3] <- 1
dat$treatment[dat$session %in% 15:18] <- 2 # Status
dat$treatment[dat$session %in% 25:27] <- 3 # Shock
dat$treatment[dat$session %in% 19:22] <- 4 # Redistribution
dat$treatment[dat$session %in% 31:36] <- 5 # non-fixed
dat$treatment_lab <- factor(dat$treatment, labels = c("Baseline",
                                                      "Status",
                                                      "Shock",
                                                      "Redistribution",
                                                      "Non-fixed"))
dat$treatment2 <- NA
dat$treatment2[dat$session %in% 1:3] <- 1
dat$treatment2[dat$session %in% 15:18 & dat$highsalary == 0] <- 2 # Status
dat$treatment2[dat$session %in% 15:18 & dat$highsalary == 1] <- 3 # Status
dat$treatment2[dat$session %in% 25:27 & dat$receiveshock == 0] <- 4 # Shock
dat$treatment2[dat$session %in% 25:27 & dat$receiveshock == 1] <- 5 # Shock
dat$treatment2[dat$session %in% 19:22] <- 6 # Redistribution
dat$treatment2[dat$session %in% 31:36] <- 7 # non-fixed
dat$treatment2_lab <- factor(dat$treatment2, labels = c("Baseline",
                                                       "Status (Low)",
                                                       "Status (High)",
                                                       "Shock (No)",
                                                       "Shock (Yes)",
                                                       "Redistribution",
                                                       "Non-fixed"))


dat$HighTax  <- as.numeric(apply(dat[,c("T20","T30", "T40")],1,sum)>0)

###########################################
### Adding 2017 fixed die data and merging 
###########################################


fixed.die <- read.csv("additional_data/fixed_die_sessions.csv", sep=";")
names(fixed.die) <- tolower(names(fixed.die))
#fixed.die<-subset(die_one, Treatment!=2 ) ### Eliminating test rounds, where RET==-1 (pushing down mean) and treatment/module==2

## Generating high performance variable


kv<-c("sujeto", "session",  "grupo","cheat", "ncorrectret", "percevaded", "safechoices", "period", "offerdg", "auditrate",
      "taxrate", "profitret", "highsalary", "shock", "receiveshock",
      "treatment_lab" ,  "treatment2_lab", "realdie"  , "die_type",
      "gender", "age_subject", "trust"  , "ideology", "init_pred"   ,      "avg_pred"
)


fixed.die$die_type<-"Fixed die"
fixed.die$treatment_lab<-"Non-fixed"
fixed.die$treatment2_lab<-"Non-fixed"
fixed.die$session<-fixed.die$session + max(dat$session)
fixed.die$sujeto<-fixed.die$sujeto + max(dat$sujeto)
fixed.die$percevaded <- (fixed.die$profitret - fixed.die$declared)/fixed.die$profitret
fixed.die$highsalary<-NA
fixed.die$shock<-NA
fixed.die$receiveshock<-NA


dat$die_type<-NA
dat$die_type[dat$treatment_lab=="Non-fixed"]<-"Incentivized die"



dat2<-dat[, kv]
fd<-fixed.die[, kv]

df<-rbind(dat2, fd)  ### Complete dataframe will all sessions run in the UK up until Oct 2017
rm(dat2, fd)


### General variables recoded with merged files for cleanness
df$cost_comply <- (df$taxrate/100) * df$profitret  ###Cost of compliance variable for all treatments
pop.median<-median(df$ncorrectret, na.rm=T) ### Population median
df<-ddply(df, c("sujeto"), mutate,
                 mean.ncorrectret=mean(ncorrectret, na.rm=T),
                 perform_high=ifelse(mean.ncorrectret>pop.median, 1 , 0)
)








################################################################################
### Subsets of data, only included treatment sessions and modules with 0% audit
################################################################################
mydf<-df[!is.na(df$treatment_lab), ]  ### Total clean data frame
mydf<-mydf[ mydf$auditrate==0 , ]  ### Only 0% audit, used for the main part of the analysis
write.dta(mydf, "masterfile_Oct2017.dta")

# Non-Fixed treatment data printed for "Duch_Solaz_Multivariate_Laroze.R"
nf<-mydf[mydf$treatment_lab=="Non-fixed",]
write.csv(nf, "additional_data/non_fixed_Oct2017.csv" , row.names=FALSE)

#############
### Figures
#############

####################
# Figure 3: Die Toss
####################

#### incentivised Die
sub.dat <- mydf[!is.na(mydf$die_type), ]
sub.dat$perform_high_lab<-ifelse(sub.dat$perform_high==1, "High Performance", "Low Performance")


### Fixed earning die
plot.data<-as.data.frame(prop.table(table(sub.dat$realdie, sub.dat$perform_high_lab, sub.dat$die_type), c(3,2)))

names(plot.data)[names(plot.data) == "Var1"] <- "die_real"
names(plot.data)[names(plot.data) == "Var2"] <- "performance"
names(plot.data)[names(plot.data) == "Var3"] <- "die_type"


plot.data$die_type<-factor(plot.data$die_type,levels=c( "Incentivized die", "Fixed die"))

ggplot(plot.data, aes(x = die_real, y = Freq)) + geom_bar(stat = "identity") + 
  facet_wrap(~ die_type + performance) + ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,0.8)) +
  xlab("Reported Die Value") + geom_hline(yintercept = 1/6, lty="dashed", col="grey70")

ggsave("die_result_incentive_types.pdf", width = 6, height = 7)


##################################
#Figure 1 - Types of participants
##################################
dat2<-mydf
cdata <- ddply(dat2, c("sujeto", "treatment_lab" ), summarise,
               mean.percevaded = mean(percevaded),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat")

plot.data<-prop.table(table(cdata$treatment_lab, cdata$cheat_pattern), 1)

plot.data<-as.data.frame(plot.data)
names(plot.data)[names(plot.data) == "Var1"] <- "treatment_lab"
names(plot.data)[names(plot.data) == "Var2"] <- "cheat_type"


cdata <- ddply(dat2, c("sujeto"), summarise,
               mean.percevaded = mean(percevaded),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat")

plot.data2<-prop.table(table(cdata$cheat_pattern))
plot.data2<-as.data.frame(plot.data2)
plot.data2$treatment_lab<-"Total"
names(plot.data2)[names(plot.data2) == "Var1"] <- "cheat_type"

plot.data<-rbind(plot.data, plot.data2)

#levels(plot.data$cheat_type) <- c("Always cheat 100%", "Always Cheat", "Sometimes Cheat"  , "Never Cheat" )

plot.data$cheat_type <- factor(plot.data$cheat_type, levels = c("Always declare 0%", "Always Cheat", "Sometimes Cheat" , "Always declare 100%"))


ggplot(plot.data, aes(x = treatment_lab, y = Freq, fill = cheat_type)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) + 
  scale_fill_manual(values = c("grey10", "grey40" ,"grey60", "grey80"),
                    guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10))


ggsave("cheat_type_v4.pdf", width = 6, height = 5)



#######################
# Figure 2 - Bootstrap 
#######################

## values obtained from the boostrap estimations in "Boostrap_replication.do"

treat<-c("All Treatments", "Baseline", "Status (Low)", "Status (High)", "Shock (No)", "Shock (Yes)", "Redistribution", "Non-fixed")
mean.diff<-c(-.2315343,-.2389356, -.3046128, -.2124557, -.1776674, -.1100031,  -.0884571, -.2954328  )
l.ci<-     c(-.3049907,-.4217697, -.5904496, -.4770943, -.432239, -.3087003, -.2351525 , -.4182795 )
u.ci<-     c( -.1580779,-.0561015, -.018776, .0521829, .0769042, .0886941, .0582383 , -.1725861)
n<-c(1:8)

boot.df<-data.frame(treat, mean.diff, l.ci,u.ci)

boot.df$mean.diff<-as.numeric(boot.df$mean.diff)
boot.df$l.ci<-as.numeric(boot.df$l.ci)
boot.df$u.ci<-as.numeric(boot.df$u.ci)

numb<-function(x){return(n)}

p<-ggplot(boot.df, aes(x = treat, y = mean.diff) ) +
   ylab("Difference in Mean Non-declared Earnings")  + xlab("") +
   geom_point(size = 4)  + #ylim(c(-0.7, 0.1)) +
   geom_errorbar(aes(ymax = u.ci, ymin = l.ci)) +
  geom_hline(yintercept=0, col="red") +
  annotate("text", label = "Cl=372", x = 1, y = 0.12) +
  annotate("text", label = "Cl=72", x = 2, y = 0.12) +
  annotate("text", label = "Cl=36", x = 7, y = 0.12) +
  annotate("text", label = "Cl=36", x = 8, y = 0.12) +
  annotate("text", label = "Cl=56", x = 5, y = 0.12) +
  annotate("text", label = "Cl=56", x = 6, y = 0.12) +
  annotate("text", label = "Cl=84", x = 4, y = 0.12) +
  annotate("text", label = "Cl=136", x = 3, y = 0.12) +
  theme( axis.text=element_text(size=10, angle = 45, hjust = 1))
  p

ggsave("bootstrap_diff.pdf", width = 7, height = 5)


################################################
# Figure 4 - Percent evaded per treatment x DG 
################################################
rsk<-mydf
rsk$DG<- NA
rsk$DG[rsk$offerdg==0]<-"No transfer"
rsk$DG[rsk$offerdg>0 & rsk$offerdg<500]<-"Less than half"
rsk$DG[rsk$offerdg>=500]<-"Half or more"

#mean percevaded, over(perform_high risk_averse_level)
#Preparing dataframe for plot
tgc <- summarySE(rsk, measurevar="percevaded", groupvars=c("treatment2_lab", "DG", "perform_high"))
tgc<-tgc[!is.na(tgc$treatment2_lab),]

tgc$perform_high[tgc$perform_high==1]<-"High Performance"
tgc$perform_high[tgc$perform_high==0]<-"Low Performance"

ggplot(tgc, aes(x = treatment2_lab, y = percevaded*100, colour= perform_high)) + 
  geom_errorbar(aes(ymin=(percevaded-se)*100, ymax=(percevaded+se)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3)) + ylab("Percent Evaded") + xlab("") +
  geom_point(position = position_dodge(width=0.3))  + facet_wrap( ~ DG) +
  scale_colour_manual(values = c("grey20", "grey50"), guide = guide_legend(title = "")) +
  theme( legend.position="bottom", axis.text=element_text(size=10, angle = 90, hjust = 1))

ggsave("perevaded_treatment_DG.pdf", width = 10, height = 7)





######################
###Summary Statistics
######################

prop.table(table(mydf$percevaded)) # Proportion of decisions where people declare 0 (evade 1)

#### Treatments and decisions 

# Total

length(unique(mydf$sujeto))
length(unique(mydf$session))
length(mydf$sujeto)


# Per treatment
cdata <- ddply(mydf, c("treatment_lab"), summarise,
               N.Subjects    = length(unique(sujeto)),
               N.Session = length(unique(session)),
               N.Decisions    = length(sujeto)
               
)



#### Mean effort, cheating 
mean(mydf$ncorrectret)
median(mydf$ncorrectret)

ddply(mydf, c("treatment2_lab"), summarise, 
      mean.effort = mean(ncorrectret),
      mean.cheat = mean(cheat)
      )

ddply(mydf, c("treatment_lab"), summarise, 
      mean.effort = mean(ncorrectret),
      mean.cheat = mean(cheat)
)

mean(mydf$cheat)
prop.table(table(mydf$percevaded))


### Table 2 data
ddply(mydf, c("taxrate", "perform_high"), summarise, 
      mean.cost = mean(cost_comply),
      mean.cheat = mean(percevaded)
      
)


### Deviations from performance type
### low performers with > 12 ncorrect ret
d<-nrow(subset(mydf, perform_high==0 & ncorrectret>12))
d
t<-nrow(subset(mydf, perform_high==0))
t
d/t ## proportion of relevant population

### high performers with < 10 ncorrectret
d<-nrow(subset(mydf, perform_high==1 & ncorrectret<10))
d
t<-nrow(subset(mydf, perform_high==1))
t
d/t  ## proportion of relevant population


#### by tax rate 

ddply(mydf, c("taxrate"), summarise, 
      mean.effort = mean(ncorrectret),
      N.Session = length(unique(session)),
      Mean.cheat = mean(cheat)
)


#### Cheater types
cdata <- ddply(mydf, c("sujeto"), summarise,
               mean.percevaded = mean(percevaded),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat"
)
prop.table(table(cdata$cheat_pattern)) 




#### Table 3

t3<-subset(mydf, !is.na(init_pred))
table(t3$session)
table(mydf$init_pred, mydf$perform_high)

prop.table(table(mydf$init_pred, mydf$perform_high), 2)



### Table 4

df

base0<-subset(df, treatment_lab=="Baseline" & auditrate==0)  

base10<-subset(df, treatment_lab=="Non-fixed" & auditrate==10)
base30<-subset(df, auditrate==30)

t.test(base0$percevaded[base0$perform_high==0], base0$percevaded[base0$perform_high==1])
t.test(base10$percevaded[base0$perform_high==0], base10$percevaded[base0$perform_high==1])
t.test(base30$percevaded[base0$perform_high==0], base30$percevaded[base0$perform_high==1])


##### Summary by DG results

ddply(mydf, c("perform_high"), summarise, 
      mean.effort = mean(offerdg)
      
)

cor(mydf$ncorrectret, mydf$offerdg)












##### Identifying extrem behaviour with only 0 and 100% declarations
interval<-c(0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7, 0.8, 0.9) ## It would give means with only 1 digit
cdata <- ddply(dat2, c("sujeto"), summarise,
               mean.percevaded = round(mean(percevaded), 5),
               cheat_0or100= if (mean.percevaded==1) "always cheat" 
               else if (mean.percevaded==0) "never cheat" 
               else if ( mean.percevaded %in%  interval && 1 %in% percevaded) "Extremes" 
               else "Other cheater"
)

#View(dat2[,c("sujeto", "profitret", "declared", "percevaded") ]) #### Checks
#View(subset(cdata, cheat_0or100=="Extremes"))
#View(subset(cdata, mean.percevaded %in% interval))

prop.table(table(cdata$cheat_0or100)) 







#### Treatments and decisions 
cdata <- ddply(dat2, c("treatment_lab", "session", "gender"), summarise,
               N.Gender    =  length(unique(sujeto)),
               Mean.cheat = mean(cheat)
               
)

cdata$gender[cdata$gender==1]<-"Male"
cdata$gender[cdata$gender==0]<-"Female"










########################
#### Appendix figures 
########################



#######################
#### Gender comparison
#######################

dat2<-subset(dat, auditrate==0)
dat2<-dat2[complete.cases(dat2$treatment_lab), ]

cdata <- ddply(dat2, c("gender"), summarise,
               N.Gender    =  length(unique(sujeto)),
               Mean.cheat = mean(cheat),
               mean.percevaded = mean(percevaded)
               
)

cdata$treatment_lab<-"All treatments"

cdata2 <- ddply(dat2, c("treatment_lab", "gender"), summarise,
               N.Gender    =  length(unique(sujeto)),
               Mean.cheat = mean(cheat),
               mean.percevaded = mean(percevaded)
               
)

cdata<-rbind(cdata, cdata2)


cdata$gender[cdata$gender==1]<-"Male"
cdata$gender[cdata$gender==0]<-"Female"



ggplot(cdata, aes(x = treatment_lab, y = mean.percevaded*100, fill=gender)) + 
  geom_bar(position = position_dodge(), stat="identity") +
  scale_fill_manual("", values = c("grey20", "grey70")) +
  ylab("Percent Evadad") + xlab("") + ylim(0, 100)

ggsave("cheat_gender.pdf", width = 7, height = 5)



## Bootstrap gender figure 
## values obtained from the boostrap estimations in "Boostrap_replication.do"

treat<-c("All Treatments", "Baseline", "Status (Low)", "Status (High)", "Shock (No)", "Shock (Yes)", "Redistribution", "Non-fixed")
mean.diff<-c(-.0733926 , .0412275, -.1908748, -.0709805, .0012624, .0068475 , -.0471375,  -.2058502)
l.ci<-     c(-.145715 , -.1354555, -.3807535, -.2980666, -.205235, -.1614298, -.1919343, -.3646495 )
u.ci<-     c( -.0010702, .2179105, -.0009961, .1561057, .2077598, .1751248 , .0976593, -.0470509 )


boot.df<-data.frame(treat, mean.diff, l.ci,u.ci)

boot.df$mean.diff<-as.numeric(boot.df$mean.diff)
boot.df$l.ci<-as.numeric(boot.df$l.ci)
boot.df$u.ci<-as.numeric(boot.df$u.ci)


p<-ggplot(boot.df, aes(x = treat, y = mean.diff) ) +
  ylab("Difference in Mean Non-declared Earnings")  + xlab("") +
  geom_point(size = 4)  + #ylim(c(-0.7, 0.1)) +
  geom_errorbar(aes(ymax = u.ci, ymin = l.ci)) +
  geom_hline(yintercept=0, col="red") +
  annotate("text", label = "Cl=372", x = 1, y = 0.12) +
  annotate("text", label = "Cl=72", x = 2, y = 0.12) +
  annotate("text", label = "Cl=36", x = 7, y = 0.12) +
  annotate("text", label = "Cl=36", x = 8, y = 0.12) +
  annotate("text", label = "Cl=56", x = 5, y = 0.12) +
  annotate("text", label = "Cl=56", x = 6, y = 0.12) +
  annotate("text", label = "Cl=84", x = 4, y = 0.12) +
  annotate("text", label = "Cl=88", x = 3, y = 0.12) +
  theme( axis.text=element_text(size=10, angle = 45, hjust = 1))
p

ggsave("bootstrap_gender_diff.pdf", width = 7, height = 5)














