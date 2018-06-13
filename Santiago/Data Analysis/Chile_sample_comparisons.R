

################################################################
## Paper: Is Cheating a national pastime? Experimental evidence
## Author code: Denise Laroze
## Year: 2017
################################################################



library(foreign)
library(ggplot2)
library(readstata13)
library(RColorBrewer)
library(rms)
theme_set(theme_bw())
library(plyr)
library(stargazer)
library(gridExtra)
library(clusterSEs)
library(car)



rm(list=ls())

setwd("C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Tax Compliance Moscow/Raw Data")
fig.path <- "Figures"
v<-"Feb2018"


dat <- read.dta13("mastern_final22.dta")
online.chile<-read.csv("CESS_Panel_DS_Stgo_2017.csv")


#names(rus)<- tolower(names(rus))

###################
### Data Management
###################

### To include lab in field sessions from Chile UDD Oct 2017

udd.sessions<-c(52:55)
dat$country[dat$session %in% udd.sessions]<-"Chile"
dat$UDD<-NA
dat$UDD[dat$country=="Chile"]<-"No"
dat$UDD[dat$session %in% udd.sessions]<-"Yes"
dat$treatment[dat$session %in% c(52:55)]<-4 


### Treatment labels
dat$treatment_lab <- NA
dat$treatment_lab[dat$treatment==1] <- "Baseline"
dat$treatment_lab[dat$treatment==2] <- "Status"
dat$treatment_lab[dat$treatment==3] <- "Shock"
dat$treatment_lab[dat$treatment==4] <- "Non-fixed"

online.chile$treatment_lab <- "Baseline"


dat$treatment2_lab <- NA
dat$treatment2_lab[dat$treatment==1] <- "Baseline"
dat$treatment2_lab[dat$treatment==2 & dat$status_L==1] <- "Status (Low)"
dat$treatment2_lab[dat$treatment==2 & dat$status_H==1] <- "Status (High)"
dat$treatment2_lab[dat$treatment==3] <- "Shock (No)"
dat$treatment2_lab[dat$treatment==3 & dat$shock_H==1] <- "Shock (Yes)"
dat$treatment2_lab[dat$treatment==4] <- "Non-fixed"

online.chile$treatment2_lab <- "Baseline"

#table(dat$country, dat$treatment)
#table(dat$country, dat$treatment_lab)
#table(dat$country, dat$treatment2_lab, dat$session)


#### Preparing data to merge 
dat<-dat[dat$auditrate==0, ] ### Only 0% audit, for comparability
dat<-subset(dat,  country=="Chile")
dat<-subset(dat, treatment_lab %in% c("Baseline", "Non-fixed"))
dat$gender<-dat$male

#Online data management
online.chile$age2<-online.chile$age
online.chile$age2[online.chile$age=="false"]<-NA
online.chile$age<-as.numeric(levels(online.chile$age2))[online.chile$age2]
online.chile$subj_id<-online.chile$muID
online.chile$sample<-"Online Chile"

names(online.chile)[names(online.chile)=="taxRate"] <- "taxrate"
names(online.chile)[names(online.chile)=="correct"] <- "ncorrectret"


dat$sample<-"CESS Lab"
dat$sample[dat$UDD=="Yes"]<-"UDD"
dat$report.rate <- dat$declared/dat$profitret

names(dat)[names(dat)=="age_subject"] <- "age"
names(dat)[names(dat)=="offerdg"] <- "DictGive"
names(dat)[names(dat)=="safechoices"] <- "risk.pref"
names(dat)[names(dat)=="profitret"] <- "prelimGain"





vars<-c( "subj_id", "ncorrectret" ,"gender", "age", "DictGive" , "sample",
         "risk.pref", "prelimGain" , "taxrate" , "report.rate", "treatment_lab" )

df<-dat[, vars]

df.o<-online.chile[, vars]

mydf<-rbind(df, df.o)

rm(df, df.o)


##### Performance calculation
#dat$subj_id<-paste0(dat$session, dat$subject)

pop.median.usach<-median(mydf$ncorrectret[mydf$sample=="CESS Lab"], na.rm=T) ### Population median
pop.median.online<-median(mydf$ncorrectret[mydf$sample=="Online Chile"], na.rm=T) ### Population median
pop.median.udd<-median(mydf$ncorrectret[mydf$sample=="UDD"], na.rm=T) ### Population median


mydf<-ddply(mydf, c("sample", "subj_id"), mutate,
          mean.ncorrectret=mean(ncorrectret, na.rm=T),
          perform_high=ifelse(sample=="CESS Lab" & mean.ncorrectret>pop.median.usach, 1 , 
                              ifelse(sample=="Online Chile" & mean.ncorrectret>pop.median.online, 1 ,
                                     ifelse(sample=="UDD" & mean.ncorrectret>pop.median.udd, 1, 0)))
)


mydf$percevaded<-1-mydf$report.rate


###########
##########################
### Figures
############################
###########

####################################
#### Percent Evaded by Performance
####################################
library(Rmisc)
tgc<- summarySE(mydf, measurevar="percevaded", groupvars=c("perform_high", "sample"), na.rm=T)
#tgc<-tgc[!is.na(tgc$treatment_lab),]

tgc$perform_high[tgc$perform_high==1]<-"High Performance"
tgc$perform_high[tgc$perform_high==0]<-"Low Performance"

tgc<-tgc[!is.na(tgc$perform_high),]


#tgc$treatment2_lab<-factor(tgc$treatment2_lab, levels = c("Baseline", "Shock (No)", "Shock (Yes)", "Non-fixed", "Status (High)", "Status (Low)" ))
ggplot(tgc, aes(x = perform_high, y = percevaded*100, colour= sample)) + 
  geom_errorbar(aes(ymin=(percevaded-ci)*100, ymax=(percevaded+ci)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3), size = 1.5) + ylab("Percent Evaded") + xlab("") +
  geom_point(position = position_dodge(width=0.3))  + #facet_wrap( ~ treatment_lab ) +
  scale_colour_brewer(palette="Set1", guide = guide_legend(title = "")) +
  theme( legend.position="bottom", axis.text=element_text(size=10))

ggsave(paste0("perevaded_performance_Chile_Sample_", v, ".pdf"), path= fig.path, width = 7, height = 7)


#############################
#### Cheater types
############################

cdata <- ddply(mydf, c("subj_id", "sample" ), summarise,
               mean.report.rate = mean(report.rate, na.rm=T),
               cheat_pattern = if (mean.report.rate=="0") "Always declare 0%" 
               else if (mean.report.rate=="1") "Always declare 100%" 
               else if ("1" %in% report.rate) "Sometimes Cheat" else "Always Cheat")

plot.data2<-prop.table(table(cdata$sample, cdata$cheat_pattern), 1)

plot.data2<-as.data.frame(plot.data2)
names(plot.data2)[names(plot.data2) == "Var1"] <- "Sample"
names(plot.data2)[names(plot.data2) == "Var2"] <- "cheat_type"


ggplot(plot.data2, aes(x = Sample, y = Freq, fill = cheat_type)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_fill_brewer(palette="Set1", guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10, angle = 45, hjust = 1) )


ggsave(paste0("comparative_type_cheaters_Chile_Samples", v, ".pdf"), path=fig.path,  width = 10, height = 6)








################
### Descriptives
################

plot.df<-mydf[,c("gender", "sample")]
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)
#plot.df$Gender2[plot.df$Gender==1]<-"Male"
#plot.df$Gender2[plot.df$Gender==2]<-"Female"
plot.df$gender[plot.df$gender=="M"]<-"Male"
plot.df$gender[plot.df$gender=="F"]<-"Female"

ggplot(plot.df, aes(x = gender, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("") + 
  scale_fill_brewer(palette="Set1", guide = guide_legend(title = "")) + 
  ylim(0, 100) + ylab("Percent")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 


ggsave(paste0("comparative_gender_Chile_samples", v, ".pdf"), path=fig.path,  width = 7, height = 4)



##########################
### Hist amount given DG
##########################

plot.df<-mydf[, c("DictGive", "sample")]
#plot.df$DictGive[plot.df$DictGive==-1]<-NA
plot.df$DictGive<-ifelse(plot.df$DictGive==0, "0", 
                         ifelse(plot.df$DictGive>0 & plot.df$DictGive<500, ">0 & <500", 
                                ifelse(plot.df$DictGive==500, "500",  ">500")))
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)
plot.df$DictGive <- factor(plot.df$DictGive, levels = c("0", ">0 & <500", "500",  ">500" ))

ggplot(plot.df, aes(x = DictGive, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Offers in Dictator Game") + 
  scale_fill_brewer(palette="Set1", guide = guide_legend(title = "")) + 
  ylim(0, 100) + ylab("Percent")+  theme(legend.position="bottom") 

ggsave(paste0("comparative_offers_DG_Chile_samples", v, ".pdf"), path=fig.path,  width = 8, height = 5)



######################
### Density age
######################

ggplot(mydf, aes(x=age)) + geom_density(aes(group=sample, fill=sample) , alpha=0.9)+
  scale_fill_brewer(palette="Set1", guide = guide_legend(title = "")) + 
  xlab("Age") + ylab("Density") +  theme(legend.position="bottom")

ggsave(paste0("comparative_density_age_Chile_samples", v, ".pdf"), path=fig.path,  width = 7, height = 4)



##########################
### Hist Risk Preferences
##########################
plot.df<-mydf[, c("risk.pref", "sample")]
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)

ggplot(plot.df, aes(x = risk.pref, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Risk Preferences") + 
  scale_fill_brewer(palette="Set1", guide = guide_legend(title = "")) + 
  ylim(0, 40) + ylab("Percent")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(paste0("comparative_hist_safe_choices_Chile_samples", v, ".pdf"), path=fig.path,  width = 7, height = 7)



### T-test differences
plot.df<-mydf[, c("risk.pref", "sample")]
pairwise.t.test(plot.df$risk.pref, plot.df$sample, p.adjust.method = p.adjust.methods,
                paired = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(plot.df$risk.pref, plot.df$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)




#########################
### Density Prelim Gains
#########################

ggplot(mydf, aes(x=prelimGain)) + geom_density(aes(group=sample, fill=sample), alpha=0.7)+
  scale_fill_brewer(palette="Set1", guide = guide_legend(title = "")) +  
  xlab("Preliminary Gains") + ylab("Density")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(paste0("comparative_density_prelimGain_Chile_samples", v, ".pdf"), path=fig.path,  width = 7, height = 4)


pairwise.t.test(p.data$prelimGain, p.data$sample, p.adjust.method = p.adjust.methods,
                pool.sd = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$prelimGain, plot.df$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)











###############
#### Die Toss
###############

# Die Toss UK


pt<-prop.table(table(dat.2$realdie, dat.2$perform_high, dat.2$country), c(3,2))

die<-as.data.frame(pt)

names(die)<-c("die", "perform_high" ,"country", "prop" )
die$perform_high_lab<-ifelse(die$perform_high==1, "High Performance", "Low Performance")
die$country <- factor(die$country, levels = c("Russia", "UK", "Chile"))



d<-ggplot(die, aes(x = die, y = prop, colour=country, fill=country)) + geom_bar(stat = "identity") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent , limits = c(0,0.7)) +
  xlab("") + labs(colour="", fill="") +
  geom_hline(yintercept = 1/6, lty="dashed", col="red")+
  facet_wrap(~  country + perform_high_lab, ncol = 2) +  theme(legend.position="bottom")
d
ggsave(paste0("comparative_die",v, ".pdf"), path=fig.path, width = 5, height = 6)


#################
#### Gender plot
#################

plot.dat<-dat.2[complete.cases(dat.2$male), ]


cdata <- ddply(plot.dat, c("male", "country"), summarise,
               N.Gender    =  length(unique(subj_id)),
               Mean.cheat = mean(cheat),
               mean.percevaded = mean(percevaded, na.rm=T)
               
)

cdata$treatment_lab<-"All treatments"

cdata2 <- ddply(plot.dat, c("treatment_lab", "male", "country"), summarise,
                N.Gender    =  length(unique(subj_id)),
                Mean.cheat = mean(cheat),
                mean.percevaded = mean(percevaded, na.rm=T)
                
)

cdata<-rbind(cdata, cdata2)
#cdata<-cdata[cdata$treatment_lab!="Redistribution",]


ggplot(cdata, aes(x = treatment_lab, y = mean.percevaded*100, fill=male)) + 
  geom_bar(position = position_dodge(), stat="identity") +
  scale_fill_manual("", values = c("red", "blue")) +
  ylab("Percent Evadad") + xlab("") + ylim(0, 100)


ggsave(paste0("cheat_gender", v, ".pdf"), path=fig.path,  width = 7, height = 5)


###########################
### Denisty N correct Sums
###########################

ggplot(dat.2, aes(x=ncorrectret)) + geom_density(aes(group=country, fill=country) , alpha=0.5)+
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515")) + 
  xlab("Number Correct Sums") + ylab("Density") 
ggsave(paste0("comparative_performance_density_overlap", v, ".pdf"), path=fig.path,  width = 7, height = 4)


######################
### Density age
######################

ggplot(dat.2, aes(x=age_subject)) + geom_density(aes(group=country, fill=country) , alpha=0.5)+
  scale_fill_manual("", values=c( "#F23E08" , "#0833F2" ,"#1AA515")) + 
  xlab("Age") + ylab("Density")
ggsave(paste0("comparative_density_age", v, ".pdf"), path=fig.path,  width = 7, height = 4)




#################
#### CESS vs UDD
#################


####################################
#### Percent Evaded by Performance
####################################
library(Rmisc)

tgc<- summarySE(cl[cl$treatment_lab=="Non-fixed" ,], measurevar="percevaded", groupvars=c("treatment2_lab", "perform_high", "UDD"), na.rm=T)
tgc<-tgc[!is.na(tgc$treatment2_lab),]

tgc$perform_high[tgc$perform_high==1]<-"High Performance"
tgc$perform_high[tgc$perform_high==0]<-"Low Performance"

tgc$treatment2_lab<-factor(tgc$treatment2_lab, levels = c("Baseline", "Shock (No)", "Shock (Yes)", "Non-fixed", "Status (High)", "Status (Low)" ))
ggplot(tgc, aes(x = perform_high, y = percevaded*100, colour= UDD)) + 
  geom_errorbar(aes(ymin=(percevaded-ci)*100, ymax=(percevaded+ci)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3), size = 1.5) + ylab("Percent Evaded") + xlab("") +
  geom_point(position = position_dodge(width=0.3))  + facet_wrap( ~ treatment2_lab ) +
  scale_color_brewer(palette="Set1", guide = guide_legend(title = "UDD Student")) +
  theme( legend.position="bottom", axis.text=element_text(size=10, angle = 45, hjust = 1))

ggsave(paste0("perevaded_performance_UDD", v, ".pdf"), path= fig.path, width = 7, height = 7)


length(unique(cl$subj_id[cl$UDD=="Yes"]))


###########
# Die Toss 
###########

pt<-prop.table(table(cl$realdie, cl$perform_high, cl$UDD), c(3,2))

die<-as.data.frame(pt)

names(die)<-c("die", "perform_high" ,"UDD", "prop" )
die$perform_high_lab<-ifelse(die$perform_high==1, "High Performance", "Low Performance")
#die$country <- factor(die$UDD, levels = c("YES", "UK", "Chile"))



d<-ggplot(die, aes(x = die, y = prop, colour=UDD, fill=UDD)) + geom_bar(stat = "identity") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent , limits = c(0,0.7)) +
  xlab("") + labs(colour="UDD Student", fill="UDD Student") +
  geom_hline(yintercept = 1/6, lty="dashed", col="red")+
  facet_wrap(~  UDD + perform_high_lab, ncol = 2) +  theme(legend.position="bottom")
d
ggsave(paste0("comparative_die_UDD",v, ".pdf"), path=fig.path, width = 5, height = 6)









