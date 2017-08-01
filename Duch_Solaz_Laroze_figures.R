
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

setwd("C:/Users/André Laroze/Dropbox/CESS-Santiago/Archive/Tax Compliance Experiments/Rep Material/Why-we-Cheat-2016-master/Replication/Laroze rep package")



#############################
## Data Frames and management
##############################

#################
### Non-fixed Die
##################
dat <- read.dta("MasterfileOxfordChile_20160506_b.dta")
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


#table(dat$treatment, dat$treatment_lab)

#####################
### Fixed die
#####################


die_one <- read.csv("additional_data/subjects_170710_1310.csv")

die_one<-subset(die_one, Treatment!=2 ) ### Eliminating test rounds, where RET==-1 (pushing down mean) and treatment/module==2

## Generating high performance variable
pop.median<-median(die_one$nCorrectRET, na.rm=T)

die_one<-ddply(die_one, c("Subject"), mutate,
                   mean.ncorrectret=mean(nCorrectRET, na.rm=T),
                   perform_high=ifelse(mean.ncorrectret>pop.median, "High Performance" , "Low Performance")
                   )



#############
### Figures
#############

##############
###figure 5 DL
##############
sub.dat <- dat[which(dat$session %in% c(1:4, 15:18, 25:27)), ]
sub.dat$treat_type <- NA
sub.dat$treat_type[sub.dat$session < 4] <- 1
sub.dat$treat_type[sub.dat$highsalary == 0] <- 2
sub.dat$treat_type[sub.dat$highsalary == 1] <- 3
sub.dat$treat_type[sub.dat$session > 24] <- 4

treats <- c("Baseline", "Low Status", "High Status", "Shock")

sub.dat$treat_type_lab <- factor(sub.dat$treat_type, labels = treats)

plot.data <- NULL
for(i in 1:4){
  tmp <- data.frame(prop.table(table(sub.dat$ncorrectret[which(sub.dat$treat_type==i)])))
  tmp$treat <- treats[i]
  plot.data <- rbind(plot.data, tmp)
}

ggplot(plot.data, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + 
  facet_wrap(~treat) + ylab("Percent") + scale_y_continuous(labels = scales::percent) +
  xlab("Number of Correct Additions") + theme(axis.text=element_text(size=6))
ggsave("addition2.pdf", width = 7, height = 5)

########################
# Figure 3: Die Toss DL
########################

#### incentivised Die
sub.dat <- dat[dat$session > 30, ]

plot.data<-as.data.frame(prop.table(table(sub.dat$realdie, sub.dat$perform_high), 2))
names(plot.data)[names(plot.data) == "Var1"] <- "die_real"
names(plot.data)[names(plot.data) == "Var2"] <- "performance"

plot.data$performance<-ifelse(plot.data$performance==1, "High Performance", "Low Performance")
plot.data$treatment<-"Incentivised Die"


### Fixed earning die
plot.data2<-as.data.frame(prop.table(table(die_one$reported_roll_real, die_one$perform_high), 2))

names(plot.data2)[names(plot.data2) == "Var1"] <- "die_real"
names(plot.data2)[names(plot.data2) == "Var2"] <- "performance"
plot.data2$treatment<-"Fixed Die"

### Merge

plot.data<-rbind(plot.data2, plot.data)
rm(plot.data2)


plot.data2<-as.data.frame(prop.table(table(die_one$reported_roll_real))
                                     
plot.data$treatment <- factor(plot.data$treatment, levels = c("Incentivised Die", "Fixed Die"))

ggplot(plot.data, aes(x = die_real, y = Freq)) + geom_bar(stat = "identity") + 
  facet_wrap(~ treatment + performance) + ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,0.8)) +
  xlab("Reported Die Value") 

ggsave("die_result_incentive_types.pdf", width = 6, height = 7)


#########################################################################################
# Figure 7: deviations in performance and cheating for low and high performance types DL
#########################################################################################

sub.dat <- dat[which(dat$session < 28 & dat$auditrate == 0), ]
sub.dat$low_high <- 0
sub.dat$low_high[sub.dat$perform_high == 1] <- 1
sub.dat$low_high[sub.dat$perform_high == 1 & sub.dat$ncorrectret < 10] <- 2
sub.dat$low_high[sub.dat$perform_high == 0] <- 2
sub.dat$low_high[sub.dat$perform_high == 0 & sub.dat$ncorrectret > 12] <- 4

sub.dat <- within(sub.dat, {avg_percevaded = ave(percevaded, sujeto)})
sub.dat$diff_low <- 0
sub.dat$diff_low[sub.dat$low_high == 2] <- sub.dat$percevaded[sub.dat$low_high == 2] - 
  sub.dat$avg_percevaded[sub.dat$low_high == 2]
sub.dat$diff_high <- 0
sub.dat$diff_high[sub.dat$low_high == 4] <- sub.dat$percevaded[sub.dat$low_high == 4] - 
  sub.dat$avg_percevaded[sub.dat$low_high == 4]

sub.dat$diff_high2 <- round(sub.dat$diff_high * 5, 1)/5
ggplot(sub.dat[sub.dat$low_high == 4, ], aes(x = diff_high2)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Average Cheating Minus Cheating When Performance is High") +
  ylab("Density")
ggsave("hist_high2.pdf", width = 5, height = 4)

sub.dat$diff_low2 <- round(sub.dat$diff_low * 2, 1)/2
ggplot(sub.dat[sub.dat$low_high == 2, ], aes(x = diff_low2)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Average Cheating Minus Cheating When Performance is Low") +
  ylab("Density")
ggsave("hist_low2.pdf", width = 5, height = 4)


####################################
#Figure 8 DL - Types of participants
#####################################

ggplot(sub.dat, aes(x = factor(safechoices))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Number of Safe Choices Selected") +
  ylab("Percent") + scale_y_continuous(labels = scales::percent)
ggsave("risk_dist_preliminary.pdf", width = 5, height = 4)

## Figure 1 DL
# (a)
sub.dat <- dat[which(dat$session < 28 & dat$auditrate == 0), ]
sub.dat$percevaded2 <- round(sub.dat$percevaded, 1) * 100
ggplot(sub.dat, aes(x = percevaded2)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Percent Evaded") +
  ylab("Percent") + scale_y_continuous(labels = scales::percent)
ggsave("undeclared2.pdf", width = 5, height = 5)


# (b)
dat2<-subset(dat, auditrate==0)
dat2<-dat2[complete.cases(dat2$treatment_lab), ]
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



############################################
## Appendix Figure, Figure 1 with HighTax DL
############################################


# (a)
sub.dat <- dat[which(dat$session < 28 & dat$auditrate == 0 & dat$HighTax==1), ]
sub.dat$percevaded2 <- round(sub.dat$percevaded, 1) * 100
ggplot(sub.dat, aes(x = percevaded2)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Percent Evaded") +
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) 
ggsave("undeclared2_hightax.pdf", width = 5, height = 5)


# (b)
sub.dat <- dat[!is.na(dat$treatment) & dat$auditrate == 0 & dat$HighTax==1, ]
sub.dat <- within(sub.dat, {avg_percevaded = ave(percevaded > 0, sujeto)})
sub.dat <- sub.dat[!duplicated(sub.dat$sujeto), ]
sub.dat$cheat_pattern <- ifelse(sub.dat$avg_percevaded == 0, 1,
                                ifelse(sub.dat$avg_percevaded == 1, 3, 2))
table(sub.dat$cheat_pattern)
plot.data <- NULL
for(i in 1:5){
  tmp <- data.frame(prop.table(table(sub.dat$cheat_pattern[which(sub.dat$treatment==i)])))
  tmp$treat <- i
  plot.data <- rbind(plot.data, tmp)
}
tmp <- data.frame(prop.table(table(sub.dat$cheat_pattern)))
tmp$treat <- 6
plot.data <- rbind(plot.data, tmp)
plot.data$treat_lab <- factor(plot.data$treat, labels = c("Baseline",
                                                          "Status",
                                                          "Shock",
                                                          "Redistribution",
                                                          "Non-fixed",
                                                          "Total"))
levels(plot.data$Var1) <- c("Never Cheat", "Sometimes Cheat", "Always Cheat")
ggplot(plot.data, aes(x = treat_lab, y = Freq, fill = Var1)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("grey20", "grey50", "grey80"),
                    guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10))
ggsave("cheat_type2_hightax.pdf", width = 6, height = 5)


##############################################
## Appendix Figure, Figure 1 with HighTax DL
################################################

# (a)
sub.dat <- dat[which(dat$session < 28 & dat$auditrate == 0 & dat$HighTax==0), ]
sub.dat$percevaded2 <- round(sub.dat$percevaded, 1) * 100
ggplot(sub.dat, aes(x = percevaded2)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  xlab("Percent Evaded") +
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) 
ggsave("undeclared2_lowtax.pdf", width = 5, height = 5)


# (b)
sub.dat <- dat[!is.na(dat$treatment) & dat$auditrate == 0 & dat$HighTax==0, ]
sub.dat <- within(sub.dat, {avg_percevaded = ave(percevaded > 0, sujeto)})
sub.dat <- sub.dat[!duplicated(sub.dat$sujeto), ]
sub.dat$cheat_pattern <- ifelse(sub.dat$avg_percevaded == 0, 1,
                                ifelse(sub.dat$avg_percevaded == 1, 3, 2))
table(sub.dat$cheat_pattern)
plot.data <- NULL
for(i in 1:5){
  tmp <- data.frame(prop.table(table(sub.dat$cheat_pattern[which(sub.dat$treatment==i)])))
  tmp$treat <- i
  plot.data <- rbind(plot.data, tmp)
}
tmp <- data.frame(prop.table(table(sub.dat$cheat_pattern)))
tmp$treat <- 6
plot.data <- rbind(plot.data, tmp)
plot.data$treat_lab <- factor(plot.data$treat, labels = c("Baseline",
                                                          "Status",
                                                          "Shock",
                                                          "Redistribution",
                                                          "Non-fixed",
                                                          "Total"))
levels(plot.data$Var1) <- c("Never Cheat", "Sometimes Cheat", "Always Cheat")
ggplot(plot.data, aes(x = treat_lab, y = Freq, fill = Var1)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values = c("grey20", "grey50", "grey80"),
                    guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10))
ggsave("cheat_type2_lowtax.pdf", width = 6, height = 5)



# Figure Not used in manuscript
plot.data <- NULL

for(i in 1:7){
  c.dat <- sub.dat[sub.dat$treatment2 == i, ]
  tmp <- data.frame(tapply(c.dat$percevaded, c.dat$high_perform, mean),
                    tapply(c.dat$percevaded, c.dat$high_perform, function(x) sd(x)/length(x)))
  tmp$treat <- i
  plot.data <- rbind(plot.data, tmp)
}
names(plot.data) <- c("Freq", "sd", "treat")
plot.data$ul <- plot.data$Freq + 1.96 * plot.data$sd
plot.data$ll <- plot.data$Freq - 1.96 * plot.data$sd

plot.data$treat_lab <- factor(plot.data$treat, labels = c("Baseline",
                                                          "Status (Low)",
                                                          "Status (High)",
                                                          "Shock (No)",
                                                          "Shock (Yes)",
                                                          "Redistribution",
                                                          "Non-fixed"))
plot.data$performance <- 1:2
plot.data$performance <- factor(plot.data$performance, labels = c("Low Performance", "High Performance"))
dodge <- position_dodge(width=0.9)
ggplot(plot.data, aes(x = treat_lab, y = Freq, fill = performance)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent) + 
  geom_errorbar(aes(max = ul, min = ll), position = dodge, width=0.25) + 
  scale_fill_manual(values = c("grey20", "grey50"),
                    guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10))
ggsave("treatment2.pdf", width = 7, height = 5)

#########################
## Bootstrap figure 2 DL
#########################

## values obtained from the boostrap estimations in "Boostrap_replication.do"

treat<-c("All Treatments", "Baseline", "Status (Low)", "Status (High)", "Shock (No)", "Shock (Yes)", "Redistribution", "Non-fixed")
mean.diff<-c(-.2363362,-.2389356, -.3046128, -.2124557, -.1776674, -.1100031,  -.0884571, -.3629419  )
l.ci<-     c(-.3108202,-.4217697, -.5904496, -.4770943, -.432239, -.3087003, -.2351525 , -.5070303 )
u.ci<-     c( -.1618522,-.0561015, -.018776, .0521829, .0769042, .0886941, .0582383 , -.2188535)
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
  annotate("text", label = "Cl=88", x = 3, y = 0.12) +
  theme( axis.text=element_text(size=10, angle = 45, hjust = 1))
  p

ggsave("bootstrap_diff.pdf", width = 7, height = 5)

#####################################################
#### Figure 9 - Percent evaded per treatment summary
######################################################

#Risk aversion/seeking/neutrality
rsk<-dat[dat$auditrate==0, ]
rsk<-rsk[!is.na(rsk$treatment2_lab),]

rsk$risk<- NA
rsk$risk[rsk$safechoice<=3]<-"Risk Taking"
rsk$risk[rsk$safechoices>=4 & rsk$safechoices<=6]<-"Risk Neutral"
rsk$risk[rsk$safechoices>=7]<-"Risk Averse"

#mean percevaded, over(perform_high risk_averse_level)
#Preparing dataframe for plot
tgc <- summarySE(rsk, measurevar="percevaded", groupvars=c("treatment2_lab", "risk", "perform_high"))
tgc<-tgc[!is.na(tgc$treatment2_lab),]

tgc$perform_high[tgc$perform_high==1]<-"High Performance"
tgc$perform_high[tgc$perform_high==0]<-"Low Performance"

ggplot(tgc, aes(x = treatment2_lab, y = percevaded*100, colour= perform_high)) + 
  geom_errorbar(aes(ymin=(percevaded-se)*100, ymax=(percevaded+se)*100), width=.1, position = position_dodge(width=0.3)) +
  geom_line(position = position_dodge(width=0.3)) + ylab("Percent Evaded") + xlab("") +
  geom_point(position = position_dodge(width=0.3))  + facet_wrap( ~ risk) +
  scale_colour_manual(values = c("grey20", "grey50"), guide = guide_legend(title = "")) +
  theme( legend.position="bottom", axis.text=element_text(size=10, angle = 90, hjust = 1))

ggsave("perevaded_treatment.pdf", width = 10, height = 7)



#####################################################
#### Figure 9 - Percent evaded per treatment x DG 
######################################################

#Risk aversion/seeking/neutrality
rsk<-dat[dat$auditrate==0, ]
rsk<-rsk[!is.na(rsk$treatment2_lab),]

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




###########################
### Summary Statistics
##########################
dat2<-subset(dat, auditrate==0)
dat2<-dat2[complete.cases(dat2$treatment_lab), ]


#### Treatments and decisions 
cdata <- ddply(dat2, c("treatment_lab"), summarise,
               N.Subjects    = length(unique(sujeto)),
               N.Session = length(unique(session)),
               N.Decisions    = length(sujeto)
               
)

#### Cheater types
cdata <- ddply(dat2, c("sujeto"), summarise,
               mean.percevaded = mean(percevaded),
               cheat_pattern = if (mean.percevaded==1) "Always declare 0%" 
               else if (mean.percevaded==0) "Always declare 100%" 
               else if (0 %in% percevaded) "Sometimes Cheat" else "Always Cheat"
)
prop.table(table(cdata$cheat_pattern)) 


 
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





