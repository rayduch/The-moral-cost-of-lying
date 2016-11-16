
library(foreign)
library(ggplot2)
theme_set(theme_bw())
library(reshape)
library(scales)
library(Rmisc)
library(plyr)


rm(list=ls())

setwd("C:/Users/André Laroze/Dropbox/CESS-Santiago/Denise/Cheating/Why-we-Cheat-2016-master/Replication/Laroze rep package")
#fig.path <- "Laroze rep package"



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

#table(dat$treatment, dat$treatment_lab)
# figure 5 DL
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


# Figure 3: Die Toss DL
sub.dat <- dat[dat$session > 30, ]


plot.data <- NULL
for(i in 0:1){
  tmp <- data.frame(prop.table(table(sub.dat$realdie[which(sub.dat$perform_high == i)])))
  tmp$perfomance <- i
  plot.data <- rbind(plot.data, tmp)
}
plot.data$perfomance[plot.data$perfomance==1]<-"High Performance"
plot.data$perfomance[plot.data$perfomance==0]<-"Low Performance"

ggplot(plot.data, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + 
  facet_wrap(~perfomance) + ylab("Percent") + scale_y_continuous(labels = scales::percent) +
  xlab("Reported Die Value") 
ggsave("die_result2.pdf", width = 7, height = 4)

# Figure 7: deviations in performance and cheating for low and high performance types DL

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


#Figure 8 DL
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
sub.dat <- dat[!is.na(dat$treatment) & dat$auditrate == 0, ]
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
ggsave("cheat_type2.pdf", width = 5, height = 5)

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


## Bootstrap figure 2 DL
## values obtained from the boostrap estimations in "Boostrap_replication.do"

treat<-c("Baseline", "Status (Low)", "Status (High)", "Shock (No)", "Shock (Yes)", "Redistribution", "Non-fixed")
mean.diff<-c(-.2389356, -.3046128, -.2124557, -.1776674, -.1100031,  -.0884571, -.3629419  )
l.ci<-     c(-.4217697, -.5904496, -.4770943, -.432239, -.3087003, -.2351525 , -.5070303 )
u.ci<-     c(-.0561015, -.018776, .0521829, .0769042, .0886941, .0582383 , -.2188535)
boot.df<-data.frame(treat, mean.diff, l.ci,u.ci)

boot.df$mean.diff<-as.numeric(boot.df$mean.diff)
boot.df$l.ci<-as.numeric(boot.df$l.ci)
boot.df$u.ci<-as.numeric(boot.df$u.ci)


p<-ggplot(boot.df, aes(x = treat, y = mean.diff) ) +
   ylab("Difference in Mean Non-declared Earnings")  + xlab("Treatments") +
   geom_point(size = 4)  + #ylim(c(-0.7, 0.1)) +
   geom_errorbar(aes(ymax = u.ci, ymin = l.ci)) +
  geom_hline(yintercept=0, col="red")
ggsave("bootstrap_diff.pdf", width = 7, height = 5)


#### Figure 9 - Percent evaded per treatment summary

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






