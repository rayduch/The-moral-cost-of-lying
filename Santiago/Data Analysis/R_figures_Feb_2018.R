

################################################################
## Paper: Is Cheating a national pastime? Experimental evidence
## Author code: Denise Laroze
## Year: 2018
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

setwd("C:/Users/Denise Laroze P/Dropbox/CESS-Santiago/Archive/Tax Compliance Moscow/Raw Data")
fig.path <- "Figures"
v<-"March2018"

dat <- read.dta13("mastern_final2018.dta")
subject<-read.dta13("subjects_2018.dta")
#online.chile<-read.csv("CESS_Panel_DS_Stgo_2017.csv")
#names(rus)<- tolower(names(rus))


##########################
### Figures
############################
###########

###########################
### Die by type Figure 1
###########################

subject<-subject[ subject$include_data==1,]


subject$ind_typenew3[subject$ind_typenew2==1] <- "Consistent Maximal"
subject$ind_typenew3[subject$ind_typenew2==2] <- "Consistent Partial"
subject$ind_typenew3[subject$ind_typenew2==3] <- "Consistent Honest"
subject$ind_typenew3[subject$ind_typenew2==4] <- "Other"


pt<-prop.table(table(subject$realdie, subject$ind_typenew3), 2)
pt<-as.data.frame(pt)

names(pt)<-c("die", "type", "prop" )
#???die$perform_high_lab<-ifelse(die$perform_high==1, "High Performance", "Low Performance")
pt$type <- factor(pt$type, levels = c("Consistent Maximal", "Consistent Partial", "Consistent Honest", "Other"))



d<-ggplot(pt, aes(x = die, y = prop, colour=type, fill=type)) + geom_bar(stat = "identity" ,position="dodge") + 
  ylab("Fraction") + scale_y_continuous( limits = c(0,1)) +
  xlab("") + labs(colour="", fill="") +
  geom_hline(yintercept = 1/6, lty="dashed", col="red")+
  scale_fill_manual(values= c("red", "darkgreen", "blue", "grey60"), guide = guide_legend(title = "")) +
  theme(legend.position="bottom")
d
ggsave(paste0("die_types",v, ".pdf"), path=fig.path, width = 9, height = 6)


#######################################
### Histogram of declarations Figure 2
######################################

p.df<-dat[dat$ind_typenew2==2,]
p.df<-p.df[p.df$include_data==1,]


p.df<-p.df[!is.na(p.df$country_code), ]



p.df<-cdata[cdata$ind_typenew2==2,]
p.df$country_code2[p.df$country_code==1] <- "Chile"
p.df$country_code2[p.df$country_code==2] <- "Russia"
p.df$country_code2[p.df$country_code==3] <- "U.K."


ggplot(p.df, aes(declared_part_av)) + 
  geom_histogram( aes(y=..density..), col="grey50", fill="grey60", bins=30)+
  scale_x_continuous(breaks=c(0, .1, .2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))+
  facet_wrap(~ country_code2)+
  xlab("") + theme(strip.text = element_text(size=15))
    
ggsave(paste0("hist_declared",v, ".pdf"), path=fig.path, width = 12, height = 6)


####################
### Reaction times
####################

p.df<-dat[, c("time_declare", "include_data", "declared_cat", "country_code"  )]
p.df$country[p.df$country_code==1] <- "Chile"
p.df$country[p.df$country_code==2] <- "Russia"
p.df$country[p.df$country_code==3] <- "U.K."


p.df$time_declare[p.df$include_data==0]<-NA
p.df<-p.df[complete.cases(p.df$time_declare),]

set.seed(23356)
p.df$time_declare_1<-p.df$time_declare+runif(nrow(p.df))


#### RT cummulative distribution plot

setEPS()
postscript("Figures/RT_ECDF.eps", width = 11, height = 7)


par(pty='m')
plot(ecdf(p.df$time_declare_1[p.df$declared_cat==1]),
     xlim=c(0,30),
     ylim=c(0,1),
     xlab="Seconds",
     ylab="",
     main="",
     col="black")

lines(ecdf(p.df$time_declare_1[p.df$declared_cat==2]),
      #xlim=c(1500,8500),
      col="red")

lines(ecdf(p.df$time_declare_1[p.df$declared_cat==3]),
      #xlim=c(1500,8500),
      col="blue")
legend('bottomright', 
       legend=c("Maximal Cheating","Partial Cheating","Honest"),  # text in the legend
       col=c("black","red","blue"),  # point colors
       pch=15
)

dev.off()


#### RT ecdf By country

p.dfc<-subset(p.df, country=="Chile")
p.dfu<-subset(p.df, country=="U.K.")
p.dfr<-subset(p.df, country=="Russia")

setEPS()
postscript("Figures/RT_ECDF_countries.eps", width = 11, height = 7)


par(mfrow=c(1,3))

# Chile
plot(ecdf(p.dfc$time_declare_1[p.dfc$declared_cat==1]),
     xlim=c(0,30),
     ylim=c(0,1),
     xlab="Seconds",
     ylab="",
     main="Chile",
     col="black")

lines(ecdf(p.dfc$time_declare_1[p.dfc$declared_cat==2]),
      #xlim=c(1500,8500),
      col="red")

lines(ecdf(p.dfc$time_declare_1[p.dfc$declared_cat==3]),
      #xlim=c(1500,8500),
      col="blue")



# UK
plot(ecdf(p.dfu$time_declare_1[p.dfu$declared_cat==1]),
     xlim=c(0,30),
     ylim=c(0,1),
     xlab="Seconds",
     ylab="",
     main="U.K.",
     col="black")

lines(ecdf(p.dfu$time_declare_1[p.dfu$declared_cat==2]),
      #xlim=c(1500,8500),
      col="red")

lines(ecdf(p.dfu$time_declare_1[p.dfu$declared_cat==3]),
      #xlim=c(1500,8500),
      col="blue")



# Russia
plot(ecdf(p.dfr$time_declare_1[p.dfr$declared_cat==1]),
     xlim=c(0,30),
     ylim=c(0,1),
     xlab="Seconds",
     ylab="",
     main="Russia",
     col="black")

lines(ecdf(p.dfr$time_declare_1[p.dfr$declared_cat==2]),
      #xlim=c(1500,8500),
      col="red")

lines(ecdf(p.dfr$time_declare_1[p.dfr$declared_cat==3]),
      #xlim=c(1500,8500),
      col="blue")
legend('bottomright', 
       legend=c("Maximal Cheating","Partial Cheating","Honest"),  # text in the legend
       col=c("black","red","blue"),  # point colors
       pch=15
)

dev.off()

############################
### Cheater type
######################.
p.df<-dat[, c("country_code", "ind_typenew2", "hightype", "include_data")]
p.df$country[p.df$country_code==1] <- "Chile"
p.df$country[p.df$country_code==2] <- "Russia"
p.df$country[p.df$country_code==3] <- "U.K."


p.df$ind_typenew2[p.df$include_data==0]<-NA
p.df<-p.df[complete.cases(p.df$ind_typenew2),]


pt<-prop.table(table(p.df$ind_typenew2, p.df$hightype, p.df$country), c(3,2))

prop.t<-as.data.frame(pt)

names(prop.t)<-c("ind_typenew2", "perform_high" ,"country", "prop" )
prop.t$perform_high_lab<-ifelse(prop.t$perform_high==1, "High Performance", "Low Performance")
prop.t$ind_typenew2_lab<-ifelse(prop.t$ind_typenew2==1, "C. Maximal", 
                                ifelse(prop.t$ind_typenew2==2, "C. Partial",
                                       ifelse(prop.t$ind_typenew2==3, "C. Honest", "Other")
                                )
)


prop.t$ind_typenew2_lab <- factor(prop.t$ind_typenew2_lab, levels = c("C. Maximal", "C. Partial", "C. Honest", "Other"))



d<-ggplot(prop.t, aes(x = ind_typenew2_lab, y = prop, colour=perform_high_lab, fill=perform_high_lab)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent , limits = c(0,0.7)) +
  xlab("") + labs(colour="", fill="") +
  theme(text = element_text(size=20))+
  scale_fill_manual("", values = c("red", "blue"))+
  #geom_hline(yintercept = 1/6, lty="dashed", col="red")+
  facet_wrap(~  country, ncol = 3) +
  theme(legend.position="bottom", legend.text=element_text(size=20))
d
ggsave(paste0("cheater_type_performance",v, ".pdf"), path=fig.path, width = 16, height = 5)



#########################
###Figure B2
#########################

p.df<-dat[, c("country_code", "hightype", "include_data", "declared_0", "declared_f", "declared_near")]
p.df$country[p.df$country_code==1] <- "Chile"
p.df$country[p.df$country_code==2] <- "Russia"
p.df$country[p.df$country_code==3] <- "U.K."


p.df$include_data[p.df$include_data==0]<-NA
p.df<-p.df[complete.cases(p.df$include_data),]


p.df$type[p.df$declared_0==1]<-"Maximal"
p.df$type[p.df$declared_f==1]<-"Limited"
p.df$type[p.df$declared_near==1]<-"Near-Maximal"



pt<-prop.table(table(p.df$type, p.df$hightype, p.df$country), c(3,2))

prop.t<-as.data.frame(pt)

names(prop.t)<-c("type", "perform_high" ,"country", "prop" )
prop.t$perform_high_lab<-ifelse(prop.t$perform_high==1, "High Performance", "Low Performance")

prop.t$type <- factor(prop.t$type, levels = c("Maximal", "Limited", "Near-Maximal"))



d<-ggplot(prop.t, aes(x = type, y = prop, colour=perform_high_lab, fill=perform_high_lab)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent , limits = c(0,1)) +
  xlab("") + labs(colour="", fill="") +
  scale_fill_manual("", values = c("red", "blue"))+
  theme(text = element_text(size=20))+
  #geom_hline(yintercept = 1/6, lty="dashed", col="red")+
  facet_wrap(~  country, ncol = 3) +  
  theme(legend.position="bottom", legend.text=element_text(size=20))
d

ggsave(paste0("FigureB2",v, ".pdf"), path=fig.path, width = 12, height = 5)

###############################
### Figure C2
################################
p.df<-dat[, c("country_code", "ind_typenew2", "offerdg" ,"offerdg_0", "include_data")]
p.df$country[p.df$country_code==1] <- "Chile"
p.df$country[p.df$country_code==2] <- "Russia"
p.df$country[p.df$country_code==3] <- "U.K."



p.df$type[p.df$ind_typenew2==1] <- "C. Maximal"
p.df$type[p.df$ind_typenew2==2] <- "C. Partial"
p.df$type[p.df$ind_typenew2==3] <- "C. Honest"
p.df$type[p.df$ind_typenew2==4] <- "Other"

p.df$dg_type<-ifelse(p.df$offerdg_0==1, "Give 0", "Give >0")



p.df$ind_typenew2[p.df$include_data==0]<-NA
p.df<-p.df[complete.cases(p.df$ind_typenew2),]


pt<-prop.table(table(p.df$type, p.df$dg_type, p.df$country), c(3,2))

prop.t<-as.data.frame(pt)

names(prop.t)<-c("type", "dg_type" ,"country", "prop" )



prop.t$type <- factor(prop.t$type, levels = c("C. Maximal", "C. Partial", "C. Honest", "Other"))
prop.t$dg_type <- factor(prop.t$dg_type, levels = c("Give 0", "Give >0"))



d<-ggplot(prop.t, aes(x = type, y = prop, colour=dg_type, fill=dg_type)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent , limits = c(0,1)) +
  xlab("") + labs(colour="", fill="") +
  theme(text = element_text(size=20))+
  scale_fill_manual("", values = c("red", "blue"))+
  #geom_hline(yintercept = 1/6, lty="dashed", col="red")+
  facet_wrap(~  country, ncol = 3) +  theme(legend.position="bottom", legend.text=element_text(size=20))
d
ggsave(paste0("cheater_type_dgoffer",v, ".pdf"), path=fig.path, width = 16, height = 5)





