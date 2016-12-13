# Why-we-Cheat-2016
Data and R Replication Code for Duch Solaz
We conduct tax compliance experiments in which subjects earn real money, are subject to a tax, and can lie about their earnings. 
Performing better on incentivized real effort tasks results in more cheating.  Cheating rises as the tax rate increases; but the higher 
levels of cheating by those who excelled at the tasks persists in high and low tax treatments.  This correlation persists when earnings 
are associated with luck; the correlation persists in experimental treatments with more redistributive taxation.  The correlation persists
for choices made in other games by the same subjects : High performance types give less in a conventional Dictator Game and also cheat
more in a classic die game in which they privately report the results of tossing a die. 

This replication packages includes:
**Experimental ZTree code: To reproduce the experimental sessions** 
 - Oxford 2012 MT - Baseline
 - Oxford 2013 MT - Redistribution	
 - Oxford 2013 MT - Status	
 - Oxford 2015 MT - Shock	
 - Oxford 2016 MT - Non-Fixed	
 - Santiago 2015 MT - Baseline	
 - Santiago 2015 MT - Shock	
 - Santiago 2015 MT - Status	
 - Santiago 2016 MT - Non-Fixed

**Data:** 
 - "MasterfileOxfordChile_20160506.dta", full dataset with all experimental sessions.
 - "MasterfileOxfordChile_20160506_b.dta", full dataset with recoding used for figures.
 - "base.dta", which includes 'baseline' and 'status' treatment sessions.
 - "shock.dta", includes 'shock' treatment sessions.
 - "redistribution.dta",  includes 'redistribution' treatment sessions.
 - "MasterfileOxford2016e.dta", includes 'Non-Fixed' treatment sessions. 
 
 
Code:
 - "Bootstrap_replication.do", to reproduce the boostrap data used in figure 2.
 - "Dach_Solaz_Multivariate_Laroze.R", replication code for table 4, figure 4, and appendix material. 
 - "Duch_Solaz_Laroze_figures.R", replication code for other figures in the text.
 
Manuscript:
 - PDF of the manuscript
 - Original .tex of the manuscript.
 
