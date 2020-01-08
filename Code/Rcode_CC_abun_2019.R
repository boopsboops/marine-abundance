rm(list = ls(all = TRUE))

library(effects)
library(lme4)
library(r2glmm)


##################################################################################################################
#Full dataset model and visualisation
#read full dataset
dat<-read.csv('../Data/All_dat.csv',header=TRUE, sep=',')

#full model
blr1a <- glmer(AbundanceChangeN ~ Position + (1|taxa_gp) + Timespan + Sampling + Hemisphere +   (1 |DOI_Record), family=binomial("logit"), data=dat)

#simplified model (fig 2a)
blr1 <- glm(AbundanceChangeN ~ Position,family=binomial("logit"), data=dat)

#model results 
summary(blr1)
eff_cf1 <- effect("Position", blr1)
r2beta(blr1, partial = TRUE, method = "sgv", data = dat)

#plot position effect from model
Modelplot <- plot(eff_cf1, rescale.axis=F, ylim=c(0,1), rug=FALSE,main="position effect plot -  all DATA ",ylab="Population abundance change ")
Modelplot
#split rug plot
plot(dat$Position, dat$AbundanceChangeN, pch="|", bg = "gray", cex=2)


##################################################################################################################
#spatially thinned dataset model
#read spatially thinned dataset
dat_NOREPEATS<-read.csv('../Data/No_repeat_dat.csv',header=TRUE, sep=',')

#full model
blra_norepeats <- glmer(AbundanceChangeN ~ Position + (1|taxa_gp) + Timespan + Sampling + Hemisphere +   (1 |DOI_Record), family=binomial("logit"), data=dat_NOREPEATS)

#simplified model (table S1)
blr_norepeats <- glm(AbundanceChangeN ~ Position,  family=binomial("logit"), data=dat_NOREPEATS)

#model results 
summary(blr_norepeats)
r2beta(blr_norepeats, partial = TRUE, method = "sgv", data = dat_NOREPEATS)


#only multispecies studies
#read multisp. dataset
dat_multisp<-read.csv('../Data/Multi_sp_dat.csv',header=TRUE, sep=',') 

#full model
blr_multi_a <- glmer(AbundanceChangeN ~ Position + (1|taxa_gp) + Timespan + Sampling + Hemisphere + 
                     +     (1 |DOI_Record), family=binomial("logit"), data=dat_multisp)

#simplified model
blr_multi <- glm(AbundanceChangeN ~ Position, family=binomial("logit"), data=dat_multisp)

#model results 
summary(blr_multi)
r2beta(blr_multi, partial = TRUE, method = "sgv", data = dat_multisp)


##################################################################################################################
#only significant trends model and visualisation
#read sig. dataset
dat_sig<-read.csv('../Data/Sig_only_dat.csv',header=TRUE, sep=',') #with one no change recrod removed

#full model
blr2a <- glmer(AbundanceChangeN ~ Position + (1|taxa_gp) + Timespan + Sampling + Hemisphere +     (1 |DOI_Record), family=binomial("logit"), data=dat_sig)

#simplified model
blr2 <- glm(AbundanceChangeN ~ Position, family=binomial("logit"), data=dat_sig)

#model results 
summary(blr2)
eff_cf2 <- effect("Position", blr2)
r2beta(blr2, partial = TRUE, method = "sgv", data = dat_sig)

#plot position effect from model
Modelplot <- plot(eff_cf2, rescale.axis=F, ylim=c(0,1), rug=FALSE,main="position effect plot - all species recorded as changing significantly",ylab="Population abundance change")
Modelplot
#split rug plot
plot(dat_sig$Position, dat_sig$AbundanceChangeN, pch="|", bg = "gray",cex=2)


##################################################################################################################
#only trends with reported r/r2 value model and visualisation
#read stat. dataset
dat_r<-read.csv('../Data/r.val_only_dat.csv',header=TRUE, sep=',')
x=dat_r$Position
y=dat_r$r_incl.r2

#simple linear model (fig. 2.c) with results
r_lm<-lm(y~x)
summary(r_lm)
#create CI data
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(r_lm, newdata=data.frame(x=newx), interval="confidence", level = 0.95)

#plotting simple lm (with CI's as calc'ed above)
plot(dat_r$Position,dat_r$r_incl.r2,main="r value records only (r value against position)")
# add CI fill
polygon(c(rev(newx), newx), c(rev(conf_interval[ ,3]), conf_interval[ ,2]), col = "#E2EDFF", border = NA)
points(dat_r$Position,dat_r$r_incl.r2,pch=16,cex=1.5)
abline(lm(dat_r$r_incl.r2 ~ dat_r$Position), col = "black",lwd=3)


##################################################################################################################
#Between functional group position effect (5 most well represented groups)
#subset full data ( 5 groups)
dat_func_group5<-subset(dat,taxa_gp=='Larvalbonyfish'|taxa_gp=='Fish'|taxa_gp=='Zooplankton'|taxa_gp=='Seabirds'|taxa_gp=='Invertebrates')

#interaction model
mod1<-glm(AbundanceChangeN ~ Position* taxa_gp, family=binomial("logit"),data=dat_func_group5)

#model results 
summary(mod1)
r2beta(mod1, partial = TRUE, method = "sgv", data = dat_func_group5)


###
#Functional group split models and visualisation
###
#Larval_fish
dat_larval<-subset(dat,taxa_gp=='Larvalbonyfish')

#model
blr_larval <- glm(AbundanceChangeN ~ Position, family=binomial("logit"), data=dat_larval) 

#model results
summary(blr_larval)
eff_cf_larval <- effect("Position", blr_larval)
r2beta(blr_larval, partial = TRUE, method = "sgv", data = dat_larval)

#plot position effect from model
Modelplot <- plot(eff_cf_larval, rescale.axis=F, ylim=c(0,1), rug=FALSE,main="position effect plot -  Larval only",ylab="Population abundance change ")
Modelplot
#split rug plot
plot(dat_larval$Position, dat_larval$AbundanceChangeN, pch="|", bg = "gray", cex=2)


###
#Fish
dat_fish<-subset(dat,taxa_gp=='Fish')

#model
blr_fish <- glm(AbundanceChangeN ~ Position, family=binomial("logit"), data=dat_fish) 

#model results
summary(blr_fish)
eff_cf_fish <- effect("Position", blr_fish)
r2beta(blr_fish, partial = TRUE, method = "sgv", data = dat_fish)

#plot position effect from model
Modelplot <- plot(eff_cf_fish, rescale.axis=F, ylim=c(0,1), rug=FALSE,main="position effect plot -  Fish only",ylab="Population abundance change ")
Modelplot
#split rug plot
plot(dat_fish$Position, dat_fish$AbundanceChangeN, pch="|", bg = "gray", cex=2)

###
#Zooplankton
dat_zoo<-subset(dat,taxa_gp=='Zooplankton')

#model
blr_zoo <- glm(AbundanceChangeN ~ Position, family=binomial("logit"), data=dat_zoo) 

#model results
summary(blr_zoo)
eff_cf_zoo <- effect("Position", blr_zoo)
r2beta(blr_zoo, partial = TRUE, method = "sgv", data = dat_zoo)

#plot position effect from model
Modelplot <- plot(eff_cf_zoo, rescale.axis=F, xlim=c(0,1), rug=FALSE,main="position effect plot -  zoo only",ylab="Population abundance change ")
Modelplot
#split rug plot
plot(dat_zoo$Position, dat_zoo$AbundanceChangeN, pch="|", bg = "gray", cex=2, ylim=c(0,1),xlim=c(0,1))


###
#Benthic invertebrates
dat_invert<-subset(dat,taxa_gp=='Invertebrates')

#model
blr_invert <- glm(AbundanceChangeN ~ Position, family=binomial("logit"), data=dat_invert) 

#model results
summary(blr_invert)
eff_cf_invert <- effect("Position", blr_invert)
r2beta(blr_invert, partial = TRUE, method = "sgv", data = dat_invert)

#plot position effect from model
Modelplot <- plot(eff_cf_invert, rescale.axis=F, ylim=c(0,1), rug=FALSE,main="position effect plot -  Invert only",ylab="Population abundance change ")
Modelplot
#split rug plot
plot(dat_invert$Position, dat_invert$AbundanceChangeN, pch="|", bg = "gray", cex=2)


###
#Seabirds
dat_birds<-subset(dat,taxa_gp=='Seabirds')

# model
blr_birds <- glm(AbundanceChangeN ~ Position, family=binomial("logit"), data=dat_birds) 

#model results
summary(blr_birds)
eff_cf_birds <- effect("Position", blr_birds)
r2beta(blr_birds, partial = TRUE, method = "sgv", data = dat_birds)

#plot position effect from model
Modelplot <- plot(eff_cf_birds, rescale.axis=F, ylim=c(0,1), rug=FALSE,main="position effect plot -  birds only",ylab="Population abundance change ")
Modelplot
plot(dat_birds$Position, dat_birds$AbundanceChangeN, pch="|", bg = "gray", cex=2)

##################################################################################################################

