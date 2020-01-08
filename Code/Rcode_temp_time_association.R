rm(list = ls(all = TRUE))

#study data - locations and timeframes detailed for each datapoint
dat_FINAL<-read.csv('../Data/location_timeframe_only_dat.csv',header=TRUE, sep=',')
dat_FINAL$ref<-1:nrow(dat_FINAL)

#hadisst data
global_nc_hadisst<-read.csv('../Data/GLOBAL_annual_mean_cell_wlocation_long_valid.csv.gz',header=TRUE, sep=',') #load hadisst dat

#make formats
global_nc_hadisst$year<-as.numeric(global_nc_hadisst$year)
dat_FINAL$yrstart<-as.numeric(dat_FINAL$yrstart)
dat_FINAL$yrend<-as.numeric(dat_FINAL$yrend)

#create subset for each unique time and location window (so not replicating within study where there are >1 species)
dat_FINAL_unq<-unique(dat_FINAL[c("yrstart","yrend","lat","lon")])
nrow(dat_FINAL_unq)
dat_FINAL_unq$ref<-1:nrow(dat_FINAL_unq)

#split data by timeframe/location 
dat_FINAL_unq_split <- split(dat_FINAL_unq,dat_FINAL_unq$ref)

#subset Hadisst data aligned with study location and timeframe data
correl_split_unq_ref <- list()
for(i in 1:length(names(dat_FINAL_unq_split))) {
  ref<- names(dat_FINAL_unq_split)[i]
  correl_split_unq_ref[[ref]]<-subset(global_nc_hadisst, lat %in% dat_FINAL_unq_split[[ref]]$lat &
                                    lon %in% dat_FINAL_unq_split[[ref]]$lon &
                                    year >= dat_FINAL_unq_split[[ref]]$yrstart &
                                    year <=  dat_FINAL_unq_split[[ref]]$yrend)
}

#drop data.frames in the list that don't have any data (i.e. don't align with HADDISSTT)
correl_split_unq_ref_drop<-correl_split_unq_ref[sapply(correl_split_unq_ref, function(x) dim(x)[1]) > 0] 

#correlation p value function
cor.func <- function(dat) {
  cor.p<-cor.test(dat$year,dat$value, method="spearman", exact = FALSE)$p.value
  return(cor.p)
}

#correlation r value function
cor.func_r <- function(dat) {
  cor.test<-cor(dat$year,dat$value, method="spearman")
  return(cor.test)
}

#run correl. 'p val output' with only filled (i.e. those with) data.frames from list
correl_split_unq_p_drop <- list()
for(i in 1:length(names(correl_split_unq_ref_drop))) {
  ref_p<-names(correl_split_unq_ref_drop)[i]
  correl_split_unq_p_drop[[ref_p]]<-cor.func(correl_split_unq_ref_drop[[ref_p]])
}

#run correl. 'r val output' with only filled (i.e. those with) data.frames from list
correl_split_unq_r_drop <- list()
for(i in 1:length(names(correl_split_unq_ref_drop))) {
  ref_p<-names(correl_split_unq_ref_drop)[i]
  correl_split_unq_r_drop[[ref_p]]<-cor.func_r(correl_split_unq_ref_drop[[ref_p]])
}


#r value dataframe for plotting
correl_split_unq_r_df<-as.data.frame(do.call(rbind, correl_split_unq_r_drop))

#boxplot of all r values
par(cex.axis=1.5)
boxplot(correl_split_unq_r_df,cex=1.5,lwd=2) #each study location/timeseries combination has a record (where Hadisst data available).



















    

LM_227_cell_dec <- list() 
for(i in 1:length(names(spp_227_split))) {
  sp_227_all <- names(spp_227_split)[i]
  LM_227_cell_dec[[sp_227_all]] <- LM_cell_ddec_func(spp_227_split[[sp_227_all]])
}


#all_LM_summ <- list() 
#for(i in 1:length(names(spp_227_split))) {
#  sp <- names(spp_227_split)[i]
#  all_LM_summ[[sp]] <- LM_summ_func(spp_227_split[[sp]])
#}
#head(all_LM_summ)

 

    #apply function #IF YOU START A FEW LINES UP YOU NEED TO LOAD THIS FUNC FROM ABOVE AGAIN TOO
    #cor_all <- list() 
    #for(i in 1:length(names(lsm_all_data_split))) {
    #  sp_sur_group_all <- names(lsm_all_data_split)[i]
    #  cor_all[[sp_sur_group_all]] <- cor_spp_sur_func(lsm_all_data_split[[sp_sur_group_all]])
    #}
    
    
    
    
    #func to create cor. dat/ls per survey per species 
    cor_spp_sur_func <- function(dat) {
      dat$cor_r<-cor(dat$mean_demidec_sp_survey_4thrt,dat$lsmean, method="spearman")
      dat$cor_p<-cor.test(dat$mean_demidec_sp_survey_4thrt,dat$lsmean, method="spearman", exact = FALSE)$p.value #USE ',exact = FALSE' if you want to avoid warnings about ties (CHECK THIS&&&&8**()@£__(£_)) https://stackoverflow.com/questions/10711395/spearman-correlation-and-ties GOOD INFO ON THIS AND COULD USE Kendall TAU but caveats for both
      dat$cor_t<-cor.test(dat$mean_demidec_sp_survey_4thrt,dat$lsmean, method="spearman", exact = FALSE)$statistic
      dat$cor_df<-cor.test(dat$mean_demidec_sp_survey_4thrt,dat$lsmean, method="spearman", exact = FALSE)$parameter
      return(dat)
    }    
      
  

#create function
LM_func <- function(dat) {
  LM<-lm(mean_demidec_sp_survey_4thrt~survey_mon_group +gridcell +demidec, data=dat)
  sum_LM<-summary(lsmeans(LM,~gridcell&~demidec&~survey_mon_group))
  LM_Wemmeans<-merge(dat,sum_LM,by=c("gridcell","survey_mon_group","demidec"))
  LM_Wemmeans$lsmean[LM_Wemmeans$lsmean < 0] <- 0
  return(LM_Wemmeans)
}



#create table of F, p etc summary model output
#LM_summ_func <- function(dat) {
#  LM<-Anova(lm(mean_demidec_sp_survey_4thrt~survey_mon_group +gridcell +demidec, data=dat))
#  LM_summ_df<-tidy(LM)
#  return(LM)
#}

#cont_shelf_mean_sp227_demidec #DAT FORMATTED

#spp_227_split <- split(cont_shelf_mean_sp227_demidec,cont_shelf_mean_sp227_demidec$valid_name)
#head(spp_227_split)

#all_LM_summ <- list() 
#for(i in 1:length(names(spp_227_split))) {
#  sp <- names(spp_227_split)[i]
#  all_LM_summ[[sp]] <- LM_summ_func(spp_227_split[[sp]])
#}
#head(all_LM_summ)

#LM_summ_all_spp_df<-as.data.frame(do.call(rbind, all_LM_summ))
#names(LM_summ_all_spp_df)[1] <- "valid_name"

#CODE TO RUN FUNC ON ALL LEVELS OF A VAR IN A DATSET - HERE PER SPECIES [SAVE FOR WHEN NEEDED and then only rerun when changes to source data happen]
#create 'list' to join output in file then ask for each level to have the species name and then run loop on all levels of the split (here linked to the relevant species name) FOR LM
#all_LM <- list() 
#for(i in 1:length(names(all_spp_split))) {
#  sp <- names(all_spp_split)[i]
#  all_LM[[sp]] <- LM_func(all_spp_split[[sp]])
#}
#head(all_LM)