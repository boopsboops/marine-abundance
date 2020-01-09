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
