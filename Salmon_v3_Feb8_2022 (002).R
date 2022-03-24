

# Run life cycle model

#TIMESTAMP SESSION
cat(paste("Session run on:",date(),"\n\n"))
#REMOVE ALL OBJECTS IN R ENVIRONMENT
rm(list=ls())
#SET CURRENT WORKING DIRECTORY

library(here) # Relative path use
library(dplyr)

### library(xlsx)
library(readxl)
library(gridExtra)
library(grid)

file.dir <- here(
    "McKenzie_LHModel","outputs")

#SET DEFAULT GRAPHICAL PARAMETERS
default.par <- par(no.readonly=TRUE)
Sys.setenv(TZ="America/Los_Angeles")	#need to add this to overcome issue with local (EDT) timezone and save re-specifying each time when working with date-time objects

options(max.print=999999)

### load libraries ###
library(extraDistr) #rtriang
library(EnvStats) #geoMean
# library(xlsx)
# library(gridExtra)
# library(grid)



### define functions ###
#PSM prediction from %wild and 7DADM temp (Zabel et al 2015, p3.6)
get_psm_zabel <- function(prop_hos, max_temp){
  lpsm <- -9.5789 + ((1-prop_hos) * -2.39) + (max_temp * 0.5492) #equation predicts PSM on logit scale
  psm <- exp(lpsm)/(1+exp(lpsm))
  return(psm)
}
#get_psm_zabel(prop_hos=0.36,max_temp=12)

#PSM prediction from PHOS and 7DADM temp (Bowerman et al 2018)
get_psm_bower <- function(pct_hos, max_temp){
  tmp_a <- rnorm(1,mean=0,sd=0.969) #study reach random effect
  tmp_b <- rnorm(1,mean=0,sd=0.428) #year random effect
  tmp_k <- rnorm(1,mean=0,sd=0.845) #observation level random effect
  lpsm <- -9.053 + (0.387*max_temp) + (0.029*pct_hos) + tmp_a + tmp_b + tmp_k #logit scale
  psm <- exp(lpsm)/(1+exp(lpsm))
  return(psm)
}
#get_psm_bower(pct_hos=64,max_temp=12)

get_summary_stats <- function(vec){
  mu <- mean(vec)
  std <- sd(vec)
  cv <- std/mu
  l95 <- quantile(vec,probs=0.025)
  med <- median(vec)
  u95 <- quantile(vec,probs=0.975)
  stats <- round(c(mu,std,cv,l95,med,u95),3)
  names(stats) <- c("mean","sd","cv","l95","median","u95")
  return(stats)
}

plot_output <- function(out,minlim=0,maxlim=1,xlabtext="") {
  #based on Roberto Licandeo function for consistency
  tmp <- get_summary_stats(out)
  hist(out, xlim=c(minlim,maxlim), xlab=xlabtext, freq=FALSE, cex.axis=1,
       breaks=20, main="", cex.lab=1.5, ylab="", las=1, col="gray70")
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  # legend("topright", bty="n", cex=1.,isummary, horiz=FALSE)
}


#alt <- "NAA"
#alt <- "Alt1"
#alt <- "Alt2a"
#alt <- "Alt2b"
#alt <- "Alt3a"
#alt <- "Alt3b"
#alt <- "Alt4"


run_lcm <- function(alt, case, n.sim){
#case <- "base-tmp"
# Old save procedure, no longer useful if using the `get_perf_metrics` f'n
  # save.dir <- paste0(here("McKenzie_LHModel",
  #     "outputs", "Chinook_McKenzie_"), case)
  # if(!dir.exists(save.dir)){
  #   dir.create(save.dir)
  # }
  
  ### control section
  N_yr <- 35 #number of years to simulate (assumes first 5 removed from outputs)
  vec_len <- N_yr+1 #length to create vectors (to include year 0)
  yr <- 0:N_yr #years
  
  TempBF <- TRUE #backfill missing temperature data with water year type mean?
  
  dtm <- FALSE #is model deterministic, or probabilistic
  
  # Prints out where you are in the script
  n.sim.ind <- seq(1000, n.sim, 1000)

  #set.seed(666)

  all_FBW <- read.csv(here("McKenzie_LHModel",
    "data", "fbw", "Feb7_2022_cgr_dam_fbw_annualsummaries.csv")) %>%
    # Mutate creates new columns, renaming "No action" to "NAA"
    mutate(alt_fbw = case_when(
        alt == "No action" ~ "NAA",
        TRUE ~ alt 
    )) %>%
    # Remove the column called "alt" so that it does not match global alt
    #   in the function call
    select(-alt) %>%
    rename(x.Year = year)
  all_FBW <- all_FBW %>%
    filter(
        # Filter to years of record, 1947+
        x.Year >= 1947
    ) %>%
    filter(
        alt_fbw == alt
    )
        # Filter to be only the alt in question
  # Create stage-specific subsets
  CGR_Fry <- all_FBW %>% filter(stage == "Fry")
  CGR_SubYr <- all_FBW %>% filter(stage == "Subs")
  CGR_Yrlg <- all_FBW %>% filter(stage == "Year")
  # All of these have the same dimensions, that's good!
  CGR_rec <- nrow(CGR_SubYr)
  #Above dam stream 7DADM temperature data 1936-2019 (for PSM bootstrap)
  AboveCGR <- read.csv(here(
      "McKenzie_LHModel",
      "data", "AboveCougar_USGS14159200_TempFlowData.csv"
  ))
  # AboveCGR$WYT <- factor(AboveCGR$WYT) # read.csv("C:/Users/tporteus/Documents/flow.csv")
  AboveCGR$WYT <- factor(AboveCGR$WYT)
  
  #...either backfill missing temperatures with across-years water year type mean
  if(TempBF==TRUE){
    Temp7DADM.WYT.mean <- tapply(AboveCGR$MayAug7dADMax,AboveCGR$WYT,mean,na.rm=TRUE)
    for(i in 1:nrow(AboveCGR)){
      if(is.na(AboveCGR$MayAug7dADMax[i])){
        tmp <- AboveCGR$WYT[i]
        #print(tmp)
        AboveCGR$MayAug7dADMax[i] <- Temp7DADM.WYT.mean[which(names(Temp7DADM.WYT.mean)==tmp)]
      }
    }
  }
  Temp7DADM <- AboveCGR$MayAug7dADMax
  names(Temp7DADM) <- AboveCGR$Year
  
  #...or simply remove all years with missing data
  Temp7DADM <- Temp7DADM[which(!is.na(Temp7DADM))]
  
  #match DPE/DPS years with 7DADM temperature years
  Temp7DADM <- Temp7DADM[which(names(Temp7DADM)%in%CGR_SubYr$x.Year)]
  yrsTempFBW <- length(Temp7DADM) #number of years with both data types
  
  #Downstream temperature data (for downstream growth)
  #USGS temperature data (2011=abundant, 2015=deficit, 2016=adequate WYT)
    ds_month_temp <- read.csv(
      here("McKenzie_LHModel","data", "Mckenzie_DownstreamDam_MeanTemperature.csv")
  )

#   ds_month_temp <- read.csv("C:/Users/tporteus/Documents/.csv",header=TRUE)
  if(alt=="NAA"){
    ds_month_temp <- ds_month_temp[,c(1,2,9,16)]
    #  CGR_Fry_Pass_Date <-
  }
  if(alt=="Alt1"){
    ds_month_temp <- ds_month_temp[,c(1,3,10,17)]
  }
  if(alt=="Alt2a"){
    ds_month_temp <- ds_month_temp[,c(1,4,11,18)]
  }
  if(alt=="Alt2b"){
    ds_month_temp <- ds_month_temp[,c(1,5,12,19)]
  }
  if(alt=="Alt3a"){
    ds_month_temp <- ds_month_temp[,c(1,6,13,20)]
  }
  if(alt=="Alt3b"){
    ds_month_temp <- ds_month_temp[,c(1,7,14,21)]
  }
  if(alt=="Alt4"){
    ds_month_temp <- ds_month_temp[,-c(1,8,15,22)]
  }
  
  #read in CJS model joint posterior for tailrace releases

  #read in adjusted RSS estimates, survivals and proportions returning at age from CJS SAS numerical matching
  #each row represents RSS and SAS estimates from the joint posterior (see North_Santiam_sa_cjssas_pa_adj.R)
  #CH_CSM_2013_CGRTR results were used (hatchery release)
  CJS_posterior <- read.csv(
      here("McKenzie_LHModel","data", "cjs_posteriors", 
        "Feb7_2022_McKenzie_sa_cjssas_pa_adj_inclrss.csv")
  )
  CJS_iter <- nrow(CJS_posterior) #number of iterations in saved joint posterior to sample from
  
  #read in estimated proportions at age, early year survivals and deviates river to ocean survival
  #values come from model calibration and stored in linked xlsx to allow for updating to calibration sheet
  
  ###!!!### DEVIATE INPUTS REQUIRED HERE
  ###!!!### The file name can be the same, just needs information in sheets _devs
  deviate_file <- here("McKenzie_LHModel","data",#  "nsantiam_eg", 
    "survival_devs_mcK_Feb102022.xlsx")
    # From file: 

  
  mcK_devs <- as.numeric(
      data.frame(
          read_xlsx(deviate_file, sheet = "MCK_devs", col_names=F)
        )[,1]) # Pulls out the first column (all rows indicated by ",")
        # and turns into a numeric vector
#   NS_devs <- as.numeric((NS_devs$X1))
#   SS_devs <- as.numeric(
#       data.frame(
#           read_xlsx(deviate_file, sheet = "SS_devs", col_names=F)
#         )[,1]) # Pulls out the first column (all rows indicated by ",")

  ###!!!### Could mix deviates with N and SSantiam
  devs <- c(mcK_devs) #,SS_devs) ##low number of deviates not ideal - future option could be to use log-t based on the empirical values?
  devs_mean_all <- mean(devs)
  #hist(devs)
  #hist(exp(devs))
  #one outlier value >8, would lead to extreme sa_1plus_3 (>0.9), only use value >2 for sa0plus3, remove from sa1plus3?
  #same deviates used on both survivals, examine effect of removing extreme value?
  devs <- devs[which(devs>-2&devs<2)]
  devs_mean_trunc <- mean(devs)
  #to avoid bias need to renormalise deviates once outlier removed so that mean is the same (this method reduces sd in deviates)
  devs <- devs + (devs_mean_all-devs_mean_trunc)
  
    sa_0plus_3_c <- as.numeric(
      read_xlsx(
          deviate_file,
          sheet="MCK_sa0plus_3", col_names = F))
#   sa_0plus_3_c <- as.numeric(sa_0plus_3_c) # (sa_0plus_3_c$X1))
  sa_1plus_3_c <- as.numeric(
      read_xlsx(
          deviate_file, 
          sheet="MCK_sa1plus_3", col_names = F))
#   sa_1plus_3_c <- as.numeric((sa_1plus_3_c$X1))

  pa_0plus_3 <- as.numeric(
      read_xlsx(
          deviate_file,
          sheet="MCK_pa0plus_3", col_names=F))
#   pa_0plus_3 <- as.numeric((pa_0plus_3$X1))
  pa_3_4 <- as.numeric(
      read_xlsx(deviate_file,
      sheet="MCK_pa3_4", col_names=FALSE))
#   pa_3_4 <- as.numeric((pa_3_4$X1))
  pa_4_5 <- as.numeric(
      read_xlsx(deviate_file,
      sheet="MCK_pa4_5", col_names=FALSE))
#   pa_4_5 <- as.numeric((pa_4_5$X1))
  pa_5_6 <- 1.0000

#   sa_0plus_3_c <- read.xlsx("survival_devs.xlsx",sheetName="NS_sa0plus_3",header=FALSE)
#   sa_0plus_3_c <- as.numeric((sa_0plus_3_c$X1))
#   sa_1plus_3_c <- read.xlsx("survival_devs.xlsx",sheetName="NS_sa1plus_3",header=FALSE)
#   sa_1plus_3_c <- as.numeric((sa_1plus_3_c$X1))

#   pa_0plus_3 <- read.xlsx("survival_devs.xlsx",sheetName="NS_pa0plus_3",header=FALSE)
#   pa_0plus_3 <- as.numeric((pa_0plus_3$X1))
#   pa_3_4 <- read.xlsx("survival_devs.xlsx",sheetName="NS_pa3_4",header=FALSE)
#   pa_3_4 <- as.numeric((pa_3_4$X1))
#   pa_4_5 <- read.xlsx("survival_devs.xlsx",sheetName="NS_pa4_5",header=FALSE)
#   pa_4_5 <- as.numeric((pa_4_5$X1))
  # pa_5_6 <- 1.0000
  
  #break
  
  ### RUN SIMULATION MODEL ###
  
  cat(paste0("Running ",alt," simulation (n=",n.sim,")\n"))
  
  ### define simulation storage vectors/lists
  NO4_NO0 <- vector("numeric",length=n.sim) #performance metric

  F_suj_nsim <- vector(mode="list",length=n.sim)
  Sr_suj_nsim <- vector(mode="list",length=n.sim)
  Ss_suj_nsim <- vector(mode="list",length=n.sim)
  Yrsw_suj_nsim <- vector(mode="list",length=n.sim)
  Yrw_suj_nsim <- vector(mode="list",length=n.sim)
  Ys_suj_nsim <- vector(mode="list",length=n.sim)
  
  NO_F_rtn_nsim <- vector(mode="list",length=n.sim)
  NO_Sr_rtn_nsim <- vector(mode="list",length=n.sim)
  NO_Ss_rtn_nsim <- vector(mode="list",length=n.sim)
  NO_Yrsw_rtn_nsim <- vector(mode="list",length=n.sim)
  NO_Yrw_rtn_nsim <- vector(mode="list",length=n.sim)
  NO_Ys_rtn_nsim <- vector(mode="list",length=n.sim)
  
  NO_rtn_cgr_nsim <- vector(mode="list",length=n.sim) #NOR returns to dam
  NO_spwn_nsim <- vector(mode="list",length=n.sim) #NO spawners (pre-cap, post PSM)
  pHOS_nsim <- vector(mode="list",length=n.sim) #PHOS (post-cap)

  psm_o_all <- vector(mode="list",length=n.sim) #PSM (above dam) time series from each simulation
  psm_e_all <- vector(mode="list",length=n.sim) #PSM (en route)
  h_rrs_all <- vector(mode="list",length=n.sim) #hatchery relative reproductive success
  sa0plus3_all <- vector(mode="list",length=n.sim) #marine survival age0+3
  sa1plus3_all <- vector(mode="list",length=n.sim) #marine survival age1+3
  bh_b_all <- vector(mode="list",length=n.sim) #egg capacity
  res_f2s_all <- vector(mode="list",length=n.sim) #reservoir survival (fry-sub)
  res_s2y_all <- vector(mode="list",length=n.sim) #reservoir survival (sub-yrlg)
  rss_f_all <- vector(mode="list",length=n.sim) #downstream migrant survival (fry)
  rss_sr_all <- vector(mode="list",length=n.sim) #downstream migrant survival (sub-resv)
  rss_ss_all <- vector(mode="list",length=n.sim) #downstream migrant survival (sub-stream)
  rss_yrsw_all <- vector(mode="list",length=n.sim) #downstream migrant survival (yrlg-resvSW)
  rss_yrw_all <- vector(mode="list",length=n.sim) #downstream migrant survival (yrlg-resvW)
  rss_ys_all <- vector(mode="list",length=n.sim) #downstream migrant survival (yrlg-stream)
  
  DPE_F_all <- vector(mode="list",length=n.sim) #DPE (fry) in all years
  DPE_S_all <- vector(mode="list",length=n.sim) #DPE (subyearlings) in all years
  DPE_Y_all <- vector(mode="list",length=n.sim) #DPE (yearlings) in all years
  DPS_F_all <- vector(mode="list",length=n.sim) #DPS (fry) in all years
  DPS_S_all <- vector(mode="list",length=n.sim) #DPS (subyearlings) in all years
  DPS_Y_all <- vector(mode="list",length=n.sim) #DPS (yearlings) in all years
  
  Temp7DADM_all <- vector(mode="list",length=n.sim) #7DADM temperature above dam in all years
  
  F_f2s_nsim <- vector(mode="list",length=n.sim) #fry-smolt survival for fry LHT
  Sr_f2s_nsim <- vector(mode="list",length=n.sim)
  Ss_f2s_nsim <- vector(mode="list",length=n.sim)
  Yrsw_f2s_nsim <- vector(mode="list",length=n.sim)
  Yrw_f2s_nsim <- vector(mode="list",length=n.sim)
  Ys_f2s_nsim <- vector(mode="list",length=n.sim)
  
  start.time <- Sys.time()
  
  for(i in 1:n.sim){
    
    if(i %in% n.sim.ind) cat(paste0("\n...",i))
    
    #bootstrap year-specific data for this simulation
    FBW_yrs <- sample(yrsTempFBW,vec_len,replace=TRUE)
    max_temp <- Temp7DADM[FBW_yrs]
    #CGR_SubYr$Year[FBW_yrs]
    
    #sample joint posterior for adjusted rss and survivals and proportions returning at age
    CJS_yrs <- sample(CJS_iter,vec_len,replace=FALSE)
    
    ### define data vectors
    NO_rtn_cgr <- vector("numeric",length=vec_len) #natural origin adult returns to Cougar dam
    NO_HO_cgr <- vector("numeric",length=vec_len) #total number of adults to be outplanted above Cougar (NOR and HOR)
    NO_outp <- vector("numeric",length=vec_len) #natural origin outplants above Cougar
    HO_outp <- vector("numeric",length=vec_len) #hatchery origin outplants above Cougar
    NO_spwn <- vector("numeric",length=vec_len) #natural origin spawners
    HO_spwn <- vector("numeric",length=vec_len)
    pHOS <- vector("numeric",length=vec_len) # % hatchery origin spawners (for use in calculating PSM and performance metrics)
    NO_egg <- vector("numeric",length=vec_len) #natural origin eggs
    HO_egg <- vector("numeric",length=vec_len)
    F_emg <- vector("numeric",length=vec_len) #emergent fry (natural and hatchery origin)
    F_hor <- vector("numeric",length=vec_len) #fry movers at head of reservoir
    S_hor <- vector("numeric",length=vec_len) #subyearling stayers at head of reservoir
    Y_hor <- vector("numeric",length=vec_len) #yearling stayers at head of reservoir
    F_fby <- vector("numeric",length=vec_len) #fry (spring subyearlings) at Cougar forebay
    Sr_fby <- vector("numeric",length=vec_len) #fall subyearling (reservoir over summer) at forebay
    Ss_fby <- vector("numeric",length=vec_len) #fall subyearling (natal stream over summer) at forebay
    Yrsw_fby <- vector("numeric",length=vec_len) #spring yearling (reservoir over summer/winter) at forebay
    Yrw_fby <- vector("numeric",length=vec_len) #spring yearling (reservoir over winter only) at forebay
    Ys_fby <- vector("numeric",length=vec_len) #spring yearling (natal stream over winter) at forebay
    F_tr <- vector("numeric",length=vec_len) #fry (spring subyearling) at Cougar tailrace
    Sr_tr <- vector("numeric",length=vec_len)
    Ss_tr <- vector("numeric",length=vec_len)
    Yrsw_tr <- vector("numeric",length=vec_len)
    Yrw_tr <- vector("numeric",length=vec_len)
    Ys_tr <- vector("numeric",length=vec_len)
    Y_dead <- vector("numeric",length=vec_len) #spring yearlings dead in reservoir
    F_suj <- vector("numeric",length=vec_len) #fry (spring subyearling) at Sullivan
    Sr_suj <- vector("numeric",length=vec_len)
    Ss_suj <- vector("numeric",length=vec_len)
    Yrsw_suj <- vector("numeric",length=vec_len)
    Yrw_suj <- vector("numeric",length=vec_len)
    Ys_suj <- vector("numeric",length=vec_len)
    
    NO_F_age0_3_sea <- vector("numeric",length=vec_len) #fish remaining at sea
    NO_Sr_age0_3_sea <- vector("numeric",length=vec_len)
    NO_Ss_age0_3_sea <- vector("numeric",length=vec_len)
    NO_Yrsw_age0_3_sea <- vector("numeric",length=vec_len)
    NO_Yrw_age0_3_sea <- vector("numeric",length=vec_len)
    NO_Ys_age0_3_sea <- vector("numeric",length=vec_len)
    NO_F_age3_4_sea <- vector("numeric",length=vec_len)
    NO_Sr_age3_4_sea <- vector("numeric",length=vec_len)
    NO_Ss_age3_4_sea <- vector("numeric",length=vec_len)
    NO_Yrsw_age3_4_sea <- vector("numeric",length=vec_len)
    NO_Yrw_age3_4_sea <- vector("numeric",length=vec_len)
    NO_Ys_age3_4_sea <- vector("numeric",length=vec_len)
    NO_F_age4_5_sea <- vector("numeric",length=vec_len)
    NO_Sr_age4_5_sea <- vector("numeric",length=vec_len)
    NO_Ss_age4_5_sea <- vector("numeric",length=vec_len)
    NO_Yrsw_age4_5_sea <- vector("numeric",length=vec_len)
    NO_Yrw_age4_5_sea <- vector("numeric",length=vec_len)
    NO_Ys_age4_5_sea <- vector("numeric",length=vec_len)
    NO_F_age5_6_sea <- vector("numeric",length=vec_len)
    NO_Sr_age5_6_sea <- vector("numeric",length=vec_len)
    NO_Ss_age5_6_sea <- vector("numeric",length=vec_len)
    NO_Yrsw_age5_6_sea <- vector("numeric",length=vec_len)
    NO_Yrw_age5_6_sea <- vector("numeric",length=vec_len)
    NO_Ys_age5_6_sea <- vector("numeric",length=vec_len)
    
    NO_F_age0_3_rtn <- vector("numeric",length=vec_len) #fish returning to river
    NO_Sr_age0_3_rtn <- vector("numeric",length=vec_len)
    NO_Ss_age0_3_rtn <- vector("numeric",length=vec_len)
    NO_Yrsw_age0_3_rtn <- vector("numeric",length=vec_len)
    NO_Yrw_age0_3_rtn <- vector("numeric",length=vec_len)
    NO_Ys_age0_3_rtn <- vector("numeric",length=vec_len)
    NO_F_age3_4_rtn <- vector("numeric",length=vec_len)
    NO_Sr_age3_4_rtn <- vector("numeric",length=vec_len)
    NO_Ss_age3_4_rtn <- vector("numeric",length=vec_len)
    NO_Yrsw_age3_4_rtn <- vector("numeric",length=vec_len)
    NO_Yrw_age3_4_rtn <- vector("numeric",length=vec_len)
    NO_Ys_age3_4_rtn <- vector("numeric",length=vec_len)
    NO_F_age4_5_rtn <- vector("numeric",length=vec_len)
    NO_Sr_age4_5_rtn <- vector("numeric",length=vec_len)
    NO_Ss_age4_5_rtn <- vector("numeric",length=vec_len)
    NO_Yrsw_age4_5_rtn <- vector("numeric",length=vec_len)
    NO_Yrw_age4_5_rtn <- vector("numeric",length=vec_len)
    NO_Ys_age4_5_rtn <- vector("numeric",length=vec_len)
    NO_F_age5_6_rtn <- vector("numeric",length=vec_len)
    NO_Sr_age5_6_rtn <- vector("numeric",length=vec_len)
    NO_Ss_age5_6_rtn <- vector("numeric",length=vec_len)
    NO_Yrsw_age5_6_rtn <- vector("numeric",length=vec_len)
    NO_Yrw_age5_6_rtn <- vector("numeric",length=vec_len)
    NO_Ys_age5_6_rtn <- vector("numeric",length=vec_len)
    
    NO_age3rtn <- vector("numeric",length=vec_len) #total of age returning across life histories
    NO_age4rtn <- vector("numeric",length=vec_len)
    NO_age5rtn <- vector("numeric",length=vec_len)
    NO_age6rtn <- vector("numeric",length=vec_len)
    NO_rtn_wff <- vector("numeric",length=vec_len) #natural origin adult returns at Willamette Falls
    
    F_f2s <- vector("numeric",length=vec_len) #fry-smolt survival for fry LHT
    Sr_f2s <- vector("numeric",length=vec_len)
    Ss_f2s <- vector("numeric",length=vec_len)
    Yrsw_f2s <- vector("numeric",length=vec_len)
    Yrw_f2s <- vector("numeric",length=vec_len)
    Ys_f2s <- vector("numeric",length=vec_len)
    
    ### define parameters ###
    ###!!!###
    ###!!!### Outplanting data needs to be updated

    Ninit_cgr <- 130 # Cougar - avg. returns last 5 years
    # 668 #runif(1,200,500) #Initial number of NO returns to CGR in year 0 (361 value in CBP phase 2 report?)
    HO_n <- 600 #Max number of HO adults to outplant each year
    if(dtm==TRUE){
        psm_e <- rep(0.165,vec_len)
    }else{
      psm_e <- rbeta(vec_len,shape1=130.543,shape2=654.93716) #runif(vec_len,min=0.141,max=0.193) #PSM en route from WFF, Keefer et al. 2017 (mean=0.165, 95%CI=[0.141,0.193])
    }
    psm_o <- vector("numeric",length=vec_len) #PSM on-site - CALULATED IN EACH YEAR BASED UPON PHOS and temperature
    
    # These have already been changed
    fec_4n <- 4500 #fecundity age-4 NO (eggs/female spawner) - MAKE AGE-RELATED???
    fec_4h <- 4250 #fecundity age-4 HO (eggs/female spawner) - MAKE AGE-RELATED???
    sxr <- 0.5 #F:M ratio (i.e. >0.5 means more females)
    if(dtm==TRUE){
      h_rrs <- rep(0.53,vec_len) #hatchery relative spawning success, data from Christie et al. (2014)
    }else{
      h_rrs <- rtriang(vec_len,a=0.39,b=0.84,c=0.53) #hatchery relative spawning success, data from Christie et al. (2014)
    }
    bh_a <- 0.57 #Bev-Holt a (fry/egg)
    if(dtm==TRUE){
      egg_cap_d <- rep(6000000,vec_len)
      egg_cap_e <- rep(12500000,vec_len)
    }else{
      egg_cap_d <- rnorm(vec_len,mean=6000000,sd=100000)
      egg_cap_e <- rnorm(vec_len,mean=12500000,sd=250000)
    }
    bh_b <- rnorm(vec_len,mean=18000000,sd=100000) # egg_cap_d+egg_cap_e #Bev-Holt b (egg capacity)
    
    fms_f <- 0.75 #Fry-Migrant Survival (fry mover) - NOT DENSITY DEPENDENT
    fms_s <- 0.425 #Fry-Migrant Survival (fall subyearling stayer) - NOT DENSITY DEPENDENT
    fms_y <- 0.325 #Fry-Migrant Survival (spring yearling stayer) - NOT DENSITY DEPENDENT
    
    res_f2f <- 1 #Reservoir Survival (fry to fry) - assumed to move straight to forebay
    if(dtm==TRUE){
      res_f2s <- rep(0.2,vec_len) #Reservoir Survival (fry to fall subyearling) - NOT DENSITY DEPENDENT
    }else{
      res_f2s <- rtriang(vec_len,a=0.15,b=0.35,c=0.2) #Reservoir Survival (fry to fall subyearling) - NOT DENSITY DEPENDENT
    }
    #
    #!# Deprecated: MODIFIED TO BE VECTORS
    # res_f2s <- rep(0.2, vec_len)
    res_s2s <- 1 #Reservoir Survival (fall subyearling to fall subyearling) - assumed to move straight to forebay
    if(dtm==TRUE){
      res_s2y <- rep(0.65,vec_len) #Reservoir survival (fall subyearling to spring yearling) - NOT DENSITY DEPENDENT
    }else{
      res_s2y <- runif(vec_len,min=0.4,max=0.9) #Reservoir survival (fall subyearling to spring yearling) - NOT DENSITY DEPENDENT
    }
    #
    #!# MODIFIED TO BE VECTORS
    # res_s2y <- rep(0.65, vec_len)
    res_y2y <- 1 #Reservoir Survival (spring yearling to spring yearling) - assumed to move straight to forebay
    
    #all RSS estimates from CJS models have been adjusted for tagging and hatchery effects
    #used Zabel 2015 (p.5.11) differences between RSS for each age to scale estimate obtained from subyearling 
    #release that were majority subyearling migrants (see CJS_adjustment_factors.R)
    if(dtm==TRUE){
      rss_f <- rep(median(CJS_posterior$rss_f),vec_len)
      rss_sr <- rep(median(CJS_posterior$rss_s),vec_len)
      rss_ss <- rep(median(CJS_posterior$rss_s),vec_len)
      rss_yrsw <- rep(median(CJS_posterior$rss_y),vec_len)
      rss_yrw <- rep(median(CJS_posterior$rss_y),vec_len)
      rss_ys <- rep(median(CJS_posterior$rss_y),vec_len)
    }else{
      rss_f <- CJS_posterior$rss_f[CJS_yrs]
      rss_sr <- CJS_posterior$rss_s[CJS_yrs]
      rss_ss <- CJS_posterior$rss_s[CJS_yrs]
      rss_yrsw <- CJS_posterior$rss_y[CJS_yrs]
      rss_yrw <- CJS_posterior$rss_y[CJS_yrs]
      rss_ys <- CJS_posterior$rss_y[CJS_yrs]
    }
    
    ###!!!### 
    # In future, change to 2000
    cap <- Inf # 5428 #Max number of outplanted adults (HGMP goal for natural production)
    
    if(dtm==TRUE){
      DPE_F <- rep(0.614580490711748,vec_len) #Dam Passage Efficiency (FBW, average for Fry)
      DPE_S <- rep(0.491250472299772,vec_len) #Dam Passage Efficiency (FBW, average for Subyearling)
      DPE_Y <- rep(0.222409625570813,vec_len) #Dam Passage Efficiency (FBW, average for Yearling)
      DPS_F <- rep(0.632843554155646,vec_len) #Dam Passage Survival (FBW, average for Fry)
      DPS_S <- rep(0.619996759527611,vec_len) #Dam Passage Survival (FBW, average for Subyearling)
      DPS_Y <- rep(0.53748726383196,vec_len) #Dam Passage Survival (FBW, average for Yearling)
    }else{
      DPE_F <- CGR_Fry$x.DPE[FBW_yrs]
      DPE_S <- CGR_SubYr$x.DPE[FBW_yrs]
      DPE_Y <- CGR_Yrlg$x.DPE[FBW_yrs]
      DPS_F <- CGR_Fry$x.DPS[FBW_yrs]
      DPS_S <- CGR_SubYr$x.DPS[FBW_yrs]
      DPS_Y <- CGR_Yrlg$x.DPS[FBW_yrs]
    }
    ### define life history splits - from Zabel et al. (2015)
    p_eF_horF <- 0.94 #Emergent fry -> Fry movers to Reservoir
    p_eF_horS <- 0.05 #Emergent fry -> Fall subyearling stayer to Reservoir
    p_eF_horY <- 0.01 #Emergent fry -> Spring Yearling stayer to Reservoir
    p_horF_fbyF <- 0.065 #Fry mover in Resv -> Fry in Forebay
    p_horF_fbyS <- 0.935 #Fry mover in Resv -> Fall subyearling in Forebay
    p_horS_fbyS <- 0.825 #Fall subyr stayer -> Fall subyearling in Forebay
    p_horS_fbyY <- 0.175 #Fall subyr stayer -> Spring yearling in Forebay
    p_horY_fbyY <- 1 #Spring yearling stayer in Resv -> Spring yearling in Forebay
    
    #survival rate at age from natural mortality in the ocean
    if(dtm==TRUE){
      #sa_0plus_3 <- 0.0132 #see NSant_NumericalMatching.SASScaling.Age_0_to_Age_3_Survival_mkm_v13_TP.xlsx
      #sa_1plus_3 <- 0.1131 #see NSant_NumericalMatching.SASScaling.Age_1_to_Age_3_Survival_mkm_v13_TP.xlsx
      sa_0plus_3_out <- rep(sa_0plus_3_c,vec_len) #river-ocean survival
      sa_0plus_3_in <- sa_0plus_3_out #ocean-river survival
      sa_1plus_3_out <- rep(sa_1plus_3_c,vec_len)
      sa_1plus_3_in <- sa_1plus_3_out
      # Same in McKenzie and Santiam:
      sa_3_4 <- 0.3969
      sa_4_5 <- 0.5000
      sa_5_6 <- 0.6300
    }else{
      # sa_0plus_3 <- CJS_posterior$sa0plus_a3[i]
      # sa_1plus_3 <- CJS_posterior$sa1plus_a3[i]
      # sa_3_4 <- CJS_posterior$sa3_a4[i]
      # sa_4_5 <- CJS_posterior$sa4_a5[i]
      # sa_5_6 <- CJS_posterior$sa5_a6[i]
      devs_yrs <- sample(devs,vec_len,replace=TRUE) #bootstrap vector of survival deviates
      sa_0plus_3_out <- sa_0plus_3_c*exp(devs_yrs) #include deviates in river-ocean survival only
      sa_0plus_3_in <- sa_0plus_3_c
      sa_1plus_3_out <- sa_1plus_3_c*exp(devs_yrs)
      sa_1plus_3_in <- sa_1plus_3_c
      sa_3_4 <- 0.3969
      sa_4_5 <- 0.5000
      sa_5_6 <- 0.6300
    }
    
    #scalar on M (same for all ages) from calibration
    #Mscale <- 1#0.7856
    
    #proportion returning to river at age - now global values
    #if(dtm==TRUE){
    #  pa_0plus_3 <- 0.0134
    #  pa_3_4 <- 0.4668
    #  pa_4_5 <- 0.9112
    #  pa_5_6 <- 1.0000
    #}else{
    #  pa_0plus_3 <- CJS_posterior$pr_ret_a0plus_a3[i]
    #  pa_3_4 <- CJS_posterior$pr_ret_a3_a4[i]
    #  pa_4_5 <- CJS_posterior$pr_ret_a4_a5[i]
    #  pa_5_6 <- 1.0000
    #}
    #harvest rate at age when in ocean (commercial fishery)
    uao_3 <- 0.0545
    uao_4 <- 0.1090
    uao_5 <- 0.1090
    uao_6 <- 0.1090
    #harvest rate at age when going from ocean to river (terminal fishery)
    uar_3 <- 0.0520
    uar_4 <- 0.0520
    uar_5 <- 0.0520
    uar_6 <- 0.0520
    
    #observed spawner age distribution
    # From calibration script 
    obs_spawn_3 <- 0
    obs_spawn_4 <- 0.51
    obs_spawn_5 <- 0.4772
    obs_spawn_6 <- 0.0128
    
    NO_rtn_cgr[1] <- Ninit_cgr
    NO_HO_cgr[1] <- min(max(NO_rtn_cgr[1],HO_n),cap)
    # if(alt=="NAA"){
    #   NO_outp[1] <- 0
    # }else{
    # If NO_rtn_cgr <= HO_n, if returns in year y are less than or equal to 
    #   max outplants, return NO_rtn_cgr; else, return hatchery + 
      NO_outp[1] <- ifelse(NO_rtn_cgr[1]<=HO_n,NO_rtn_cgr[1],NO_HO_cgr[1])
    # }
    HO_outp[1] <- NO_HO_cgr[1]-NO_outp[1]
    #calculate psm_o (onsite PSM) given PHOS and 7DADM temperature
    if(dtm==TRUE){
      pHOS[1] <- HO_outp[1]/(NO_outp[1]+HO_outp[1])
      # Changed by MD but made no difference in calibration script according to OM
      psm_o[1] <- 0.1 # 0.057
    }else{
      pHOS[1] <- HO_outp[1]/(NO_outp[1]+HO_outp[1])
      psm_o[1] <- get_psm_bower(pct_hos = pHOS[1], max_temp = max_temp[1])
    }
    
    for(y in 2:(N_yr+1)){
      #life cycle model
      NO_spwn[y] <- NO_outp[y-1]*(1-psm_o[y-1])
      HO_spwn[y] <- HO_outp[y-1]*(1-psm_o[y-1])
      #pHOS[y] <- HO_spwn[y]/(NO_spwn[y]+HO_spwn[y])
      NO_egg[y] <- NO_spwn[y]*sxr*fec_4n
      HO_egg[y] <- HO_spwn[y]*sxr*fec_4h*h_rrs[y]
      F_emg[y] <- (bh_a*(NO_egg[y]+HO_egg[y]))/(1+bh_a*(NO_egg[y]+HO_egg[y])/bh_b[y])
      
      F_hor[y] <- F_emg[y]*p_eF_horF*fms_f
      S_hor[y] <- F_emg[y]*p_eF_horS*fms_s
      Y_hor[y] <- ifelse(y==2, 0, F_emg[y-1]*p_eF_horY*fms_y)
      
      F_fby[y] <- F_hor[y]*p_horF_fbyF*res_f2f
      Sr_fby[y] <- F_hor[y]*p_horF_fbyS*res_f2s[y] + F_fby[y]*(1-DPE_F[y])*res_f2s[y]
      Ss_fby[y] <- S_hor[y]*p_horS_fbyS*res_s2s
      Yrsw_fby[y] <- ifelse(y==2, 0, S_hor[y-1]*p_horS_fbyY*res_s2y[y-1] + Sr_fby[y-1]*(1-DPE_S[y-1])*res_s2y[y-1]) #DPE[y-1]???
      Yrw_fby[y] <- ifelse(y==2, 0, Ss_fby[y-1]*(1-DPE_S[y-1])*res_s2y[y-1])
      Ys_fby[y] <- ifelse(y==2, 0, Y_hor[y]*res_y2y)

      F_tr[y] <- F_fby[y]*DPE_F[y]*DPS_F[y]
      Sr_tr[y] <- Sr_fby[y]*DPE_S[y]*DPS_S[y]
      Ss_tr[y] <- Ss_fby[y]*DPE_S[y]*DPS_S[y]
      Yrsw_tr[y] <- ifelse(y==2, 0, Yrsw_fby[y]*DPE_Y[y]*DPS_Y[y])
      Yrw_tr[y] <- ifelse(y==2, 0, Yrw_fby[y]*DPE_Y[y]*DPS_Y[y])
      Ys_tr[y] <- ifelse(y==2, 0, Ys_fby[y]*DPE_Y[y]*DPS_Y[y])
      Y_dead[y] <- ifelse(y==2, 0, (Yrsw_fby[y]+Yrw_fby[y] + Ys_fby[y])*(1-DPE_Y[y]))
      
      F_suj[y] <- F_tr[y]*rss_f[y]
      Sr_suj[y] <- Sr_tr[y]*rss_sr[y]
      Ss_suj[y] <- Ss_tr[y]*rss_ss[y]
      Yrsw_suj[y] <- ifelse(y==2, 0, Yrsw_tr[y]*rss_yrsw[y])
      Yrw_suj[y] <- ifelse(y==2, 0, Yrw_tr[y]*rss_yrw[y])
      Ys_suj[y] <- ifelse(y==2, 0, Ys_tr[y]*rss_ys[y])
  
      
      ## at sea processes including M scalar
      # NO_F_age0_3_sea[y] <- ifelse(y%in%2:3, 0, F_suj[y-2]*(1-pa_0plus_3)*exp(-(Mscale*-log(sa_0plus_3)))*(1-uao_3))
      # NO_Sr_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Sr_suj[y-2]*(1-pa_0plus_3)*exp(-(Mscale*-log(sa_1plus_3)))*(1-uao_3))
      # NO_Ss_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Ss_suj[y-2]*(1-pa_0plus_3)*exp(-(Mscale*-log(sa_1plus_3)))*(1-uao_3))
      # NO_Yrsw_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Yrsw_suj[y-1]*(1-pa_0plus_3)*exp(-(Mscale*-log(sa_1plus_3)))*(1-uao_3))
      # NO_Yrw_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Yrw_suj[y-1]*(1-pa_0plus_3)*exp(-(Mscale*-log(sa_1plus_3)))*(1-uao_3))
      # NO_Ys_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Ys_suj[y-1]*(1-pa_0plus_3)*exp(-(Mscale*-log(sa_1plus_3)))*(1-uao_3))
      # 
      # NO_F_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_F_age0_3_sea[y-1]*(1-pa_3_4)*exp(-(Mscale*-log(sa_3_4)))*(1-uao_4))
      # NO_Sr_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Sr_age0_3_sea[y-1]*(1-pa_3_4)*exp(-(Mscale*-log(sa_3_4)))*(1-uao_4))
      # NO_Ss_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Ss_age0_3_sea[y-1]*(1-pa_3_4)*exp(-(Mscale*-log(sa_3_4)))*(1-uao_4))
      # NO_Yrsw_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Yrsw_age0_3_sea[y-1]*(1-pa_3_4)*exp(-(Mscale*-log(sa_3_4)))*(1-uao_4))
      # NO_Yrw_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Yrw_age0_3_sea[y-1]*(1-pa_3_4)*exp(-(Mscale*-log(sa_3_4)))*(1-uao_4))
      # NO_Ys_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Ys_age0_3_sea[y-1]*(1-pa_3_4)*exp(-(Mscale*-log(sa_3_4)))*(1-uao_4))
      # 
      # NO_F_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_F_age3_4_sea[y-1]*(1-pa_4_5)*exp(-(Mscale*-log(sa_4_5)))*(1-uao_5))
      # NO_Sr_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Sr_age3_4_sea[y-1]*(1-pa_4_5)*exp(-(Mscale*-log(sa_4_5)))*(1-uao_5))
      # NO_Ss_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Ss_age3_4_sea[y-1]*(1-pa_4_5)*exp(-(Mscale*-log(sa_4_5)))*(1-uao_5))
      # NO_Yrsw_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Yrsw_age3_4_sea[y-1]*(1-pa_4_5)*exp(-(Mscale*-log(sa_4_5)))*(1-uao_5))
      # NO_Yrw_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Yrw_age3_4_sea[y-1]*(1-pa_4_5)*exp(-(Mscale*-log(sa_4_5)))*(1-uao_5))
      # NO_Ys_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Ys_age3_4_sea[y-1]*(1-pa_4_5)*exp(-(Mscale*-log(sa_4_5)))*(1-uao_5))
      # 
      # NO_F_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_F_age4_5_sea[y-1]*(1-pa_5_6)*exp(-(Mscale*-log(sa_5_6)))*(1-uao_6))
      # NO_Sr_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Sr_age4_5_sea[y-1]*(1-pa_5_6)*exp(-(Mscale*-log(sa_5_6)))*(1-uao_6))
      # NO_Ss_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Ss_age4_5_sea[y-1]*(1-pa_5_6)*exp(-(Mscale*-log(sa_5_6)))*(1-uao_6))
      # NO_Yrsw_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Yrsw_age4_5_sea[y-1]*(1-pa_5_6)*exp(-(Mscale*-log(sa_5_6)))*(1-uao_6))
      # NO_Yrw_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Yrw_age4_5_sea[y-1]*(1-pa_5_6)*exp(-(Mscale*-log(sa_5_6)))*(1-uao_6))
      # NO_Ys_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Ys_age4_5_sea[y-1]*(1-pa_5_6)*exp(-(Mscale*-log(sa_5_6)))*(1-uao_6))
      # 
      # 
      # NO_F_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, F_suj[y-2]*pa_0plus_3*exp(-(Mscale*-log(sa_0plus_3)))*(1-uar_3))
      # NO_Sr_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Sr_suj[y-2]*pa_0plus_3*exp(-(Mscale*-log(sa_1plus_3)))*(1-uar_3))
      # NO_Ss_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Ss_suj[y-2]*pa_0plus_3*exp(-(Mscale*-log(sa_1plus_3)))*(1-uar_3))
      # NO_Yrsw_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Yrsw_suj[y-1]*pa_0plus_3*exp(-(Mscale*-log(sa_1plus_3)))*(1-uar_3))
      # NO_Yrw_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Yrw_suj[y-1]*pa_0plus_3*exp(-(Mscale*-log(sa_1plus_3)))*(1-uar_3))
      # NO_Ys_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Ys_suj[y-1]*pa_0plus_3*exp(-(Mscale*-log(sa_1plus_3)))*(1-uar_3))
      # 
      # NO_F_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_F_age0_3_sea[y-1]*pa_3_4*exp(-(Mscale*-log(sa_3_4)))*(1-uar_4))
      # NO_Sr_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Sr_age0_3_sea[y-1]*pa_3_4*exp(-(Mscale*-log(sa_3_4)))*(1-uar_4))
      # NO_Ss_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Ss_age0_3_sea[y-1]*pa_3_4*exp(-(Mscale*-log(sa_3_4)))*(1-uar_4))
      # NO_Yrsw_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Yrsw_age0_3_sea[y-1]*pa_3_4*exp(-(Mscale*-log(sa_3_4)))*(1-uar_4))
      # NO_Yrw_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Yrw_age0_3_sea[y-1]*pa_3_4*exp(-(Mscale*-log(sa_3_4)))*(1-uar_4))
      # NO_Ys_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Ys_age0_3_sea[y-1]*pa_3_4*exp(-(Mscale*-log(sa_3_4)))*(1-uar_4))
      # 
      # NO_F_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_F_age3_4_sea[y-1]*pa_4_5*exp(-(Mscale*-log(sa_4_5)))*(1-uar_5))
      # NO_Sr_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Sr_age3_4_sea[y-1]*pa_4_5*exp(-(Mscale*-log(sa_4_5)))*(1-uar_5))
      # NO_Ss_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Ss_age3_4_sea[y-1]*pa_4_5*exp(-(Mscale*-log(sa_4_5)))*(1-uar_5))
      # NO_Yrsw_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Yrsw_age3_4_sea[y-1]*pa_4_5*exp(-(Mscale*-log(sa_4_5)))*(1-uar_5))
      # NO_Yrw_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Yrw_age3_4_sea[y-1]*pa_4_5*exp(-(Mscale*-log(sa_4_5)))*(1-uar_5))
      # NO_Ys_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Ys_age3_4_sea[y-1]*pa_4_5*exp(-(Mscale*-log(sa_4_5)))*(1-uar_5))
      # 
      # NO_F_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_F_age4_5_sea[y-1]*pa_5_6*exp(-(Mscale*-log(sa_5_6)))*(1-uar_6))
      # NO_Sr_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Sr_age4_5_sea[y-1]*pa_5_6*exp(-(Mscale*-log(sa_5_6)))*(1-uar_6))
      # NO_Ss_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Ss_age4_5_sea[y-1]*pa_5_6*exp(-(Mscale*-log(sa_5_6)))*(1-uar_6))
      # NO_Yrsw_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Yrsw_age4_5_sea[y-1]*pa_5_6*exp(-(Mscale*-log(sa_5_6)))*(1-uar_6))
      # NO_Yrw_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Yrw_age4_5_sea[y-1]*pa_5_6*exp(-(Mscale*-log(sa_5_6)))*(1-uar_6))
      # NO_Ys_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Ys_age4_5_sea[y-1]*pa_5_6*exp(-(Mscale*-log(sa_5_6)))*(1-uar_6))
      
      
      ## at sea processes NOT including M scalar but included devs
      NO_F_age0_3_sea[y] <- ifelse(y%in%2:3, 0, F_suj[y-2]*(1-pa_0plus_3)*sa_0plus_3_out[y-2]*(1-uao_3))
      NO_Sr_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Sr_suj[y-2]*(1-pa_0plus_3)*sa_1plus_3_out[y-2]*(1-uao_3))
      NO_Ss_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Ss_suj[y-2]*(1-pa_0plus_3)*sa_1plus_3_out[y-2]*(1-uao_3))
      NO_Yrsw_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Yrsw_suj[y-1]*(1-pa_0plus_3)*sa_1plus_3_out[y-2]*(1-uao_3))
      NO_Yrw_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Yrw_suj[y-1]*(1-pa_0plus_3)*sa_1plus_3_out[y-2]*(1-uao_3))
      NO_Ys_age0_3_sea[y] <- ifelse(y%in%2:3, 0, Ys_suj[y-1]*(1-pa_0plus_3)*sa_1plus_3_out[y-2]*(1-uao_3))

      NO_F_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_F_age0_3_sea[y-1]*(1-pa_3_4)*sa_3_4*(1-uao_4))
      NO_Sr_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Sr_age0_3_sea[y-1]*(1-pa_3_4)*sa_3_4*(1-uao_4))
      NO_Ss_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Ss_age0_3_sea[y-1]*(1-pa_3_4)*sa_3_4*(1-uao_4))
      NO_Yrsw_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Yrsw_age0_3_sea[y-1]*(1-pa_3_4)*sa_3_4*(1-uao_4))
      NO_Yrw_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Yrw_age0_3_sea[y-1]*(1-pa_3_4)*sa_3_4*(1-uao_4))
      NO_Ys_age3_4_sea[y] <- ifelse(y%in%2:4, 0, NO_Ys_age0_3_sea[y-1]*(1-pa_3_4)*sa_3_4*(1-uao_4))

      NO_F_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_F_age3_4_sea[y-1]*(1-pa_4_5)*sa_4_5*(1-uao_5))
      NO_Sr_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Sr_age3_4_sea[y-1]*(1-pa_4_5)*sa_4_5*(1-uao_5))
      NO_Ss_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Ss_age3_4_sea[y-1]*(1-pa_4_5)*sa_4_5*(1-uao_5))
      NO_Yrsw_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Yrsw_age3_4_sea[y-1]*(1-pa_4_5)*sa_4_5*(1-uao_5))
      NO_Yrw_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Yrw_age3_4_sea[y-1]*(1-pa_4_5)*sa_4_5*(1-uao_5))
      NO_Ys_age4_5_sea[y] <- ifelse(y%in%2:5, 0, NO_Ys_age3_4_sea[y-1]*(1-pa_4_5)*sa_4_5*(1-uao_5))

      NO_F_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_F_age4_5_sea[y-1]*(1-pa_5_6)*sa_5_6*(1-uao_6))
      NO_Sr_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Sr_age4_5_sea[y-1]*(1-pa_5_6)*sa_5_6*(1-uao_6))
      NO_Ss_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Ss_age4_5_sea[y-1]*(1-pa_5_6)*sa_5_6*(1-uao_6))
      NO_Yrsw_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Yrsw_age4_5_sea[y-1]*(1-pa_5_6)*sa_5_6*(1-uao_6))
      NO_Yrw_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Yrw_age4_5_sea[y-1]*(1-pa_5_6)*sa_5_6*(1-uao_6))
      NO_Ys_age5_6_sea[y] <- ifelse(y%in%2:6, 0, NO_Ys_age4_5_sea[y-1]*(1-pa_5_6)*sa_5_6*(1-uao_6))

      NO_F_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, F_suj[y-2]*pa_0plus_3*sa_0plus_3_in*(1-uar_3))
      NO_Sr_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Sr_suj[y-2]*pa_0plus_3*sa_1plus_3_in*(1-uar_3))
      NO_Ss_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Ss_suj[y-2]*pa_0plus_3*sa_1plus_3_in*(1-uar_3))
      NO_Yrsw_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Yrsw_suj[y-1]*pa_0plus_3*sa_1plus_3_in*(1-uar_3))
      NO_Yrw_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Yrw_suj[y-1]*pa_0plus_3*sa_1plus_3_in*(1-uar_3))
      NO_Ys_age0_3_rtn[y] <- ifelse(y%in%2:3, 0, Ys_suj[y-1]*pa_0plus_3*sa_1plus_3_in*(1-uar_3))

      NO_F_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_F_age0_3_sea[y-1]*pa_3_4*sa_3_4*(1-uar_4))
      NO_Sr_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Sr_age0_3_sea[y-1]*pa_3_4*sa_3_4*(1-uar_4))
      NO_Ss_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Ss_age0_3_sea[y-1]*pa_3_4*sa_3_4*(1-uar_4))
      NO_Yrsw_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Yrsw_age0_3_sea[y-1]*pa_3_4*sa_3_4*(1-uar_4))
      NO_Yrw_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Yrw_age0_3_sea[y-1]*pa_3_4*sa_3_4*(1-uar_4))
      NO_Ys_age3_4_rtn[y] <- ifelse(y%in%2:4, 0, NO_Ys_age0_3_sea[y-1]*pa_3_4*sa_3_4*(1-uar_4))

      NO_F_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_F_age3_4_sea[y-1]*pa_4_5*sa_4_5*(1-uar_5))
      NO_Sr_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Sr_age3_4_sea[y-1]*pa_4_5*sa_4_5*(1-uar_5))
      NO_Ss_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Ss_age3_4_sea[y-1]*pa_4_5*sa_4_5*(1-uar_5))
      NO_Yrsw_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Yrsw_age3_4_sea[y-1]*pa_4_5*sa_4_5*(1-uar_5))
      NO_Yrw_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Yrw_age3_4_sea[y-1]*pa_4_5*sa_4_5*(1-uar_5))
      NO_Ys_age4_5_rtn[y] <- ifelse(y%in%2:5, 0, NO_Ys_age3_4_sea[y-1]*pa_4_5*sa_4_5*(1-uar_5))

      NO_F_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_F_age4_5_sea[y-1]*pa_5_6*sa_5_6*(1-uar_6))
      NO_Sr_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Sr_age4_5_sea[y-1]*pa_5_6*sa_5_6*(1-uar_6))
      NO_Ss_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Ss_age4_5_sea[y-1]*pa_5_6*sa_5_6*(1-uar_6))
      NO_Yrsw_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Yrsw_age4_5_sea[y-1]*pa_5_6*sa_5_6*(1-uar_6))
      NO_Yrw_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Yrw_age4_5_sea[y-1]*pa_5_6*sa_5_6*(1-uar_6))
      NO_Ys_age5_6_rtn[y] <- ifelse(y%in%2:6, 0, NO_Ys_age4_5_sea[y-1]*pa_5_6*sa_5_6*(1-uar_6))

      
      NO_age3rtn[y] <- ifelse(y%in%2:3, obs_spawn_3*(Ninit_cgr/(1-psm_e[y])), NO_F_age0_3_rtn[y] + NO_Sr_age0_3_rtn[y] + NO_Ss_age0_3_rtn[y] + 
                                NO_Yrsw_age0_3_rtn[y] + NO_Yrw_age0_3_rtn[y] + NO_Ys_age0_3_rtn[y])
      
      NO_age4rtn[y] <- ifelse(y%in%2:4, obs_spawn_4*(Ninit_cgr/(1-psm_e[y])), 
                              NO_F_age3_4_rtn[y] + NO_Sr_age3_4_rtn[y] + NO_Ss_age3_4_rtn[y] + 
                                NO_Yrsw_age3_4_rtn[y] + NO_Yrw_age3_4_rtn[y] + NO_Ys_age3_4_rtn[y])
      
      NO_age5rtn[y] <- ifelse(y%in%2:5, obs_spawn_5*(Ninit_cgr/(1-psm_e[y])), 
                              NO_F_age4_5_rtn[y] + NO_Sr_age4_5_rtn[y] + NO_Ss_age4_5_rtn[y] + 
                                NO_Yrsw_age4_5_rtn[y] + NO_Yrw_age4_5_rtn[y] + NO_Ys_age4_5_rtn[y])
      
      NO_age6rtn[y] <- ifelse(y%in%2:6, obs_spawn_6*(Ninit_cgr/(1-psm_e[y])), 
                              NO_F_age5_6_rtn[y] + NO_Sr_age5_6_rtn[y] + NO_Ss_age5_6_rtn[y] + 
                                NO_Yrsw_age5_6_rtn[y] + NO_Yrw_age5_6_rtn[y] + NO_Ys_age5_6_rtn[y])
      
      NO_rtn_wff[y] <- NO_age3rtn[y]+NO_age4rtn[y]+NO_age5rtn[y]+NO_age6rtn[y]
      
      NO_rtn_cgr[y] <- NO_rtn_wff[y]*(1-psm_e[y])
      
      NO_HO_cgr[y] <- min(max(NO_rtn_cgr[y],HO_n),cap)
      #if(alt=="NAA"){
      #  NO_outp[y] <- 0
      #}else{
        NO_outp[y] <- ifelse(NO_rtn_cgr[y]<=HO_n,NO_rtn_cgr[y],NO_HO_cgr[y])
      #}
      HO_outp[y] <- NO_HO_cgr[y]-NO_outp[y]
      
      #calculate psm_o (onsite PSM) given PHOS and 7DADM temperature
      if(dtm==TRUE){
        pHOS[y] <- HO_outp[y]/(NO_outp[y]+HO_outp[y])
        psm_o[y] <- 0.057
      }else{
        pHOS[y] <- HO_outp[y]/(NO_outp[y]+HO_outp[y])
        psm_o[y] <- get_psm_bower(pct_hos = pHOS[y], max_temp = max_temp[y])
        #cat(paste0("Year ",y-1,": pHOS=",round(pHOS[y],3),", 7DADM=",max_temp[y],", PSM=",round(psm_o[y],3),"\n"))
      }
      
      #calculate LHT fry-smolt survival in each year [fry to reservoir*reservoir*DPS*tailrace to SUJ]
      F_f2s[y] <- fms_f*res_f2f*DPS_F[y]*rss_f[y]
      Sr_f2s[y] <- fms_f*res_f2s[y]*DPS_S[y]*rss_sr[y]
      Ss_f2s[y] <- fms_s*res_s2s*DPS_S[y]*rss_ss[y]
      Yrsw_f2s[y] <- ifelse(y==2, NA, fms_f*res_f2s[y-1]*res_s2y[y-1]*DPS_Y[y]*rss_yrsw[y])
      Yrw_f2s[y] <- ifelse(y==2, NA, fms_s*res_s2y[y-1]*DPS_Y[y]*rss_yrw[y])
      Ys_f2s[y] <- ifelse(y==2, NA, fms_y*res_y2y*DPS_Y[y]*rss_ys[y])
      
    }#end y
    
    ### create summaries for performance metrics ###
    #number of natural-origin spawners (post-PSM, before outplant cap) in year 1 of generation 4 vs. year 0 of generation 0
    NO_gen4 <- NO_rtn_cgr[21]*(1-psm_o[21])
    NO_gen0 <- ((HO_outp[1]*h_rrs[1]) + NO_outp[1])*(1-psm_o[1])
    NO4_NO0[i] <- NO_gen4 / NO_gen0
    
    F_suj_nsim[[i]] <- F_suj
    Sr_suj_nsim[[i]] <- Sr_suj
    Ss_suj_nsim[[i]] <- Ss_suj
    Yrsw_suj_nsim[[i]] <- Yrsw_suj
    Yrw_suj_nsim[[i]] <- Yrw_suj
    Ys_suj_nsim[[i]] <- Ys_suj
    
    NO_F_rtn_nsim[[i]] <- NO_F_age0_3_rtn + NO_F_age3_4_rtn + NO_F_age4_5_rtn + NO_F_age5_6_rtn
    NO_Sr_rtn_nsim[[i]] <- NO_Sr_age0_3_rtn + NO_Sr_age3_4_rtn + NO_Sr_age4_5_rtn + NO_Sr_age5_6_rtn
    NO_Ss_rtn_nsim[[i]] <- NO_Ss_age0_3_rtn + NO_Ss_age3_4_rtn + NO_Ss_age4_5_rtn + NO_Ss_age5_6_rtn
    NO_Yrsw_rtn_nsim[[i]] <- NO_Yrsw_age0_3_rtn + NO_Yrsw_age3_4_rtn + NO_Yrsw_age4_5_rtn + NO_Yrsw_age5_6_rtn
    NO_Yrw_rtn_nsim[[i]] <- NO_Yrw_age0_3_rtn + NO_Yrw_age3_4_rtn + NO_Yrw_age4_5_rtn + NO_Yrw_age5_6_rtn
    NO_Ys_rtn_nsim[[i]] <- NO_Ys_age0_3_rtn + NO_Ys_age3_4_rtn + NO_Ys_age4_5_rtn + NO_Ys_age5_6_rtn
    
    psm_o_all[[i]] <- psm_o
    psm_e_all[[i]] <- psm_e
    h_rrs_all[[i]] <- h_rrs
    sa0plus3_all[[i]] <- sa_0plus_3_out #incorporating deviates only in river-ocean survival
    sa1plus3_all[[i]] <- sa_1plus_3_out #incorporating deviates only in river-ocean survival
    bh_b_all[[i]] <- bh_b
    res_f2s_all[[i]] <- res_f2s
    res_s2y_all[[i]] <- res_s2y
    rss_f_all[[i]] <- rss_f
    rss_sr_all[[i]] <- rss_sr
    rss_ss_all[[i]] <- rss_ss
    rss_yrsw_all[[i]] <- rss_yrsw
    rss_yrw_all[[i]] <- rss_yrw
    rss_ys_all[[i]] <- rss_ys
    
    DPE_F_all[[i]] <- DPE_F
    DPE_S_all[[i]] <- DPE_S
    DPE_Y_all[[i]] <- DPE_Y
    DPS_F_all[[i]] <- DPS_F
    DPS_S_all[[i]] <- DPS_S
    DPS_Y_all[[i]] <- DPS_Y
    
    Temp7DADM_all[[i]] <- max_temp
    
    F_f2s_nsim[[i]] <- F_f2s
    Sr_f2s_nsim[[i]] <- Sr_f2s
    Ss_f2s_nsim[[i]] <- Ss_f2s
    Yrsw_f2s_nsim[[i]] <- Yrsw_f2s
    Yrw_f2s_nsim[[i]] <- Yrw_f2s
    Ys_f2s_nsim[[i]] <- Ys_f2s
    
    
    NO_rtn_cgr_nsim[[i]] <- NO_rtn_cgr
    NO_spwn_nsim[[i]] <- NO_rtn_cgr*(1-psm_o) #number of natural-origin spawners (post-PSM, before outplant cap)
    pHOS_nsim[[i]] <- pHOS
    
  }#end i
  
  finish.time <- Sys.time()
  time.taken <- finish.time-start.time
  cat("\n\n")
  print(time.taken)
  cat("\n")
  
  #annual pHOS
  pHOS_dat <- do.call("rbind",pHOS_nsim)
  #annual NOR returns
  NO_rtn_dat <- do.call("rbind",NO_rtn_cgr_nsim)
  #annual NOR spawners (post-PSM, before outplant cap)
  NO_spwn_dat <- do.call("rbind",NO_spwn_nsim)
  #juvenile smolts by life history type
  F_suj_dat <- do.call("rbind",F_suj_nsim)
  Sr_suj_dat <- do.call("rbind",Sr_suj_nsim)
  Ss_suj_dat <- do.call("rbind",Ss_suj_nsim)
  Yrsw_suj_dat <- do.call("rbind",Yrsw_suj_nsim)
  Yrw_suj_dat <- do.call("rbind",Yrw_suj_nsim)
  Ys_suj_dat <- do.call("rbind",Ys_suj_nsim)
  
  #adult returns by life history type
  NO_F_rtn_dat <- do.call("rbind",NO_F_rtn_nsim)
  NO_Sr_rtn_dat <- do.call("rbind",NO_Sr_rtn_nsim)
  NO_Ss_rtn_dat <- do.call("rbind",NO_Ss_rtn_nsim)
  NO_Yrsw_rtn_dat <- do.call("rbind",NO_Yrsw_rtn_nsim)
  NO_Yrw_rtn_dat <- do.call("rbind",NO_Yrw_rtn_nsim)
  NO_Ys_rtn_dat <- do.call("rbind",NO_Ys_rtn_nsim)
  
  #fry-smolt survival by life history type
  F_f2s_dat <- do.call("rbind",F_f2s_nsim)
  Sr_f2s_dat <- do.call("rbind",Sr_f2s_nsim)
  Ss_f2s_dat <- do.call("rbind",Ss_f2s_nsim)
  Yrsw_f2s_dat <- do.call("rbind",Yrsw_f2s_nsim)
  Yrw_f2s_dat <- do.call("rbind",Yrw_f2s_nsim)
  Ys_f2s_dat <- do.call("rbind",Ys_f2s_nsim)
  
  #parameters with uncertainty
  Temp7DADM_all <- unlist(Temp7DADM_all)
  psm_o_all <- unlist(psm_o_all)
  psm_e_all <- unlist(psm_e_all)
  h_rrs_all <- unlist(h_rrs_all)
  bh_b_all <- unlist(bh_b_all)/1e6
  res_f2s_all <- unlist(res_f2s_all)
  res_s2y_all <- unlist(res_s2y_all)
  rss_f_all <- unlist(rss_f_all)
  rss_sr_all <- unlist(rss_sr_all)
  rss_ss_all <- unlist(rss_ss_all)
  rss_yrsw_all <- unlist(rss_yrsw_all)
  rss_yrw_all <- unlist(rss_yrw_all)
  rss_ys_all <- unlist(rss_ys_all)
  sa0plus3_all <- unlist(sa0plus3_all)
  sa1plus3_all <- unlist(sa1plus3_all)
  DPE_F_all <- unlist(DPE_F_all)
  DPE_S_all <- unlist(DPE_S_all)
  DPE_Y_all <- unlist(DPE_Y_all)
  DPS_F_all <- unlist(DPS_F_all)
  DPS_S_all <- unlist(DPS_S_all)
  DPS_Y_all <- unlist(DPS_Y_all)
  
  #return data list for performance metric calculations
  return(list(NO4_NO0=NO4_NO0,pHOS_dat=pHOS_dat,NO_rtn_dat=NO_rtn_dat,NO_spwn_dat=NO_spwn_dat,
              F_suj_dat=F_suj_dat,Sr_suj_dat=Sr_suj_dat,Ss_suj_dat=Ss_suj_dat,
              Yrsw_suj_dat=Yrsw_suj_dat,Yrw_suj_dat=Yrw_suj_dat,Ys_suj_dat=Ys_suj_dat,
              NO_F_rtn_dat=NO_F_rtn_dat,NO_Sr_rtn_dat=NO_Sr_rtn_dat,NO_Ss_rtn_dat=NO_Ss_rtn_dat,
              NO_Yrsw_rtn_dat=NO_Yrsw_rtn_dat,NO_Yrw_rtn_dat=NO_Yrw_rtn_dat,NO_Ys_rtn_dat=NO_Ys_rtn_dat,
              F_f2s_dat=F_f2s_dat,Sr_f2s_dat=Sr_f2s_dat,Ss_f2s_dat=Ss_f2s_dat,
              Yrsw_f2s_dat=Yrsw_f2s_dat,Yrw_f2s_dat=Yrw_f2s_dat,Ys_f2s_dat=Ys_f2s_dat,
              Temp7DADM_all=Temp7DADM_all,psm_o_all=psm_o_all,psm_e_all=psm_e_all,h_rrs_all=h_rrs_all,
              bh_b_all=bh_b_all,res_f2s_all=res_f2s_all,res_s2y_all=res_s2y_all,
              rss_f_all=rss_f_all,rss_sr_all=rss_sr_all,rss_ss_all=rss_ss_all,
              rss_yrsw_all=rss_yrsw_all,rss_yrw_all=rss_yrw_all,rss_ys_all=rss_ys_all,
              sa0plus3_all=sa0plus3_all,sa1plus3_all=sa1plus3_all,
              DPE_F_all=DPE_F_all,DPE_S_all=DPE_S_all,DPE_Y_all=DPE_Y_all,
              DPS_F_all=DPS_F_all,DPS_S_all=DPS_S_all,DPS_Y_all=DPS_Y_all))
  
}

#case <- "base-tmp"
#alt <- "NAA"
#run_lcm("NAA","base-tmp")
#break

# MD Note for future: change file.dir to here() type commands?
get_perf_metrics <- function(alt,case,n.sim){

  dat <<- run_lcm(alt,case,n.sim)
  ### control section
  ### Save directory - include more directory structure
  head.dir <- paste0(file.dir,"/McKenzie_deviates_Feb11")
  save.dir <- paste0(file.dir,"/McKenzie_deviates_Feb11/",case)
  # Added this to also create "head.dir"
  if(!dir.exists(head.dir)){
    dir.create(head.dir)
  }
  if(!dir.exists(save.dir)){
    dir.create(save.dir)
  }
  alt_save.dir <- paste0(file.dir,"/McKenzie_deviates_Feb11/",case, "/", alt)
  # Save any global data in save.dir, alt-specific data in alt folder
  if(!dir.exists(alt_save.dir)){
    dir.create(alt_save.dir)
  }
  setwd(alt_save.dir)
  NO4_NO0_stats <- get_summary_stats(dat$NO4_NO0)
  
  tmp_breaks <- seq(0,max(dat$NO4_NO0)+1,length.out=40)
  png(
      paste0(
          "CH_MCK_",alt,"_NO4_NO0_pdf_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  tmp1 <- hist(dat$NO4_NO0,freq=FALSE,breaks=tmp_breaks,
    xlim=c(0,quantile(dat$NO4_NO0,probs=1)), #0.999)),
               xlab="NO spawners generation 4 / generation 0",main=alt)
  tmp <- as.numeric(round(get_summary_stats(dat$NO4_NO0),3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  dev.off()
  
  #calculate annual NOR returns statistics (remove yr0-5)
  NO_rtn_dat <- dat$NO_rtn_dat[,-(1:6)]
  NO_rtn_median <- apply(NO_rtn_dat,2,median)
  NO_rtn_mean <- apply(NO_rtn_dat,2,mean)
  NO_rtn_l95 <- sapply(1:ncol(NO_rtn_dat),function(i) quantile(NO_rtn_dat[,i],probs=0.025))
  NO_rtn_u95 <- sapply(1:ncol(NO_rtn_dat),function(i) quantile(NO_rtn_dat[,i],probs=0.975))
  
  #read in observed NOR returns from recent years
#  NOR_obs <- c(647,521,502,245,788,1609,464) #2015-2021
  NOR_obs <- c(157,244,165,68,78,95)
  NOR_obs_mean <- mean(NOR_obs)
  NOR_obs_l95 <- quantile(NOR_obs,probs=0.025)
  NOR_obs_u95 <- quantile(NOR_obs,probs=0.975)
  
  png(paste0("CH_MCK_",alt,"_NO_returns_ts_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  plot(NO_rtn_median,type="l",lwd=2,xaxt="n",ylim=c(0,max(NO_rtn_u95)),xlab="Year",ylab="NOR returns",main=alt)
  lines(NO_rtn_l95,lty=2)
  lines(NO_rtn_u95,lty=2)
  if(alt=="NAA") lines(rep(NOR_obs_mean,length(NO_rtn_median)),lty=1,col="red")
  if(alt=="NAA") lines(rep(NOR_obs_l95,length(NO_rtn_median)),lty=2,col="red")
  if(alt=="NAA") lines(rep(NOR_obs_u95,length(NO_rtn_median)),lty=2,col="red")
  if(alt=="NAA") legend("topleft",c("observed","model"),lty=1,col=c("red","black"),bty="n")
  axis(1,at=c(0,5,10,15,20,25,30),labels=c(0,5,10,15,20,25,30))
  dev.off()
  # 
  
  #parameter distributions
  png(paste0("CH_MCK_",alt,"_Pars1_",case,"_",Sys.Date(),".png"), height=16, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  par(mfrow=c(4,2),mai=c(0.5,0.3,0.4,0.05))
  plot_output(dat$psm_o_all,minlim = 0, maxlim = 1, xlabtext = "PSM (above CGR)")
  plot_output(dat$psm_e_all,minlim = 0, maxlim = 1, xlabtext = "PSM (below CGR)")
  plot_output(dat$h_rrs_all,minlim = 0, maxlim = 1, xlabtext = "Hatchery relative reproductive success")
  plot_output(dat$bh_b_all,minlim = 15, maxlim = 25, xlabtext = "B-H b (eggs, /1e6)")
  plot_output(dat$res_f2s_all,minlim = 0, maxlim = 1, xlabtext = "Reservoir survival F->S")
  plot_output(dat$res_s2y_all,minlim = 0, maxlim = 1, xlabtext = "Reservoir survival S->Y")
  plot_output(dat$Temp7DADM_all,minlim = 5, maxlim = 25, xlabtext = "7DADM temperature above dam")
  par(default.par)
  dev.off()
  
  png(paste0("CH_MCK_",alt,"_Pars2_",case,"_",Sys.Date(),".png"), height=16, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  par(mfrow=c(4,2),mai=c(0.5,0.3,0.4,0.05))
  plot_output(dat$rss_f_all,minlim = 0, maxlim = 1, xlabtext = "CGR-SUJ survival (fry)")
  plot_output(dat$rss_sr_all,minlim = 0, maxlim = 1, xlabtext = "CGR-SUJ survival (subyrlg-res)")
  plot_output(dat$rss_ss_all,minlim = 0, maxlim = 1, xlabtext = "CGR-SUJ survival (subyrlg-str)")
  plot_output(dat$rss_yrsw_all,minlim = 0, maxlim = 1, xlabtext = "CGR-SUJ survival (yrlg-resSW)")
  plot_output(dat$rss_yrw_all,minlim = 0, maxlim = 1, xlabtext = "CGR-SUJ survival (yrlg-resW)")
  plot_output(dat$rss_ys_all,minlim = 0, maxlim = 1, xlabtext = "CGR-SUJ survival (yrlg-str)")
  plot_output(dat$sa0plus3_all,minlim = 0, maxlim = 0.1, xlabtext = "Survival age0+_3")
  plot_output(dat$sa1plus3_all,minlim = 0, maxlim = 1, xlabtext = "Survival age1+_3")
  par(default.par)
  dev.off()
  
  
  ### DIVERSITY PERFORMANCE METRICS ###
  #calculate % composition of juvenile life histories and adult returns
  #uses geometric mean of last five years
  lhs_div_6 <- data.frame(Fry=apply(dat$F_suj_dat[,32:36],1,geoMean),
                          Sub_res=apply(dat$Sr_suj_dat[,32:36],1,geoMean),
                          Sub_str=apply(dat$Ss_suj_dat[,32:36],1,geoMean),
                          Yr_resSW=apply(dat$Yrsw_suj_dat[,32:36],1,geoMean),
                          Yr_resW=apply(dat$Yrw_suj_dat[,32:36],1,geoMean),
                          Yr_str=apply(dat$Ys_suj_dat[,32:36],1,geoMean))
  lhs_div_6_prop <- lhs_div_6 / rowSums(lhs_div_6)

  lhs_div_6_prop_stats <- sapply(1:6, function(i) get_summary_stats(lhs_div_6_prop[,i]))
  colnames(lhs_div_6_prop_stats) <- names(lhs_div_6_prop)

  lhr_div_6 <- data.frame(Fry=apply(dat$NO_F_rtn_dat[,32:36],1,geoMean),
                          Sub_res=apply(dat$NO_Sr_rtn_dat[,32:36],1,geoMean),
                          Sub_str=apply(dat$NO_Ss_rtn_dat[,32:36],1,geoMean),
                          Yr_resSW=apply(dat$NO_Yrsw_rtn_dat[,32:36],1,geoMean),
                          Yr_resW=apply(dat$NO_Yrw_rtn_dat[,32:36],1,geoMean),
                          Yr_str=apply(dat$NO_Ys_rtn_dat[,32:36],1,geoMean))
  lhr_div_6_prop <- lhr_div_6 / rowSums(lhr_div_6)

  lhr_div_6_prop_stats <- sapply(1:6, function(i) get_summary_stats(lhr_div_6_prop[,i]))
  colnames(lhr_div_6_prop_stats) <- names(lhr_div_6_prop)
  
   
  #Proportion life history diversity of smolts and adults returning to spawn
  png(paste0("CH_MCK_",alt,"_lhdiv6_",case,"_",Sys.Date(),".png"), height=16, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  par(mfcol=c(2,1),mai=c(0.4,0.65,0.1,0.05))
  boxplot(lhs_div_6_prop,outline=FALSE,ylim=c(0,1),ylab="Proportion of smolts at SUJ (yr 26-30)")
  boxplot(lhr_div_6_prop,outline=FALSE,ylim=c(0,1),ylab="Proportion of adult returns to WFF (yr 26-30)")
  par(default.par)
  dev.off()
  
  #life history relative adult return rates over final five years
  F_SAR <- dat$NO_F_rtn_dat[,32:36] / dat$F_suj_dat[,27:31]
  Sr_SAR <- dat$NO_Sr_rtn_dat[,32:36] / dat$Sr_suj_dat[,27:31]
  Ss_SAR <- dat$NO_Ss_rtn_dat[,32:36] / dat$Ss_suj_dat[,27:31]
  Yrsw_SAR <- dat$NO_Yrsw_rtn_dat[,32:36] / dat$Yrsw_suj_dat[,27:31]
  Yrw_SAR <- dat$NO_Yrw_rtn_dat[,32:36] / dat$Yrw_suj_dat[,27:31]
  Ys_SAR <- dat$NO_Ys_rtn_dat[,32:36] / dat$Ys_suj_dat[,27:31]
  
  lh_SAR <- data.frame(Fry=apply(F_SAR,1,mean),
                       Sub_res=apply(Sr_SAR,1,mean),
                       Sub_str=apply(Ss_SAR,1,mean),
                       Yr_resSW=apply(Yrsw_SAR,1,mean),
                       Yr_resW=apply(Yrw_SAR,1,mean),
                       Yr_str=apply(Ys_SAR,1,mean))
  
  lh_SAR_stats <- sapply(1:6, function(i) get_summary_stats(lh_SAR[,i]))
  colnames(lh_SAR_stats) <- names(lh_SAR)
  
  
  png(paste0("CH_MCK_",alt,"_lh_SAR_",case,"_",Sys.Date(),".png"), height=16, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  boxplot(lh_SAR,outline=FALSE,ylab="SAR (mean of yr 26-30)")
  par(default.par)
  dev.off()
  
  #mean of pHOS over final five years of projections (not using geometric mean as can have zero values...)
  pHOS_mean <- apply(dat$pHOS_dat[,32:36],1,mean)
  pHOS_stats <- get_summary_stats(pHOS_mean)
  
  tmp_breaks <- seq(0,1,0.05)
  png(paste0("CH_MCK_",alt,"_pHOS_mean_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  hist(pHOS_mean,freq=FALSE,breaks=tmp_breaks,xlim=c(0,1),
       xlab="pHOS (mean of yr 26-30)",main=alt)
  tmp <- as.numeric(round(pHOS_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  dev.off()
  
  #calculate annual pHOS statistics (remove generation zero, yr0-5)
  pHOS_dat <- dat$pHOS_dat[,-(1:6)]
  pHOS_median <- apply(pHOS_dat,2,median)
  pHOS_mean <- apply(pHOS_dat,2,mean)
  pHOS_l95 <- sapply(1:ncol(pHOS_dat),function(i) quantile(pHOS_dat[,i],probs=0.025))
  pHOS_u95 <- sapply(1:ncol(pHOS_dat),function(i) quantile(pHOS_dat[,i],probs=0.975))
  
  png(paste0("CH_MCK_",alt,"_pHOS_ts_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  plot(pHOS_median,type="l",lwd=2,xaxt="n",ylim=c(0,1),xlab="Year",ylab="pHOS",main=alt)
  axis(1,at=c(0,5,10,15,20,25,30),labels=c(0,5,10,15,20,25,30))
  lines(pHOS_l95,lty=2)
  lines(pHOS_u95,lty=2)
  dev.off()
  
  
  ### PRODUCTIVITY PERFORMANCE METRICS ###
  #recruits per spawner = geometric mean of first five years after first generation removed (yr 6-10)
  r_ts <- dat$NO_spwn_dat[,12:16]
  s_ts <- dat$NO_spwn_dat[,7:11] #element 1 is yr0
  r_per_s <- r_ts/s_ts
  r_per_s_geomean <- apply(r_per_s,1,geoMean)
  
  r_per_s_geomean_stats <- get_summary_stats(r_per_s_geomean)
  
  tmp_breaks <- seq(0,max(r_per_s_geomean)+0.1,0.1)
  png(paste0("CH_MCK_",alt,"_mean_R-S_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  hist(r_per_s_geomean,freq=FALSE,breaks=tmp_breaks,xlim=c(0,quantile(r_per_s_geomean,probs=1)), #0.99)),
       xlab="R/S (geometric mean of yr 1-5)",main=alt)
  tmp <- as.numeric(round(r_per_s_geomean_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  dev.off()

  #SAR = ratio of the number of NOR adults after terminal harvest (e.g. at WFF) to the number of outgoing juvenile
  #Chinook salmon at SUJ five years previously, first five years after generation zero burnin (yr 6-10)
  #track brood years, i.e. yearlings smolt at t+1 (needs refining)
  smolts <- dat$F_suj_dat[,7:11] + dat$Sr_suj_dat[,7:11] + dat$Ss_suj_dat[,7:11] + 
              dat$Yrsw_suj_dat[,8:12] + dat$Yrw_suj_dat[,8:12] + dat$Ys_suj_dat[,8:12]
  adults <- dat$NO_F_rtn_dat[,12:16] + dat$NO_Sr_rtn_dat[,12:16] + dat$NO_Ss_rtn_dat[,12:16] + 
              dat$NO_Yrsw_rtn_dat[,12:16] + dat$NO_Yrw_rtn_dat[,12:16] + dat$NO_Ys_rtn_dat[,12:16]
  SAR <- adults/smolts
  SAR_mean <- apply(SAR,1,mean)
  SAR_mean_stats <- get_summary_stats(SAR_mean)
  
  tmp_breaks <- seq(0,max(SAR_mean)+0.01,0.01)
  png(paste0("CH_MCK_",alt,"_mean_SAR_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  hist(SAR_mean,freq=FALSE,breaks=tmp_breaks,xlim=c(0,quantile(SAR_mean,probs=1)), #0.99)),
       xlab="SAR (mean of yr 1-5)",main=alt)
  tmp <- as.numeric(round(SAR_mean_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  dev.off()
  
  #fry-smolt survival (by life history type), first five years after generation zero burnin (yr 6-10)
  lh_f2s <- data.frame(Fry=apply(dat$F_f2s_dat[,7:11],1,mean),
                       Sub_res=apply(dat$Sr_f2s_dat[,7:11],1,mean),
                       Sub_str=apply(dat$Ss_f2s_dat[,7:11],1,mean),
                       Yr_resSW=apply(dat$Yrsw_f2s_dat[,7:11],1,mean),
                       Yr_resW=apply(dat$Yrw_f2s_dat[,7:11],1,mean),
                       Yr_str=apply(dat$Ys_f2s_dat[,7:11],1,mean))
  lh_f2s_stats <- sapply(1:6, function(i) get_summary_stats(lh_f2s[,i]))
  colnames(lh_f2s_stats) <- names(lh_f2s)
  
  png(paste0("CH_MCK_",alt,"_lh_fry-smolt_",case,"_",Sys.Date(),".png"), height=16, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  boxplot(lh_f2s,outline=FALSE,ylab="Fry-smolt survival (mean of yr 1-5)")
  par(default.par)
  dev.off()
  
  
  ### ABUNDANCE PERFORMANCE METRICS ###
  #calculate annual spawner statistics (remove generation zero, yr0-5)
  NO_spwn_dat <- dat$NO_spwn_dat[,-(1:6)]
  NO_spwn_median <- apply(NO_spwn_dat,2,median)
  NO_spwn_mean <- apply(NO_spwn_dat,2,mean)
  NO_spwn_l95 <- sapply(1:ncol(NO_spwn_dat),function(i) quantile(NO_spwn_dat[,i],probs=0.025))
  NO_spwn_u95 <- sapply(1:ncol(NO_spwn_dat),function(i) quantile(NO_spwn_dat[,i],probs=0.975))
  
  png(paste0("CH_MCK_",alt,"_NO_spawners_ts_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  plot(NO_spwn_median,type="l",lwd=2,xaxt="n",ylim=c(0,max(NO_spwn_u95)),xlab="Year",ylab="NOR spawners (post-PSM)",main=alt)
  axis(1,at=c(0,5,10,15,20,25,30),labels=c(0,5,10,15,20,25,30))
  lines(NO_spwn_l95,lty=2)
  lines(NO_spwn_u95,lty=2)
  dev.off()
  
  #gemometric mean abundance of NOR spawners above dam (post PSM, before outplant cap), years 21-35 (final 15 years)
  NO_spwn_dat <- dat$NO_spwn_dat[,22:36]
  NO_spwn_geomean <- apply(NO_spwn_dat,1,geoMean)
  NO_spwn_geomean_stats <- get_summary_stats(NO_spwn_geomean)
  NO_spwn_geomean_overall <- geoMean(NO_spwn_geomean)
  
  png(paste0("CH_MCK_",alt,"_geomean_NOR_spawners_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  hist(NO_spwn_geomean,freq=FALSE,breaks=20,xlim=c(0,quantile(NO_spwn_geomean,probs=1)), #0.999)),
       xlab="NOR spawners (post-PSM, geometric mean of yr 16-30)",main=alt)
  tmp <- as.numeric(round(NO_spwn_geomean_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  dev.off()
  
  
  ### EXTINCTION RISK PERFORMANCE METRICS ###

  ##!!!### 
  NO_crit <- 167 # 250 #McKenzie Santiam critical abundance threshold = 250, prorated
  NO_spwn_4yr_mean <- vector()
  NO_spwn_below_crit <- vector()
  #tmp <- vector()
  P_QET <- vector()
  
  for(j in 1:nrow(dat$NO_spwn_dat)){
    for(i in 22:36){
      NO_spwn_4yr_mean[i] <- mean(dat$NO_spwn_dat[j,][(i-3):i]) #4-year average to year t
      NO_spwn_below_crit[i] <- ifelse(NO_spwn_4yr_mean[i]<NO_crit,1,0) #do average spawner numbers fall below threshold?
    }
    #tmp[j] <- sum(NO_spwn_below_crit,na.rm=TRUE)
    P_QET[j] <- ifelse(sum(NO_spwn_below_crit,na.rm=TRUE)>0,1,0) #population extinct if in any year numbers below threshold
  }
  #mean(P_QET) #mean probability of extinction across simulations
  P_QET_stats <- get_summary_stats(P_QET)
  
  P_QET_01 <- factor(P_QET,levels=c(0,1))
  P_QET_01 <- table(P_QET_01)/sum(table(P_QET_01))
  
  png(paste0("CH_MCK_",alt,"_P-QET_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  barplot(P_QET_01,xlab="Prob < Quasi-Extinction Threshold = 167 (4-yr mean, yr 16-30)",ylab="Proportion of simulations",main=alt)
  tmp <- as.numeric(round(P_QET_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  dev.off()
  
  
  ### DAM PASSAGE JUVENILE SURVIVAL PERFORMANCE METRICS ###
  png(paste0("CH_MCK_",alt,"_DPE-DPS_CGR_",case,"_",Sys.Date(),".png"), height=12, width=16, units="cm", res=300, pointsize=10) # antialias="cleartype")
  par(mfcol=c(3,2),mai=c(0.5,0.3,0.4,0.05))
  plot_output(dat$DPE_F_all,minlim = 0, maxlim = 1, xlabtext = "DPE (fry)")
  plot_output(dat$DPE_S_all,minlim = 0, maxlim = 1, xlabtext = "DPE (subyearlings)")
  plot_output(dat$DPE_Y_all,minlim = 0, maxlim = 1, xlabtext = "DPE (yearlings)")
  plot_output(dat$DPS_F_all,minlim = 0, maxlim = 1, xlabtext = "DPS (fry)")
  plot_output(dat$DPS_S_all,minlim = 0, maxlim = 1, xlabtext = "DPS (subyearlings)")
  plot_output(dat$DPS_Y_all,minlim = 0, maxlim = 1, xlabtext = "DPS (yearlings)")
  par(default.par)
  dev.off()
  
  
  NO_spwn_geomean_table <- data.frame(Life_history="All",Alternative=alt,PM="NOR (post-PSM)",t(NO_spwn_geomean_stats))
  r_per_s_geomean_table <- data.frame(Life_history="All",Alternative=alt,PM="R/S",t(r_per_s_geomean_stats))
  SAR_mean_table <- data.frame(Life_history="All",Alternative=alt,PM="SAR",t(SAR_mean_stats))
  pHOS_table <- data.frame(Life_history="All",Alternative=alt,PM="pHOS",t(pHOS_stats))
  P_QET_table <- data.frame(Life_history="All",Alternative=alt,PM="P<QET",t(P_QET_stats))
  lhs_div_6_prop_table <- cbind(Life_history=colnames(lhs_div_6_prop_stats),Alternative=rep(alt,6),PM=rep("Prop. smolt SUJ",6),
                                as.data.frame(t(lhs_div_6_prop_stats)))
  lhr_div_6_prop_table <- cbind(Life_history=colnames(lhr_div_6_prop_stats),Alternative=rep(alt,6),PM=rep("Prop. adult WFF",6),
                                as.data.frame(t(lhr_div_6_prop_stats)))
  lh_f2s_table <- cbind(Life_history=colnames(lh_f2s_stats),Alternative=rep(alt,6),PM=rep("fry-smolt survival",6),
                        as.data.frame(t(lh_f2s_stats)))
  lh_SAR_table <- cbind(Life_history=colnames(lh_SAR_stats),Alternative=rep(alt,6),PM=rep("SAR",6),
                                as.data.frame(t(lh_SAR_stats)))
  
  pm_table <- rbind(NO_spwn_geomean_table,
                    r_per_s_geomean_table,
                    SAR_mean_table,
                    pHOS_table,
                    P_QET_table,
                    lhs_div_6_prop_table,
                    lhr_div_6_prop_table,
                    lh_f2s_table,
                    lh_SAR_table)
  colnames(pm_table) <- c("Life history","Alternative","PM","Mean","SD","CV","2.5%","Median","97.5%")
  
  
  ### COMBINED PLOTS AND PM TABLE###
  # These are global, so pop back to global results directory
  setwd(save.dir)
  #save into PDF
  pdf(paste0("CH_MCK_",alt,"_combined_perf_metrics_",case,"_",Sys.Date(),".pdf"),paper="letter",width=7.5,height=10,font=NULL,pointsize=11)
  par(mfrow=c(3,2),mai=c(0.7,0.7,0.4,0.1))
  #returns
  plot(NO_rtn_median,type="l",lwd=2,xaxt="n",ylim=c(0,max(NO_rtn_u95)),xlab="Year",ylab="NOR returns to Cougar",main=alt)
  lines(NO_rtn_l95,lty=2)
  lines(NO_rtn_u95,lty=2)
  if(alt=="NAA") lines(rep(NOR_obs_mean,length(NO_rtn_median)),lty=1,col="red")
  if(alt=="NAA") lines(rep(NOR_obs_l95,length(NO_rtn_median)),lty=2,col="red")
  if(alt=="NAA") lines(rep(NOR_obs_u95,length(NO_rtn_median)),lty=2,col="red")
  if(alt=="NAA") legend("topleft",c("observed","model"),lty=1,col=c("red","black"),bty="n")
  axis(1,at=c(0,5,10,15,20,25,30),labels=c(0,5,10,15,20,25,30))
  #spawners
  hist(NO_spwn_geomean,freq=FALSE,breaks=20,xlim=c(0,quantile(NO_spwn_geomean,probs=0.999)),
       xlab="NOR spawners (post-PSM, geometric mean of yr 16-30)",main=NA)
  tmp <- as.numeric(round(NO_spwn_geomean_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  #R/S
  tmp_breaks <- seq(0,max(r_per_s_geomean)+0.1,0.1)
  hist(r_per_s_geomean,freq=FALSE,breaks=tmp_breaks,xlim=c(0,quantile(r_per_s_geomean,probs=1)), #0.99)),
       xlab="R/S (geometric mean of yr 1-5)",main=NA)
  tmp <- as.numeric(round(r_per_s_geomean_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  #SAR
  tmp_breaks <- seq(0,max(SAR_mean)+0.01,0.01)
  hist(SAR_mean,freq=FALSE,breaks=tmp_breaks,xlim=c(0,quantile(SAR_mean,probs=1)), # 0.99)),
       xlab="SAR (mean of yr 1-5)",main=NA)
  tmp <- as.numeric(round(SAR_mean_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  #P-QET
  barplot(P_QET_01,xlab="Prob < Quasi-Extinction Threshold = 167 (4-yr mean, yr 16-30)",ylab="Proportion of simulations",main=NA)
  tmp <- as.numeric(round(P_QET_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)
  #pHOS
  tmp_breaks <- seq(0,1,0.05)
  hist(pHOS_mean,freq=FALSE,breaks=tmp_breaks,xlim=c(0,1),
       xlab="pHOS (mean of yr 26-30)",main=NA)
  tmp <- as.numeric(round(pHOS_stats,3))
  isummary <- paste0("mean=",tmp[1]," sd=",tmp[2]," cv=",tmp[3],"\n",
                     " 2.5%=",tmp[4]," median=",tmp[5], " 97.5%=", tmp[6])
  mtext(isummary, side = 3, cex=.75)

  par(mfrow=c(3,2),mai=c(0.7,0.7,0.4,0.1))
  #life history smolts
  boxplot(lhs_div_6_prop,ylim=c(0,1),outline=FALSE,
          ylab="Proportion of smolts at SUJ (yr 26-30)",main=paste0("Smolts by life history under ",alt),las=3)
  #life history adults
  boxplot(lhr_div_6_prop,ylim=c(0,1),outline=FALSE,
          ylab="Proportion of adult returns to WFF (yr 26-30)",main="Adult returns by life history",las=3)
  #life history fry-smolt survival
  boxplot(lh_f2s,outline=FALSE,
    ylab="Fry-smolt survival (mean of yr 1-5)",main="Fry-smolt survival by life history",las=3)
  #life history SAR
  boxplot(lh_SAR,outline=FALSE,
    ylab="SAR (mean of yr 26-30)",main="Relative adult return rate by life history",las=3)
  
  ## plot table
  grid.newpage()
  grid.table(pm_table,rows=NULL,theme=ttheme_default(base_size=10))
  
  par(default.par)
  dev.off()
  
  
  write.csv(pm_table,paste0("CH_MCK_",alt,"_Perf_Metrics_",case,"_",Sys.Date(),".csv"),quote=FALSE,row.names=FALSE)
  
  
  sink(paste0("CH_MCK_",alt,"_Perf_Metrics_",case,"_",Sys.Date(),".txt"))
  
    cat(paste0(" dam passage evaluations: ",alt," (",prettyNum(n.sim,big.mark=",")," simulations)\n"))
    cat("\nNO4/NO0 stats\n")
    print(NO4_NO0_stats)
    
    cat("\n\nDIVERSITY PERFORMANCE METRICS\n")
    cat("\nLife history (6 groups) diversity: proportion of smolts at SUJ (geometric mean of post-burnin years 26-30)\n")
    print(t(lhs_div_6_prop_stats))
    cat("\nLife history (6 groups) diversity: proportion of adults at WFF (geometric mean of post-burnin years 26-30)\n")
    print(t(lhs_div_6_prop_stats))
    cat("\nLife history (6 groups) diversity: relative adult return rates (mean of post-burnin years 26-30)\n")
    print(t(lh_SAR_stats))
    cat("\npHOS (mean of post-burnin years 26-30)\n")
    print(pHOS_stats)
    
    cat("\n\nPRODUCTIVITY PERFORMANCE METRICS\n")
    cat("\nR/S (geometric mean of post-burnin years 1-5)\n")
    print(r_per_s_geomean_stats)
    cat("\nSAR (mean of post-burnin years 1-5)\n")
    print(SAR_mean_stats)
    cat("\nFry-smolt survival (at SUJ) by life history (mean of post-burnin years 1-5)\n")
    print(t(lh_f2s_stats))
    
    cat("\n\nABUNDANCE PERFORMANCE METRICS\n")
    cat("\nNOR spawners, post-PSM (geometric mean of post-burnin years 16-30)\n")
    print(NO_spwn_geomean_stats)
    cat("\nNOR spawners, post-PSM (geometric mean of all projections for post-burnin years 16-30)\n")
    cat(NO_spwn_geomean_overall)
    
    cat("\n\n\nPROBABILITY < QUASI-EXTINCTION THRESHOLD PERFORMANCE METRIC\n")
    print(P_QET_stats)
    
  sink()
  
  
  save.image(paste0("CH_MCK_",alt,"_",case,"_",Sys.Date(),".RData"))
  
  setwd(file.dir)
}

#get_perf_metrics("NAA","base-1200",10000)
#break

alts <- c("NAA","Alt1","Alt2a","Alt2b","Alt3a","Alt3b","Alt4")
case <- "base"
#case <- "base-bowerman-no-ranef"

sapply(1:length(alts), function(i) get_perf_metrics(alts[i],case,10000))

# After this, run the table modification script
