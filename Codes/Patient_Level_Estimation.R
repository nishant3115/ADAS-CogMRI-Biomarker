###############################################################################
# Code Description: Patient-level estimation of baseline cognitive impairment 
# and progression rates
###############################################################################
# install packages (if needed)
if ("MCMCglmm" %in% rownames(installed.packages())==0){
  install.packages('MCMCglmm')}
if ("lme4" %in% rownames(installed.packages())==0){install.packages('lme4')}
if ("ggplot2" %in% rownames(installed.packages())==0){
  install.packages('ggplot2')}
if ("caret" %in% rownames(installed.packages())==0){install.packages('caret')}
if ("pROC" %in% rownames(installed.packages())==0){install.packages('pROC')}

# Load the required libraries
library(MCMCglmm)
library(lme4)
library(ggplot2)
library(caret)
library(pROC)

# Load the longitudinal data
load(file = '../RData/Longitudinal_ADAS_MRI_Data.RData')
load(file = '..//RData/ADNI_FreeSufer_Longitudinal_Data.RData')
# Load Mplus parameter data
load(file = '../RData/Mplus_MRI_ADAS_Parameters.RData')
# Load patient covariates
load(file = '../RData/ADNI_Patient_Covariates.RData')

# Enter diagnostic information
FS_ADAS_Data$Visit_Diagnosis <- 
  ADNI_FS_Long_Data$Visit_Diagnosis[
    match(interaction(FS_ADAS_Data$RID,FS_ADAS_Data$VISCODE2),
          interaction(ADNI_FS_Long_Data$RID, ADNI_FS_Long_Data$VISCODE2))]

# Find RID of patients diagnosed as MCI at baseline
MCI_RIDs <- unique(FS_ADAS_Data$RID[which(FS_ADAS_Data$Visit_Diagnosis=='MCI' &
                                            FS_ADAS_Data$VISCODE2=='sc')])
MCI_Label <- mat.or.vec(length(MCI_RIDs),1) + NA
# Label them as progressors (P) or non-progressors (NP)
for(rid in 1:length(MCI_RIDs)){
  ind <- which(FS_ADAS_Data$RID==MCI_RIDs[rid])
  if(length(unique(FS_ADAS_Data$Visit_Diagnosis[ind]))==1){ 
    MCI_Label[rid] <- 'NP' }
  if(length(unique(FS_ADAS_Data$Visit_Diagnosis[ind]))>1){ 
    MCI_Label[rid] <- 'P' }}

# Get the longitudinal MCI data
MCI_Data <- FS_ADAS_Data[FS_ADAS_Data$RID%in%MCI_RIDs,]
# Enter P vs NP as ARM data in the model
MCI_Data$ARM <- MCI_Label[match(MCI_Data$RID, MCI_RIDs)]

# Prepare data for modeling with MCMCglmm package
Data <- MCI_Data
# Select patients with ADAS between 15-35 (mild to moderate Alzheimer's disease)
Q2_SCORE <- rowSums(Data[,35:46])
Q2_SCORE[Q2_SCORE<=2] <- 0
Q2_SCORE[Q2_SCORE<=5 & Q2_SCORE>=3] <- 1
Q2_SCORE[Q2_SCORE<=8 & Q2_SCORE>=6] <- 2
Q2_SCORE[Q2_SCORE<=11 & Q2_SCORE>=9] <- 3
Q2_SCORE[Q2_SCORE<=14 & Q2_SCORE>=12] <- 4
Q2_SCORE[Q2_SCORE<=17 & Q2_SCORE>=15] <- 5
Q3_SCORE <- rowSums(Data[,47:49])
Q4_SCORE <- rowSums(Data[,50:52])
Q5_SCORE <- rowSums(Data[,53:55])
Q6_SCORE <- rowSums(Data[,56:62])
Data$ADAS70 <- Q2_SCORE + Q3_SCORE + Q4_SCORE+ Q5_SCORE + Q6_SCORE + 
  rowSums(Data[,c(34,63:67)])

# change NA in slope matrix as 0
MRI_ADAS_F3_Slopes[which(is.na(MRI_ADAS_F3_Slopes))] <- 0

# convert VISCODE in data from categorical to numeric in years (baseline = 0)
Data$Visits <- Data$VISCODE2
Data$Visits[Data$VISCODE=='sc'] <- 0
Data$Visits[Data$VISCODE=='m03'] <- 3
Data$Visits[Data$VISCODE=='m06'] <- 6
Data$Visits[Data$VISCODE=='m12'] <- 12
Data$Visits[Data$VISCODE=='m18'] <- 18
Data$Visits[Data$VISCODE=='m24'] <- 24
Data$Visits[Data$VISCODE=='m36'] <- 36
Data$Visits[Data$VISCODE=='m48'] <- 48
Data$Visits[Data$VISCODE=='m60'] <- 60
Data$Visits <- as.numeric(Data$Visits)/12

# convert the first follow-up visit to baseline (visit=0)
uniq_RID <- unique(Data$RID)
for(rid in 1:length(uniq_RID)){
  ind <- which(Data$RID==uniq_RID[rid])
  pat_visits <- Data$Visits[ind]
  if(min(pat_visits)>0){    pat_visits <- pat_visits - min(pat_visits)  }
  Data$Visits[ind] <- pat_visits }

# Remove NA rows
na.ind <- which(is.na(Data), arr.ind = TRUE)
Data <- Data[-1*unique(na.ind[,1]),]

# Sample patients with normally distributed baseline ADAS70 scores 
# (mean = 10, sd = 5)
BL_Data <- Data[which(Data$Visits==0),]

# select P patients
pb = exp(-1*((BL_Data$ADAS70[which(BL_Data$ARM=='P')]-10)^2)/2*1)/sqrt(2*pi*1)
pb <- pb/sum(pb)
P_ARM_patients <- sample(BL_Data$RID[which(BL_Data$ARM=='P')], size = 100, 
                         replace = FALSE, prob = pb)

# select NP patients
pb = exp(-1*((BL_Data$ADAS70[which(BL_Data$ARM=='NP')]-10)^2)/2*1)/sqrt(2*pi*1)
pb <- pb/sum(pb)
NP_ARM_patients <- sample(BL_Data$RID[which(BL_Data$ARM=='NP')], size = 100, 
                          replace = FALSE, prob = pb)

Data <- Data[which(Data$RID%in%c(P_ARM_patients, NP_ARM_patients)),]

# compile data
Mem_Slopes <- c(); Lang_Slopes <- c(); Prax_Slopes <- c(); 
Item_Intercept <- c(); Item_Responses <- c();
MRI_Indicator <- c(); Item_family <- c(); Item_trait <- c(); Item_trait2 <- c();
Patient_RID <- c(); Patient_Gender <- c(); Patient_Age <- c();
Patient_APOE <- c(); Patient_EDU <- c();
Patient_CM <- c(); Patient_Visits <- c(); Patient_Arm <- c();
Item_Model <- c(3:68)  # remove Q213 
Item_names <- colnames(Data)[Item_Model]
for(it in 1:length(Item_Model)){
  item <- Item_Model[it]
  if(it!=40){ # Not for Q213 item
    if(item<=33){  # continous MRI variables
      Item_Responses <- 
        c(Item_Responses, (Data[,item]- Mean_MRI_Measurements[it])/
            SD_MRI_Measurements[it])  
      Mem_Slopes <- c(Mem_Slopes, rep(MRI_ADAS_F3_Slopes[it, 1], dim(Data)[1]))
      Lang_Slopes <- c(Lang_Slopes, rep(MRI_ADAS_F3_Slopes[it, 2], 
                                        dim(Data)[1]))
      Prax_Slopes <- c(Prax_Slopes, rep(MRI_ADAS_F3_Slopes[it, 3], 
                                        dim(Data)[1]))
      Item_family <- c(Item_family, rep('gaussian', dim(Data)[1]))
      Item_trait <- c(Item_trait, rep(Item_names[it], dim(Data)[1]))
      Item_trait2 <- c(Item_trait2, rep(Item_names[it], dim(Data)[1]))
      Item_Intercept <- c(Item_Intercept, rep(MRI_ADAS_F3_Intercepts[it,1], 
                                              dim(Data)[1]))  
      Patient_RID <- c(Patient_RID, Data$RID)
      MRI_Indicator <- c(MRI_Indicator, rep('MRI', dim(Data)[1]))
      Patient_Gender <- c(Patient_Gender, Data$Gender)
      Patient_EDU <- c(Patient_EDU, Data$Education)
      Patient_Age <- c(Patient_Age, Data$Age)
      Patient_APOE <- c(Patient_APOE, Data$APOE)
      Patient_CM <- c(Patient_CM, Data$CM_Status)
      Patient_Visits <- c(Patient_Visits, Data$Visits)
      Patient_Arm <- c(Patient_Arm, (Data$ARM=='P')*1)
    }
    if(item>33){ # categorical variables of ADAS-cog
      uniq_item <- sort(unique(Data[,item])) # unique score categories of item
      for(cat in 2:length(uniq_item)){      # categories parameters are modeled
        Item_Responses <- c(Item_Responses, (Data[,item]>=uniq_item[cat]))  
        Mem_Slopes <- c(Mem_Slopes, rep(MRI_ADAS_F3_Slopes[it, 1], 
                                        dim(Data)[1]))
        Lang_Slopes <- c(Lang_Slopes, rep(MRI_ADAS_F3_Slopes[it, 2], 
                                          dim(Data)[1]))
        Prax_Slopes <- c(Prax_Slopes, rep(MRI_ADAS_F3_Slopes[it, 3], 
                                          dim(Data)[1]))
        Item_family <- c(Item_family, rep('categorical', dim(Data)[1]))
        Item_trait <- c(Item_trait, rep(Item_names[it], dim(Data)[1]))
        Item_trait2 <- c(Item_trait2, rep('ZADAS', dim(Data)[1]))      
        Item_Intercept <- 
          c(Item_Intercept, rep(MRI_ADAS_F3_Intercepts[it, (cat-1)], 
                                dim(Data)[1]))
        Patient_RID <- c(Patient_RID, Data$RID)
        MRI_Indicator <- c(MRI_Indicator, rep('NADAS', dim(Data)[1]))
        Patient_Gender <- c(Patient_Gender, Data$Gender)
        Patient_EDU <- c(Patient_EDU, Data$Education)
        Patient_Age <- c(Patient_Age, Data$Age)
        Patient_APOE <- c(Patient_APOE, Data$APOE)
        Patient_CM <- c(Patient_CM, Data$CM_Status)
        Patient_Visits <- c(Patient_Visits, Data$Visits)   
        Patient_Arm <- c(Patient_Arm, (Data$ARM=='P')*1)
      }}}}

Item_trait <- as.factor(Item_trait)
MRI_Indicator <- as.factor(MRI_Indicator)
Patient_Gender <- Patient_Gender - 1  # 1= female, 0 = male
Patient_RID <- as.factor(Patient_RID)
Item_trait2 <- as.factor(Item_trait2)
mu_Age <- mean(Patient_Age, na.rm = TRUE)
sd_Age <- sd(Patient_Age, na.rm = TRUE)
Patient_Age <- (Patient_Age- mean(Patient_Age, na.rm = TRUE))/
  sd(Patient_Age, na.rm = TRUE)
Patient_EDU <- (Patient_EDU - mean(Patient_EDU, na.rm = TRUE))/
  sd(Patient_EDU, na.rm = TRUE)

# combine into a dataframe
MRI_ADAS_MCMCglmm_Long_Data <- 
  data.frame(Item_Responses, Mem_Slopes, Lang_Slopes, Prax_Slopes, 
             family = Item_family, trait = Item_trait, Item_trait, 
             Item_Intercept, 
             Patient_RID, ItemID = Item_trait, MRI_Indicator,
             Patient_Gender, Patient_EDU, Patient_APOE, Patient_Age,
             Patient_CM, Patient_Visits, Item_trait2,
             Mem_Rate = Mem_Slopes*Patient_Visits,
             Lang_Rate = Lang_Slopes*Patient_Visits,
             Prax_Rate = Prax_Slopes*Patient_Visits,
             Patient_Arm)

#------------------------------------------------------------------------------
# --------------------------ADAS-CogMRI biomarker----------------------------
#------------------------------------------------------------------------------
# Define prior
Vfix <- diag(82)*1000; Vfix[1,1] <- 1e-10
MCMC_prior_MRI <- 
  list(B = list(mu = c(1,rep(0,81)), V = Vfix), 
       G = list(G1 = list(V=diag(6), nu = 7, alpha.mu = rep(0,6), 
                          alpha.V = diag(6)*1000), 
                G2 = list(V=diag(31), nu = 50)),
       R = list(V = diag(32), nu = 33, fix = 32))

rand_form <- ~ us(Mem_Slopes+Lang_Slopes+Prax_Slopes +
                    Mem_Rate+Lang_Rate+Prax_Rate):Patient_RID +
  us(at.level(Item_trait,'Bankssts_TA')+
       at.level(Item_trait,'CaudalMiddleFrontal_TA')+
       at.level(Item_trait,'Cuneus_TA')+
       at.level(Item_trait,'Entorhinal_TA')+
       at.level(Item_trait,'FrontalPole_TA')+
       at.level(Item_trait,'Fusiform_TA')+
       at.level(Item_trait,'InferiorParietal_TA')+
       at.level(Item_trait,'InferiorTemporal_TA')+
       at.level(Item_trait,'LateralOccipital_TA')+
       at.level(Item_trait,'LateralOrbitofrontal_TA')+
       at.level(Item_trait,'Lingual_TA')+
       at.level(Item_trait,'MedialOrbitofrontal_TA')+
       at.level(Item_trait,'MiddleTemporal_TA')+
       at.level(Item_trait,'Paracentral_TA')+
       at.level(Item_trait,'Parahippocampal_TA')+
       at.level(Item_trait,'ParsOpercularis_TA')+
       at.level(Item_trait,'ParsOrbitalis_TA')+
       at.level(Item_trait,'ParsTriangularis_TA')+
       at.level(Item_trait,'Pericalcarine_TA')+
       at.level(Item_trait,'Postcentral_TA')+
       at.level(Item_trait,'Precentral_TA')+
       at.level(Item_trait,'Precuneus_TA')+
       at.level(Item_trait,'RostralMiddleFrontal_TA')+
       at.level(Item_trait,'SuperiorFrontal_TA')+
       at.level(Item_trait,'SuperiorParietal_TA')+
       at.level(Item_trait,'SuperiorTemporal_TA')+
       at.level(Item_trait,'Supramarginal_TA')+
       at.level(Item_trait,'TemporalPole_TA')+
       at.level(Item_trait,'Insula_TA')+
       at.level(Item_trait,'Amygdala_SV')+
       at.level(Item_trait,'Hippocampus_SV')):Patient_RID

MCMC_Long_MRI <- MCMCglmm(fixed = Item_Responses ~ -1 + Item_Intercept + 
                            Mem_Slopes + Lang_Slopes + Prax_Slopes + 
                            Mem_Rate + Lang_Rate + Prax_Rate + 
                            at.level(MRI_Indicator, 'MRI'):trait +
                            at.level(MRI_Indicator, 'MRI'):trait:Patient_Age +                         
                            I(Mem_Rate*Patient_Age) + 
                            I(Lang_Rate*Patient_Age) +
                            I(Prax_Rate*Patient_Age) + 
                            I(Mem_Slopes*Patient_APOE) +
                            I(Mem_Rate*Patient_APOE) + 
                            I(Lang_Rate*Patient_APOE) + 
                            I(Prax_Rate*Patient_APOE) + 
                            I(Mem_Slopes*Patient_Arm) + 
                            I(Lang_Slopes*Patient_Arm) + 
                            I(Prax_Slopes*Patient_Arm) +
                            I(Mem_Rate*Patient_Arm) + 
                            I(Lang_Rate*Patient_Arm) + 
                            I(Prax_Rate*Patient_Arm),
                          random= rand_form, rcov= ~ idh(Item_trait2):units,
                          family = NULL, prior = MCMC_prior_MRI, 
                          data = MRI_ADAS_MCMCglmm_Long_Data,
                          verbose = TRUE, slice = TRUE)
MCMC_MRI_Summary <- summary(MCMC_Long_MRI)

#------------------------------------------------------------------------------
# --------------------------ADAS-CogIRT methodology----------------------------
#------------------------------------------------------------------------------
Vfix <- diag(20)*1000; Vfix[1,1] <- 1e-10
MCMCprior_ADASCogIRT <- 
  list(B = list(mu = c(1,rep(0,19)), V = Vfix), 
       G = list(G1 = list(V=diag(6), nu = 7, alpha.mu = rep(0,6), 
                          alpha.V = diag(6)*1000)),
       R = list(V = 1, fix = 1))

rand_form_ADASCogIRT <- ~ us(Mem_Slopes + Lang_Slopes + Prax_Slopes +
                               Mem_Rate + Lang_Rate + Prax_Rate):Patient_RID

MCMC_Long_ADASCogIRT <- MCMCglmm(fixed = Item_Responses ~ -1 + Item_Intercept +
                                   Mem_Slopes + Lang_Slopes + 
                                   Prax_Slopes + Mem_Rate + Lang_Rate + 
                                   Prax_Rate + 
                                   I(Mem_Rate*Patient_Age) + 
                                   I(Lang_Rate*Patient_Age) + 
                                   I(Prax_Rate*Patient_Age) + 
                                   I(Mem_Slopes*Patient_APOE) +
                                   I(Mem_Rate*Patient_APOE) + 
                                   I(Lang_Rate*Patient_APOE) + 
                                   I(Prax_Rate*Patient_APOE) +
                                   I(Mem_Slopes*Patient_Arm) + 
                                   I(Lang_Slopes*Patient_Arm) + 
                                   I(Prax_Slopes*Patient_Arm) +
                                   I(Mem_Rate*Patient_Arm) + 
                                   I(Lang_Rate*Patient_Arm) + 
                                   I(Prax_Rate*Patient_Arm),
                                 random = rand_form_ADASCogIRT, 
                                 family = 'categorical',
                                 prior = MCMCprior_ADASCogIRT, rcov = ~ units,
                                 data = MRI_ADAS_MCMCglmm_Long_Data
          [which(MRI_ADAS_MCMCglmm_Long_Data$MRI_Indicator=='NADAS'),c(-5,-6)],
                                 verbose = TRUE, slice = TRUE)

MCMC_ADASCogIRT_Summary <- summary(MCMC_Long_ADASCogIRT)



























