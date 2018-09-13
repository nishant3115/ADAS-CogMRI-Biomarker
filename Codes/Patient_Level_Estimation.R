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

# MRI+ADAS biomarker
# Define prior
Vfix <- diag(82)*1000; Vfix[1,1] <- 1e-10
MCMC_prior_MRI <- list(B = list(mu = c(1,rep(0,81)), V = Vfix), 
                       G = list(G1 = list(V=diag(6), nu = 7, alpha.mu = rep(0,6), alpha.V = diag(6)*1000),
                                G2 = list(V=diag(31), nu = 50)),
                       R = list(V = diag(32), nu = 33, fix = 32))

rand_form <- ~ us(Mem_Slopes+Lang_Slopes+Prax_Slopes +
                    Mem_Rate+Lang_Rate+Prax_Rate):Patient_RID +
  us(at.level(Item_trait,'Bankssts_TA')+at.level(Item_trait,'CaudalMiddleFrontal_TA')+
       at.level(Item_trait,'Cuneus_TA')+at.level(Item_trait,'Entorhinal_TA')+
       at.level(Item_trait,'FrontalPole_TA')+at.level(Item_trait,'Fusiform_TA')+
       at.level(Item_trait,'InferiorParietal_TA')+at.level(Item_trait,'InferiorTemporal_TA')+
       at.level(Item_trait,'LateralOccipital_TA')+at.level(Item_trait,'LateralOrbitofrontal_TA')+
       at.level(Item_trait,'Lingual_TA')+at.level(Item_trait,'MedialOrbitofrontal_TA')+
       at.level(Item_trait,'MiddleTemporal_TA')+at.level(Item_trait,'Paracentral_TA')+
       at.level(Item_trait,'Parahippocampal_TA')+at.level(Item_trait,'ParsOpercularis_TA')+
       at.level(Item_trait,'ParsOrbitalis_TA')+at.level(Item_trait,'ParsTriangularis_TA')+
       at.level(Item_trait,'Pericalcarine_TA')+at.level(Item_trait,'Postcentral_TA')+
       at.level(Item_trait,'Precentral_TA')+at.level(Item_trait,'Precuneus_TA')+
       at.level(Item_trait,'RostralMiddleFrontal_TA')+at.level(Item_trait,'SuperiorFrontal_TA')+
       at.level(Item_trait,'SuperiorParietal_TA')+at.level(Item_trait,'SuperiorTemporal_TA')+
       at.level(Item_trait,'Supramarginal_TA')+at.level(Item_trait,'TemporalPole_TA')+
       at.level(Item_trait,'Insula_TA')+at.level(Item_trait,'Amygdala_SV')+
       at.level(Item_trait,'Hippocampus_SV')):Patient_RID

MCMC_Long_MRI <- MCMCglmm(fixed = Item_Responses ~ -1 + Item_Intercept + 
                            Mem_Slopes + Lang_Slopes + Prax_Slopes + 
                            Mem_Rate + Lang_Rate + Prax_Rate + 
                            at.level(MRI_Indicator, 'MRI'):trait +
                            at.level(MRI_Indicator, 'MRI'):trait:Patient_Age +                         
                            I(Mem_Rate*Patient_Age) + I(Lang_Rate*Patient_Age) + I(Prax_Rate*Patient_Age) + 
                            I(Mem_Slopes*Patient_APOE) +
                            I(Mem_Rate*Patient_APOE) + I(Lang_Rate*Patient_APOE) + I(Prax_Rate*Patient_APOE) + 
                            I(Mem_Slopes*Patient_Arm) + I(Lang_Slopes*Patient_Arm) + I(Prax_Slopes*Patient_Arm) +
                            I(Mem_Rate*Patient_Arm) + I(Lang_Rate*Patient_Arm) + I(Prax_Rate*Patient_Arm),
                          random= rand_form, rcov= ~ idh(Item_trait2):units,
                          family = NULL, prior = MCMC_prior_MRI, 
                          data = MRI_ADAS_MCMCglmm_Long_Data,
                          verbose = TRUE, slice = TRUE)













# for very visit, calculate 
for(vis in 1:length(Uniq_visit)){
  MCMC_Long_MCI <- MCMCglmm(fixed = Item_Responses ~ -1 + Item_Intercept + 
                            Mem_Slopes + Lang_Slopes + Prax_Slopes + 
                            Mem_Rate + Lang_Rate + Prax_Rate + 
                            at.level(MRI_Indicator, 'MRI'):trait +
                            at.level(MRI_Indicator, 'MRI'):trait:Patient_Age +                         
                            I(Mem_Rate*Patient_Age) + I(Lang_Rate*Patient_Age) + I(Prax_Rate*Patient_Age) + 
                            I(Mem_Slopes*Patient_APOE) +
                            I(Mem_Rate*Patient_APOE) + I(Lang_Rate*Patient_APOE) + I(Prax_Rate*Patient_APOE)+ 
                              I(Mem_Slopes*Patient_Arm) + I(Lang_Slopes*Patient_Arm) + I(Prax_Slopes*Patient_Arm) +
                              I(Mem_Rate*Patient_Arm) + I(Lang_Rate*Patient_Arm) + I(Prax_Rate*Patient_Arm),
                          random= rand_form, 
                          rcov= ~ idh(Item_trait2):units,
                          family = NULL,
                          prior = MCMCprior_MCI_Comb,
                          data = MRI_ADAS_MCMCglmm_Long_Data[which(MRI_ADAS_MCMCglmm_Long_Data$Patient_Visits<=Uniq_visit[vis]),],
                          verbose = TRUE, saveZ = TRUE, saveX = TRUE, pr = TRUE, pl = TRUE, 
                          nitt = 160000,
                          burnin = 10000,
                          thin = 150, slice = TRUE)
  save(file = paste('/Users/nishant/Desktop/Imaging_Alzheimers_Factor_Analysis/Balanced_Samples_New/MCMC_MCI_Ability_Estimation_Visit_', vis, '.RData', sep=''),
       list = c('MCMC_Long_MCI','MRI_ADAS_MCMCglmm_Long_Data'))
}
Npat <- length(unique(MRI_ADAS_MCMCglmm_Long_Data$Patient_RID))
Scores_MRI <- matrix(colMeans(MCMC_Long_MCI$Sol[,c(82 + c(1:(Npat*6)))]), Npat, 6)
Scores_MRI <- Scores_MRI+ (mat.or.vec(dim(Scores_MRI)[1],1)+1)%*%t(colMeans(MCMC_Long_MCI$Sol[,c(2:7)]))
row.names(Scores_MRI) <- substr(colnames(MCMC_Long_MCI$Sol[,c(82 + c(1:Npat))]), 36, 40)
colnames(Scores_MRI) <- c('Mem_Slopes', 'Lang_Slopes', 'Prax_Slopes', 'Mem_Rate', 'Lang_Rate', 'Prax_Rate')
Scores_MRI <- data.frame(Scores_MRI)
Scores_MRI$Class <- MRI_ADAS_MCMCglmm_Long_Data$Patient_Arm[match(row.names(Scores_MRI), MRI_ADAS_MCMCglmm_Long_Data$Patient_RID)]
Scores_MRI$Class[Scores_MRI$Class==0] <- 'NP'; Scores_MRI$Class[Scores_MRI$Class==1] <- 'P'
Scores_MRI$APOE <- MRI_ADAS_MCMCglmm_Long_Data$Patient_APOE[match(row.names(Scores_MRI), MRI_ADAS_MCMCglmm_Long_Data$Patient_RID)]
# Class
Scores_MRI[which(Scores_MRI$Class=='P'),1:6] <- Scores_MRI[which(Scores_MRI$Class=='P'),1:6] + 
  (mat.or.vec(dim(Scores_MRI[which(Scores_MRI$Class=='P'),1:6])[1],1)+1)%*%t(colMeans(MCMC_Long_MCI$Sol[,c(15:20)]))
# APOE
Scores_MRI[which(Scores_MRI$APOE==1),c(4:6)] <- Scores_MRI[which(Scores_MRI$APOE==1),4:6] + 
  (mat.or.vec(dim(Scores_MRI[which(Scores_MRI$APOE==1),])[1],1)+1)%*%t(colMeans(MCMC_Long_MCI$Sol[,c(12:14)]))

print(c(apply(Scores_MRI[which(Scores_MRI$Class=='P'),c(4:6)], 2, mean), apply(Scores_MRI[which(Scores_MRI$Class=='NP'),c(4:6)], 2, mean)))
print(c(apply(Scores_MRI[which(Scores_MRI$Class=='P'),c(4:6)], 2, sd), apply(Scores_MRI[which(Scores_MRI$Class=='NP'),c(4:6)], 2, sd)))
P_num <- length(which(Scores_MRI$Class=='P')) -1
NP_num <- length(which(Scores_MRI$Class=='NP')) -1
Eff_size <- (apply(Scores_MRI[which(Scores_MRI$Class=='P'),c(4:6)], 2, mean) - apply(Scores_MRI[which(Scores_MRI$Class=='NP'),c(4:6)], 2, mean))/
  sqrt((((apply(Scores_MRI[which(Scores_MRI$Class=='P'),c(4:6)], 2, sd))^2)*P_num + 
          ((apply(Scores_MRI[which(Scores_MRI$Class=='NP'),c(4:6)], 2, sd))^2)*NP_num)/(P_num+NP_num))
print(Eff_size)

# Using ADAS-CogIRT methodology
Vfix <- diag(20)*1000; Vfix[1,1] <- 1e-10
MCMCprior_MCI2 <- list(B = list(mu = c(1,rep(0,19)), V = Vfix), 
                       G = list(G1 = list(V=diag(6), nu = 7, alpha.mu = rep(0,6), alpha.V = diag(6)*1000)),
                       R = list(V = 1, fix = 1))

rand_form2 <- ~ us(Mem_Slopes+Lang_Slopes+Prax_Slopes +
                     Mem_Rate+Lang_Rate+Prax_Rate):Patient_RID 


for(vis in 1:length(Uniq_visit)){
  MCMC_Long_MCI_ADAS_CogIRT <- MCMCglmm(fixed = Item_Responses ~ -1 + Item_Intercept + 
                                          Mem_Slopes + Lang_Slopes + Prax_Slopes + 
                                          Mem_Rate + Lang_Rate + Prax_Rate + 
                                          I(Mem_Rate*Patient_Age) + I(Lang_Rate*Patient_Age) + I(Prax_Rate*Patient_Age) + 
                                          I(Mem_Slopes*Patient_APOE) + 
                                          I(Mem_Rate*Patient_APOE) + I(Lang_Rate*Patient_APOE) + I(Prax_Rate*Patient_APOE) + 
                                          I(Mem_Slopes*Patient_Arm) + I(Lang_Slopes*Patient_Arm) + I(Prax_Slopes*Patient_Arm) +
                                          I(Mem_Rate*Patient_Arm) + I(Lang_Rate*Patient_Arm) + I(Prax_Rate*Patient_Arm),
                                        random= rand_form2, 
                                        rcov= ~ units,
                                        family = 'categorical',
                                        prior = MCMCprior_MCI2,
                                        data = MRI_ADAS_MCMCglmm_Long_Data
                                        [which(MRI_ADAS_MCMCglmm_Long_Data$MRI_Indicator=='NADAS' &
                                                 MRI_ADAS_MCMCglmm_Long_Data$Patient_Visits<=Uniq_visit[vis]),c(-5,-6)],
                                        verbose = TRUE,
                                        saveZ = TRUE, 
                                        saveX = TRUE,
                                        pr = TRUE, 
                                        pl = TRUE, 
                                        nitt = 500000,
                                        burnin = 50000,
                                        thin = 500)  
  save(file = paste('/Users/nishant/Desktop/Imaging_Alzheimers_Factor_Analysis/Balanced_Samples/MCMC_MCI_Ability_Estimation_ADASCogIRT_Visit_', vis, '.RData', sep=''),
       list = c('MCMC_Long_MCI_ADAS_CogIRT'))
}

Scores <- matrix(colMeans(MCMC_Long_MCI_ADAS_CogIRT$Sol[,c(14 + c(1:(565*6)))]), 565, 6)
row.names(Scores) <- substr(colnames(MCMC_Long_MCI_ADAS_CogIRT$Sol[,c(14 + c(1:565))]), 36,40)
colnames(Scores) <- c('Mem_Slopes', 'Lang_Slopes', 'Prax_Slopes', 'Mem_Rate', 'Lang_Rate', 'Prax_Rate')
Scores <- data.frame(Scores)
Scores$Class <- MCI_Label[match(row.names(Scores), MCI_RIDs)]
print(c(apply(Scores[which(Scores$Class=='P'),c(4:6)], 2, mean), apply(Scores[which(Scores$Class=='NP'),c(4:6)], 2, mean)))
print(c(apply(Scores[which(Scores$Class=='P'),c(4:6)], 2, sd), apply(Scores[which(Scores$Class=='NP'),c(4:6)], 2, sd)))
P_num <- length(which(Scores$Class=='P')) -1
NP_num <- length(which(Scores$Class=='NP')) -1
Eff_size <- (apply(Scores[which(Scores$Class=='P'),c(4:6)], 2, mean) - apply(Scores[which(Scores$Class=='NP'),c(4:6)], 2, mean))/
  sqrt((((apply(Scores[which(Scores$Class=='P'),c(4:6)], 2, sd))^2)*P_num + ((apply(Scores[which(Scores$Class=='NP'),c(4:6)], 2, sd))^2)*NP_num)/(P_num+NP_num))
print(Eff_size)



# Select the appropriate MCI-P and MCI-NP patients (comparable baseline ADAS-Cog scores)
MCI_RIDs_Select <- unique(Data$RID[which(Data$Visits==0 & ADAS70>=5 & ADAS70<=15)])
MCI_RIDs_Select <- unique(Data$RID[which(Data$Visits==3)])

# Define a Classifier training and testing function
MCI_Progression_LR_Classifiers <- function(Data, Boot_num, rep){
  
  Classifier_Accuracies <- mat.or.vec(Boot_num,2) + NA
  for(boot in 1:Boot_num){
    print(boot)
    # partition total data into training and testing subsets
    inTrain <- createDataPartition(y = Data$Class, p = 0.75, list = FALSE) # 75% for training
    TrainingData <- Data[inTrain,]
    Training_RIDs <- row.names(TrainingData)
    TestData <- Data[-inTrain,]
    Testing_RIDs <- row.names(TestData)
    
    # Define control parameters of classifier training
    ctrl <- trainControl(method = "repeatedcv", repeats = rep, classProbs = TRUE, summaryFunction = twoClassSummary)
    # train LR classifier
    MCI_Prog_Classifier_LR <- train(Class ~ ., preProc = c("center", "scale"), 
                                    data = TrainingData, method = 'glm', tuneLength = 20,
                                    family = binomial, trControl = ctrl, metric = 'ROC')
    # test LR classifier
    LR_Predict <- predict(MCI_Prog_Classifier_LR, newdata = TestData, type="prob")
    LR_ROC <- roc(response = (TestData$Class=='P')*1, LR_Predict[,"P"])
    Classifier_Accuracies[boot,1:2] <- c(MCI_Prog_Classifier_LR$results$ROC, LR_ROC$auc)
  }
  return(Classifier_Accuracies)
}
MCI_Progression_SVM_Classifiers <- function(Data, Boot_num, rep){
  
  Classifier_Accuracies <- mat.or.vec(Boot_num,2) + NA
  for(boot in 1:Boot_num){
    print(boot)
    # partition total data into training and testing subsets
    inTrain <- createDataPartition(y = Data$Class, p = 0.75, list = FALSE) # 75% for training
    TrainingData <- Data[inTrain,]
    Training_RIDs <- row.names(TrainingData)
    TestData <- Data[-inTrain,]
    Testing_RIDs <- row.names(TestData)
    
    # Define control parameters of classifier training
    ctrl <- trainControl(method = "repeatedcv", repeats = rep, classProbs = TRUE, summaryFunction = twoClassSummary)
    # train LR classifier
    MCI_Prog_Classifier_SVM <- train(Class ~ ., preProc = c("center", "scale"), 
                                     data = TrainingData, method = 'svmRadial', tuneLength = 20,
                                     trControl = ctrl, metric = 'ROC')
    # test LR classifier
    SVM_Predict <- predict(MCI_Prog_Classifier_SVM, newdata = TestData, type="prob")
    SVM_ROC <- roc(response = (TestData$Class=='P')*1, SVM_Predict[,"P"])
    Classifier_Accuracies[boot,1:2] <- c(max(MCI_Prog_Classifier_SVM$results$ROC), SVM_ROC$auc)
  }
  return(Classifier_Accuracies)
}


######## Load the files and organize data for classifier training and testing
# Get all unique RIDs of patients
Pat_RIDs <- unique(MRI_ADAS_MCMCglmm_Long_Data$Patient_RID)
Npat <- length(Pat_RIDs)

## total ADAS-Cog scores based prediction
LMER_ADAS70 <- lmer(ADAS70 ~ Visits + (Visits|RID), data = Data, subset = which(Visits<=1))
Patient_ADAS_Scores <- (mat.or.vec(Npat,1)+1)%*%t(LMER_ADAS70@beta) + ranef(LMER_ADAS70)$RID
Patient_ADAS_Scores$Class <- as.factor(MCI_Label[match(row.names(Patient_ADAS_Scores), MCI_RIDs)])
Patient_ADAS_Scores <- Patient_ADAS_Scores[row.names(Patient_ADAS_Scores)%in%MCI_RIDs_Select,]
ADAS70_1year <- MCI_Progression_Classifiers(Patient_ADAS_Scores, 100, 10)

# Load the saved data
load(file = paste('/Users/Nishant/Desktop/Imaging_Worspace_Files/MCI_Ability_Estimation_Data/MCMC_MCI_Ability_Estimation_Visit_1.RData'))
# Get patient estimates
Patient_Estimates <- colMeans(MCMC_Long_MCI$Sol[,77:20981])
Patient_traits <- matrix(Patient_Estimates[c(1:(Npat*6))], nrow = Npat, ncol = 6)
row.names(Patient_traits) <- substr(labels(Patient_Estimates[1:Npat]), 36, 40)
# Add average values across population
Patient_traits <- Patient_traits + (mat.or.vec(Npat,1)+1)%*%t(colMeans(MCMC_Long_MCI$Sol[,2:7]))
# Add contribution from APOE and Age
# Age
Patient_traits[,4:6] <- Patient_traits[,4:6] + ((Data$Age[match(row.names(Patient_traits), 
                                                  Data$RID)]-mu_Age)/sd_Age)%*%t(colMeans(MCMC_Long_MCI$Sol[,8:10]))
#APOE
Patient_traits[,1] <- Patient_traits[,1] + Data$APOE[match(row.names(Patient_traits), 
                                                           Data$RID)]*mean(MCMC_Long_MCI$Sol[,11])
Patient_traits[,4:6] <- Patient_traits[,4:6] + Data$APOE[match(row.names(Patient_traits), 
                                                  Data$RID)]%*%t(colMeans(MCMC_Long_MCI$Sol[,12:14]))

colnames(Patient_traits) <- c('MemB','LangB','PraxB','MemR','LangR','PraxR')
Patient_MRI_Meas <- matrix(Patient_Estimates[(Npat*6+1):20905], nrow = Npat, ncol = 31)
row.names(Patient_MRI_Meas) <- substr(labels(Patient_Estimates[1:Npat]), 36, 40)
colnames(Patient_MRI_Meas) <- colnames(Data)[3:33]
Patient_MRI_Meas <- data.frame(Patient_MRI_Meas)
Patient_MRI_Meas$Class <- as.factor(MCI_Label[match(row.names(Patient_MRI_Meas), MCI_RIDs)])


# Include the class (MCI-P vs. MCI-NP) in the matrix 
Patient_traits <- data.frame(Patient_traits)
Patient_traits$Class <- as.factor(MCI_Label[match(row.names(Patient_traits), MCI_RIDs)])
Patient_traits <- Patient_traits[row.names(Patient_traits)%in%MCI_RIDs_Select,]

# classifier design
Acc_MRIADAS_6months <- MCI_Progression_Classifiers(Patient_traits, 100, 10)


############################# Prediction using MRI+ADAS ##########################################



    
    
    
    
    
    

########################################### Classifier Training #############################################
# 1. Logistic regression
# ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
#0.9009972  0.8436364  0.7963636  0.06598697  0.1098562  0.1242727
MCI_Prog_Classifier_LR <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'glm', tuneLength = 20,
                                family = binomial, trControl = ctrl, metric = 'ROC')
# LR testing
LR_Predict <- predict(MCI_Prog_Classifier_LR, newdata = TestData, type="prob")
LR_ROC <- roc(response = (TestData$Class=='P')*1, LR_Predict[,"P"])


# 2. Naive Bayes
#                 ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
#   FALSE      0.9002231  0.8727273  0.7651515  0.05669791  0.08604545  0.1364594
MCI_Prog_Classifier_NB <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'nb', tuneLength = 20,
                                trControl = ctrl, metric = 'ROC')

# 3. k-nearest neighbors
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 43  0.8767245  0.8884848  0.7481818  0.06866143  0.09279957  0.1225071
MCI_Prog_Classifier_kNN <- train(Class ~ ., preProc = c("center", "scale"), 
                                 data = TrainingData, method = 'knn', tuneLength = 20,
                                 trControl = ctrl, metric = 'ROC')

# 4. Random forest
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 2     0.8987865  0.8200000  0.8357576  0.05416099  0.1034156  0.12880699
MCI_Prog_Classifier_RF <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'rf', tuneLength = 20,
                                trControl = ctrl, metric = 'ROC')

# 5. SVM
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 2.00  0.8929229  0.8175758  0.8148485  0.05986058  0.10035370  0.1094946
MCI_Prog_Classifier_SVM <- train(Class ~ ., preProc = c("center", "scale"), 
                                 data = TrainingData, method = 'svmRadial', tuneLength = 20,
                                 trControl = ctrl, metric = 'ROC')

########################################### Classifier Testing #############################################
# LR
LR_Predict <- predict(MCI_Prog_Classifier_LR, newdata = TestData, type="prob")
LR_ROC <- roc(response = (TestData$Class=='P')*1, LR_Predict[,"P"])

#NB
NB_Predict <- predict(MCI_Prog_Classifier_NB, newdata = TestData, type="prob")
NB_ROC <- roc(response = (TestData$Class=='P')*1, NB_Predict[,"P"])

#kNN
knn_Predict <- predict(MCI_Prog_Classifier_kNN, newdata = TestData, type="prob")
knn_ROC <- roc(response = (TestData$Class=='P')*1, knn_Predict[,"P"])

# RF
RF_Predict <- predict(MCI_Prog_Classifier_RF, newdata = TestData, type="prob")
RF_ROC <- roc(response = (TestData$Class=='P')*1, RF_Predict[,"P"])

# SVM
SVM_Predict <- predict(MCI_Prog_Classifier_SVM, newdata = TestData, type="prob")
SVM_ROC <- roc(response = (TestData$Class=='P')*1, SVM_Predict[,"P"])

#######################################################################################################







Pat_RIDs <- unique(MRI_ADAS_MCMCglmm_Long_Data$Patient_RID)

# Define prior for MCMC modeling
Vfix <- diag(38)*1000 ; Vfix[1,1] <- 1e-10
MCMCprior_MCI <- list(B = list(mu = c(1, rep(0,37)), V = Vfix), 
                      G = NULL,
                      R = list(V = diag(2), nu = 3, fix = 2))

Patient_Results <- mat.or.vec(length(Pat_RIDs), 6)
for(rid in 1:length(Pat_RIDs)){
  pat_rid <- Pat_RIDs[rid]
  print(rid)
  Compiled_Pat_Data <- MRI_ADAS_MCMCglmm_Long_Data[which(MRI_ADAS_MCMCglmm_Long_Data$Patient_RID==pat_rid),]
  if(length(unique(Compiled_Pat_Data$Patient_Visits))>1){
  MCMC_Patient <- MCMCglmm(fixed = Item_Responses ~ -1 + Item_Intercept + 
                              Mem_Slopes + Lang_Slopes + Prax_Slopes + 
                              Mem_Rate + Lang_Rate + Prax_Rate + 
                              at.level(MRI_Indicator, 'MRI'):trait,
                            random = NULL, rcov = ~ idh(MRI_Indicator):units,
                            family = NULL,
                            prior = MCMCprior_MCI,
                            data = Compiled_Pat_Data,
                            verbose = FALSE, nitt = 50000, burnin = 5000, thin = 50)
  Patient_Results[rid,] <- colMeans(MCMC_Patient$Sol[,2:7])  
}}

save(file = '/Users/nishant/Dropbox/Imaging_Alzheimers_Research/RData/MCI_Patient_Level_Estimates.RData',
     list = c('Patient_Results'))


# Include the class (MCI-P vs. MCI-NP) in the matrix 
Patient_Results <- data.frame(Patient_Results)
row.names(Patient_Results) <- Pat_RIDs
Patient_Results$Class <- as.factor(MCI_Label[match(Pat_RIDs, MCI_RIDs)])

# Remove patients with NA values
temp <- rowSums(abs(Patient_Results[,1:6]))
rm_ind <- which(temp==0)
Patient_Results <- Patient_Results[-1*rm_ind,]


############################# Prediction using MRI+ADAS ##########################################
# partition total data into training and testing subsets
inTrain <- createDataPartition(y = Patient_Results$Class, p = 0.75, list = FALSE) # 75% for training
TrainingData <- Patient_Results[inTrain,]
Training_RIDs <- row.names(TrainingData)
TestData <- Patient_Results[-inTrain,]
Testing_RIDs <- row.names(TestData)

# Define control parameters of classifier training
ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

########################################### Classifier Training #############################################
# 1. Logistic regression
# ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
#0.9009972  0.8436364  0.7963636  0.06598697  0.1098562  0.1242727
MCI_Prog_Classifier_LR <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'glm', tuneLength = 10,
                                family = binomial, trControl = ctrl, metric = 'ROC')

# 2. Naive Bayes
#                 ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
#   FALSE      0.9002231  0.8727273  0.7651515  0.05669791  0.08604545  0.1364594
MCI_Prog_Classifier_NB <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'nb', tuneLength = 10,
                                trControl = ctrl, metric = 'ROC')

# 3. k-nearest neighbors
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 43  0.8767245  0.8884848  0.7481818  0.06866143  0.09279957  0.1225071
MCI_Prog_Classifier_kNN <- train(Class ~ ., preProc = c("center", "scale"), 
                                 data = TrainingData, method = 'knn', tuneLength = 20,
                                 trControl = ctrl, metric = 'ROC')

# 4. Random forest
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 2     0.8987865  0.8200000  0.8357576  0.05416099  0.1034156  0.12880699
MCI_Prog_Classifier_RF <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'rf', tuneLength = 20,
                                trControl = ctrl, metric = 'ROC')

# 5. SVM
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 2.00  0.8929229  0.8175758  0.8148485  0.05986058  0.10035370  0.1094946
MCI_Prog_Classifier_SVM <- train(Class ~ ., preProc = c("center", "scale"), 
                                 data = TrainingData, method = 'svmRadial', tuneLength = 20,
                                 trControl = ctrl, metric = 'ROC')

########################################### Classifier Testing #############################################
# LR
LR_Predict <- predict(MCI_Prog_Classifier_LR, newdata = TestData, type="prob")
LR_ROC <- roc(response = (TestData$Class=='P')*1, LR_Predict[,"P"])

#NB
NB_Predict <- predict(MCI_Prog_Classifier_NB, newdata = TestData, type="prob")
NB_ROC <- roc(response = (TestData$Class=='P')*1, NB_Predict[,"P"])

#kNN
knn_Predict <- predict(MCI_Prog_Classifier_kNN, newdata = TestData, type="prob")
knn_ROC <- roc(response = (TestData$Class=='P')*1, knn_Predict[,"P"])

# RF
RF_Predict <- predict(MCI_Prog_Classifier_RF, newdata = TestData, type="prob")
RF_ROC <- roc(response = (TestData$Class=='P')*1, RF_Predict[,"P"])

# SVM
SVM_Predict <- predict(MCI_Prog_Classifier_SVM, newdata = TestData, type="prob")
SVM_ROC <- roc(response = (TestData$Class=='P')*1, SVM_Predict[,"P"])

#######################################################################################################






#######################################################################################################
# Now, obtain patient-level estimates of ADAS-CogIRT methodology
#######################################################################################################

# Define prior for MCMC modeling
Vfix <- diag(7)*1000 ; Vfix[1,1] <- 1e-10
MCMCprior_MCI2 <- list(B = list(mu = c(1, rep(0,6)), V = Vfix), 
                      G = NULL,
                      R = list(V = 1, fix = 1))

Patient_Results_ADASCogIRT <- mat.or.vec(length(Pat_RIDs), 6)
for(rid in 1:length(Pat_RIDs)){
  pat_rid <- Pat_RIDs[rid]
  print(rid)
  Compiled_Pat_Data <- MRI_ADAS_MCMCglmm_Long_Data[which(MRI_ADAS_MCMCglmm_Long_Data$Patient_RID==pat_rid),]
  if(length(unique(Compiled_Pat_Data$Patient_Visits))>1){
    MCMC_Patient <- MCMCglmm(fixed = Item_Responses ~ -1 + Item_Intercept + 
                               Mem_Slopes + Lang_Slopes + Prax_Slopes + 
                               Mem_Rate + Lang_Rate + Prax_Rate,
                             random = NULL, rcov = ~ units,
                             family = 'categorical',
                             prior = MCMCprior_MCI2,
                             data = Compiled_Pat_Data[which(Compiled_Pat_Data$MRI_Indicator=='NADAS'), c(-5,-6)],
                             verbose = FALSE, nitt = 50000, burnin = 5000, thin = 50)
    Patient_Results_ADASCogIRT[rid,] <- colMeans(MCMC_Patient$Sol[,2:7])  
  }}


save(file = '/Users/nishant/Dropbox/Imaging_Alzheimers_Research/RData/MCI_Patient_Level_Estimates_ADASCogIRT.RData',
     list = c('Patient_Results_ADASCogIRT'))

# Include the class (MCI-P vs. MCI-NP) in the matrix 
Patient_Results_ADASCogIRT <- data.frame(Patient_Results_ADASCogIRT)
row.names(Patient_Results_ADASCogIRT) <- Pat_RIDs
Patient_Results_ADASCogIRT$Class <- as.factor(MCI_Label[match(Pat_RIDs, MCI_RIDs)])

# Remove patients with NA values
temp <- rowSums(abs(Patient_Results_ADASCogIRT[,1:6]))
rm_ind <- which(temp==0)
Patient_Results_ADASCogIRT <- Patient_Results_ADASCogIRT[-1*rm_ind,]


############################# Prediction using MRI+ADAS ##########################################
# partition total data into training and testing subsets
inTrain <- createDataPartition(y = Patient_Results_ADASCogIRT$Class, p = 0.75, list = FALSE) # 75% for training
TrainingData <- Patient_Results_ADASCogIRT[inTrain,]
Training_RIDs <- row.names(TrainingData)
TestData <- Patient_Results_ADASCogIRT[-inTrain,]
Testing_RIDs <- row.names(TestData)

# Define control parameters of classifier training
ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

########################################### Classifier Training #############################################
# 1. Logistic regression
# ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
#0.9009972  0.8436364  0.7963636  0.06598697  0.1098562  0.1242727
MCI_Prog_Classifier_LR <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'glm', tuneLength = 10,
                                family = binomial, trControl = ctrl, metric = 'ROC')

# 2. Naive Bayes
#                 ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
#   FALSE      0.9002231  0.8727273  0.7651515  0.05669791  0.08604545  0.1364594
MCI_Prog_Classifier_NB <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'nb', tuneLength = 10,
                                trControl = ctrl, metric = 'ROC')

# 3. k-nearest neighbors
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 43  0.8767245  0.8884848  0.7481818  0.06866143  0.09279957  0.1225071
MCI_Prog_Classifier_kNN <- train(Class ~ ., preProc = c("center", "scale"), 
                                 data = TrainingData, method = 'knn', tuneLength = 20,
                                 trControl = ctrl, metric = 'ROC')

# 4. Random forest
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 2     0.8987865  0.8200000  0.8357576  0.05416099  0.1034156  0.12880699
MCI_Prog_Classifier_RF <- train(Class ~ ., preProc = c("center", "scale"), 
                                data = TrainingData, method = 'rf', tuneLength = 20,
                                trControl = ctrl, metric = 'ROC')

# 5. SVM
#         ROC        Sens       Spec       ROC SD      Sens SD     Spec SD  
# 2.00  0.8929229  0.8175758  0.8148485  0.05986058  0.10035370  0.1094946
MCI_Prog_Classifier_SVM <- train(Class ~ ., preProc = c("center", "scale"), 
                                 data = TrainingData, method = 'svmRadial', tuneLength = 20,
                                 trControl = ctrl, metric = 'ROC')

########################################### Classifier Testing #############################################
# LR
LR_Predict <- predict(MCI_Prog_Classifier_LR, newdata = TestData, type="prob")
LR_ROC <- roc(response = (TestData$Class=='P')*1, LR_Predict[,"P"])

#NB
NB_Predict <- predict(MCI_Prog_Classifier_NB, newdata = TestData, type="prob")
NB_ROC <- roc(response = (TestData$Class=='P')*1, NB_Predict[,"P"])

#kNN
knn_Predict <- predict(MCI_Prog_Classifier_kNN, newdata = TestData, type="prob")
knn_ROC <- roc(response = (TestData$Class=='P')*1, knn_Predict[,"P"])

# RF
RF_Predict <- predict(MCI_Prog_Classifier_RF, newdata = TestData, type="prob")
RF_ROC <- roc(response = (TestData$Class=='P')*1, RF_Predict[,"P"])

# SVM
SVM_Predict <- predict(MCI_Prog_Classifier_SVM, newdata = TestData, type="prob")
SVM_ROC <- roc(response = (TestData$Class=='P')*1, SVM_Predict[,"P"])

#######################################################################################################























