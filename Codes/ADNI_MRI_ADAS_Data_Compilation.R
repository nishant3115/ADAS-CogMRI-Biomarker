###############################################################################
# Summary: This code combines freesurfer and ADAS data of ADNI ################
###############################################################################
# FreeSurfer data
load(file = '..//RData/ADNI_FreeSufer_Longitudinal_Data.RData')
# ADAS-Cog response data
load(file = '..//RData/ADNI_ADAS_Data.RData')
# Patient covariates
load(file = '../RData/ADNI_Patient_Covariates.RData')

# Clean data
FS_Data_FA <- ADNI_FS_Long_Data_Avg[,-1*c(45)]
# remove missing data
temp <- which(is.na(FS_Data_FA), arr.ind = TRUE)
FS_Data_FA <- FS_Data_FA[-1*c(unique(temp[,1])),]
# Normalize the data with ICV (cols 4-63: all CV & SV)
FS_Data_FA[,4:63] <- (FS_Data_FA[,4:63]/FS_Data_FA$ICV_CV)*
  mean(FS_Data_FA$ICV_CV, na.rm = TRUE)

# Select only MCI and AD patients (remove normal patients NL)
FS_Data_FA <- FS_Data_FA[which(FS_Data_FA$Visit_Diagnosis=='MCI' | 
                                 FS_Data_FA$Visit_Diagnosis=='AD'),]

# Indices for cortical volumes, subcortical volumes, and cortical thicknesses
CV_indices <- c(4:37)
SV_indices <- c(38:63)
SV_indices <- SV_indices[-1*c(1:9, 11:14, 16:26)]
TA_indices <- c(64:97)[-1*c(2, 10, 23, 26, 33)]

# Subset of data found important in factor analysis
FS_Data_FA <- FS_Data_FA[,c(1,2, TA_indices, 47, 52)]

# Add ADNI data
ADNI_ADAS <- c()
# Match ADAS-cog scores with the FS Data (RID and VISCODE)
# Change VISCODE encoding of ADAS data
ADNI_ADAS_Data$VISCODE <- as.character(ADNI_ADAS_Data$VISCODE)
ADNI_ADAS_Data$VISCODE[ADNI_ADAS_Data$VISCODE=='bl'] <- 'sc'
FS_Data_FA$VISCODE2[which(FS_Data_FA$VISCODE2=='bl')] <- 'sc'
FS_Data_FA$VISCODE2[which(FS_Data_FA$VISCODE2=='scmri')] <- 'sc'
# Match the entries
empty_ind <- c()
for(r in 1:dim(FS_Data_FA)[1]){
  t <- which(ADNI_ADAS_Data$RID==FS_Data_FA$RID[r] & 
               ADNI_ADAS_Data$VISCODE==FS_Data_FA$VISCODE2[r])[1]
  if(length(t)>0){ADNI_ADAS <- rbind(ADNI_ADAS, ADNI_ADAS_Data[t,])  }
  if(length(t)==0){empty_ind <- c(empty_ind, r)
                   ADNI_ADAS <- rbind(ADNI_ADAS, NA)} 
}

# Compile into the correct format
ADNI_ADAS_Data <- data.frame('Q1'= round(ADNI_ADAS$Q1_SCORE))
ADNI_ADAS_Data$Q1[which(ADNI_ADAS_Data$Q1==0)] <- 1
ADNI_ADAS_Data$Q1[which(ADNI_ADAS_Data$Q1>9)] <- 9
ADNI_ADAS_Data$Q2H <- ((ADNI_ADAS$Q21+ADNI_ADAS$Q22+ADNI_ADAS$Q23+
                          ADNI_ADAS$Q24)>0)*1
ADNI_ADAS_Data$Q25 <- ADNI_ADAS$Q25
ADNI_ADAS_Data$Q26 <- ADNI_ADAS$Q26
ADNI_ADAS_Data$Q2M <- ((ADNI_ADAS$Q27+ADNI_ADAS$Q28+ADNI_ADAS$Q29)>0)*1
ADNI_ADAS_Data$Q210 <- ADNI_ADAS$Q210
ADNI_ADAS_Data$Q211 <- ADNI_ADAS$Q211
ADNI_ADAS_Data$Q212 <- ADNI_ADAS$Q212
ADNI_ADAS_Data$Q213 <- ADNI_ADAS$Q213
ADNI_ADAS_Data$Q214 <- ADNI_ADAS$Q214
ADNI_ADAS_Data$Q215 <- ADNI_ADAS$Q215
ADNI_ADAS_Data$Q216 <- ADNI_ADAS$Q216
ADNI_ADAS_Data$Q217 <- ADNI_ADAS$Q217
ADNI_ADAS_Data$Q3E <- ((ADNI_ADAS$Q32+ADNI_ADAS$Q33)>0)*1
ADNI_ADAS_Data$Q34 <- ADNI_ADAS$Q34
ADNI_ADAS_Data$Q35 <- ADNI_ADAS$Q35
ADNI_ADAS_Data$Q4E <- ((ADNI_ADAS$Q42+ADNI_ADAS$Q43)>0)*1
ADNI_ADAS_Data$Q45 <- ADNI_ADAS$Q44
ADNI_ADAS_Data$Q46 <- ADNI_ADAS$Q45
ADNI_ADAS_Data$Q5E <- ((ADNI_ADAS$Q51+ADNI_ADAS$Q52+ADNI_ADAS$Q53)>0)*1
ADNI_ADAS_Data$Q54 <- ADNI_ADAS$Q54
ADNI_ADAS_Data$Q55 <- ADNI_ADAS$Q55
ADNI_ADAS_Data$Q62 <- ADNI_ADAS$Q62
ADNI_ADAS_Data$Q63 <- ADNI_ADAS$Q63
ADNI_ADAS_Data$Q64 <- ADNI_ADAS$Q64
ADNI_ADAS_Data$Q65 <- ADNI_ADAS$Q65
ADNI_ADAS_Data$Q66 <- ADNI_ADAS$Q66
ADNI_ADAS_Data$Q67 <- ADNI_ADAS$Q67
ADNI_ADAS_Data$Q68 <- ADNI_ADAS$Q68
ADNI_ADAS_Data$Q7 <- round(ADNI_ADAS$Q7_SCORE)
ADNI_ADAS_Data$Q7[ADNI_ADAS_Data$Q7>12] <- 12
ADNI_ADAS_Data$Q8 <- ADNI_ADAS$Q8_SCORE
ADNI_ADAS_Data$Q8[ADNI_ADAS_Data$Q8>3]<-3
ADNI_ADAS_Data$Q9 <- ADNI_ADAS$Q9_SCORE
ADNI_ADAS_Data$Q9[ADNI_ADAS_Data$Q9>3]<-3
ADNI_ADAS_Data$Q10 <- ADNI_ADAS$Q10_SCORE
ADNI_ADAS_Data$Q10[ADNI_ADAS_Data$Q10>3]<- 3
ADNI_ADAS_Data$Q11 <- ADNI_ADAS$Q11_SCORE
ADNI_ADAS_Data$Q11[ADNI_ADAS_Data$Q11>3]<-3
ADNI_ADAS_Data$Q12 <- ADNI_ADAS$Q12_SCORE
ADNI_ADAS_Data$Q12[ADNI_ADAS_Data$Q12>10]<-10
ADNI_ADAS_Data$Q12[ADNI_ADAS_Data$Q12==0]<-1
ADNI_ADAS_Data$Q7[ADNI_ADAS_Data$Q7>9] <- 9

ADNI_ADAS_Data[ADNI_ADAS_Data<0] <- NA
temp <- ADNI_ADAS_Data[,c(2:29)]
temp[temp>1] <- NA
ADNI_ADAS_Data[,c(2:29)] <- temp

# Take cross-sectional data (using matched.indices computed earlier)
FS_ADAS_Data <- cbind(FS_Data_FA, ADNI_ADAS_Data)
FS_ADAS_Data$Gender <- ADNI_Covariates$Gender[match(FS_ADAS_Data$RID, 
                                                    ADNI_Covariates$RID)]
FS_ADAS_Data$APOE <- ADNI_Covariates$APOE_Binary[match(FS_ADAS_Data$RID, 
                                                       ADNI_Covariates$RID)]
FS_ADAS_Data$CM_Status <- ADNI_Covariates$CM_Med[match(FS_ADAS_Data$RID, 
                                                       ADNI_Covariates$RID)]
FS_ADAS_Data$Education <- ADNI_Covariates$Education[match(FS_ADAS_Data$RID, 
                                                          ADNI_Covariates$RID)]
FS_ADAS_Data$Age <- ADNI_Covariates$Age[match(FS_ADAS_Data$RID, 
                                              ADNI_Covariates$RID)]

# Enter diagnosis information
FS_ADAS_Data$Visit_Diagnosis <- 
  ADNI_FS_Long_Data$Visit_Diagnosis[
    match(interaction(FS_ADAS_Data$RID,FS_ADAS_Data$VISCODE2),
          interaction(ADNI_FS_Long_Data$RID, ADNI_FS_Long_Data$VISCODE2))]

# MCI and AD data
save(file = '../RData/Longitudinal_ADAS_MRI_Data.RData', 
     list = c('FS_ADAS_Data'))

# NL, MCI and AD data
save(file = '../RData/Longitudinal_ADAS_MRI_Data_With_NL.RData', 
     list = c('FS_ADAS_Data'))