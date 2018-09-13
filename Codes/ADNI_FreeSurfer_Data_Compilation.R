###############################################################################
# Summary: This code compiles data sheets containing volumetric and surface 
# measurements on ADNI patients 
###############################################################################
# ADNI-1 freeSurfer results (multiple visits/patient)
ADNI1_FS_Data <- read.csv('..//Data/ADNI1_FS_Long_Sheet.csv', header  = TRUE, 
                          stringsAsFactors = FALSE)
# ADNI-GO/2 freeSurfer results (multiple visits/patient)
ADNI2_FS_Data <- read.csv('..//Data/ADNI2GO_FS_Long_Sheet.csv', header  = TRUE,
                          stringsAsFactors = FALSE)
# Diagnosis data
ADNI_Diagnosis <- read.csv('..//Data/ADNI_Diagnosis_Summary.csv', header = TRUE,
                           stringsAsFactors = FALSE)


# change FreeSurfer codes to real descriptive labels
# Redefine cerebral volume (CV) labels
Old_CV_labels <- 
  c('ST10CV', 'ST13CV', 'ST14CV', 'ST15CV', 'ST23CV', 'ST24CV', 'ST25CV',
    'ST26CV', 'ST31CV', 'ST32CV', 'ST34CV', 'ST35CV', 'ST36CV',
    'ST38CV', 'ST39CV', 'ST40CV', 'ST43CV', 'ST44CV', 'ST45CV',
    'ST46CV', 'ST47CV', 'ST48CV', 'ST49CV', 'ST50CV', 'ST51CV',
    'ST52CV', 'ST54CV', 'ST55CV', 'ST56CV', 'ST57CV', 'ST58CV',
    'ST59CV', 'ST60CV', 'ST62CV', 'ST72CV', 'ST73CV', 'ST74CV',
    'ST82CV', 'ST83CV', 'ST84CV', 'ST85CV', 'ST90CV', 'ST91CV',
    'ST93CV', 'ST94CV', 'ST95CV', 'ST97CV', 'ST98CV', 'ST99CV',
    'ST102CV', 'ST103CV', 'ST104CV', 'ST105CV', 'ST106CV', 'ST107CV',
    'ST108CV', 'ST109CV', 'ST110CV', 'ST111CV', 'ST113CV', 'ST114CV',
    'ST115CV', 'ST116CV', 'ST117CV', 'ST118CV', 'ST119CV', 'ST121CV', 
    'ST129CV', 'ST130CV')

New_CV_labels <- c('ICV', 'LeftBankssts', 'LeftCaudalAnteriorCingulate', 
                   'LeftCaudalMiddleFrontal', 'LeftCuneus', 'LeftEntorhinal', 
                   'LeftFrontalPole',
                    'LeftFusiform', 'LeftInferiorParietal', 
                   'LeftInferiorTemporal', 'LeftIsthmusCingulate', 
                   'LeftLateralOccipital', 'LeftLateralOrbitofrontal',
                    'LeftLingual', 'LeftMedialOrbitofrontal', 
                   'LeftMiddleTemporal', 'LeftParacentral', 
                   'LeftParahippocampal', 'LeftParsOpercularis',
                    'LeftParsOrbitalis', 'LeftParsTriangularis', 
                   'LeftPericalcarine', 'LeftPostcentral', 
                   'LeftPosteriorCingulate', 'LeftPrecentral',
                    'LeftPrecuneus', 'LeftRostralAnteriorCingulate', 
                   'LeftRostralMiddleFrontal', 'LeftSuperiorFrontal', 
                   'LeftSuperiorParietal', 'LeftSuperiorTemporal',
                    'LeftSupramarginal', 'LeftTemporalPole', 
                   'LeftTransverseTemporal', 'RightBankssts', 
                   'RightCaudalAnteriorCingulate', 'RightCaudalMiddleFrontal',
                    'RightCuneus', 'RightEntorhinal', 'RightFrontalPole', 
                   'RightFusiform', 'RightInferiorParietal', 
                   'RightInferiorTemporal',
                    'RightIsthmusCingulate', 'RightLateralOccipital', 
                   'RightLateralOrbitofrontal', 'RightLingual', 
                   'RightMedialOrbitofrontal', 'RightMiddleTemporal',
                    'RightParacentral', 'RightParahippocampal', 
                   'RightParsOpercularis', 'RightParsOrbitalis', 
                   'RightParsTriangularis', 'RightPericalcarine',
                    'RightPostcentral', 'RightPosteriorCingulate', 
                   'RightPrecentral', 'RightPrecuneus', 
                   'RightRostralAnteriorCingulate', 'RightRostralMiddleFrontal',
                    'RightSuperiorFrontal', 'RightSuperiorParietal', 
                   'RightSuperiorTemporal', 'RightSupramarginal', 
                   'RightTemporalPole', 'RightTransverseTemporal', 
                   'LeftInsula', 'RightInsula')

New_CV_labels <- paste(New_CV_labels, '_CV', sep='')

# Redefine subcortical volume (SV) labels
Old_SV_labels <- c('ST1SV', 'ST2SV', 'ST3SV', 'ST4SV', 'ST5SV', 
                   'ST6SV', 'ST7SV', 'ST8SV', 'ST9SV', 'ST11SV', 'ST12SV',
                   'ST16SV', 'ST17SV', 'ST18SV', 'ST21SV', 'ST29SV', 'ST30SV', 
                   'ST37SV', 'ST42SV', 'ST53SV', 'ST65SV', 'ST66SV', 'ST68SV',
                   'ST69SV', 'ST70SV', 'ST71SV', 'ST61SV', 'ST75SV', 'ST76SV',
                   'ST77SV', 'ST80SV', 'ST88SV', 'ST89SV', 'ST96SV', 'ST101SV',
                   'ST112SV', 'ST120SV', 'ST124SV', 'ST125SV', 'ST127SV', 
                   'ST128SV')         

New_SV_labels <- c('Brainstem', 'CorpusCallosumAnterior', 
                   'CorpusCallosumCentral', 'CorpusCallosumMidAnterior', 
                   'CorpusCallosumMidPosterior', 
                   'CorpusCallosumPosterior', 'Csf', 'FifthVentricle', 
                   'FourthVentricle', 'LeftAccumbensArea', 'LeftAmygdala', 
                   'LeftCaudate', 'LeftCerebellumCortex', 'LeftCerebellumWM', 
                   'LeftChoroidPlexus', 'LeftHippocampus', 
                   'LeftInferiorLateralVentricle', 
                   'LeftLateralVentricle', 'LeftPallidum', 'LeftPutamen', 
                   'LeftVentralDC', 'LeftVessel', 'NonWMHypoIntensities',
                   'OpticChiasm', 'RightAccumbensArea', 'RightAmygdala', 
                   'LeftThalamus', 'RightCaudate', 'RightCerebellumCortex',
                   'RightCerebellumWM', 'RightChoroidPlexus', 
                   'RightHippocampus', 'RightInferiorLateralVentricle', 
                   'RightLateralVentricle', 'RightPallidum', 
                   'RightPutamen', 'RightThalamus', 'RightVentralDC', 
                   'RightVessel', 'ThirdVentricle', 'WMHypoIntensities')
              
New_SV_labels <- paste(New_SV_labels, '_SV', sep='')

# Redefine cortical thickness (TA) labels
Old_TA_labels <- c('ST13TA', 'ST14TA', 'ST15TA', 'ST23TA', 'ST24TA',
                   'ST25TA', 'ST26TA', 'ST31TA', 'ST32TA', 'ST34TA',
                   'ST35TA', 'ST36TA', 'ST38TA', 'ST39TA', 'ST40TA',
                   'ST43TA', 'ST44TA', 'ST45TA', 'ST46TA', 'ST47TA',
                   'ST48TA', 'ST49TA', 'ST50TA', 'ST51TA', 'ST52TA',
                   'ST54TA', 'ST55TA', 'ST56TA', 'ST57TA', 'ST58TA',
                   'ST59TA', 'ST60TA', 'ST62TA', 'ST72TA', 'ST73TA',
                   'ST74TA', 'ST82TA', 'ST83TA', 'ST84TA', 'ST85TA',
                   'ST90TA', 'ST91TA', 'ST93TA', 'ST94TA', 'ST95TA',
                   'ST97TA', 'ST98TA', 'ST99TA', 'ST102TA', 'ST103TA',
                   'ST104TA', 'ST105TA', 'ST106TA', 'ST107TA', 'ST108TA',
                   'ST109TA', 'ST110TA', 'ST111TA', 'ST113TA', 'ST114TA',
                   'ST115TA', 'ST116TA', 'ST117TA', 'ST118TA', 'ST119TA',
                   'ST121TA', 'ST129TA', 'ST130TA')


New_TA_labels <- c('LeftBankssts', 'LeftCaudalAnteriorCingulate', 
                   'LeftCaudalMiddleFrontal', 'LeftCuneus', 'LeftEntorhinal',
                   'LeftFrontalPole', 'LeftFusiform', 'LeftInferiorParietal', 
                   'LeftInferiorTemporal', 'LeftIsthmusCingulate', 
                   'LeftLateralOccipital', 'LeftLateralOrbitofrontal', 
                   'LeftLingual', 'LeftMedialOrbitofrontal', 
                   'LeftMiddleTemporal', 
                   'LeftParacentral', 'LeftParahippocampal', 
                   'LeftParsOpercularis', 'LeftParsOrbitalis', 
                   'LeftParsTriangularis',
                   'LeftPericalcarine', 'LeftPostcentral', 
                   'LeftPosteriorCingulate', 'LeftPrecentral', 'LeftPrecuneus',
                   'LeftRostralAnteriorCingulate', 'LeftRostralMiddleFrontal', 
                   'LeftSuperiorFrontal', 'LeftSuperiorParietal', 
                   'LeftSuperiorTemporal',
                   ' LeftSupramarginal', 'LeftTemporalPole', 
                   'LeftTransverseTemporal', 'RightBankssts', 
                   'RightCaudalAnteriorCingulate', 
                   'RightCaudalMiddleFrontal', 'RightCuneus', 
                   'RightEntorhinal', 'RightFrontalPole', 'RightFusiform', 
                   'RightInferiorParietal', 'RightInferiorTemporal', 
                   'RightIsthmusCingulate', 'RightLateralOccipital', 
                   'RightLateralOrbitofrontal',
                   'RightLingual', 'RightMedialOrbitofrontal', 
                   'RightMiddleTemporal', 'RightParacentral', 
                   'RightParahippocampal',
                   'RightParsOpercularis', 'RightParsOrbitalis', 
                   'RightParsTriangularis', 'RightPericalcarine', 
                   'RightPostcentral',
                   'RightPosteriorCingulate', 'RightPrecentral', 
                   'RightPrecuneus', 'RightRostralAnteriorCingulate', 
                   'RightRostralMiddleFrontal',
                   'RightSuperiorFrontal', 'RightSuperiorParietal', 
                   'RightSuperiorTemporal', 'RightSupramarginal', 
                   'RightTemporalPole',
                   'RightTransverseTemporal', 'LeftInsula', 'RightInsula'                 
                   )

New_TA_labels <- paste(New_TA_labels, '_TA', sep='')

# Change column names of data sheets
Pat_Info <- c('RID', 'VISCODE2')
ADNI1_Data <- ADNI1_FS_Data[,c(Pat_Info, Old_CV_labels, 
                               Old_SV_labels, Old_TA_labels)]
ADNI2_Data <- ADNI2_FS_Data[,c(Pat_Info, Old_CV_labels, 
                               Old_SV_labels, Old_TA_labels)]

# Remove rows where overall quality control is 'Fail'
ADNI1_Data <- ADNI1_Data[which(ADNI1_FS_Data$OVERALLQC=='Pass'),]
ADNI2_Data <- ADNI2_Data[which(ADNI2_FS_Data$OVERALLQC=='Pass'),]

# Take average of duplicate rows (only present in ADNI1 data)
unq_RID <- unique(ADNI1_Data$RID)
s <- 0
rm_ind <- c()
for(i in 1:length(unq_RID)){
  pat_ind <- which(ADNI1_Data$RID==unq_RID[i])
  vis_pat <- ADNI1_Data$VISCODE2[pat_ind]
  if(length(unique(vis_pat))<length(vis_pat)){
    unq_vis <- unique(vis_pat)
    for(j in 1:length(unq_vis)){
      p <- which(vis_pat==unq_vis[j])
      if(length(p)>1){
        print(length(p))
        ADNI1_Data[pat_ind[p[1]], 3:dim(ADNI1_Data)[2]] <- 
          colMeans(ADNI1_Data[pat_ind[p], 3:dim(ADNI1_Data)[2]])
        rm_ind <- c(rm_ind, pat_ind[p[2:length(p)]])
      }}}}
ADNI1_Data <- ADNI1_Data[-1*rm_ind,]

# Total ADNI Data
ADNI_FS_Long_Data <- rbind(ADNI1_Data, ADNI2_Data)
colnames(ADNI_FS_Long_Data) <- 
  gsub(" ", "", c(Pat_Info, New_CV_labels, New_SV_labels, New_TA_labels), 
       fixed = TRUE) 

# Take average of left and right sides
col_vec <- colnames(ADNI_FS_Long_Data[,3:180])
left_rm_col <- simplify2array(lapply(X = col_vec, FUN = 
                                       function(x) substr(x, 5, nchar(x))))
left_rm_ind <- which(simplify2array(lapply(
  X = col_vec, FUN = function(x) (substr(x, 1, 4)=='Left')*1))==1)
left_rm_col <- left_rm_col[left_rm_ind]
right_rm_col <- simplify2array(lapply(
  X = col_vec, FUN = function(x) substr(x, 6, nchar(x))))
right_rm_ind <- which(simplify2array(lapply(
  X = col_vec, FUN = function(x) (substr(x, 1, 5)=='Right')*1))==1)
right_rm_col <- right_rm_col[right_rm_ind]

# Take average of left and right side of the structures
ADNI_FS_Long_Data_Avg <- ADNI_FS_Long_Data
rm_col <- c()
for(k in 1:length(left_rm_col)){
  s <- right_rm_ind[which(right_rm_col == left_rm_col[k])]
  if(length(s)>0){
    k1 <- left_rm_ind[k]
    temp_lm <- lm(ADNI_FS_Long_Data[,(2+s)] ~ ADNI_FS_Long_Data[,(2+k1)])
    print(paste(left_rm_col[k], summary(temp_lm)$r.squared), sep=' ')
    
    ADNI_FS_Long_Data_Avg[,(2+k1)] <- (ADNI_FS_Long_Data_Avg[,(2+k1)] + 
                                         ADNI_FS_Long_Data_Avg[,(2+s)])/2
    colnames(ADNI_FS_Long_Data_Avg)[(2+k1)] <- left_rm_col[k]
    rm_col <- c(rm_col, (2+s))
  }
}
ADNI_FS_Long_Data_Avg <- ADNI_FS_Long_Data_Avg[,-1*(rm_col)]

# Extract visit diagnosis information from diagnosis summary sheet
# DXCURREN: 1=NL;  2=MCI; 3=AD
# DXCHANGE: 1=Stable: NL to NL
#           2=Stable: MCI to MCI
#           3=Stable: Dementia to Dementia
#           4=Conversion: NL to MCI
#           5=Conversion: MCI to Dementia
#           6=Conversion: NL to Dementia 
#           7=Reversion: MCI to NL
#           8=Reversion: Dementia to MCI
#           9=Reversion: Dementia to NL
# Convert ADNI2/GO format
DX_Decode <- ADNI_Diagnosis$DXCHANGE
DX_Decode[DX_Decode==1] <- 'NL'; 
DX_Decode[DX_Decode==2] <- 'MCI'; 
DX_Decode[DX_Decode==3] <- 'AD';
DX_Decode[DX_Decode==4] <- 'MCI'; 
DX_Decode[DX_Decode==5] <- 'AD'; 
DX_Decode[DX_Decode==6] <- 'AD';
DX_Decode[DX_Decode==7] <- 'NL'; 
DX_Decode[DX_Decode==8] <- 'MCI'; 
DX_Decode[DX_Decode==9] <- 'NL';

# Convert ADNI1 format
ADNI_Diagnosis$Visit_Diagnosis <- ADNI_Diagnosis$DXCURREN
ADNI_Diagnosis$Visit_Diagnosis[ADNI_Diagnosis$Visit_Diagnosis==1] <- 'NL'
ADNI_Diagnosis$Visit_Diagnosis[ADNI_Diagnosis$Visit_Diagnosis==2] <- 'MCI'
ADNI_Diagnosis$Visit_Diagnosis[ADNI_Diagnosis$Visit_Diagnosis==3] <- 'AD'

# Convert ADNI2/GO into same format as ADNI1
ADNI_Diagnosis$Visit_Diagnosis[which(is.na(ADNI_Diagnosis$Visit_Diagnosis))] <- 
  DX_Decode[which(is.na(ADNI_Diagnosis$Visit_Diagnosis))]

# Some diagnoses are not with confidence (remove them !)
# DXCONFID (physician's confidence): 1=Uncertain; 2=Mildly Confident; 
# 3=Moderately Confident; 4=Highly Confident
ADNI_Diagnosis$Visit_Diagnosis[which(ADNI_Diagnosis$DXCONFID==1)] <- NA

# Change 'bl' to 'sc' in FS data and diagnosis data
ADNI_FS_Long_Data$VISCODE2[ADNI_FS_Long_Data$VISCODE2=='bl'] <- 'sc'
ADNI_FS_Long_Data$VISCODE2[ADNI_FS_Long_Data$VISCODE2=='scmri'] <- 'sc'
ADNI_Diagnosis$VISCODE2[ADNI_Diagnosis$VISCODE2=='bl'] <- 'sc'

# Incorporate into FS long data
Diag_vec <- c()
for(id in 1:dim(ADNI_FS_Long_Data)[1]){
  id_match <- which(
    ADNI_Diagnosis$RID==ADNI_FS_Long_Data$RID[id] &
      ADNI_Diagnosis$VISCODE2==ADNI_FS_Long_Data$VISCODE2[id])
  if(length(id_match)>=1){  Diag_vec <- 
    c(Diag_vec, ADNI_Diagnosis$Visit_Diagnosis[id_match[1]]) }
  if(ADNI_FS_Long_Data$VISCODE2[id]=='m03'){Diag_vec <- 
    c(Diag_vec, ADNI_Diagnosis$Visit_Diagnosis[
      which(ADNI_Diagnosis$RID==ADNI_FS_Long_Data$RID[id] & 
              ADNI_Diagnosis$VISCODE2=='sc')][1])}
  if(length(id_match)==0 & ADNI_FS_Long_Data$VISCODE2[id]!='m03'){
    Diag_vec <- c(Diag_vec,NA) 
    }
}
ADNI_FS_Long_Data$Visit_Diagnosis <- Diag_vec
ADNI_FS_Long_Data_Avg$Visit_Diagnosis <- Diag_vec

# save FreeSurfer measurements dataframe
save(file = '..//RData/ADNI_FreeSufer_Longitudinal_Data.RData', 
     list = c('ADNI_FS_Long_Data', 'ADNI_FS_Long_Data_Avg', 'ADNI_Diagnosis'))
  

#------------------------------------------------------------------------------
# ------------------------ Patient Covariates ---------------------------------
#------------------------------------------------------------------------------

# ADNI Sheets
# demographics
ADNI_Demo_Data <- read.csv('..//Data/ADNI12GO_PTDEMOG.csv', 
                           header = TRUE, stringsAsFactors = FALSE)
# baseline diagnosis and diagnosis changes
ADNI_DXSUM <- read.csv('..//Data/DXSUM_PDXCONV_ADNIALL.csv', 
                       header = TRUE, stringsAsFactors = FALSE)
# genome information
ADNI_APOE_Data <- read.csv('..//Data/ADNI_APOERES.csv', 
                           header = TRUE, stringsAsFactors = FALSE)
# concomitant medication information
ADNI_CM_Data <- read.csv('..//Data/ADNI_RECCMEDS.csv', 
                         header = TRUE, stringsAsFactors = FALSE)

# patient RIDs
uniq_RID <- unique(ADNI_FS_Long_Data_Avg$RID)

# baseline diagnosis
ADNI_DXSUM_BL <- ADNI_DXSUM[which(ADNI_DXSUM$VISCODE2=='bl' | 
                                    ADNI_DXSUM$VISCODE2=='sc'),]

# Date of birth
ADNI_DOB <- (as.Date(paste("01",ADNI_Demo_Data$PTDOBMM,
                           ADNI_Demo_Data$PTDOBYY,sep="/"), 
                     format = "%d/%m/%Y"))[match(uniq_RID,ADNI_Demo_Data$RID)]

# calculate patient age 
ADNI_BL_Date <- ADNI_DXSUM_BL$EXAMDATE[match(uniq_RID, ADNI_DXSUM_BL$RID)]
ADNI_BL_Date <- as.Date(ADNI_BL_Date, format="%m/%d/%y")
ADNI_Age <- (ADNI_BL_Date-ADNI_DOB)/365

# demographics data
ADNI_Gender <- ADNI_Demo_Data$PTGENDER[match(uniq_RID, ADNI_Demo_Data$RID)]
ADNI_Race   <- ADNI_Demo_Data$PTRACCAT[match(uniq_RID, ADNI_Demo_Data$RID)]
ADNI_Ethnicity <- ADNI_Demo_Data$PTETHCAT[match(uniq_RID, ADNI_Demo_Data$RID)]
ADNI_Education <- ADNI_Demo_Data$PTEDUCAT[match(uniq_RID, ADNI_Demo_Data$RID)]

# genetic APOE allele data
ADNI_APOE_Allele1 <- ADNI_APOE_Data$APGEN1[match(uniq_RID, ADNI_APOE_Data$RID)]
ADNI_APOE_Allele2 <- ADNI_APOE_Data$APGEN2[match(uniq_RID, ADNI_APOE_Data$RID)]
# APOE-e4 status
ADNI_APOE <- mat.or.vec(length(ADNI_APOE_Allele1),1) + NA
ADNI_APOE_Binary <- mat.or.vec(length(ADNI_APOE_Allele1),1) + NA
ADNI_APOE[which(ADNI_APOE_Allele1==2 & ADNI_APOE_Allele2==2)] <- 1
ADNI_APOE[which((ADNI_APOE_Allele1==2 & ADNI_APOE_Allele2==3) | 
                  (ADNI_APOE_Allele2==2 & ADNI_APOE_Allele1==3))] <- 2
ADNI_APOE[which((ADNI_APOE_Allele1==2 & ADNI_APOE_Allele2==4) | 
                  (ADNI_APOE_Allele2==2 & ADNI_APOE_Allele1==4))] <- 3
ADNI_APOE[which(ADNI_APOE_Allele1==3 & ADNI_APOE_Allele2==3)] <- 4
ADNI_APOE[which((ADNI_APOE_Allele1==3 & ADNI_APOE_Allele2==4) | 
                  (ADNI_APOE_Allele2==3 & ADNI_APOE_Allele1==4))] <- 5
ADNI_APOE[which(ADNI_APOE_Allele1==4 & ADNI_APOE_Allele2==4)] <- 6
ADNI_APOE_Binary <- (ADNI_APOE>4)*1

# concomitant medication status
# 1: razadyne/galantamine
# 2: exelon/rivastigmine
# 3: aricept/donepezil
# 4: cognex/tacrine
# 5: namenda/memantine
# 6: vitamin e
ADNI_CM1 <- mat.or.vec(length(uniq_RID),1)
ADNI_CM2 <- mat.or.vec(length(uniq_RID),1)
ADNI_CM3 <- mat.or.vec(length(uniq_RID),1)
ADNI_CM4 <- mat.or.vec(length(uniq_RID),1)
ADNI_CM5 <- mat.or.vec(length(uniq_RID),1)
ADNI_CM6 <- mat.or.vec(length(uniq_RID),1)

for(i in 1:length(uniq_RID)){
  CM_patIDs <- tolower(ADNI_CM_Data$CMMED[which(ADNI_CM_Data$RID==uniq_RID[i])])
  if(sum((CM_patIDs=='razadyne' | CM_patIDs=='galantamine'))>0){ADNI_CM1[i]=1}
  if(sum((CM_patIDs=='exelon' | CM_patIDs=='rivastigmine'))>0){ADNI_CM2[i]=2}
  if(sum((CM_patIDs=='aricept' | CM_patIDs=='donepezil'))>0){ADNI_CM3[i]=3}
  if(sum((CM_patIDs=='cognex' | CM_patIDs=='tacrine'))>0){ADNI_CM4[i]=4}
  if(sum((CM_patIDs=='namenda' | CM_patIDs=='memantine'))>0){ADNI_CM5[i]=5}
  if(sum((CM_patIDs=='vitamin e' ))>0){ADNI_CM6[i]=6}
}
ADNI_CI <- ((ADNI_CM1 + ADNI_CM2 + ADNI_CM3 + ADNI_CM5)>0)*1

# combine patient clinical info in a dataframe
ADNI_Covariates <- data.frame(RID = uniq_RID, Gender = ADNI_Gender, 
                              APOE_Genotype = ADNI_APOE, 
                              APOE_Binary = ADNI_APOE_Binary, CM_Med = ADNI_CI, 
                              Education = ADNI_Education, ADNI_CM1, ADNI_CM2, 
                              ADNI_CM3, ADNI_CM5, Age = ADNI_Age)

# save data frame
save(file = '../RData/ADNI_Patient_Covariates.RData', 
     list = c('ADNI_Covariates'))

