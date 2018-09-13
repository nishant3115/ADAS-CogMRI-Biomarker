# Compiles ADAS-Cog response data from patients across the three ADNI phases 
# ADNI-1, ADNI-GO and ADNI-2

# load data from spreadsheets
# ADNI-1 responses
# ADAS_ADNI1.csv only contains subitem level responses
ADNI1_Sheet = read.csv('..//Data/ADAS_ADNI1.csv', header = TRUE, 
                       stringsAsFactors = FALSE)
# ADASSCORES.csv contains item-level responses 
ADNI1_Summary_Sheet = read.csv('..//Data/ADASSCORES.csv', header = TRUE, 
                               stringsAsFactors = FALSE)

# ADNI-GO and ADNI-2 responses (contain subitem & item-level responses)
ADNI2_Sheet = read.csv('..//Data/ADAS_ADNIGO2.csv', header= TRUE, 
                       stringsAsFactors = FALSE)

# Patients' roster IDs (RID)
RID1 = ADNI1_Sheet$RID; 
RID2 = ADNI2_Sheet$RID;
RID_comb = c(RID1, RID2);

# visit code
Vcode1 = ADNI1_Sheet$VISCODE; 
Vcode2 = ADNI2_Sheet$VISCODE2;
Vcode_comb = c(Vcode1, Vcode2)

#------------------------------------------------------------------------------
# ------------------------ ADAS-Cog scores compilation ------------------------
#------------------------------------------------------------------------------
# Q1: Word Recall Task
Q1_SCORE <- c((10-rowMeans(cbind(ADNI1_Sheet$COT1SCOR, ADNI1_Sheet$COT2SCOR, 
                                 ADNI1_Sheet$COT3SCOR), na.rm = TRUE)),
              ADNI2_Sheet$Q1SCORE)

#-------------------------------------------------------------------------------
# Q2: Commands (Q21,...,Q26 responses (0 or 1) or -4 if missing response)
splitted_Q2 = strsplit(c(ADNI1_Sheet$COCOMND, ADNI2_Sheet$Q2TASK),":")
Q21 = c(); Q22 = c(); Q23 = c(); Q24 = c(); Q25 = c(); Q26 = c(); 
Q2_SCORE = c();
for (i in 1:(length(splitted_Q2))){
  response_vec = as.numeric(splitted_Q2[[i]])
  Q21[i] = any(response_vec==1)*1;                          # fist
  Q22[i] = any(response_vec==2)*1;                          # ceiling+floor
  Q23[i] = any(response_vec==3)*1;                          # pencil+card
  Q24[i] = any(response_vec==4)*1;                          # watch+pencil+card
  Q25[i] = any(response_vec==5)*1;                          # shoulder
  Q26[i] = ((any(response_vec==6))|(any(response_vec==0)))*1; # none
  if((length(response_vec)==1) & (response_vec[1] == -4)){
    Q21[i]=-4; Q22[i]=-4; Q23[i]=-4; Q24[i]=-4; Q25[i]=-4; Q26[i]=-4;  }  
  if(i<= dim(ADNI1_Sheet)[1]){
    Q2_SCORE[i] = ADNI1_Summary_Sheet$Q2[
      (ADNI1_Summary_Sheet$RID==ADNI1_Sheet$RID[i]) & 
        (ADNI1_Summary_Sheet$VISCODE==ADNI1_Sheet$VISCODE[i])]
  }
  else
  {Q2_SCORE[i] = ADNI2_Sheet$Q2SCORE[(i - dim(ADNI1_Sheet)[1])];}
  if(is.na(Q2_SCORE[i])){ Q2_SCORE[i] = -4; }
}

#-------------------------------------------------------------------------------
# Q3: Constructional praxis (Q31: none correct/not attempted, Q32:circle, 
# Q33: rectangle, Q34:rhombus, Q35:cube 
# coded 0,1 for ADNI1 and 0,1,2,3 in ADNI2 (look at the key)
ADNI1_Q3_split = strsplit(ADNI1_Sheet$COCONSTR,":");
Q31 = c(); Q32 = c(); Q33 = c(); Q34 = c(); Q35 = c(); Q3_SCORE = c();
for (i in 1:(length(ADNI1_Q3_split))){
  response_vec = as.numeric(ADNI1_Q3_split[[i]])
  Q31[i] = any(response_vec==1)*1; # none drawn + no attempt
  Q32[i] = any(response_vec==2)*1; # circle
  Q33[i] = any(response_vec==3)*1; # rectangles
  Q34[i] = any(response_vec==4)*1; # rhombus
  Q35[i] = any(response_vec==5)*1; # cube
  Q3_SCORE[i] = ADNI1_Summary_Sheet$Q3[
    (ADNI1_Summary_Sheet$RID==ADNI1_Sheet$RID[i]) & 
      (ADNI1_Summary_Sheet$VISCODE==ADNI1_Sheet$VISCODE[i])]
  if((length(response_vec)==1) & (response_vec[1] == -4)){
    Q31[i]=-4; Q32[i]=-4; Q33[i]=-4; Q34[i]=-4; Q35[i]=-4; }
}
Q32 = c(Q32, (ADNI2_Sheet$Q3TASK1==1)); # circle
Q33 = c(Q33, (ADNI2_Sheet$Q3TASK2==1)); # rectangles
Q34 = c(Q34, (ADNI2_Sheet$Q3TASK3==1)); # rhombus
Q35 = c(Q35, (ADNI2_Sheet$Q3TASK4==1)); # cube
# 1=Shape drawn correctly;2=Shape drawn incorrectly (at least one side/section 
# of shape drawn);3=No recognizable attempt at drawing any side/section of shape
tt =  ((ADNI2_Sheet$Q3TASK1!=1) & (ADNI2_Sheet$Q3TASK2!=1) & 
         (ADNI2_Sheet$Q3TASK3!=1) & (ADNI2_Sheet$Q3TASK4!=1))*1
Q31 = c(Q31, (tt==1)*1) # incorrectly drawn or not attempted

Q3_SCORE = c(Q3_SCORE, ADNI2_Sheet$Q3SCORE)

#-------------------------------------------------------------------------------
# Q4: Delayed Word Recall Task
Q4_SCORE <- c((10-ADNI1_Sheet$COT4TOTL), ADNI2_Sheet$Q4SCORE)

#-------------------------------------------------------------------------------
# Q5 - Naming task
ADNI_Q5_split = strsplit(c(ADNI1_Sheet$CONAME, ADNI2_Sheet$Q5TASK),":")
Q51 = c(); Q52 = c(); Q53 = c(); Q54 = c(); Q55 = c(); Q56 = c(); 
Q57 = c(); Q58 = c(); Q59 = c(); Q510 = c(); Q511 = c(); Q512 = c(); 
Q513 = c(); Q514 = c(); Q515 = c(); Q516 = c(); Q517 = c(); Q518 = c(); 
Q5_SCORE = c();

for (i in 1:(length(ADNI_Q5_split))){
  response_vec = as.numeric(ADNI_Q5_split[[i]])
  Q51[i] = any(response_vec==1)*1; # flower
  Q52[i] = any(response_vec==2)*1; # bed
  Q53[i] = any(response_vec==3)*1; # whistle
  Q54[i] = any(response_vec==4)*1; # pencil
  Q55[i] = any(response_vec==5)*1; # rattle
  Q56[i] = any(response_vec==6)*1; # mask
  Q57[i] = any(response_vec==7)*1; # scissors
  Q58[i] = any(response_vec==8)*1; # comb
  Q59[i] = any(response_vec==9)*1; # wallet
  Q510[i] = any(response_vec==10)*1; # harmonica
  Q511[i] = any(response_vec==11)*1; # stethoscope
  Q512[i] = any(response_vec==12)*1; # tongs
  Q513[i] = any(response_vec==13)*1; # thumb
  Q514[i] = any(response_vec==14)*1; # middle
  Q515[i] = any(response_vec==15)*1; # ring
  Q516[i] = any(response_vec==16)*1; # index
  Q517[i] = any(response_vec==17)*1; # pinky
  Q518[i] = (any(response_vec==18) | any(response_vec==0))*1; # none correct
  
  if(i<= dim(ADNI1_Sheet)[1]){
    Q5_SCORE[i] = ADNI1_Summary_Sheet$Q5[
      (ADNI1_Summary_Sheet$RID==ADNI1_Sheet$RID[i]) & 
        (ADNI1_Summary_Sheet$VISCODE==ADNI1_Sheet$VISCODE[i])]}
  else
  { Q5_SCORE[i] = ADNI2_Sheet$Q5SCORE[(i - dim(ADNI1_Sheet)[1])]; }
  
  if((length(response_vec)==1) & (response_vec[1] == -4)){
    Q51[i]=-4; Q52[i]=-4; Q53[i]=-4; Q54[i]=-4; Q55[i]=-4; Q56[i]=-4;
    Q57[i]=-4; Q58[i]=-4; Q59[i]=-4; Q510[i]=-4; Q511[i]=-4; Q512[i]=-4;
    Q513[i]=-4; Q514[i]=-4; Q515[i]=-4; Q516[i]=-4; Q517[i]=-4; Q518[i]=-4;
    if(is.na(Q5_SCORE[i])){ Q5_SCORE[i] =-4; }
  }
}

#-------------------------------------------------------------------------------
# Q6 - Ideational Praxis
ADNI_Q6_split = strsplit(c(ADNI1_Sheet$COIDEA, ADNI2_Sheet$Q6TASK),":")
Q61 = c(); Q62 = c(); Q63 = c(); Q64 = c(); Q65 = c(); Q66 = c(); 
Q6_SCORE = c();

for (i in 1:(length(ADNI_Q6_split))){
  response_vec = as.numeric(ADNI_Q6_split[[i]])
  Q61[i] = any(response_vec==1)*1; # fold letter
  Q62[i] = any(response_vec==2)*1; # letter in envelope
  Q63[i] = any(response_vec==3)*1; # seal envelope
  Q64[i] = any(response_vec==4)*1; # address envelope
  Q65[i] = any(response_vec==5)*1;  # stamp
  Q66[i] = (any(response_vec==6) | any(response_vec==0))*1; # none correct
  
  if(i<= dim(ADNI1_Sheet)[1]){
    Q6_SCORE[i] = ADNI1_Summary_Sheet$Q6[
      (ADNI1_Summary_Sheet$RID==ADNI1_Sheet$RID[i]) & 
        (ADNI1_Summary_Sheet$VISCODE==ADNI1_Sheet$VISCODE[i])]}
  else
  {Q6_SCORE[i] = ADNI2_Sheet$Q6SCORE[(i - dim(ADNI1_Sheet)[1])];}
  if((length(response_vec)==1) & (response_vec[1] == -4)){
    Q61[i]=-4; Q62[i]=-4; Q63[i]=-4; Q64[i]=-4; Q65[i]=-4; Q66[i]=-4;
    if(is.na(Q6_SCORE[i])){ Q6_SCORE[i] = -4; }
  }
}

#-------------------------------------------------------------------------------
# Q7 - Orientation 
ADNI_Q7_split = strsplit(c(ADNI1_Sheet$COORIEN, ADNI2_Sheet$Q7TASK),":")
Q71 = c(); Q72 = c(); Q73 = c(); Q74 = c(); Q75 = c(); Q76 = c(); 
Q77 = c(); Q78 = c(); Q79 = c(); Q7_SCORE = c();

for (i in 1:(length(ADNI_Q7_split))){
  response_vec = as.numeric(ADNI_Q7_split[[i]])
  Q71[i] = any(response_vec==1)*1; # Full name
  Q72[i] = any(response_vec==2)*1; # month
  Q73[i] = any(response_vec==3)*1; # date
  Q74[i] = any(response_vec==4)*1; # year
  Q75[i] = any(response_vec==5)*1; # day
  Q76[i] = any(response_vec==6)*1; # season
  Q77[i] = any(response_vec==7)*1; # place
  Q78[i] = any(response_vec==8)*1; # time
  Q79[i] = (any(response_vec==9) | any(response_vec==0))*1; # none correct
  
  if(i<= dim(ADNI1_Sheet)[1]){
    Q7_SCORE[i] = ADNI1_Summary_Sheet$Q7[
      (ADNI1_Summary_Sheet$RID==ADNI1_Sheet$RID[i]) & 
        (ADNI1_Summary_Sheet$VISCODE==ADNI1_Sheet$VISCODE[i])]}
  else
  {Q7_SCORE[i] = ADNI2_Sheet$Q7SCORE[(i - dim(ADNI1_Sheet)[1])];}
  if((length(response_vec)==1) & (response_vec[1] == -4)){
    Q71[i]=-4; Q72[i]=-4; Q73[i]=-4; Q74[i]=-4; Q75[i]=-4; Q76[i]=-4;
    Q77[i]=-4; Q78[i]=-4; Q79[i]=-4;
    if(is.na(Q7_SCORE[i])){ Q7_SCORE[i] = -4; }
  }
}

#-------------------------------------------------------------------------------
# Q8: Word Recognition Task
Q8 <- c()
for (i in 1:(dim(ADNI1_Sheet)[1])){
  Q8[i] = ADNI1_Summary_Sheet$Q8[
    (ADNI1_Summary_Sheet$RID==ADNI1_Sheet$RID[i])
    & (ADNI1_Summary_Sheet$VISCODE==ADNI1_Sheet$VISCODE[i])]  }
Q8_SCORE <- c(Q8, ADNI2_Sheet$Q8SCORE)

#-------------------------------------------------------------------------------
# Q9 - Instructions
Q9_SCORE = c((ADNI1_Sheet$COINSTRC-1), ADNI2_Sheet$Q9SCORE);

#-------------------------------------------------------------------------------
# Q10 - Comprehension (corresponds to Q12 in ADASSCORES sheet)
Q10_SCORE = c((ADNI1_Sheet$COCOMPRE-1), ADNI2_Sheet$Q10SCORE);

#-------------------------------------------------------------------------------
# Q11 - Word finding
Q11_SCORE = c((ADNI1_Sheet$COWRDFND-1), ADNI2_Sheet$Q11SCORE);

#-------------------------------------------------------------------------------
# Q12 - Spoken Language  (corresponds to Q10 in ADASSCORES sheet) 
Q12_SCORE = c((ADNI1_Sheet$COLANG-1), ADNI2_Sheet$Q12SCORE);

# compile all response data in a data frame
ADNI_ADAS_Data = data.frame(RID = RID_comb, VISCODE = Vcode_comb, 
                          Q1_SCORE,  
                          'Q3_SCORE' = Q2_SCORE, 'Q31' = (1-Q21), 
                          'Q32' = (1-Q22), 'Q33'=(1-Q23), 
                          'Q34'=(1-Q24), 'Q35'=(1-Q25), 'Q36'=(1-Q26), 
                          'Q4_SCORE'= Q3_SCORE, 'Q41'=(1-Q31), 'Q42'=(1-Q32), 
                          'Q43'=(1-Q33), 'Q44'=(1-Q34), 'Q45'=(1-Q35), 
                          'Q12_SCORE'= Q4_SCORE, 
                          'Q2_SCORE'= Q5_SCORE, 'Q21'=(1-Q51), 'Q22'=(1-Q52),
                          'Q23'=(1-Q53), 'Q24'=(1-Q54), 'Q25'=(1-Q55), 
                          'Q26'=(1-Q56), 'Q27'=(1-Q57), 'Q28'=(1-Q58), 
                          'Q29'=(1-Q59), 'Q210'=(1-Q510), 'Q211'=(1-Q511), 
                          'Q212'=(1-Q512), 
                          'Q213'=(1-Q513), 'Q214'=(1-Q514), 'Q215'=(1-Q515),
                          'Q216'=(1-Q516), 'Q217'=(1-Q517), 'Q218'=(1-Q518), 
                          'Q5_SCORE'=Q6_SCORE, 'Q51'=(1-Q61), 'Q52'=(1-Q62), 
                          'Q53'=(1-Q63), 
                          'Q54'=(1-Q64), 'Q55'=(1-Q65), 'Q56'=(1-Q66),
                          'Q6_SCORE'=Q7_SCORE, 'Q61'=(1-Q71), 'Q62'=(1-Q72), 
                          'Q63'=(1-Q73), 'Q64'=(1-Q74), 'Q65'=(1-Q75), 
                          'Q66'=(1-Q76), 'Q67'=(1-Q77), 
                          'Q68'=(1-Q78), 'Q69'=(1-Q79), 
                          'Q7_SCORE'=Q8_SCORE, 
                          'Q11_SCORE'=Q9_SCORE, 
                          'Q9_SCORE'=Q10_SCORE, 'Q10_SCORE'=Q11_SCORE, 
                          'Q8_SCORE'=Q12_SCORE)

# Save dataframe
save(file = '..//RData/ADNI_ADAS_Data.RData', list = c('ADNI_ADAS_Data'))
