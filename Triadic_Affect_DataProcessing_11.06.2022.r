library(ggplot2)
library(dplyr)
library(psych)
library(readxl)
library(tictoc)
library(DescTools)
library(sjPlot)
library(sjmisc)
library(car)
library(stringr)
library(MplusAutomation)
library(jtools)
library(stringr)
library(rempsyc)
library(flextable)
library(haven)


# Pre-Processing ####

#gc(rm(list=ls()))

data <- read.csv("data_6pp_Microcoded_FreePlay_CategoricalTimeSeries_1sec_BY_PERSON_AND_DIMENSION.csv", na.strings = "NA")[,-1]
sensitivity <- read.csv("qualitative data final_clean.csv")
names(sensitivity) <- c("id","qual_mom","qual_dad")
baseline <- read_excel("HATCH 6.4.2021 with time with baby.xlsx")
baseline_spss <- read_sav("HATCH 6.4.2021 with time with baby.sav")


data <- data %>% select(id, new_time, affect_Mom, affect_Dad, affect_Inf)
data <- data[rowSums(is.na(data)) < 3,]

data_list <- split(data, data$id)
# Compute the proportion of affect codes that are missing
prop_missing <- matrix(NA, nrow = length(unique(data$id)), ncol = 4)
for(i in 1:length(unique(data$id))){
  df <- data_list[[i]]
  id <- unique(df$id)
  dad <- sum(df$affect_Dad =="au", na.rm = TRUE)/length(df$affect_Dad)
  mom <- sum(df$affect_Mom =="au", na.rm = TRUE)/length(df$affect_Mom)
  infant <- sum(df$affect_Inf =="au", na.rm = TRUE)/length(df$affect_Inf)
  fam <- c(id,dad,mom,infant)
  prop_missing[i,] <- fam
}
prop_missing <- data.frame(prop_missing)
names(prop_missing) <- c("id", "dad", "mom","infant")
prop_missing <- round(prop_missing, digits = 2)
prop_missing

mean(prop_missing$dad)
mean(prop_missing$mom)
mean(prop_missing$infant)

data_list <- data_list[prop_missing$dad <.70  & prop_missing$mom < .70 & prop_missing$infant < .70]

# Grab the baseline measures of interest

baseline_s <- baseline %>% select(CoupID, Education.1, Education.2, Age_1.1, Age_1.2, #Parental age and education
                                  bage6pp, Baby.sex, ZBirthweightgr2, ZGestationalAgeWeeks, # Infant Age, Sex, Birth Weight, and Gestational Age
                                  sur.1, sur.2, #Infant Positive Affectivity/surgency
                                  neg.1, neg.2, # Infant negative affectivity
                                  eff.1, eff.2, # Infant orienting and regulatory capacity
                                  BCQstructure.1, BCQstructure.2, # Baby Care Q; "Structure"
                                  BCQattunement.1, BCQattunement.2, #Baby Care Q; "Attunement"
                                  # See Winstanley & Grattis, 2013 for BCQ
                                  EPDS1pp, EPDS2pp, #Postpartum Depression
                                  PSI.tot.1, PSI.tot.2, #Parenting stress index
                                  PSS.1pp, PSS.2pp, #Percieved Stress scale
                                  MAI_Tot.1, MAI_Tot.2, #Maternal/Paternal Attachment,
                                  PSQI_tot.1pp, PSQI_tot.2pp, #Sleep Quality Index
                                  momhairPPconverted.trunc, #Mom Hair cortisol
                                  dadhairPPconverted.trunc, #Dad Hair cortisol
                                  infhairPPconverted.trunc, #Infant hair cortisol
                                  prolactinPP.1.00, prolacPP.1.trunc2sd, #Prolactin in Mom
                                  prolactinPP.2.00, prolacPP.2.trunc2sd, #Prolactin in Dad
                                  OTextractPP.1.00, OTextractPP.1.t2sd, #Oxytocin in Mom
                                  OTextractPP.2.00, OTextractPP.2.t2sd, #Oxytocin in Dad
                                  Testo1pp.1.00, Testo1pp.1trunc, #Mom Testo at time 1
                                  Testo3pp.1.00, Testo3pp.1trunc, #Mom Testo at time 3
                                  Testo6pp.1.00, Testo6pp.1trunc, #Mom Testo at time 6
                                  Testo1pp.2.00, Testo1pp.2.trunc, #Dad Testo at time 1
                                  Testo3pp.2.00, Testo3pp.2.trunc, #Dad Testo at time 3
                                  Testo6pp.2.00, Testo6pp.2.trunc, #Dad Testo at time 6
                                  fcort1pp, fcort1ppt, #Mom Cortisol at time 1
                                  fcort2pp, fcort2ppt, #Mom Cortisol at time 2
                                  fcort3pp, fcort3ppt, #Mom Cortisol at time 3
                                  fcort4pp, fcort4ppt, #Mom Cortisol at time 4
                                  fcort5pp, fcort5ppt, #Mom Cortisol at time 5
                                  fcort6pp, fcort6ppt, #Mom Cortisol at time 6
                                  mcort1pp, mcort1ppt, #Dad Cortisol at time 1
                                  mcort2pp, mcort2ppt, #Dad Cortisol at time 2
                                  mcort3pp, mcort3ppt, #Dad Cortisol at time 3
                                  mcort4pp, mcort4ppt, #Dad Cortisol at time 4
                                  mcort5pp, mcort5ppt, #Dad Cortisol at time 5
                                  mcort6pp, mcort6ppt, #Dad Cortisol at time 6
                                  Anx1pp, Anx2pp # Anxiety at 6 months pp
                                  )


baseline_s$PSI.tot.1 <- scale(baseline_s$PSI.tot.1, center = TRUE, scale =TRUE)[,1]
baseline_s$PSI.tot.2 <- scale(baseline_s$PSI.tot.2, center = TRUE, scale =TRUE)[,1]

baseline_s$MAI_Tot.1 <- scale(baseline_s$MAI_Tot.1, center = TRUE, scale =TRUE)[,1]
baseline_s$MAI_Tot.2 <- scale(baseline_s$MAI_Tot.2, center = TRUE, scale =TRUE)[,1]

baseline_s$Anx1pp <- scale(baseline_s$Anx1pp, center = TRUE, scale =TRUE)[,1]
baseline_s$Anx2pp <- scale(baseline_s$Anx2pp, center = TRUE, scale =TRUE)[,1]

baseline_s$neg.1 <- scale(baseline_s$neg.1, center = TRUE, scale =TRUE)[,1]
baseline_s$neg.2 <- scale(baseline_s$neg.2, center = TRUE, scale =TRUE)[,1]

baseline_s$PSS.1pp <- scale(baseline_s$PSS.1pp, center = TRUE, scale =TRUE)[,1]
baseline_s$PSS.2pp <- scale(baseline_s$PSS.2pp, center = TRUE, scale =TRUE)[,1]



##Female
baseline_s$Testo.pp.1.trunc.AUC <- 1
baseline_s$Testo.pp.1.trunc.Avg <- 1
baseline_s$Testo.pp.1.trunc.CountMissing <- 1
baseline_s$Cort.pp.1.trunc.AUC <- 1
baseline_s$Cort.pp.1.trunc.Avg <- 1
baseline_s$Cort.pp.1.trunc.CountMissing <- 1


for(i in 1:nrow(baseline_s)){
  baseline_s$Testo.pp.1.trunc.AUC[i] <- AUC(x = c(1,2,3), y = c(baseline_s$Testo1pp.1trunc[i],
                                                           baseline_s$Testo3pp.1trunc[i],
                                                           baseline_s$Testo6pp.1trunc[i]), na.rm = TRUE)
  
  baseline_s$Testo.pp.1.trunc.Avg[i] <- mean(c(baseline_s$Testo1pp.1trunc[i],
                                          baseline_s$Testo3pp.1trunc[i],
                                          baseline_s$Testo6pp.1trunc[i]), na.rm = TRUE)
  baseline_s$Testo.pp.1.trunc.CountMissing[i] <- sum(is.na(c(baseline_s$Testo1pp.1trunc[i],
                                                        baseline_s$Testo3pp.1trunc[i],
                                                        baseline_s$Testo6pp.1trunc[i])))
  
  
  baseline_s$Cort.pp.1.trunc.AUC[i] <- AUC(x = c(1,2,3,4,5,6), y = c(baseline_s$fcort1ppt[i],
                                                                baseline_s$fcort2ppt[i],
                                                                baseline_s$fcort3ppt[i],
                                                                baseline_s$fcort4ppt[i],
                                                                baseline_s$fcort5ppt[i],
                                                                baseline_s$fcort6ppt[i]), na.rm = TRUE)
  
  baseline_s$Cort.pp.1.trunc.Avg[i] <- mean(c(baseline_s$fcort1ppt[i],
                                         baseline_s$fcort2ppt[i],
                                         baseline_s$fcort3ppt[i],
                                         baseline_s$fcort4ppt[i],
                                         baseline_s$fcort5ppt[i],
                                         baseline_s$fcort6ppt[i]), na.rm = TRUE)
  baseline_s$Cort.pp.1.trunc.CountMissing[i] <- sum(is.na(c(baseline_s$fcort1ppt[i],
                                                       baseline_s$fcort2ppt[i],
                                                       baseline_s$fcort3ppt[i],
                                                       baseline_s$fcort4ppt[i],
                                                       baseline_s$fcort5ppt[i],
                                                       baseline_s$fcort6ppt[i])))
  
}


##Male
baseline_s$Testo.pp.2.trunc.AUC <- 1
baseline_s$Testo.pp.2.trunc.Avg <- 1
baseline_s$Testo.pp.2.trunc.CountMissing <- 1
baseline_s$Cort.pp.2.trunc.AUC <- 1
baseline_s$Cort.pp.2.trunc.Avg <- 1
baseline_s$Cort.pp.2.trunc.CountMissing <- 1

for(i in 1:nrow(baseline_s)){
  baseline_s$Testo.pp.2.trunc.AUC[i] <- AUC(x = c(1,2,3), y = c(baseline_s$Testo1pp.2.trunc[i],
                                                           baseline_s$Testo3pp.2.trunc[i],
                                                           baseline_s$Testo6pp.2.trunc[i]), na.rm = TRUE)
  baseline_s$Testo.pp.2.trunc.Avg[i] <- mean(c(baseline_s$Testo1pp.2.trunc[i],
                                          baseline_s$Testo3pp.2.trunc[i],
                                          baseline_s$Testo6pp.2.trunc[i]), na.rm = TRUE)
  baseline_s$Testo.pp.2.trunc.CountMissing[i] <- sum(is.na(c(baseline_s$Testo1pp.2.trunc[i],
                                                        baseline_s$Testo3pp.2.trunc[i],
                                                        baseline_s$Testo6pp.2.trunc[i])))
  
  baseline_s$Cort.pp.2.trunc.AUC[i] <- AUC(x = c(1,2,3,4,5,6), y = c(baseline_s$mcort1ppt[i],
                                                                baseline_s$mcort2ppt[i],
                                                                baseline_s$mcort3ppt[i],
                                                                baseline_s$mcort4ppt[i],
                                                                baseline_s$mcort5ppt[i],
                                                                baseline_s$mcort6ppt[i]), na.rm = TRUE)
  
  baseline_s$Cort.pp.2.trunc.Avg[i] <- mean(c(baseline_s$mcort1ppt[i],
                                         baseline_s$mcort2ppt[i],
                                         baseline_s$mcort3ppt[i],
                                         baseline_s$mcort4ppt[i],
                                         baseline_s$mcort5ppt[i],
                                         baseline_s$mcort6ppt[i]), na.rm = TRUE)
  baseline_s$Cort.pp.2.trunc.CountMissing[i] <- sum(is.na(c(baseline_s$mcort1ppt[i],
                                                       baseline_s$mcort2ppt[i],
                                                       baseline_s$mcort3ppt[i],
                                                       baseline_s$mcort4ppt[i],
                                                       baseline_s$mcort5ppt[i],
                                                       baseline_s$mcort6ppt[i])))
  
}

baseline_s$sur <- 1
baseline_s$neg <- 1
baseline_s$eff <- 1

for(i in 1:nrow(baseline_s)){
 baseline_s$sur[i] <- mean(c(baseline_s$sur.1[i],
                             baseline_s$sur.2[i]),
                             na.rm = TRUE) 
 baseline_s$neg[i] <- mean(c(baseline_s$neg.1[i],
                             baseline_s$neg.2[i]),
                           na.rm = TRUE) 
 baseline_s$eff[i] <- mean(c(baseline_s$eff.1[i],
                             baseline_s$eff.2[i]),
                           na.rm = TRUE) 
}

baseline_s <- baseline_s %>% dplyr::mutate_all(~ifelse(is.nan(.), NA, .))
colnames(baseline_s)[1] <- "id"

baseline_final <- inner_join(baseline_s, id_data, "id")

data <- left_join(data, sensitivity, "id")
data <- left_join(data, baseline_s, "id")



##Dad
convert_affect <- function(data1){
  data <- data1
  data$affect_Dad_binary <- data$affect_Dad
  data$affect_Dad_binary <- ifelse(data$affect_Dad=="aw"|data$affect_Dad=="aa", 0,
                                   data$affect_Dad_binary)
  data$affect_Dad_binary <- ifelse(data$affect_Dad=="aph", 1,
                                   data$affect_Dad_binary)
  data$affect_Dad_binary <- ifelse(data$affect_Dad=="apl", 1,
                                   data$affect_Dad_binary)
  data$affect_Dad_binary <- ifelse(data$affect_Dad=="an", 0,
                                   data$affect_Dad_binary)
  data$affect_Dad_binary <- ifelse(data$affect_Dad=="au", NA,
                                   data$affect_Dad_binary)
  
  ##Mom
  data$affect_Mom_binary <- data$affect_Mom
  data$affect_Mom_binary <- ifelse(data$affect_Mom=="aw"|data$affect_Mom=="aa", 0,
                                   data$affect_Mom_binary)
  data$affect_Mom_binary <- ifelse(data$affect_Mom=="apl", 1,
                                   data$affect_Mom_binary)
  data$affect_Mom_binary <- ifelse(data$affect_Mom=="aph", 1,
                                   data$affect_Mom_binary)
  data$affect_Mom_binary <- ifelse(data$affect_Mom=="an", 0,
                                   data$affect_Mom_binary)
  data$affect_Mom_binary <- ifelse(data$affect_Mom=="au", NA,
                                   data$affect_Mom_binary)
  
  ##Infant
  data$affect_Inf_binary_pos <- data$affect_Inf
  data$affect_Inf_binary_pos <- ifelse(data$affect_Inf_binary_pos=="aa", "neg",
                                       data$affect_Inf_binary_pos)
  data$affect_Inf_binary_pos <- ifelse(data$affect_Inf_binary_pos=="an", "neu",
                                       data$affect_Inf_binary_pos)
  data$affect_Inf_binary_pos <- ifelse(data$affect_Inf_binary_pos=="au", "un",
                                       data$affect_Inf_binary_pos)
  data$affect_Inf_binary_pos <- ifelse(data$affect_Inf_binary_pos=="ap", "pos",
                                       data$affect_Inf_binary_pos)
  data$affect_Inf_binary_pos <- ifelse(data$affect_Inf_binary_pos=="pos",1,0)
  
  data$affect_Inf_binary_neg <- data$affect_Inf
  data$affect_Inf_binary_neg <- ifelse(data$affect_Inf_binary_neg=="aa", "neg",
                                       data$affect_Inf_binary_neg)
  data$affect_Inf_binary_neg <- ifelse(data$affect_Inf_binary_neg=="an", "neu",
                                       data$affect_Inf_binary_neg)
  data$affect_Inf_binary_neg <- ifelse(data$affect_Inf_binary_neg=="au", "un",
                                       data$affect_Inf_binary_neg)
  data$affect_Inf_binary_neg <- ifelse(data$affect_Inf_binary_neg=="ap", "pos",
                                       data$affect_Inf_binary_neg)
  data$affect_Inf_binary_neg <- ifelse(data$affect_Inf_binary_neg=="neg",1,0)
  
  data$affect_Inf_binary_neg <- as.factor(data$affect_Inf_binary_neg)
  data$affect_Inf_binary_pos <- as.factor(data$affect_Inf_binary_pos)

  data$affect_Mom_binary <- as.numeric(data$affect_Mom_binary)
  data$affect_Dad_binary <- as.numeric(data$affect_Dad_binary)
  
  data$affect_Inf_ord <- data$affect_Inf
  data$affect_Inf_ord <- ifelse(data$affect_Inf_ord == "aa",
                                "0",
                                data$affect_Inf_ord)
  data$affect_Inf_ord <- ifelse(data$affect_Inf_ord == "an",
                                "1",
                                data$affect_Inf_ord)
  data$affect_Inf_ord <- ifelse(data$affect_Inf_ord == "ap",
                                "2",
                                data$affect_Inf_ord)
  data$affect_Inf_ord <- ifelse(data$affect_Inf_ord == "au",
                                NA,
                                data$affect_Inf_ord)
  data$affect_Inf_ord <- as.numeric(data$affect_Inf_ord)
  
  data$affect_MomDad_Joint <-ifelse(data$affect_Dad_binary == 1 & data$affect_Mom_binary == 1, 1, 0)
  data$affect_MomInf_Joint <-ifelse(data$affect_Inf_binary_pos == 1 & data$affect_Mom_binary == 1, 1, 0)
  data$affect_DadInf_Joint <-ifelse(data$affect_Inf_binary_pos == 1 & data$affect_Dad_binary == 1, 1, 0)
  
  data <-
    data %>% 
    group_by(id) %>% 
    mutate(affect_Mom_binary_c = mean(affect_Mom_binary, na.rm = TRUE),
           affect_Dad_binary_c = mean(affect_Dad_binary, na.rm = TRUE),
           affect_Dad_binary_w = (affect_Dad_binary - mean(affect_Dad_binary, na.rm = TRUE)),
           affect_Mom_binary_w = (affect_Mom_binary - mean(affect_Mom_binary, na.rm = TRUE))) %>% 
    ungroup()
  
  return(data)
}

data <- convert_affect(data)

#Rename variables as needed

#dput(names(data))

data_for_mplus <- data %>% select(id,
                                  new_time,
                                  bage6pp,
                                  Baby.sex,
                                  ZBirthweightgr2,
                                  ZGestationalAgeWeeks, 
                                  affect_Mom_binary,
                                  affect_Dad_binary,
                                  affect_Inf_ord,
                                  momhairPPconverted.trunc, #Mom Hair cortisol
                                  dadhairPPconverted.trunc, #Dad Hair cortisol
                                  infhairPPconverted.trunc, #Infant hair cortisol
                                  OTextractPP.1.t2sd, #Oxytocin in Mom
                                  OTextractPP.2.t2sd, #Oxytocin in Dad
                                  EPDS1pp, EPDS2pp, #Postpartum Depression
                                  PSI.tot.1, PSI.tot.2, #Parenting stress index
                                  Anx1pp, Anx2pp, #State-trait anxiety scale
                                  PSS.1pp, PSS.2pp, #Perceived Stress Scale
                                  neg.1, neg.2, #Infant's negative affectivity
                                  MAI_Tot.1, MAI_Tot.2 #Maternal/paternal attachment
                                  )

data_for_mplus <- data_for_mplus %>%        
  group_by(id) %>%
  dplyr::mutate(mom_lag1 = dplyr::lag(affect_Mom_binary, n = 1, default = NA),
                dad_lag1 = dplyr::lag(affect_Dad_binary, n = 1, default = NA),
                inf_lag1 = dplyr::lag(affect_Inf_ord, n = 1, default = NA)) %>% 
  as.data.frame()

newNames <- c("id",
              "time", "age_i", "sex_i", "b_weight","z_gest",
              "aff_m","aff_d","aff_i",
              "HCC.m", "HCC.d", "HCC.inf", 
              "OT.m","OT.d", "EPDS.m","EPDS.D",
              "PSI.m","PSI.d",
              "Anx.m", "Anx.d",
              "PSS.m", "PSS.d", #Perceived Stress Scale
              "neg.m", "neg.d", #Infant's negative affectivity
              "mai.m", "mai.d", #Maternal/paternal attachment
              "mom_lag1", "dad_lag1", "inf_lag1")
names(data_for_mplus) <- newNames


list_data <- split(data_for_mplus, data_for_mplus$id)
seconds <- c()
for(i in 1:length(list_data)){
  seconds[i] <- max(list_data[[i]]$time)
}
seconds
mean(seconds)
mean(seconds/60)

length(data_for_mplus$time)

#Infant age
mean(unique(data$bage6pp))
sd(unique(data$bage6pp))

avg_emot <- data %>% select(id, affect_Mom, affect_Dad, affect_Inf)
avg_emot$affect_Mom <- ifelse(avg_emot$affect_Mom=="au", NA,
                         avg_emot$affect_Mom)
avg_emot$affect_Dad <- ifelse(avg_emot$affect_Dad=="au", NA,
                         avg_emot$affect_Dad)
avg_emot$affect_Dad <- ifelse(avg_emot$affect_Dad=="na", NA,
                         avg_emot$affect_Dad)
avg_emot$affect_Mom <- ifelse(avg_emot$affect_Mom=="aw", "aa",
                         avg_emot$affect_Mom)
avg_emot$affect_Dad <- ifelse(avg_emot$affect_Dad=="aw", "aa",
                         avg_emot$affect_Dad)
avg_emot$affect_Inf <- ifelse(avg_emot$affect_Inf=="au", NA,
                         avg_emot$affect_Inf)
avg_emot$affect_Mom <- ifelse(avg_emot$affect_Mom=="aph", "apl",
                         avg_emot$affect_Mom)
avg_emot$affect_Dad <- ifelse(avg_emot$affect_Dad=="aph", "apl",
                         avg_emot$affect_Dad)

list_data <- split(avg_emot, avg_emot$id)

desc <- matrix(data = NA, ncol = 13, nrow = length(list_data))
for(i in 1:length(list_data)){
  desc[i,1] <- list_data[[i]][1,1][[1]]
  desc[i,2] <- mean(is.na(list_data[[i]][,2][[1]]), na.rm = TRUE) #Proportion Missing
  desc[i,3] <- mean(is.na(list_data[[i]][,3][[1]]), na.rm = TRUE)
  desc[i,4] <- mean(is.na(list_data[[i]][,4][[1]]), na.rm = TRUE)
  
  desc[i,5] <- mean(list_data[[i]][,2][[1]]=="aa", na.rm = TRUE) #Proportion Negative
  desc[i,6] <- mean(list_data[[i]][,3][[1]]=="aa", na.rm = TRUE)
  desc[i,7] <- mean(list_data[[i]][,4][[1]]=="aa", na.rm = TRUE)
  
  desc[i,8] <- mean(list_data[[i]][,2][[1]]=="an", na.rm = TRUE) #Proportion Neutral
  desc[i,9] <- mean(list_data[[i]][,3][[1]]=="an", na.rm = TRUE)
  desc[i,10] <- mean(list_data[[i]][,4][[1]]=="an", na.rm = TRUE)
  
  desc[i,11] <- mean(list_data[[i]][,2][[1]]=="apl", na.rm = TRUE) #Proportion Positive
  desc[i,12] <- mean(list_data[[i]][,3][[1]]=="apl", na.rm = TRUE)
  desc[i,13] <- mean(list_data[[i]][,4][[1]]=="ap",  na.rm = TRUE)
}
desc <- as.data.frame(desc)
names(desc) <- c("id",
                 "aff.M.NA", "aff.D.NA", "aff.I.NA",
                 "aff.M.AA", "aff.D.AA", "aff.I.AA",
                 "aff.M.AN", "aff.D.AN", "aff.I.AN",
                 "aff.M.AP", "aff.D.AP", "aff.I.AP")

desc_joined <- left_join(desc, sensitivity, "id")
desc_joined <- left_join(desc_joined, baseline_s, "id")

t.test(desc_joined$PSI.tot.1, desc_joined$PSI.tot.2, paired = TRUE)
t.test(desc_joined$momhairPPconverted.trunc, desc_joined$dadhairPPconverted.trunc, paired = TRUE)


mean(desc$aff.M.AA==0)
mean(desc$aff.D.AA==0)
mean(desc$aff.I.AA<.03)

t.test(desc$aff.M.AP, desc$aff.D.AP, paired = TRUE)
mean(desc$aff.M.AP)
mean(desc$aff.D.AP)

library(correlation)
library(see) # for plotting
library(ggraph) # needs to be loaded

corrp <- desc_joined %>% select(momhairPPconverted.trunc, dadhairPPconverted.trunc, infhairPPconverted.trunc,
                                PSI.tot.1, PSI.tot.2, aff.M.AP, aff.D.AP, aff.I.AP)
names(corrp)<- c("MomHCC","DadHCC","InfHCC",
                 "MomPSI","DadPSI", "AvgPA.Mom", "AvgPA.Dad", "AvgPA.Infant")

plot(correlation(corrp, partial = TRUE)) +
  scale_edge_color_continuous(low = "#000004FF", high = "#FCFDBFFF")

plot(correlation(corrp, partial = FALSE)) +
  scale_edge_color_continuous(low = "#000004FF", high = "#FCFDBFFF")

corr.results <- correlation(corrp)

corr.results %>%
  summary(redundant = FALSE) %>%
  plot()

corrplot(corrp)


ggplot(desc, aes(x=aff.I.AP)) + 
  geom_histogram(color="black", fill="gray80") +
 # theme_apa() +
  xlab("Infant Positive Affect") +
  ylab("Frequency")


ggplot(desc, aes(x=aff.I.AP)) + 
  geom_histogram(color="black", fill="gray80") +
#  theme_apa() +
  xlab("Infant Positive Affect") +
  ylab("Frequency")

summ(lm(qual_mom ~ aff.M.AP, data = desc_joined), digits = 5)
summ(lm(qual_dad ~ aff.D.AP, data = desc_joined), digits = 5)
# 
# summ(lm(sur.1 ~ aff.i.or, data = desc_joined), digits = 5)
# summ(lm(sur.2 ~ aff.i.or, data = desc_joined), digits = 5)
# summ(lm(eff.1 ~ aff.i.or, data = desc_joined), digits = 5)
# summ(lm(eff.2 ~ aff.i.or, data = desc_joined), digits = 5)
# summ(lm(neg.1 ~ aff.i.or, data = desc_joined), digits = 5)
# summ(lm(neg.2 ~ aff.i.or, data = desc_joined), digits = 5)
# 
pairs.panels(desc[,2:4],
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)

df.corrp <- describe(corrp)
df.corrp <- df.corrp %>% select(n, mean, sd)
df.corrp <- round(df.corrp, digits = 2)
