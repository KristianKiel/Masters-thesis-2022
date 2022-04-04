
#Loading packages 
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(tidyverse)
library(stargazer)
library(readr)
library(dplyr)
library(caret)
library(expss)
library(MASS)
library(devtools)
library(cowplot)
library(urbnmapr)
library(usmap)
library(readxl)
library(broom)
library(psych)
library(haven)
library(stringr)
library(tidyr)
library(rio)
library(matrixStats)
library(naniar)
library(zoo)
library(lfe)
library(aod)
library(lmtest)
library(rstatix)
library(ivreg)
library(ggridges)
library(writexl)
library(xlsx)
library(mifa)
library(mice)
library(lazyeval)
library(estimatr)
library(Rcpp)
library(RcppEigen)
library(glue)
library(GPArotation)
library(knitr)
library(cregg)
library(cjpowR)
library(devtools)
library(plotly)
library(hrbrthemes)
library(Hmisc)
library(NCmisc)


#1.1 ############## Loading data ######################
setwd("/Users/kristiankiel/Documents/Statskundskab/KU /11. Semester (Speciale)/Masters thesis")

#Loading initial set of conjoint tasks 
choice_df<-read.csv2(file="./data/conjoint_set_1.csv")

#Loading second set of conjoint tasks 
choice_df_2<-read.csv2(file="./data/conjoint_set_2.csv")

#Loading data for making figure of diseases from Petersen et al (2019)
disease_df<-read.csv2(file="./data/Diseases.csv")

#1.2 ############# Initial datacleaning #########

#Making numeric variable for age,excluding people below 18 
choice_df<-choice_df%>%
  mutate(Q4_ALDER=as.numeric(Q4_ALDER))%>%
  filter(Q4_ALDER>=18)

#Making new variable for education. Note that data is pivoted, due to wide format from Conjoint.ly
choice_df = subset(choice_df, select = -c(Q5_UDDANNELSE_O9_ANDEN_UDDANNELSE_ANGIV_VENLIGST) )

#Pivoting to long format 
choice_df<-choice_df%>%
  pivot_longer(
    cols = starts_with("Q5_UDDANNELSE_O"),
    names_to = "Uddannelsesniveau",
    values_to= "Uddannelsesniveau_d",
    names_prefix = "Q5_UDDANNELSE_O")

#Returns educational level if initial dichotomous value is 1 
choice_df<-choice_df%>%
  mutate(Uddannelsesniveau_cat=case_when(Uddannelsesniveau_d==1~Uddannelsesniveau))

#Dropping NA 
choice_df<-choice_df%>%
  dplyr::filter(!is.na(Uddannelsesniveau_cat))

#Making a dichotomous variable, where 1 is further education, and 0 is not
choice_df<-choice_df%>%
  mutate(Edu_cat_num=case_when(Uddannelsesniveau_cat=="1_INGEN_UDDANNELSE"~0,
                               Uddannelsesniveau_cat=="2_FOLKESKOLEEKSAMEN_MELLEMSKOLE_REALEKSAMEN_ELLER_LIGNENDE_GRUNDSKOLE"~0,
                               Uddannelsesniveau_cat=="3_ERHVERVSFAGLIG_UDDANNELSE_EUD_EUX_FAGL_RT"~0,
                               Uddannelsesniveau_cat=="4_STUDENTEREKSAMEN_STX_HHX_HTX_ELLER_HF"~0,
                               Uddannelsesniveau_cat=="5_KORT_VIDEREG_ENDE_UDDANNELSE_UNDER_3_R_INSTALLAT_R_POLITIBETJENT_LABORANT_MV"~1,
                               Uddannelsesniveau_cat=="6_MELLEMLANG_VIDEREG_ENDE_UDDANNELSE_FOLKESKOLEL_RER_SYGEPLEJERSKE_P_DAGOG_DIPLOMINGENI_R_MV"~1,
                               Uddannelsesniveau_cat=="7_BACHELORUDDANNELSE_FOR_EKSEMPEL_1_DEL_AF_EN_LANG_VIDEREG_ENDE_UDDANNELSE"~1,
                               Uddannelsesniveau_cat=="8_LANG_VIDEREG_ENDE_UDDANNELSE_L_GE_ADVOKAT_CIVILINGENI_R_MV"~1
  ))

#Making a similar variable, but as a factor 
choice_df<-choice_df%>%
  mutate(Edu_cat_dich=case_when(Uddannelsesniveau_cat=="1_INGEN_UDDANNELSE"~"No further education",
                               Uddannelsesniveau_cat=="2_FOLKESKOLEEKSAMEN_MELLEMSKOLE_REALEKSAMEN_ELLER_LIGNENDE_GRUNDSKOLE"~"No further education",
                               Uddannelsesniveau_cat=="3_ERHVERVSFAGLIG_UDDANNELSE_EUD_EUX_FAGL_RT"~"No further education",
                               Uddannelsesniveau_cat=="4_STUDENTEREKSAMEN_STX_HHX_HTX_ELLER_HF"~"No further education",
                               Uddannelsesniveau_cat=="5_KORT_VIDEREG_ENDE_UDDANNELSE_UNDER_3_R_INSTALLAT_R_POLITIBETJENT_LABORANT_MV"~"Further education",
                               Uddannelsesniveau_cat=="6_MELLEMLANG_VIDEREG_ENDE_UDDANNELSE_FOLKESKOLEL_RER_SYGEPLEJERSKE_P_DAGOG_DIPLOMINGENI_R_MV"~"Further education",
                               Uddannelsesniveau_cat=="7_BACHELORUDDANNELSE_FOR_EKSEMPEL_1_DEL_AF_EN_LANG_VIDEREG_ENDE_UDDANNELSE"~"Further education",
                               Uddannelsesniveau_cat=="8_LANG_VIDEREG_ENDE_UDDANNELSE_L_GE_ADVOKAT_CIVILINGENI_R_MV"~"Further education"
  ))

#Translating educational levels in detail 
choice_df<-choice_df%>%
  mutate(Edu_cat=case_when(Uddannelsesniveau_cat=="1_INGEN_UDDANNELSE"~"No Education",
                           Uddannelsesniveau_cat=="2_FOLKESKOLEEKSAMEN_MELLEMSKOLE_REALEKSAMEN_ELLER_LIGNENDE_GRUNDSKOLE"~"Primary Education",
                           Uddannelsesniveau_cat=="3_ERHVERVSFAGLIG_UDDANNELSE_EUD_EUX_FAGL_RT"~"Vocational training ",
                           Uddannelsesniveau_cat=="4_STUDENTEREKSAMEN_STX_HHX_HTX_ELLER_HF"~"High school degree",
                           Uddannelsesniveau_cat=="5_KORT_VIDEREG_ENDE_UDDANNELSE_UNDER_3_R_INSTALLAT_R_POLITIBETJENT_LABORANT_MV"~"Short cycle education",
                           Uddannelsesniveau_cat=="6_MELLEMLANG_VIDEREG_ENDE_UDDANNELSE_FOLKESKOLEL_RER_SYGEPLEJERSKE_P_DAGOG_DIPLOMINGENI_R_MV"~"Professional bachelor",
                           Uddannelsesniveau_cat=="7_BACHELORUDDANNELSE_FOR_EKSEMPEL_1_DEL_AF_EN_LANG_VIDEREG_ENDE_UDDANNELSE"~"Bachelor's degree",
                           Uddannelsesniveau_cat=="8_LANG_VIDEREG_ENDE_UDDANNELSE_L_GE_ADVOKAT_CIVILINGENI_R_MV"~"Master's degree"
  ))

#Making dichotomous gender variable. MAN is 1, WOMAN is 0
choice_df<-choice_df%>%
  mutate(Kon_dich=case_when(Q3_K_N_O1_MAND==1~1,
                            Q3_K_N_O2_KVINDE==1~0))

#Making factor gender variable 
choice_df<-choice_df%>%
  mutate(Kon_cat=case_when(Q3_K_N_O1_MAND==1~"Men",
                            Q3_K_N_O2_KVINDE==1~"Women"))
choice_df$Kon_cat<-as.factor(choice_df$Kon_cat)

#making new variable for demographics
choice_df<-choice_df%>%
  pivot_longer(
    cols = starts_with("Q9_DEMOGRAFI_"),
    names_to = "Demografi",
    values_to= "Demografi_d",
    names_prefix = "Q9_DEMOGRAFI_")

choice_df<-choice_df%>%
  mutate(Demografi_cat=case_when(Demografi_d==1~Demografi))

choice_df<-choice_df%>%
  filter(!is.na(Demografi_cat))

#Trasnslating demographic categories 
choice_df<-choice_df%>%
  mutate(demo_cat=case_when(Demografi_cat=="O1_K_BENHAVN_STORK_BENHAVN"~"Copenhagen metropolitan area",
                            Demografi_cat=="O2_AARHUS_AALBORG_ODENSE"~"Århus, Aalborg or Odense",
                            Demografi_cat=="O3_EN_BY_MED_OVER_40_000_INDBYGGERE"~"City with more than 40.000 residents",
                            Demografi_cat=="O4_EN_BY_MED_20_000_39_999_INDBYGGERE"~"City with between 20.000 & 39.999 residents",
                            Demografi_cat=="O5_EN_BY_MED_10_000_19_999_INDBYGGERE"~"City with between 10.000 & 19.999 residents",
                            Demografi_cat=="O6_EN_BY_MED_1_000_9_999_INDBYGGERE"~"City with between 1.000 & 9.999 residents",
                            Demografi_cat=="O7_EN_BY_MED_UNDER_999_INDBYGGERE"~"City with less than 999 residents"))
#Making variable for political party 
choice_df<-choice_df%>%
  pivot_longer(
    cols = starts_with("Q19_PARTI_"),
    names_to = "Partivalg",
    values_to="Partivalg_d",
    names_prefix = "Q19_PARTI_")

choice_df<-choice_df%>%
  mutate(Partivalg_cat=case_when(Partivalg_d==1~Partivalg))

choice_df<-choice_df%>%
  dplyr::filter(!is.na(Partivalg_cat))

levels(as.factor(choice_df$party_cat))
#Translating party names 
choice_df<-choice_df%>%
  mutate(party_cat=case_when(Partivalg_cat=="O1_A_SOCIALDEMOKRATIET"~"Social Democrats",
                             Partivalg_cat=="O2_B_RADIKALE_VENSTRE"~"Danish Social Liberal Party",
                             Partivalg_cat=="O3_C_DET_KONSERVATIVE_FOLKEPARTI"~"Conservative People's Party",
                             Partivalg_cat=="O4_D_NYE_BORGERLIGE"~"The New Right",
                             Partivalg_cat=="O5_F_SF_SOCIALISTISK_FOLKEPARTI"~"Socialist People's Party",
                             Partivalg_cat=="O6_I_LIBERAL_ALLIANCE"~"Liberal Alliance",
                             Partivalg_cat=="O7_O_DANSK_FOLKEPARTI"~"Danish People's Party",
                             Partivalg_cat=="O8_V_VENSTRE_DANMARKS_LIBERALE_PARTI"~"Venstre",
                             Partivalg_cat=="O9_ENHEDSLISTEN_DE_R_D_GR_NNE"~"Red-Green Alliance",
                             Partivalg_cat=="O10_ALTERNATIVET"~"The Alternative",
                             Partivalg_cat=="O11_G_VEGANERPARTIET"~"Vegan Party",
                             Partivalg_cat=="O12_K_KRISTENDEMOKRATERNE"~"Christian Democrats",
                             Partivalg_cat=="O13_Q_FRIE_GR_NNE_DANMARKS_NYE_VENSTREFL_JSPARTI"~"Independent Greens"))

#Making binned age groups 
choice_df<-choice_df%>%
  mutate(Age_binned=case_when(Q4_ALDER>=18 & Q4_ALDER<=35 ~"18-35",
                              Q4_ALDER>=36 & Q4_ALDER<=53~"36-53",
                              Q4_ALDER>53~"54+"))
#Recodinng variables for sophistication, so that 1 is a right answer, and 0 is a wrong answer 

choice_df<-choice_df%>%
  mutate(soph_1=case_when(Q22_PS2_DRIFT_AF_HOSPITAL_O2_REGIONERNE==1~1,
                          Q22_PS2_DRIFT_AF_HOSPITAL_O3_STATEN==0|Q22_PS2_DRIFT_AF_HOSPITAL_O1_KOMMUNERNE==0~0))

choice_df<-choice_df%>%
  mutate(soph_2=case_when(Q23_PS1_MAI_VILLADSEN_O9_ENHEDSLISTEN_DE_R_D_GR_NNE==1~1,
                          Q23_PS1_MAI_VILLADSEN_O1_A_SOCIALDEMOKRATIET==1 |
                            Q23_PS1_MAI_VILLADSEN_O2_B_RADIKALE_VENSTRE==1|
                            Q23_PS1_MAI_VILLADSEN_O3_C_DET_KONSERVATIVE_FOLKEPARTI==1|
                            Q23_PS1_MAI_VILLADSEN_O4_D_NYE_BORGERLIGE==1|
                            Q23_PS1_MAI_VILLADSEN_O5_F_SF_SOCIALISTISK_FOLKEPARTI==1|
                            Q23_PS1_MAI_VILLADSEN_O6_I_LIBERAL_ALLIANCE==1|
                            Q23_PS1_MAI_VILLADSEN_O7_O_DANSK_FOLKEPARTI==1|
                            Q23_PS1_MAI_VILLADSEN_O8_V_VENSTRE_DANMARKS_LIBERALE_PARTI==1|
                            Q23_PS1_MAI_VILLADSEN_O10_ALTERNATIVET==1|
                            Q23_PS1_MAI_VILLADSEN_O11_G_VEGANERPARTIET==1|
                            Q23_PS1_MAI_VILLADSEN_O12_K_KRISTENDEMOKRATERNE==1|
                            Q23_PS1_MAI_VILLADSEN_O13_Q_FRIE_GR_NNE_DANMARKS_NYE_VENSTREFL_JSPARTI==1~0))

choice_df<-choice_df%>%
  mutate(soph_3=case_when(Q24_PS3_POLICY_SKAT_O1_FOR==1~1,
                          Q24_PS3_POLICY_SKAT_O2_IMOD==1~0))

#Setting 0 to NA for alle features for initial conjoint
choice_df[, 6:10][choice_df[, 6:10] == 0] <- NA

#Translating feature names for initial conjoint 
#Health status 
choice_df<-choice_df%>%
  mutate(HELBREDSTILSTAND=case_when(HELBREDSTILSTAND=="Få eller ingen sygdomsmæssige komplikationer" ~"Few or none sickness-related complications",
                                    HELBREDSTILSTAND=="Funktionsnedsættelse som følge af svær overvægt"~"Disability caused by severe obesity",
                                    HELBREDSTILSTAND=="Lider af knogleskørhed"~"Suffering from osteoporosis",
                                    HELBREDSTILSTAND=="Lider af kronisk bronkitis “rygerlunger”"~"Suffering from chronic bronchitis (“smokers lungs”)",
                                    HELBREDSTILSTAND=="Lider af slidgigt"~"Suffering from osteoarthritis",
                                    HELBREDSTILSTAND=="Rygproblemer"~"Back problems"))

#Professions 
choice_df<-choice_df%>%
  mutate(PROFESSION=case_when(PROFESSION=="Konsulent"~"Consultant",
                              PROFESSION=="Læge"~"Doctor",
                              PROFESSION=="Lagermedarbejder"~"Warehouse worker",
                              PROFESSION=="Pædagog"~"Nursery teacher",
                              PROFESSION=="Politimand"~"Police officer",
                              PROFESSION=="Rengøringsassistent"~"Cleaning assistant",
                              PROFESSION=="Slagteriarbejder"~"Slaughterhouse worker",
                              PROFESSION=="Socialrådgiver"~"Social worker",
                              PROFESSION=="SOSU-assistent"~"Social and health care assistant",
                              PROFESSION=="Sygeplejerske"~"Nurse",
                              PROFESSION=="Tømrer"~"Carpenter",
                              PROFESSION=="Kasseassistent"~"Cashier assistant",
                              PROFESSION=="Advokat"~"Lawyer",
                              PROFESSION=="Bogholder"~"Book keeper",
                              PROFESSION=="Embedsmand i staten"~"Government official",
                              PROFESSION=="Folkeskolelærer"~"Primary school teacher",
                              PROFESSION=="Jord- og betonarbejder"~"Soil -and concrete worker"))
#Translating gender
choice_df<-choice_df%>%
  mutate(K_N=case_when(K_N=="Mand"~"Man",
                       K_N=="Kvinde"~"Woman"))

#Translating attribute names and adding labels for initial conjoint 
attr(choice_df$ALDER, "label") <- "Age"
attr(choice_df$K_N, "label") <- "Gender"
attr(choice_df$HELBREDSTILSTAND, "label") <- "Health Status"
attr(choice_df$PROFESSION, "label") <- "Profession"
attr(choice_df$ANTAL_R_P_ARBEJDSMARKEDET, "label") <- "Years on labour market"

#Setting labels for features for for  initial conjoint 
feature_list<-list(ALDER = "Age",HELBREDSTILSTAND="Health Status",ANTAL_R_P_ARBEJDSMARKEDET="Years on labour market",PROFESSION="Profession",K_N="Gender",HEALTH_STATUS_X_PROFESSION="Health Status * Profession")

#Setting 0 to NA for alle features. (otherwise, 0 will be treated as an attribute) for second conjoint
choice_df_2[, 6:10][choice_df_2[, 6:10] == 0] <- NA

#Translating feature names for second conjoint 
choice_df_2<-choice_df_2%>%
  mutate(HELBREDSTILSTAND=case_when(HELBREDSTILSTAND=="Få eller ingen sygdomsmæssige komplikationer" ~"Few or none sickness-related complications",
                                    HELBREDSTILSTAND=="Funktionsnedsættelse som følge af svær overvægt"~"Disability caused by severe obesity",
                                    HELBREDSTILSTAND=="Lider af knogleskørhed"~"Suffering from osteoporosis",
                                    HELBREDSTILSTAND=="Lider af kronisk bronkitis “rygerlunger”"~"Suffering from chronic bronchitis (“smokers lungs”)",
                                    HELBREDSTILSTAND=="Lider af slidgigt"~"Suffering from osteoarthritis",
                                    HELBREDSTILSTAND=="Rygproblemer"~"Back problems"))

choice_df_2<-choice_df_2%>%
  mutate(PROFESSION=case_when(PROFESSION=="Konsulent"~"Consultant",
                              PROFESSION=="Læge"~"Doctor",
                              PROFESSION=="Lagermedarbejder"~"Warehouse worker",
                              PROFESSION=="Pædagog"~"Nursery teacher",
                              PROFESSION=="Politimand"~"Police officer",
                              PROFESSION=="Rengøringsassistent"~"Cleaning assistant",
                              PROFESSION=="Slagteriarbejder"~"Slaughterhouse worker",
                              PROFESSION=="Socialrådgiver"~"Social worker",
                              PROFESSION=="SOSU-assistent"~"Social and health care assistant",
                              PROFESSION=="Sygeplejerske"~"Nurse",
                              PROFESSION=="Tømrer"~"Carpenter",
                              PROFESSION=="Kasseassistent"~"Cashier assistant",
                              PROFESSION=="Advokat"~"Lawyer",
                              PROFESSION=="Bogholder"~"Book keeper",
                              PROFESSION=="Embedsmand i staten"~"Government official",
                              PROFESSION=="Folkeskolelærer"~"Primary school teacher",
                              PROFESSION=="Jord- og betonarbejder"~"Soil -and concrete worker"))


choice_df_2<-choice_df_2%>%
  mutate(K_N=case_when(K_N=="Mand"~"Man",
                       K_N=="Kvinde"~"Woman"))

#Translating attribute names and adding labels for second conjoint 
attr(choice_df_2$ALDER, "label") <- "Age"
attr(choice_df_2$K_N, "label") <- "Gender"
attr(choice_df_2$HELBREDSTILSTAND, "label") <- "Health Status"
attr(choice_df_2$PROFESSION, "label") <- "Profession"
attr(choice_df_2$ANTAL_R_P_ARBEJDSMARKEDET, "label") <- "Years on labour market"

#setting labels for features for conjoinnt 
feature_list<-list(ALDER = "Age",HELBREDSTILSTAND="Health Status",ANTAL_R_P_ARBEJDSMARKEDET="Years on labor market",PROFESSION="Profession",K_N="Gender",HEALTH_STATUS_X_PROFESSION="Health Status * Profession")

#Reordering factors 
choice_df$HELBREDSTILSTAND <- factor(choice_df$HELBREDSTILSTAND, levels = c("Few or none sickness-related complications", 
                                                                            "Disability caused by severe obesity",
                                                                            "Suffering from chronic bronchitis (“smokers lungs”)",
                                                                            "Back problems", 
                                                                            "Suffering from osteoarthritis", 
                                                                            "Suffering from osteoporosis"))

choice_df_2$HELBREDSTILSTAND <- factor(choice_df_2$HELBREDSTILSTAND, levels = c("Few or none sickness-related complications", 
                                                                                "Disability caused by severe obesity",
                                                                                "Suffering from chronic bronchitis (“smokers lungs”)",
                                                                                "Back problems", 
                                                                                "Suffering from osteoarthritis", 
                                                                                "Suffering from osteoporosis"))


#Makingn variable for response time for both conjoint sets 
choice_df$response_time_1<-as.numeric(choice_df$RESPONDENT_LENGTH_OF_INTERVIEW_SECONDS)

choice_df$response_time_1<-as.numeric(choice_df$RESPONDENT_LENGTH_OF_INTERVIEW_SECONDS)
choice_df_2$response_time_2<-as.numeric(choice_df_2$RESPONDENT_LENGTH_OF_INTERVIEW_SECONDS)

response_time_df_1<-choice_df%>%
  select(RESPONDENT_ID,response_time_1)%>%
  mutate(RESPONDENT_ID=as.integer(RESPONDENT_ID))%>%
  distinct()

response_time_df_2<-choice_df_2%>%
  select(RESPONDENT_ID,response_time_2)%>%
  distinct()

response_time_total_df<-left_join(response_time_df_2,response_time_df_1)

response_time_total_df<-response_time_total_df%>%
  mutate(response_time_total=response_time_1+response_time_1)

summary(response_time_total_df$response_time_total)

#Calculating total time spent 
#Binning these for subgroup analysis 
choice_df<-choice_df%>%
  mutate(response_time_cut=cut_number(response_time_1,n=2))

choice_df<-choice_df%>%
  mutate(response_time_cut=as.factor(response_time_cut))

#Creating factors 
choice_df<-choice_df%>%
  mutate(response_time_cut_labeled=case_when(response_time_cut=="[86,365]"~"Short response time",
                                             response_time_cut=="(365,1.52e+04]"~"Long response time"))

choice_df$response_time_cut_labeled<-as.factor(choice_df$response_time_cut_labeled)


#1.3 Factor analysis and index score creation 
#############################################

#Reversing score for egaliatarianism item, which is reversely wordeed 
choice_df<-choice_df%>%
  mutate(Q14_EGA3_rev=case_when(Q14_EGA3==1~5,
                               Q14_EGA3==2~4,
                               Q14_EGA3==3~3,
                               Q14_EGA3==4~2,
                               Q14_EGA3==5~1))

#Making a dataframe with unique respondent scores to be used for factor loadings
choice_factor<-choice_df%>%
  dplyr::select(RESPONDENT_ID
                ,Q12_EGA1
                ,Q13_EGA2
                ,Q14_EGA3_rev
                ,Q16_HUM1
                ,Q17_HUM2
                ,Q18_HUM3)%>%
  mutate(RESPONDENT_ID
         ,Q12_EGA1
         ,Q13_EGA2
         ,Q14_EGA3_rev
         ,Q16_HUM1
         ,Q17_HUM2
         ,Q18_HUM3)%>%
          distinct()%>%
          select(-RESPONDENT_ID)

#Making a scree plot for parallel analysis 
scree_plot<-psych::fa.parallel(choice_factor,fm = "minres", fa = "fa")

#Making a dataframe holding our factor analysis 
fit <- fa(choice_factor,nfactors = 2,rotate = "oblimin",fm="minres")

#plotting the loadings  
fa_diagram<-fa.diagram(fit)

#expoert to latex code 
xtable(unclass(fit$loadings))
xtable(unclass(fit$Vaccounted))

stargazer(fit$loadings,summary=FALSE)


#Choosing variables for index construction 
df_1_m<-choice_df%>%
  dplyr::select(Q12_EGA1
                ,Q13_EGA2
                ,Q14_EGA3
                ,Q16_HUM1
                ,Q17_HUM2
                ,Q18_HUM3
                ,RESPONDENT_ID)%>%
  mutate(Q12_EGA1
         ,Q13_EGA2
         ,Q14_EGA3
         ,Q16_HUM1
         ,Q17_HUM2
         ,Q18_HUM3
         ,RESPONDENT_ID)

#Creating empty data frame with respondent ID's 
ref_1<-choice_df%>%
  select(RESPONDENT_ID)%>%
  mutate(RESPONDENT_ID)

#Mkaing an empty list, and denoting the placement of items for each index. 
m.keys.list <- list(egal=c(1,2,3),
                    hum=c(4,5,6))

m.keys <- make.keys(df_1_m,m.keys.list,item.labels=colnames(df_1_m))

#Making indexscores 
m.scored <-scoreItems(m.keys, df_1_m,impute="none",
                      min=1, max=5, digits=2)

#Pulling indexscores from choice_dfframe 
m_scores<-data.frame(m.scored$scores)

#Binding indexscores to unique respondent ID 
choice_df<-cbind(choice_df,m_scores)

#Finding cronbachs alpga for all indexes 
output.alpha.egal<- alpha(df_1_m[,abs(m.keys.list$egal)],
                              check.keys=TRUE)

output.alpha.hum <- alpha(df_1_m[,abs(m.keys.list$hum)],
                              check.keys=TRUE)

#Making labels for cronbachs alpha 
scale.names <- c("Egalitarianism", "Humanitarianism")

#Cronbachs alpha in one table 
m.alphas <- as.numeric(c(output.alpha.egal$total[2],
                         output.alpha.hum$total[2]))
l 
m.alpha.table <- data.frame(Scale = scale.names, Std.Alpha = m.alphas)

#Creating latex outputs
xtable(as.data.frame(output.alpha.egal$alpha.drop))
xtable(as.data.frame(output.alpha.hum$alpha.drop))
print(xtable(m.alpha.table,include.rownames=FALSE))

#Creating grouped variable for sophisticaiton, simply consisting of cumulated correct answers 
choice_df<-choice_df%>%
  mutate(soph_simple=soph_1+soph_2+soph_3)

#Creating grouped variable for egalitarianism relying on median 
choice_df<-choice_df%>%
  mutate(egal_grouped=cut_number(egal,n=2))

#Creating grouped variable for humanitarianism relying on median 
choice_df<-choice_df%>%
  mutate(hum_grouped=cut_number(hum,n=2))

#Labelling sophistictaion 
choice_df<-choice_df%>%
  mutate(soph_simple_grouped_labelled=case_when(soph_simple<=2~"Low sophistication",
                                                soph_simple==3~"High sophistication"))
#Labelling egalitarianism 
choice_df<-choice_df%>%
  mutate(egal_grouped_labelled=case_when(egal_grouped=="[1,3.33]"~"Low egalitarianism",
                                        egal_grouped=="(3.33,5]"~"High egalitarianism"))

#Labelling humanitarianism  
choice_df<-choice_df%>%
  mutate(hum_grouped_labelled=case_when(hum_grouped=="[1,4]"~"Low humanitarianism",
                                         hum_grouped=="(4,5]"~"High humanitarianism"))




attr(choice_df_2$cue_given, "label") <- "Experimental situation"

#1.4 Merging initial conjoint with second conjoint frame
###########################
#Marking each conjoint set with an indicator 
choice_df<-choice_df%>%
  mutate(CONJOINT_SET=1)

choice_df_2<-choice_df_2%>%
  mutate(CONJOINT_SET=2)

#Making categorical variables for cue given and setting to factor 
choice_df_2<-choice_df_2%>%
  mutate(cue_given=case_when(Q3_NEED=="1"|Q3_NEED=="2"|Q3_NEED=="3"|Q3_NEED=="4"|Q3_NEED=="5" ~"Need",
                             Q2_RECIPROCITY=="1"|Q2_RECIPROCITY=="2"|Q2_RECIPROCITY=="3"|Q2_RECIPROCITY=="4"|Q2_RECIPROCITY=="5"~"Reciprocity"))
choice_df_2$cue_given<-factor(choice_df_2$cue_given)

choice_df_2<-choice_df_2%>%
  mutate(RESPONDENT_ID=VARIABLE_ID)

#Creating numeric variable for cue given 
choice_df_2<-choice_df_2%>%
  mutate(cue_given_num=case_when(cue_given=="Need"~0,
                                 cue_given=="Reciprocity"~1))

#Putting in empty variables for second conjoint (since this dataset does not contain background variables. These will
#later be filled, grouped by respondent ID)
choice_df_2<-choice_df_2%>%
  mutate(Kon_dich=NA,Edu_cat_num=NA,Kon_cat=NA,party_cat=NA,demo_cat=NA,Q4_ALDER=NA,Age_binned=NA,Edu_cat=NA)

#Mutating cue variable for first conjoint 
choice_df<-choice_df%>%
  mutate(cue_given=NA)

#Making new dataframes for merging 
choice_df_cue_1<-choice_df%>%
  select(ALDER,K_N,ANTAL_R_P_ARBEJDSMARKEDET,HELBREDSTILSTAND,PROFESSION,CHOICE_INDICATOR,CONJOINT_SET,RESPONDENT_ID,Q4_ALDER,Kon_dich,Edu_cat_num,Kon_cat,party_cat,demo_cat,Age_binned,Edu_cat,cue_given)

choice_df_cue_2<-choice_df_2%>%
  select(ALDER,K_N,ANTAL_R_P_ARBEJDSMARKEDET,HELBREDSTILSTAND,PROFESSION,CHOICE_INDICATOR,CONJOINT_SET,RESPONDENT_ID,Q4_ALDER,Kon_dich,Edu_cat_num,Kon_cat,party_cat,demo_cat,Age_binned,Edu_cat,cue_given) 

#Rbinding the two dataframes 
choice_df_cue<-rbind(choice_df_cue_1,choice_df_cue_2)

#Assigning first conjoint as control group, second as treatment group 
choice_df_cue<-choice_df_cue%>%
  mutate(CONJOINT_SET=case_when(CONJOINT_SET==1~"Before priming",
                   CONJOINT_SET==2~"After priming"))

choice_df_cue$CONJOINT_SET<-as.factor(choice_df_cue$CONJOINT_SET)

#Setting variable to factor 
choice_df$CHOICE_SET<-as.factor(choice_df$CHOICE_SET)
choice_df$LABEL<-as.factor(choice_df$LABEL) 

choice_df_2$CHOICE_SET<-as.factor(choice_df_2$CHOICE_SET)
choice_df_2$LABEL<-as.factor(choice_df_2$LABEL) 

#assigning cue to initial conjoint  
choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(cue_given, .direction = "up")

#Assigning background variables to second conjoint set 
choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(Q4_ALDER, .direction = "down")

choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(Edu_cat_num, .direction = "down")

choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(Edu_cat, .direction = "down")

choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(party_cat, .direction = "down")

choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(Kon_dich, .direction = "down")

choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(Kon_cat, .direction = "down")

choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(demo_cat, .direction = "down")

choice_df_cue<-choice_df_cue%>% 
  group_by(RESPONDENT_ID)%>% 
  fill(Age_binned, .direction = "down")

#Setting numeric 
choice_df_cue$Edu_cat_num<-as.numeric(choice_df_cue$Edu_cat_num)
choice_df_cue$CHOICE_INDICATOR<-as.numeric(choice_df_cue$CHOICE_INDICATOR)
choice_df$CHOICE_INDICATOR<-as.numeric(choice_df$CHOICE_INDICATOR)

#Setting factors 
choice_df$Age_binned<-as.factor(choice_df$Age_binned)
choice_df$demo_cat<-as.factor(choice_df$demo_cat)
choice_df$party_cat<-as.factor(choice_df$party_cat)
choice_df$Edu_cat<-as.factor(choice_df$Edu_cat)

choice_df_cue$Age_binned<-as.factor(choice_df_cue$Age_binned)
choice_df_cue$demo_cat<-as.factor(choice_df_cue$demo_cat)
choice_df_cue$party_cat<-as.factor(choice_df_cue$party_cat)
choice_df_cue$Edu_cat<-as.factor(choice_df_cue$Edu_cat)

  
#Counting the distribution of the two cues 
choice_df_2%>%
  select(RESPONDENT_ID,cue_given)%>%
  filter(cue_given=="Need")%>%
  distinct()%>%
  nrow()
  
choice_df_2%>%
  select(RESPONDENT_ID,cue_given)%>%
  filter(cue_given=="Reciprocity")%>%
  distinct()%>%
  nrow()

#Putting together variable policy approval 
choice_df_2[choice_df_2=="NULL"] <- NA
choice_df_2<-choice_df_2%>%
  mutate(POL_approval=coalesce(Q2_RECIPROCITY,Q3_NEED))
choice_df_2$POL_approval<-as.numeric(choice_df_2$POL_approval)

#Setting variables to factors
choice_df<-choice_df%>%
  mutate(egal_grouped_labelled=factor(egal_grouped_labelled))%>%
  mutate(hum_grouped_labelled=factor(hum_grouped_labelled))%>%
  mutate(soph_simple_grouped_labelled=factor(soph_simple_grouped_labelled))

#Setting features to factors for initial conjoint sets 
choice_df<-choice_df%>%
  mutate(ALDER=factor(ALDER))%>%
  mutate(ANTAL_R_P_ARBEJDSMARKEDET=factor(ANTAL_R_P_ARBEJDSMARKEDET))%>%
  mutate(HELBREDSTILSTAND=factor(HELBREDSTILSTAND))%>%
  mutate(PROFESSION=factor(PROFESSION))%>%
  mutate(K_N=factor(K_N))

#Setting features to factors for second conjoint sets 
choice_df_2<-choice_df_2%>%
  mutate(ALDER=factor(ALDER))%>%
  mutate(ANTAL_R_P_ARBEJDSMARKEDET=factor(ANTAL_R_P_ARBEJDSMARKEDET))%>%
  mutate(HELBREDSTILSTAND=factor(HELBREDSTILSTAND))%>%
  mutate(PROFESSION=factor(PROFESSION))%>%
  mutate(K_N=factor(K_N))

#etting features to factors for combined data 
choice_df_cue<-choice_df_cue%>%
  mutate(ALDER=factor(ALDER))%>%
  mutate(ANTAL_R_P_ARBEJDSMARKEDET=factor(ANTAL_R_P_ARBEJDSMARKEDET))%>%
  mutate(HELBREDSTILSTAND=factor(HELBREDSTILSTAND))%>%
  mutate(PROFESSION=factor(PROFESSION))%>%
  mutate(K_N=factor(K_N))


