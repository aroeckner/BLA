#Roeckner et al., When plasticity becomes pathological: Sequential decreases in basolateral amygdala response to threat cues predict failure to recover from PTSD

library(readxl)
AURORA_BLA <- read_excel("AURORA_BLA.xlsx")
Dataset2 <- read_excel("Dataset2.xlsx")

#Discovery cohort
#Create data frame for only n=99 participants who have MRI data at 6 months post trauma
AURORA_BLA_6mOnly <- AURORA_BLA[!is.na(AURORA_BLA$M6_LBLA), ]

#PCL-5 Paired t-tests
t.test(AURORA_BLA_6mOnly$M6_PCL5_RS, AURORA_BLA_6mOnly$WK2_PCL5_RS,  paired=TRUE)

#Left and right BLA paired t-test
t.test(AURORA_BLA$L_BLA, AURORA_BLA$M6_LBLA, paired=TRUE)
t.test(AURORA_BLA$R_BLA, AURORA_BLA$M6_RBLA, paired=TRUE)

#Standard deviation and mean
sd(AURORA_BLA$WK2_PCL5_RS)
mean(AURORA_BLA$WK2_PCL5_RS)

sd(AURORA_BLA$M6_PCL5_RS)
mean(AURORA_BLA$M6_PCL5_RS)

sd(AURORA_BLA$L_BLA)
mean(AURORA_BLA$L_BLA)

sd(AURORA_BLA$M6_LBLA)
mean(AURORA_BLA$M6_LBLA)

sd(AURORA_BLA$R_BLA)
mean(AURORA_BLA$R_BLA)

sd(AURORA_BLA$M6_RBLA)
mean(AURORA_BLA$M6_RBLA)

#Jarque-Bera test 
#skewness, kurtosis
library(moments)
jarque.test(AURORA_BLA$WK2_PCL5_RS)
jarque.test(AURORA_BLA$M6_PCL5_RS)
jarque.test(AURORA_BLA$M6_Wk2)

library(e1071)
skewness(AURORA_BLA$WK2_PCL5_RS)
skewness(AURORA_BLA$M6_PCL5_RS) 
skewness(AURORA_BLA$M6_Wk2) 

#Test for co-linearity: chance of dying with trauma event
modelwk2 <- lm(ED_ChanceofDying ~ ED_Event_BroadClass, data = AURORA_BLA)
summary(modelwk2)
modelm6 <- lm(ED_ChanceofDying ~ ED_Event_BroadClass, data = AURORA_BLA_6mOnly)
summary(modelm6)

#Poisson regression models
#Week2 pcl, week2 BLA, Table S3
#Left
Wk2pcl_Wk2LBLA <- glm(WK2_PCL5_RS ~ Mclean + Temple + WSU + Emory + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + L_BLA, family="poisson", data=AURORA_BLA)
summary(Wk2pcl_Wk2LBLA) 
#IRR 95% CI
exp(confint(Wk2pcl_Wk2LBLA))
# Extract the coefficient estimates
coefficients <- coef(Wk2pcl_Wk2LBLA)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Right
Wk2pcl_Wk2RBLA <- glm(WK2_PCL5_RS ~ Mclean + Temple + WSU + Emory + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + R_BLA, family="poisson", data=AURORA_BLA)
summary(Wk2pcl_Wk2RBLA) 
#IRR 95% CI
exp(confint(Wk2pcl_Wk2RBLA))
# Extract the coefficient estimates
coefficients <- coef(Wk2pcl_Wk2RBLA)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Month6 pcl, week2 BLA, Table S4
#Left
M6pcl_Wk2LBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + Emory + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + L_BLA, family="poisson", data=AURORA_BLA)
summary(M6pcl_Wk2LBLA) 
#IRR 95% CI
exp(confint(M6pcl_Wk2LBLA))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_Wk2LBLA)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Right
M6pcl_Wk2RBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + Emory + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + R_BLA, family="poisson", data=AURORA_BLA)
summary(M6pcl_Wk2RBLA) 
#IRR 95% CI
exp(confint(M6pcl_Wk2RBLA))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_Wk2RBLA)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Sensitivity analyses, CTQ and Pre-PCL, Table S5
#Pre-pcl, 6monthpcl and 2wk RBLA
M6pcl_Wk2RBLA_prepcl <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + Emory + PRE_PCL5_RS + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + R_BLA, family="poisson", data=AURORA_BLA)
summary(M6pcl_Wk2RBLA_prepcl) 
#IRR 95% CI
exp(confint(M6pcl_Wk2RBLA_prepcl))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_Wk2RBLA_prepcl)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#CTQ, 6monthpcl and R 2wk BLA
M6pcl_Wk2RBLA_CTQ <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + Emory + WK2_CTQSF_Total_RS + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + R_BLA, family="poisson", data=AURORA_BLA)
summary(M6pcl_Wk2RBLA_CTQ) 
#IRR 95% CI
exp(confint(M6pcl_Wk2RBLA_CTQ))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_Wk2RBLA_CTQ)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)


#Fearful and neutral Poisson regression models, Table S6
#fear
M6pcl_Wk2RBLAfear <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + Emory +  ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + RBLA_wk2_fear, family="poisson", data=AURORA_BLA)
summary(M6pcl_Wk2RBLAfear) 
#IRR 95% CI
exp(confint(M6pcl_Wk2RBLAfear))
# Extract the coefficient estimates
coefficientsM6pcl_Wk2RBLAfear <- coef(M6pcl_Wk2RBLAfear)
# Calculate the incidence rate ratios
IRRsM6pcl_Wk2RBLAfear <- exp(coefficientsM6pcl_Wk2RBLAfear)
# Print the IRRs
print(IRRsM6pcl_Wk2RBLAfear)

#neutral
M6pcl_Wk2RBLAneutral <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + Emory + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + RBLA_wk2_neutral, family="poisson", data=AURORA_BLA)
summary(M6pcl_Wk2RBLAneutral) 
#IRR 95% CI
exp(confint(M6pcl_Wk2RBLAneutral))
# Extract the coefficient estimates
coefficientsM6pcl_Wk2RBLAneutral <- coef(M6pcl_Wk2RBLAneutral)
# Calculate the incidence rate ratios
IRRsM6pcl_Wk2RBLAneutral <- exp(coefficientsM6pcl_Wk2RBLAneutral)
# Print the IRRs
print(IRRsM6pcl_Wk2RBLAneutral)


#Month6 pcl, month6 BLA, Table S7
#Left
M6pcl_M6LBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + M6_LBLA, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6LBLA) 
#IRR 95% CI
exp(confint(M6pcl_M6LBLA))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_M6LBLA)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Right
M6pcl_M6RBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + M6_RBLA, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6RBLA) 
#IRR 95% CI
exp(confint(M6pcl_M6RBLA))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_M6RBLA)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Sensitivity analyses, CTQ and Pre-PCL5, Table S8
#Pre-pcl, 6monthpcl and 6m LBLA
M6pcl_M6LBLA_prepcl <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + PRE_PCL5_RS + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + M6_LBLA, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6LBLA_prepcl) 
#IRR 95% CI
exp(confint(M6pcl_M6LBLA_prepcl))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_M6LBLA_prepcl)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#CTQ, 6monthpcl and 6m LBLA
M6pcl_M6LBLA_CTQ <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + WK2_CTQSF_Total_RS + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + M6_LBLA, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6LBLA_CTQ) 
#IRR 95% CI
exp(confint(M6pcl_M6LBLA_CTQ))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_M6LBLA_CTQ)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Fearful and neutral Poisson regression models, Table S9
#fear
M6pcl_M6LBLAfear <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU +  ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + LBLA_M6_fear, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6LBLAfear) 
#IRR 95% CI
exp(confint(M6pcl_M6LBLAfear))
# Extract the coefficient estimates
coefficientsM6pcl_M6LBLAfear <- coef(M6pcl_M6LBLAfear)
# Calculate the incidence rate ratios
IRRsM6pcl_M6LBLAfear <- exp(coefficientsM6pcl_M6LBLAfear)
# Print the IRRs
print(IRRsM6pcl_M6LBLAfear)

#neutral
M6pcl_M6LBLAneutral <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + LBLA_M6_neutral, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6LBLAneutral) 
#IRR 95% CI
exp(confint(M6pcl_M6LBLAneutral))
# Extract the coefficient estimates
coefficientsM6pcl_M6LBLAneutral <- coef(M6pcl_M6LBLAneutral)
# Calculate the incidence rate ratios
IRRsM6pcl_M6LBLAneutral <- exp(coefficientsM6pcl_M6LBLAneutral)
# Print the IRRs
print(IRRsM6pcl_M6LBLAneutral)

#McFadden Pseudo_R2 scores
install.packages('pscl')
library(pscl)
#calculate McFadden's R-squared for model
pR2(Wk2pcl_Wk2LBLA)['McFadden']
pR2(Wk2pcl_Wk2RBLA)['McFadden']
pR2(M6pcl_Wk2LBLA)['McFadden']
pR2(M6pcl_Wk2RBLA)['McFadden']
pR2(M6pcl_M6LBLA)['McFadden']
pR2(M6pcl_M6RBLA)['McFadden']

#M6pcl_Wk2RBLA without Wk2RBLA
M6pcl1 <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + Emory + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert, family="poisson", data=AURORA_BLA)
summary(M6pcl1)
#McFadden R squared change
pseudoR2_m6pclWk2RBLA <- (pR2(M6pcl_Wk2RBLA)['McFadden']) - (pR2(M6pcl1)['McFadden'])
print(pseudoR2_m6pclWk2RBLA)
#M6pcl_M6LBLA without m6LBLA
M6pcl2 <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert, family="poisson", data=AURORA_BLA_6mOnly)
summary(M6pcl2)
#Mcfadden R squared change
pseudoR2_m6pclM6LBLA <- (pR2(M6pcl_M6LBLA)['McFadden']) - (pR2(M6pcl2)['McFadden'])
print(pseudoR2_m6pclM6LBLA)


#Change in BLA reactivity and change in PTSD symptoms, Table S10
#Left
LBLA_PCL_Change <- lm(M6_Wk2  ~ Mclean + Temple + WSU +ED_Age + ED_RaceEthCode + ED_Event_BroadClass +  ED_GenderBirthCert +  LBLA_dif, data=AURORA_BLA_6mOnly)
summary(LBLA_PCL_Change)

#Right
RBLA_PCL_Change <- lm(M6_Wk2  ~ Mclean + Temple + WSU +ED_Age + ED_RaceEthCode + ED_Event_BroadClass +  ED_GenderBirthCert +  RBLA_dif, data=AURORA_BLA_6mOnly)
summary(RBLA_PCL_Change)

#Change in BLA reactivity between timepoints and PTSD symptoms
#left BLA change and M6PCL
M6pcl_LBLAdif <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + LBLA_dif, family="poisson", data=AURORA_BLA_6mOnly)
summary(M6pcl_LBLAdif)
#IRR 95% CI
exp(confint(M6pcl_LBLAdif))
# Extract the coefficient estimates
coefficientsLBLAdif <- coef(M6pcl_LBLAdif)
# Calculate the incidence rate ratios
IRRsLBLAdif <- exp(coefficientsLBLAdif)
# Print the IRRs
print(IRRsLBLAdif)

#Right BLA change and M6PCL
M6pcl_RBLAdif <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + RBLA_dif, family="poisson", data=AURORA_BLA_6mOnly)
summary(M6pcl_RBLAdif)
#IRR 95% CI
exp(confint(M6pcl_RBLAdif))
# Extract the coefficient estimates
coefficientsRBLAdif <- coef(M6pcl_RBLAdif)
# Calculate the incidence rate ratios
IRRsRBLAdif <- exp(coefficientsRBLAdif)
# Print the IRRs
print(IRRsRBLAdif)

#McFadden Pseudo_R2 scores
install.packages('pscl')
library(pscl)
#calculate McFadden's R-squared for model
pR2(M6pcl_RBLAdif)['McFadden']
pR2(M6pcl_LBLAdif)['McFadden']
#M6pcl_M6LBLA without m6LBLA
M6pcl2 <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert, family="poisson", data=AURORA_BLA_6mOnly)
summary(M6pcl2)
#Mcfadden R squared change M6pcl_RBLAdif
pseudoR2_m6pclRBLAdif <- (pR2(M6pcl_RBLAdif)['McFadden']) - (pR2(M6pcl2)['McFadden'])
print(pseudoR2_m6pclRBLAdif)
#Mcfadden R squared change M6pcl_LBLAdif
pseudoR2_m6pclLBLAdif <- (pR2(M6pcl_LBLAdif)['McFadden']) - (pR2(M6pcl2)['McFadden'])
print(pseudoR2_m6pclLBLAdif)


#Linear and non-linear effects of baseline left BLA reactivity, Sable S11
# Create the z-scores variable for left BLA change score
mean_LBLAdif <- mean(AURORA_BLA_6mOnly$LBLA_dif)
sd_LBLAdif <- sd(AURORA_BLA_6mOnly$LBLA_dif)
print(sd_LBLAdif)
# Create the z-scores variable
AURORA_BLA_6mOnly$z_score_LBLAdif <- (AURORA_BLA_6mOnly$LBLA_dif - mean_LBLAdif) / sd_LBLAdif
print(AURORA_BLA_6mOnly$z_score_LBLAdif)

#z-score baseline LBLA
mean_LBLA2 <- mean(AURORA_BLA_6mOnly$L_BLA)
sd_LBLA2 <- sd(AURORA_BLA_6mOnly$L_BLA)
print(sd_LBLA2)
# Create the z-scores variable
AURORA_BLA_6mOnly$z_score_LBLA2 <- (AURORA_BLA_6mOnly$L_BLA - mean_LBLA2) / sd_LBLA2
print(AURORA_BLA_6mOnly$z_score_LBLA2)

###Square the  variables
AURORA_BLA_6mOnly$z_LBLAdif_sq <- AURORA_BLA_6mOnly$z_score_LBLAdif^2
print(AURORA_BLA_6mOnly$z_LBLAdif_sq)
AURORA_BLA_6mOnly$z_LBLA2_sq <- AURORA_BLA_6mOnly$z_score_LBLA2^2
print(AURORA_BLA_6mOnly$z_LBLA2_sq)

##Variables
#z_score_LBLA2, z_score_LBLAdif
#z_LBLA2_sq, z_LBLAdif_sq 

#Z-score change Left BLA
Zmodel1LBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + z_score_LBLAdif, family="poisson", data=AURORA_BLA_6mOnly)
summary(Zmodel1LBLA)
#IRR 95% CI
exp(confint(Zmodel1LBLA))
# Extract the coefficient estimates
coefficientsLBLAdifz <- coef(Zmodel1LBLA)
# Calculate the incidence rate ratios
IRRsLBLAdifz <- exp(coefficientsLBLAdifz)
# Print the IRRs
print(IRRsLBLAdifz)

#Z-score change Left BLA and z-score 2-week left BLA
Zmodel2LBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + z_score_LBLA2 +ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + z_score_LBLAdif, family="poisson", data=AURORA_BLA_6mOnly)
summary(Zmodel2LBLA)
#IRR 95% CI
exp(confint(Zmodel2LBLA))
# Extract the coefficient estimates
coefficientsLBLAdifz2 <- coef(Zmodel2LBLA)
# Calculate the incidence rate ratios
IRRsLBLAdifz2 <- exp(coefficientsLBLAdifz2)
# Print the IRRs
print(IRRsLBLAdifz2)

##Z-score change Left BLA, z-score 2-week left BLA, z-score 2-week left BLA squared
Zmodel3LBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + z_score_LBLA2 + z_LBLA2_sq+ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + z_score_LBLAdif, family="poisson", data=AURORA_BLA_6mOnly)
summary(Zmodel3LBLA)
#IRR 95% CI
exp(confint(Zmodel3LBLA))
# Extract the coefficient estimates
coefficientsLBLAdifz3 <- coef(Zmodel3LBLA)
# Calculate the incidence rate ratios
IRRsLBLAdifz3 <- exp(coefficientsLBLAdifz3)
# Print the IRRs
print(IRRsLBLAdifz3)

#Modelfit
library(flexmix)
BIC(M6pcl_LBLAdif)
BIC(Zmodel1LBLA)
BIC(Zmodel2LBLA)
BIC(Zmodel3LBLA)
AIC(M6pcl_LBLAdif)
AIC(Zmodel1LBLA)
AIC(Zmodel2LBLA)
AIC(Zmodel3LBLA)


#Linear and non-linear effects of baseline right BLA reactivity, Table S12
# Create the z-scores variable for right BLA change score
mean_RBLAdif <- mean(AURORA_BLA_6mOnly$RBLA_dif)
sd_RBLAdif <- sd(AURORA_BLA_6mOnly$RBLA_dif)
print(sd_RBLAdif)
# Create the z-scores variable
AURORA_BLA_6mOnly$z_score_RBLAdif <- (AURORA_BLA_6mOnly$RBLA_dif - mean_RBLAdif) / sd_RBLAdif
print(AURORA_BLA_6mOnly$z_score_RBLAdif)
#z-score baseline RBLA
mean_RBLA2 <- mean(AURORA_BLA_6mOnly$R_BLA)
sd_RBLA2 <- sd(AURORA_BLA_6mOnly$R_BLA)
print(sd_RBLA2)
# Create the z-scores variable
AURORA_BLA_6mOnly$z_score_RBLA2 <- (AURORA_BLA_6mOnly$R_BLA - mean_RBLA2) / sd_RBLA2
print(AURORA_BLA_6mOnly$z_score_RBLA2)

###Square the  variables
AURORA_BLA_6mOnly$z_RBLAdif_sq <- AURORA_BLA_6mOnly$z_score_RBLAdif^2
print(AURORA_BLA_6mOnly$z_RBLAdif_sq)
AURORA_BLA_6mOnly$z_RBLA2_sq <- AURORA_BLA_6mOnly$z_score_RBLA2^2
print(AURORA_BLA_6mOnly$z_RBLA2_sq)

##Variables
#z_score_RBLA2, z_score_RBLAdif
#z_RBLA2_sq, z_RBLAdif_sq 

#Z-score change Right BLA
Zmodel1RBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + z_score_RBLAdif, family="poisson", data=AURORA_BLA_6mOnly)
summary(Zmodel1RBLA)
#IRR 95% CI
exp(confint(Zmodel1RBLA))
# Extract the coefficient estimates
coefficientsRBLAdifz1 <- coef(Zmodel1RBLA)
# Calculate the incidence rate ratios
IRRsRBLAdifz1 <- exp(coefficientsRBLAdifz1)
# Print the IRRs
print(IRRsRBLAdifz1)

#Z-score change Right BLA and z-score 2-week left BLA
Zmodel2RBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + z_score_RBLA2+ ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + z_score_RBLAdif, family="poisson", data=AURORA_BLA_6mOnly)
summary(Zmodel2RBLA)
#IRR 95% CI
exp(confint(Zmodel2RBLA))
# Extract the coefficient estimates
coefficientsRBLAdifz2 <- coef(Zmodel2RBLA)
# Calculate the incidence rate ratios
IRRsRBLAdifz2 <- exp(coefficientsRBLAdifz2)
# Print the IRRs
print(IRRsRBLAdifz2)

##Z-score change Right BLA, z-score 2-week left BLA, z-score 2-week left BLA squared
Zmodel3RBLA <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + z_score_RBLA2+z_RBLA2_sq + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + z_score_RBLAdif, family="poisson", data=AURORA_BLA_6mOnly)
summary(Zmodel3RBLA)
#IRR 95% CI
exp(confint(Zmodel3RBLA))
# Extract the coefficient estimates
coefficientsRBLAdifz3 <- coef(Zmodel3RBLA)
# Calculate the incidence rate ratios
IRRsRBLAdifz3 <- exp(coefficientsRBLAdifz3)
# Print the IRRs
print(IRRsRBLAdifz3)

#Modelfit
library(flexmix)

BIC(M6pcl_RBLAdif)
BIC(Zmodel1RBLA)
BIC(Zmodel2RBLA)
BIC(Zmodel3RBLA)
AIC(M6pcl_RBLAdif)
AIC(Zmodel1RBLA)
AIC(Zmodel2RBLA)
AIC(Zmodel3RBLA)


#Sensitivity analyses, CTQ and Pre-PCL5, Table S13
#Pre-pcl5
#Left change BLA
M6pcl_ChangeLBLA_prepcl <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + PRE_PCL5_RS + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + LBLA_dif, family="poisson", data=AURORA_BLA_6mOnly)
summary(M6pcl_ChangeLBLA_prepcl) 
#IRR 95% CI
exp(confint(M6pcl_ChangeLBLA_prepcl))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_ChangeLBLA_prepcl)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Right change BLA
M6pcl_changeRBLA_prepcl <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + PRE_PCL5_RS + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + RBLA_dif, family="poisson", data=AURORA_BLA)
summary(M6pcl_changeRBLA_prepcl) 
#IRR 95% CI
exp(confint(M6pcl_changeRBLA_prepcl))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_changeRBLA_prepcl)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#CTQ
#Left Change BLA
M6pcl_changeLBLA_CTQ <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + WK2_CTQSF_Total_RS + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + LBLA_dif, family="poisson", data=AURORA_BLA)
summary(M6pcl_changeLBLA_CTQ) 
#IRR 95% CI
exp(confint(M6pcl_changeLBLA_CTQ))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_changeLBLA_CTQ)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)

#Right Change BLA
M6pcl_changeRBLA_CTQ <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + WK2_CTQSF_Total_RS + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + RBLA_dif, family="poisson", data=AURORA_BLA)
summary(M6pcl_changeRBLA_CTQ) 
#IRR 95% CI
exp(confint(M6pcl_changeRBLA_CTQ))
# Extract the coefficient estimates
coefficients <- coef(M6pcl_changeRBLA_CTQ)
# Calculate the incidence rate ratios
IRRs <- exp(coefficients)
# Print the IRRs
print(IRRs)


#Fearful and neutral Poisson regression models, Table S14
#Month6 pcl, Change LBLA, fear,neutral
#fear
M6pcl_M6LBLAfearchange <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU +  ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + LBLA_fear_change, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6LBLAfearchange) 
#IRR 95% CI
exp(confint(M6pcl_M6LBLAfearchange))
# Extract the coefficient estimates
coefficientsM6pcl_M6LBLAfearchange <- coef(M6pcl_M6LBLAfearchange)
# Calculate the incidence rate ratios
IRRsM6pcl_M6LBLAfearchange <- exp(coefficientsM6pcl_M6LBLAfearchange)
# Print the IRRs
print(IRRsM6pcl_M6LBLAfearchange)

#neutral
M6pcl_M6LBLAneutralchange <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + LBLA_neutral_change, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6LBLAneutralchange) 
#IRR 95% CI
exp(confint(M6pcl_M6LBLAneutralchange))
# Extract the coefficient estimates
coefficientsM6pcl_M6LBLAneutralchange <- coef(M6pcl_M6LBLAneutralchange)
# Calculate the incidence rate ratios
IRRsM6pcl_M6LBLAneutralchange <- exp(coefficientsM6pcl_M6LBLAneutralchange)
# Print the IRRs
print(IRRsM6pcl_M6LBLAneutralchange)


#Month6 pcl, Change RBLA, fear,neutral
#fear
M6pcl_M6RBLAfearchange <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU +  ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + RBLA_fear_change, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6RBLAfearchange) 
#IRR 95% CI
exp(confint(M6pcl_M6RBLAfearchange))
# Extract the coefficient estimates
coefficientsM6pcl_M6RBLAfearchange <- coef(M6pcl_M6RBLAfearchange)
# Calculate the incidence rate ratios
IRRsM6pcl_M6RBLAfearchange <- exp(coefficientsM6pcl_M6RBLAfearchange)
# Print the IRRs
print(IRRsM6pcl_M6RBLAfearchange)


#neutral
M6pcl_M6RBLAneutralchange <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + RBLA_neutral_change, family="poisson", data=AURORA_BLA)
summary(M6pcl_M6RBLAneutralchange) 
#IRR 95% CI
exp(confint(M6pcl_M6RBLAneutralchange))
# Extract the coefficient estimates
coefficientsM6pcl_M6RBLAneutralchange <- coef(M6pcl_M6RBLAneutralchange)
# Calculate the incidence rate ratios
IRRsM6pcl_M6RBLAneutralchange <- exp(coefficientsM6pcl_M6RBLAneutralchange)
# Print the IRRs
print(IRRsM6pcl_M6RBLAneutralchange)



#Dissociation (DESB) Models, Table S15
#WK2_DESB_RS	M6_DESB_RS
#Month6 DESB, week2 RBLA

#Month6 DESB, Month6 LBLA, 
M6DESB_M6LBLA <- glm(M6_DESB_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + M6_LBLA, family="poisson", data=AURORA_BLA)
summary(M6DESB_M6LBLA) 
#IRR 95% CI
exp(confint(M6DESB_M6LBLA))
# Extract the coefficient estimates
coefficientsM6DESB_M6LBLA<- coef(M6DESB_M6LBLA)
# Calculate the incidence rate ratios
IRRsM6DESB_M6LBLA <- exp(coefficientsM6DESB_M6LBLA)
# Print the IRRs
print(IRRsM6DESB_M6LBLA)

#Month6 DESB, Change LBLA
M6DESB_LBLAchange <- glm(M6_DESB_RS ~ Mclean + Temple + WSU +  ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + LBLA_dif, family="poisson", data=AURORA_BLA)
summary(M6DESB_LBLAchange) 
#IRR 95% CI
exp(confint(M6DESB_LBLAchange))
# Extract the coefficient estimates
coefficientsM6DESB_LBLAchange <- coef(M6DESB_LBLAchange)
# Calculate the incidence rate ratios
IRRsM6DESB_LBLAchange <- exp(coefficientsM6DESB_LBLAchange)
# Print the IRRs
print(IRRsM6DESB_LBLAchange)

#Month6 DESB, Change RBLA
M6DESB_RBLAchange <- glm(M6_DESB_RS ~ Mclean + Temple + WSU +  ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert + RBLA_fear_change, family="poisson", data=AURORA_BLA)
summary(M6DESB_RBLAchange) 
#IRR 95% CI
exp(confint(M6DESB_RBLAchange))
# Extract the coefficient estimates
coefficientsM6DESB_RBLAchange <- coef(M6DESB_RBLAchange)
# Calculate the incidence rate ratios
IRRsM6DESB_RBLAchange<- exp(coefficientsM6DESB_RBLAchange)
# Print the IRRs
print(IRRsM6DESB_RBLAchange)



#####
#Functional Connectivity partial correlation with change in BLA reactivity, controlling for site
library(ppcor)
pcor.test(AURORA_BLA_6mOnly$LBLA_dif,AURORA_BLA_6mOnly$Change_Precuneus,AURORA_BLA_6mOnly$site)
pcor.test(AURORA_BLA_6mOnly$RBLA_dif,AURORA_BLA_6mOnly$Change_Precuneus,AURORA_BLA_6mOnly$site)




#Conceptual Replication Cohort
library(readxl)
Dataset2 <- read_excel("Dataset2.xlsx")

#PSS Paired t-tests
t.test(Dataset2$PSStotal_1month,
       Dataset2$PSStotal_6month_adjust,
       paired=TRUE)

#Left and right BLA paired t-test
t.test(Dataset$LBLA,
       Dataset2$LBLA_v2,
       paired=TRUE)

t.test(Dataset2$RBLA,
       Dataset2$RBLA_v2,
       paired=TRUE)


#Standard deviation and mean
sd(Dataset2$PSStotal_1month)
mean(Dataset2$PSStotal_1month)

sd(Dataset2$PSStotal_6month_adjust)
mean(Dataset2$PSStotal_6month_adjust)

sd(Dataset$LBLA)
mean(Dataset$LBLA)

sd(Dataset2$LBLA_v2)
mean(Dataset2$LBLA_v2)

sd(Dataset2$RBLA)
mean(Dataset2$RBLA)

sd(Dataset2$RBLA_v2)
mean(Dataset2$RBLA_v2)

#Jarque-Bera test 
#skewness, kurtosis
library(moments)
jarque.test(Dataset2$PSStotal_6month_adjust)


library(e1071)
skewness(Dataset2$PSStotal_6month_adjust) 

#Poisson regression models
#Model M6 PSS, no BLA
M6PSSModel  <- glm(PSStotal_6month_adjust ~ scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2, family="poisson", data=Dataset2)
summary(M6PSSModel)
residuals <- resid(M6PSSModel)
residuals
Dataset2$PSS_residuals <- residuals

#Change in BLA reactivity and change in PTSD symptoms, Table S16
#Left
M6PSS_changeLBLA <- glm(PSStotal_6month_adjust ~  scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + LBLA_change, family="poisson", data=Dataset2)
summary(M6PSS_changeLBLA)
#McFadden R squared change
library(pscl)
pseudoR2_M6PSS_changeLBLA  <- (pR2(M6PSS_changeLBLA)['McFadden']) - (pR2(M6PSSModel)['McFadden'])
print(pseudoR2_M6PSS_changeLBLA )
#IRR 95% CI
exp(confint(M6PSS_changeLBLA))
# Extract the coefficient estimates
coefficientsLBLAdif <- coef(M6PSS_changeLBLA)
# Calculate the incidence rate ratios
IRRsLBLAdif <- exp(coefficientsLBLAdif)
# Print the IRRs
print(IRRsLBLAdif)

#Right
M6PSS_changeRBLA  <- glm(PSStotal_6month_adjust ~ scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + RBLA_change, family="poisson", data=Dataset2)
summary(M6PSS_changeRBLA )
#McFadden R squared change
library(pscl)
pseudoR2_M6PSS_changeRBLA  <- (pR2(M6PSS_changeRBLA)['McFadden']) - (pR2(M6PSSModel)['McFadden'])
print(pseudoR2_M6PSS_changeRBLA )
#IRR 95% CI
exp(confint(M6PSS_changeRBLA))
# Extract the coefficient estimates
coefficientsRBLAdif <- coef(M6PSS_changeRBLA)
# Calculate the incidence rate ratios
IRRsRBLAdif <- exp(coefficientsRBLAdif)
# Print the IRRs
print(IRRsRBLAdif)


#Sensitivity Analyses, CTQ and PDI, Table S17
#pdi_ed_total, left BLA
PDI <- glm(PSStotal_6month_adjust ~ pdi_ed_total + scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + LBLA_change, family="poisson", data=Dataset2)
summary(PDI)
exp(confint(PDI))
# Extract the coefficient estimates
coefficientsPDI <- coef(PDI)
# Calculate the incidence rate ratios
IRRscoefficientsPDI <- exp(coefficientsPDI)
# Print the IRRs
print(IRRscoefficientsPDI)

#CTQTOT
CTQ2 <- glm(PSStotal_6month_adjust ~ CTQTOT + scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + LBLA_change, family="poisson", data=Dataset2)
summary(CTQ2)
exp(confint(CTQ2))
# Extract the coefficient estimates
coefficientsCTQ2 <- coef(CTQ2)
# Calculate the incidence rate ratios
IRRscoefficientsCTQ2 <- exp(coefficientsCTQ2)
# Print the IRRs
print(IRRscoefficientsCTQ2)


#pdi_ed_total, right BLA
rightPDI <- glm(PSStotal_6month_adjust ~ pdi_ed_total + scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + RBLA_change, family="poisson", data=Dataset2)
summary(rightPDI)
exp(confint(rightPDI))
# Extract the coefficient estimates
coefficientsrightPDI <- coef(rightPDI)
# Calculate the incidence rate ratios
IRRscoefficientsrightPDI <- exp(coefficientsrightPDI)
# Print the IRRs
print(IRRscoefficientsrightPDI)

#CTQTOT, right BLA
rightCTQ2 <- glm(PSStotal_6month_adjust ~ CTQTOT + scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + RBLA_change, family="poisson", data=Dataset2)
summary(rightCTQ2)
exp(confint(rightCTQ2))
# Extract the coefficient estimates
coefficientsrightCTQ2 <- coef(rightCTQ2)
# Calculate the incidence rate ratios
IRRscoefficientsrightCTQ2 <- exp(coefficientsrightCTQ2)
# Print the IRRs
print(IRRscoefficientsrightCTQ2)


#Fearful and Neutral faces, Table S18
#LBLA  fear
LBLAFear <- glm(PSStotal_6month_adjust ~ scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + LBLA_fear_change, family="poisson", data=Dataset2)
summary(LBLAFear)
#IRR 95% CI
exp(confint(LBLAFear))
# Extract the coefficient estimates
coefficientsLBLAFear <- coef(LBLAFear)
# Calculate the incidence rate ratios
IRRsLBLAFear <- exp(coefficientsLBLAFear)
# Print the IRRs
print(IRRsLBLAFear)

#LBLA neutral
LBLANeutral <- glm(PSStotal_6month_adjust ~ scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + LBLA_neutral_change, family="poisson", data=Dataset2)
summary(LBLANeutral)
#IRR 95% CI
exp(confint(LBLANeutral))
# Extract the coefficient estimates
coefficientsLBLANeutral <- coef(LBLANeutral)
# Calculate the incidence rate ratios
IRRsLBLANeutral <- exp(coefficientsLBLANeutral)
# Print the IRRs
print(IRRsLBLANeutral)

#RBLA fear
RBLAFear <- glm(PSStotal_6month_adjust ~ scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + RBLA_fear_change, family="poisson", data=Dataset2)
summary(RBLAFear)
#IRR 95% CI
exp(confint(RBLAFear))
# Extract the coefficient estimates
coefficientsRBLAFear <- coef(RBLAFear)
# Calculate the incidence rate ratios
IRRsRBLAFear <- exp(coefficientsRBLAFear)
# Print the IRRs
print(IRRsRBLAFear)

#RBLA neutral 
RBLANeutral <- glm(PSStotal_6month_adjust ~ scanner + ed_age_sti1 + ed_race_sti4 + ed_traumacode + ed_gender_sti2 + RBLA_neutral_change, family="poisson", data=Dataset2)
summary(RBLANeutral)
#IRR 95% CI
exp(confint(RBLANeutral))
# Extract the coefficient estimates
coefficientsRBLANeutral <- coef(RBLANeutral)
# Calculate the incidence rate ratios
IRRsRBLANeutral <- exp(coefficientsRBLANeutral)
# Print the IRRs
print(IRRsRBLANeutral)


###Graphs 
#Residuals, Wk2 BLA, M6 PCL
M6pcl1 <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + Emory + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert, family="poisson", data=AURORA_BLA)
summary(M6pcl1)
residuals1 <- resid(M6pcl1)
residuals1
AURORA_BLA$PCL_residuals <- residuals1


#Residuals, M6 BLA only
M6pcl2 <- glm(M6_PCL5_RS ~ Mclean + Temple + WSU + ED_Age + ED_RaceEthCode + ED_Event_BroadClass + ED_GenderBirthCert, family="poisson", data=AURORA_BLA_6mOnly)
summary(M6pcl2)
residuals <- resid(M6pcl2)
residuals
AURORA_BLA_6mOnly$PCL_residuals <- residuals

#Figure 1B. M6PCl, Wk2 RBLA
library(ggplot2)
ggplot(data=AURORA_BLA, aes(y=PCL_residuals,x=R_BLA))+ 
  geom_point(position="jitter", size=4, colour="turquoise3") +
  geom_vline(xintercept = 0, size=.5)+
  geom_smooth(method="lm", se=TRUE, col="turquoise4", fill="paleturquoise3")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(limits=c(-.75,.75), breaks=seq(-.75,.75,0.25))+
  scale_y_continuous(limits=c(-8, 8), breaks=seq(-8,8,4))+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.y = element_text(size=20))+
  labs(x="Right BLA 2-Week Reactivity", y="M6-week PCL-5, adj.")


#Figure 1C. M6PCL, M6 LBLA
ggplot(data=AURORA_BLA_6mOnly, aes(y=PCL_residuals,x=M6_LBLA))+ 
  geom_point(position="jitter", size=4, colour="orangered3") +
  geom_vline(xintercept = 0, size=.5)+
  geom_smooth(method="lm", se=TRUE, col="orangered4", fill="orangered")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(limits=c(-.75,.75), breaks=seq(-.75,.75,0.25))+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.y = element_text(size=20))+
  labs(x="Left BLA 6-Month Reactivity", y="6-Month PCL-5, adj.")



#Figure 2A. M6 PCL, Change LBLA
ggplot(data=AURORA_BLA_6mOnly, aes(y=PCL_residuals,x=LBLA_dif))+ 
  geom_point(position="jitter", size=4, colour="khaki3") +
  geom_vline(xintercept = 0, size=.5)+
  geom_smooth(method="lm", se=TRUE, col="khaki4", fill="khaki3")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(limits=c(-8.0, 8.0), breaks=seq(-8.0,8.0,4.0))+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.y = element_text(size=20))+
  labs(x="Difference Left BLA 6 month - 2 week Reactivity", y="6-Month PCL-5, adj.")


#Figure 2B.
ggplot(data=Dataset2, aes(y=PSS_residuals,x=LBLA_change))+ 
  geom_point(position="jitter", size=4, colour="cyan4") +
  geom_vline(xintercept = 0, size=.5)+
  geom_smooth(method="lm", se=TRUE, col="darkcyan", fill="cyan4")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(limits=c(-1, 0.50), breaks=seq(-1,0.50,.5))+
  scale_y_continuous(limits=c(-8.0, 8.0), breaks=seq(-8.0,8.0,4.0))+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.y = element_text(size=20))+
  labs(x="Change Left BLA Reactivity", y="6-Month PSS, adj.")


#Figure 2C. Individual differences. 
#Create long form data frame for graph
#Create "pair" variable
library(dplyr)
AURORA_BLA_6mOnly <- AURORA_BLA_6mOnly %>%
  mutate(pair = seq_along(subject_id))
#Create long form data
library(reshape2)
AURORA_long <- melt(AURORA_BLA_6mOnly,
                    # variables to keep
                    id.vars=c("subject_id", "pair", "M6_Wk2", "LBLA_dif"),
                    # source columns
                    measure.vars=c("L_BLA", "M6_LBLA" ),
                    # Name of the column that will identify the Timepoint
                    # Name Reactivity column
                    variable.name="Timepoint",
                    value.name="Reactivity")
# Rename factor names from "L_BLA" and "M6_LBLA" to "2-Week" and "6-Month"
levels(AURORA_long$Timepoint)[levels(AURORA_long$Timepoint)=="L_BLA"] <- "2-Week"
levels(AURORA_long$Timepoint)[levels(AURORA_long$Timepoint)=="M6_LBLA"] <- "6-Month"


library(ggplot2)
ggplot(data = AURORA_long, aes(x=Timepoint,y=Reactivity, fill=Timepoint)) +
  geom_boxplot(lwd=1) + #move down
  scale_fill_manual(values = c("lightcyan1", "lightsalmon")) +
  geom_point(size=4, color="darkgrey")+ 
  geom_line(aes(group=pair, color=M6_Wk2), lwd=1.5, alpha=.5) + 
  scale_colour_gradient2(low = "orangered2",
                         mid = "grey",
                         high = "turquoise4",
                         midpoint = 0) +
  theme(text = element_text(size=25), panel.background = element_rect(fill = "white",
                                                                      colour = "white",
                                                                      size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(colour = "black"), panel.grid.major.x = element_blank())+
  scale_y_continuous(limits = c(-0.6, 0.8), breaks = seq(-0.6, 0.8, 0.2),
                     labels = function(x) round(x, 1))+
  labs(x="Timepoint", y="Left BLA Reactivity")


#Figure 2D
library(dplyr)
Dataset2 <- Dataset2 %>%
  mutate(pair = seq_along(SubjID))
#Create long form data
library(reshape2)
Dataset2_long <- melt(Dataset2,
                      # variables to keep
                      id.vars=c("SubjID", "pair", "M6_Wk2", "LBLA_change"),
                      # source columns
                      measure.vars=c("LBLA", "LBLA_v2" ),
                      # Name of the column that will identify the Timepoint
                      # Name Reactivity column
                      variable.name="Timepoint",
                      value.name="Reactivity")
# Rename factor names from "LBLA" and "LBLA_v2" to "1-Month" and "6-Month"
levels(Dataset2_long$Timepoint)[levels(Dataset2_long$Timepoint)=="LBLA"] <- "1-Month"
levels(Dataset2_long$Timepoint)[levels(Dataset2_long$Timepoint)=="LBLA_v2"] <- "6-Month"


library(ggplot2)
ggplot(data = Dataset2_long, aes(x=Timepoint,y=Reactivity, fill=Timepoint)) +
  geom_boxplot(lwd=1) + #move down
  scale_fill_manual(values = c("lightcyan1", "lightsalmon")) +
  geom_point(size=4, color="darkgrey")+ 
  geom_line(aes(group=pair, color=M6_Wk2), lwd=1.5, alpha=.5) + 
  scale_colour_gradient2(low = "orangered2",
                         mid = "grey",
                         high = "turquoise4",
                         midpoint = 0) +
  theme(text = element_text(size=25), panel.background = element_rect(fill = "white",
                                                                      colour = "white",
                                                                      size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(colour = "black"), panel.grid.major.x = element_blank())+
  scale_y_continuous(limits = c(-0.6, 0.8), breaks = seq(-0.6, 0.8, 0.2),
                     labels = function(x) round(x, 1))+
  labs(x="Timepoint", y="Left BLA Reactivity")


#Figure 3C. Change PCl, Change LBLA-Precuneus connectivity
ggplot(data=AURORA_BLA_6mOnly, aes(y=M6_Wk2,x=Change_Precuneus))+ 
  geom_point(position="jitter", size=4, colour="darkolivegreen3") +
  geom_vline(xintercept = 0, size=.5)+
  geom_smooth(method="lm", se=TRUE, col="darkolivegreen4", fill="darkolivegreen3")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.y = element_text(size=20))+
  labs(x="Left BLA-Precuneus 6 month - 2 week Connectivity", y="Change PCL-5")

#Figure S1. Whole brain Occipital, Change pcl
ggplot(data=AURORA_BLA_6mOnly, aes(y=M6_Wk2,x=WholebrainOccipital_change))+ 
  geom_point(position="jitter", size=4, colour="magenta3") +
  geom_vline(xintercept = 0, size=.5)+
  geom_smooth(method="lm", se=TRUE, col="magenta4", fill="orchid3")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),text = element_text(size=20),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(limits=c(-1.5, 1), breaks=seq(-1.5,1,0.5))+
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.y = element_text(size=20))+
  labs(x="Bilateral Occipital Cortex Reactivity", y="Change PCL-5")





