#Logistic Regression HW 2
#Downloading needed packages
install.packages("MASS")
install.packages("unbalanced")
install.packages("multcomp")
install.packages("brglm")
install.packages("car")

install.packages("visreg")


install.packages("mgcv")

library(MASS)
library(visreg)
library(brglm)

library(mgcv)
library(haven)
library(unbalanced)
library(multcomp)

#Uploading the data
input.file1 <- "insurance_t_bin.sas7bdat"

# Reads the data 

insurance <- read_sas(paste( input.file1,sep = ""))

#Testing to see what variables have missing values 
apply(is.na(insurance),2,sum)
#Variables with missing values  INV CC CCPURC HMOWN 
#Converting variables to factors and initializing a missing variable 

insurance$INV= as.factor(insurance$INV)
levels(insurance$INV)=c(levels(insurance$INV), "M")
insurance$CC= as.factor(insurance$CC)
levels(insurance$CC)=c(levels(insurance$CC), "M")
insurance$CCPURC= as.factor(insurance$CCPURC)
levels(insurance$CCPURC)=c(levels(insurance$CCPURC), "M")
insurance$HMOWN= as.factor(insurance$HMOWN)
levels(insurance$HMOWN)=c(levels(insurance$HMOWN), "M")

#Replacing missing values in 4 columns with M

#Replacing INV
for (i in 1:8495){
  if( is.na(insurance$INV[i]==TRUE)){

    insurance$INV[i]="M"
  }
}
insurance$INV


#Replacing CC
for (i in 1:8495){
  if( is.na(insurance$CC[i]==TRUE)){
    
    insurance$CC[i]="M"
  }
}
#Replacing CCPURC
for (i in 1:8495){
  if( is.na(insurance$CCPURC[i]==TRUE)){
    
    insurance$CCPURC[i]="M"
  }
}
#Replacing HMOWN
for (i in 1:8495){
  if( is.na(insurance$HMOWN[i]==TRUE)){
    
    insurance$HMOWN[i]="M"
  }
}
#Checking to make sure there are no more missing values

apply(is.na(insurance),2,sum)

#Checking for Convergence issues 

table(insurance$DDA, insurance$INS) #good
table(insurance$CASHBK, insurance$INS) #Convergence Issue
table(insurance$DIRDEP, insurance$INS) #good
table(insurance$NSF, insurance$INS) #good
table(insurance$SAV, insurance$INS) #good
table(insurance$ATM, insurance$INS) #good
table(insurance$CD, insurance$INS) #good
table(insurance$IRA, insurance$INS) #good
table(insurance$LOC, insurance$INS) #good
table(insurance$INV, insurance$INS) #good
table(insurance$ILS, insurance$INS) #good
table(insurance$MM, insurance$INS) #good
table(insurance$MMCRED, insurance$INS) #Convergence Issue
table(insurance$MTG, insurance$INS) #good
table(insurance$CC, insurance$INS) #good
table(insurance$CCPURC, insurance$INS) #good
table(insurance$SDB, insurance$INS) #good
table(insurance$HMOWN, insurance$INS) #good
table(insurance$MOVED, insurance$INS) #good
table(insurance$INAREA, insurance$INS) #good
table(insurance$BRANCH, insurance$INS) #good
table(insurance$RES, insurance$INS) #good
table(insurance$DDABAL_Bin, insurance$INS) #good
table(insurance$ACCTAGE_Bin, insurance$INS) #good
table(insurance$DEPAMT_Bin, insurance$INS) #good
table(insurance$CHECKS_Bin, insurance$INS) #good
table(insurance$NSFAMT_Bin, insurance$INS) #good
table(insurance$PHONE_Bin, insurance$INS) #good
table(insurance$TELLER_Bin, insurance$INS) #good
table(insurance$SAVBAL_Bin, insurance$INS) #good
table(insurance$ATMAMT_Bin, insurance$INS) #good
table(insurance$POS_Bin, insurance$INS) #good
table(insurance$POSAMT_Bin, insurance$INS) #good
table(insurance$CDBAL_Bin, insurance$INS) #good
table(insurance$IRABAL_Bin, insurance$INS) #good
table(insurance$LOCBAL_Bin, insurance$INS) #good
table(insurance$INVBAL_Bin, insurance$INS) #good
table(insurance$ILSBAL_Bin, insurance$INS) #good
table(insurance$MMBAL_Bin, insurance$INS) #good
table(insurance$MTGBAL_Bin, insurance$INS) #good
table(insurance$CCBAL_Bin, insurance$INS) #good
table(insurance$INCOME_Bin, insurance$INS) #good
table(insurance$LORES_Bin, insurance$INS) #good
table(insurance$HMVAL_Bin, insurance$INS) #good
table(insurance$AGE_Bin, insurance$INS) #good
table(insurance$CRSCORE_Bin, insurance$INS) #good

#Variables with convergence issues MMCRED CASHBK
#Seeing what categories to combine for MMCRED to solve convergence 

table(insurance$MMCRED, insurance$INS)
#Combining categories 3 and 5 to a 3+ category 
insurance$MMCRED[which(insurance$MMCRED>=3)]="3+"
table(insurance$MMCRED, insurance$INS)

#Seeing what categories to combine for CASHBK to solve convergence 
table(insurance$CASHBK, insurance$INS) 
#Combining categories 1 and 2 to a 1+ category 
insurance$CASHBK[which(insurance$CASHBK>=1)]="1+"
table(insurance$CASHBK, insurance$INS) 

#Using backward selection to find a model 
#Model with all the variables in it 
table(insurance$CC, insurance$INS)
typeof(insurance$CC)

full.model <- glm(INS ~ factor(DDA) + factor(CASHBK) + factor(DIRDEP) + factor(NSF) + factor(SAV)
                  + factor(ATM)+ factor(CD)+ factor(IRA)+ factor(LOC) + factor(INV)+ factor(ILS)
                  + factor(MM) + factor(MMCRED) + factor(MTG) + factor(CC) + factor(CCPURC) + factor(SDB)
                  + factor(HMOWN) + factor(MOVED) + factor(INAREA) + factor(BRANCH) + factor(RES) + factor(DDABAL_Bin)
                  + factor(ACCTAGE_Bin) + factor(DEPAMT_Bin) + factor(CHECKS_Bin) + factor(NSFAMT_Bin) 
                  + factor(PHONE_Bin) + factor(TELLER_Bin) + factor(SAVBAL_Bin) + factor(ATMAMT_Bin) 
                  + factor(POS_Bin) + factor(POSAMT_Bin) + factor(CDBAL_Bin) + factor(IRABAL_Bin) 
                  + factor(LOCBAL_Bin) + factor(INVBAL_Bin) + factor(MMBAL_Bin) + factor(ILSBAL_Bin)
                  + factor(MTGBAL_Bin) + factor(INCOME_Bin) + factor(CCBAL_Bin) + factor(LORES_Bin)
                  + factor(HMVAL_Bin) + factor(AGE_Bin) + factor(CRSCORE_Bin), 
                  data = insurance, family = binomial(link = "logit"))
#Running a backward selection
#Finding an what to set k equal to so p values are less than .002
qchisq(0.002, 1, lower.tail=FALSE)
#k should equal 9.549536

back.model <- step(full.model, direction = "backward", k=9.549536)
summary(back.model)
#CC M is NA
ftable( insurance$INS,insurance$INV, insurance$CC)
#Found that CC and INV have a linear combination problem 
#Trying model with out CC
model_without_CC<- glm(INS ~ factor(NSF) + factor(MTG)  + factor(ILS) + factor(INV) + factor(IRA) + factor(DDA)+ factor(TELLER_Bin) 
                        + factor(ATMAMT_Bin) + factor(CHECKS_Bin) + factor(MM) + factor(CDBAL_Bin) + factor(DDABAL_Bin) 
                      + factor(SAVBAL_Bin),  data = insurance, family = binomial(link = "logit"))
summary(model_without_CC)

#Removing either variable solves the problem of NA for CC

#Final Variables in the  Backward Selection Model 
#factor(NSF)  factor(MTG)   factor(ILS)  factor(INV) or factor(CC) factor(IRA)  factor(DDA) factor(TELLER_Bin) 
#factor(ATMAMT_Bin)  factor(CHECKS_Bin)  factor(MM)  factor(CDBAL_Bin) factor(DDABAL_Bin)  factor(SAVBAL_Bin)

#The backward selection model
backward.model <- glm(INS ~ factor(NSF) + factor(MTG)  + factor(ILS) + factor(INV) + factor(IRA) + factor(DDA)+ factor(TELLER_Bin) 
                      + factor(CC)  + factor(ATMAMT_Bin) + factor(CHECKS_Bin) + factor(MM) + factor(CDBAL_Bin) + factor(DDABAL_Bin) 
                      + factor(SAVBAL_Bin),  data = insurance, family = binomial(link = "logit"))
#Finding the odds ratio
exp(
  cbind(coef(backward.model), confint(backward.model))
)


#Forward selection model 
#The backward selection model
backward.model <- glm(INS ~ factor(NSF) + factor(MTG)  + factor(ILS) + factor(INV) + factor(IRA) + factor(DDA)+ factor(TELLER_Bin) 
                      + factor(CC)  + factor(ATMAMT_Bin) + factor(CHECKS_Bin) + factor(MM) + factor(CDBAL_Bin) + factor(DDABAL_Bin) 
                      + factor(SAVBAL_Bin), data = insurance, family = binomial(link = "logit"))

#Full interaction model without CC
interaction_without_CC.model <- glm(INS ~ (factor(NSF) + factor(MTG)  + factor(ILS) + factor(INV) + factor(IRA) + factor(DDA)+ factor(TELLER_Bin) 
                         + factor(ATMAMT_Bin) + factor(CHECKS_Bin) + factor(MM) + factor(CDBAL_Bin) + factor(DDABAL_Bin) 
                         + factor(SAVBAL_Bin))^2, data = insurance, family = binomial(link = "logit"))
summary(interaction_without_CC.model)
#Model Did not converge 



#Forward Selection without CC
for.model <- step(backward.model, 
                  scope = list(lower=formula(backward.model),
                               upper=formula(interaction_without_CC.model)), 
                  direction = "forward", k=9.549536)
summary(for.model)


#Finding the problem combinations 
#NSF
ftable( insurance$INS,insurance$NSF, insurance$MTG)
ftable( insurance$INS,insurance$NSF, insurance$ILS)
ftable( insurance$INS,insurance$NSF, insurance$INV)
ftable( insurance$INS,insurance$NSF, insurance$IRA)
ftable( insurance$INS,insurance$NSF, insurance$DDA)#Convergence problem 
ftable( insurance$INS,insurance$NSF, insurance$TELLER_Bin)
ftable( insurance$INS,insurance$NSF, insurance$CC)
ftable( insurance$INS,insurance$NSF, insurance$ATMAMT_Bin)
ftable( insurance$INS,insurance$NSF, insurance$CHECKS_Bin)
ftable( insurance$INS,insurance$NSF, insurance$MM)
ftable( insurance$INS,insurance$NSF, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$NSF, insurance$DDABAL_Bin)

#MTG

ftable( insurance$INS,insurance$MTG, insurance$ILS)
ftable( insurance$INS,insurance$MTG, insurance$INV)
ftable( insurance$INS,insurance$MTG, insurance$IRA)
ftable( insurance$INS,insurance$MTG, insurance$DDA)
ftable( insurance$INS,insurance$MTG, insurance$TELLER_Bin)
ftable( insurance$INS,insurance$MTG, insurance$CC)#Convergence problem 
ftable( insurance$INS,insurance$MTG, insurance$ATMAMT_Bin)
ftable( insurance$INS,insurance$MTG, insurance$CHECKS_Bin)
ftable( insurance$INS,insurance$MTG, insurance$MM)
ftable( insurance$INS,insurance$MTG, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$MTG, insurance$DDABAL_Bin)

#ILS
ftable( insurance$INS,insurance$ILS, insurance$INV)
ftable( insurance$INS,insurance$ILS, insurance$IRA)
ftable( insurance$INS,insurance$ILS, insurance$DDA)
ftable( insurance$INS,insurance$ILS, insurance$TELLER_Bin)
ftable( insurance$INS,insurance$ILS, insurance$CC)#Convergence problem 
ftable( insurance$INS,insurance$ILS, insurance$ATMAMT_Bin)
ftable( insurance$INS,insurance$ILS, insurance$CHECKS_Bin)
ftable( insurance$INS,insurance$ILS, insurance$MM)
ftable( insurance$INS,insurance$ILS, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$ILS, insurance$DDABAL_Bin)

#INV

ftable( insurance$INS,insurance$INV, insurance$IRA)
ftable( insurance$INS,insurance$INV, insurance$DDA)
ftable( insurance$INS,insurance$INV, insurance$TELLER_Bin)
ftable( insurance$INS,insurance$INV, insurance$CC)#Convergence problem 
ftable( insurance$INS,insurance$INV, insurance$ATMAMT_Bin)
ftable( insurance$INS,insurance$INV, insurance$CHECKS_Bin)
ftable( insurance$INS,insurance$INV, insurance$MM)
ftable( insurance$INS,insurance$INV, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$INV, insurance$DDABAL_Bin)

#IRA
ftable( insurance$INS,insurance$IRA, insurance$DDA)
ftable( insurance$INS,insurance$IRA, insurance$TELLER_Bin)
ftable( insurance$INS,insurance$IRA, insurance$CC) 
ftable( insurance$INS,insurance$IRA, insurance$ATMAMT_Bin)
ftable( insurance$INS,insurance$IRA, insurance$CHECKS_Bin)
ftable( insurance$INS,insurance$IRA, insurance$MM)
ftable( insurance$INS,insurance$IRA, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$IRA, insurance$DDABAL_Bin)

#DDA
ftable( insurance$INS,insurance$DDA, insurance$TELLER_Bin)#Convergence problem 
ftable( insurance$INS,insurance$DDA, insurance$CC)
ftable( insurance$INS,insurance$DDA, insurance$ATMAMT_Bin)
ftable( insurance$INS,insurance$DDA, insurance$CHECKS_Bin)#Convergence problem 
ftable( insurance$INS,insurance$DDA, insurance$MM)
ftable( insurance$INS,insurance$DDA, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$DDA, insurance$DDABAL_Bin)#Convergence problem

#TELLER_Bin
ftable( insurance$INS,insurance$TELLER_Bin, insurance$CC)
ftable( insurance$INS,insurance$TELLER_Bin, insurance$ATMAMT_Bin)
ftable( insurance$INS,insurance$TELLER_Bin, insurance$CHECKS_Bin)
ftable( insurance$INS,insurance$TELLER_Bin, insurance$MM)
ftable( insurance$INS,insurance$TELLER_Bin, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$TELLER_Bin, insurance$DDABAL_Bin)


#CC
ftable( insurance$INS,insurance$CC, insurance$ATMAMT_Bin)
ftable( insurance$INS,insurance$CC, insurance$CHECKS_Bin)
ftable( insurance$INS,insurance$CC, insurance$MM)
ftable( insurance$INS,insurance$CC, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$CC, insurance$DDABAL_Bin)

#ATMAMT_Bin
ftable( insurance$INS,insurance$ATMAMT_Bin, insurance$CHECKS_Bin)
ftable( insurance$INS,insurance$ATMAMT_Bin, insurance$MM)
ftable( insurance$INS,insurance$ATMAMT_Bin, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$ATMAMT_Bin, insurance$DDABAL_Bin)

#CHECKS_Bin
ftable( insurance$INS,insurance$CHECKS_Bin, insurance$MM)
ftable( insurance$INS,insurance$CHECKS_Bin, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$CHECKS_Bin, insurance$DDABAL_Bin)

#MM
ftable( insurance$INS,insurance$MM, insurance$CDBAL_Bin)
ftable( insurance$INS,insurance$MM, insurance$DDABAL_Bin)

#CDBAL_Bin
ftable( insurance$INS,insurance$CDBAL_Bin, insurance$DDABAL_Bin)

#Combinations with convergence issues, NSF:DDA, MTG:CC, ILS:CC, INV:CC, DDA:TELLER_Bin, DDA:CHECKS_Bin, DDA:DDABAL_Bin
ftable( insurance$INS,insurance$NSF, insurance$DDA)#Convergence problem 
ftable( insurance$INS,insurance$MTG, insurance$CC)#Convergence problem 
ftable( insurance$INS,insurance$ILS, insurance$CC)#Convergence problem 
ftable( insurance$INS,insurance$INV, insurance$CC)#Convergence problem 
ftable( insurance$INS,insurance$DDA, insurance$TELLER_Bin)#Convergence problem 
ftable( insurance$INS,insurance$DDA, insurance$CHECKS_Bin)#Convergence problem 
ftable( insurance$INS,insurance$DDA, insurance$DDABAL_Bin)#Convergence problem

#Final Model found in SAS
# Logistic Regression Model #
logit.model <- glm(low ~ age + lwt + factor(smoke) + factor(race), 
                   data = bwt, family = binomial(link = "logit"))
summary(logit.model)

# Overall Test for Categorical Variables #
logit.model.r <- glm(low ~ age + lwt + factor(smoke), 
                     data = bwt, family = binomial(link = "logit"))

anova(logit.model, logit.model.r, test = 'LRT')



