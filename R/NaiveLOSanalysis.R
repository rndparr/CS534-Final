path <- './data/dat.csv'
dat <- read.csv(path)

head(dat)

Sys.setenv('R_MAX_VSIZE'=32000000000)


########################## Functions ##############################
MAE <- function(pred, actual) {
  mean(abs(pred - actual))
}

MRE <- function(pred, actual) {
  mean(abs(pred - actual)/abs(actual))
}

RMSE <- function(pred, actual) {
  MSE <- mean((actual - pred)^2) 
  sqrt(MSE)
}

###################################################################
# Set factor vars
dat$Health.Service.Area <- as.factor(dat$Health.Service.Area)
dat$Hospital.County <- as.factor(dat$Hospital.County)
dat$Operating.Certificate.Number <- as.factor(dat$Operating.Certificate.Number)
dat$Facility.Id <- as.factor(dat$Facility.Id)
dat$Age.Group <- as.factor(dat$Age.Group)
dat$Zip <- as.factor(dat$Zip)
# dat$Zip.Code...3.digits <- as.factor(dat$Zip.Code...3.digits)
dat$Gender <- as.factor(dat$Gender)
dat$Race <- as.factor(dat$Race)
dat$Ethnicity <- as.factor(dat$Ethnicity)
dat$Type.of.Admission <- as.factor(dat$Type.of.Admission)
dat$Patient.Disposition <- as.factor(dat$Patient.Disposition)
dat$CCS.Diagnosis.Code <- as.factor(dat$CCS.Diagnosis.Code)
dat$APR.DRG.Code <- as.factor(dat$APR.DRG.Code)
dat$CCS.Procedure.Code <- as.factor(dat$CCS.Procedure.Code)
dat$APR.MDC.Code <- as.factor(dat$APR.MDC.Code)
dat$APR.Severity.of.Illness.Code <- as.factor(dat$APR.Severity.of.Illness.Code)
dat$APR.Risk.of.Mortality <- as.factor(dat$APR.Risk.of.Mortality)
dat$APR.Medical.Surgical.Description <- as.factor(dat$APR.Medical.Surgical.Description)
dat$Payment.Typology.1 <- as.factor(dat$Payment.Typology.1)
dat$Payment.Typology.2 <- as.factor(dat$Payment.Typology.2)
dat$Payment.Typology.3 <- as.factor(dat$Payment.Typology.3)
# dat$Abortion.Edit.Indicator <- as.factor(dat$Abortion.Edit.Indicator)
dat$Emergency.Department.Indicator <- as.factor(dat$Emergency.Department.Indicator)

# make Length.of.Stay numeric
dat[which(dat$Length.of.Stay == '120 +'), 'Length.of.Stay'] <- '121'
dat$Length.of.Stay <- as.numeric(dat$Length.of.Stay)


# Split into training and testing
library(caret)
set.seed(42)
intrain<-createDataPartition(y=dat$Length.of.Stay,p=0.7,list=FALSE)
training<-dat[intrain,]
testing<-dat[-intrain,]

# Linear regression
linearReg <- lm(Length.of.Stay ~ Age.Group
                + Zip
                + Gender
                + Race
                + Ethnicity
                + Type.of.Admission
                + CCS.Diagnosis.Code
                + CCS.Procedure.Code
                + APR.DRG.Code
                + APR.MDC.Code
                + APR.Severity.of.Illness.Code
                + APR.Risk.of.Mortality
                + Payment.Typology.1
                + Payment.Typology.2
                + Payment.Typology.3
                + Emergency.Department.Indicator
                + Health.Service.Area
                + Hospital.County 
                + Facility.Id, data = training)
summary(linearReg)

linearReg_preds <- predict(linearReg, testing)

linearReg_MAE <- MAE(linearReg_preds, testing$Length.of.Stay)
linearReg_MRE <- MAE(linearReg_preds, testing$Length.of.Stay)
linearReg_RMSE <- MAE(linearReg_preds, testing$Length.of.Stay)

