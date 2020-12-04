# path <- './data/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2015.csv'
path <- './data/dat.csv'
dat <- read.csv(path, 
	colClasses = c(rep('factor',13),'integer',rep('factor',16),'numeric','numeric'))
colnames(dat)
sort(as.numeric(unique(dat$APR.MDC.Code)))
######
# Error functinos
MAE <- function(pred, actual) {mean(abs(pred - actual))}
MRE <- function(pred, actual) {mean(abs(pred - actual)/abs(actual))}
RMSE <- function(pred, actual) {sqrt(mean((actual - pred)^2))}

#######

# factor names
c('Health.Service.Area', 'Hospital.County', 'Operating.Certificate.Number', 'Facility.Id', 'Age.Group', 'Zip', 'Zip.Code...3.digits', 'Gender', 'Race', 'Ethnicity', 'Type.of.Admission', 'Patient.Disposition', 'CCS.Diagnosis.Code', 'APR.DRG.Code', 'CCS.Procedure.Code', 'APR.MDC.Code', 'APR.Severity.of.Illness.Code', 'APR.Risk.of.Mortality', 'APR.Medical.Surgical.Description', 'Payment.Typology.1', 'Payment.Typology.2', 'Payment.Typology.3', 'Abortion.Edit.Indicator', 'Emergency.Department.Indicator')

# make Length.of.Stay numeric
dat[which(dat$Length.of.Stay == '120 +'), 'Length.of.Stay'] <- '121'
dat$Length.of.Stay <- as.numeric(dat$Length.of.Stay)


#######
# View summary table
library(papeR)
summarize(dat)
summarize(dat, type='factor')
cat(colnames(Filter(f = is.factor, dat)))

tab <- summarize(dat, type='factor',
	variables=c('Health.Service.Area',
# 'Hospital.County',
# 'Operating.Certificate.Number',
# 'Facility.Id',
'Age.Group',
# 'Zip',
'Gender',
'Race',
'Ethnicity',
'Type.of.Admission',
# 'CCS.Diagnosis.Code',
# 'CCS.Procedure.Code',
# 'APR.DRG.Code',
'APR.MDC.Code',
'APR.Severity.of.Illness.Code',
'APR.Risk.of.Mortality',
'APR.Medical.Surgical.Description',
'Payment.Typology.1',
'Payment.Typology.2',
'Payment.Typology.3',
'Emergency.Department.Indicator'))


#######
# library(caret)
# library(future)
library(spaMM)
spaMM.options('nb_cores' = future::availableCores() - 1)
set.seed(123)

rows <- caret::createDataPartition(y=dat$Length.of.Stay, p=0.7, list=FALSE) 
training <- dat[rows,]
test <- dat[-rows,]

rm(dat)

# dat_sub_rows <- createDataPartition(y=dat$Length.of.Stay, p=0.5, list=FALSE)
# dat_sub <- dat[dat_sub_rows,]
# dat <- dat[dat_sub_rows,]
# intrain <- createDataPartition(y=dat_sub$Length.of.Stay, p=0.7, list=FALSE) 
# training <- dat_sub[intrain,] 
# testing <-dat_sub[-intrain,] 

# rm(dat)
# rm(dat_sub)
save.image()




# GLM with zero-truncated Poisson
model1 <- glm(Length.of.Stay ~ 
		+ Age.Group
		# + Zip
		+ Gender
		+ Race
		+ Ethnicity
		+ Type.of.Admission
		# + CCS.Diagnosis.Code
		# + CCS.Procedure.Code
		# + APR.DRG.Code
		+ APR.MDC.Code
		+ APR.Severity.of.Illness.Code
		+ APR.Risk.of.Mortality
		+ Payment.Typology.1
		# + Payment.Typology.2
		# + Payment.Typology.3
		+ Emergency.Department.Indicator
		+ Health.Service.Area
		+ Hospital.County 
		+ Facility.Id,
		,
	family = Tpoisson(link = 'log'),
	data = training)
summary(model1)


# model1 <- glm(Length.of.Stay ~ 
# 		+ Age.Group
# 		# + Zip
# 		+ Gender
# 		+ Race
# 		+ Ethnicity
# 		+ Type.of.Admission
# 		# + CCS.Diagnosis.Code
# 		# + CCS.Procedure.Code
# 		# + APR.DRG.Code
# 		+ APR.MDC.Code
# 		+ APR.Severity.of.Illness.Code
# 		+ APR.Risk.of.Mortality
# 		+ Payment.Typology.1
# 		# + Payment.Typology.2
# 		# + Payment.Typology.3
# 		+ Emergency.Department.Indicator
# 		+ Health.Service.Area
# 		+ Hospital.County 
# 		# + Facility.Id,
# 		,
# 	family = Tpoisson(link = 'log'),
# 	data = training)
# summary(model1)



# model1_2 <- glm(Length.of.Stay ~ 
# 		+ Age.Group
# 		# + Zip
# 		+ Gender
# 		+ Race
# 		+ Ethnicity
# 		+ Type.of.Admission
# 		# + CCS.Diagnosis.Code
# 		# + CCS.Procedure.Code
# 		# + APR.DRG.Code
# 		+ APR.MDC.Code
# 		+ APR.Severity.of.Illness.Code
# 		+ APR.Risk.of.Mortality
# 		+ Payment.Typology.1
# 		# + Payment.Typology.2
# 		# + Payment.Typology.3
# 		+ Emergency.Department.Indicator
# 		+ Health.Service.Area
# 		+ Hospital.County 
# 		+ Facility.Id,
# 		,
# 	family = Tpoisson(link = 'log'),
# 	data = training)
# summary(model1_2)



# stepmodel1 <- MASS::stepAIC(model1)

# GLM with zero-truncated Poisson, nesting
model2 <- HLfit(Length.of.Stay ~ 
		+ Age.Group
		# + Zip
		+ Gender
		+ Race
		+ Ethnicity
		+ Type.of.Admission
		# + CCS.Diagnosis.Code
		# + CCS.Procedure.Code
		# + APR.DRG.Code
		+ APR.MDC.Code
		+ APR.Severity.of.Illness.Code
		+ APR.Risk.of.Mortality
		+ Payment.Typology.1
		# + Payment.Typology.2
		# + Payment.Typology.3
		+ Emergency.Department.Indicator
		+ Health.Service.Area
		+ (1 | Hospital.County / Health.Service.Area)
		,
	family = Tpoisson(link = 'log'), 
	data = training)
summary(model2, details=TRUE, max.print=99999999)


# GLM with zero-truncated Poisson, nesting
model3 <- HLfit(Length.of.Stay ~ 
		+ Age.Group
		# + Zip
		+ Gender
		+ Race
		+ Ethnicity
		+ Type.of.Admission
		# + CCS.Diagnosis.Code
		# + CCS.Procedure.Code
		# + APR.DRG.Code
		+ APR.MDC.Code
		+ APR.Severity.of.Illness.Code
		+ APR.Risk.of.Mortality
		+ Payment.Typology.1
		# + Payment.Typology.2
		# + Payment.Typology.3
		+ Emergency.Department.Indicator
		+ Health.Service.Area
		+ (1 | Hospital.County / Health.Service.Area)
		+ (1 | Facility.Id / Hospital.County / Health.Service.Area)
		,
	family = Tpoisson(link = 'log'), 
	data = training)
summary(model3, details=TRUE, max.print=99999999)





# # GLM with zero-truncated Poisson
# model1 <- glmmTMB(Length.of.Stay ~ 
# 		+ Age.Group
# 		# + Zip
# 		+ Gender
# 		+ Race
# 		+ Ethnicity
# 		+ Type.of.Admission
# 		# + CCS.Diagnosis.Code
# 		# + CCS.Procedure.Code
# 		# + APR.DRG.Code
# 		+ APR.MDC.Code
# 		+ APR.Severity.of.Illness.Code
# 		+ APR.Risk.of.Mortality
# 		+ Payment.Typology.1
# 		# + Payment.Typology.2
# 		# + Payment.Typology.3
# 		+ Emergency.Department.Indicator
# 		+ Health.Service.Area
# 		+ Hospital.County 
# 		# + Facility.Id,
# 		,
# 	family = truncated_poisson(link = 'log'),
# 	data = training)
# summary(model1)
# # [1:464216,]

# # GLM with zero-truncated Poisson, nesting
# model2 <- glmmTMB(Length.of.Stay ~ 
# 		+ Age.Group
# 		# + Zip
# 		+ Gender
# 		+ Race
# 		+ Ethnicity
# 		+ Type.of.Admission
# 		# + CCS.Diagnosis.Code
# 		# + CCS.Procedure.Code
# 		# + APR.DRG.Code
# 		+ APR.MDC.Code
# 		+ APR.Severity.of.Illness.Code
# 		+ APR.Risk.of.Mortality
# 		+ Payment.Typology.1
# 		# + Payment.Typology.2
# 		# + Payment.Typology.3
# 		+ Emergency.Department.Indicator
# 		+ Health.Service.Area
# 		+ (1 | Hospital.County / Health.Service.Area)
# 		+ (1 | Facility.Id / Hospital.County / Health.Service.Area)
# 		,
# 	family = truncated_poisson(link = 'log'), 
# 	data = dat2)


unique(dat2$Length.of.Stay)
