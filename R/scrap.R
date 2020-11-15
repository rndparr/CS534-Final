path  <- './data/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2015.csv'

dat <- read.csv(path)

head(dat)

colnames(dat)


max(as.numeric(dat$Length.of.Stay), na.rm = TRUE)

head(dat$Birth.Weight,200)

hist(dat$Birth.Weight)

max(dat$Birth.Weight)

unique(dat$Health.Service.Area)

unique(dat$Hospital.County)

length(sort(unique(dat$Facility.Name)))

length(unique(dat$Facility.Id))

dat2 <- unique(dat[,c('Facility.Name','Facility.Id')])
dat2 <- dat2[order(dat2$Facility.Name),]

duplicated(dat2$Facility.Id)

dups <- dat2$Facility.Id[duplicated(dat2$Facility.Id)]

dat3 <- dat2[dat2$Facility.Id %in% dups,]
dat3[order(dat3$Facility.Id),]
o
dat2$Facility.Id[duplicated(dat2$Facility.Id)]


head(dat[dat$Abortion.Edit.Indicator == 'Y',],30)

dat4 <- dat[dat$Abortion.Edit.Indicator == 'Y',]
unique(dat4$CCS.Diagnosis.Description)

table(dat4$CCS.Diagnosis.Description)
sum(dat$CCS.Diagnosis.Description == 'Ectopic pregnancy')
sum(dat$CCS.Diagnosis.Description == 'Spontaneous abortion')
length()


sum(dat$APR.MDC.Description == 'Pregnancy, Childbirth and the Puerperium')

sum(dat$Abortion.Edit.Indicator == 'Y')


sum(dat$Abortion.Edit.Indicator == 'Y') / sum(dat$APR.MDC.Description == 'Pregnancy, Childbirth and the Puerperium')

sum(dat$Abortion.Edit.Indicator == 'Y')/sum(is.na(dat$Facility.Id))

sum(dat$Facility.Id == NA)

sum(is.na(dat$Facility.Id))


