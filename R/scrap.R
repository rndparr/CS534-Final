path  <- './data/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2015.csv'

dat <- read.csv(path)

head(dat)

colnames(dat)


#######
# OBGYN FILTERING

aprdrg <- unique(dat[,c('APR.DRG.Code','APR.DRG.Description')])
aprdrg <- aprdrg[order(aprdrg$APR.DRG.Code),]
rownames(aprdrg) <- NULL
# paste0(aprdrg[,1],'-',aprdrg[,2]) # view

ccs <- unique(dat[,c('CCS.Diagnosis.Code','CCS.Diagnosis.Description')])
ccs <- ccs[order(ccs$CCS.Diagnosis.Code),]
rownames(ccs) <- NULL
# paste0(ccs[,1],'-',ccs[,2]) # view

# terms for filtering out OB-related
obgyn_terms <- c('abortion', 'amniot', 'birth', 'C-section', 'delivery', 'fetal', 'fetopelvic', 'labor', 'liveborn', 'malposition', 'natal','neonat', 'obstetric', 'partum', 'pregnancy', 'OB-related', 'umbilical cord complication')
exclude_terms <- c('non-obstetric', 'except in labor')

# get APR.DRG.Code, CCS.Diagnosis.Code to filter out
aprdrg_obgyn_desc <- grep(paste0(obgyn_terms, collapse='|'), aprdrg$APR.DRG.Description, ignore.case=TRUE, value=TRUE)
aprdrg_obgyn_desc <- grep(paste0(exclude_terms, collapse='|') , aprdrg_obgyn_desc, ignore.case=TRUE, invert=TRUE, value=TRUE)
aprdrg_obgyn_codes <- aprdrg[aprdrg$APR.DRG.Description %in% aprdrg_obgyn_desc, 'APR.DRG.Code']

ccs_obgyn_desc <- grep(paste0(obgyn_terms, collapse='|'), ccs$CCS.Diagnosis.Description, ignore.case=TRUE, value=TRUE)
ccs_obgyn_desc <- grep(paste0(exclude_terms, collapse='|') , ccs_obgyn_desc, ignore.case=TRUE, invert=TRUE, value=TRUE)
ccs_obgyn_codes <- ccs[ccs$CCS.Diagnosis.Description %in% ccs_obgyn_desc, 'CCS.Diagnosis.Code']

# infant data
infant_dat <- dat[dat$Birth.Weight > 0, ]

# obgyn data
obgyn_dat <- dplyr::filter(dat, (CCS.Diagnosis.Code %in% ccs_obgyn_codes | APR.DRG.Code %in% aprdrg_obgyn_codes | APR.MDC.Code == 14 | APR.MDC.Code == 15 | Abortion.Edit.Indicator == 'Y' | Type.of.Admission == 'Newborn') & Birth.Weight == 0 )

# non-obgyn data
dat2 <- dplyr::filter(dat, !(CCS.Diagnosis.Code %in% ccs_obgyn_codes | APR.DRG.Code %in% aprdrg_obgyn_codes | APR.MDC.Code == 14 | APR.MDC.Code == 15 | Abortion.Edit.Indicator == 'Y' | Type.of.Admission == 'Newborn')  & Birth.Weight == 0)


# nrow(dat)
# nrow(infant_dat)
# nrow(obgyn_dat)
# nrow(dat2)


# nrow(infant_dat) + nrow(obgyn_dat) + nrow(dat2)

####

dat5 <- obgyn_dat[obgyn_dat$Birth.Weight == 0, ]
unique(dat5$Age.Group)


head(dat5)

head(obgyn_dat)

dat3 <- dat2[dat2$Birth.Weight > 0, ]
unique(dat3$Age.Group)



dat4 <- obgyn_dat[obgyn_dat$Birth.Weight > 0, ]
unique(dat4$Age.Group)
head(dat4)

table(dat3$CCS.Diagnosis.Description)

# obgyn_filter <- CCS.Diagnosis.Code %in% ccs_obgyn_codes | APR.DRG.Code %in% aprdrg_obgyn_codes | APR.MDC.Code == 14 | APR.MDC.Code == 15 | Abortion.Edit.Indicator == 'Y' | Type.of.Admission == 'Newborn'


nrow(dat3)

unique(dat3$Facility.Id)


 unique(dat3$Facility.Name)

head(dat3)
obgyn_dat

dat <- 

head(obgyn_dat)



unique(dat$Type.of.Admission)

######

ccs <- unique(dat[,c('CCS.Diagnosis.Code','CCS.Diagnosis.Description')])
ccs <- ccs[order(ccs$CCS.Diagnosis.Code),]
rownames(ccs) <- NULL
ccs <- ccs[order(ccs$CCS.Diagnosis.Description),]
paste0(ccs[,1],'-',ccs[,2])

168-196



aprdrg <- unique(dat[,c('APR.DRG.Code','APR.DRG.Description')])
aprdrg <- aprdrg[order(aprdrg$APR.DRG.Code),]
rownames(aprdrg) <- NULL
# aprdrg <- aprdrg[order(aprdrg$APR.DRG.Description),]
paste0(aprdrg[,1],'-',aprdrg[,2])

obgyn_terms <- c('abortion', 'amniot', 'birth', 'C-section', 'delivery', 'fetal', 'fetopelvic', 'labor', 'liveborn', 'malposition', 'neonat', 'obstetric', 'partum', 'pregnancy', 'OB-related', 'umbilical cord complication')

# obgyn_terms <- c('abortion', 'amniot', 'birth', 'C-section', 'contraceptive', 'delivery', 'endometriosis', 'female genital', 'female infert', 'female pelvic', 'female reproductive', 'fetal', 'fetopelvic', 'hysterectomy', 'labor', 'liveborn', 'malposition', 'menopaus', 'menstrua', 'neonat', 'obstetric', 'ovarian cyst', 'partum', 'pregnancy', 'umbilical', 'uterin', 'vagin', 'vulva')
# exclude_terms <- c('cancer', 'except in labor', 'except inguinal, femoral & umbilical', 'malig')

exclude_terms <- c('non-obstetric', 'except in labor')

# some of these are radical and could be cancer-related?; may be better to use CCS criteria; going to try filtering and see if any of the ambigious ones removed
aprdrg_obgyn_desc <- grep(paste0(obgyn_terms, collapse='|'), aprdrg$APR.DRG.Description, ignore.case=TRUE, value=TRUE)
aprdrg_obgyn_desc <- grep(paste0(exclude_terms, collapse='|') , aprdrg_obgyn_desc, ignore.case=TRUE, invert=TRUE, value=TRUE)
aprdrg_obgyn_codes <- aprdrg[aprdrg$APR.DRG.Description %in% aprdrg_obgyn_desc, 'APR.DRG.Code']


# grep('amniot', aprdrg$APR.DRG.Description, ignore.case=TRUE, value=TRUE)
# grep('amniot', ccs$CCS.Diagnosis.Description, ignore.case=TRUE, value=TRUE)



ccs_obgyn_desc <- grep(paste0(obgyn_terms, collapse='|'), ccs$CCS.Diagnosis.Description, ignore.case=TRUE, value=TRUE)
ccs_obgyn_desc <- grep(paste0(exclude_terms, collapse='|') , ccs_obgyn_desc, ignore.case=TRUE, invert=TRUE, value=TRUE)
ccs_obgyn_codes <- ccs[ccs$CCS.Diagnosis.Description %in% ccs_obgyn_desc, 'CCS.Diagnosis.Code']


bleh <- dat[!(dat$CCS.Diagnosis.Code %in% ccs_obgyn_codes),  c('APR.DRG.Code','APR.DRG.Description','CCS.Diagnosis.Code','CCS.Diagnosis.Description')]

ccs_bleh <- unique(bleh[,c('CCS.Diagnosis.Code','CCS.Diagnosis.Description')])
ccs_bleh <- ccs_bleh[order(ccs_bleh$CCS.Diagnosis.Code),]
rownames(ccs_bleh) <- NULL
ccs_bleh <- ccs_bleh[order(ccs_bleh$CCS.Diagnosis.Description),]
paste0(ccs_bleh[,1],'-',ccs_bleh[,2])


aprdrg_bleh <- unique(bleh[,c('APR.DRG.Code','APR.DRG.Description')])
aprdrg_bleh <- aprdrg_bleh[order(aprdrg_bleh$APR.DRG.Code),]
rownames(aprdrg_bleh) <- NULL
paste0(aprdrg_bleh[,1],'-',aprdrg_bleh[,2])


#######

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


