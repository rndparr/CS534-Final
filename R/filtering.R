path  <- './data/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2015.csv'
dat <- read.csv(path)

######
# rename Zip.Code...3.digits
colnames(dat)[which(colnames(dat) == 'Zip.Code...3.digits')] <- 'Zip'

######
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

# DUE TO MEMORY ISSUES ONLY LOAD WHEN YOU WANNA USE IT
# infant data
infant_dat <- dat[dat$Birth.Weight > 0, ]

# obgyn data
obgyn_dat <- dplyr::filter(dat, (CCS.Diagnosis.Code %in% ccs_obgyn_codes | APR.DRG.Code %in% aprdrg_obgyn_codes | APR.MDC.Code == 14 | APR.MDC.Code == 15 | Abortion.Edit.Indicator == 'Y' | Type.of.Admission == 'Newborn') & Birth.Weight == 0 )

# non-obgyn data
dat <- dplyr::filter(dat, !(CCS.Diagnosis.Code %in% ccs_obgyn_codes | APR.DRG.Code %in% aprdrg_obgyn_codes | APR.MDC.Code == 14 | APR.MDC.Code == 15 | Abortion.Edit.Indicator == 'Y' | Type.of.Admission == 'Newborn')  & Birth.Weight == 0)

dat <- dat[, which(!(colnames(dat) %in% c('Abortion.Edit.Indicator','Birth.Weight')))]

#######
# write to file
write.csv(infant_dat, file = './data/infant.csv', row.names = FALSE)
write.csv(obgyn_dat, file = './data/obgyn_dat.csv', row.names = FALSE)
write.csv(dat, file = './data/dat.csv', row.names = FALSE)

