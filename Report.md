---
title: 'CS534 Final Report'
subtitle: 'Predicting Patient Length of Stay and Cost per Day'
author: 'Courtney Gale & Randy Parrish'
date: '12/04/2020'
output: 
  html_document:
    number_sections: true
    fig_caption: true
    toc: true
    toc_depth: 2
    toc_float: 
      collapsed: false
      smooth_scroll: false
    fig_width: 7
    fig_height: 4.5
    pandoc_args: [
      "--bibliography", "input/biblio.bib",
      "--csl", "input/citestyle.csl",
      "--metadata", "link-citations=true"
      ]
    code_folding: hide
    theme: lumen
    highlight: tango
    keep_md: true
---


# Introduction
Predicting patient outcomes early in the admission process is an important part of hospital management [@tierneyPredictingInpatientCosts1995;@tsaiLengthHospitalStay2016]. Estimating a patient's length of stay, for instance, assists in logistics such as allocation of hospital resources [@tsaiLengthHospitalStay2016]. The ability to predict patient-specific healthcare costs helps us gain understanding of the situations that lead to high healthcare costs and can inform future decisions in preventative care, which has been shown to be one of the best ways to decrease healthcare cost [@yangMachineLearningApproaches2018].  

For our final project we will be using the Statewide Planning and Research Cooperative System (SPARCS) Inpatient De-identified File dataset (originally provided the New York State Department of Health [@newyorkstatedepartmentofhealthStatewidePlanningResearch2017]) as it appears on Kaggle to predict length of stay (LOS) in days, a metric used to assess hospital efficiency and quality of care [@oecdAverageLengthStay2011;@robinsonPredictionHospitalLength1966;@tsaiLengthHospitalStay2016], and patient cost per day. The full dataset contains nearly 2.35 million de-identified observations and 37 features about hospital inpatient discharge data for 2015.


# Set Up

<style type="text/css">
pre {
  max-height: 300px;
  overflow-y: auto;
}

pre[class] {
  max-height: 600px;
}
</style>



## Libraries


```{.r .fold-show}
library(ggplot2)
library(ggiraph)
library(kableExtra)
library(lme4)
library(spaMM)
spaMM.options(nb_cores = future::availableCores() - 2)
```

## Miscellaneous Functions


```{.r .fold-show}
report_table <- function(x, w = NULL, h = "500px", ec = NULL) {
    scroll_box(kable_paper(kable_styling(kbl(x, row.names = FALSE), full_width = T, 
        position = "center", bootstrap_options = c("striped", "hover"), fixed_thead = T), 
        full_width = T), fixed_thead = T, height = h, width = w, extra_css = ec)
}
```

# Data Import


```{.r .fold-show}
# Import the data
dat <- read.csv("input/Hospital_Inpatient_Discharges__SPARCS_De-Identified___2015.csv", 
    colClasses = c(rep("factor", 10), "character", rep("factor", 21), "numeric", 
        rep("factor", 2), rep("character", 2)), na.strings = c("NA", ""))
```

We will also be incorporating two more variables into our analysis from a second dataset called Health Facility General Information which is also provided by the New York State Department of Health. We can match the Facility IDs between the two datasets and be able to use two additional variables in the analysis, Facility Type and Ownership Type [@newyorkstatedepartmentofhealthHealthFacilityGeneral2014].


```{.r .fold-show}
# read in facility id and description columns from facility information dataset
dat_hf <- read.csv("input/Health_Facility_General_Information.csv", colClasses = c("factor", 
    rep("NULL", 2), "factor", rep("NULL", 28), "factor", rep("NULL", 3)), skipNul = TRUE)

# remove duplicate rows
dat_hf <- dat_hf[!duplicated(dat_hf), ]

# rename columns
colnames(dat_hf) <- c("Facility.Id", "Facility.Type", "Ownership.Type")

# merge datasets by Facility Id
dat <- droplevels(merge(dat, dat_hf, by = "Facility.Id", all.x = TRUE, sort = FALSE))
```


```r
report_table(head(dat, 50))
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:500px; "><table class="table table-striped table-hover lightable-paper" style='margin-left: auto; margin-right: auto; font-family: "Arial Narrow", arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Facility.Id </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Health.Service.Area </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Hospital.County </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Operating.Certificate.Number </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Facility.Name </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Age.Group </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Zip.Code...3.digits </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Gender </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Race </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Ethnicity </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Length.of.Stay </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Type.of.Admission </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Patient.Disposition </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Discharge.Year </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> CCS.Diagnosis.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> CCS.Diagnosis.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> CCS.Procedure.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> CCS.Procedure.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.DRG.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.DRG.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.MDC.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.MDC.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.Severity.of.Illness.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.Severity.of.Illness.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.Risk.of.Mortality </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.Medical.Surgical.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Payment.Typology.1 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Payment.Typology.2 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Payment.Typology.3 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Attending.Provider.License.Number </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Operating.Provider.License.Number </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Other.Provider.License.Number </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Birth.Weight </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Abortion.Edit.Indicator </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Emergency.Department.Indicator </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Total.Charges </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Total.Costs </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Facility.Type </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Ownership.Type </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease and bronchiectasis </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 197453 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $18309.20 </td>
   <td style="text-align:left;"> $10309.98 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 108 </td>
   <td style="text-align:left;"> Congestive heart failure; nonhypertensive </td>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> HEMODIALYSIS </td>
   <td style="text-align:left;"> 194 </td>
   <td style="text-align:left;"> Heart failure </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 40003993 </td>
   <td style="text-align:left;"> 193890 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $12310.76 </td>
   <td style="text-align:left;"> $6034.20 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Septicemia (except in labor) </td>
   <td style="text-align:left;"> 222 </td>
   <td style="text-align:left;"> BLOOD TRANSFUSION </td>
   <td style="text-align:left;"> 720 </td>
   <td style="text-align:left;"> Septicemia &amp; disseminated infections </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 266409 </td>
   <td style="text-align:left;"> 266335 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $48242.20 </td>
   <td style="text-align:left;"> $20494.93 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> OOS </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> DX CARDIAC CATHETERIZTN </td>
   <td style="text-align:left;"> 190 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 90271326 </td>
   <td style="text-align:left;"> 183914 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $12794.77 </td>
   <td style="text-align:left;"> $7369.54 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 190 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 266141 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $10292.68 </td>
   <td style="text-align:left;"> $4001.00 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Short-term Hospital </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 130 </td>
   <td style="text-align:left;"> Pleurisy; pneumothorax; pulmonary collapse </td>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> INCISION OF PLEURA </td>
   <td style="text-align:left;"> 143 </td>
   <td style="text-align:left;"> Other respiratory diagnoses except signs, symptoms &amp; minor diagnoses </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 271326 </td>
   <td style="text-align:left;"> 273279 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $14177.09 </td>
   <td style="text-align:left;"> $7711.28 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 660 </td>
   <td style="text-align:left;"> Alcohol-related disorders </td>
   <td style="text-align:left;"> 231 </td>
   <td style="text-align:left;"> OTHER THERAPEUTIC PRCS </td>
   <td style="text-align:left;"> 280 </td>
   <td style="text-align:left;"> Alcoholic liver disease </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Hepatobiliary System and Pancreas </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 40003993 </td>
   <td style="text-align:left;"> 239912 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $21448.73 </td>
   <td style="text-align:left;"> $9511.37 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 256322 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3400 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $2393.10 </td>
   <td style="text-align:left;"> $1327.02 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 182357 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2900 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $4191.00 </td>
   <td style="text-align:left;"> $2136.17 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> Skilled Nursing Home </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 204 </td>
   <td style="text-align:left;"> Other non-traumatic joint disorders </td>
   <td style="text-align:left;"> 152 </td>
   <td style="text-align:left;"> ARTHROPLASTY KNEE </td>
   <td style="text-align:left;"> 302 </td>
   <td style="text-align:left;"> Knee joint replacement </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Musculoskeletal System and Conn Tissue </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 257934 </td>
   <td style="text-align:left;"> 257934 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $29514.01 </td>
   <td style="text-align:left;"> $59253.25 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 231077 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2700 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $813.50 </td>
   <td style="text-align:left;"> $459.49 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Septicemia (except in labor) </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 720 </td>
   <td style="text-align:left;"> Septicemia &amp; disseminated infections </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 271326 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $10114.85 </td>
   <td style="text-align:left;"> $4943.62 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 115 </td>
   <td style="text-align:left;"> CIRCUMCISION </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 231077 </td>
   <td style="text-align:left;"> 231077 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2600 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $1510.00 </td>
   <td style="text-align:left;"> $929.25 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 129 </td>
   <td style="text-align:left;"> Aspiration pneumonitis; food/vomitus </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 137 </td>
   <td style="text-align:left;"> Major respiratory infections &amp; inflammations </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 266141 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $7330.35 </td>
   <td style="text-align:left;"> $2502.54 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 237701 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2900 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $1528.50 </td>
   <td style="text-align:left;"> $907.69 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 115 </td>
   <td style="text-align:left;"> CIRCUMCISION </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 237701 </td>
   <td style="text-align:left;"> 60001091 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3200 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $851.50 </td>
   <td style="text-align:left;"> $496.77 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 187 </td>
   <td style="text-align:left;"> Malposition; malpresentation </td>
   <td style="text-align:left;"> 134 </td>
   <td style="text-align:left;"> CESAREAN SECTION </td>
   <td style="text-align:left;"> 540 </td>
   <td style="text-align:left;"> Cesarean delivery </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 147483 </td>
   <td style="text-align:left;"> 147483 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $8550.41 </td>
   <td style="text-align:left;"> $4645.93 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Inpatient Rehabilitation Facility </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> Other endocrine disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 424 </td>
   <td style="text-align:left;"> Other endocrine disorders </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Endocrine, Nutritional and Metabolic Diseases and Disorders </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 40003993 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $17076.26 </td>
   <td style="text-align:left;"> $9286.99 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Skilled Nursing Home </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Septicemia (except in labor) </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> OT VASC CATH; NOT HEART </td>
   <td style="text-align:left;"> 720 </td>
   <td style="text-align:left;"> Septicemia &amp; disseminated infections </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 197563 </td>
   <td style="text-align:left;"> 258490 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $58394.67 </td>
   <td style="text-align:left;"> $40505.88 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> OOS </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease and bronchiectasis </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Federal/State/Local/VA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 271326 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $17623.18 </td>
   <td style="text-align:left;"> $8901.52 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 103 </td>
   <td style="text-align:left;"> Pulmonary heart disease </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 134 </td>
   <td style="text-align:left;"> Pulmonary embolism </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 271326 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $8016.13 </td>
   <td style="text-align:left;"> $2608.73 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> Mood disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 753 </td>
   <td style="text-align:left;"> Bipolar disorders </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 40003856 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $7380.68 </td>
   <td style="text-align:left;"> $9880.01 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 115 </td>
   <td style="text-align:left;"> CIRCUMCISION </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 182357 </td>
   <td style="text-align:left;"> 182357 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3200 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $1964.00 </td>
   <td style="text-align:left;"> $1261.62 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Cancer of head and neck </td>
   <td style="text-align:left;"> 71 </td>
   <td style="text-align:left;"> GASTROSTOMY; TEMP/PERM </td>
   <td style="text-align:left;"> 110 </td>
   <td style="text-align:left;"> Ear, nose, mouth, throat, cranial/facial malignancies </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Ear, Nose, Mouth, Throat and Craniofacial Diseases and Disorders </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> Federal/State/Local/VA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 256322 </td>
   <td style="text-align:left;"> 251671 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $7568.00 </td>
   <td style="text-align:left;"> $5248.66 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Skilled Nursing Home </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> Diabetes mellitus with complications </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 468 </td>
   <td style="text-align:left;"> Other kidney &amp; urinary tract diagnoses, signs &amp; symptoms </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 276511 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $10902.98 </td>
   <td style="text-align:left;"> $5216.32 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease and bronchiectasis </td>
   <td style="text-align:left;"> 216 </td>
   <td style="text-align:left;"> RESP INTUB/MECH VENTIL </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 165169 </td>
   <td style="text-align:left;"> 273279 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $14544.13 </td>
   <td style="text-align:left;"> $6380.94 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 182357 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2500 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $1216.42 </td>
   <td style="text-align:left;"> $811.52 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> Mood disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 753 </td>
   <td style="text-align:left;"> Bipolar disorders </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Federal/State/Local/VA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 267162 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $2415.05 </td>
   <td style="text-align:left;"> $1876.09 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 196 </td>
   <td style="text-align:left;"> Other pregnancy and delivery including normal </td>
   <td style="text-align:left;"> 137 </td>
   <td style="text-align:left;"> OT PRCS TO ASSIST DELIV </td>
   <td style="text-align:left;"> 541 </td>
   <td style="text-align:left;"> Vaginal delivery w sterilization &amp;/or D&amp;C </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Federal/State/Local/VA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 197563 </td>
   <td style="text-align:left;"> 197563 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $4256.44 </td>
   <td style="text-align:left;"> $3224.37 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> Mood disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 751 </td>
   <td style="text-align:left;"> Major depressive disorders &amp; other/unspecified psychoses </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 267162 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $5176.78 </td>
   <td style="text-align:left;"> $6235.63 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Skilled Nursing Home </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> Secondary malignancies </td>
   <td style="text-align:left;"> 231 </td>
   <td style="text-align:left;"> OTHER THERAPEUTIC PRCS </td>
   <td style="text-align:left;"> 952 </td>
   <td style="text-align:left;"> Nonextensive procedure unrelated to principal diagnosis </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 271326 </td>
   <td style="text-align:left;"> 239912 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $22783.03 </td>
   <td style="text-align:left;"> $12623.80 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease and bronchiectasis </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 271326 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $15927.38 </td>
   <td style="text-align:left;"> $5406.24 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 62 </td>
   <td style="text-align:left;"> Coagulation and hemorrhagic disorders </td>
   <td style="text-align:left;"> 76 </td>
   <td style="text-align:left;"> COLONOSCOPY AND BIOPSY </td>
   <td style="text-align:left;"> 661 </td>
   <td style="text-align:left;"> Coagulation &amp; platelet disorders </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Diseases and Disorders of Blood, Blood Forming Organs and Immunological Disorders </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 40003993 </td>
   <td style="text-align:left;"> 137074 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $12730.75 </td>
   <td style="text-align:left;"> $4252.01 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Inpatient Rehabilitation Facility </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 135 </td>
   <td style="text-align:left;"> Intestinal infection </td>
   <td style="text-align:left;"> 222 </td>
   <td style="text-align:left;"> BLOOD TRANSFUSION </td>
   <td style="text-align:left;"> 248 </td>
   <td style="text-align:left;"> Major gastrointestinal &amp; peritoneal infections </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 266141 </td>
   <td style="text-align:left;"> 251437 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $17490.31 </td>
   <td style="text-align:left;"> $9772.47 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Skilled Nursing Home </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 157 </td>
   <td style="text-align:left;"> Acute and unspecified renal failure </td>
   <td style="text-align:left;"> 222 </td>
   <td style="text-align:left;"> BLOOD TRANSFUSION </td>
   <td style="text-align:left;"> 460 </td>
   <td style="text-align:left;"> RENAL FAILURE </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 266141 </td>
   <td style="text-align:left;"> 276511 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $19586.28 </td>
   <td style="text-align:left;"> $8822.05 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Left Against Medical Advice </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 196 </td>
   <td style="text-align:left;"> Other pregnancy and delivery including normal </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 561 </td>
   <td style="text-align:left;"> Postpartum &amp; post abortion diagnoses w/o procedure </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 256322 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $2203.28 </td>
   <td style="text-align:left;"> $894.46 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Skilled Nursing Home </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 159 </td>
   <td style="text-align:left;"> Urinary tract infections </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 463 </td>
   <td style="text-align:left;"> Kidney &amp; urinary tract infections </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 170599 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $10387.40 </td>
   <td style="text-align:left;"> $4716.31 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> Neoplasms of unspecified nature or uncertain behavior </td>
   <td style="text-align:left;"> 124 </td>
   <td style="text-align:left;"> HYSTERECTOMY; AB/VAG </td>
   <td style="text-align:left;"> 513 </td>
   <td style="text-align:left;"> Uterine &amp; adnexa procedures for non-malignancy except leiomyoma </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Female Reproductive System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 155651 </td>
   <td style="text-align:left;"> 155651 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $15570.16 </td>
   <td style="text-align:left;"> $12042.57 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 159 </td>
   <td style="text-align:left;"> Urinary tract infections </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> OT VASC CATH; NOT HEART </td>
   <td style="text-align:left;"> 463 </td>
   <td style="text-align:left;"> Kidney &amp; urinary tract infections </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 161469 </td>
   <td style="text-align:left;"> 161469 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $17509.05 </td>
   <td style="text-align:left;"> $9587.57 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Inpatient Rehabilitation Facility </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Bacterial infection; unspecified site </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> OT VASC CATH; NOT HEART </td>
   <td style="text-align:left;"> 724 </td>
   <td style="text-align:left;"> Other infectious &amp; parasitic diseases </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 197563 </td>
   <td style="text-align:left;"> 197563 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $12811.88 </td>
   <td style="text-align:left;"> $6991.10 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 197 </td>
   <td style="text-align:left;"> Skin and subcutaneous tissue infections </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 383 </td>
   <td style="text-align:left;"> Cellulitis &amp; other skin infections </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Skin, Subcutaneous Tissue and Breast </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 161469 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $10372.37 </td>
   <td style="text-align:left;"> $5325.48 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> Mood disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 751 </td>
   <td style="text-align:left;"> Major depressive disorders &amp; other/unspecified psychoses </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 275408 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $3688.28 </td>
   <td style="text-align:left;"> $2800.36 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Skilled Nursing Home </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 153 </td>
   <td style="text-align:left;"> Gastrointestinal hemorrhage </td>
   <td style="text-align:left;"> 92 </td>
   <td style="text-align:left;"> OTHER BOWEL DX PRCS </td>
   <td style="text-align:left;"> 253 </td>
   <td style="text-align:left;"> Other &amp; unspecified gastrointestinal hemorrhage </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 141658 </td>
   <td style="text-align:left;"> 251671 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $11755.70 </td>
   <td style="text-align:left;"> $4987.86 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Hosp Basd Medicare Approved Swing Bed </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 197 </td>
   <td style="text-align:left;"> Skin and subcutaneous tissue infections </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> OT VASC CATH; NOT HEART </td>
   <td style="text-align:left;"> 383 </td>
   <td style="text-align:left;"> Cellulitis &amp; other skin infections </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Skin, Subcutaneous Tissue and Breast </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 266141 </td>
   <td style="text-align:left;"> 252850 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $14615.33 </td>
   <td style="text-align:left;"> $8883.59 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 153 </td>
   <td style="text-align:left;"> Gastrointestinal hemorrhage </td>
   <td style="text-align:left;"> 92 </td>
   <td style="text-align:left;"> OTHER BOWEL DX PRCS </td>
   <td style="text-align:left;"> 253 </td>
   <td style="text-align:left;"> Other &amp; unspecified gastrointestinal hemorrhage </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 256322 </td>
   <td style="text-align:left;"> 137074 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $7648.80 </td>
   <td style="text-align:left;"> $2457.26 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 184 </td>
   <td style="text-align:left;"> Early or threatened labor </td>
   <td style="text-align:left;"> 137 </td>
   <td style="text-align:left;"> OT PRCS TO ASSIST DELIV </td>
   <td style="text-align:left;"> 560 </td>
   <td style="text-align:left;"> Vaginal delivery </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 60420192 </td>
   <td style="text-align:left;"> 60420192 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $5429.70 </td>
   <td style="text-align:left;"> $3396.63 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home or Self Care </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 146 </td>
   <td style="text-align:left;"> Diverticulosis and diverticulitis </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 244 </td>
   <td style="text-align:left;"> Diverticulitis &amp; diverticulosis </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> 197453 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $4622.00 </td>
   <td style="text-align:left;"> $1691.56 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> OOS </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> Short-term Hospital </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> DX CARDIAC CATHETERIZTN </td>
   <td style="text-align:left;"> 190 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 271326 </td>
   <td style="text-align:left;"> 183914 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> $22161.20 </td>
   <td style="text-align:left;"> $12458.00 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Home w/ Home Health Services </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 226 </td>
   <td style="text-align:left;"> Fracture of neck of femur (hip) </td>
   <td style="text-align:left;"> 153 </td>
   <td style="text-align:left;"> HIP REPLACEMENT,TOT/PRT </td>
   <td style="text-align:left;"> 301 </td>
   <td style="text-align:left;"> Hip joint replacement </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Musculoskeletal System and Conn Tissue </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 172880 </td>
   <td style="text-align:left;"> 172880 </td>
   <td style="text-align:left;"> 90012432 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $30155.82 </td>
   <td style="text-align:left;"> $55346.46 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> 0401001 </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> Skilled Nursing Home </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> 149 </td>
   <td style="text-align:left;"> Biliary tract disease </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 284 </td>
   <td style="text-align:left;"> Disorders of gallbladder &amp; biliary tract </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Hepatobiliary System and Pancreas </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 165169 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:left;"> $13651.03 </td>
   <td style="text-align:left;"> $5599.37 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
  </tr>
</tbody>
</table></div>

## Data Cleaning

```{.r .fold-show}
# rename Zip.Code...3.digits
colnames(dat)[which(colnames(dat) == "Zip.Code...3.digits")] <- "Zip"

# make Length.of.Stay numeric
dat[which(dat$Length.of.Stay == "120 +"), "Length.of.Stay"] <- "121"
dat$Length.of.Stay <- as.numeric(dat$Length.of.Stay)

# Remove dollar sign from Total Costs and Total Charges and make numeric
dat$Total.Charges <- as.numeric(substring(dat$Total.Charges, 2))
dat$Total.Costs <- as.numeric(substring(dat$Total.Costs, 2))

# Create cost per day variable
dat$Cost.per.Day <- dat$Total.Costs/dat$Length.of.Stay

head(dat[, c("APR.MDC.Code", "APR.MDC.Description")])
```

```
##   APR.MDC.Code                                              APR.MDC.Description
## 1            4                 Diseases and Disorders of the Respiratory System
## 2            5                 Diseases and Disorders of the Circulatory System
## 3           18 Infectious and Parasitic Diseases, Systemic or Unspecified Sites
## 4            5                 Diseases and Disorders of the Circulatory System
## 5            5                 Diseases and Disorders of the Circulatory System
## 6            4                 Diseases and Disorders of the Respiratory System
```

```{.r .fold-show}
# Re-order factors
dat$APR.MDC.Code <- factor(dat$APR.MDC.Code, levels = as.character(0:25))

dat$APR.Severity.of.Illness.Description <- factor(dat$APR.Severity.of.Illness.Description, 
    levels = c("Minor", "Moderate", "Major", "Extreme"))

# Remove columns: discharge year is 2015 for all rows provider license numbers
# are many-leveled factors unused in analysis patient disposition information is
# known at end of hospital stay, not at admission
dat <- dat[, which(!(colnames(dat) %in% c("Discharge.Year", "Operating.Certificate.Number", 
    "Attending.Provider.License.Number", "Operating.Provider.License.Number", "Other.Provider.License.Number", 
    "Patient.Disposition")))]
```



```{.r .fold-show}
# Look at makeup of dataset
summary(dat)
```

```
##   Facility.Id          Health.Service.Area   Hospital.County   
##  1456   :  55896   New York City :1092224   Manhattan: 400991  
##  541    :  47712   Long Island   : 339277   Kings    : 248955  
##  1464   :  47518   Hudson Valley : 245945   Queens   : 197202  
##  1458   :  42997   Capital/Adiron: 167367   Bronx    : 188160  
##  1169   :  42720   Western NY    : 163927   Nassau   : 181164  
##  (Other):2107006   (Other)       : 335109   (Other)  :1127377  
##  NA's   :   2911   NA's          :   2911   NA's     :   2911  
##                                                         Facility.Name    
##  Mount Sinai Hospital                                          :  55896  
##  North Shore University Hospital                               :  47712  
##  New York Presbyterian Hospital - Columbia Presbyterian Center :  47518  
##  New York Presbyterian Hospital - New York Weill Cornell Center:  42997  
##  Montefiore Medical Center - Henry & Lucy Moses Div            :  42720  
##  Maimonides Medical Center                                     :  41909  
##  (Other)                                                       :2068008  
##        Age.Group           Zip          Gender     
##  0 to 17    :352588   112    : 313548   F:1307507  
##  18 to 29   :247223   104    : 224997   M:1039206  
##  30 to 49   :458457   117    : 182030   U:     47  
##  50 to 69   :645004   100    : 170127              
##  70 or Older:643488   113    : 121512              
##                       (Other):1330119              
##                       NA's   :   4427              
##                      Race                     Ethnicity       Length.of.Stay   
##  Black/African American: 444944   Multi-ethnic     :   8693   Min.   :  1.000  
##  Multi-racial          :  22380   Not Span/Hispanic:1954468   1st Qu.:  2.000  
##  Other Race            : 544559   Spanish/Hispanic : 278587   Median :  3.000  
##  White                 :1334877   Unknown          : 105012   Mean   :  5.476  
##                                                               3rd Qu.:  6.000  
##                                                               Max.   :121.000  
##                                                                                
##      Type.of.Admission   CCS.Diagnosis.Code
##  Elective     : 447280   218    : 226813   
##  Emergency    :1488031   2      : 108709   
##  Newborn      : 227100   203    :  63476   
##  Not Available:   1173   108    :  58760   
##  Trauma       :   6394   657    :  56950   
##  Urgent       : 176782   122    :  47082   
##                          (Other):1784970   
##                                                                   CCS.Diagnosis.Description
##  Liveborn                                                                      : 226813    
##  Septicemia (except in labor)                                                  : 108709    
##  Osteoarthritis                                                                :  63476    
##  Congestive heart failure; nonhypertensive                                     :  58760    
##  Mood disorders                                                                :  56950    
##  Pneumonia (except that caused by tuberculosis or sexually transmitted disease):  47082    
##  (Other)                                                                       :1784970    
##  CCS.Procedure.Code           CCS.Procedure.Description  APR.DRG.Code    
##  0      : 610252    NO PROC                : 610252     640    : 198114  
##  231    : 175510    OTHER THERAPEUTIC PRCS : 175510     560    : 147035  
##  137    :  83658    OT PRCS TO ASSIST DELIV:  83658     720    :  94966  
##  228    :  81060    PROPHYLACTIC VAC/INOCUL:  81060     540    :  76059  
##  134    :  74806    CESAREAN SECTION       :  74806     194    :  56375  
##  216    :  74069    RESP INTUB/MECH VENTIL :  74069     139    :  43040  
##  (Other):1247405    (Other)                :1247405     (Other):1731171  
##                                                         APR.DRG.Description 
##  Neonate birthwt >2499g, normal newborn or neonate w other problem: 198114  
##  Vaginal delivery                                                 : 147035  
##  Septicemia & disseminated infections                             :  94966  
##  Cesarean delivery                                                :  76059  
##  Heart failure                                                    :  56375  
##  Other pneumonia                                                  :  43040  
##  (Other)                                                          :1731171  
##   APR.MDC.Code   
##  5      :289531  
##  14     :254079  
##  15     :231854  
##  8      :204675  
##  6      :197654  
##  4      :196486  
##  (Other):972481  
##                                                                       APR.MDC.Description
##  Diseases and Disorders of the Circulatory System                               :289531  
##  Pregnancy, Childbirth and the Puerperium                                       :254079  
##  Newborns and Other Neonates with Conditions Originating in the Perinatal Period:231854  
##  Diseases and Disorders of the Musculoskeletal System and Conn Tissue           :204675  
##  Diseases and Disorders of the Digestive System                                 :197654  
##  Diseases and Disorders of the Respiratory System                               :196486  
##  (Other)                                                                        :972481  
##  APR.Severity.of.Illness.Code APR.Severity.of.Illness.Description
##  0:   112                     Minor   :785526                    
##  1:785526                     Moderate:897251                    
##  2:897251                     Major   :517129                    
##  3:517129                     Extreme :146742                    
##  4:146742                     NA's    :   112                    
##                                                                  
##                                                                  
##  APR.Risk.of.Mortality APR.Medical.Surgical.Description
##  Extreme : 121749      Medical       :1779501          
##  Major   : 335242      Not Applicable:    112          
##  Minor   :1389588      Surgical      : 567147          
##  Moderate: 500069                                      
##  NA's    :    112                                      
##                                                        
##                                                        
##                 Payment.Typology.1                Payment.Typology.2
##  Medicare                :876258   Medicaid                :554720  
##  Medicaid                :717285   Self-Pay                :366081  
##  Private Health Insurance:334558   Medicare                :336787  
##  Blue Cross/Blue Shield  :275552   Private Health Insurance:161646  
##  Self-Pay                : 67879   Blue Cross/Blue Shield  :124393  
##  Miscellaneous/Other     : 28985   (Other)                 : 40787  
##  (Other)                 : 46243   NA's                    :762346  
##                 Payment.Typology.3   Birth.Weight    Abortion.Edit.Indicator
##  Self-Pay                : 479620   Min.   :   0.0   N:2343849              
##  Medicaid                : 124629   1st Qu.:   0.0   Y:   2911              
##  Private Health Insurance:  34861   Median :   0.0                          
##  Blue Cross/Blue Shield  :  21473   Mean   : 326.4                          
##  Medicare                :  18971   3rd Qu.:   0.0                          
##  (Other)                 :  21636   Max.   :9900.0                          
##  NA's                    :1645570                                           
##  Emergency.Department.Indicator Total.Charges      Total.Costs     
##  N: 977695                      Min.   :      0   Min.   :      0  
##  Y:1369065                      1st Qu.:  12027   1st Qu.:   4724  
##                                 Median :  23481   Median :   8791  
##                                 Mean   :  43206   Mean   :  15985  
##                                 3rd Qu.:  46607   3rd Qu.:  16835  
##                                 Max.   :7248391   Max.   :5236615  
##                                                                    
##                                           Facility.Type    
##  Hospital                                        :2296627  
##  Primary Care Hospital - Critical Access Hospital:   9361  
##  NA's                                            :  40772  
##                                                            
##                                                            
##                                                            
##                                                            
##                     Ownership.Type     Cost.per.Day    
##  County                    :   3711   Min.   :      0  
##  Municipality              : 186339   1st Qu.:   1611  
##  Not for Profit Corporation:1979711   Median :   2465  
##  Public Benefit Corporation:  47937   Mean   :   3482  
##  State                     :  88290   3rd Qu.:   4098  
##  NA's                      :  40772   Max.   :1017571  
## 
```


```r
report_table(head(dat, 50))
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:500px; "><table class="table table-striped table-hover lightable-paper" style='margin-left: auto; margin-right: auto; font-family: "Arial Narrow", arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Facility.Id </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Health.Service.Area </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Hospital.County </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Facility.Name </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Age.Group </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Zip </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Gender </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Race </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Ethnicity </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Length.of.Stay </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Type.of.Admission </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> CCS.Diagnosis.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> CCS.Diagnosis.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> CCS.Procedure.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> CCS.Procedure.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.DRG.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.DRG.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.MDC.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.MDC.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.Severity.of.Illness.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.Severity.of.Illness.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.Risk.of.Mortality </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> APR.Medical.Surgical.Description </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Payment.Typology.1 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Payment.Typology.2 </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Payment.Typology.3 </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Birth.Weight </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Abortion.Edit.Indicator </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Emergency.Department.Indicator </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Total.Charges </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Total.Costs </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Facility.Type </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Ownership.Type </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Cost.per.Day </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease and bronchiectasis </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 18309.20 </td>
   <td style="text-align:right;"> 10309.98 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 2061.9960 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 108 </td>
   <td style="text-align:left;"> Congestive heart failure; nonhypertensive </td>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> HEMODIALYSIS </td>
   <td style="text-align:left;"> 194 </td>
   <td style="text-align:left;"> Heart failure </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 12310.76 </td>
   <td style="text-align:right;"> 6034.20 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 2011.4000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Septicemia (except in labor) </td>
   <td style="text-align:left;"> 222 </td>
   <td style="text-align:left;"> BLOOD TRANSFUSION </td>
   <td style="text-align:left;"> 720 </td>
   <td style="text-align:left;"> Septicemia &amp; disseminated infections </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 48242.20 </td>
   <td style="text-align:right;"> 20494.93 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 2277.2144 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> OOS </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> DX CARDIAC CATHETERIZTN </td>
   <td style="text-align:left;"> 190 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 12794.77 </td>
   <td style="text-align:right;"> 7369.54 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 3684.7700 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 190 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 10292.68 </td>
   <td style="text-align:right;"> 4001.00 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1000.2500 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 130 </td>
   <td style="text-align:left;"> Pleurisy; pneumothorax; pulmonary collapse </td>
   <td style="text-align:left;"> 39 </td>
   <td style="text-align:left;"> INCISION OF PLEURA </td>
   <td style="text-align:left;"> 143 </td>
   <td style="text-align:left;"> Other respiratory diagnoses except signs, symptoms &amp; minor diagnoses </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 14177.09 </td>
   <td style="text-align:right;"> 7711.28 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1101.6114 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 660 </td>
   <td style="text-align:left;"> Alcohol-related disorders </td>
   <td style="text-align:left;"> 231 </td>
   <td style="text-align:left;"> OTHER THERAPEUTIC PRCS </td>
   <td style="text-align:left;"> 280 </td>
   <td style="text-align:left;"> Alcoholic liver disease </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Hepatobiliary System and Pancreas </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 21448.73 </td>
   <td style="text-align:right;"> 9511.37 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1056.8189 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3400 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 2393.10 </td>
   <td style="text-align:right;"> 1327.02 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 442.3400 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2900 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 4191.00 </td>
   <td style="text-align:right;"> 2136.17 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 534.0425 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> 204 </td>
   <td style="text-align:left;"> Other non-traumatic joint disorders </td>
   <td style="text-align:left;"> 152 </td>
   <td style="text-align:left;"> ARTHROPLASTY KNEE </td>
   <td style="text-align:left;"> 302 </td>
   <td style="text-align:left;"> Knee joint replacement </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Musculoskeletal System and Conn Tissue </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 29514.01 </td>
   <td style="text-align:right;"> 59253.25 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 14813.3125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2700 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 813.50 </td>
   <td style="text-align:right;"> 459.49 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 459.4900 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Septicemia (except in labor) </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 720 </td>
   <td style="text-align:left;"> Septicemia &amp; disseminated infections </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 10114.85 </td>
   <td style="text-align:right;"> 4943.62 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1235.9050 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 115 </td>
   <td style="text-align:left;"> CIRCUMCISION </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2600 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 1510.00 </td>
   <td style="text-align:right;"> 929.25 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 464.6250 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 129 </td>
   <td style="text-align:left;"> Aspiration pneumonitis; food/vomitus </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 137 </td>
   <td style="text-align:left;"> Major respiratory infections &amp; inflammations </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 7330.35 </td>
   <td style="text-align:right;"> 2502.54 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1251.2700 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2900 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 1528.50 </td>
   <td style="text-align:right;"> 907.69 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 453.8450 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 115 </td>
   <td style="text-align:left;"> CIRCUMCISION </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3200 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 851.50 </td>
   <td style="text-align:right;"> 496.77 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 496.7700 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> 187 </td>
   <td style="text-align:left;"> Malposition; malpresentation </td>
   <td style="text-align:left;"> 134 </td>
   <td style="text-align:left;"> CESAREAN SECTION </td>
   <td style="text-align:left;"> 540 </td>
   <td style="text-align:left;"> Cesarean delivery </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 8550.41 </td>
   <td style="text-align:right;"> 4645.93 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1548.6433 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> Other endocrine disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 424 </td>
   <td style="text-align:left;"> Other endocrine disorders </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Endocrine, Nutritional and Metabolic Diseases and Disorders </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 17076.26 </td>
   <td style="text-align:right;"> 9286.99 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 928.6990 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Septicemia (except in labor) </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> OT VASC CATH; NOT HEART </td>
   <td style="text-align:left;"> 720 </td>
   <td style="text-align:left;"> Septicemia &amp; disseminated infections </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 58394.67 </td>
   <td style="text-align:right;"> 40505.88 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 3115.8369 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> OOS </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease and bronchiectasis </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Federal/State/Local/VA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 17623.18 </td>
   <td style="text-align:right;"> 8901.52 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1271.6457 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 103 </td>
   <td style="text-align:left;"> Pulmonary heart disease </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 134 </td>
   <td style="text-align:left;"> Pulmonary embolism </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 8016.13 </td>
   <td style="text-align:right;"> 2608.73 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1304.3650 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> Mood disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 753 </td>
   <td style="text-align:left;"> Bipolar disorders </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 7380.68 </td>
   <td style="text-align:right;"> 9880.01 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1411.4300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 115 </td>
   <td style="text-align:left;"> CIRCUMCISION </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3200 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 1964.00 </td>
   <td style="text-align:right;"> 1261.62 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 420.5400 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Cancer of head and neck </td>
   <td style="text-align:left;"> 71 </td>
   <td style="text-align:left;"> GASTROSTOMY; TEMP/PERM </td>
   <td style="text-align:left;"> 110 </td>
   <td style="text-align:left;"> Ear, nose, mouth, throat, cranial/facial malignancies </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Ear, Nose, Mouth, Throat and Craniofacial Diseases and Disorders </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> Federal/State/Local/VA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 7568.00 </td>
   <td style="text-align:right;"> 5248.66 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1312.1650 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> Diabetes mellitus with complications </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 468 </td>
   <td style="text-align:left;"> Other kidney &amp; urinary tract diagnoses, signs &amp; symptoms </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 10902.98 </td>
   <td style="text-align:right;"> 5216.32 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 869.3867 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease and bronchiectasis </td>
   <td style="text-align:left;"> 216 </td>
   <td style="text-align:left;"> RESP INTUB/MECH VENTIL </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 14544.13 </td>
   <td style="text-align:right;"> 6380.94 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1595.2350 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 0 to 17 </td>
   <td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Newborn </td>
   <td style="text-align:left;"> 218 </td>
   <td style="text-align:left;"> Liveborn </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 640 </td>
   <td style="text-align:left;"> Neonate birthwt &gt;2499g, normal newborn or neonate w other problem </td>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2500 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 1216.42 </td>
   <td style="text-align:right;"> 811.52 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 405.7600 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> Mood disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 753 </td>
   <td style="text-align:left;"> Bipolar disorders </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Federal/State/Local/VA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 2415.05 </td>
   <td style="text-align:right;"> 1876.09 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 938.0450 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> 196 </td>
   <td style="text-align:left;"> Other pregnancy and delivery including normal </td>
   <td style="text-align:left;"> 137 </td>
   <td style="text-align:left;"> OT PRCS TO ASSIST DELIV </td>
   <td style="text-align:left;"> 541 </td>
   <td style="text-align:left;"> Vaginal delivery w sterilization &amp;/or D&amp;C </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Federal/State/Local/VA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 4256.44 </td>
   <td style="text-align:right;"> 3224.37 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 3224.3700 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> Mood disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 751 </td>
   <td style="text-align:left;"> Major depressive disorders &amp; other/unspecified psychoses </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 5176.78 </td>
   <td style="text-align:right;"> 6235.63 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1247.1260 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> Secondary malignancies </td>
   <td style="text-align:left;"> 231 </td>
   <td style="text-align:left;"> OTHER THERAPEUTIC PRCS </td>
   <td style="text-align:left;"> 952 </td>
   <td style="text-align:left;"> Nonextensive procedure unrelated to principal diagnosis </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 22783.03 </td>
   <td style="text-align:right;"> 12623.80 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1402.6444 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 127 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease and bronchiectasis </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> Chronic obstructive pulmonary disease </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 15927.38 </td>
   <td style="text-align:right;"> 5406.24 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 2703.1200 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 62 </td>
   <td style="text-align:left;"> Coagulation and hemorrhagic disorders </td>
   <td style="text-align:left;"> 76 </td>
   <td style="text-align:left;"> COLONOSCOPY AND BIOPSY </td>
   <td style="text-align:left;"> 661 </td>
   <td style="text-align:left;"> Coagulation &amp; platelet disorders </td>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Diseases and Disorders of Blood, Blood Forming Organs and Immunological Disorders </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 12730.75 </td>
   <td style="text-align:right;"> 4252.01 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1417.3367 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 135 </td>
   <td style="text-align:left;"> Intestinal infection </td>
   <td style="text-align:left;"> 222 </td>
   <td style="text-align:left;"> BLOOD TRANSFUSION </td>
   <td style="text-align:left;"> 248 </td>
   <td style="text-align:left;"> Major gastrointestinal &amp; peritoneal infections </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 17490.31 </td>
   <td style="text-align:right;"> 9772.47 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1628.7450 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 157 </td>
   <td style="text-align:left;"> Acute and unspecified renal failure </td>
   <td style="text-align:left;"> 222 </td>
   <td style="text-align:left;"> BLOOD TRANSFUSION </td>
   <td style="text-align:left;"> 460 </td>
   <td style="text-align:left;"> RENAL FAILURE </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 19586.28 </td>
   <td style="text-align:right;"> 8822.05 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 882.2050 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 196 </td>
   <td style="text-align:left;"> Other pregnancy and delivery including normal </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 561 </td>
   <td style="text-align:left;"> Postpartum &amp; post abortion diagnoses w/o procedure </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 2203.28 </td>
   <td style="text-align:right;"> 894.46 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 894.4600 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 140 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 159 </td>
   <td style="text-align:left;"> Urinary tract infections </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 463 </td>
   <td style="text-align:left;"> Kidney &amp; urinary tract infections </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 10387.40 </td>
   <td style="text-align:right;"> 4716.31 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 786.0517 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> Neoplasms of unspecified nature or uncertain behavior </td>
   <td style="text-align:left;"> 124 </td>
   <td style="text-align:left;"> HYSTERECTOMY; AB/VAG </td>
   <td style="text-align:left;"> 513 </td>
   <td style="text-align:left;"> Uterine &amp; adnexa procedures for non-malignancy except leiomyoma </td>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Female Reproductive System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 15570.16 </td>
   <td style="text-align:right;"> 12042.57 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 6021.2850 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 159 </td>
   <td style="text-align:left;"> Urinary tract infections </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> OT VASC CATH; NOT HEART </td>
   <td style="text-align:left;"> 463 </td>
   <td style="text-align:left;"> Kidney &amp; urinary tract infections </td>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 17509.05 </td>
   <td style="text-align:right;"> 9587.57 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1597.9283 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 30 to 49 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Bacterial infection; unspecified site </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> OT VASC CATH; NOT HEART </td>
   <td style="text-align:left;"> 724 </td>
   <td style="text-align:left;"> Other infectious &amp; parasitic diseases </td>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 12811.88 </td>
   <td style="text-align:right;"> 6991.10 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 3495.5500 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> 197 </td>
   <td style="text-align:left;"> Skin and subcutaneous tissue infections </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 383 </td>
   <td style="text-align:left;"> Cellulitis &amp; other skin infections </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Skin, Subcutaneous Tissue and Breast </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 10372.37 </td>
   <td style="text-align:right;"> 5325.48 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 887.5800 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 141 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 657 </td>
   <td style="text-align:left;"> Mood disorders </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 751 </td>
   <td style="text-align:left;"> Major depressive disorders &amp; other/unspecified psychoses </td>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 3688.28 </td>
   <td style="text-align:right;"> 2800.36 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 933.4533 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 153 </td>
   <td style="text-align:left;"> Gastrointestinal hemorrhage </td>
   <td style="text-align:left;"> 92 </td>
   <td style="text-align:left;"> OTHER BOWEL DX PRCS </td>
   <td style="text-align:left;"> 253 </td>
   <td style="text-align:left;"> Other &amp; unspecified gastrointestinal hemorrhage </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Extreme </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 11755.70 </td>
   <td style="text-align:right;"> 4987.86 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1246.9650 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 148 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 197 </td>
   <td style="text-align:left;"> Skin and subcutaneous tissue infections </td>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> OT VASC CATH; NOT HEART </td>
   <td style="text-align:left;"> 383 </td>
   <td style="text-align:left;"> Cellulitis &amp; other skin infections </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Skin, Subcutaneous Tissue and Breast </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 14615.33 </td>
   <td style="text-align:right;"> 8883.59 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1480.5983 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 153 </td>
   <td style="text-align:left;"> Gastrointestinal hemorrhage </td>
   <td style="text-align:left;"> 92 </td>
   <td style="text-align:left;"> OTHER BOWEL DX PRCS </td>
   <td style="text-align:left;"> 253 </td>
   <td style="text-align:left;"> Other &amp; unspecified gastrointestinal hemorrhage </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 7648.80 </td>
   <td style="text-align:right;"> 2457.26 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1228.6300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 18 to 29 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> F </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> 184 </td>
   <td style="text-align:left;"> Early or threatened labor </td>
   <td style="text-align:left;"> 137 </td>
   <td style="text-align:left;"> OT PRCS TO ASSIST DELIV </td>
   <td style="text-align:left;"> 560 </td>
   <td style="text-align:left;"> Vaginal delivery </td>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 5429.70 </td>
   <td style="text-align:right;"> 3396.63 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1132.2100 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 146 </td>
   <td style="text-align:left;"> Diverticulosis and diverticulitis </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 244 </td>
   <td style="text-align:left;"> Diverticulitis &amp; diverticulosis </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Moderate </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 4622.00 </td>
   <td style="text-align:right;"> 1691.56 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 845.7800 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> OOS </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Elective </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> DX CARDIAC CATHETERIZTN </td>
   <td style="text-align:left;"> 190 </td>
   <td style="text-align:left;"> Acute myocardial infarction </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:right;"> 22161.20 </td>
   <td style="text-align:right;"> 12458.00 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 6229.0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 50 to 69 </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 226 </td>
   <td style="text-align:left;"> Fracture of neck of femur (hip) </td>
   <td style="text-align:left;"> 153 </td>
   <td style="text-align:left;"> HIP REPLACEMENT,TOT/PRT </td>
   <td style="text-align:left;"> 301 </td>
   <td style="text-align:left;"> Hip joint replacement </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Musculoskeletal System and Conn Tissue </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Minor </td>
   <td style="text-align:left;"> Surgical </td>
   <td style="text-align:left;"> Blue Cross/Blue Shield </td>
   <td style="text-align:left;"> Private Health Insurance </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 30155.82 </td>
   <td style="text-align:right;"> 55346.46 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 18448.8200 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> Western NY </td>
   <td style="text-align:left;"> Cattaraugus </td>
   <td style="text-align:left;"> Olean General Hospital </td>
   <td style="text-align:left;"> 70 or Older </td>
   <td style="text-align:left;"> 147 </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> White </td>
   <td style="text-align:left;"> Not Span/Hispanic </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> Emergency </td>
   <td style="text-align:left;"> 149 </td>
   <td style="text-align:left;"> Biliary tract disease </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NO PROC </td>
   <td style="text-align:left;"> 284 </td>
   <td style="text-align:left;"> Disorders of gallbladder &amp; biliary tract </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Hepatobiliary System and Pancreas </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Major </td>
   <td style="text-align:left;"> Medical </td>
   <td style="text-align:left;"> Medicare </td>
   <td style="text-align:left;"> Medicaid </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Y </td>
   <td style="text-align:right;"> 13651.03 </td>
   <td style="text-align:right;"> 5599.37 </td>
   <td style="text-align:left;"> Hospital </td>
   <td style="text-align:left;"> Not for Profit Corporation </td>
   <td style="text-align:right;"> 1399.8425 </td>
  </tr>
</tbody>
</table></div>


# Preliminary Data Exploration

## MDC Codes

The following codes are Major Diagnostic Categories (MDC) that generally correspond to a major organ system. The original 1-23 MDC codes are grouped by primary diagnosis. In 1986, they expanded the MDC codes to include MDC Codes 0 (Pre-MDC), 24 (Multiple Significant Trauma), and 25 (Human Immunodeficiency Virus or HIV), the latter two of which are based on both primary and secondary diagnosis unlike codes 1-23. Patient diagnosis of MDC code 24 is based having on two or more significant traumas to different systems of the body. To receive an MDC code of 25, a patient must either have a primary diagnosis of HIV with a secondary diagnosis of an HIV infection complication or the primary diagnosis could be the HIV complication with the secondary diagnosis being  an HIV infection. Patients are assigned as Pre-MDC when the first step in the assigning the Diagnosis-Related Group (DRG) is based on procedure instead of primary diagnosis. Patients in the Pre-MDC group are mostly transplant patients (heart, liver, intestinal, bone marrow, etc.) and patients receiving tracheostomies. These are grouped separately as these are very resource intensive procedures and the procedures may be done for diagnoses across many different MDCs [@centersformedicaremedicaidservicesICD10CMPCSMSDRG2020].


```r
mdc_codes <- data.frame(`MDC Code` = as.character(0:25), Description = c("Pre-MDC or Ungroupable", 
    "Diseases and Disorders of the Nervous System", "Diseases and Disorders of the Eye", 
    "Ear, Nose, Mouth, Throat, and Craniofacial Diseases and Disorders", "Diseases and Disorders of the Respiratory System", 
    "Diseases and Disorders of the Circulatory System", "Diseases and Disorders of the Digestive System", 
    "Diseases and Disorders of the Hepatobiliary System and Pancreas", "Diseases and Disorders of the Musculoskeletal System and Connective Tissue", 
    "Diseases and Disorders of the Skin, Subcutaneous Tissue and Breast", "Endocrine, Nutritional and Metabolic Diseases and Disorders", 
    "Diseases and Disorders of the Kidney and Urinary Tract", "Diseases and Disorders of the Male Reproductive System", 
    "Diseases and Disorders of the Female Reproductive System", "Pregnancy, Childbirth and the Puerperium", 
    "Newborns and Other Neonates with Conditions Originating in the Perinatal Period", 
    "Diseases and Disorders of Blood, Blood Forming Organs and Immunological Disorders", 
    "Lymphatic, Hematopoietic, Other Malignancies, Chemotherapy and Radiotherapy", 
    "Infectious and Parasitic Diseases, Systemic or Unspecified Sites", "Mental Diseases and Disorders", 
    "Alcohol/Drug Use and Alcohol/Drug Induced Organic Mental Disorders", "Poisonings, Toxic Effects, Other Injuries and Other Complications of Treatment", 
    "Burns", "Rehabilitation, Aftercare, Other Factors Influencing Health Status and Other Health Service Contacts", 
    "Human Immunodeficiency Virus (HIV) Infections", "Multiple Significant Trauma"))

report_table(mdc_codes, w = "650px", ec = "position: center; display: block; margin: auto;")
```

<div style="border: 1px solid #ddd; padding: 0px; position: center; display: block; margin: auto;overflow-y: scroll; height:500px; overflow-x: scroll; width:650px; "><table class="table table-striped table-hover lightable-paper" style='margin-left: auto; margin-right: auto; font-family: "Arial Narrow", arial, helvetica, sans-serif; margin-left: auto; margin-right: auto;'>
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> MDC.Code </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;"> Description </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Pre-MDC or Ungroupable </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Nervous System </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Eye </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Ear, Nose, Mouth, Throat, and Craniofacial Diseases and Disorders </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Respiratory System </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Circulatory System </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Digestive System </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Hepatobiliary System and Pancreas </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Musculoskeletal System and Connective Tissue </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Skin, Subcutaneous Tissue and Breast </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> Endocrine, Nutritional and Metabolic Diseases and Disorders </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Kidney and Urinary Tract </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 12 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Male Reproductive System </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> Diseases and Disorders of the Female Reproductive System </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 14 </td>
   <td style="text-align:left;"> Pregnancy, Childbirth and the Puerperium </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> Newborns and Other Neonates with Conditions Originating in the Perinatal Period </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 16 </td>
   <td style="text-align:left;"> Diseases and Disorders of Blood, Blood Forming Organs and Immunological Disorders </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> Lymphatic, Hematopoietic, Other Malignancies, Chemotherapy and Radiotherapy </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> Infectious and Parasitic Diseases, Systemic or Unspecified Sites </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> Mental Diseases and Disorders </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Alcohol/Drug Use and Alcohol/Drug Induced Organic Mental Disorders </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 21 </td>
   <td style="text-align:left;"> Poisonings, Toxic Effects, Other Injuries and Other Complications of Treatment </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 22 </td>
   <td style="text-align:left;"> Burns </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> Rehabilitation, Aftercare, Other Factors Influencing Health Status and Other Health Service Contacts </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> Human Immunodeficiency Virus (HIV) Infections </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> Multiple Significant Trauma </td>
  </tr>
</tbody>
</table></div>


## Figures

Length of stay goes from 1 to 121 (previously 120+) but the majority of the data lies between 1 and 30. With the most patients having stayed 2 days. 


```r
ggplot(dat, aes(x = Length.of.Stay)) + geom_histogram(binwidth = 1, fill = "#69b3a2", 
    color = "#e9ecef", alpha = 0.9) + ggtitle("Distribution of Length of Stay") + 
    xlab("Length of Stay") + ylab("Frequency") + scale_x_continuous(breaks = c(1, 
    10, 20, 30, 40), limits = c(0, 40)) + scale_y_continuous(labels = scales::comma)
```

<img src="Report_files/figure-html/los-dist-1.png" style="display: block; margin: auto;" />

Median length of stay increases as age group increases. The interquartile range of length of stay also increases as age group increases.


```r
ggplot(dat, aes(x = Age.Group, y = Length.of.Stay, fill = Age.Group)) + geom_boxplot() + 
    theme(legend.position = "none") + ggtitle("Length of Stay by Age Group") + xlab("Age Group") + 
    ylab("Length of Stay") + scale_y_continuous(breaks = c(1, 10, 20), limits = c(0, 
    20))
```

<img src="Report_files/figure-html/los-by-age-1.png" style="display: block; margin: auto;" />

The longest average length of stay among MDC groups was the 0 MDC group which corresponds to Pre-MDC or Ungroupable. As discussed earlier, the Pre-MDC group is primarily composed of transplant patients which explains the longer average time spent as an inpatient.


```r
# ggplot(dat, aes(x = APR.MDC.Code, y = Length.of.Stay, tooltip =
# APR.MDC.Description)) + stat_summary(fun = 'mean', geom = 'bar', fill =
# '#69b3a2') + ggtitle('Average Length of Stay by APR MDC Code') + xlab('APR MDC
# Code') + ylab('Average Length of Stay') los_avg_mdc <- ggplot(dat, aes(x =
# APR.MDC.Code, y = Length.of.Stay, tooltip = APR.MDC.Description)) +
# geom_bar_interactive(stat = 'summary', fun = 'mean', fill = '#69b3a2') +
# ggtitle('Average Length of Stay by APR MDC Code') + xlab('APR MDC Code') +
# ylab('Average Length of Stay')
girafe_options(girafe(code = print(ggplot(dat, aes(x = APR.MDC.Code, y = Length.of.Stay, 
    tooltip = APR.MDC.Description)) + geom_bar_interactive(stat = "summary", fun = "mean", 
    fill = "#69b3a2") + ggtitle("Average Length of Stay by APR MDC Code") + xlab("APR MDC Code") + 
    ylab("Average Length of Stay"))), opts_sizing(rescale = TRUE), opts_tooltip(opacity = 0.8, 
    css = "background-color:gray;color:white;padding:2px;border-radius:2px;"), opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;stroke-width:1;"))
```

<!--html_preserve--><div id="htmlwidget-e04a64c9ba8176adb16f" style="width:672px;height:432px;" class="girafe html-widget"></div>
<script type="application/json" data-for="htmlwidget-e04a64c9ba8176adb16f">{"x":{"html":"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc' viewBox='0 0 432.00 360.00'>\n  <g>\n    <defs>\n      <clipPath id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_1'>\n        <rect x='0.00' y='0.00' width='432.00' height='360.00'/>\n      <\/clipPath>\n    <\/defs>\n    <rect x='0.00' y='0.00' width='432.00' height='360.00' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_1' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_1)' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.75' stroke-linejoin='round' stroke-linecap='round'/>\n    <defs>\n      <clipPath id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_2'>\n        <rect x='0.00' y='0.00' width='432.00' height='360.00'/>\n      <\/clipPath>\n    <\/defs>\n    <rect x='0.00' y='0.00' width='432.00' height='360.00' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_2' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_2)' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='round'/>\n    <defs>\n      <clipPath id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3'>\n        <rect x='33.26' y='23.35' width='393.26' height='304.92'/>\n      <\/clipPath>\n    <\/defs>\n    <rect x='33.26' y='23.35' width='393.26' height='304.92' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_3' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#EBEBEB' fill-opacity='1' stroke='none'/>\n    <polyline points='33.26,275.18 426.52,275.18' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_4' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='33.26,196.73 426.52,196.73' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_5' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='33.26,118.27 426.52,118.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_6' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='33.26,39.82 426.52,39.82' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_7' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='33.26,314.41 426.52,314.41' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_8' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='33.26,235.95 426.52,235.95' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_9' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='33.26,157.50 426.52,157.50' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_10' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='33.26,79.05 426.52,79.05' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_11' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='42.27,328.27 42.27,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_12' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='57.28,328.27 57.28,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_13' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='72.29,328.27 72.29,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_14' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='87.30,328.27 87.30,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_15' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='102.31,328.27 102.31,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_16' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='117.32,328.27 117.32,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_17' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='132.33,328.27 132.33,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_18' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='147.34,328.27 147.34,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_19' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='162.35,328.27 162.35,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_20' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='177.36,328.27 177.36,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_21' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='192.37,328.27 192.37,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_22' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='207.38,328.27 207.38,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_23' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='222.39,328.27 222.39,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_24' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='237.40,328.27 237.40,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_25' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='252.41,328.27 252.41,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_26' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='267.42,328.27 267.42,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_27' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='282.43,328.27 282.43,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_28' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='297.44,328.27 297.44,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_29' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='312.45,328.27 312.45,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_30' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='327.46,328.27 327.46,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_31' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='342.47,328.27 342.47,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_32' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='357.48,328.27 357.48,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_33' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='372.49,328.27 372.49,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_34' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='387.49,328.27 387.49,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_35' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='402.50,328.27 402.50,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_36' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='417.51,328.27 417.51,23.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_37' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <rect x='35.51' y='37.21' width='13.51' height='277.20' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_38' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Pre-MDC or Ungroupable'/>\n    <rect x='50.52' y='222.97' width='13.51' height='91.44' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_39' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Nervous System'/>\n    <rect x='65.53' y='256.49' width='13.51' height='57.92' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_40' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Eye'/>\n    <rect x='80.54' y='255.87' width='13.51' height='58.54' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_41' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Ear, Nose, Mouth, Throat and Craniofacial Diseases and Disorders'/>\n    <rect x='95.55' y='225.05' width='13.51' height='89.35' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_42' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Respiratory System'/>\n    <rect x='110.56' y='235.39' width='13.51' height='79.01' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_43' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Circulatory System'/>\n    <rect x='125.57' y='234.64' width='13.51' height='79.76' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_44' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Digestive System'/>\n    <rect x='140.58' y='227.49' width='13.51' height='86.91' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_45' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Hepatobiliary System and Pancreas'/>\n    <rect x='155.59' y='240.53' width='13.51' height='73.88' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_46' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Musculoskeletal System and Conn Tissue'/>\n    <rect x='170.60' y='242.44' width='13.51' height='71.97' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_47' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Skin, Subcutaneous Tissue and Breast'/>\n    <rect x='185.61' y='252.87' width='13.51' height='61.54' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_48' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Endocrine, Nutritional and Metabolic Diseases and Disorders'/>\n    <rect x='200.62' y='228.91' width='13.51' height='85.49' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_49' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Kidney and Urinary Tract'/>\n    <rect x='215.63' y='258.76' width='13.51' height='55.65' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_50' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Male Reproductive System'/>\n    <rect x='230.64' y='263.38' width='13.51' height='51.02' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_51' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Female Reproductive System'/>\n    <rect x='245.65' y='268.97' width='13.51' height='45.44' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_52' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Pregnancy, Childbirth and the Puerperium'/>\n    <rect x='260.66' y='254.84' width='13.51' height='59.56' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_53' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Newborns and Other Neonates with Conditions Originating in the Perinatal Period'/>\n    <rect x='275.67' y='234.52' width='13.51' height='79.88' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_54' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of Blood, Blood Forming Organs and Immunological Disorders'/>\n    <rect x='290.68' y='159.69' width='13.51' height='154.72' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_55' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Lymphatic, Hematopoietic, Other Malignancies, Chemotherapy and Radiotherapy'/>\n    <rect x='305.69' y='177.08' width='13.51' height='137.33' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_56' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Infectious and Parasitic Diseases, Systemic or Unspecified Sites'/>\n    <rect x='320.70' y='119.65' width='13.51' height='194.76' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_57' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Mental Diseases and Disorders'/>\n    <rect x='335.71' y='215.33' width='13.51' height='99.08' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_58' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Alcohol/Drug Use and Alcohol/Drug Induced Organic Mental Disorders'/>\n    <rect x='350.72' y='245.66' width='13.51' height='68.74' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_59' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Poisonings, Toxic Effects, Other Injuries and Other Complications of Treatment'/>\n    <rect x='365.73' y='185.35' width='13.51' height='129.05' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_60' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Burns'/>\n    <rect x='380.74' y='142.48' width='13.51' height='171.93' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_61' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Rehabilitation, Aftercare, Other Factors Influencing Health Status and Other Health Service Contacts'/>\n    <rect x='395.75' y='177.55' width='13.51' height='136.86' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_62' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Human Immunodeficiency Virus Infections'/>\n    <rect x='410.76' y='146.90' width='13.51' height='167.50' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_63' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Multiple Significant Trauma'/>\n    <defs>\n      <clipPath id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4'>\n        <rect x='0.00' y='0.00' width='432.00' height='360.00'/>\n      <\/clipPath>\n    <\/defs>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='23.44' y='317.56' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_64' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>0<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='23.44' y='239.11' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_65' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>5<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='18.54' y='160.66' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_66' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>10<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='18.54' y='82.20' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_67' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>15<\/text>\n    <\/g>\n    <polyline points='30.52,314.41 33.26,314.41' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_68' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='30.52,235.95 33.26,235.95' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_69' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='30.52,157.50 33.26,157.50' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_70' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='30.52,79.05 33.26,79.05' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_71' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='42.27,331.01 42.27,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_72' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='57.28,331.01 57.28,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_73' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='72.29,331.01 72.29,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_74' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='87.30,331.01 87.30,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_75' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='102.31,331.01 102.31,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_76' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='117.32,331.01 117.32,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_77' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='132.33,331.01 132.33,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_78' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='147.34,331.01 147.34,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_79' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='162.35,331.01 162.35,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_80' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='177.36,331.01 177.36,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_81' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='192.37,331.01 192.37,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_82' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='207.38,331.01 207.38,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_83' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='222.39,331.01 222.39,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_84' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='237.40,331.01 237.40,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_85' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='252.41,331.01 252.41,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_86' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='267.42,331.01 267.42,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_87' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='282.43,331.01 282.43,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_88' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='297.44,331.01 297.44,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_89' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='312.45,331.01 312.45,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_90' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='327.46,331.01 327.46,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_91' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='342.47,331.01 342.47,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_92' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='357.48,331.01 357.48,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_93' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='372.49,331.01 372.49,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_94' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='387.49,331.01 387.49,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_95' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='402.50,331.01 402.50,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_96' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='417.51,331.01 417.51,328.27' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_97' clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='39.82' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_98' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>0<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='54.83' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_99' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>1<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='69.84' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_100' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>2<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='84.85' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_101' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>3<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='99.86' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_102' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>4<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='114.87' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_103' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>5<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='129.88' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_104' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>6<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='144.89' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_105' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>7<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='159.90' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_106' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>8<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='174.91' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_107' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>9<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='187.47' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_108' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>10<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='202.48' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_109' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>11<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='217.49' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_110' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>12<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='232.50' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_111' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>13<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='247.51' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_112' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>14<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='262.52' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_113' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>15<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='277.53' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_114' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>16<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='292.54' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_115' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>17<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='307.55' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_116' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>18<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='322.56' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_117' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>19<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='337.57' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_118' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>20<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='352.58' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_119' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>21<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='367.59' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_120' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>22<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='382.60' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_121' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>23<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='397.61' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_122' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>24<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='412.62' y='339.51' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_123' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>25<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='189.85' y='352.09' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_124' font-size='8.25pt' font-family='Helvetica'>APR MDC Code<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text transform='translate(13.37,233.29) rotate(-90.00)' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_125' font-size='8.25pt' font-family='Helvetica'>Average Length of Stay<\/text>\n    <\/g>\n    <g clip-path='url(#svg_ed6519e3-2633-448f-a1fc-b464fadacddc_cl_4)'>\n      <text x='33.26' y='14.95' id='svg_ed6519e3-2633-448f-a1fc-b464fadacddc_el_126' font-size='9.90pt' font-family='Helvetica'>Average Length of Stay by APR MDC Code<\/text>\n    <\/g>\n  <\/g>\n<\/svg>","js":null,"uid":"svg_ed6519e3-2633-448f-a1fc-b464fadacddc","ratio":1.2,"settings":{"tooltip":{"css":".tooltip_SVGID_ { background-color:gray;color:white;padding:2px;border-radius:2px; ; position:absolute;pointer-events:none;z-index:999;}\n","offx":10,"offy":0,"use_cursor_pos":true,"opacity":0.8,"usefill":false,"usestroke":false,"delay":{"over":200,"out":500}},"hover":{"css":".hover_SVGID_ { fill:#1279BF;stroke:#1279BF;cursor:pointer;stroke-width:1; }\n","reactive":false},"hoverkey":{"css":".hover_key_SVGID_ { stroke:red; }\n","reactive":false},"hovertheme":{"css":".hover_theme_SVGID_ { fill:green; }\n","reactive":false},"hoverinv":{"css":""},"zoom":{"min":1,"max":1},"capture":{"css":".selected_SVGID_ { fill:red;stroke:gray; }\n","type":"multiple","only_shiny":true,"selected":[]},"capturekey":{"css":".selected_key_SVGID_ { stroke:gray; }\n","type":"single","only_shiny":true,"selected":[]},"capturetheme":{"css":".selected_theme_SVGID_ { stroke:gray; }\n","type":"single","only_shiny":true,"selected":[]},"toolbar":{"position":"topright","saveaspng":true,"pngname":"diagram"},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

As severity of illness increases, average length of stay increases. Cases labeled as extreme stay for an average of 15 days.


```r
ggplot(subset(dat, !is.na(APR.Severity.of.Illness.Description)), aes(x = reorder(APR.Severity.of.Illness.Description, 
    Length.of.Stay), y = Length.of.Stay, fill = APR.Severity.of.Illness.Description)) + 
    stat_summary(fun = "mean", geom = "bar") + theme(legend.position = "none") + 
    ggtitle("Length of Stay by Severity of Illness") + xlab("Severity of Illness") + 
    ylab("Average Length of Stay")
```

<img src="Report_files/figure-html/los-by-aprseverity-1.png" style="display: block; margin: auto;" />


Ownership type was one of the variables merged from the second dataset which gave additional information about facility type. There is a clear distinction in mean length of stay between the different ownership types with the longest average length of stay being for a Public Benefit Corporation (PBC). A PBC is a type of for-profit entity. Municipality owned hospitals, which are run by the local government, have the second largest average length of stay 

Similarly, the boxplot shows a higher median length of stay for PBC and a larger spread.


```r
ggplot(subset(dat, !is.na(Ownership.Type)), aes(x = reorder(Ownership.Type, Length.of.Stay), 
    y = Length.of.Stay, fill = Ownership.Type)) + stat_summary(fun = "mean", geom = "bar") + 
    theme(legend.position = "none") + ggtitle("Length of Stay by Ownership Type") + 
    xlab("Ownership Type") + ylab("Average Length of Stay")
```

<img src="Report_files/figure-html/los-by-ownership-1.png" style="display: block; margin: auto;" />

```r
ggplot(dat, aes(x = Ownership.Type, y = Length.of.Stay, fill = Ownership.Type)) + 
    geom_boxplot() + theme(legend.position = "none") + ggtitle("Length of Stay by Ownership Type") + 
    xlab("Ownership Type") + ylab("Length of Stay") + scale_y_continuous(breaks = c(1, 
    10, 20), limits = c(0, 20))
```

<img src="Report_files/figure-html/los-by-ownership-2.png" style="display: block; margin: auto;" />

The average length of stay is greater in hospitals than in Critical Access Hospitals. Critical Access Hospitals are rural hospitals overseen by the Centers for Medicare and Medicaid Services (CMS) that meet the following criteria: 25 or fewer acute care inpatient beds, at least 35 miles from the nearest hospital, keep the average length of stay for the year at 96 hours or less for acute care patients, and have emergency care services available 24/7. By keeping their status as a Critical Access Hospital, they receive certain benefits from CMS which would Critical Access Hopsitals to keep the length of stay lower in order to keep their status. https://www.ruralhealthinfo.org/topics/critical-access-hospitals ## Cite.


```r
ggplot(dat, aes(x = Facility.Type, y = Length.of.Stay, fill = Facility.Type)) + stat_summary(fun = "mean", 
    geom = "bar") + theme(legend.position = "none") + ggtitle("Average Length of stay by Facility Type") + 
    xlab("Facility Type") + ylab("Average Length of Stay")
```

<img src="Report_files/figure-html/los-facility-type-1.png" style="display: block; margin: auto;" />

Cost per day is highest in patients with elective procedures and cases of trauma.


```r
ggplot(dat, aes(x = Type.of.Admission, y = Cost.per.Day, fill = Type.of.Admission)) + 
    stat_summary(fun = "mean", geom = "bar") + theme(legend.position = "none") + 
    ggtitle("Cost per Day by Type of Admission") + xlab("Admission Type") + ylab("Average Cost per Day")
```

<img src="Report_files/figure-html/cpd-by-typeadmit-1.png" style="display: block; margin: auto;" />

Average cost per day increased as age group increased with the highest average cost in the age group from 50 to 69. Surprisingly, average cost for those 70 or older was less than both age groups of 30 to 49 and 50 to 69.


```r
ggplot(dat, aes(x = Age.Group, y = Cost.per.Day, fill = Age.Group)) + stat_summary(fun = "mean", 
    geom = "bar") + theme(legend.position = "none") + ggtitle("Cost per day by Age Group") + 
    xlab("Age Group") + ylab("Average Cost per day")
```

<img src="Report_files/figure-html/cpd-by-age-1.png" style="display: block; margin: auto;" />

Diseases and Disorders of the Male Reproductive System (MDC Code 12), Diseases and Disorders of the Musculoskeletal System and Connective Tissue (MDC Code 8), Diseases and Disorders of the Female Reproductive System (MDC Code 13) had the highest average costs per day. 


```r
# ggplot(dat, aes(x = APR.MDC.Code, y = Cost.per.Day)) + stat_summary(fun =
# 'mean', geom = 'bar', fill = '#69b3a2') + ggtitle('Average Cost per day by APR
# MDC Code') + xlab('APR MDC Code') + ylab('Average Cost per day')

# girafe_options( girafe(code = print(ggplot(dat, aes(x = APR.MDC.Code, y =
# Length.of.Stay, tooltip = APR.MDC.Description)) + geom_bar_interactive(stat =
# 'summary', fun = 'mean', fill = '#69b3a2') + ggtitle('Average Length of Stay by
# APR MDC Code') + xlab('APR MDC Code') + ylab('Average Length of Stay'))),
# opts_sizing(rescale = TRUE), opts_tooltip(opacity = .8, css =
# 'background-color:gray;color:white;padding:2px;border-radius:2px;'),
# opts_hover(css = 'fill:#1279BF;stroke:#1279BF;cursor:pointer;stroke-width:1;'))

girafe_options(girafe(code = print(ggplot(dat, aes(x = APR.MDC.Code, y = Cost.per.Day, 
    tooltip = APR.MDC.Description)) + geom_bar_interactive(stat = "summary", fun = "mean", 
    fill = "#69b3a2") + ggtitle("Average Cost per day by APR MDC Code") + xlab("APR MDC Code") + 
    ylab("Average Cost per day"))), opts_sizing(rescale = TRUE), opts_tooltip(opacity = 0.9, 
    css = "background-color:gray;color:white;padding:2px;border-radius:2px;"), opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;stroke-width:1;"))
```

<!--html_preserve--><div id="htmlwidget-ba5f9dc9ba96713bbac7" style="width:672px;height:432px;" class="girafe html-widget"></div>
<script type="application/json" data-for="htmlwidget-ba5f9dc9ba96713bbac7">{"x":{"html":"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d' viewBox='0 0 432.00 360.00'>\n  <g>\n    <defs>\n      <clipPath id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_1'>\n        <rect x='0.00' y='0.00' width='432.00' height='360.00'/>\n      <\/clipPath>\n    <\/defs>\n    <rect x='0.00' y='0.00' width='432.00' height='360.00' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_1' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_1)' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.75' stroke-linejoin='round' stroke-linecap='round'/>\n    <defs>\n      <clipPath id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_2'>\n        <rect x='0.00' y='0.00' width='432.00' height='360.00'/>\n      <\/clipPath>\n    <\/defs>\n    <rect x='0.00' y='0.00' width='432.00' height='360.00' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_2' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_2)' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='round'/>\n    <defs>\n      <clipPath id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3'>\n        <rect x='43.05' y='23.35' width='383.47' height='304.92'/>\n      <\/clipPath>\n    <\/defs>\n    <rect x='43.05' y='23.35' width='383.47' height='304.92' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_3' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#EBEBEB' fill-opacity='1' stroke='none'/>\n    <polyline points='43.05,271.91 426.52,271.91' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_4' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='43.05,186.91 426.52,186.91' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_5' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='43.05,101.90 426.52,101.90' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_6' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.53' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='43.05,314.41 426.52,314.41' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_7' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='43.05,229.41 426.52,229.41' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_8' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='43.05,144.40 426.52,144.40' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_9' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='43.05,59.40 426.52,59.40' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_10' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='51.83,328.27 51.83,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_11' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='66.47,328.27 66.47,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_12' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='81.10,328.27 81.10,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_13' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='95.74,328.27 95.74,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_14' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='110.38,328.27 110.38,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_15' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='125.01,328.27 125.01,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_16' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='139.65,328.27 139.65,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_17' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='154.29,328.27 154.29,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_18' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='168.92,328.27 168.92,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_19' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='183.56,328.27 183.56,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_20' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='198.19,328.27 198.19,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_21' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='212.83,328.27 212.83,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_22' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='227.47,328.27 227.47,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_23' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='242.10,328.27 242.10,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_24' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='256.74,328.27 256.74,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_25' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='271.38,328.27 271.38,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_26' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='286.01,328.27 286.01,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_27' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='300.65,328.27 300.65,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_28' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='315.28,328.27 315.28,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_29' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='329.92,328.27 329.92,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_30' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='344.56,328.27 344.56,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_31' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='359.19,328.27 359.19,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_32' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='373.83,328.27 373.83,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_33' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='388.47,328.27 388.47,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_34' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='403.10,328.27 403.10,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_35' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='417.74,328.27 417.74,23.35' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_36' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <rect x='45.25' y='236.06' width='13.17' height='78.34' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_37' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Pre-MDC or Ungroupable'/>\n    <rect x='59.88' y='141.89' width='13.17' height='172.52' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_38' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Nervous System'/>\n    <rect x='74.52' y='133.78' width='13.17' height='180.62' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_39' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Eye'/>\n    <rect x='89.15' y='130.70' width='13.17' height='183.70' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_40' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Ear, Nose, Mouth, Throat and Craniofacial Diseases and Disorders'/>\n    <rect x='103.79' y='186.49' width='13.17' height='127.91' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_41' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Respiratory System'/>\n    <rect x='118.43' y='106.50' width='13.17' height='207.91' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_42' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Circulatory System'/>\n    <rect x='133.06' y='172.38' width='13.17' height='142.03' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_43' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Digestive System'/>\n    <rect x='147.70' y='164.34' width='13.17' height='150.07' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_44' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Hepatobiliary System and Pancreas'/>\n    <rect x='162.34' y='46.58' width='13.17' height='267.83' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_45' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Musculoskeletal System and Conn Tissue'/>\n    <rect x='176.97' y='166.93' width='13.17' height='147.48' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_46' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Skin, Subcutaneous Tissue and Breast'/>\n    <rect x='191.61' y='134.97' width='13.17' height='179.44' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_47' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Endocrine, Nutritional and Metabolic Diseases and Disorders'/>\n    <rect x='206.24' y='167.31' width='13.17' height='147.10' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_48' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Kidney and Urinary Tract'/>\n    <rect x='220.88' y='37.21' width='13.17' height='277.20' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_49' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Male Reproductive System'/>\n    <rect x='235.52' y='73.68' width='13.17' height='240.73' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_50' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of the Female Reproductive System'/>\n    <rect x='250.15' y='187.47' width='13.17' height='126.94' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_51' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Pregnancy, Childbirth and the Puerperium'/>\n    <rect x='264.79' y='259.82' width='13.17' height='54.59' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_52' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Newborns and Other Neonates with Conditions Originating in the Perinatal Period'/>\n    <rect x='279.43' y='169.95' width='13.17' height='144.45' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_53' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Diseases and Disorders of Blood, Blood Forming Organs and Immunological Disorders'/>\n    <rect x='294.06' y='143.56' width='13.17' height='170.85' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_54' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Lymphatic, Hematopoietic, Other Malignancies, Chemotherapy and Radiotherapy'/>\n    <rect x='308.70' y='194.55' width='13.17' height='119.85' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_55' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Infectious and Parasitic Diseases, Systemic or Unspecified Sites'/>\n    <rect x='323.33' y='254.68' width='13.17' height='59.72' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_56' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Mental Diseases and Disorders'/>\n    <rect x='337.97' y='243.81' width='13.17' height='70.60' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_57' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Alcohol/Drug Use and Alcohol/Drug Induced Organic Mental Disorders'/>\n    <rect x='352.61' y='158.22' width='13.17' height='156.19' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_58' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Poisonings, Toxic Effects, Other Injuries and Other Complications of Treatment'/>\n    <rect x='367.24' y='115.09' width='13.17' height='199.32' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_59' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Burns'/>\n    <rect x='381.88' y='211.61' width='13.17' height='102.79' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_60' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Rehabilitation, Aftercare, Other Factors Influencing Health Status and Other Health Service Contacts'/>\n    <rect x='396.52' y='168.33' width='13.17' height='146.08' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_61' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Human Immunodeficiency Virus Infections'/>\n    <rect x='411.15' y='95.11' width='13.17' height='219.30' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_62' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_3)' fill='#69B3A2' fill-opacity='1' stroke='none' title='Multiple Significant Trauma'/>\n    <defs>\n      <clipPath id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4'>\n        <rect x='0.00' y='0.00' width='432.00' height='360.00'/>\n      <\/clipPath>\n    <\/defs>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='33.22' y='317.56' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_63' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>0<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='18.54' y='232.56' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_64' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>2000<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='18.54' y='147.56' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_65' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>4000<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='18.54' y='62.56' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_66' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>6000<\/text>\n    <\/g>\n    <polyline points='40.31,314.41 43.05,314.41' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_67' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='40.31,229.41 43.05,229.41' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_68' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='40.31,144.40 43.05,144.40' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_69' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='40.31,59.40 43.05,59.40' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_70' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='51.83,331.01 51.83,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_71' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='66.47,331.01 66.47,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_72' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='81.10,331.01 81.10,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_73' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='95.74,331.01 95.74,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_74' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='110.38,331.01 110.38,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_75' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='125.01,331.01 125.01,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_76' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='139.65,331.01 139.65,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_77' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='154.29,331.01 154.29,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_78' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='168.92,331.01 168.92,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_79' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='183.56,331.01 183.56,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_80' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='198.19,331.01 198.19,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_81' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='212.83,331.01 212.83,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_82' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='227.47,331.01 227.47,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_83' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='242.10,331.01 242.10,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_84' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='256.74,331.01 256.74,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_85' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='271.38,331.01 271.38,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_86' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='286.01,331.01 286.01,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_87' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='300.65,331.01 300.65,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_88' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='315.28,331.01 315.28,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_89' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='329.92,331.01 329.92,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_90' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='344.56,331.01 344.56,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_91' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='359.19,331.01 359.19,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_92' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='373.83,331.01 373.83,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_93' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='388.47,331.01 388.47,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_94' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='403.10,331.01 403.10,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_95' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <polyline points='417.74,331.01 417.74,328.27' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_96' clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='49.39' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_97' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>0<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='64.02' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_98' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>1<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='78.66' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_99' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>2<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='93.29' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_100' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>3<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='107.93' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_101' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>4<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='122.57' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_102' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>5<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='137.20' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_103' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>6<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='151.84' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_104' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>7<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='166.48' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_105' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>8<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='181.11' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_106' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>9<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='193.30' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_107' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>10<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='207.94' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_108' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>11<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='222.57' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_109' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>12<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='237.21' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_110' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>13<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='251.85' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_111' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>14<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='266.48' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_112' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>15<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='281.12' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_113' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>16<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='295.75' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_114' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>17<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='310.39' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_115' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>18<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='325.03' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_116' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>19<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='339.66' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_117' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>20<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='354.30' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_118' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>21<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='368.94' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_119' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>22<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='383.57' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_120' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>23<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='398.21' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_121' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>24<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='412.84' y='339.51' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_122' font-size='6.60pt' fill='#4D4D4D' fill-opacity='1' font-family='Helvetica'>25<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='194.75' y='352.09' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_123' font-size='8.25pt' font-family='Helvetica'>APR MDC Code<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text transform='translate(13.37,229.00) rotate(-90.00)' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_124' font-size='8.25pt' font-family='Helvetica'>Average Cost per day<\/text>\n    <\/g>\n    <g clip-path='url(#svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_cl_4)'>\n      <text x='43.05' y='14.95' id='svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d_el_125' font-size='9.90pt' font-family='Helvetica'>Average Cost per day by APR MDC Code<\/text>\n    <\/g>\n  <\/g>\n<\/svg>","js":null,"uid":"svg_8586391e-2cd4-4486-8a6c-a99f9d00b97d","ratio":1.2,"settings":{"tooltip":{"css":".tooltip_SVGID_ { background-color:gray;color:white;padding:2px;border-radius:2px; ; position:absolute;pointer-events:none;z-index:999;}\n","offx":10,"offy":0,"use_cursor_pos":true,"opacity":0.9,"usefill":false,"usestroke":false,"delay":{"over":200,"out":500}},"hover":{"css":".hover_SVGID_ { fill:#1279BF;stroke:#1279BF;cursor:pointer;stroke-width:1; }\n","reactive":false},"hoverkey":{"css":".hover_key_SVGID_ { stroke:red; }\n","reactive":false},"hovertheme":{"css":".hover_theme_SVGID_ { fill:green; }\n","reactive":false},"hoverinv":{"css":""},"zoom":{"min":1,"max":1},"capture":{"css":".selected_SVGID_ { fill:red;stroke:gray; }\n","type":"multiple","only_shiny":true,"selected":[]},"capturekey":{"css":".selected_key_SVGID_ { stroke:gray; }\n","type":"single","only_shiny":true,"selected":[]},"capturetheme":{"css":".selected_theme_SVGID_ { stroke:gray; }\n","type":"single","only_shiny":true,"selected":[]},"toolbar":{"position":"topright","saveaspng":true,"pngname":"diagram"},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Municipality owned hospitals' cost per patient per day is at least $1000 more on average than the other four ownership types. 


```r
ggplot(dat, aes(x = Ownership.Type, y = Cost.per.Day, fill = Ownership.Type)) + stat_summary(fun = "mean", 
    geom = "bar") + theme(legend.position = "none") + ggtitle("Average Cost per day by Ownership Type") + 
    xlab("Ownership Type") + ylab("Average Cost per day")
```

<img src="Report_files/figure-html/cpd-owner-type-1.png" style="display: block; margin: auto;" />

## Data Filtering

We chose to filter out observations that were related to pregnancy and newborn data as there are unique characteristics of their features. The birthweight feature, for example, was only reported for newborns. For all other observations, the birthweight was recorded as 0. With pregnancy related variables, some observations had a lot of missing data due to confidentiality issues in cases that may have been related to abortion (i.e. abortion procedure, miscarriage, ectopic pregnancy, etc.). Due to these unique characteritics of this data, we decided to filter out these observations.

Rows meeting the following criteria were filtered 

APR.MDC.Code = 14 ['Pregnancy, Childbirth And Puerperium']
APR.MDC.Code = 15 ['Newborn And Other Neonates (Perinatal Period)']
Abortion.Edit.Indicator = 'Y'
Type.of.Admission = 'Newborn'


```{.r .fold-show}
# get unique codes, descriptions
aprdrg <- unique(dat[, c("APR.DRG.Code", "APR.DRG.Description")])
aprdrg <- aprdrg[order(aprdrg$APR.DRG.Code), ]
rownames(aprdrg) <- NULL

ccs <- unique(dat[, c("CCS.Diagnosis.Code", "CCS.Diagnosis.Description")])
ccs <- ccs[order(ccs$CCS.Diagnosis.Code), ]
rownames(ccs) <- NULL

# terms for filtering out OB-related
obgyn_terms <- paste0(c("abortion", "amniot", "birth", "C-section", "delivery", "fetal", 
    "fetopelvic", "labor", "liveborn", "malposition", "natal", "neonat", "obstetric", 
    "OB-related", "partum", "pregnancy", "umbilical cord complication"), collapse = "|")
exclude_terms <- paste0(c("non-obstetric", "except in labor"), collapse = "|")

filter_obgyn <- function(x) {
    return(grep(exclude_terms, grep(obgyn_terms, x, ignore.case = TRUE, value = TRUE), 
        ignore.case = TRUE, value = TRUE, invert = TRUE))
}

# get APR.DRG.Code values to filter out
aprdrg_obgyn_codes <- aprdrg[aprdrg$APR.DRG.Description %in% filter_obgyn(aprdrg$APR.DRG.Description), 
    "APR.DRG.Code"]

# get CCS.Diagnosis.Code values to filter out
ccs_obgyn_codes <- ccs[ccs$CCS.Diagnosis.Description %in% filter_obgyn(ccs$CCS.Diagnosis.Description), 
    "CCS.Diagnosis.Code"]

# filter dat to include only non-obgyn data, non-neonate data
obgyn_rows <- with(dat, which(CCS.Diagnosis.Code %in% ccs_obgyn_codes | APR.DRG.Code %in% 
    aprdrg_obgyn_codes | APR.MDC.Code == 14 | APR.MDC.Code == 15 | Abortion.Edit.Indicator == 
    "Y" | Type.of.Admission == "Newborn" | Birth.Weight > 0))
dat <- dat[-obgyn_rows, ]

# remove columns
dat <- dat[, which(!(colnames(dat) %in% c("Abortion.Edit.Indicator", "Birth.Weight")))]
```

# Medicare Patient Data

Due to the amount of computing power that it would take to analyze the whole dataset, we chose to subset the data by if any of a patient's payment types (primary, secondary, or tertiary) were Medicare.  870,472 patients had Medicare as their primary payment type, and 55,922 patients had Medicare as either their secondary or tertiary payment type for a total of 926,394


```{.r .fold-show}
# Subset to only medicare patients
medicare_rows <- with(dat, Vectorize(isTRUE)(Payment.Typology.1 == "Medicare" | Payment.Typology.2 == 
    "Medicare" | Payment.Typology.3 == "Medicare"))
dat <- dat[medicare_rows, ]

# drop unused levels
dat <- droplevels(dat)
```
<!-- 
# CHECK THESE?? 
# just type1 medicare is 876258 rows
# any is medicare is 932499
 -->

## Figures
 <!-- using subset of data (Only Medicare) -->

The distribution of length of stay in the subset with only patients with Medicare resembles the overall distribution, but the largest frequency of length of stay in this subset is 3 days.


```r
ggplot(dat, aes(x = Length.of.Stay)) + geom_histogram(binwidth = 1, fill = "#69b3a2", 
    color = "#e9ecef", alpha = 0.9) + ggtitle("Distribution of Length of Stay") + 
    xlab("Length of Stay") + ylab("Frequency") + scale_x_continuous(breaks = c(1, 
    10, 20, 30, 40), limits = c(0, 40)) + scale_y_continuous(labels = scales::comma)
```

<img src="Report_files/figure-html/los-dist-medicare-1.png" style="display: block; margin: auto;" />

The greatest number of medicare patients were admitted due to Diseases and Disorders of the Circulatory System (MDC Code 5) then  Diseases and Disorders of the Respiratory System (MDC Code 4) and Diseases and Disorders of the Musculoskeletal System and Connective Tissue (MDC Code 8).


```r
girafe_options(girafe(ggplot(dat, aes(x = APR.MDC.Code, tooltip = APR.MDC.Description)) + 
    geom_bar_interactive(fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) + ggtitle("Frequency of APR MDC Code in Medicare Patients") + 
    xlab("APR MDC Code") + ylab("Frequency")), opts_sizing(rescale = FALSE), opts_tooltip(opacity = 0.9, 
    css = "background-color:gray;color:white;padding:2px;border-radius:2px;"), opts_hover(css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;"))
```

<!--html_preserve--><div id="htmlwidget-328d4ec8f1145a51e0a6" style="width:672px;height:432px;" class="girafe html-widget"></div>
<script type="application/json" data-for="htmlwidget-328d4ec8f1145a51e0a6">{"x":{"html":"","js":null,"uid":"svg_bc8c3d94-2534-4aac-8f52-b9660fa5d226","ratio":1.2,"settings":{"tooltip":{"css":".tooltip_SVGID_ { background-color:gray;color:white;padding:2px;border-radius:2px; ; position:absolute;pointer-events:none;z-index:999;}\n","offx":10,"offy":0,"use_cursor_pos":true,"opacity":0.9,"usefill":false,"usestroke":false,"delay":{"over":200,"out":500}},"hover":{"css":".hover_SVGID_ { fill:#1279BF;stroke:#1279BF;cursor:pointer; }\n","reactive":false},"hoverkey":{"css":".hover_key_SVGID_ { stroke:red; }\n","reactive":false},"hovertheme":{"css":".hover_theme_SVGID_ { fill:green; }\n","reactive":false},"hoverinv":{"css":""},"zoom":{"min":1,"max":1},"capture":{"css":".selected_SVGID_ { fill:red;stroke:gray; }\n","type":"multiple","only_shiny":true,"selected":[]},"capturekey":{"css":".selected_key_SVGID_ { stroke:gray; }\n","type":"single","only_shiny":true,"selected":[]},"capturetheme":{"css":".selected_theme_SVGID_ { stroke:gray; }\n","type":"single","only_shiny":true,"selected":[]},"toolbar":{"position":"topright","saveaspng":true,"pngname":"diagram"},"sizing":{"rescale":false,"width":1}}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

As expected, the majority of patients with Medicare are 70 or older and also many in the 50 to 69 age group.


```r
ggplot(dat, aes(x = Age.Group)) + geom_bar(fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) + 
    ggtitle("Frequency of Individuals with Medicare by Age Group") + xlab("Medicare") + 
    ylab("Frequency") + scale_y_continuous(labels = scales::comma)
```

<img src="Report_files/figure-html/los-by-age-medicare-1.png" style="display: block; margin: auto;" />

# Analysis 


```{.r .fold-show}
# Split train and test set
set.seed(123)
train_rows <- caret::createDataPartition(y = dat$Length.of.Stay, p = 0.7, list = FALSE)
train <- dat[train_rows, ]
test <- dat[-train_rows, ]
```

## Evaluation Metrics

We will be using the same evaluation metrics used in the literature for the assessment similar length of stay prediction models:
$R^2$ [@tierneyPredictingInpatientCosts1995], mean absolute error (MAE) [@tsaiLengthHospitalStay2016;@tierneyPredictingInpatientCosts1995], mean relative error (MRE) [@tsaiLengthHospitalStay2016], and root mean squared error (RMSE) [@tsaiLengthHospitalStay2016].

$$ MAE = \frac {\sum_{i = 1}^{n} |\hat y_i - y_i|}{n} $$

$$ MRE = \frac {\sum_{i = 1}^{n} (|\hat y_i - y_i|/y_i)}{n}$$

$$ RMSE = \sqrt\frac {\sum_{i = 1}^{n} (\hat y_i - y_i)^2}{n}$$
where $\hat y_i$ is the predicted value for length of stay or cost per dayand $y_i$ is the actual value for length of stay or cost per day. 


```{.r .fold-show}
# Error functions
MAE <- function(pred, actual) {
    mean(abs(actual - pred))
}
MRE <- function(pred, actual) {
    mean(abs(actual - pred)/actual)
}
RMSE <- function(pred, actual) {
    sqrt(mean((actual - pred)^2))
}

evaluate_model <- function(model, test, response) {
    
    actual <- test[[response]]
    pred <- predict(model, test)
    
    # Remove rows when response == 0; MRE = infinity for those observations
    if (any(test[[response]] == 0)) {
        test_mre <- test[!test[[response]] == 0, ]
        pred_mre <- predict(model, test_mre)
        actual_mre <- test_mre[[response]]
    } else {
        pred_mre <- pred
        actual_mre <- actual
    }
    
    return(c(`Mean Absolute Error` = round(MAE(pred, actual), digits = 2), `Mean Relative Error` = round(MRE(pred_mre, 
        actual_mre), digits = 2), `Root Mean Squared Error` = round(RMSE(pred, actual), 
        digits = 2)))
}
```


## Length of Stay

### Linear Regression
We wanted to focus our efforts in predicting length of stay through regression methods and not classification methods because knowing the exact number of days a patient would need to stay would be overall more useful than knowing a range of days a patient may stay. We start with a simple linear regression as our base model although we acknowledge that fundamentally, this is not the proper model that we want to use to model length of stay. 



```{.r .fold-show}
linearReg <- lm(Length.of.Stay ~ Age.Group + Gender + Race + Ethnicity + Type.of.Admission + 
    APR.MDC.Code + APR.Severity.of.Illness.Code + APR.Risk.of.Mortality + Ownership.Type + 
    Facility.Type + Emergency.Department.Indicator + Health.Service.Area + Hospital.County, 
    data = train)
summary(linearReg)
```

```
## 
## Call:
## lm(formula = Length.of.Stay ~ Age.Group + Gender + Race + Ethnicity + 
##     Type.of.Admission + APR.MDC.Code + APR.Severity.of.Illness.Code + 
##     APR.Risk.of.Mortality + Ownership.Type + Facility.Type + 
##     Emergency.Department.Indicator + Health.Service.Area + Hospital.County, 
##     data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -24.177  -2.925  -0.991   1.348 119.052 
## 
## Coefficients: (7 not defined because of singularities)
##                                                                 Estimate
## (Intercept)                                                    0.8904713
## Age.Group18 to 29                                              1.9105310
## Age.Group30 to 49                                              2.0162868
## Age.Group50 to 69                                              2.0917004
## Age.Group70 or Older                                           1.6539604
## GenderM                                                       -0.0618477
## GenderU                                                       -7.2389177
## RaceMulti-racial                                              -0.0451209
## RaceOther Race                                                -0.5147375
## RaceWhite                                                     -0.4187032
## EthnicityNot Span/Hispanic                                     0.0005199
## EthnicitySpanish/Hispanic                                     -0.3367566
## EthnicityUnknown                                              -0.0524886
## Type.of.AdmissionEmergency                                    -0.5622573
## Type.of.AdmissionNewborn                                      -1.0148970
## Type.of.AdmissionNot Available                                -0.9410423
## Type.of.AdmissionTrauma                                        0.6023867
## Type.of.AdmissionUrgent                                        0.8996517
## APR.MDC.Code2                                                 -0.7084378
## APR.MDC.Code3                                                 -0.9487852
## APR.MDC.Code4                                                 -0.6669177
## APR.MDC.Code5                                                 -1.1224392
## APR.MDC.Code6                                                 -0.0951421
## APR.MDC.Code7                                                 -0.3442114
## APR.MDC.Code8                                                 -0.0735727
## APR.MDC.Code9                                                  0.0140938
## APR.MDC.Code10                                                -1.2723340
## APR.MDC.Code11                                                -0.9930641
## APR.MDC.Code12                                                -1.0146809
## APR.MDC.Code13                                                -0.7869975
## APR.MDC.Code14                                                -1.7074509
## APR.MDC.Code15                                                 2.7138338
## APR.MDC.Code16                                                -0.6919292
## APR.MDC.Code17                                                 2.1094250
## APR.MDC.Code18                                                -0.6661475
## APR.MDC.Code19                                                 9.6670971
## APR.MDC.Code20                                                 1.9124979
## APR.MDC.Code21                                                -1.4184796
## APR.MDC.Code22                                                 5.4550219
## APR.MDC.Code23                                                 4.2822706
## APR.MDC.Code24                                                -0.9333009
## APR.MDC.Code25                                                 1.2496106
## APR.Severity.of.Illness.Code2                                  1.1194781
## APR.Severity.of.Illness.Code3                                  3.1966600
## APR.Severity.of.Illness.Code4                                  9.0774230
## APR.Risk.of.MortalityMajor                                    -1.3945218
## APR.Risk.of.MortalityMinor                                    -3.2544589
## APR.Risk.of.MortalityModerate                                 -2.4734622
## Ownership.TypeMunicipality                                     5.9145388
## Ownership.TypeNot for Profit Corporation                       4.6054131
## Ownership.TypePublic Benefit Corporation                       6.6236489
## Ownership.TypeState                                            4.8904553
## Facility.TypePrimary Care Hospital - Critical Access Hospital -0.5916545
## Emergency.Department.IndicatorY                               -0.3006372
## Health.Service.AreaCentral NY                                 -1.3201502
## Health.Service.AreaFinger Lakes                               -2.8557387
## Health.Service.AreaHudson Valley                               0.4721913
## Health.Service.AreaLong Island                                 0.0423467
## Health.Service.AreaNew York City                               0.3871223
## Health.Service.AreaSouthern Tier                              -1.3525717
## Health.Service.AreaWestern NY                                  3.7740839
## Hospital.CountyAllegany                                       -5.0409804
## Hospital.CountyBronx                                           0.1430348
## Hospital.CountyBroome                                          0.5007817
## Hospital.CountyCattaraugus                                    -5.4714249
## Hospital.CountyCayuga                                         -0.3776768
## Hospital.CountyChautauqua                                     -5.2334163
## Hospital.CountyChemung                                         1.9660785
## Hospital.CountyChenango                                               NA
## Hospital.CountyClinton                                        -0.0589917
## Hospital.CountyColumbia                                        0.1752274
## Hospital.CountyCortland                                       -0.4295151
## Hospital.CountyDelaware                                       -0.6490867
## Hospital.CountyDutchess                                       -1.6003697
## Hospital.CountyErie                                           -4.5542495
## Hospital.CountyEssex                                          -0.7243896
## Hospital.CountyFranklin                                       -1.8455346
## Hospital.CountyFulton                                         -1.2976631
## Hospital.CountyGenesee                                        -5.1926560
## Hospital.CountyHerkimer                                        0.5942764
## Hospital.CountyJefferson                                       1.3141325
## Hospital.CountyKings                                          -0.0976475
## Hospital.CountyLewis                                           4.2624065
## Hospital.CountyLivingston                                      2.7830813
## Hospital.CountyMadison                                        -0.1298181
## Hospital.CountyManhattan                                      -0.4745205
## Hospital.CountyMonroe                                          3.0155709
## Hospital.CountyMontgomery                                     -1.2493595
## Hospital.CountyNassau                                          0.3185391
## Hospital.CountyNiagara                                        -4.0580476
## Hospital.CountyOneida                                          0.8083703
## Hospital.CountyOnondaga                                        1.1850888
## Hospital.CountyOntario                                         1.9986217
## Hospital.CountyOrange                                         -1.2914011
## Hospital.CountyOrleans                                        -3.4177959
## Hospital.CountyOswego                                          0.3315722
## Hospital.CountyOtsego                                         -0.4656128
## Hospital.CountyPutnam                                         -2.4480239
## Hospital.CountyQueens                                          0.1454018
## Hospital.CountyRensselaer                                     -0.2339306
## Hospital.CountyRichmond                                               NA
## Hospital.CountyRockland                                       -0.7285559
## Hospital.CountySaratoga                                        0.1515545
## Hospital.CountySchenectady                                    -0.3477801
## Hospital.CountySchuyler                                        2.6933416
## Hospital.CountySt Lawrence                                    -0.0119358
## Hospital.CountySteuben                                         1.0355611
## Hospital.CountySuffolk                                                NA
## Hospital.CountySullivan                                       -1.4775530
## Hospital.CountyTompkins                                               NA
## Hospital.CountyUlster                                         -0.7822473
## Hospital.CountyWarren                                         -0.5347665
## Hospital.CountyWayne                                           1.3079409
## Hospital.CountyWestchester                                            NA
## Hospital.CountyWyoming                                                NA
## Hospital.CountyYates                                                  NA
##                                                               Std. Error
## (Intercept)                                                    1.7488492
## Age.Group18 to 29                                              0.4212032
## Age.Group30 to 49                                              0.4111329
## Age.Group50 to 69                                              0.4095378
## Age.Group70 or Older                                           0.4095484
## GenderM                                                        0.0186695
## GenderU                                                        5.1980167
## RaceMulti-racial                                               0.1035007
## RaceOther Race                                                 0.0353262
## RaceWhite                                                      0.0281089
## EthnicityNot Span/Hispanic                                     0.1873162
## EthnicitySpanish/Hispanic                                      0.1897518
## EthnicityUnknown                                               0.1936852
## Type.of.AdmissionEmergency                                     0.0438569
## Type.of.AdmissionNewborn                                       1.1791541
## Type.of.AdmissionNot Available                                 0.3952457
## Type.of.AdmissionTrauma                                        0.2100736
## Type.of.AdmissionUrgent                                        0.0477787
## APR.MDC.Code2                                                  0.2554885
## APR.MDC.Code3                                                  0.0904717
## APR.MDC.Code4                                                  0.0422907
## APR.MDC.Code5                                                  0.0385792
## APR.MDC.Code6                                                  0.0435209
## APR.MDC.Code7                                                  0.0650291
## APR.MDC.Code8                                                  0.0434411
## APR.MDC.Code9                                                  0.0639397
## APR.MDC.Code10                                                 0.0605722
## APR.MDC.Code11                                                 0.0477974
## APR.MDC.Code12                                                 0.1307281
## APR.MDC.Code13                                                 0.1262310
## APR.MDC.Code14                                                 0.1933626
## APR.MDC.Code15                                                 1.3098779
## APR.MDC.Code16                                                 0.0797751
## APR.MDC.Code17                                                 0.1002028
## APR.MDC.Code18                                                 0.0460816
## APR.MDC.Code19                                                 0.0619211
## APR.MDC.Code20                                                 0.0944272
## APR.MDC.Code21                                                 0.0967378
## APR.MDC.Code22                                                 0.4830312
## APR.MDC.Code23                                                 0.0669925
## APR.MDC.Code24                                                 0.1961567
## APR.MDC.Code25                                                 0.2350038
## APR.Severity.of.Illness.Code2                                  0.0294572
## APR.Severity.of.Illness.Code3                                  0.0368966
## APR.Severity.of.Illness.Code4                                  0.0556740
## APR.Risk.of.MortalityMajor                                     0.0453934
## APR.Risk.of.MortalityMinor                                     0.0560459
## APR.Risk.of.MortalityModerate                                  0.0504246
## Ownership.TypeMunicipality                                     1.6879614
## Ownership.TypeNot for Profit Corporation                       1.6873050
## Ownership.TypePublic Benefit Corporation                       1.6890345
## Ownership.TypeState                                            1.6881833
## Facility.TypePrimary Care Hospital - Critical Access Hospital  0.2424760
## Emergency.Department.IndicatorY                                0.0372065
## Health.Service.AreaCentral NY                                  0.1716250
## Health.Service.AreaFinger Lakes                                0.4817210
## Health.Service.AreaHudson Valley                               0.0671749
## Health.Service.AreaLong Island                                 0.0629531
## Health.Service.AreaNew York City                               0.0776531
## Health.Service.AreaSouthern Tier                               0.3073653
## Health.Service.AreaWestern NY                                  1.7088287
## Hospital.CountyAllegany                                        1.7320728
## Hospital.CountyBronx                                           0.0694059
## Hospital.CountyBroome                                          0.3129273
## Hospital.CountyCattaraugus                                     1.7158708
## Hospital.CountyCayuga                                          0.2338627
## Hospital.CountyChautauqua                                      1.7128352
## Hospital.CountyChemung                                         0.4901398
## Hospital.CountyChenango                                               NA
## Hospital.CountyClinton                                         0.1404805
## Hospital.CountyColumbia                                        0.1613704
## Hospital.CountyCortland                                        0.2572202
## Hospital.CountyDelaware                                        0.4830640
## Hospital.CountyDutchess                                        0.0826360
## Hospital.CountyErie                                            1.7084999
## Hospital.CountyEssex                                           0.5307376
## Hospital.CountyFranklin                                        0.1894195
## Hospital.CountyFulton                                          0.2397101
## Hospital.CountyGenesee                                         1.7195192
## Hospital.CountyHerkimer                                        0.4519121
## Hospital.CountyJefferson                                       0.2228079
## Hospital.CountyKings                                           0.0661124
## Hospital.CountyLewis                                           1.7619395
## Hospital.CountyLivingston                                      0.5481553
## Hospital.CountyMadison                                         0.2603637
## Hospital.CountyManhattan                                       0.0634534
## Hospital.CountyMonroe                                          0.4806337
## Hospital.CountyMontgomery                                      0.1540825
## Hospital.CountyNassau                                          0.0470473
## Hospital.CountyNiagara                                         1.7106115
## Hospital.CountyOneida                                          0.1790618
## Hospital.CountyOnondaga                                        0.1716711
## Hospital.CountyOntario                                         0.4917984
## Hospital.CountyOrange                                          0.0786243
## Hospital.CountyOrleans                                         1.7567895
## Hospital.CountyOswego                                          0.2481280
## Hospital.CountyOtsego                                          0.1207229
## Hospital.CountyPutnam                                          0.1532920
## Hospital.CountyQueens                                          0.0682313
## Hospital.CountyRensselaer                                      0.1287317
## Hospital.CountyRichmond                                               NA
## Hospital.CountyRockland                                        0.0874956
## Hospital.CountySaratoga                                        0.1381172
## Hospital.CountySchenectady                                     0.1037468
## Hospital.CountySchuyler                                        0.5793495
## Hospital.CountySt Lawrence                                     0.2109106
## Hospital.CountySteuben                                         0.5017361
## Hospital.CountySuffolk                                                NA
## Hospital.CountySullivan                                        0.2061062
## Hospital.CountyTompkins                                               NA
## Hospital.CountyUlster                                          0.1178500
## Hospital.CountyWarren                                          0.1147441
## Hospital.CountyWayne                                           0.5091726
## Hospital.CountyWestchester                                            NA
## Hospital.CountyWyoming                                                NA
## Hospital.CountyYates                                                  NA
##                                                               t value Pr(>|t|)
## (Intercept)                                                     0.509 0.610629
## Age.Group18 to 29                                               4.536 5.74e-06
## Age.Group30 to 49                                               4.904 9.38e-07
## Age.Group50 to 69                                               5.107 3.27e-07
## Age.Group70 or Older                                            4.038 5.38e-05
## GenderM                                                        -3.313 0.000924
## GenderU                                                        -1.393 0.163732
## RaceMulti-racial                                               -0.436 0.662875
## RaceOther Race                                                -14.571  < 2e-16
## RaceWhite                                                     -14.896  < 2e-16
## EthnicityNot Span/Hispanic                                      0.003 0.997786
## EthnicitySpanish/Hispanic                                      -1.775 0.075944
## EthnicityUnknown                                               -0.271 0.786391
## Type.of.AdmissionEmergency                                    -12.820  < 2e-16
## Type.of.AdmissionNewborn                                       -0.861 0.389404
## Type.of.AdmissionNot Available                                 -2.381 0.017270
## Type.of.AdmissionTrauma                                         2.868 0.004137
## Type.of.AdmissionUrgent                                        18.830  < 2e-16
## APR.MDC.Code2                                                  -2.773 0.005556
## APR.MDC.Code3                                                 -10.487  < 2e-16
## APR.MDC.Code4                                                 -15.770  < 2e-16
## APR.MDC.Code5                                                 -29.094  < 2e-16
## APR.MDC.Code6                                                  -2.186 0.028807
## APR.MDC.Code7                                                  -5.293 1.20e-07
## APR.MDC.Code8                                                  -1.694 0.090338
## APR.MDC.Code9                                                   0.220 0.825541
## APR.MDC.Code10                                                -21.005  < 2e-16
## APR.MDC.Code11                                                -20.777  < 2e-16
## APR.MDC.Code12                                                 -7.762 8.39e-15
## APR.MDC.Code13                                                 -6.235 4.53e-10
## APR.MDC.Code14                                                 -8.830  < 2e-16
## APR.MDC.Code15                                                  2.072 0.038282
## APR.MDC.Code16                                                 -8.674  < 2e-16
## APR.MDC.Code17                                                 21.052  < 2e-16
## APR.MDC.Code18                                                -14.456  < 2e-16
## APR.MDC.Code19                                                156.120  < 2e-16
## APR.MDC.Code20                                                 20.254  < 2e-16
## APR.MDC.Code21                                                -14.663  < 2e-16
## APR.MDC.Code22                                                 11.293  < 2e-16
## APR.MDC.Code23                                                 63.922  < 2e-16
## APR.MDC.Code24                                                 -4.758 1.96e-06
## APR.MDC.Code25                                                  5.317 1.05e-07
## APR.Severity.of.Illness.Code2                                  38.004  < 2e-16
## APR.Severity.of.Illness.Code3                                  86.638  < 2e-16
## APR.Severity.of.Illness.Code4                                 163.046  < 2e-16
## APR.Risk.of.MortalityMajor                                    -30.721  < 2e-16
## APR.Risk.of.MortalityMinor                                    -58.068  < 2e-16
## APR.Risk.of.MortalityModerate                                 -49.053  < 2e-16
## Ownership.TypeMunicipality                                      3.504 0.000458
## Ownership.TypeNot for Profit Corporation                        2.729 0.006344
## Ownership.TypePublic Benefit Corporation                        3.922 8.80e-05
## Ownership.TypeState                                             2.897 0.003769
## Facility.TypePrimary Care Hospital - Critical Access Hospital  -2.440 0.014685
## Emergency.Department.IndicatorY                                -8.080 6.48e-16
## Health.Service.AreaCentral NY                                  -7.692 1.45e-14
## Health.Service.AreaFinger Lakes                                -5.928 3.06e-09
## Health.Service.AreaHudson Valley                                7.029 2.08e-12
## Health.Service.AreaLong Island                                  0.673 0.501157
## Health.Service.AreaNew York City                                4.985 6.19e-07
## Health.Service.AreaSouthern Tier                               -4.401 1.08e-05
## Health.Service.AreaWestern NY                                   2.209 0.027204
## Hospital.CountyAllegany                                        -2.910 0.003610
## Hospital.CountyBronx                                            2.061 0.039318
## Hospital.CountyBroome                                           1.600 0.109530
## Hospital.CountyCattaraugus                                     -3.189 0.001429
## Hospital.CountyCayuga                                          -1.615 0.106322
## Hospital.CountyChautauqua                                      -3.055 0.002248
## Hospital.CountyChemung                                          4.011 6.04e-05
## Hospital.CountyChenango                                            NA       NA
## Hospital.CountyClinton                                         -0.420 0.674538
## Hospital.CountyColumbia                                         1.086 0.277536
## Hospital.CountyCortland                                        -1.670 0.094953
## Hospital.CountyDelaware                                        -1.344 0.179050
## Hospital.CountyDutchess                                       -19.366  < 2e-16
## Hospital.CountyErie                                            -2.666 0.007684
## Hospital.CountyEssex                                           -1.365 0.172293
## Hospital.CountyFranklin                                        -9.743  < 2e-16
## Hospital.CountyFulton                                          -5.413 6.18e-08
## Hospital.CountyGenesee                                         -3.020 0.002529
## Hospital.CountyHerkimer                                         1.315 0.188501
## Hospital.CountyJefferson                                        5.898 3.68e-09
## Hospital.CountyKings                                           -1.477 0.139678
## Hospital.CountyLewis                                            2.419 0.015557
## Hospital.CountyLivingston                                       5.077 3.83e-07
## Hospital.CountyMadison                                         -0.499 0.618059
## Hospital.CountyManhattan                                       -7.478 7.54e-14
## Hospital.CountyMonroe                                           6.274 3.52e-10
## Hospital.CountyMontgomery                                      -8.108 5.14e-16
## Hospital.CountyNassau                                           6.771 1.28e-11
## Hospital.CountyNiagara                                         -2.372 0.017679
## Hospital.CountyOneida                                           4.514 6.35e-06
## Hospital.CountyOnondaga                                         6.903 5.09e-12
## Hospital.CountyOntario                                          4.064 4.83e-05
## Hospital.CountyOrange                                         -16.425  < 2e-16
## Hospital.CountyOrleans                                         -1.945 0.051718
## Hospital.CountyOswego                                           1.336 0.181453
## Hospital.CountyOtsego                                          -3.857 0.000115
## Hospital.CountyPutnam                                         -15.970  < 2e-16
## Hospital.CountyQueens                                           2.131 0.033088
## Hospital.CountyRensselaer                                      -1.817 0.069188
## Hospital.CountyRichmond                                            NA       NA
## Hospital.CountyRockland                                        -8.327  < 2e-16
## Hospital.CountySaratoga                                         1.097 0.272515
## Hospital.CountySchenectady                                     -3.352 0.000802
## Hospital.CountySchuyler                                         4.649 3.34e-06
## Hospital.CountySt Lawrence                                     -0.057 0.954871
## Hospital.CountySteuben                                          2.064 0.039022
## Hospital.CountySuffolk                                             NA       NA
## Hospital.CountySullivan                                        -7.169 7.57e-13
## Hospital.CountyTompkins                                            NA       NA
## Hospital.CountyUlster                                          -6.638 3.19e-11
## Hospital.CountyWarren                                          -4.661 3.15e-06
## Hospital.CountyWayne                                            2.569 0.010207
## Hospital.CountyWestchester                                         NA       NA
## Hospital.CountyWyoming                                             NA       NA
## Hospital.CountyYates                                               NA       NA
##                                                                  
## (Intercept)                                                      
## Age.Group18 to 29                                             ***
## Age.Group30 to 49                                             ***
## Age.Group50 to 69                                             ***
## Age.Group70 or Older                                          ***
## GenderM                                                       ***
## GenderU                                                          
## RaceMulti-racial                                                 
## RaceOther Race                                                ***
## RaceWhite                                                     ***
## EthnicityNot Span/Hispanic                                       
## EthnicitySpanish/Hispanic                                     .  
## EthnicityUnknown                                                 
## Type.of.AdmissionEmergency                                    ***
## Type.of.AdmissionNewborn                                         
## Type.of.AdmissionNot Available                                *  
## Type.of.AdmissionTrauma                                       ** 
## Type.of.AdmissionUrgent                                       ***
## APR.MDC.Code2                                                 ** 
## APR.MDC.Code3                                                 ***
## APR.MDC.Code4                                                 ***
## APR.MDC.Code5                                                 ***
## APR.MDC.Code6                                                 *  
## APR.MDC.Code7                                                 ***
## APR.MDC.Code8                                                 .  
## APR.MDC.Code9                                                    
## APR.MDC.Code10                                                ***
## APR.MDC.Code11                                                ***
## APR.MDC.Code12                                                ***
## APR.MDC.Code13                                                ***
## APR.MDC.Code14                                                ***
## APR.MDC.Code15                                                *  
## APR.MDC.Code16                                                ***
## APR.MDC.Code17                                                ***
## APR.MDC.Code18                                                ***
## APR.MDC.Code19                                                ***
## APR.MDC.Code20                                                ***
## APR.MDC.Code21                                                ***
## APR.MDC.Code22                                                ***
## APR.MDC.Code23                                                ***
## APR.MDC.Code24                                                ***
## APR.MDC.Code25                                                ***
## APR.Severity.of.Illness.Code2                                 ***
## APR.Severity.of.Illness.Code3                                 ***
## APR.Severity.of.Illness.Code4                                 ***
## APR.Risk.of.MortalityMajor                                    ***
## APR.Risk.of.MortalityMinor                                    ***
## APR.Risk.of.MortalityModerate                                 ***
## Ownership.TypeMunicipality                                    ***
## Ownership.TypeNot for Profit Corporation                      ** 
## Ownership.TypePublic Benefit Corporation                      ***
## Ownership.TypeState                                           ** 
## Facility.TypePrimary Care Hospital - Critical Access Hospital *  
## Emergency.Department.IndicatorY                               ***
## Health.Service.AreaCentral NY                                 ***
## Health.Service.AreaFinger Lakes                               ***
## Health.Service.AreaHudson Valley                              ***
## Health.Service.AreaLong Island                                   
## Health.Service.AreaNew York City                              ***
## Health.Service.AreaSouthern Tier                              ***
## Health.Service.AreaWestern NY                                 *  
## Hospital.CountyAllegany                                       ** 
## Hospital.CountyBronx                                          *  
## Hospital.CountyBroome                                            
## Hospital.CountyCattaraugus                                    ** 
## Hospital.CountyCayuga                                            
## Hospital.CountyChautauqua                                     ** 
## Hospital.CountyChemung                                        ***
## Hospital.CountyChenango                                          
## Hospital.CountyClinton                                           
## Hospital.CountyColumbia                                          
## Hospital.CountyCortland                                       .  
## Hospital.CountyDelaware                                          
## Hospital.CountyDutchess                                       ***
## Hospital.CountyErie                                           ** 
## Hospital.CountyEssex                                             
## Hospital.CountyFranklin                                       ***
## Hospital.CountyFulton                                         ***
## Hospital.CountyGenesee                                        ** 
## Hospital.CountyHerkimer                                          
## Hospital.CountyJefferson                                      ***
## Hospital.CountyKings                                             
## Hospital.CountyLewis                                          *  
## Hospital.CountyLivingston                                     ***
## Hospital.CountyMadison                                           
## Hospital.CountyManhattan                                      ***
## Hospital.CountyMonroe                                         ***
## Hospital.CountyMontgomery                                     ***
## Hospital.CountyNassau                                         ***
## Hospital.CountyNiagara                                        *  
## Hospital.CountyOneida                                         ***
## Hospital.CountyOnondaga                                       ***
## Hospital.CountyOntario                                        ***
## Hospital.CountyOrange                                         ***
## Hospital.CountyOrleans                                        .  
## Hospital.CountyOswego                                            
## Hospital.CountyOtsego                                         ***
## Hospital.CountyPutnam                                         ***
## Hospital.CountyQueens                                         *  
## Hospital.CountyRensselaer                                     .  
## Hospital.CountyRichmond                                          
## Hospital.CountyRockland                                       ***
## Hospital.CountySaratoga                                          
## Hospital.CountySchenectady                                    ***
## Hospital.CountySchuyler                                       ***
## Hospital.CountySt Lawrence                                       
## Hospital.CountySteuben                                        *  
## Hospital.CountySuffolk                                           
## Hospital.CountySullivan                                       ***
## Hospital.CountyTompkins                                          
## Hospital.CountyUlster                                         ***
## Hospital.CountyWarren                                         ***
## Hospital.CountyWayne                                          *  
## Hospital.CountyWestchester                                       
## Hospital.CountyWyoming                                           
## Hospital.CountyYates                                             
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.35 on 648419 degrees of freedom
##   (4222 observations deleted due to missingness)
## Multiple R-squared:  0.2084,	Adjusted R-squared:  0.2082 
## F-statistic:  1580 on 108 and 648419 DF,  p-value: < 2.2e-16
```

Note that 7 coefficients are not defined due to singularities. The undefined factors are all from within the hospital county variable. We believe that this is due to the nested structure of the data. Hospital county can directly predict hospital service area because the hospital county variable is nested within the hospital service area. This is another reason that simple linear regression is not the best model for this data.

The $R^2$ value for the simple linear regression on the training data is 0.2094. 


```{.r .fold-show}
# Evaluate linear regression model
eval_LOS_linear <- evaluate_model(linearReg, test, "Length.of.Stay")
```

```
## Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels): factor Hospital.County has new levels Schoharie
```


### Poisson Model
When you look at thet possible values of length of stay, you see that length of stay can only be integer values greater than 1. Linear regression, on the other hand, will give you continuous values from -infinty to infinity. Therefore, our next step was to model length of stay as a Poisson regression which models count data. This still isn't quite the right model as a normal Poisson can take on values of 0. Since this is inpatient data, there will never be a patient who has a length of stay of 0.


```{.r .fold-show}
Poi <- glm(Length.of.Stay ~ Age.Group + Gender + Race + Ethnicity + Type.of.Admission + 
    APR.MDC.Code + APR.Severity.of.Illness.Code + APR.Risk.of.Mortality + Ownership.Type + 
    Facility.Type + Emergency.Department.Indicator + Health.Service.Area + Hospital.County, 
    family = Poisson(link = "log"), data = train)
summary(Poi)
```

```
## 
## Call:
## glm(formula = Length.of.Stay ~ Age.Group + Gender + Race + Ethnicity + 
##     Type.of.Admission + APR.MDC.Code + APR.Severity.of.Illness.Code + 
##     APR.Risk.of.Mortality + Ownership.Type + Facility.Type + 
##     Emergency.Department.Indicator + Health.Service.Area + Hospital.County, 
##     family = Poisson(link = "log"), data = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -9.8915  -1.3751  -0.5118   0.5406  26.0926  
## 
## Coefficients: (7 not defined because of singularities)
##                                                                 Estimate
## (Intercept)                                                    0.4444101
## Age.Group18 to 29                                              0.5397389
## Age.Group30 to 49                                              0.5365170
## Age.Group50 to 69                                              0.5185151
## Age.Group70 or Older                                           0.4466479
## GenderM                                                       -0.0090355
## GenderU                                                       -0.6560288
## RaceMulti-racial                                              -0.0080776
## RaceOther Race                                                -0.0765170
## RaceWhite                                                     -0.0614906
## EthnicityNot Span/Hispanic                                     0.0022092
## EthnicitySpanish/Hispanic                                     -0.0481027
## EthnicityUnknown                                              -0.0049547
## Type.of.AdmissionEmergency                                    -0.0982620
## Type.of.AdmissionNewborn                                      -0.1848193
## Type.of.AdmissionNot Available                                -0.1381166
## Type.of.AdmissionTrauma                                        0.0517409
## Type.of.AdmissionUrgent                                        0.0950448
## APR.MDC.Code2                                                 -0.1275565
## APR.MDC.Code3                                                 -0.1782863
## APR.MDC.Code4                                                 -0.0967873
## APR.MDC.Code5                                                 -0.1833800
## APR.MDC.Code6                                                 -0.0141416
## APR.MDC.Code7                                                 -0.0575784
## APR.MDC.Code8                                                  0.0007515
## APR.MDC.Code9                                                  0.0115550
## APR.MDC.Code10                                                -0.2209254
## APR.MDC.Code11                                                -0.1466371
## APR.MDC.Code12                                                -0.1837246
## APR.MDC.Code13                                                -0.1285797
## APR.MDC.Code14                                                -0.3588573
## APR.MDC.Code15                                                 0.6796965
## APR.MDC.Code16                                                -0.1156841
## APR.MDC.Code17                                                 0.2365491
## APR.MDC.Code18                                                -0.0845272
## APR.MDC.Code19                                                 1.1110520
## APR.MDC.Code20                                                 0.3472254
## APR.MDC.Code21                                                -0.2364962
## APR.MDC.Code22                                                 0.5494395
## APR.MDC.Code23                                                 0.5142560
## APR.MDC.Code24                                                -0.1462423
## APR.MDC.Code25                                                 0.1164006
## APR.Severity.of.Illness.Code2                                  0.2742267
## APR.Severity.of.Illness.Code3                                  0.6221151
## APR.Severity.of.Illness.Code4                                  1.1475987
## APR.Risk.of.MortalityMajor                                    -0.1553040
## APR.Risk.of.MortalityMinor                                    -0.4898741
## APR.Risk.of.MortalityModerate                                 -0.3428491
## Ownership.TypeMunicipality                                     1.0624215
## Ownership.TypeNot for Profit Corporation                       0.8796005
## Ownership.TypePublic Benefit Corporation                       1.1432462
## Ownership.TypeState                                            0.9258461
## Facility.TypePrimary Care Hospital - Critical Access Hospital -0.2241443
## Emergency.Department.IndicatorY                               -0.0417333
## Health.Service.AreaCentral NY                                 -0.2055981
## Health.Service.AreaFinger Lakes                               -0.4207467
## Health.Service.AreaHudson Valley                               0.0647761
## Health.Service.AreaLong Island                                 0.0035787
## Health.Service.AreaNew York City                               0.0568753
## Health.Service.AreaSouthern Tier                              -0.2917591
## Health.Service.AreaWestern NY                                  0.7570871
## Hospital.CountyAllegany                                       -0.9962236
## Hospital.CountyBronx                                           0.0165657
## Hospital.CountyBroome                                          0.1516738
## Hospital.CountyCattaraugus                                    -1.0390330
## Hospital.CountyCayuga                                         -0.0737648
## Hospital.CountyChautauqua                                     -0.9786234
## Hospital.CountyChemung                                         0.2868015
## Hospital.CountyChenango                                               NA
## Hospital.CountyClinton                                         0.0014055
## Hospital.CountyColumbia                                        0.0259961
## Hospital.CountyCortland                                       -0.0940943
## Hospital.CountyDelaware                                       -0.1006875
## Hospital.CountyDutchess                                       -0.2392646
## Hospital.CountyErie                                           -0.8837858
## Hospital.CountyEssex                                          -0.1649933
## Hospital.CountyFranklin                                       -0.3285908
## Hospital.CountyFulton                                         -0.2738251
## Hospital.CountyGenesee                                        -1.0279571
## Hospital.CountyHerkimer                                        0.1708348
## Hospital.CountyJefferson                                       0.2163962
## Hospital.CountyKings                                          -0.0166414
## Hospital.CountyLewis                                           0.8010861
## Hospital.CountyLivingston                                      0.3724824
## Hospital.CountyMadison                                        -0.1191523
## Hospital.CountyManhattan                                      -0.0722980
## Hospital.CountyMonroe                                          0.4405225
## Hospital.CountyMontgomery                                     -0.1850469
## Hospital.CountyNassau                                          0.0503028
## Hospital.CountyNiagara                                        -0.8012892
## Hospital.CountyOneida                                          0.1261274
## Hospital.CountyOnondaga                                        0.1800106
## Hospital.CountyOntario                                         0.2757171
## Hospital.CountyOrange                                         -0.1918725
## Hospital.CountyOrleans                                        -0.5766158
## Hospital.CountyOswego                                          0.0746063
## Hospital.CountyOtsego                                         -0.0764154
## Hospital.CountyPutnam                                         -0.3990061
## Hospital.CountyQueens                                          0.0168643
## Hospital.CountyRensselaer                                     -0.0379491
## Hospital.CountyRichmond                                               NA
## Hospital.CountyRockland                                       -0.0888174
## Hospital.CountySaratoga                                        0.0300342
## Hospital.CountySchenectady                                    -0.0794736
## Hospital.CountySchuyler                                        0.4530930
## Hospital.CountySt Lawrence                                    -0.0115467
## Hospital.CountySteuben                                         0.0246673
## Hospital.CountySuffolk                                                NA
## Hospital.CountySullivan                                       -0.2083734
## Hospital.CountyTompkins                                               NA
## Hospital.CountyUlster                                         -0.1221685
## Hospital.CountyWarren                                         -0.0821194
## Hospital.CountyWayne                                           0.1461198
## Hospital.CountyWestchester                                            NA
## Hospital.CountyWyoming                                                NA
## Hospital.CountyYates                                                  NA
##                                                               Std. Error
## (Intercept)                                                    0.1346568
## Age.Group18 to 29                                              0.0327408
## Age.Group30 to 49                                              0.0324455
## Age.Group50 to 69                                              0.0323956
## Age.Group70 or Older                                           0.0323962
## GenderM                                                        0.0009850
## GenderU                                                        0.2582264
## RaceMulti-racial                                               0.0054010
## RaceOther Race                                                 0.0018291
## RaceWhite                                                      0.0014326
## EthnicityNot Span/Hispanic                                     0.0100587
## EthnicitySpanish/Hispanic                                      0.0101984
## EthnicityUnknown                                               0.0103883
## Type.of.AdmissionEmergency                                     0.0022885
## Type.of.AdmissionNewborn                                       0.0694220
## Type.of.AdmissionNot Available                                 0.0214021
## Type.of.AdmissionTrauma                                        0.0100873
## Type.of.AdmissionUrgent                                        0.0023703
## APR.MDC.Code2                                                  0.0161095
## APR.MDC.Code3                                                  0.0056100
## APR.MDC.Code4                                                  0.0022719
## APR.MDC.Code5                                                  0.0021228
## APR.MDC.Code6                                                  0.0023708
## APR.MDC.Code7                                                  0.0034923
## APR.MDC.Code8                                                  0.0024325
## APR.MDC.Code9                                                  0.0035970
## APR.MDC.Code10                                                 0.0035865
## APR.MDC.Code11                                                 0.0026110
## APR.MDC.Code12                                                 0.0083914
## APR.MDC.Code13                                                 0.0075530
## APR.MDC.Code14                                                 0.0135024
## APR.MDC.Code15                                                 0.0815702
## APR.MDC.Code16                                                 0.0045972
## APR.MDC.Code17                                                 0.0044567
## APR.MDC.Code18                                                 0.0022830
## APR.MDC.Code19                                                 0.0026784
## APR.MDC.Code20                                                 0.0048510
## APR.MDC.Code21                                                 0.0055886
## APR.MDC.Code22                                                 0.0183232
## APR.MDC.Code23                                                 0.0030453
## APR.MDC.Code24                                                 0.0092540
## APR.MDC.Code25                                                 0.0102104
## APR.Severity.of.Illness.Code2                                  0.0019904
## APR.Severity.of.Illness.Code3                                  0.0023009
## APR.Severity.of.Illness.Code4                                  0.0028148
## APR.Risk.of.MortalityMajor                                     0.0018636
## APR.Risk.of.MortalityMinor                                     0.0026962
## APR.Risk.of.MortalityModerate                                  0.0022363
## Ownership.TypeMunicipality                                     0.1302434
## Ownership.TypeNot for Profit Corporation                       0.1302232
## Ownership.TypePublic Benefit Corporation                       0.1302714
## Ownership.TypeState                                            0.1302547
## Facility.TypePrimary Care Hospital - Critical Access Hospital  0.0179793
## Emergency.Department.IndicatorY                                0.0018879
## Health.Service.AreaCentral NY                                  0.0098622
## Health.Service.AreaFinger Lakes                                0.0342013
## Health.Service.AreaHudson Valley                               0.0034979
## Health.Service.AreaLong Island                                 0.0033553
## Health.Service.AreaNew York City                               0.0040773
## Health.Service.AreaSouthern Tier                               0.0204468
## Health.Service.AreaWestern NY                                  0.1311635
## Hospital.CountyAllegany                                        0.1323851
## Hospital.CountyBronx                                           0.0036105
## Hospital.CountyBroome                                          0.0207141
## Hospital.CountyCattaraugus                                     0.1315062
## Hospital.CountyCayuga                                          0.0138664
## Hospital.CountyChautauqua                                      0.1313462
## Hospital.CountyChemung                                         0.0345631
## Hospital.CountyChenango                                               NA
## Hospital.CountyClinton                                         0.0075409
## Hospital.CountyColumbia                                        0.0089484
## Hospital.CountyCortland                                        0.0154696
## Hospital.CountyDelaware                                        0.0357054
## Hospital.CountyDutchess                                        0.0045117
## Hospital.CountyErie                                            0.1311532
## Hospital.CountyEssex                                           0.0413021
## Hospital.CountyFranklin                                        0.0118104
## Hospital.CountyFulton                                          0.0156478
## Hospital.CountyGenesee                                         0.1317282
## Hospital.CountyHerkimer                                        0.0307412
## Hospital.CountyJefferson                                       0.0124006
## Hospital.CountyKings                                           0.0034341
## Hospital.CountyLewis                                           0.1352485
## Hospital.CountyLivingston                                      0.0382491
## Hospital.CountyMadison                                         0.0171552
## Hospital.CountyManhattan                                       0.0033152
## Hospital.CountyMonroe                                          0.0341555
## Hospital.CountyMontgomery                                      0.0087743
## Hospital.CountyNassau                                          0.0025208
## Hospital.CountyNiagara                                         0.1312435
## Hospital.CountyOneida                                          0.0103169
## Hospital.CountyOnondaga                                        0.0098822
## Hospital.CountyOntario                                         0.0347323
## Hospital.CountyOrange                                          0.0042673
## Hospital.CountyOrleans                                         0.1335704
## Hospital.CountyOswego                                          0.0139125
## Hospital.CountyOtsego                                          0.0067116
## Hospital.CountyPutnam                                          0.0092972
## Hospital.CountyQueens                                          0.0035353
## Hospital.CountyRensselaer                                      0.0067450
## Hospital.CountyRichmond                                               NA
## Hospital.CountyRockland                                        0.0046027
## Hospital.CountySaratoga                                        0.0073937
## Hospital.CountySchenectady                                     0.0053381
## Hospital.CountySchuyler                                        0.0402895
## Hospital.CountySt Lawrence                                     0.0123513
## Hospital.CountySteuben                                         0.0356452
## Hospital.CountySuffolk                                                NA
## Hospital.CountySullivan                                        0.0113967
## Hospital.CountyTompkins                                               NA
## Hospital.CountyUlster                                          0.0065396
## Hospital.CountyWarren                                          0.0062603
## Hospital.CountyWayne                                           0.0357087
## Hospital.CountyWestchester                                            NA
## Hospital.CountyWyoming                                                NA
## Hospital.CountyYates                                                  NA
##                                                                z value Pr(>|z|)
## (Intercept)                                                      3.300 0.000966
## Age.Group18 to 29                                               16.485  < 2e-16
## Age.Group30 to 49                                               16.536  < 2e-16
## Age.Group50 to 69                                               16.006  < 2e-16
## Age.Group70 or Older                                            13.787  < 2e-16
## GenderM                                                         -9.173  < 2e-16
## GenderU                                                         -2.541 0.011069
## RaceMulti-racial                                                -1.496 0.134766
## RaceOther Race                                                 -41.834  < 2e-16
## RaceWhite                                                      -42.924  < 2e-16
## EthnicityNot Span/Hispanic                                       0.220 0.826160
## EthnicitySpanish/Hispanic                                       -4.717 2.40e-06
## EthnicityUnknown                                                -0.477 0.633397
## Type.of.AdmissionEmergency                                     -42.937  < 2e-16
## Type.of.AdmissionNewborn                                        -2.662 0.007762
## Type.of.AdmissionNot Available                                  -6.453 1.09e-10
## Type.of.AdmissionTrauma                                          5.129 2.91e-07
## Type.of.AdmissionUrgent                                         40.099  < 2e-16
## APR.MDC.Code2                                                   -7.918 2.41e-15
## APR.MDC.Code3                                                  -31.780  < 2e-16
## APR.MDC.Code4                                                  -42.602  < 2e-16
## APR.MDC.Code5                                                  -86.385  < 2e-16
## APR.MDC.Code6                                                   -5.965 2.45e-09
## APR.MDC.Code7                                                  -16.487  < 2e-16
## APR.MDC.Code8                                                    0.309 0.757383
## APR.MDC.Code9                                                    3.212 0.001316
## APR.MDC.Code10                                                 -61.598  < 2e-16
## APR.MDC.Code11                                                 -56.161  < 2e-16
## APR.MDC.Code12                                                 -21.894  < 2e-16
## APR.MDC.Code13                                                 -17.024  < 2e-16
## APR.MDC.Code14                                                 -26.577  < 2e-16
## APR.MDC.Code15                                                   8.333  < 2e-16
## APR.MDC.Code16                                                 -25.164  < 2e-16
## APR.MDC.Code17                                                  53.077  < 2e-16
## APR.MDC.Code18                                                 -37.024  < 2e-16
## APR.MDC.Code19                                                 414.813  < 2e-16
## APR.MDC.Code20                                                  71.578  < 2e-16
## APR.MDC.Code21                                                 -42.318  < 2e-16
## APR.MDC.Code22                                                  29.986  < 2e-16
## APR.MDC.Code23                                                 168.868  < 2e-16
## APR.MDC.Code24                                                 -15.803  < 2e-16
## APR.MDC.Code25                                                  11.400  < 2e-16
## APR.Severity.of.Illness.Code2                                  137.775  < 2e-16
## APR.Severity.of.Illness.Code3                                  270.378  < 2e-16
## APR.Severity.of.Illness.Code4                                  407.702  < 2e-16
## APR.Risk.of.MortalityMajor                                     -83.337  < 2e-16
## APR.Risk.of.MortalityMinor                                    -181.687  < 2e-16
## APR.Risk.of.MortalityModerate                                 -153.314  < 2e-16
## Ownership.TypeMunicipality                                       8.157 3.43e-16
## Ownership.TypeNot for Profit Corporation                         6.755 1.43e-11
## Ownership.TypePublic Benefit Corporation                         8.776  < 2e-16
## Ownership.TypeState                                              7.108 1.18e-12
## Facility.TypePrimary Care Hospital - Critical Access Hospital  -12.467  < 2e-16
## Emergency.Department.IndicatorY                                -22.106  < 2e-16
## Health.Service.AreaCentral NY                                  -20.847  < 2e-16
## Health.Service.AreaFinger Lakes                                -12.302  < 2e-16
## Health.Service.AreaHudson Valley                                18.518  < 2e-16
## Health.Service.AreaLong Island                                   1.067 0.286162
## Health.Service.AreaNew York City                                13.949  < 2e-16
## Health.Service.AreaSouthern Tier                               -14.269  < 2e-16
## Health.Service.AreaWestern NY                                    5.772 7.83e-09
## Hospital.CountyAllegany                                         -7.525 5.26e-14
## Hospital.CountyBronx                                             4.588 4.47e-06
## Hospital.CountyBroome                                            7.322 2.44e-13
## Hospital.CountyCattaraugus                                      -7.901 2.77e-15
## Hospital.CountyCayuga                                           -5.320 1.04e-07
## Hospital.CountyChautauqua                                       -7.451 9.28e-14
## Hospital.CountyChemung                                           8.298  < 2e-16
## Hospital.CountyChenango                                             NA       NA
## Hospital.CountyClinton                                           0.186 0.852149
## Hospital.CountyColumbia                                          2.905 0.003671
## Hospital.CountyCortland                                         -6.083 1.18e-09
## Hospital.CountyDelaware                                         -2.820 0.004803
## Hospital.CountyDutchess                                        -53.031  < 2e-16
## Hospital.CountyErie                                             -6.739 1.60e-11
## Hospital.CountyEssex                                            -3.995 6.48e-05
## Hospital.CountyFranklin                                        -27.822  < 2e-16
## Hospital.CountyFulton                                          -17.499  < 2e-16
## Hospital.CountyGenesee                                          -7.804 6.02e-15
## Hospital.CountyHerkimer                                          5.557 2.74e-08
## Hospital.CountyJefferson                                        17.450  < 2e-16
## Hospital.CountyKings                                            -4.846 1.26e-06
## Hospital.CountyLewis                                             5.923 3.16e-09
## Hospital.CountyLivingston                                        9.738  < 2e-16
## Hospital.CountyMadison                                          -6.946 3.77e-12
## Hospital.CountyManhattan                                       -21.808  < 2e-16
## Hospital.CountyMonroe                                           12.898  < 2e-16
## Hospital.CountyMontgomery                                      -21.090  < 2e-16
## Hospital.CountyNassau                                           19.955  < 2e-16
## Hospital.CountyNiagara                                          -6.105 1.03e-09
## Hospital.CountyOneida                                           12.225  < 2e-16
## Hospital.CountyOnondaga                                         18.216  < 2e-16
## Hospital.CountyOntario                                           7.938 2.05e-15
## Hospital.CountyOrange                                          -44.963  < 2e-16
## Hospital.CountyOrleans                                          -4.317 1.58e-05
## Hospital.CountyOswego                                            5.363 8.21e-08
## Hospital.CountyOtsego                                          -11.386  < 2e-16
## Hospital.CountyPutnam                                          -42.917  < 2e-16
## Hospital.CountyQueens                                            4.770 1.84e-06
## Hospital.CountyRensselaer                                       -5.626 1.84e-08
## Hospital.CountyRichmond                                             NA       NA
## Hospital.CountyRockland                                        -19.297  < 2e-16
## Hospital.CountySaratoga                                          4.062 4.86e-05
## Hospital.CountySchenectady                                     -14.888  < 2e-16
## Hospital.CountySchuyler                                         11.246  < 2e-16
## Hospital.CountySt Lawrence                                      -0.935 0.349862
## Hospital.CountySteuben                                           0.692 0.488922
## Hospital.CountySuffolk                                              NA       NA
## Hospital.CountySullivan                                        -18.284  < 2e-16
## Hospital.CountyTompkins                                             NA       NA
## Hospital.CountyUlster                                          -18.681  < 2e-16
## Hospital.CountyWarren                                          -13.118  < 2e-16
## Hospital.CountyWayne                                             4.092 4.28e-05
## Hospital.CountyWestchester                                          NA       NA
## Hospital.CountyWyoming                                              NA       NA
## Hospital.CountyYates                                                NA       NA
##                                                                  
## (Intercept)                                                   ***
## Age.Group18 to 29                                             ***
## Age.Group30 to 49                                             ***
## Age.Group50 to 69                                             ***
## Age.Group70 or Older                                          ***
## GenderM                                                       ***
## GenderU                                                       *  
## RaceMulti-racial                                                 
## RaceOther Race                                                ***
## RaceWhite                                                     ***
## EthnicityNot Span/Hispanic                                       
## EthnicitySpanish/Hispanic                                     ***
## EthnicityUnknown                                                 
## Type.of.AdmissionEmergency                                    ***
## Type.of.AdmissionNewborn                                      ** 
## Type.of.AdmissionNot Available                                ***
## Type.of.AdmissionTrauma                                       ***
## Type.of.AdmissionUrgent                                       ***
## APR.MDC.Code2                                                 ***
## APR.MDC.Code3                                                 ***
## APR.MDC.Code4                                                 ***
## APR.MDC.Code5                                                 ***
## APR.MDC.Code6                                                 ***
## APR.MDC.Code7                                                 ***
## APR.MDC.Code8                                                    
## APR.MDC.Code9                                                 ** 
## APR.MDC.Code10                                                ***
## APR.MDC.Code11                                                ***
## APR.MDC.Code12                                                ***
## APR.MDC.Code13                                                ***
## APR.MDC.Code14                                                ***
## APR.MDC.Code15                                                ***
## APR.MDC.Code16                                                ***
## APR.MDC.Code17                                                ***
## APR.MDC.Code18                                                ***
## APR.MDC.Code19                                                ***
## APR.MDC.Code20                                                ***
## APR.MDC.Code21                                                ***
## APR.MDC.Code22                                                ***
## APR.MDC.Code23                                                ***
## APR.MDC.Code24                                                ***
## APR.MDC.Code25                                                ***
## APR.Severity.of.Illness.Code2                                 ***
## APR.Severity.of.Illness.Code3                                 ***
## APR.Severity.of.Illness.Code4                                 ***
## APR.Risk.of.MortalityMajor                                    ***
## APR.Risk.of.MortalityMinor                                    ***
## APR.Risk.of.MortalityModerate                                 ***
## Ownership.TypeMunicipality                                    ***
## Ownership.TypeNot for Profit Corporation                      ***
## Ownership.TypePublic Benefit Corporation                      ***
## Ownership.TypeState                                           ***
## Facility.TypePrimary Care Hospital - Critical Access Hospital ***
## Emergency.Department.IndicatorY                               ***
## Health.Service.AreaCentral NY                                 ***
## Health.Service.AreaFinger Lakes                               ***
## Health.Service.AreaHudson Valley                              ***
## Health.Service.AreaLong Island                                   
## Health.Service.AreaNew York City                              ***
## Health.Service.AreaSouthern Tier                              ***
## Health.Service.AreaWestern NY                                 ***
## Hospital.CountyAllegany                                       ***
## Hospital.CountyBronx                                          ***
## Hospital.CountyBroome                                         ***
## Hospital.CountyCattaraugus                                    ***
## Hospital.CountyCayuga                                         ***
## Hospital.CountyChautauqua                                     ***
## Hospital.CountyChemung                                        ***
## Hospital.CountyChenango                                          
## Hospital.CountyClinton                                           
## Hospital.CountyColumbia                                       ** 
## Hospital.CountyCortland                                       ***
## Hospital.CountyDelaware                                       ** 
## Hospital.CountyDutchess                                       ***
## Hospital.CountyErie                                           ***
## Hospital.CountyEssex                                          ***
## Hospital.CountyFranklin                                       ***
## Hospital.CountyFulton                                         ***
## Hospital.CountyGenesee                                        ***
## Hospital.CountyHerkimer                                       ***
## Hospital.CountyJefferson                                      ***
## Hospital.CountyKings                                          ***
## Hospital.CountyLewis                                          ***
## Hospital.CountyLivingston                                     ***
## Hospital.CountyMadison                                        ***
## Hospital.CountyManhattan                                      ***
## Hospital.CountyMonroe                                         ***
## Hospital.CountyMontgomery                                     ***
## Hospital.CountyNassau                                         ***
## Hospital.CountyNiagara                                        ***
## Hospital.CountyOneida                                         ***
## Hospital.CountyOnondaga                                       ***
## Hospital.CountyOntario                                        ***
## Hospital.CountyOrange                                         ***
## Hospital.CountyOrleans                                        ***
## Hospital.CountyOswego                                         ***
## Hospital.CountyOtsego                                         ***
## Hospital.CountyPutnam                                         ***
## Hospital.CountyQueens                                         ***
## Hospital.CountyRensselaer                                     ***
## Hospital.CountyRichmond                                          
## Hospital.CountyRockland                                       ***
## Hospital.CountySaratoga                                       ***
## Hospital.CountySchenectady                                    ***
## Hospital.CountySchuyler                                       ***
## Hospital.CountySt Lawrence                                       
## Hospital.CountySteuben                                           
## Hospital.CountySuffolk                                           
## Hospital.CountySullivan                                       ***
## Hospital.CountyTompkins                                          
## Hospital.CountyUlster                                         ***
## Hospital.CountyWarren                                         ***
## Hospital.CountyWayne                                          ***
## Hospital.CountyWestchester                                       
## Hospital.CountyWyoming                                           
## Hospital.CountyYates                                             
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 3787026  on 648527  degrees of freedom
## Residual deviance: 2560627  on 648419  degrees of freedom
##   (4222 observations deleted due to missingness)
## AIC: 4743325
## 
## Number of Fisher Scoring iterations: 5
```

Note, the same hospital county coefficients were not estimated in using the Poisson model either. 


```{.r .fold-show}
# Evaluate Poisson model
eval_LOS_Poi <- evaluate_model(Poi, test, "Length.of.Stay")
```

```
## Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels): factor Hospital.County has new levels Schoharie
```


### Zero-Truncated Poisson Model
Next we look at the Zero-Truncated Poisson model. This is a modification of the Poisson model, and it can only take integer values from 1 to infinity. Therfore, this model is more appropriate to model length of stay because it will never take 0 values.


```{.r .fold-show}
# GLM with zero-truncated Poisson
ZeroTruncated_Poi <- glm(Length.of.Stay ~ Age.Group + Gender + Race + Ethnicity + 
    Type.of.Admission + APR.MDC.Code + APR.Severity.of.Illness.Code + APR.Risk.of.Mortality + 
    Ownership.Type + Facility.Type + Emergency.Department.Indicator + Health.Service.Area + 
    Hospital.County, family = Tpoisson(link = "log"), data = train)
summary(ZeroTruncated_Poi)
```


```{.r .fold-show}
# Evaluate Zero-Truncated Poisson model
eval_LOS_ZeroTruncPoi <- evaluate_model(ZeroTruncated_Poi, test, "Length.of.Stay")
```

```
## Error in predict(model, test): object 'ZeroTruncated_Poi' not found
```


### Hierarchical Zero-Truncated Poisson Model
Lastly, we fit a hierarchical zero-truncted poisson model. If you look at the variables for Health Serivce Area and Hospital County, you can see that these are nested varaibles. For example, specific hospital counties will only occur in specific health areas. By modeling length of stay as a multi-level model, we can take into account the area-level and county-level effects on length of stay by accounting for unexplained variation in hospital service areas and hospital counties. 


```{.r .fold-show}
# GLM with zero-truncated Poisson, nesting
Hierarchical_LOS <- HLfit(Length.of.Stay ~ Age.Group
                + Gender
                + Race
                + Ethnicity
                + Type.of.Admission
                + APR.MDC.Code
                + APR.Severity.of.Illness.Code
                + APR.Risk.of.Mortality
                + Ownership.Type
                + Facility.Type
                + Emergency.Department.Indicator
		       #     + Health.Service.Area
		            + (1 | Hospital.County / Health.Service.Area),
	              family = Tpoisson(link = 'log'), 
	              data = train)
summary(Hierarchical_LOS, details=TRUE, max.print=99999999)
```


```{.r .fold-show}
# Evaluate Hierarchical LOS model
eval_LOS_Hier <- evaluate_model(Hierarchical_LOS, test, "Length.of.Stay")
```

```
## Error in predict(model, test): object 'Hierarchical_LOS' not found
```

## Cost per Day

### Linear Regression
In predicting cost per day, we focused on comparing simple linear regression results to the results for a hierarchical linear model accounting for the variance in serivce area and hospital county. We predicted that we would see an improvement in performance by using a more complicated and computationally heavy approach using the hierarchical structure. 


```{.r .fold-show}
# Linear regression for cost per day
linearReg_Cost <- lm(Cost.per.Day ~ Age.Group + Gender + Race + Ethnicity + Type.of.Admission + 
    APR.MDC.Code + APR.Severity.of.Illness.Code + APR.Risk.of.Mortality + Ownership.Type + 
    Facility.Type + Emergency.Department.Indicator + Health.Service.Area + Hospital.County, 
    data = train)
summary(linearReg_Cost)
```

7 coefficients were not estimated due to singularities similarly to the results from the length of stay analysis. 

The output shows a $R^2$ of 0.2268 on the training data. 


```{.r .fold-show}
# Evaluate Linear regression cost per day model
eval_Cost_linear <- evaluate_model(linearReg_Cost, test, "Cost.per.Day")
```

### Hierarchical Linear Regression


```{.r .fold-show}
Hierarchical_cost <- lmer(Cost.per.Day ~  Age.Group
                + Gender
                + Race
                + Ethnicity
                + Type.of.Admission
                + APR.MDC.Code
                + APR.Severity.of.Illness.Code
                + APR.Risk.of.Mortality
                + Ownership.Type
                + Facility.Type
                + Emergency.Department.Indicator
              #  + Health.Service.Area
                + (1 | Hospital.County / Health.Service.Area), 
                data = train)

summary(Hierarchical_cost)
```


```{.r .fold-show}
# Evaluation Hierarchical model for cost per day
eval_Cost_Hier <- evaluate_model(Hierarchical_cost, test, "Cost.per.Day")
```

## Model Comparison

```{.r .fold-show}
compare <- data.frame(matrix(, nrow = 3, ncol = 3))
colnames(compare) <- c("", "Linear Regression", "Hierarchical Linear Regression")

# Factor
compare[1, 1] <- "Mean Absolute Error"
compare[2, 1] <- "Mean Relative Error"
compare[3, 1] <- "Root Mean Squared Error"

# Linear Regression
compare[1, 2] <- eval_Cost_linear[1]
compare[2, 2] <- eval_Cost_linear[2]
compare[3, 2] <- eval_Cost_linear[3]

# Hierarchical Linear Regression
compare[1, 3] <- eval_Cost_Hier[1]
compare[2, 3] <- eval_Cost_Hier[2]
compare[3, 3] <- eval_Cost_Hier[3]

# Print table
report_table(compare)
```

The evaluation metrics between the two methods are very similar indicating that there wasn't much to gain in performance by using the more computationally intensive method. We believe this could perhaps be the case because there is likely not much difference in cost between hospitals, especially between hospitals in the same general area. However, the reward you do get for running the hierarchical model is that it better represents the structure of the data, and it knows how to treat the nested variables. Since linear regression does not have the knowledge of the correlation between the nested variables, it ends up not being able to estimate certain coefficients and you get the warning "prediction from a rank-deficient fit may be misleading" when trying to predict using the linear regression model. Thus, it is better to take into account the hierarchical structure of the results to be able to trust the results of your predictions.


# Bibliography

