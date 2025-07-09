# load packages
library(tidyverse)
library(labelled)

### Manually request a data extract from IPUMS (https://cps.ipums.org/cps/index.shtml)

## Samples
# IPUMS-CPS, ASEC 2011	
# IPUMS-CPS, ASEC 2012	
# IPUMS-CPS, ASEC 2013	
# IPUMS-CPS, ASEC 2014	
# IPUMS-CPS, ASEC 2015	
# IPUMS-CPS, ASEC 2016	
# IPUMS-CPS, ASEC 2017	
# IPUMS-CPS, ASEC 2018	
#
## Variables
# Type	Variable	Label
# H	YEAR	Survey year
# H	SERIAL	Household serial number
# H	MONTH	Month
# H	CPSID	CPSID, household record
# H	ASECFLAG	Flag for ASEC
# H	HFLAG	Flag for the 3/8 file 2014
# H	ASECWTH	Annual Social and Economic Supplement Household weight
# P	PERNUM	Person number in sample unit
# P	CPSIDP	CPSID, person record
# P	ASECWT	Annual Social and Economic Supplement Weight
# P	AGE	Age
# P	EMPSTAT	Employment status
# P	OCC2010	Occupation, 2010 basis
# P	AHRSWORKT	Hours worked last week
# P	HEALTH	Health status

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("raw/cps_00023.xml") # replace file name by the xml file of your data extract
data <- read_ipums_micro(ddi)

### age groups
data <- data %>% mutate(age_group = case_when(
  AGE<16 ~ 1, AGE>=16 & AGE<=17 ~ 2, AGE>=18 & AGE<=19 ~ 3,
  AGE>=20 & AGE<=24 ~ 4, AGE>=25 & AGE<=34 ~ 5, AGE>=35 & AGE<=44 ~ 6,
  AGE>=45 & AGE<=54 ~ 7, AGE>=55 & AGE<=64 ~ 8, AGE>=65 ~ 9, TRUE ~ 0))

### employment status
data <- data %>% mutate(emp=(EMPSTAT %in% c("10","12")))

### occupation
data <- data %>% mutate(occ2_cps = trunc(OCC2010/100))
data <- data %>% mutate(occ_code = case_when(
  occ2_cps<=4 ~ 11,
  occ2_cps<=9 ~ 13,
  occ2_cps<=12 ~ 15,
  occ2_cps<=15 ~ 17,
  occ2_cps<=19 ~ 19,
  occ2_cps<=20 ~ 21,
  occ2_cps<=21 ~ 23,
  occ2_cps<=25 ~ 25,
  occ2_cps<=29 ~ 27,
  occ2_cps<=35 ~ 29,
  occ2_cps<=36 ~ 31,
  occ2_cps<=39 ~ 33,
  occ2_cps<=41 ~ 35,
  occ2_cps<=42 ~ 37,
  occ2_cps<=46 ~ 39,
  occ2_cps<=49 ~ 41,
  occ2_cps<=59 ~ 43,
  occ2_cps<=61 ~ 45,
  occ2_cps<=69 ~ 47,
  occ2_cps<=76 ~ 49,
  occ2_cps<=89 ~ 51,
  occ2_cps<=97 ~ 53,
  TRUE ~ 55)
)
data <- data %>% mutate(occ_code=as.character(occ_code)) %>% dplyr::select(-occ2_cps)

### sample weights
data <- data %>% mutate(
  wgt = ASECWTH,
  ftewgt = AHRSWORKT/40*wgt)

### save data
data <- data %>% rename(year=YEAR)
data <- remove_labels(data)
saveRDS(data,file = "data/ASEC.RDS")
