# load packages
library(tidyverse)
library(labelled)

### Manually request a data extract from IPUMS (https://cps.ipums.org/cps/index.shtml)

## Samples
# IPUMS-CPS, January 2011	
# IPUMS-CPS, February 2011	
# IPUMS-CPS, March 2011	
# IPUMS-CPS, April 2011	
# IPUMS-CPS, May 2011	
# IPUMS-CPS, June 2011	
# IPUMS-CPS, July 2011	
# IPUMS-CPS, August 2011	
# IPUMS-CPS, September 2011	
# IPUMS-CPS, October 2011	
# IPUMS-CPS, November 2011	
# IPUMS-CPS, December 2011	
# IPUMS-CPS, January 2012	
# IPUMS-CPS, February 2012	
# IPUMS-CPS, March 2012	
# IPUMS-CPS, April 2012	
# IPUMS-CPS, May 2012	
# IPUMS-CPS, June 2012	
# IPUMS-CPS, July 2012	
# IPUMS-CPS, August 2012	
# IPUMS-CPS, September 2012	
# IPUMS-CPS, October 2012	
# IPUMS-CPS, November 2012	
# IPUMS-CPS, December 2012	
# IPUMS-CPS, January 2013	
# IPUMS-CPS, February 2013	
# IPUMS-CPS, March 2013	
# IPUMS-CPS, April 2013	
# IPUMS-CPS, May 2013	
# IPUMS-CPS, June 2013	
# IPUMS-CPS, July 2013	
# IPUMS-CPS, August 2013	
# IPUMS-CPS, September 2013	
# IPUMS-CPS, October 2013	
# IPUMS-CPS, November 2013	
# IPUMS-CPS, December 2013	
# IPUMS-CPS, January 2014	
# IPUMS-CPS, February 2014	
# IPUMS-CPS, March 2014	
# IPUMS-CPS, April 2014	
# IPUMS-CPS, May 2014	
# IPUMS-CPS, June 2014	
# IPUMS-CPS, July 2014	
# IPUMS-CPS, August 2014	
# IPUMS-CPS, September 2014	
# IPUMS-CPS, October 2014	
# IPUMS-CPS, November 2014	
# IPUMS-CPS, December 2014	
# IPUMS-CPS, January 2015	
# IPUMS-CPS, February 2015	
# IPUMS-CPS, March 2015	
# IPUMS-CPS, April 2015	
# IPUMS-CPS, May 2015	
# IPUMS-CPS, June 2015	
# IPUMS-CPS, July 2015	
# IPUMS-CPS, August 2015	
# IPUMS-CPS, September 2015	
# IPUMS-CPS, October 2015	
# IPUMS-CPS, November 2015	
# IPUMS-CPS, December 2015	
# IPUMS-CPS, January 2016	
# IPUMS-CPS, February 2016	
# IPUMS-CPS, March 2016	
# IPUMS-CPS, April 2016	
# IPUMS-CPS, May 2016	
# IPUMS-CPS, June 2016	
# IPUMS-CPS, July 2016	
# IPUMS-CPS, August 2016	
# IPUMS-CPS, September 2016	
# IPUMS-CPS, October 2016	
# IPUMS-CPS, November 2016	
# IPUMS-CPS, December 2016	
# IPUMS-CPS, January 2017	
# IPUMS-CPS, February 2017	
# IPUMS-CPS, March 2017	
# IPUMS-CPS, April 2017	
# IPUMS-CPS, May 2017	
# IPUMS-CPS, June 2017	
# IPUMS-CPS, July 2017	
# IPUMS-CPS, August 2017	
# IPUMS-CPS, September 2017	
# IPUMS-CPS, October 2017	
# IPUMS-CPS, November 2017	
# IPUMS-CPS, December 2017	
# IPUMS-CPS, January 2018	
# IPUMS-CPS, February 2018	
# IPUMS-CPS, March 2018	
# IPUMS-CPS, April 2018	
# IPUMS-CPS, May 2018	
# IPUMS-CPS, June 2018	
# IPUMS-CPS, July 2018	
# IPUMS-CPS, August 2018	
# IPUMS-CPS, September 2018	
# IPUMS-CPS, October 2018	
# IPUMS-CPS, November 2018	
# IPUMS-CPS, December 2018
#
## Variables
# Type	Variable	Label
# H	YEAR	Survey year
# H	SERIAL	Household serial number
# H	MONTH	Month
# H	HWTFINL	Household weight, Basic Monthly
# H	CPSID	CPSID, household record
# H	ASECFLAG	Flag for ASEC
# P	PERNUM	Person number in sample unit
# P	WTFINL	Final Basic Weight
# P	CPSIDP	CPSID, person record
# P	AGE	Age
# P	SEX	Sex
# P	RACE	Race
# P	HISPAN	Hispanic origin
# P	EMPSTAT	Employment status
# P	OCC2010	Occupation, 2010 basis
# P	CLASSWKR	Class of worker
# P	AHRSWORKT	Hours worked last week
# P	EDUC	Educational attainment recode

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("raw/cps_00022.xml")  # replace file name by the xml file of your data extract
data <- read_ipums_micro(ddi)

### age groups
data <- data %>% mutate(age_group = case_when(
  AGE<16 ~ 1, AGE>=16 & AGE<=17 ~ 2, AGE>=18 & AGE<=19 ~ 3,
  AGE>=20 & AGE<=24 ~ 4, AGE>=25 & AGE<=34 ~ 5, AGE>=35 & AGE<=44 ~ 6,
  AGE>=45 & AGE<=54 ~ 7, AGE>=55 & AGE<=64 ~ 8, AGE>=65 ~ 9, TRUE ~ 0))

### employment status
data <- data %>% mutate(emp=(EMPSTAT %in% c("10","12")))

### hours worked
data <- data %>% mutate(AHRSWORKT = case_when(!emp ~ NA, as.character(EMPSTAT)=="12" ~ 0, TRUE ~ as.numeric(AHRSWORKT)))

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
  wgt = WTFINL/12,
  ftewgt = AHRSWORKT/40*wgt)

### save prepared data
data <- data %>% rename(year=YEAR)
data <- remove_labels(data)
saveRDS(data,file = "data/CPS.RDS")

