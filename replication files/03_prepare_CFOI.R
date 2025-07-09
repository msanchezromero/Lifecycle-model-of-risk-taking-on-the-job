# load packages
library(tidyverse)

# manually download the following files in a "raw"-subfolder of the working directory:
# https://download.bls.gov/pub/time.series/fw/fw.data.1.AllData --> (about 600 MB!)
# https://download.bls.gov/pub/time.series/fw/fw.occupation

fw_raw <- read_delim("raw/fw.data.1.AllData", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
# parse series_id
fw <- fw_raw %>% mutate(
  survey = str_sub(series_id,1,2),
  seasonal = str_sub(series_id,3,3),
  category_code = str_sub(series_id,4,6),
  detail_code = str_sub(series_id,7,12),
  datatype_code = str_sub(series_id,13,13),
  case_code = str_sub(series_id,14,14),
  area_code = str_sub(series_id,15,17),
)
# load occupation classifiers
occinfo <- read.table("raw/fw.occupation",header=TRUE,sep="\t")

## A. select the sample with detailed occupational information
##    case_code = O ... Fatalities by detailed occupation (all sectors)
##    area_code = N00 ... All US
##    Categories specifying the age groups:
fw_sel <- fw %>% filter(case_code=="O", area_code=="N00")
category_list <- c("AAX","ABX","ACX","ADX","AEX","AFX","AGX","AHX","AIX")

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
fw_age <- tibble()

for (age in seq(1,length(category_list))) {

  fw_occ_age <- fw_sel %>% filter(category_code==category_list[age]) %>% dplyr::select(detail_code,year,value)
  
  ## B. the data is in some years missing some aggregation levels
  ##    need to introduce them manually to no miss observations
  
  # construct 2, 3, 5 digit occupational codes
  fw_occ_age <- fw_occ_age %>% mutate(occ2 = substr(detail_code,1,2), occ3 = substr(detail_code,1,3), occ4 = substr(detail_code,1,4), occ5 = substr(detail_code,1,5))
  # number of digits in occupation classification (ignore the warnings here!!)
  fw_occ_age <- fw_occ_age %>% mutate(digits = case_when(as.numeric(detail_code)==0 ~ 0, !is.na(as.numeric(detail_code)) ~ 6, !is.na(as.numeric(occ5)) ~ 5, !is.na(as.numeric(occ4)) ~ 4, !is.na(as.numeric(occ3)) ~ 3, TRUE ~ 2))
  
  # delete all 4 digit (exist only in one industry with few deaths, would just complicate the analysis)
  fw_occ_age <- fw_occ_age %>% filter(!digits==4)
  
  # check aggregation from 6 digit to 5 digit
  fw_occ_age6 <- fw_occ_age %>% filter(digits==6)
  fw_occ_age5 <- fw_occ_age %>% filter(digits==5)
  join56 <- left_join(fw_occ_age6,fw_occ_age5,by=c("year","occ5"), suffix = c("",".y"))
  nomatch56 <- join56 %>% filter(is.na(value.y))
  # aggregate the 6 digit entries missing their parent category to the 5 digit level
  nomatch56 <- nomatch56 %>% dplyr::select(!ends_with(".y"))
  nomatch56 <- nomatch56 %>% mutate(detail_code = paste0(occ5,"X")) %>% group_by(year,detail_code,occ2,occ3,occ5) %>% summarize(value=sum(value)) %>% mutate(digits=5)
  # append the rows to the data
  cat(paste0("appending ",nrow(nomatch56)," 5 digit occupations\n"))
  fw_occ_age <- bind_rows(fw_occ_age,nomatch56)
  
  # check aggregation from 5 digit to 3 digit
  fw_occ_age5 <- fw_occ_age %>% filter(digits==5)
  fw_occ_age3 <- fw_occ_age %>% filter(digits==3)
  join35 <- left_join(fw_occ_age5,fw_occ_age3,by=c("year","occ3"), suffix = c("",".y"))
  nomatch35 <- join35 %>% filter(is.na(value.y))
  # aggregate the 5 digit entries missing their parent category to the 3 digit level
  nomatch35 <- nomatch35 %>% dplyr::select(!ends_with(".y"))
  nomatch35 <- nomatch35 %>% mutate(detail_code = paste0(occ3,"XXX")) %>% group_by(year,detail_code,occ2,occ3) %>% summarize(value=sum(value)) %>% mutate(digits=3)
  # append the rows to the data
  cat(paste0("appending ",nrow(nomatch35)," 3 digit occupations\n"))
  fw_occ_age <- bind_rows(fw_occ_age,nomatch35)
  
  # check aggregation from 3 digit to 2 digit
  fw_occ_age3 <- fw_occ_age %>% filter(digits==3)
  fw_occ_age2 <- fw_occ_age %>% filter(digits==2)
  join23 <- left_join(fw_occ_age3,fw_occ_age2,by=c("year","occ2"), suffix = c("",".y"))
  nomatch23 <- join23 %>% filter(is.na(value.y))
  # aggregate the 3 digit entries missing their parent category to the 2 digit level
  nomatch23 <- nomatch23 %>% dplyr::select(!ends_with(".y"))
  nomatch23 <- nomatch23 %>% mutate(detail_code = paste0(occ2,"XXXX")) %>% group_by(year,detail_code,occ2) %>% summarize(value=sum(value)) %>% mutate(digits=2)
  # append the rows to the data
  cat(paste0("appending ",nrow(nomatch23)," 2 digit occupations\n"))
  fw_occ_age <- bind_rows(fw_occ_age,nomatch23)
  
  # add occupation info 
  fw_occ_age <- fw_occ_age %>% left_join(occinfo, by = c("detail_code" = "occupation_code"))
  fw_occ_age <- fw_occ_age %>% mutate(occ_code=substr(detail_code,1,pmax(digits,1))) %>% rename(occ_text = occupation_text)
  fw_occ_age <- fw_occ_age %>% ungroup() %>% dplyr::select(-c(selectable,sort_sequence,display_level,detail_code))
  fw_occ_age <- fw_occ_age %>% dplyr::select(order(colnames(fw_occ_age)))
  
  ## put into parent data frame
  fw_occ_age <- fw_occ_age %>% mutate(age_group = age)
  fw_age <- bind_rows(fw_age,fw_occ_age)
  
}

## D. Save the prepared data
saveRDS(fw_age,file="data/CFOI.RDS")
