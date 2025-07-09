# load packages
library(tidyverse)
library(MASS)
library(stargazer)
library(sandwich)
library(ggplot2)

################################
### DATA PREPARATION
################################

### prepare CPS monthly data
dataCPS <- readRDS("data/CPS.RDS")

# create dummy variables for regression
dataCPS <- dataCPS %>% mutate(
  male  = (as.character(SEX)=="1"),
  hispanic  = !(as.character(HISPAN) %in% c("0","901","902")),
  white = (as.character(RACE)=="100" & !hispanic),
  black = (as.character(RACE)=="200" & !hispanic),
  asian = (substr(as.character(RACE),1,2)=="65"& !hispanic),
  highschool = (as.numeric(EDUC)>=73 & as.numeric(EDUC)<999),
  college = (as.numeric(EDUC)>=90 & as.numeric(EDUC)<999),
  selfemp = (substr(as.character(CLASSWKR),1,1)=="1")
)

# aggregate to cells
CPS_agg <- dataCPS %>%
  filter(emp & age_group>=4 & age_group<=8) %>%
  group_by(age_group,occ_code,year) %>%
  summarize(
    cases_emp = n(),
    emp = sum(wgt),
    fte = sum(ftewgt),
    sharemale = sum(ftewgt*male)/fte,
    sharewhite = sum(ftewgt*white)/fte,
    shareblack = sum(ftewgt*black)/fte,
    shareasian = sum(ftewgt*asian)/fte,
    sharehispanic = sum(ftewgt*hispanic)/fte,
    shareselfemp = sum(ftewgt*selfemp)/fte,
    sharehighschool = sum(ftewgt*highschool)/fte,
    sharecollege = sum(ftewgt*college)/fte)

### prepare CPS ASEC data
dataASEC <- readRDS("data/ASEC.RDS")

# create dummy variables for regression
dataASEC <- dataASEC %>% mutate(
  health1 = (HEALTH==1),
  health2 = (HEALTH==2),
  health3 = (HEALTH==3),
  health4 = (HEALTH==4),
  health5 = (HEALTH==5)  
)

# aggregate to cells
ASEC_agg <- dataASEC %>%
  filter(emp & age_group>=4 & age_group<=8) %>%
  group_by(age_group,occ_code,year) %>%
  summarize(
    cases_emp = n(),
    emp = sum(wgt),
    fte = sum(ftewgt),
    share_health1 = sum(ftewgt*health1)/fte,
    share_health2 = sum(ftewgt*health2)/fte,
    share_health3 = sum(ftewgt*health3)/fte,
    share_health4 = sum(ftewgt*health4)/fte,
    share_health5 = sum(ftewgt*health5)/fte)

### prepare CFOI data
dataCFOI <- readRDS("data/CFOI.RDS")
CFOI_agg <- dataCFOI %>%
  filter(age_group>=4 & age_group<=8 & occ_code<55 & year<=2018 & digits==2) %>%
  group_by(age_group,occ_code,year) %>%
  summarize(deaths=sum(value))

### join data
combined <- left_join(CPS_agg, ASEC_agg, by=join_by(age_group, occ_code, year), suffix=c("",".ASEC"))
combined <- full_join(combined, CFOI_agg) %>%
  mutate(
    deaths = tidyr::replace_na(deaths,0),
    rate = deaths/fte*10^5)


################################
### DESCRIPTIVE STATISTICS (Table A1)
################################

# Column 2: Population-level statistics

# 1. Deaths and FTE
pop_sums <- combined %>% ungroup() %>% summarise(deaths=sum(deaths),fte=sum(fte)/10^5)

# 2. Employment shares
emp_shares <- dataCPS %>%
  filter(emp & age_group>=4 & age_group<=8) %>%
  summarize(
    fte = sum(ftewgt),
    sharemale = sum(ftewgt*male)/fte,
    sharewhite = sum(ftewgt*white)/fte,
    shareblack = sum(ftewgt*black)/fte,
    shareasian = sum(ftewgt*asian)/fte,
    sharehispanic = sum(ftewgt*hispanic)/fte,
    shareselfemp = sum(ftewgt*selfemp)/fte,
    sharehighschool = sum(ftewgt*highschool)/fte,
    sharecollege = sum(ftewgt*college)/fte) %>% dplyr::select(-fte)

# 3. Health shares
health_shares <- dataASEC %>%
  filter(emp & age_group>=4 & age_group<=8) %>%
  summarize(
    fte = sum(ftewgt),
    share_health1 = sum(ftewgt*health1)/fte,
    share_health2 = sum(ftewgt*health2)/fte,
    share_health3 = sum(ftewgt*health3)/fte,
    share_health4 = sum(ftewgt*health4)/fte,
    share_health5 = sum(ftewgt*health5)/fte) %>% dplyr::select(-fte)

# 4. Combine
popstats <- bind_cols(pop_sums,emp_shares,health_shares)
popstats <- round(t(popstats),3)
colnames(popstats) <- "Population"

# Columns 3-7: Cell-level statistics

excerpt <- combined %>% ungroup() %>% dplyr::select(deaths,fte,sharemale,sharewhite,shareblack,shareasian,sharehispanic,shareselfemp,sharehighschool,sharecollege,contains("share_health"))
means <- excerpt %>% summarise(across(everything(), ~ mean(.x,na.rm=TRUE)))
sds <- excerpt %>% summarise(across(everything(), ~ sd(.x,na.rm=TRUE)))
mins <- excerpt %>% summarise(across(everything(), ~ min(.x,na.rm=TRUE)))
maxs <- excerpt %>% summarise(across(everything(), ~ max(.x,na.rm=TRUE)))
cellstats <- bind_rows(means,sds,mins,maxs)
cellstats <- cellstats %>% mutate(fte=fte/10^5)
cellstats <- as.data.frame(t(cellstats))
colnames(cellstats) <- c("Mean","S.D.","Min","Max")
cellstats <- cellstats %>% mutate(Mean=round(Mean,3),`S.D.`=round(`S.D.`,3),Min=round(Min,2),Max=round(Max,2))

# print table
TableA1 <- bind_cols(popstats,cellstats)
print(TableA1)


################################
### REGRESSON TABLES (Table A2)
################################

# Set age group 35-44 as baseline
combined_reg <- combined %>% mutate(age_group = ifelse(age_group==6,0,age_group))
combined_reg <- combined_reg %>% mutate(age_group=as.factor(age_group))

# Model specifications
fm0a <- deaths ~ age_group + offset(log(fte)) # (1)
fm1a <- deaths ~ age_group + occ_code + offset(log(fte)) # (2)
fm2a <- deaths ~ age_group + occ_code + sharemale + sharewhite + shareblack + shareasian + sharehispanic + sharehighschool + sharecollege + shareselfemp + offset(log(fte)) # (3)
fm3a <- deaths ~ age_group + occ_code + sharemale + sharewhite + shareblack + shareasian + sharehispanic + sharehighschool + sharecollege + shareselfemp + share_health2 + share_health3 + share_health4 + share_health5 + offset(log(fte)) #
                                                                                                                                                                                                              
# Fit models and obtain robust standard errors
m0a <- glm(fm0a, data=combined_reg, family="poisson")
m0a_se <- sqrt(diag(vcovHC(m0a)))
m1a <- glm(fm1a, data=combined_reg, family="poisson")
m1a_se <- sqrt(diag(vcovHC(m1a)))
m2a <- glm(fm2a, data=combined_reg, family="poisson")
m2a_se <- sqrt(diag(vcovHC(m2a)))
m3a <- glm(fm3a, data=combined_reg, family="poisson")
m3a_se <- sqrt(diag(vcovHC(m3a)))

# Regression table
TableA2 <- stargazer(m0a,m1a,m2a,m3a,omit=c("occ_code"),type="text",report="vc*s",digits=3,single.row=FALSE,coef = NULL, se = list(m0a_se,m1a_se,m2a_se,m3a_se),t.auto=TRUE,p.auto=TRUE,omit.stat=c("ll","theta","aic"))


################################
### PLOTS (Figure 2)
################################

# Define regression sample
combined_plot <- combined %>% mutate(age_group=as.factor(age_group))

# Model specifications
fm0b <- deaths ~ age_group + offset(log(fte)) - 1 # (1)
fm1b <- deaths ~ age_group + occ_code + offset(log(fte)) - 1 # (2)
fm2b <- deaths ~ age_group + occ_code + sharemale + sharewhite + shareblack + shareasian + sharehispanic + sharehighschool + sharecollege + shareselfemp + offset(log(fte)) - 1 # (3)

# Fit models and obtain robust standard errors
m0b <- glm(fm0b, data=combined_plot, family="poisson")
m0b_se <- sqrt(diag(vcovHC(m0b)))
m1b <- glm(fm1b, data=combined_plot, family="poisson")
m1b_se <- sqrt(diag(vcovHC(m1b)))
m2b <- glm(fm2b, data=combined_plot, family="poisson")
m2b_se <- sqrt(diag(vcovHC(m2b)))
#stargazer(m0b,m1b,m2b,omit=c("occ_code"),type="text",report="vc*s",digits=3,single.row=FALSE,coef = NULL, se = list(m0b_se,m1b_se,m2b_se),t.auto=TRUE,p.auto=TRUE,omit.stat=c("ll","theta"))

# Difference to age group 35-44
c0 <- c(exp(coef(m0b)[1:5]-coef(m0b)[3]))
c1 <- c(exp(coef(m1b)[1:5]-coef(m1b)[3]))
c2 <- c(exp(coef(m2b)[1:5]-coef(m2b)[3]))

# Collect data for the plot
plottable <- data.frame(rbind(c0,c1,c2))
plottable <- cbind(case=c("age FE","age FE + occupation FE","age FE + occupation FE + demographic char."),plottable)
colnames(plottable) <- c("case","20–24","25–34","35–44","45–54","55–64")
plottable <- pivot_longer(plottable,2:6,names_to="age_group",values_to="rate")

# Compute standard errors for specification (3)
A <- rbind( c(1,0,-1,0,0), c(0,1,-1,0,0), c(0,0,0,0,0), c(0,0,-1,1,0) , c(0,0,-1,0,1) )
V <- vcovHC(m2b)[1:5,1:5]
sig2 <- diag(A%*%V%*%t(A))
# Compute 95% confidence intervals for specification (3)
upper2 <- c2*exp(1.96*sqrt(sig2))
lower2 <- c2*exp(-1.96*sqrt(sig2))

# Add to data for the plot
plottable <- plottable %>% mutate(lower=NA,upper=NA)
plottable$upper[11:15] <- upper2
plottable$lower[11:15] <- lower2

# Generate plot
ggplot(plottable, aes(x = age_group, y = rate, group = case, color = case, shape = case)) +
  geom_errorbar( aes(x=age_group, ymin=lower, ymax=upper), width=0.2, alpha=0.7) +  
  geom_line(linewidth=0.7) + geom_point(size=2) + scale_y_continuous(breaks = seq(min(0.6), max(1.8), by = 0.2), limits = c(0.6,1.8),expand = expansion(add=c(0,0))) + 
    xlab("Age group") + ylab("Occupational fatality rate relative to age group 35–44") +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(), panel.border = element_blank(), axis.line = element_line(color = "black")) +
  theme(legend.title=element_blank(), axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

# Save plot
ggsave("figures/Figure_2.png", width = 6, height = 5)
