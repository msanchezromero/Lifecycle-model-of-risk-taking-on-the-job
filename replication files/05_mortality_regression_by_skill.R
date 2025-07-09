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

# share of low-skilled (non-college) per occupation group
skill_occ <- dataCPS %>%
  filter(age_group>=4 & age_group<=8) %>%
  mutate(college = as.numeric(college)) %>%
  group_by(occ_code,college) %>%
  summarise(N=sum(wgt))
skill_occ <- pivot_wider(skill_occ, names_from=college, names_prefix = "college", values_from=N)
skill_occ <- skill_occ %>% mutate(sharecollege = college1/(college0+college1))
# indicate low-skill occupations
skill_occ <- skill_occ %>% mutate(SKILLED = 1*(sharecollege>.5))
# share per occupation
skill_occ <- skill_occ %>% dplyr::select(occ_code, sharecollege, SKILLED)
print(skill_occ,n=50)


### prepare CFOI data
dataCFOI <- readRDS("data/CFOI.RDS")
CFOI_agg <- dataCFOI %>%
  filter(age_group>=4 & age_group<=8 & occ_code<55 & year<=2018 & digits==2) %>%
  group_by(age_group,occ_code,year) %>%
  summarize(deaths=sum(value))

### match data
combined <- full_join(CPS_agg, CFOI_agg) %>%
  mutate(
    deaths = tidyr::replace_na(deaths,0),
    rate = deaths/fte*10^5)

# merge with skill table
combined <- left_join(combined,skill_occ %>% dplyr::select(-sharecollege))
# remove group 11 as it is very special (CEOs and farmers together!)
combined <- combined %>% mutate(SKILLED = ifelse(occ_code %in% c("11","55"),NA,SKILLED))


################################
### DATA ANALYSIS
################################

# Loop through skill levels
for (i in 0:1) {

  # Select sample for given skill level
  combined_reg <- combined %>% filter(SKILLED==i) %>% mutate(age_group=as.factor(age_group))
  
  # Set age group 35-44 as baseline
  combined_tab <- combined_reg %>% mutate(age_group = ifelse(age_group==6,0,age_group))
  
  
  ################################
  ### REGRESSION TABLES (unpublished)
  ################################
  
  # Model specifications
  fm0a <- deaths ~ age_group + offset(log(fte)) # (1)
  fm1a <- deaths ~ age_group + occ_code + offset(log(fte)) # (2)
  fm2a <- deaths ~ age_group + occ_code + sharemale + sharewhite + shareblack + shareasian + sharehispanic + sharehighschool + sharecollege + shareselfemp + offset(log(fte)) # (3)

  # Fit models and obtain robust standard errors
  m0a <- glm(fm0a, data=combined_tab, family="poisson")
  m0a_se <- sqrt(diag(vcovHC(m0a)))
  m1a <- glm(fm1a, data=combined_tab, family="poisson")
  m1a_se <- sqrt(diag(vcovHC(m1a)))
  m2a <- glm(fm2a, data=combined_tab, family="poisson")
  m2a_se <- sqrt(diag(vcovHC(m2a)))  
  
  # Regression table
  stargazer(m0a,m1a,m2a,type="text",report="vc*s",digits=3,single.row=FALSE,coef = NULL, se = list(m0a_se,m1a_se,m2a_se),t.auto=TRUE,p.auto=TRUE,omit.stat=c("ll","theta"))
  
  
  ################################
  ### PLOTS (Figure A1)
  ################################
  
  combined_plot <- combined_reg
  
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
  plottable <- cbind(case=c("age FE","age FE + occ. FE","age FE + occ. FE + demogr."),plottable)
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
    geom_line(linewidth=0.7) + geom_point(size=2) + scale_y_continuous(breaks = seq(0.2, 2.2, by = 0.2), limits = c(0.2,2.2),expand = expansion(add=c(0,0))) +
    xlab("Age group") + ylab("Occupational fatality rate rel. to age group 35–44") +
    theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(), panel.border = element_blank(), axis.line = element_line(color = "black")) +
    theme(legend.title=element_blank(), axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  
  # Save plot
  ggsave(paste0("figures/FigureA1_",letters[i+1],".png"), width = 4.6, height = 5)
  
}

