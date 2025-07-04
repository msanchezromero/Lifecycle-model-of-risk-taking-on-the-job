using RCall

# -------------------------------------------------------------------------------------
# Table 2: 
# -------------------------------------------------------------------------------------
function f_results_table2(data)
    
    @rput data
    R"""
    #suppressWarnings({
    #install.packages("stargazer",  repos = "https://cloud.r-project.org")
    #install.packages("kableExtra", repos = "https://cloud.r-project.org")
    #})
    library("stargazer")
    library(tidyr)
    library(dplyr)
    library(kableExtra)
    
        
    # All ages
     tb2.all1<-data %>% 
       mutate(Mx=-100000*12*log(1-MortalityJob/100000)) %>%
       subset(Employment==1 & (Age2 %in% c(seq(20,65))) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~MortalityJob+factor(Age2)-1) %>% 
        summary()
    
    tb2.all2<-data %>% 
      subset(Employment==1 & (Age2 %in% c(seq(20,64))) & (Simu=="Benchmark")) %>% 
      summarize(
        w=mean(Wages),
        VSL=tb2.all1$coefficients[1,1]*w*0.1,
        VOL=mean(VOL)/1000000, .groups = "drop") 
    
    tb2.all<-cbind(t(tb2.all1$coefficients[1,1:2]),data.frame(tb2.all2))
      
    # Age 40
    tb2.40_1<-data %>% 
      mutate(Mx=-100000*12*log(1-MortalityJob/100000)) %>%
      subset(Employment==1 & (Age2==40) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~MortalityJob) %>% 
        summary()
    
    tb2.40_2<-data %>% subset(Employment==1 & (Age2==40) & (Simu=="Benchmark")) %>% 
      summarize(
        w=mean(Wages),    
        VSL=tb2.40_1$coefficients[2,1]*w*0.1,
        VOL=mean(VOL)/1000000, .groups = "drop")
    
    tb2.40<-cbind(t(tb2.40_1$coefficients[2,1:2]),data.frame(tb2.40_2))
    
    # Age 50
    tb2.50_1<-data %>% 
      mutate(Mx=-100000*12*log(1-MortalityJob/100000)) %>%
      subset(Employment==1 & (Age2==50) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~MortalityJob) %>% 
        summary()
    
    tb2.50_2<-data %>% 
      subset(Employment==1 & (Age2==50) & (Simu=="Benchmark")) %>% 
      summarize(
        w=mean(Wages),
        VSL=tb2.50_1$coefficients[2,1]*w*0.1,
        VOL=mean(VOL)/1000000, .groups = "drop")
    
    tb2.50<-cbind(t(tb2.50_1$coefficients[2,1:2]),data.frame(tb2.50_2))
    
    # Age 60
    tb2.60_1<-data %>% 
      mutate(Mx=-100000*12*log(1-MortalityJob/100000)) %>%
      subset(Employment==1 & (Age2==60) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~MortalityJob) %>% 
        summary()
    
    tb2.60_2<-data %>% 
      subset(Employment==1 & (Age2==60) & (Simu=="Benchmark")) %>% 
      summarize(
        w=mean(Wages),
        VSL=tb2.60_1$coefficients[2,1]*w*0.1,
        VOL=mean(VOL)/1000000, .groups = "drop")
    
    tb2.60<-cbind(t(tb2.60_1$coefficients[2,1:2]),data.frame(tb2.60_2))
    
    tb2<-data.frame(t(tb2.all),t(tb2.40),t(tb2.50),t(tb2.60))
    tb2$Variable<-row.names(tb2)
    colnames(tb2)<-c("All","Age=40","Age=50","Age=60","Variable")
    
    tb2.final<-tb2 %>% 
      gather(Case, Value, All:'Age=60', factor_key=TRUE) %>% 
      spread(Variable, Value) %>%
      select(Case,Estimate,'Std. Error',w,VSL,VOL) %>% 
      mutate(
        Estimate=round(Estimate,4),
        w=round(w,0),
        VSL=round(VSL,2),
        VOL=round(VOL,2)) %>% 
      gather(Variable, Value, Estimate:VOL, factor_key=TRUE) %>%
      spread(Case, Value) 
    
    kableExtra:::kbl(
      tb2.final,
      booktabs = T,
      caption="Value of a statistical life overall and by age",
      format="simple") 
    
    """

end


# -------------------------------------------------------------------------------------
# Table 3: 
# -------------------------------------------------------------------------------------
function f_results_table3(data)
    
    @rput data
    R"""
    #suppressWarnings(
    #install.packages("stargazer", repos = "https://cloud.r-project.org")
    #)
    library("stargazer")
    library(tidyr)
    library(dplyr)
    
    # log(MortalityJob)~log(Assets)
    # Age 50
    ols.1<-data %>% 
        subset(Employment==1 & (Age2==50) & (Simu=="Benchmark")) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    # Age 55
    ols.2<-data %>% 
        subset(Employment==1 & (Age2==55) & (Simu=="Benchmark")) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    # Age 60
    ols.3<-data %>% 
        subset(Employment==1 & (Age2==60) & (Simu=="Benchmark")) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    
    # log(Wages)~log(Assets)
    # Age 50
    olsW.1<-data %>% 
        subset(Employment==1 & (Age2==50) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~log(Assets))
    # Age 55
    olsW.2<-data %>% 
        subset(Employment==1 & (Age2==55) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~log(Assets))
    # Age 60
    olsW.3<-data %>% 
        subset(Employment==1 & (Age2==60) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~log(Assets))
    
    stargazer(ols.1,  ols.2,  ols.3,  title="Results", align=TRUE, type="text", out.header=TRUE)
    stargazer(olsW.1, olsW.2, olsW.3, title="Results", align=TRUE, type="text", out.header=TRUE)
    
    """

end

# -------------------------------------------------------------------------------------
# Table 4: 
# -------------------------------------------------------------------------------------
function f_results_table4(data,sample_size,Par_U::Params_Unmut)

    ParM01 = mutable_struct( 1,sample_size,Par_U)
    ParM11 = mutable_struct(11,sample_size,Par_U)
    
    sigmaG01 = ParM01.σC
    sigmaG11 = ParM01.σC
    
    @rput sigmaG01
    @rput sigmaG11   
    @rput data
    R"""
    #suppressWarnings({
    #install.packages("stargazer", repos = "https://cloud.r-project.org")
    #install.packages("kableExtra", repos = "https://cloud.r-project.org")
    #})
    library("stargazer")
    library(tidyr)
    library(dplyr)
    library(kableExtra)
    
    tbl4<-data %>% 
        subset(Age<1200 & !(Simu %in% c("Exp4","Exp5","Exp6","Exp7","Exp8"))) %>%
        mutate(sigmaG = ifelse(Simu %in% c("Exp5", "Exp6", "Exp7", "Exp8"), sigmaG11, sigmaG01)) %>%
      group_by(Simu,Ind) %>% 
        mutate(
          IUtil  =Survival*((Consumption^(1-1/sigmaG))-1)/(1-1/sigmaG),
          IUtil.d=Survival*(Consumption^(1-1/sigmaG))/(1-1/sigmaG),
          ) %>% 
      group_by(Simu,Ind) %>% 
        mutate(
          #LE     =sum(Survival)/12,
          EUtil  =(sum(IUtil)+IUtil-cumsum(IUtil))/Survival,
          EUtil.d=(sum(IUtil.d)+IUtil.d-cumsum(IUtil.d))/Survival) %>% 
      group_by(Ind) %>% 
        mutate(Li=1-(1+(EUtil[Simu=="Benchmark"]-EUtil)/EUtil.d)^(1/(1-1/sigmaG))) %>% 
        subset(Age2 %in% c(20,30,40,50,60)) %>%
      group_by(Simu,Age2) %>% 
        summarize(Lambda=round(100*mean(Li),2), .groups = "drop") %>% spread(Simu, Lambda) 
    
    kableExtra:::kbl(
      tbl4,
      booktabs = T,
      caption="Table 4: Welfare effects of pension reforms and ageing by exact age. Ref=Benchmark (mean values, in %)",
      format="simple") 
    """

end


# -------------------------------------------------------------------------------------
# Table 5: 
# -------------------------------------------------------------------------------------
function f_results_table5(data,sample_size,Par_U::Params_Unmut)

    ParM01 = mutable_struct( 1,sample_size,Par_U)
    ParM11 = mutable_struct(11,sample_size,Par_U)
    
    sigmaG01 = ParM01.σC
    sigmaG11 = ParM01.σC
    
    @rput sigmaG01
    @rput sigmaG11
    @rput data
    R"""
    #suppressWarnings({
    #install.packages("stargazer", repos = "https://cloud.r-project.org")
    #install.packages("kableExtra", repos = "https://cloud.r-project.org")
    #})
    library("stargazer")
    library(tidyr)
    library(dplyr)
    library(kableExtra)
        
    tbl5<-data %>% 
        subset(Age<1200 & !(Simu %in% c("Benchmark","Exp1","Exp2","Exp3","Exp4"))) %>%
        mutate(sigmaG = ifelse(Simu %in% c("Exp5", "Exp6", "Exp7", "Exp8"), sigmaG11, sigmaG01)) %>%
        group_by(Simu,Productivity,Ind) %>% 
        mutate(
          IUtil  =Survival*((Consumption^(1-1/sigmaG))-1)/(1-1/sigmaG),
          IUtil.d=Survival*(Consumption^(1-1/sigmaG))/(1-1/sigmaG),
          ) %>% 
        group_by(Simu,Productivity,Ind) %>% 
        mutate(
          #LE     =sum(Survival)/12,
          EUtil  =(sum(IUtil)+IUtil-cumsum(IUtil))/Survival,
          EUtil.d=(sum(IUtil.d)+IUtil.d-cumsum(IUtil.d))/Survival) %>% 
        group_by(Productivity,Ind) %>% 
        mutate(Li=1-(1+(EUtil[Simu=="Exp5"]-EUtil)/EUtil.d)^(1/(1-1/sigmaG))) %>% 
        mutate(Simu = case_when( 
                            Simu=="Exp5" ~ "Benchmark",
                            Simu=="Exp6" ~ "Exp1",
                            Simu=="Exp7" ~ "Exp2",
                            Simu=="Exp8" ~ "Exp3")
            ) %>%          
        subset(Age2 %in% c(20,30,40,50,60)) %>%
        group_by(Simu,Productivity,Age2) %>% 
        summarize(Lambda=round(100*mean(Li),2), .groups = "drop") %>% spread(Simu, Lambda) 
    
    kableExtra:::kbl(
      tbl5,
      booktabs = T,
      caption="Table 5: Welfare effects of pension reforms and ageing by exact age. Ref=Benchmark (mean values, in %)",
      format="simple") 
    
    """

end



# -------------------------------------------------------------------------------------
# Table A3: 
# -------------------------------------------------------------------------------------
function f_results_tableA3(data)
    # Table A3: Descriptive statistics of four alternative simulations
    # (Period: Month)

    @rput data
    R"""    

    #install.packages("radiant.data", repos = "https://cloud.r-project.org")
    
    library(tidyr)
    library(dplyr)
    library(kableExtra)
    
    # Our simulations are based on a stationary population with 500 new individuals born each month. 
    # For simplicity, we standardized the values in the Table to 1000 individuals annually.
    AdjFactor<-(1000/(12*500))
    popgrowth<-1.00^(1/12)
    
    tblA3.top<-data %>% subset(Simu!=c("Exp4")) %>%
      mutate(popC=popgrowth^(1-Age)) %>%
      group_by(Simu) %>% 
      summarize(
        N=round(sum(Survival*popC)*AdjFactor,0),
        L=round(sum(Survival*popC*ifelse(Employment==1,1,0))*AdjFactor,0),
        U=round(sum(Survival*popC*ifelse(Employment==0,1,0))*AdjFactor,0),
        R=round(sum(Survival*popC*ifelse(Employment==2,1,0))*AdjFactor,0),
        U_rate=round(100*U/(L+U),2)) %>%   
      gather(Variable, Value, N:U_rate, factor_key=TRUE) %>% 
      spread(Simu, Value) 
    
    kableExtra:::kbl(
      tblA3.top,
      format = "pandoc",  
      booktabs = T,
      caption="Table A3: Descriptive statistics of four alternative simulations (Period: Month)") #%>%
      #kableExtra:::kable_classic(full_width = F, html_font = "Cambria")
    
    
    tblA3.bottom<-data %>% subset(Simu!="Exp4") %>% 
      mutate(popC  =popgrowth^(1-Age),
             weight=Survival*popC/sum(Survival*popC),
             weight1 = 1,
             Mx    =-100000*12*log(1-MortalityJob/100000)) %>%
      subset(Employment==1 & (Age2<65.0)) %>% 
      mutate(F.L=Wages/Output) %>% 
      group_by(Simu) %>% 
      summarize(
        mT.mean  =round(weighted.mean(Mortality,w=weight),2),
        mT.sd    =round(radiant.data::weighted.sd(x = Mortality, wt = weight),2),
        m.mean   =round(weighted.mean(MortalityJob,w=weight),2),
        m.sd     =round(radiant.data::weighted.sd(x = MortalityJob, wt = weight),2),
        mu.mean  =round(weighted.mean(Mx,w=weight),2),
        mu.sd    =round(radiant.data::weighted.sd(x = Mx, wt = weight),2),
        w.mean   =round(weighted.mean(Wages,w=weight),0),
        w.sd     =round(radiant.data::weighted.sd(x = Wages, wt = weight),0),
        y.mean   =round(weighted.mean(Wages/0.66,w=weight),0),
        y.sd     =round(radiant.data::weighted.sd(x = Wages/0.66, wt = weight),0),
        c.mean   =round(weighted.mean(Consumption,w=weight),0),
        c.sd     =round(radiant.data::weighted.sd(x = Consumption, wt = weight),0),
        a.mean   =round(weighted.mean(Assets,w=weight)/1000,0),
        a.sd     =round(radiant.data::weighted.sd(x = Assets, wt = weight)/1000,0),
    #    K.mean   =round(sum(Assets*Survival*popC)/1000,0),
    #    K.sd     =round(radiant.data::weighted.sd(x = Assets*Survival*popC, wt = weight1)/1000,0),
        vol.mean =round(weighted.mean(VOL,w=weight)/1000,0),
        vol.sd   =round(radiant.data::weighted.sd(x = VOL, wt = weight)/1000,0),
        t.mean   =weighted.mean(Tax,w=weight),
        r.mean   =round(100*(0.33*(0.66/mean(F.L))^(2.0)-(1.05^(1/12)-1.0)),2)) %>%  
      gather(Variable, Value, mT.mean:r.mean, factor_key=TRUE) 
    
    tblA3.bottom$type<-c(rep(c(rep("Mean",8),rep("SD",8)),8),rep(c(rep("Mean",8)),2))
    tblA3.bottom<-tblA3.bottom %>% mutate(Variable=case_when(
        Variable=="mT.mean"  ~ "1.mT",
        Variable=="mT.sd"    ~ "1.mT",
        Variable=="m.mean"   ~ "2.m",
        Variable=="m.sd"     ~ "2.m",
        Variable=="mu.mean"  ~ "3.mu",
        Variable=="mu.sd"    ~ "3.mu",
        Variable=="w.mean"   ~ "4.w",
        Variable=="w.sd"     ~ "4.w",
        Variable=="y.mean"   ~ "5.y",
        Variable=="y.sd"     ~ "5.y",    
        Variable=="c.mean"   ~ "6.c",
        Variable=="c.sd"     ~ "6.c",    
        Variable=="a.mean"   ~ "7.a",
        Variable=="a.sd"     ~ "7.a",
        #Variable=="K.mean"  ~ "11.a",
        #Variable=="K.sd"    ~ "11.a",    
        Variable=="vol.mean" ~ "8.vol",
        Variable=="vol.sd"   ~ "8.vol",    
        Variable=="t.mean"   ~ "9.t",
        Variable=="r.mean"   ~ "10.r")) %>% spread(type, Value) 
    
    kableExtra:::kbl(
      cbind(
        tblA3.bottom %>% subset(Simu=="Benchmark") %>% select(Variable,Mean,SD),
        tblA3.bottom %>% subset(Simu=="Exp1") %>% select(Mean,SD),
        tblA3.bottom %>% subset(Simu=="Exp2") %>% select(Mean,SD),
        tblA3.bottom %>% subset(Simu=="Exp3") %>% select(Mean,SD)),
      format = "pandoc",
      booktabs = T,
      col.names=c("",paste0(rep(c("Mean","SD"),4))),
      caption="Table A3: Descriptive statistics of four alternative simulations (Period: Month)") #%>%
      #kableExtra:::add_header_above(c("", "Benchmark" = 2, "Exp1"=2,"Exp2"=2,"Exp3"=2, "Exp5"=2)) %>%
      #kableExtra:::kable_classic(full_width = F, html_font = "Cambria")
    """

end

# -------------------------------------------------------------------------------------
# Table A5: 
# -------------------------------------------------------------------------------------
function f_results_tableA5(data)
    
    @rput data
    R"""
    #suppressWarnings({
    #install.packages("stargazer",  repos = "https://cloud.r-project.org")
    #install.packages("kableExtra", repos = "https://cloud.r-project.org")
    #})
    library("stargazer")
    library(tidyr)
    library(dplyr)
    library(kableExtra)
    
    Prod <- data %>% subset(Simu=="Exp5") %>% select(Productivity) %>% unique()
    
    # ---------------- FIRST PRODUCTIVITY LEVEL ----------------
    prod1 <- as.numeric(Prod$Productivity[1])
    
    # All ages
    df_all <- data %>%
      mutate(Mx = -100000 * 12 * log(1 - MortalityJob / 100000)) %>%
      subset(Employment == 1 & Age2 %in% 20:65 & Simu == "Exp5" & Productivity == prod1)
    mod_all <- lm(log(Wages) ~ MortalityJob + factor(Age2) - 1, data = df_all)
    tbA5.all1 <- summary(mod_all)
    
    df_all2 <- data %>%
      subset(Employment == 1 & Age2 %in% 20:64 & Simu == "Exp5" & Productivity == prod1)
    tbA5.all2 <- df_all2 %>%
      summarize(w = mean(Wages), VSL = tbA5.all1$coefficients[1,1] * w * 0.1, VOL = mean(VOL) / 1e6)
    
    tbA5.all <- cbind(t(tbA5.all1$coefficients[1,1:2]), tbA5.all2)
    
    # Age 40
    df40 <- subset(data, Employment == 1 & Age2 == 40 & Simu == "Exp5" & Productivity == prod1)
    mod40 <- lm(log(Wages) ~ MortalityJob, data = df40)
    tbA5.40_1 <- summary(mod40)
    tbA5.40_2 <- df40 %>%
      summarize(w = mean(Wages), VSL = coef(mod40)[2] * w * 0.1, VOL = mean(VOL) / 1e6)
    tbA5.40 <- cbind(t(tbA5.40_1$coefficients[2,1:2]), tbA5.40_2)
    
    # Age 50
    df50 <- subset(data, Employment == 1 & Age2 == 50 & Simu == "Exp5" & Productivity == prod1)
    mod50 <- lm(log(Wages) ~ MortalityJob, data = df50)
    tbA5.50_1 <- summary(mod50)
    tbA5.50_2 <- df50 %>%
      summarize(w = mean(Wages), VSL = coef(mod50)[2] * w * 0.1, VOL = mean(VOL) / 1e6)
    tbA5.50 <- cbind(t(tbA5.50_1$coefficients[2,1:2]), tbA5.50_2)
    
    # Age 60
    df60 <- subset(data, Employment == 1 & Age2 == 60 & Simu == "Exp5" & Productivity == prod1)
    mod60 <- lm(log(Wages) ~ MortalityJob, data = df60)
    tbA5.60_1 <- summary(mod60)
    tbA5.60_2 <- df60 %>%
      summarize(w = mean(Wages), VSL = coef(mod60)[2] * w * 0.1, VOL = mean(VOL) / 1e6)
    tbA5.60 <- cbind(t(tbA5.60_1$coefficients[2,1:2]), tbA5.60_2)
    
    # Combine and format table
    tbA5 <- data.frame(t(tbA5.all), t(tbA5.40), t(tbA5.50), t(tbA5.60))
    tbA5$Variable <- row.names(tbA5)
    colnames(tbA5) <- c("All", "Age=40", "Age=50", "Age=60", "Variable")
    
    tbA5.final <- tbA5 %>%
      gather(Case, Value, All:`Age=60`, factor_key = TRUE) %>%
      spread(Variable, Value) %>%
      select(Case, Estimate, `Std. Error`, w, VSL, VOL) %>%
      mutate(across(c(Estimate, VSL, VOL), round, 2),
             w = round(w, 0)) %>%
      gather(Variable, Value, Estimate:VOL, factor_key = TRUE) %>%
      spread(Case, Value)
    
    kableExtra:::kbl(
      tbA5.final,
      booktabs = TRUE,
      caption = "A5, Value of a statistical life overall and by age — Productivity Level 1",
      format = "simple"
    ) %>% print()
    
    
    # ---------------- SECOND PRODUCTIVITY LEVEL ----------------
    prod2 <- as.numeric(Prod$Productivity[2])
    
    # All ages
    df_allB <- data %>%
      mutate(Mx = -100000 * 12 * log(1 - MortalityJob / 100000)) %>%
      subset(Employment == 1 & Age2 %in% 20:65 & Simu == "Exp5" & Productivity == prod2)
    mod_allB <- lm(log(Wages) ~ MortalityJob + factor(Age2) - 1, data = df_allB)
    tbB5.all1 <- summary(mod_allB)
    
    df_all2B <- data %>%
      subset(Employment == 1 & Age2 %in% 20:64 & Simu == "Exp5" & Productivity == prod2)
    tbB5.all2 <- df_all2B %>%
      summarize(w = mean(Wages), VSL = tbB5.all1$coefficients[1,1] * w * 0.1, VOL = mean(VOL) / 1e6)
    
    tbB5.all <- cbind(t(tbB5.all1$coefficients[1,1:2]), tbB5.all2)
    
    # Age 40
    df40B <- subset(data, Employment == 1 & Age2 == 40 & Simu == "Exp5" & Productivity == prod2)
    mod40B <- lm(log(Wages) ~ MortalityJob, data = df40B)
    tbB5.40_1 <- summary(mod40B)
    tbB5.40_2 <- df40B %>%
      summarize(w = mean(Wages), VSL = coef(mod40B)[2] * w * 0.1, VOL = mean(VOL) / 1e6)
    tbB5.40 <- cbind(t(tbB5.40_1$coefficients[2,1:2]), tbB5.40_2)
    
    # Age 50
    df50B <- subset(data, Employment == 1 & Age2 == 50 & Simu == "Exp5" & Productivity == prod2)
    mod50B <- lm(log(Wages) ~ MortalityJob, data = df50B)
    tbB5.50_1 <- summary(mod50B)
    tbB5.50_2 <- df50B %>%
      summarize(w = mean(Wages), VSL = coef(mod50B)[2] * w * 0.1, VOL = mean(VOL) / 1e6)
    tbB5.50 <- cbind(t(tbB5.50_1$coefficients[2,1:2]), tbB5.50_2)
    
    # Age 60
    df60B <- subset(data, Employment == 1 & Age2 == 60 & Simu == "Exp5" & Productivity == prod2)
    mod60B <- lm(log(Wages) ~ MortalityJob, data = df60B)
    tbB5.60_1 <- summary(mod60B)
    tbB5.60_2 <- df60B %>%
      summarize(w = mean(Wages), VSL = coef(mod60B)[2] * w * 0.1, VOL = mean(VOL) / 1e6)
    tbB5.60 <- cbind(t(tbB5.60_1$coefficients[2,1:2]), tbB5.60_2)
    
    # Combine and format table
    tbB5 <- data.frame(t(tbB5.all), t(tbB5.40), t(tbB5.50), t(tbB5.60))
    tbB5$Variable <- row.names(tbB5)
    colnames(tbB5) <- c("All", "Age=40", "Age=50", "Age=60", "Variable")
    
    tbB5.final <- tbB5 %>%
      gather(Case, Value, All:`Age=60`, factor_key = TRUE) %>%
      spread(Variable, Value) %>%
      select(Case, Estimate, `Std. Error`, w, VSL, VOL) %>%
      mutate(across(c(Estimate, VSL, VOL), round, 2),
             w = round(w, 0)) %>%
      gather(Variable, Value, Estimate:VOL, factor_key = TRUE) %>%
      spread(Case, Value)
    
    kableExtra:::kbl(
      tbB5.final,
      booktabs = TRUE,
      caption = "A5, Value of a statistical life overall and by age — Productivity Level 2",
      format = "simple"
    ) %>% print()
    """

end


# -------------------------------------------------------------------------------------
# Table A6: 
# -------------------------------------------------------------------------------------
function f_results_tableA6(data)
    
    @rput data
    R"""
    #suppressWarnings(
    #install.packages("stargazer", repos = "https://cloud.r-project.org")
    #)
    library("stargazer")
    library(tidyr)
    library(dplyr)

    Prod <- data %>% subset(Simu=="Exp5") %>% select(Productivity) %>% unique()
    
    # ---------------- FIRST PRODUCTIVITY LEVEL ----------------
    prod1 <- as.numeric(Prod$Productivity[1])
    
    # log(MortalityJob)~log(Assets)
    # Age 50
    ols.1<-data %>% 
        subset(Employment==1 & (Age2==50) & (Simu=="Exp5") & (Productivity==prod1)) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    # Age 55
    ols.2<-data %>% 
        subset(Employment==1 & (Age2==55) & (Simu=="Exp5") & (Productivity==prod1)) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    # Age 60
    ols.3<-data %>% 
        subset(Employment==1 & (Age2==60) & (Simu=="Exp5") & (Productivity==prod1)) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    
    # log(Wages)~log(Assets)
    # Age 50
    olsW.1<-data %>% 
        subset(Employment==1 & (Age2==50) & (Simu=="Exp5") & (Productivity==prod1)) %>% 
        lm(formula=log(Wages)~log(Assets))
    # Age 55
    olsW.2<-data %>% 
        subset(Employment==1 & (Age2==55) & (Simu=="Exp5") & (Productivity==prod1)) %>% 
        lm(formula=log(Wages)~log(Assets))
    # Age 60
    olsW.3<-data %>% 
        subset(Employment==1 & (Age2==60) & (Simu=="Exp5") & (Productivity==prod1)) %>% 
        lm(formula=log(Wages)~log(Assets))
    
    stargazer(ols.1,  ols.2,  ols.3,  title="Marginal effect of wealth on on-the-job mortality and wages by skill group: Low-skilled", align=TRUE, type="text", out.header=TRUE) #%>% print()
    stargazer(olsW.1, olsW.2, olsW.3, title="Marginal effect of wealth on on-the-job mortality and wages by skill group: Low-skilled", align=TRUE, type="text", out.header=TRUE) #%>% print()

    # ---------------- SECOND PRODUCTIVITY LEVEL ----------------
    prod2 <- as.numeric(Prod$Productivity[2])
    
    # log(MortalityJob)~log(Assets)
    # Age 50
    olsB.1<-data %>% 
        subset(Employment==1 & (Age2==50) & (Simu=="Exp5") & (Productivity==prod2)) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    # Age 55
    olsB.2<-data %>% 
        subset(Employment==1 & (Age2==55) & (Simu=="Exp5") & (Productivity==prod2)) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    # Age 60
    olsB.3<-data %>% 
        subset(Employment==1 & (Age2==60) & (Simu=="Exp5") & (Productivity==prod2)) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    
    # log(Wages)~log(Assets)
    # Age 50
    olsWB.1<-data %>% 
        subset(Employment==1 & (Age2==50) & (Simu=="Exp5") & (Productivity==prod2)) %>% 
        lm(formula=log(Wages)~log(Assets))
    # Age 55
    olsWB.2<-data %>% 
        subset(Employment==1 & (Age2==55) & (Simu=="Exp5") & (Productivity==prod2)) %>% 
        lm(formula=log(Wages)~log(Assets))
    # Age 60
    olsWB.3<-data %>% 
        subset(Employment==1 & (Age2==60) & (Simu=="Exp5") & (Productivity==prod2)) %>% 
        lm(formula=log(Wages)~log(Assets))
    
    stargazer(olsB.1,  olsB.2,  olsB.3,  title="Marginal effect of wealth on on-the-job mortality and wages by skill group: High-skilled ", align=TRUE, type="text", out.header=TRUE) #%>% print()
    stargazer(olsWB.1, olsWB.2, olsWB.3, title="Marginal effect of wealth on on-the-job mortality and wages by skill group: High-skilled", align=TRUE, type="text", out.header=TRUE) #%>% print()    
    """

end


# -------------------------------------------------------------------------------------
# Table A7: 
# -------------------------------------------------------------------------------------
function f_results_tableA7(data)
    #Table A7: Descriptive statistics of Exp 5 under different skill groups
    #(Period: Month)

    @rput data    
    R"""
    # Our simulations are based on a stationary population with 500 new individuals born each month. 
    # For simplicity, we standardized the values in the Table to 1000 individuals annually.
    AdjFactor<-(1000/(12*500))
    popgrowth<-1.00^(1/12)
    
    tblA7.top<-data %>% 
      subset(Simu %in% c("Exp5","Exp6","Exp7","Exp8")) %>%
      mutate(popC=popgrowth^(1-Age)) %>%
      group_by(Simu,Productivity) %>% 
      summarize(
        N=round(sum(Survival*popC)*AdjFactor,0),
        L=round(sum(Survival*popC*ifelse(Employment==1,1,0))*AdjFactor,0),
        U=round(sum(Survival*popC*ifelse(Employment==0,1,0))*AdjFactor,0),
        R=round(sum(Survival*popC*ifelse(Employment==2,1,0))*AdjFactor,0),
        U_rate=round(100*U/(L+U),2), .groups = "drop") %>%   
      gather(Variable, Value, N:U_rate, factor_key=TRUE) %>% 
      spread(Simu, Value) 
    
    kableExtra:::kbl(
      tblA7.top,
      booktabs = T,
      format = "pandoc",
      caption="Table A7: Descriptive statistics of four alternative simulations with two skill groups (Period: Month)")
    
    
    
    tblA7.bottom<-data %>% 
      subset(Simu %in% c("Exp5","Exp6","Exp7","Exp8")) %>% 
      mutate(Simu = case_when( 
                            Simu=="Exp5" ~ "Benchmark",
                            Simu=="Exp6" ~ "Exp1",
                            Simu=="Exp7" ~ "Exp2",
                            Simu=="Exp8" ~ "Exp3")
            ) %>%
      mutate(popC  =popgrowth^(1-Age),
             weight=Survival*popC/sum(Survival*popC),
             Mx    =-100000*12*log(1-MortalityJob/100000)) %>%
      subset(Employment==1 & (Age2<65.0)) %>% 
      mutate(F.L=Wages/Output) %>% 
      group_by(Simu,Productivity) %>% 
      summarize(
        mT.mean=round(weighted.mean(Mortality,w=weight),2),
        mT.sd=round(radiant.data::weighted.sd(x = Mortality, wt = weight),2),
        m.mean=round(weighted.mean(MortalityJob,w=weight),2),
        m.sd=round(radiant.data::weighted.sd(x = MortalityJob, wt = weight),2),
        mu.mean=round(weighted.mean(Mx,w=weight),2),
        mu.sd=round(radiant.data::weighted.sd(x = Mx, wt = weight),2),
        w.mean=round(weighted.mean(Wages,w=weight),0),
        w.sd=round(radiant.data::weighted.sd(x = Wages, wt = weight),0),
        y.mean=round(weighted.mean(Wages/0.66,w=weight),0),
        y.sd=round(radiant.data::weighted.sd(x = Wages/0.66, wt = weight),0),
        c.mean=round(weighted.mean(Consumption,w=weight),0),
        c.sd=round(radiant.data::weighted.sd(x = Consumption, wt = weight),0),
        a.mean=round(weighted.mean(Assets,w=weight)/1000,0),
        a.sd=round(radiant.data::weighted.sd(x = Assets, wt = weight)/1000,0),
        vol.mean=round(weighted.mean(VOL,w=weight)/1000,0),
        vol.sd=round(radiant.data::weighted.sd(x = VOL, wt = weight)/1000,0),
        t.mean=weighted.mean(Tax,w=weight),
        r.mean=round(100*(0.33*(0.66/mean(F.L))^(2.0)-(1.05^(1/12)-1.0)),2), .groups = "drop") %>%  
      gather(Variable, Value, mT.mean:r.mean, factor_key=TRUE) 
    
    tblA7.bottom$type<-c(rep(c(rep("Mean",8),rep("SD",8)),8),rep(c(rep("Mean",8)),2))
    tblA7.bottom<-tblA7.bottom %>% 
        mutate(Variable=case_when(
            Variable=="mT.mean" ~ "1.mT",
            Variable=="mT.sd"   ~ "1.mT",
            Variable=="m.mean"  ~ "2.m",
            Variable=="m.sd"    ~ "2.m",
            Variable=="mu.mean" ~ "3.mu",
            Variable=="mu.sd"   ~ "3.mu",
            Variable=="w.mean"  ~ "4.w",
            Variable=="w.sd"    ~ "4.w",
            Variable=="y.mean"  ~ "5.y",
            Variable=="y.sd"    ~ "5.y",    
            Variable=="c.mean"  ~ "6.c",
            Variable=="c.sd"    ~ "6.c",    
            Variable=="a.mean"  ~ "7.a",
            Variable=="a.sd"    ~ "7.a",    
            Variable=="vol.mean"~ "8.vol",
            Variable=="vol.sd"  ~ "8.vol",    
            Variable=="t.mean"  ~ "9.t",
            Variable=="r.mean"  ~ "10.r")) %>% spread(type, Value) %>% print()
        
    kableExtra:::kbl(
    tblA7.bottom %>% subset(Productivity==unique(tblA7.bottom$Productivity)[1]),
      booktabs = T,
      format = "pandoc",
      #col.names=c("Simu", "Variable","Mean","SD"),
      caption="Table A7: Descriptive statistics of four alternative simulations with two skill groups (Period: Month): Low-skilled"
    ) %>% print()   

    kableExtra:::kbl(
    tblA7.bottom %>% subset(Productivity==unique(tblA7.bottom$Productivity)[2]),
      booktabs = T,
      format = "pandoc",
      #col.names=c("Simu", "Variable","Mean","SD"),
      caption="Table A7: Descriptive statistics of four alternative simulations with two skill groups (Period: Month): High-skilled"
    ) %>% print()   
    
    
    """

end


# -------------------------------------------------------------------------------------
# Figure 4: 
# -------------------------------------------------------------------------------------
function f_results_figure4(data)
    
    @rput data
    R"""
    #Figure 4. Age profiles of annual labor income (A), wealth (B), and the
    #conditional mortality on the job (C). Grey areas indicate 2.5% and 97.5%
    #percentiles of the simulated distribution. Red points indicate the data.
    #Case: Benchmark. Data source: CFOI, ACS, own simulations.

    options(warn = -1)  # Turn off warnings
    library("dplyr")
    library("ggplot2")
    library("ggpmisc")
    library("gridExtra")

    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(ggplot2))
    suppressPackageStartupMessages(library(ggpmisc))
    suppressPackageStartupMessages(library(gridExtra))    
    
    # Income data: CPS data
    
    inc.ages<-c(20:64)
    inc.wage<-c(2119.4656,2126.424,2126.1632,2168.8336,2188.72,
      2388.6816,2475.664,2502.7376,2566.12,2576.4912,
      2644.072,2684.856,2678.3408,2715.6768,2730.0496,
      2909.7344,2930.032,2968.6608,2919.544,2938.9792,
      2976.176,2988.7936,2998.1696,3027.1472,3004.6176,
      3030.36,3022.2048,3053.2832,3037.4208,3005.416,
      3049.088,3064.64,3053.9472,3044.2688,3081.9312,
      3096.9072,3122.6496,3086.7984,3091,3093.0512,
      3049.0592,3054.36,3050.1632,2998.7936,2997.0128)
    
    
    
    inc.data<-data.frame(inc.ages,inc.wage)
    
    x.a<-c(22,30,40,50,60)
    y.a<-c(22504,42702,55620,60126,58569)
    y.b<-c(2.23,2.61,2.87,3.49,4.46)
    
    d<-data.frame(x.a,y.a,y.b)
    
    fig4.A<-data %>% 
      subset((Employment==1) & (Simu=="Benchmark")) %>% 
      ggplot(aes(x=Age2,y=Wages))+
      geom_line(color="gray")+
      geom_point(data=inc.data,
                 aes(x=inc.ages,y=inc.wage),color="red",cex=2)+
      labs(
            x="Age",
            y="Monthly wage rate (in $)",
            #title="A. Wage"
            )+
      theme_bw()+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),
        )
    
    fig4.B<-data %>% subset(Simu=="Benchmark") %>% 
      ggplot(aes(x=Age2,y=Assets/1000))+
      geom_line(color="gray")+
      labs(
            x="Age",
            y="Wealth (in $1K)",
            #title="B. Wealth"
            )+
      theme_bw()+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    fig4.C<-data %>% 
      subset((Employment==1) & (Simu=="Benchmark")) %>%
      ggplot(aes(x=Age2,y=-100000*12*log(1-MortalityJob/100000)))+
      geom_line(color="gray")+
      geom_point(data=d,aes(x=x.a,y=y.b),color="red",cex=3)+
      xlim(20,64)+
      labs(
            x="Age",
            y="On-the-job mortality rate (per 100k)",
            #title="C. On-the-job mortality"
            )+
      theme_bw()+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    ggsave("../plots/Figure_4a.png", fig4.A, width = 0.33*21*0.6, height = 7*0.6, dpi=300)
    ggsave("../plots/Figure_4b.png", fig4.B, width = 0.33*21*0.6, height = 7*0.6, dpi=300)
    ggsave("../plots/Figure_4c.png", fig4.C, width = 0.33*21*0.6, height = 7*0.6, dpi=300)

    
    fig4<-grid.arrange(fig4.A, fig4.B, fig4.C,  
                 ncol = 3, nrow = 1, heights=c(2),
                 widths=c(2,2,2))
    
    ggsave("../plots/Figure_4.png", fig4, width = 21*0.6, height = 7*0.6, dpi=300)
    fig4    
    """

end

# -------------------------------------------------------------------------------------
# Figure A4: 
# -------------------------------------------------------------------------------------
function f_results_figureA4(data)
    
    @rput data
    R"""
    #Figure 4. Age profiles of annual labor income (A), wealth (B), and the
    #conditional mortality on the job (C). Grey areas indicate 2.5% and 97.5%
    #percentiles of the simulated distribution. Red points indicate the data.
    #Case: Benchmark. Data source: CFOI, ACS, own simulations.

    options(warn = -1)  # Turn off warnings
    library("dplyr")
    library("ggplot2")
    library("ggpmisc")
    library("gridExtra")
    
    # Income data: CPS data
    
    inc.ages<-c(20:64)
    
    
    inc.wagelow<-c(1899.555,1935.812,1962.56,2025.365,
                   2050.393,2228.502,2285.198,2325.334,
                   2363.148,2364.002,2410.928,2447.561,
                   2439.85,2481.682,2496.911,2631.989,
                   2642.081,2675.14,2621.059,2637.175,
                   2673.44,2689.062,2688.423,2711.995,
                   2694.753,2719.02,2714.095,2752.417,
                   2728.322,2719.82,2755.376,2768.307,
                   2740.702,2742.118,2749.24,2756.414,
                   2774.55,2753.729,2761.51,2725.94,
                   2729.736,2663.436,2686.991,2619.071,2643.466)
    
    inc.wagehigh<-c(2676.216,2649.775,2620.351,2621.681,
                    2654.408,2994.087,3214.443,3179.956,
                    3331.059,3385.082,3553.685,3602.22,
                    3634.531,3633.007,3608.932,3942.893,
                    3991.166,4025.641,3989.256,4023.253,
                    4069.254,4090.107,4126.373,4190.594,
                    4121.438,4157.045,4123.25,4161.691,
                    4159.839,4040.368,4087.472,4148.912,
                    4204.439,4140.462,4297.942,4336.736,
                    4374.844,4277.884,4254.392,4428.364,
                    4174.511,4377.107,4347.422,4404.752,4279.617)
    
    inc.data<-data.frame(inc.ages,inc.wagelow,inc.wagehigh)
    
    x.a<-c(22,30,40,50,60)
    y.a<-c(22504,42702,55620,60126,58569)
    y.b<-c(2.23,2.61,2.87,3.49,4.46)
    y.l<-c(3.5452,4.0143,4.6064,5.4541,6.6608)
    y.h<-c(0.2656,0.4711,0.5554,0.5719,0.7783)
    
    d<-data.frame(x.a,y.a,y.b,y.l,y.h)
    
 
    figA4.A<-ggplot(data=data %>% subset((Employment==1) & (Simu=="Exp5")),
              aes(x=Age2,
                 y=Wages))+
      geom_line(stat = "identity", aes(color = factor(Productivity)),cex=1.5)+
      scale_color_manual(name="",values = c("black", "gray"))+
      geom_point(data=inc.data,
                 aes(x=inc.ages,y=inc.wagelow),
                 color="red",cex=1)+
      geom_point(data=inc.data,
                 aes(x=inc.ages,y=inc.wagehigh),
                 color="red",cex=1)+
      labs(x="Age",y="Monthly wage rate (in $)",legend="")+
      ylim(0,5000)+
      theme_bw()+
      theme(legend.position  = "none",
            panel.border     = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line        = element_line(colour = "black"))
    
    figA4.B<-ggplot(data=data %>% subset(Simu=="Exp5"),
                    aes(x=Age2,y=Assets/1000))+
      geom_line(stat="identity",
                aes(color=factor(Productivity)))+
      scale_color_manual(name="",values = c("black", "gray"))+
      labs(x="Age",y="Wealth (in $1K)")+
      theme_bw()+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),
            legend.position = "none")
    
    figA4.C<-ggplot(data=data %>% subset((Employment==1) & (Simu=="Exp5")),
             aes(x=Age2,
                 y=-100000*12*log(1-MortalityJob/100000)))+
      geom_line(stat="identity",
                aes(color=factor(Productivity)))+
      scale_color_manual(name="", values = c("black", "gray"))+
      geom_point(data=d,aes(x=x.a,y=y.l),color="red",cex=1.5)+
      geom_point(data=d,aes(x=x.a,y=y.h),color="red",cex=1.5)+
      xlim(20,64)+
      labs(x="Age",
           y="On-the-job mortality rate (per 100k)",
           )+
      theme_bw()+
      theme(legend.position="none",
            panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    
    
    ggsave("../plots/Figure_A4a.png", figA4.A, width = 0.33*21*0.6, height = 7*0.6, dpi=300)
    ggsave("../plots/Figure_A4b.png", figA4.B, width = 0.33*21*0.6, height = 7*0.6, dpi=300)
    ggsave("../plots/Figure_A4c.png", figA4.C, width = 0.33*21*0.6, height = 7*0.6, dpi=300)

    
    figA4<-grid.arrange(figA4.A, figA4.B, figA4.C,  
                 ncol = 3, nrow = 1, heights=c(2),
                 widths=c(2,2,2))
    
    ggsave("../plots/Figure_A4.png", figA4, width = 21*0.6, height = 7*0.6, dpi=300)
    figA4    
    """

end

# -------------------------------------------------------------------------------------
# Figure 5: 
# -------------------------------------------------------------------------------------
function f_results_figure5(data)
    
    @rput data
    R"""
    ### - The effect of wealth on mortality and wages (Benchmark)
    
    #Figure 5: Simulated age profiles of the relationship between wealth and
    #on-the-job mortality (Panel A) and the wage (Panel B) at each exact age.
    
    fig5.A<-data %>% 
      subset((Age2 %in% c(50,55,60)) & (Employment==1) & (Simu=="Benchmark")) %>% 
      ggplot(aes(x=Assets/1000,
                 y=-100000*12*log(1-MortalityJob/100000)
                 #MortalityJob
                 ))+
      geom_point(aes(color=as.factor(Age2)))+
      scale_colour_brewer("Exact age")+
      labs(
            x="wealth (in $1K)",
            y="on-the-job mortality (per 100K)",
            #title="A. On-the-job mortality"
            )+
      theme_bw()+
      theme(legend.position = c(0.2, 0.85),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    fig5.B<-data %>% 
      subset((Age2 %in% c(50,55,60)) & (Employment==1) & (Simu=="Benchmark")) %>% 
      ggplot(aes(x=Assets/1000,y=Wages))+
      geom_point(aes(color=as.factor(Age2)))+
      scale_colour_brewer("Exact age")+
      labs(
            x="wealth (in $1K)",
            y="wage (in $)",
            #title="B. Wage"
            )+
      theme_bw()+
      theme(legend.position = "none",
            panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))

    ggsave("../plots/Figure_5a.png", fig5.A, width = 0.5*12.0, height =4.0, dpi=300)
    ggsave("../plots/Figure_5b.png", fig5.B, width = 0.5*12.0, height =4.0, dpi=300)

    
    fig5<-grid.arrange(fig5.A, fig5.B, 
                 ncol = 2, nrow = 1, heights=c(3), widths=c(3,3))
    
    ggsave("../plots/Figure_5.png", fig5, width = 12.0, height =4.0, dpi=300)
    
    fig5
    """

end


# -------------------------------------------------------------------------------------
# Figure 6: 
# -------------------------------------------------------------------------------------
function f_results_figure6(data)

    @rput data
    R"""

    options(warn = -1)  # Turn off warnings
    library("ggplot2")
    library("dplyr")
    
    
    fig6<-data %>% 
    subset(Employment==1 & !(Simu %in% c("Exp4","Exp5","Exp6","Exp7","Exp8"))) %>% 
    mutate(Simulation=Simu) %>% 
    group_by(Simulation,Age2) %>% 
    summarize(MortalityJ=mean(MortalityJob), .groups = "drop") %>% 
    ggplot(aes(x=Age2,y=-100000*12*log(1-MortalityJ/100000)))+
      geom_line(aes(color=Simulation,linetype=Simulation),cex=0.75)+
      scale_color_discrete(
        name="Simulation",
        breaks=c("Benchmark","Exp1","Exp2","Exp3"),
        labels=c("Benchmark","Exp. I","Exp. II","Exp. III"))+
      scale_linetype_discrete(
        name="Simulation",
        breaks=c("Benchmark","Exp1","Exp2","Exp3"),
        labels=c("Benchmark","Exp. I","Exp. II","Exp. III"))+
        xlim(20,64)+
      labs(
        x="Age",
        y="On-the-job mortality rate (per 100K)")+
      theme_bw()+
      theme(legend.position = c(0.20, 0.70),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    ggsave("../plots/Figure_6.png", fig6, width = 21*0.6, height = 7*0.6, dpi=300)
    fig6
    """

end

# -------------------------------------------------------------------------------------
# Figure 7: 
# -------------------------------------------------------------------------------------
function f_results_figure7(data)
    
    @rput data
    R"""

    options(warn = -1)  # Turn off warnings
    library("ggplot2")
    library("dplyr")
    library("ggpmisc")
    library("gridExtra")
    
    datanew<-data %>% 
      subset(Employment==1 & 
               (Simu %in% c("Exp5","Exp6","Exp7","Exp8")))
    
    fig7a<-datanew %>% 
      subset(Productivity==unique(datanew$Productivity)[1]) %>% 
      group_by(Simu,Age2) %>%
      summarize(MortalityJ=mean(MortalityJob), .groups = "drop") %>%
      ggplot(aes(x=Age2,y=-100000*12*log(1-MortalityJ/100000)))+
      geom_line(aes(color=factor(Simu),linetype=factor(Simu)),cex=0.75)+
      scale_color_discrete(
        name="Simulation",
        breaks=c("Exp5","Exp6","Exp7","Exp8"),
        labels=c("Benchmark","Exp. I","Exp. II","Exp. III"))+
      scale_linetype_discrete(
        name="Simulation",
        breaks=c("Exp5","Exp6","Exp7","Exp8"),
        labels=c("Benchmark","Exp. I","Exp. II","Exp. III"))+
        xlim(20,64)+
      labs(
        x="Age",
        y="On-the-job mortality rate (per 100K)",
        title="Low-skilled")+
      theme_bw()+
      theme(legend.position = c(0.85, 0.30),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    fig7b<-datanew %>% 
      subset(Productivity==unique(datanew$Productivity)[2]) %>% 
      group_by(Simu,Age2) %>%
      summarize(MortalityJ=mean(MortalityJob), .groups = "drop") %>%
      ggplot(aes(x=Age2,y=-100000*12*log(1-MortalityJ/100000)))+
      geom_line(aes(color=factor(Simu),linetype=factor(Simu)),cex=0.75)+
      scale_color_discrete(
        name="Simulation",
        breaks=c("Exp5","Exp6","Exp7","Exp8"),
        labels=c("Benchmark","Exp. I","Exp. II","Exp. III"))+
      scale_linetype_discrete(
        name="Simulation",
        breaks=c("Exp5","Exp6","Exp7","Exp8"),
        labels=c("Benchmark","Exp. I","Exp. II","Exp. III"))+
        xlim(20,64)+
      labs(
        x="Age",
        y="On-the-job mortality rate (per 100K)",
        title="High-skilled")+
      theme_bw()+
      theme(legend.position = "none",
            panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    fig7c<-datanew %>% 
      group_by(Simu,Productivity,Age2) %>%
      mutate(MortalityJ=mean(MortalityJob),
             Mort=-100000*12*log(1-MortalityJ/100000)) %>%
      group_by(Simu,Age2) %>%  
      summarize(Mort2=Mort[Productivity==unique(datanew$Productivity)[1]]-Mort[Productivity==unique(datanew$Productivity)[2]], .groups = "drop") %>%
      ggplot(aes(x=Age2,y=Mort2))+
      geom_line(aes(color=factor(Simu),linetype=factor(Simu)),cex=0.75)+
      scale_color_discrete(
        name="Simulation",
        breaks=c("Exp5","Exp6","Exp7","Exp8"),
        labels=c("Exp. V","Exp. VI","Exp. VII","Exp. VIII"))+
      scale_linetype_discrete(
        name="Simulation",
        breaks=c("Exp5","Exp6","Exp7","Exp8"),
        labels=c("Exp. V","Exp. VI","Exp. VII","Exp. VIII"))+
        xlim(20,64)+
      labs(
        x="Age",
        y="Rate (per 100K)",
        title="Difference btw low-skilled and high-skilled workers")+
      theme_bw()+
      theme(legend.position = "", #c(0.85, 0.30),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    fig7<-grid.arrange(fig7a, fig7b, fig7c, 
                 ncol = 1, nrow = 3, heights=c(3,3,3), widths=c(3))
    
    ggsave("../plots/Figure_7.png", fig7, width = 7.5*0.8, height =4.0*3*0.8, dpi=300)
    fig7
    """

end

