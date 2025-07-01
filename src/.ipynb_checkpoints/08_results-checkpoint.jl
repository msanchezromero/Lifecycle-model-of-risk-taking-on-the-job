using RCall

# This file contains the code for figures 4, 5, 6, 7 and tables 2, 3, 4, 5 in the paper

R"""
library("dplyr")
Sigma  <- c("0.8685")
Lambda <- c("_Lambda612.84","_Lambda710.769996","_Lambda779.67996","_Lambda1175.69004")

Simulations <- c("benchmark",
                   "hretage70",
                   "hretben50",
                   "sLEhigh",
                   "TwoGroups",
                   "TwoGroupsS2",
                   "TwoGroupsS5",
                   "TwoGroupsS6")
Simu <- c("Benchmark","Exp1","Exp2","Exp3","Exp5","Exp6","Exp7","Exp8")

path <-"../results/Results_Sigma"

dfT  <- c()
for (i in 1:length(Simulations)){
  df      <- read.csv(file=paste0(path,Sigma[1],Lambda[1],"_",Simulations[i],".csv"))
  df$Simu <- Simu[i]
  dfT     <- rbind(dfT,df)
}
dfT  <- dfT %>% mutate(Age2=20+(Age-1)/12)
"""
@rget dfT;


# -------------------------------------------------------------------------------------
# Table 2: 
# -------------------------------------------------------------------------------------
function f_results_table2(data)
    
    @rput data
    R"""
    suppressWarnings(
    install.packages("stargazer", repos = "https://cloud.r-project.org")
    install.packages("kableExtra", repos = "https://cloud.r-project.org")
    )
    library("stargazer")
    library(tidyr)
    library(dplyr)
    library(kableExtra)
    
        
    # All ages
     tb2.all1<-dfT %>% 
       mutate(Mx=-100000*12*log(1-MortalityJob/100000)) %>%
       subset(Employment==1 & (Age2 %in% c(seq(20,65))) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~MortalityJob+factor(Age2)-1) %>% 
        summary()
    
    tb2.all2<-dfT %>% 
      subset(Employment==1 & (Age2 %in% c(seq(20,64))) & (Simu=="Benchmark")) %>% 
      summarize(
        w=mean(Wages),
        VSL=tb2.all1$coefficients[1,1]*w*0.1,
        VOL=mean(VOL)/1000000) 
    
    tb2.all<-cbind(t(tb2.all1$coefficients[1,1:2]),data.frame(tb2.all2))
      
    # Age 40
    tb2.40_1<-dfT %>% 
      mutate(Mx=-100000*12*log(1-MortalityJob/100000)) %>%
      subset(Employment==1 & (Age2==40) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~MortalityJob) %>% 
        summary()
    
    tb2.40_2<-dfT %>% subset(Employment==1 & (Age2==40) & (Simu=="Benchmark")) %>% 
      summarize(
        w=mean(Wages),    
        VSL=tb2.40_1$coefficients[2,1]*w*0.1,
        VOL=mean(VOL)/1000000)
    
    tb2.40<-cbind(t(tb2.40_1$coefficients[2,1:2]),data.frame(tb2.40_2))
    
    # Age 50
    tb2.50_1<-dfT %>% 
      mutate(Mx=-100000*12*log(1-MortalityJob/100000)) %>%
      subset(Employment==1 & (Age2==50) & (Simu=="Benchmark")) %>% 
    lm(formula=log(Wages)~MortalityJob) %>% summary()
    tb2.50_2<-dfT %>% 
      subset(Employment==1 & (Age2==50) & (Simu=="Benchmark")) %>% 
      summarize(
        w=mean(Wages),
        VSL=tb2.50_1$coefficients[2,1]*w*0.1,
        VOL=mean(VOL)/1000000)
    
    tb2.50<-cbind(t(tb2.50_1$coefficients[2,1:2]),data.frame(tb2.50_2))
    
    # Age 60
    tb2.60_1<-dfT %>% 
      mutate(Mx=-100000*12*log(1-MortalityJob/100000)) %>%
      subset(Employment==1 & (Age2==60) & (Simu=="Benchmark")) %>% 
    lm(formula=log(Wages)~MortalityJob) %>% summary()
    tb2.60_2<-dfT %>% 
      subset(Employment==1 & (Age2==60) & (Simu=="Benchmark")) %>% 
      summarize(
        w=mean(Wages),
        VSL=tb2.60_1$coefficients[2,1]*w*0.1,
        VOL=mean(VOL)/1000000)
    
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
      format="simple") #%>%
      #kableExtra:::kable_classic(full_width = F, html_font = "Cambria")
    
    """

end


# -------------------------------------------------------------------------------------
# Table 3: 
# -------------------------------------------------------------------------------------
function f_results_table3(data)
    
    @rput data
    R"""
    suppressWarnings(
    install.packages("stargazer", repos = "https://cloud.r-project.org")
    )
    library("stargazer")
    library(tidyr)
    library(dplyr)
    
    # log(MortalityJob)~log(Assets)
    # Age 50
    ols.1<-dfT %>% 
        subset(Employment==1 & (Age2==50) & (Simu=="Benchmark")) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    # Age 55
    ols.2<-dfT %>% 
        subset(Employment==1 & (Age2==55) & (Simu=="Benchmark")) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    # Age 60
    ols.3<-dfT %>% 
        subset(Employment==1 & (Age2==60) & (Simu=="Benchmark")) %>% 
        lm(formula=log(MortalityJob)~log(Assets)) 
    
    # log(Wages)~log(Assets)
    # Age 50
    olsW.1<-dfT %>% 
        subset(Employment==1 & (Age2==50) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~log(Assets))
    # Age 55
    olsW.2<-dfT %>% 
        subset(Employment==1 & (Age2==55) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~log(Assets))
    # Age 60
    olsW.3<-dfT %>% 
        subset(Employment==1 & (Age2==60) & (Simu=="Benchmark")) %>% 
        lm(formula=log(Wages)~log(Assets))
    
    stargazer(ols.1,  ols.2,  ols.3,  title="Results", align=TRUE, type="text", out.header=TRUE)
    stargazer(olsW.1, olsW.2, olsW.3, title="Results", align=TRUE, type="text", out.header=TRUE)
    
    """

end

# -------------------------------------------------------------------------------------
# Table 4: 
# -------------------------------------------------------------------------------------
function f_results_table4(data)
    
    @rput data
    R"""
    suppressWarnings(
    install.packages("stargazer", repos = "https://cloud.r-project.org")
    install.packages("kableExtra", repos = "https://cloud.r-project.org")
    )
    library("stargazer")
    library(tidyr)
    library(dplyr)
    library(kableExtra)
    
    sigmaG = as.numeric(Sigma) # This is the value of sigma_G
    tbl6<-dfT %>% subset(Age<1200 & !(Simu %in% c("Exp4","Exp5","Exp6","Exp7","Exp8"))) %>%
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
        summarize(Lambda=round(100*mean(Li),2)) %>% spread(Simu, Lambda) 
    
    kableExtra:::kbl(
      tbl6,
      booktabs = T,
      caption="Table 6: Welfare effects of pension reforms and ageing by exact age. Ref=Benchmark (mean values, in %)",
      format="simple") #%>%
      #kableExtra:::kable_classic(full_width = F, html_font = "Cambria")
    """

end


# -------------------------------------------------------------------------------------
# Table 5: 
# -------------------------------------------------------------------------------------
function f_results_table5(data,Par_U::Params_Unmut)
    
    sigmaG = Par_U.σC
    @rput sigmaG
    @rput data
    R"""
    suppressWarnings(
    install.packages("stargazer", repos = "https://cloud.r-project.org")
    install.packages("kableExtra", repos = "https://cloud.r-project.org")
    )
    library("stargazer")
    library(tidyr)
    library(dplyr)
    library(kableExtra)
        
    tbl6b<-dfT %>% subset(Age<1200 & !(Simu %in% c("Benchmark","Exp1","Exp2","Exp3","Exp4"))) %>%
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
        subset(Age2 %in% c(20,30,40,50,60)) %>%
      group_by(Simu,Productivity,Age2) %>% 
        summarize(Lambda=round(100*mean(Li),2)) %>% spread(Simu, Lambda) 
    
    kableExtra:::kbl(
      tbl6b,
      booktabs = T,
      caption="Table 6b: Welfare effects of pension reforms and ageing by exact age. Ref=Benchmark (mean values, in %)",
      format="simple") #%>%
      #kableExtra:::kable_classic(full_width = F, html_font = "Cambria")
    
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
    
    inc.data<-data.frame(inc.ages,inc.wage,inc.wagelow,inc.wagehigh)
    
    x.a<-c(22,30,40,50,60)
    y.a<-c(22504,42702,55620,60126,58569)
    y.b<-c(2.23,2.61,2.87,3.49,4.46)
    y.l<-c(3.5452,4.0143,4.6064,5.4541,6.6608)
    y.h<-c(0.2656,0.4711,0.5554,0.5719,0.7783)
    
    d<-data.frame(x.a,y.a,y.b,y.l,y.h)
    
    fig4.A<-dfT %>% 
      subset((Employment==1) & (Simu=="Benchmark")) %>% 
      ggplot(aes(x=Age2,y=Wages))+
      geom_line(color="gray")+
      geom_point(data=inc.data,
                 aes(x=inc.ages,y=inc.wage),color="red",cex=2)+
      labs(x="Age",y="Monthly wage rate (in $)",title="A. Wage")+
      theme_bw()+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),
        )
    
    fig4.B<-dfT %>% subset(Simu=="Benchmark") %>% 
      ggplot(aes(x=Age2,y=Assets/1000))+
      geom_line(color="gray")+
      labs(x="Age",y="Wealth (in $1K)",title="B. Wealth")+
      theme_bw()+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    fig4.C<-dfT %>% 
      subset((Employment==1) & (Simu=="Benchmark")) %>%
      ggplot(aes(x=Age2,y=-100000*12*log(1-MortalityJob/100000)))+
      geom_line(color="gray")+
      geom_point(data=d,aes(x=x.a,y=y.b),color="red",cex=3)+
      xlim(20,64)+
      labs(x="Age",y="On-the-job mortality rate (per 100k)",title="C. On-the-job mortality")+
      theme_bw()+
      theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    fig4<-grid.arrange(fig4.A, fig4.B, fig4.C,  
                 ncol = 3, nrow = 1, heights=c(2),
                 widths=c(2,2,2))

    ggsave("../plots/Figure_4.png", fig4, width = 21*0.6, height = 7*0.6, dpi=300)
    fig4    
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
    
    fig5.A<-dfT %>% 
      subset((Age2 %in% c(50,55,60)) & (Employment==1) & (Simu=="Benchmark")) %>% 
      ggplot(aes(x=Assets/1000,
                 y=-100000*12*log(1-MortalityJob/100000)
                 #MortalityJob
                 ))+
      geom_point(aes(color=as.factor(Age2)))+
      scale_colour_brewer("Exact age")+
      labs(x="wealth (in $1K)",y="on-the-job mortality (per 100K)",title="A. On-the-job mortality")+
      theme_bw()+
      theme(legend.position = c(0.2, 0.85),
            panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
    fig5.B<-dfT %>% 
      subset((Age2 %in% c(50,55,60)) & (Employment==1) & (Simu=="Benchmark")) %>% 
      ggplot(aes(x=Assets/1000,y=Wages))+
      geom_point(aes(color=as.factor(Age2)))+
      scale_colour_brewer("Exact age")+
      labs(x="wealth (in $1K)",y="wage (in $)",title="B. Wage")+
      theme_bw()+
      theme(legend.position = "none",
            panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
    
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
    
    
    fig6<-dfT %>% 
    subset(Employment==1 & !(Simu %in% c("Exp4","Exp5","Exp6","Exp7","Exp8"))) %>% 
    mutate(Simulation=Simu) %>% 
    group_by(Simulation,Age2) %>% 
    summarize(MortalityJ=mean(MortalityJob)) %>% 
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
    
    dfTnew<-dfT %>% 
      subset(Employment==1 & 
               (Simu %in% c("Exp5","Exp6","Exp7","Exp8")))
    
    fig7a<-dfTnew %>% 
      subset(Productivity==unique(dfTnew$Productivity)[1]) %>% 
      group_by(Simu,Age2) %>%
      summarize(MortalityJ=mean(MortalityJob)) %>%
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
    
    fig7b<-dfTnew %>% 
      subset(Productivity==unique(dfTnew$Productivity)[2]) %>% 
      group_by(Simu,Age2) %>%
      summarize(MortalityJ=mean(MortalityJob)) %>%
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
    
    fig7c<-dfTnew %>% 
      group_by(Simu,Productivity,Age2) %>%
      mutate(MortalityJ=mean(MortalityJob),
             Mort=-100000*12*log(1-MortalityJ/100000)) %>%
      group_by(Simu,Age2) %>%  
      summarize(Mort2=Mort[Productivity==unique(dfTnew$Productivity)[1]]-Mort[Productivity==unique(dfTnew$Productivity)[2]]) %>%
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

