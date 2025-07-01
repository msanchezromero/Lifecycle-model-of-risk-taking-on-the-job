# ---------------------------- Demographic characteristics ---------------------------------
EntAge        = 20                    # Entry age
ppys          = 12                    # Periods per year
ns            = 1.00^(1.0/ppys)-1.0   # Population growth rate

# ----------------------------- Individuals' preferences -----------------------------------
ds            = 0.0                   # Disutility of labor
χs            = 1.0                   # Marginal impact of survival on the disutility of labor
βs            = 1.000^(1.0/ppys)      # Intertemporal discounting
σCs           = 0.8685                # Inverse elasticity of substitution (note: modify the program)

"""Sensitivity"""
Iter_Sensitivity = 1

# -------------------------------------- Firm ----------------------------------------------
αYs           = 0.33                  # Capital share
δYs           = 1.05^(1.0/ppys)-1.0   # Capital depreciation rate

# Elasticity output to on-the-job risk
σys_vector    = [0.0145,0.0145,0.0148,0.0158]   
σys           = σys_vector[Iter_Sensitivity]  # Elasticity output to on-the-job risk
σys_l         = 0.0210                # Elasticity output to on-the-job risk for low skills
σys_h         = 0.0025                # Elasticity output to on-the-job risk for high skills

# ----------------------------------- Absence rates ----------------------------------------
λs_vector     = [0.0,8.160833,13.90333,46.90417] 

# Absence rate + death rate
λs            = (51.07+1*λs_vector[Iter_Sensitivity])*12.0  
λs_l          = 48.86*12.0            # Absence rate for low skills
λs_h          = 85.36*12.0            # Absence rate for high skills

# --------------- Labor transition probabilities: Job finding rates ------------------------
κs            = 0.45;                 # Prob of finding a job
κs_l          = 0.4532;               # Prob of finding a job (low-skilled)
κs_h          = 0.4428;               # Prob of finding a job (high-skilled)

# ------------------------------------ Asset grid ------------------------------------------
# This is the grid of the potential range of assets held by individuals
AssetsGrid    = range(-5.0e+4,6.0e+5,length=400); 

ParU          = Params_Unmut(
                    AssetsGrid,
                    ppys,
                    EntAge,
                    ns,
                    ds,χs,βs,σCs,
                    σys,σys_l,σys_h,
                    λs,λs_l,λs_h,
                    αYs,δYs,
                    κs,κs_l,κs_h)

# Mutable struct function
function mutable_struct(Scenario,sample_size,Par_U::Params_Unmut)

    ppy = Par_U.ppy
    
    Rets   = if (Scenario==2 || Scenario==12) 
                70 
            else 
                65
            end
    
    # Fixed conditional survival probabilities
    # Eliason, M., & Storrie, D. (2009). Does job loss shorten life?. 
    # Journal of Human Resources, 44(2), 277-302.
    # For the sensitivity we assume that the value is lower by 0.90 or higher by 1.10
    πUs    = if Scenario==8
                exp(-(1.1/ppy)*0.007024615)
             elseif Scenario==9
                exp(-(0.9/ppy)*0.007024615)
             else
                exp(-(1.0/ppy)*0.007024615)    
             end

    # ------------------- Labor transition probabilities: Separation rates ------------------------
    # Shimer (2005) ->0.034 or Alternative 0.045
    ss            = (Scenario==3)  ? 0.045 : 0.034; 
    ss_l          = (Scenario==13) ? 0.0410*(0.045/0.034) : 0.0410;
    ss_h          = (Scenario==13) ? 0.0183*(0.045/0.034) : 0.0183;

    #----------------------------------------- Benefits -------------------------------------------
    ϕUs           = (Scenario==4 || Scenario==14) ? 0.50 : 0.40;  # Unemployment replacement
    ϕRs           = (Scenario==5 || Scenario==15) ? 0.50 : 0.40;  # Retirement replacement
    

    ### Sample size and initial heterogeneity
    Random.seed!(1234) # This script fixes the random sample in all the experiments
    
    # ------------------------------------ Income data ------------------------------------
    ȳ_mean        = (exp(6.60)/1.0595674074074075)    
    #"""1.5215 $4057 for high skill relative to $2666.5 for low skill in the age group 35-44"""
    ȳ_highlow     = (Scenario==10) ? 1.5215 : ((Scenario>10) ? 1.25 : 1.00 );  
    #"""88.5 million workers in the low skill group out of 127.9 million workers"""
    α_low         = 0.692 
    ȳ_low         = ȳ_mean/(α_low+(1.0-α_low)*ȳ_highlow)
    ȳ_high        = ȳ_highlow*ȳ_low
    ȳ_Sample      = sample([ȳ_low,ȳ_high],Weights([α_low,1.0-α_low]),sample_size)    
    
    # ---------------------- Conditional mortality after retirement -----------------------
    πR_Sample     = (ss*πUs+(1.0-ss)*exp(-(4.5/100000)/ppy)).*ones(sample_size)
    
    # --------------------------------- Retirement ages -----------------------------------
    RetAge_Sample = rand(Rets:Rets,sample_size);
    
    return Params_Mut(Scenario,
                        ȳ_mean,
                        ȳ_Sample,
                        Rets,
                        RetAge_Sample,
                        πR_Sample,
                        πUs,
                        ss,ss_l,ss_h,
                        ϕUs,ϕRs)
end
