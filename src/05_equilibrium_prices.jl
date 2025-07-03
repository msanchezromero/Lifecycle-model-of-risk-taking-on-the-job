function fequilibrium_prices(R0,tau0,Scenario,sample_size,Par_U::Params_Unmut)

    DumpingF  = (Model=="General") ? 0.10 : 0.0; # General or Partial equilibrium setting
    Tol       = 1e-4 # This is the convergence criterium of the model
    Err       = 1.0;
    
    # Initial values for the interest rate, wage rates, benefits, and social contribution rate
    Rs        = R0^(1.0/Par_U.ppy)
    wages     = (1.0-Par_U.αY)*(Par_U.αY/(Rs-1.0+Par_U.δY))^(Par_U.αY/(1.0-Par_U.αY))
    zv        = 1000.0*ones((120-Par_U.E)*Par_U.ppy,1)
    ben       = zeros((120-Par_U.E)*Par_U.ppy,sample_size);
    τs        = tau0 ; # General Equilibrium (benchmark value)

    ParM      = mutable_struct(Scenario,sample_size,Par_U)
    
    @time for i=1:50 # The maximum number of iterations is 50
        
        Hs    = 0.0 # Total Human Capital
        Ks    = 0.0 # Total Physical Capital
                
        ###############################################################################################
        #------------------------------------ ECONOMIC MODEL -----------------------------------------#
        ###############################################################################################
        
        # Population size at each age 
        PopC     = IPS(Par_U,ParM)
        
        
        argFinal = (fκs(Scenario,Par_U,ParM),
                        fss(Scenario,ParM),
                        fσys(Scenario,Par_U,ParM),
                        fλys(Scenario,Par_U,ParM),
                        zv,τs,wages,Rs,
                        Par_U,ParM)
                
        @time CM, YLM, YM, ASM, CSM, SM, LM, INCM, VOLM = Para_FVector(argFinal...);    
        
        ############# BENEFITS (Unemployment + Retirement) and Tax Rate ###################
        zv   = fben(ParM.ϕU,ParM.ϕR,YLM,LM,SM.*PopC,Par_U.E,Par_U.ppy)
        for i=1:(120-Par_U.E)*Par_U.ppy
            ben[i,:]=zv[i].*(LM[i,:].!=1)
        end
        ErrG = abs(fSocCon(YLM,ben,SM.*PopC)-τs)
        τs   = (Model=="Partial_Partial") ? τs : fSocCon(YLM,ben,SM.*PopC);
        
        ############################# MARKET PRICES #######################################
        Hs    = sum(sum(  (YM.*SM) .* (PopC./sample_size),dims=2)) # Labor stock
        Ks    = sum(sum( (ASM.*SM) .* (PopC./sample_size),dims=2)) # Capital stock
        
        ErrK  = abs(Rs-(1.0+fCobbDouglas(Ks,Hs,Par_U.αY,Par_U.δY)[1]))
        
        Rs    = Rs*(1.0-DumpingF)+(1.0+fCobbDouglas(Ks,Hs,Par_U.αY,Par_U.δY)[1])*DumpingF
        wages = (1.0-Par_U.αY)*(Par_U.αY/(Rs-1.0+Par_U.δY))^(Par_U.αY/(1.0-Par_U.αY))
        
        #################### GENERAL VS PARTIAL EQUILIBRIUM SETTING########################
        Err   = (Model=="General") ? ErrG+ErrK : ((Model=="Partial") ? ErrG : 0.0); 
        
        ###################################################################################

        display([i Err])
        
        if i>2 && (Err<Tol)
            
            display(["Iteration: $i";
                     "Capitalizataion factor: $(Rs^Par_U.ppy)";
                     "Contribution rate: $τs"; 
                     "Wage rate: $wages";
                     "Error Government Transfers: $ErrG";
                     "Error Capital $ErrK"; 
                     "Error Total $Err"])
            
           return CM, YLM, YM, ASM, CSM, SM, LM, INCM, VOLM, PopC, ben, wages, zv, τs, Rs
           break 
        end
        
    end

end