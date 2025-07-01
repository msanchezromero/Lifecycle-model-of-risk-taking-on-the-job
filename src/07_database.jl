function DatabaseGenerator(Con,Out,Wage,Income,Πj,Π,S,A,VoL,LabHist,
                                τ,Benefits,Scenario,sample_size,Par_U::Params_Unmut)
      
    ParM  = mutable_struct(Scenario,sample_size,Par_U)

    Ret   = ParM.R_sample
    ȳ     = ParM.y_sample
    πR    = ParM.πR_sample
    s     = fss(Scenario,ParM)#fss(ȳ,ParM.y_mean)
    pθ    = fκs(Scenario,Par_U,ParM)#fκs(ȳ,ParM.y_mean)
    
    
    WorkLength, N0 = size(Wage)
    # We first define the dimension of the array where we will store the data
    M=Array{Any,2}(undef,N0*WorkLength,19)
    iter=1
    for i=1:N0
        for t=1:WorkLength
            M[iter,:]=[i t Wage[t,i] Income[t,i] 100_000*(1.0-Π[t,i]) 100_000*(1.0-Πj[t,i]) A[t,i] VoL[t,i] LabHist[t,i] Con[t,i] Out[t,i] S[t,i] Ret[i] ȳ[i] πR[i] s[i] pθ[i] τ Benefits[t,i]];
            iter+=1
        end
    end

    df=DataFrame(Ind=M[:,1], Age=M[:,2], Wages=M[:,3], Income=M[:,4], Mortality=M[:,5], MortalityJob=M[:,6], Assets=M[:,7], VOL=M[:,8], Employment=M[:,9], Consumption=M[:,10], Output=M[:,11], Survival=M[:,12], Retire=M[:,13], Productivity=M[:,14], SurvRet=M[:,15], Sep_Rate=M[:,16], Empl_Pr=M[:,17], Tax=M[:,18], Benefit=M[:,19]);

    df[!,:Age]         =convert.(Int64,  df[!,:Age])    
    df[!,:Wages]       =convert.(Float64,df[!,:Wages])
    df[!,:Mortality]   =convert.(Float64,df[!,:Mortality])
    df[!,:MortalityJob]=convert.(Float64,df[!,:MortalityJob])    
    df[!,:Assets]      =convert.(Float64,df[!,:Assets])
    df[!,:VOL]         =convert.(Float64,df[!,:VOL])    
    df[!,:Consumption] =convert.(Float64,df[!,:Consumption])
    df[!,:Output]      =convert.(Float64,df[!,:Output])
    df[!,:Income]      =convert.(Float64,df[!,:Income])    
    df[!,:Survival]    =convert.(Float64,df[!,:Survival])    
    df[!,:Retire]      =convert.(Int64,  df[!,:Retire])    
    df[!,:Productivity]=convert.(Float64,df[!,:Productivity])    
    df[!,:SurvRet]     =convert.(Float64,df[!,:SurvRet])        
    df[!,:Sep_Rate]    =convert.(Float64,df[!,:Sep_Rate])    
    df[!,:Empl_Pr]     =convert.(Float64,df[!,:Empl_Pr])    
    df[!,:Tax]         =convert.(Float64,df[!,:Tax])    
    df[!,:Benefit]     =convert.(Float64,df[!,:Benefit])        
    return df
end;