using Random
using Interpolations

################################# READ ME ###################################
# This script includes functions for calculating the life cycle problem
#
#
#############################################################################



#CaseScenario=load("Experiment.jld")["ScenarioE"]

function fpπ(age,dt,Scenario)
    # This function returns the underlying survival probability for an individual
    if Scenario==6 || Scenario==16
        απ=exp(-9.63-0.16)
    elseif Scenario==7
        απ=exp(-9.63+0.16)
    else
        απ=exp(-9.63) 
    end
    βπ=0.081853
    
    y=exp(-(απ.*exp(βπ.*age))*dt)
    return y
end;

#################################################################################

function fRYL(Ret,πR,R,dt,Scenario)
    # This function calculates the Remaining Years Lived
    age=120.0
    C::Float64=0.0
    while age>=Ret
        C=1.0+fpπ(age,dt,Scenario)*πR*C./R
        age-=dt
    end
    return C
    end;

function fRC(Ret,πR,R,β,σC,dt,Scenario)
    # This function calculates the present value of the remaining consumption 
    # measured relative to the initial consumption level
    age=120.0
    C::Float64=0.0
    while age>=Ret
        C=(1.0+((R*β)^σC)*fpπ(age,dt,Scenario)*πR*C./R)
        age-=dt
    end
    return C
    end;


function fRU(c0,Ret,πR,R,β,dt,σC,E,Scenario)
    # This function calculates the welfare at age Ret 
    # measured relative to the initial consumption level at age Ret
    WorkLength =(Ret-E)*(1.0/dt);
    MaxLength  =(120-E)*(1.0/dt);
    C::Float64=0.0
    for t=1:(MaxLength-WorkLength)
        period=MaxLength+1-t
        C=fU(c0*(R*β)^(σC*(period-(WorkLength+1))),σC)+β*fpπ(period*dt+E,dt,Scenario)*πR*C         
    end
    return C
    end;

####################################################################################

###################### Exogenous age profile of productivity #######################
#### One group
# Data to generate the wage profile
age_group = [20:24;25:34;35:44;45:54;55:64]
wtws      = [0.72346161*ones(5,1);
                0.87528882*ones(10,1);
                1.0*ones(10,1);
                1.02632019*ones(10,1);
                1.03296964*ones(10,1)]
mtms      = [0.77646788*ones(5,1);
                0.90664890*ones(10,1);
                1.0*ones(10,1);
                1.21288279*ones(10,1);
                1.55270722*ones(10,1)];
X  = [ones(length(age_group),1) age_group age_group.^2.0]
Y  = (wtws.*exp.(λs.*mtms.*(2.87e-05))./(mtms.^σys))
βX = X\Y

#### Two groups (low and high)
# Low
wtws_l = [0.74057*ones(5,1);
            0.89417*ones(10,1);
            1.0*ones(10,1);
            1.02716*ones(10,1);
            1.01687*ones(10,1)]

mtms_l = [0.76961*ones(5,1);
            0.87146*ones(10,1);
            1.0*ones(10,1);
            1.18402*ones(10,1);
            1.44598*ones(10,1)];
Y_l    = (wtws_l.*exp.(λs_l.*mtms_l.*(4.606e-05))./(mtms_l.^σys_l))
βX_l   = X\Y_l
# High
wtws_h = [0.65183*ones(5,1);
            0.84144*ones(10,1);
            1.0*ones(10,1);
            1.02345*ones(10,1);
            1.06620*ones(10,1)]
mtms_h = [0.47827*ones(5,1);
            0.84819*ones(10,1);
            1.0*ones(10,1);
            1.02985*ones(10,1);
            1.40148*ones(10,1)];
Y_h    = (wtws_h.*exp.(λs_h.*mtms_h.*(0.5554e-05))./(mtms_h.^σys_h))
βX_h   = X\Y_h
####################################################################################


function fȳt(t,ȳ,ȳmean,Scenario)              
    if Scenario==11
        return (ȳ<ȳmean) ? ȳ*(βX_l[1]+βX_l[2]*t+βX_l[3]*(t^2.0)) : ȳ*(βX_h[1]+βX_h[2]*t+βX_h[3]*(t^2.0));
    else
        return ȳ*(βX[1]+βX[2]*t+βX[3]*(t^2.0));
    end
end    
    
fU(x,σC)        = (σC==1.0) ? log(max(0.01,x)) : (max(0.01,x)^(1.0-1.0/σC)-1.0)/(1.0-1.0/σC);
fUc(x,σC)       = 1.0/(max(0.01,x)^(1.0/σC));
fWR(cR,πR,σC)   = fU(cR,σC)/(1.0-πR);
fdWR(cR,σC)     = fUc(cR,σC);
fEWx(dWL,dWU,x) = (1.0-x)*dWL+x*dWU;
fUcx(EWx,R,β)   = R*β*EWx;
fc(Uc,σC)       = (Uc)^(-σC);
fdy(EWL,Uc,effwage,d,χ,β,τ,t,dt,Scenario) = (fpπ(t,dt,Scenario)/((1.0-τ)*effwage))*((d*χ+β*EWL)/Uc);

# ---------------------- New optimal m values --------------------
# Root finder
fmroot(m,dy,ȳt,σy,λ)  = m^(1.0-σy)-(σy-λ*m)*(ȳt/dy)*exp(-λ*m);

# First derivative of fmroot
fdmroot(m,dy,ȳt,σy,λ) = (1.0-σy)*m^(-σy)+(ȳt/dy)*λ*(1.0+σy-λ*m)*exp(-λ*m);

function fm(dy,ȳt,σy,λ;
        Err::Float64=1.0,
        Tol::Float64=1.0e-10,
        MaxIter::Int64=50,
        Iter::Int64=1)
    
        m0 = 1.0*(σy*ȳt/dy)^(1.0/(1.0-σy))
        for iter=1:MaxIter
            m1  = m0-fmroot(m0,dy,ȳt,σy,λ)/fdmroot(m0,dy,ȳt,σy,λ)
            Err = abs(m1-m0)

            if Err<Tol
                break
            end
            m0 = max(1e-8,min(m1,(σy/λ)*0.99))
        end

        return m0
    end;
# -------------------- New production function -------------------
fy(m,ȳt,σy,λ)           =ȳt*(m^σy)*exp(-λ*m);

# ----------------------------------------------------------------
#fm(dy,ȳt,σy)          =(σy*ȳt/dy)^(1.0/(1.0-σy));
#fy(m,ȳt,σy)           =ȳt*(m^σy);
fWL(U,EWL,π,β,d,χ)    =U-d*(1.0-χ*π)+β*π*EWL;
fWU(U,EWU,πU,β)       =U+β*πU*EWU;

function linegrid(Min,Max,GridSize)
   X=zeros(GridSize)
   Δ=(Max-Min)/(1.0+GridSize)
   X[1]=Min 
       for i=2:GridSize 
           X[i]=X[i-1]+Δ
       end     
   return X 
end;

function rootwageNoNash(t,ȳ,σy,λ,effwage,τ,Ewl,Ewu,ul,uu,Ucl,
                            Par_U::Params_Unmut,Par_M::Params_Mut)

    Scenario = Par_M.Scenario
    
    ppy      = Par_U.ppy
    dt       = 1.0/ppy
    E        = Par_U.E
    d        = Par_U.d
    χ        = Par_U.χ
    β        = Par_U.β
    πU       = Par_M.πU
    
    y_mean   = Par_M.y_mean
    
    age = (t-1)*dt+E
    πhat= fpπ(age,dt,Scenario)
    dy  = fdy(Ewl,Ucl,effwage,d,χ,β,τ,age,dt,Scenario)
    ȳt  = fȳt(age,ȳ,y_mean,Scenario)    
    mt  = fm(dy,ȳt,σy,λ)
    y   = fy(mt,ȳt,σy,λ)  
    w   = effwage*y
    WL  = fWL(ul,Ewl,πhat*(1.0-mt),β,d,χ)    
    WU  = fWU(uu,Ewu,πhat*πU,β)  
    return w, 1.0-mt, y, WL, WU
end;

function MatrixPreallocation(Ret,N)
    UcL = zeros(Ret,N)
    UcU = zeros(Ret,N)
    EWL = zeros(Ret,N)
    EWU = zeros(Ret,N)
    EJ  = zeros(Ret,N)
    CL  = zeros(Ret,N)
    CU  = zeros(Ret,N)
    WV  = zeros(Ret,N) #wage rate
    YV  = zeros(Ret,N)
    πV  = zeros(Ret,N)
    
    return UcL, UcU, EWL, EWU, CL, CU, WV, YV, πV
end;

function VectorPreallocation(N)
    
    ucl, ucu, cl, cu, ul, uu = zeros(N), zeros(N), zeros(N), zeros(N), zeros(N), zeros(N)
    wl, wu, ewl, ewu         = zeros(N), zeros(N), zeros(N), zeros(N)
    wv, yv, πv               = zeros(N), zeros(N), zeros(N) 
    al, au                   = zeros(N), zeros(N)
    
    return ucl, ucu, cl, cu, uu, wl, wu, ewl, ewu, wv, yv, πv, al, au
end;

function Interpol(x,y,Grid)
    # This function gives the new interpolated values of y for Grid 
    f   = LinearInterpolation(x, y,extrapolation_bc=Line())
    new = f.(Grid)
    return new
end;

fBoolean(x)= (x==false) ? 0.0 : 1.0;
Random.seed!(1234); # This script fixes the seed of all random samples generated.  
                    # In this way, we avoid having different samples inside loops


faf(a1,π,R,c,x)     =(R/π)*(a1+x-c); # Assets forward


function Parallel(Ret,πRr,s,pθ,σy,λ,ȳ,
                    XL,XU,
                    z,τ,effwage,R,
                    Par_U::Params_Unmut,Par_M::Params_Mut)

    Scenario        = Par_M.Scenario
    
    dt              = 1.0/Par_U.ppy    
    β               = Par_U.β
    σC              = Par_M.σC

    E               = Par_U.E
    MaxLength::Int  = (120-E)*Par_U.ppy
    WorkY::Int      = (Ret-E)*Par_U.ppy
    RealAge         = Ret
    working_ages    = range(1,WorkY) 
    retirement_ages = range(WorkY+1,MaxLength) 
    
    
    cM    = zeros(MaxLength,1)
    ylM   = zeros(MaxLength,1)
    yM    = zeros(MaxLength,1)
    asM   = zeros(MaxLength,1)
    csurM = zeros(MaxLength,1) 
    surM  = zeros(MaxLength,1) 
    LHM   = 2.0*ones(MaxLength,1)
    incM  = zeros(MaxLength,1)   
    VoLM  = zeros(MaxLength,1)

    πR    = 1.0-(1.0/fRYL(Ret,πRr,0.0,dt,Scenario))          # Cond. survival (retired)  
    Dz    = fRYL(Ret,πRr,R,dt,Scenario)
    Dc    =  fRC(Ret,πRr,R,β,σC,dt,Scenario)

    # The function returns the reaction functions
    res_reactions = ReactionFunctions(Ret,πRr,s,pθ,σy,λ,ȳ,
                                        z,τ,effwage,R,
                                        πR,Dz,Dc,
                                        Par_U,Par_M)
    cu  = res_reactions[1]
    cl  = res_reactions[2]
    w   = res_reactions[3]
    out = res_reactions[4]
    π   = res_reactions[5]
    ewl = res_reactions[6]
    ewu = res_reactions[7]

    # This function returns the optimal age profiles
    res_profiles  = profiles(cu,cl,w,out,π,ewl,ewu,
                                Ret,s,pθ,πR,z,τ,R,
                                XL,XU,
                                Par_U,Par_M)
        
       cM[working_ages,1] = res_profiles[1]
      ylM[working_ages,1] = res_profiles[2]
       yM[working_ages,1] = res_profiles[3]
      asM[working_ages,1] = res_profiles[4]
    csurM[working_ages,1] = res_profiles[5]
     surM[working_ages,1] = res_profiles[6]
     incM[working_ages,1] = res_profiles[7]
      LHM[working_ages,1] = res_profiles[8]
     VoLM[working_ages,1] = res_profiles[9]
        
        for t in retirement_ages.-1 
            cM[t,1]        = (z[t]*fRYL((t-1)*dt+E,πRr,R,dt,Scenario)[1]+asM[t,1])/fRC((t-1)*dt+E,πRr,R,β,σC,dt,Scenario)
            csurM[t,1]     = fpπ((t-1)*dt+E,dt,Scenario)*πRr       
            surM[t+1,1]    = surM[t,1]*csurM[t,1]      
            asM[t+1,1]     = (R/csurM[t,1])*(asM[t,1]+z[t]-cM[t,1])        
        end
    incM[retirement_ages,1] .= res_profiles[7][end]
    
    return cM, ylM, yM, asM, csurM, surM, LHM, incM, VoLM
    end;


function profiles(cu,cl,w,out,π,ewl,ewu,Ret,s,pθ,πR,z,τ,R,
                    XL,XU,
                    Par_U::Params_Unmut,Par_M::Params_Mut)

    Scenario = Par_M.Scenario
    
    AGrid    = Par_U.Assets
    πU       = Par_M.πU
    E        = Par_U.E
    ppy      = Par_U.ppy
    σC       = Par_M.σC
    
    WorkL    = (Ret-E)*ppy
    dt       = 1.0/ppy
    
    consum   = zeros(WorkL,1)
    labinc   = zeros(WorkL,1)    
    income   = zeros(WorkL,1)
    output   = zeros(WorkL,1)    
    condsu   = zeros(WorkL,1)   
    Lhist    = zeros(WorkL,1)    
    surviv   = ones(WorkL,1)
    assets   = zeros(WorkL,1)
    welfal   = zeros(WorkL,1)    
    welfau   = zeros(WorkL,1)
    voll     = zeros(WorkL,1) # Value of life    
    
    for t=1:(WorkL-1)
        welfal[t]  = Interpol(AGrid,ewl[t,:],assets[t])  
        welfau[t]  = Interpol(AGrid,ewu[t,:],assets[t])          
        consum[t]  = (Lhist[t]==0.0) ? Interpol(AGrid,cu[t,:],assets[t]) : Interpol(AGrid,cl[t,:],assets[t])
        labinc[t]  = (Lhist[t]==0.0) ? 0.0  : Interpol(AGrid,w[t,:],assets[t])
        income[t]  = (Lhist[t]==0.0) ? z[t] : Interpol(AGrid,(1.0.-τ).*w[t,:],assets[t])
        output[t]  = (Lhist[t]==0.0) ? 0.0  : Interpol(AGrid,out[t,:],assets[t])           
        condsu[t]  = (Lhist[t]==0.0) ? fpπ((t-1)*dt+E,dt,Scenario)*πU : fpπ((t-1)*dt+E,dt,Scenario)*Interpol(AGrid,π[t,:],assets[t])
        # We calculate the value of life
        voll[t]    = (Lhist[t]==0.0) ? welfau[t]/fUc(consum[t],σC) : welfal[t]/fUc(consum[t],σC)
        
        # Now we assume that all agents (firms and individuals) take the labor history as given
        Lhist[t+1] = (Lhist[t]==0) ? fBoolean(XU[t])[1] : fBoolean(XL[t])[1]               
        surviv[t+1]= surviv[t]*condsu[t]            
        assets[t+1]= faf(assets[t],condsu[t],R,consum[t],income[t])            
    end
    
    consum[WorkL]  = Interpol(AGrid,cl[WorkL,:],assets[WorkL])
    income[WorkL]  = z[WorkL]
    condsu[WorkL]  = fpπ((WorkL-1)*dt+E,dt, Scenario)*πR
    Lhist[WorkL]   = 2.0             
    
    return consum, labinc, output, assets, condsu, surviv, income, Lhist, voll
end;


function ReactionFunctions(Ret,πRi,s,pθ,σy,λ,ȳ,
                            z,τ,effwage,R,
                            πR,Dz,Dc,
                            Par_U::Params_Unmut,Par_M::Params_Mut)

    Scenario   = Par_M.Scenario
        
    AGrid      = Par_U.Assets
    E          = Par_U.E
    ppy        = Par_U.ppy
    σC         = Par_M.σC
    β          = Par_U.β
    πU         = Par_M.πU
    
    WorkLength = (Ret-E)*ppy; # Length of work
    dt         = 1.0/ppy
    t::Int64   = WorkLength-1
    
    Na         = length(AGrid)
    UcL, UcU, EWL, EWU, CL, CU, WV, YV, πV                     = MatrixPreallocation(WorkLength,Na)
    ucl, ucu, cl, cu, uu, wl, wu, ewl, ewu, wv, yv, πv, al, au = VectorPreallocation(Na)

    itp_cl     = Array{Any}(undef, WorkLength)
    itp_cu     = Array{Any}(undef, WorkLength)
    itp_w      = Array{Any}(undef, WorkLength)
    itp_y      = Array{Any}(undef, WorkLength)
    itp_π      = Array{Any}(undef, WorkLength)
    itp_ewl    = Array{Any}(undef, WorkLength)
    itp_ewu    = Array{Any}(undef, WorkLength)
    
    CL[t+1,:]  = (z[t+1].*Dz.+AGrid)./Dc # This is the consumption the first year of retirement
    CU[t+1,:]  = (z[t+1].*Dz.+AGrid)./Dc # This is the consumption the first year of retirement   
    UcL[t+1,:] = fUc.(CL[t+1,:],σC)
    UcU[t+1,:] = fUc.(CU[t+1,:],σC)
    ul         = fU.(CL[t+1,:],σC)
    uu         = fU.(CU[t+1,:],σC)
    wl         = fRU.(CL[t+1,:],Ret,πRi,R,β,dt,σC,E,Scenario)
    wu         = fRU.(CU[t+1,:],Ret,πRi,R,β,dt,σC,E,Scenario)
    EWL[t+1,:] = (1.0.-s).*wl.+s.*wu
    EWU[t+1,:] = pθ.*wl.+(1.0.-pθ).*wu
    πV[t+1,:] .= πR 
    
    for age=1:(WorkLength-1)
        t    =WorkLength-age
        ucl          = R.*β.*((1.0.-s).*UcL[t+1,:].+s.*UcU[t+1,:])
        ucu          = R.*β.*(pθ.*UcL[t+1,:].+(1.0.-pθ).*UcU[t+1,:])
        cl           = fc.(ucl,σC)
        cu           = fc.(ucu,σC)
        ul           = fU.(cl,σC)
        uu           = fU.(cu,σC)   
    
        for i=1:Na
            argsm    = (t,ȳ,σy,λ,effwage,τ,EWL[t+1,i],EWU[t+1,i],ul[i],uu[i],ucl[i],Par_U,Par_M)
            wv[i],πv[i],yv[i],wl[i],wu[i] = rootwageNoNash(argsm...)     
        end
    
        ewl          = (1.0.-s).*wl.+s.*wu
        ewu          = pθ.*wl.+(1.0.-pθ).*wu
        al           = (fpπ((t-1)*dt+E,dt,Scenario).*πv./R).*AGrid.+cl.-(1.0.-τ).*wv
        au           = (fpπ((t-1)*dt+E,dt,Scenario).*πU./R).*AGrid.+cu.-z[t];
        
        # Interpolation section in order to have always the same assets grid
        CL[t,:]      = Interpol(al,cl,AGrid);
        CU[t,:]      = Interpol(au,cu,AGrid);
        UcL[t,:]     = fUc.(CL[t,:],σC)
        UcU[t,:]     = fUc.(CU[t,:],σC)
        WV[t,:]      = Interpol(al,wv,AGrid);
        YV[t,:]      = Interpol(al,yv,AGrid);
        πV[t,:]      = Interpol(al,πv,AGrid);
        EWL[t,:]     = (1.0.-s).*Interpol(al,wl,AGrid).+s.*Interpol(au,wu,AGrid)
        EWU[t,:]     = pθ.*Interpol(al,wl,AGrid).+(1.0.-pθ).*Interpol(au,wu,AGrid) 
    end
    
    return CU, CL, WV, YV, πV, EWL, EWU    
end;
    
function fCobbDouglas(K,H,α,δ)
   # Production function 
   # capital per effective worker
    local x=K/H
    
    return α*(x^(α-1.0))-δ,  (1.0-α)*(x^α), H*(x^α)
end;

function fben(ϕU,ϕR,yl,E,S,Ini,ppy)
    # Calculation of the unemployment benefits and pension benefits
    MaxRet::Int=(65-Ini)*ppy # We fix the calculation to age 65 in all calculations

    return ϕU.*mean(yl,dims=2).+((ϕR.*sum(sum(yl[1:MaxRet,:].*S[1:MaxRet,:],dims=2))./sum(sum(S[1:MaxRet,:].*(E[1:MaxRet,:].==1),dims=2))).*mean((E.==2),dims=2));
end;

# Calculation of the social security contribution rate
fSocCon(yl,b,S)   =sum(sum(b.*S,dims=2))/sum(sum(yl.*S,dims=2));

function IPS(Par_U::Params_Unmut,Par_M::Params_Mut)
    # Initial Population Size per cohort
    N              = length(Par_M.R_sample)
    MaxLength::Int = (120-Par_U.E)*Par_U.ppy

    monthly_growth = (1.0+Par_U.n)
    
    IPop=ones(MaxLength,N)
    for i=1:MaxLength-1
       IPop[i+1,:]=IPop[i,:]./monthly_growth 
    end
    return IPop
end;


