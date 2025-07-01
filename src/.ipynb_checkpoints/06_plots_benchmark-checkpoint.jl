function file_plots(Wage,Benefits,Con,A,Π,S,VoL,LabHist,Scenario,sample_size,Par_U::Params_Unmut)

    ParM  = mutable_struct(Scenario,sample_size,Par_U)

    
    E   = Par_U.E
    ppy = Par_U.ppy
    dt  = 1.0/ppy
    
    Ret = ParM.R_sample

    
    plt=plot(layout=(2,2),size=(850,800), margin=5Plots.mm)
    
    ############################ Wages ####################################
    plot!(20.0:120-(dt),Wage[1:ppy:(120-E)*ppy,:]./1000,
        xlim=(20,100),
        xlab="Age",
        color="red",
        alpha=0.1,
        label="",subplot=1)
    plot!(20.0:120-(dt),mean(Wage[1:ppy:(120-E)*ppy,:],dims=2)./1000,
        xlim=(20,100),
        xlab="Age",
        color="red",
        width=3,
        alpha=1.0,
        label="",subplot=1)
    plot!([22 30 40 50 60]',[1.97 2.38 2.67 2.74 2.71]',
        marker=:x,color="darkred",label="",
        subplot=1) 
    plot!([22 30 40 50 60]',[2.64 3.41 4.06 4.15 4.33]',
        marker=:x,color="darkred",label="",
        subplot=1)     
    #######################################################################    
    
    
    ############# Benefits (unemployment and retirement) ##################
    plot!(20.0:120-(dt),Benefits[1:ppy:(120-E)*ppy,:]./1000,
        xlim=(20,80),
        xlab="Age",
        color="orange",
        width=3,
        alpha=1.0,
        label="",subplot=1)
    #######################################################################    
    
    
    ############################ Consumption ##############################    
    plot!(20.0:120-(dt),Con[1:ppy:(120-E)*ppy,:]./1000,
        xlab="Age",
        ylab="Wage rate, Consumption",
        color="blue",
        alpha=0.1,
        label="",
        legend=:bottomright,subplot=1)
    plot!(20.0:120-(dt),mean(Con[1:ppy:(120-E)*ppy,:],dims=2)./1000,
        xlab="Age",
        ylab="Wage rate, Consumption (in 1000s)",
        color="blue",
        alpha=1.0,
        width=3,
        xlim=(20,80),
        ylim=(0.0,5.0),
        label="",
        legend=:bottomright,subplot=1)
    #######################################################################    
    
    
    ############################### Wealth ################################        
    plot!(20.0:dt:120-(dt),A[1:(120-E)*ppy,:]./1000,
        xlab="Age",
        ylab="Wealth (in 1000s)",
        color="gray",
        alpha=0.3,
        ylim=(-50.0,750.0),
        label="",subplot=2)
    plot!(20.0:dt:120-(dt),mean(A[1:(120-E)*ppy,:],dims=2)./1000,
        xlab="Age",
        ylab="Wealth (in 1000s)",
        color="black",
        width=3,
        alpha=1.0,
        label="",subplot=2)
    #######################################################################    
    
    
    ####################### Conditional mortality rate on the job ####################################
    AgeVector=Vector(20:dt:Ret[1])
    Πj       =ones((120-E)*ppy,sample_size)
    D_ojt    =ones(length(AgeVector),sample_size)
    M_ojt    =zeros(Ret[1]-20,sample_size)
    iter=1
    for age in AgeVector
        # Conditional survival probability on-the-job
        Πj[iter,:]       =Π[iter,:]./fpπ(age,dt,Scenario)
        # Expected number of deaths each month
        D_ojt[iter,:]   =S[iter,:].*(-log.(Πj[iter,:]))
        iter+=1
    end

    for t=1:(Ret[1]-20)
        # Mortality rate on-the-job among employed individuals 
        # (Expected deaths from risk taken on the job divided by expected people working each month)
        M_ojt[t,:]=(sum(D_ojt[(1+(t-1)*ppy):(t*ppy),:].*(LabHist.==1.0)[(1+(t-1)*ppy):(t*ppy),:],dims=1)./
        mean(S[(1+(t-1)*ppy):(t*ppy),:].*(LabHist.==1.0)[(1+(t-1)*ppy):(t*ppy),:],dims=1))
    end
    
    plot!(20:Ret[1]-1,100_000.0.*M_ojt,
        linetype=:scatter,
        color="gray",
        alpha=0.2,
        xlab="Age",
        ylab="Conditional mortality\n on the job (per 100k)",
        label="",subplot=3)
    plot!([22 30 40 50 60]',[2.23 2.61 2.87 3.49 4.46]',
        linetype=:scatter,color="red",label="",
        xlim=(20,70),
        #ylim=(1.5,5.5),
        subplot=3)
    plot!([22 30 40 50 60]',[3.545 4.014 4.606 5.454 6.661]',
        linetype=:scatter,color="blue",label="",
        xlim=(20,70),
        #ylim=(1.5,5.5),
        subplot=3)
    plot!([22 30 40 50 60]',[0.2656 0.4711 0.5554 0.5719 0.7783]',
        linetype=:scatter,color="blue",label="",
        xlim=(20,70),
        #ylim=(1.5,5.5),
        subplot=3)
    
    ##################################################################################################
    
    ######################### Value of Life ##############################
    plot!(20.0:dt:120-(dt),VoL[1:(120-E)*ppy,:]./1000000,
        xlab="Age",
            color="gray",
        alpha=0.3,
        ylim=(5.0,20.0),
        label="",subplot=4)
    plot!(20.0:dt:120-(dt),mean(VoL[1:(120-E)*ppy,:],dims=2)./1000000,
            xlab="Age",
            ylab="Value of Life (in millions)",
            color="black",
            width=3,
            alpha=1.0,
            label="",subplot=4)
    #######################################################################    
 

    return plt, Πj
end

