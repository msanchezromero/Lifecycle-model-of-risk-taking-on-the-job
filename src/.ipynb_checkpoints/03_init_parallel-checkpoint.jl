using SharedArrays
using Distributions
using Random
using Base.Threads

### Paralelization code

function Para_FVector(pθ,s,σy,λ,
                        z,τ,effwage,R,
                        Par_U::Params_Unmut,Par_M::Params_Mut)

    RetVec         = Par_M.R_sample
    πRVec          = Par_M.πR_sample
    ȳVec           = Par_M.y_sample
    
    N              = length(Par_M.R_sample)
    MaxLength::Int = (120-Par_U.E)*Par_U.ppy

    XLM            = zeros(MaxLength,N)
    XUM            = zeros(MaxLength,N)   
    
    Random.seed!(1234) # This script fixes the random sample in all the experiments   
    for i=1:N
        XLM[:,i] = rand(Bernoulli(1.0-s[i]),MaxLength,N)[:,i]
        XUM[:,i] = rand(Bernoulli(pθ[i]),   MaxLength,N)[:,i]        
    end
        
    cM    = SharedArray{Float64,2}(zeros(MaxLength,N))
    ylM   = SharedArray{Float64,2}(zeros(MaxLength,N))
    yM    = SharedArray{Float64,2}(zeros(MaxLength,N))
    asM   = SharedArray{Float64,2}(zeros(MaxLength,N))
    csurM = SharedArray{Float64,2}(zeros(MaxLength,N))
    surM  = SharedArray{Float64,2}(zeros(MaxLength,N))
    LHM   = SharedArray{Float64,2}(zeros(MaxLength,N))
    incM  = SharedArray{Float64,2}(zeros(MaxLength,N))
    VoLM  = SharedArray{Float64,2}(zeros(MaxLength,N))

# ----- Uncomment for parallelizing using Distributed ------    
    @sync @distributed for i=1:N  
# ------- Uncomment for parallelizing using Threads --------        
    #Threads.@threads for i=1:N  
        argF = (RetVec[i],πRVec[i],s[i],pθ[i],σy[i],λ[i],ȳVec[i],
                XLM[:,i],XUM[:,i],
                z,τ,effwage,R,
                Par_U,Par_M)
        
        cM[:,i], ylM[:,i], yM[:,i], asM[:,i], csurM[:,i], 
            surM[:,i], LHM[:,i], incM[:,i], VoLM[:,i]=Parallel(argF...)
    end

    return cM, ylM, yM, asM, csurM, surM, LHM, incM, VoLM
    
end;