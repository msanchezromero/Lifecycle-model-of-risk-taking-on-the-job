"""Elasticity output to on-the-job risk"""
function fσys(Scenario,Par_U::Params_Unmut,Par_M::Params_Mut)
    x      = zeros(length(Par_M.y_sample))
    y_mean = Par_M.y_mean 
    σy_l   = Par_U.σy_l
    σy_h   = Par_U.σy_h
    
    if Scenario>=11
        for (i,y) in enumerate(Par_M.y_sample)
            x[i] = (y<y_mean) ? σy_l : σy_h
        end    
    else
        x  .= Par_U.σy 
    end
    return x
end


"""Absence rate"""
function fλys(Scenario,Par_U::Params_Unmut,Par_M::Params_Mut)
    x      = zeros(length(Par_M.y_sample))
    y_mean = Par_M.y_mean 
    λ_l    = Par_U.λ_l
    λ_h    = Par_U.λ_h
    
    if Scenario>=11
        for (i,y) in enumerate(Par_M.y_sample)
            x[i] = (y<y_mean) ? λ_l : λ_h
        end    
    else
        x  .= Par_U.λ
    end
    return x
end


"""Rescale to be consistent with Shimer (2005)"""
function fss(Scenario,Par_M::Params_Mut)
    x      = zeros(length(Par_M.y_sample))
    y_mean = Par_M.y_mean 
    s_l    = Par_M.s_l
    s_h    = Par_M.s_h
    if Scenario>=11
        for (i,y) in enumerate(Par_M.y_sample)
            x[i] = (y<y_mean) ? s_l : s_h
        end    
    else
        x .= Par_M.s
    end
    return x
end

function fκs(Scenario,Par_U::Params_Unmut,Par_M::Params_Mut)
    x      = zeros(length(Par_M.y_sample))
    y_mean = Par_M.y_mean 
    κ_l    = Par_U.κ_l
    κ_h    = Par_U.κ_h
    if Scenario>=11
        for (i,y) in enumerate(Par_M.y_sample)
            x[i] = (y<y_mean) ? κ_l : κ_h
        end    
    else
        x .= Par_U.κ
    end
    return x
end