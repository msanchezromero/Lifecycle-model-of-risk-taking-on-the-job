"""
Unmutable Parameters:
- Assets "Asset grid"
- ppy    "Periods per year";
- E      "Entry age";
- n      "Population growth rate";
- d      "Disutility of labor";
- χ      "Marginal impact of survival on the disutility of labor";
- β      "Intertemporal discounting";
- σC     "Inverse elasticity of substitution";
- σy     "Elasticity output to on-the-job risk";
- σy_l   "Elasticity output to on-the-job risk (low)";
- σy_h   "Elasticity output to on-the-job risk (high)";  
- λ      "Absenteeism";
- λ_l    "Absenteeism (low)";
- λ_h    "Absenteeism (high)";    
- αY     "Capital share";
- δY     "Capital depreciation rate";
- κ      "Prob finding a job";
- κ_l    "Prob finding a job (low)";
- κ_h    "Prob finding a job (high)";    
"""
struct Params_Unmut
    Assets::Vector{Float64}
    ppy::Int64
    E::Int64
    n::Float64
    d::Float64
    χ::Float64
    β::Float64
    σC::Float64
    σy::Float64
    σy_l::Float64
    σy_h::Float64
    λ::Float64
    λ_l::Float64
    λ_h::Float64
    αY::Float64
    δY::Float64
    κ::Float64
    κ_l::Float64
    κ_h::Float64
end;


"""
Mutable Parameters:
- Scenario "Scenario"
- y_mean   "Mean income"
- y_samp   "Income sample"
- R        "Exit age"
- R_samp   "Retirement sample"
- πR_samp  "Cond. survival (retirement) sample"
- πU       "Cond. survival (unemployed)"
- s        "Separation rate"
- s_l      "Separation rate (low)"
- s_h      "Separation rate (high)"
- ϕU       "Unemployment replacement"
- ϕR       "Retirement replacement"
"""
mutable struct Params_Mut
    Scenario::Int64
    y_mean::Float64
    y_sample::Vector{Float64}
    R::Int64
    R_sample::Vector{Int64}
    πR_sample::Vector{Float64}
    πU::Float64
    s::Float64
    s_l::Float64
    s_h::Float64
    ϕU::Float64
    ϕR::Float64
end
