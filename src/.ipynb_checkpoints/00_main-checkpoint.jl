using Plots
using Distributed
using DataFrames
using ColorSchemes
using StatsPlots
using GLM
using MixedModels
using CSV
using JLD

using Random, LinearAlgebra, StatsBase

display(CaseScenario)

addprocs((CaseScenario==1) ? 2 : 0)
#println(Threads.nthreads())
display("Number of processors: "*string(nprocs()))

@everywhere path    = "../"

@everywhere include(path*"src/01_dictionaries.jl");
@everywhere include(path*"src/02a_param_struct.jl");
@everywhere include(path*"src/02b_parameters.jl");
@everywhere include(path*"src/02c_parameter_functions.jl");

@everywhere include(path*"src/03_init_parallel.jl");
@everywhere include(path*"src/04_functions_micro.jl");
@everywhere include(path*"src/05_equilibrium_prices.jl");
@everywhere include(path*"src/06_plots_benchmark.jl");
@everywhere include(path*"src/07_database.jl");



