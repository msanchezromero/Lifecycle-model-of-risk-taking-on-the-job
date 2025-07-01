#----------------------- SCENARIOS ----------------------------

Scenarios_Dict = Dict{Integer,String}(
1  => "benchmark",   # Benchmark
2  => "hretage70",   # Retirement age 70 rather than 65
3  => "hseprate",    # Separation rate of 4.5% rather than 3.4%
4  => "hunemben50",  # Unemployment benefits replacement rate of 50% rather than 40%
5  => "hretben50",   # Retirement benefits replacement rate of 50% rather than 40%
6  => "sLEhigh",     # Higher LE
7  => "sLElow",      # Lower LE
8  => "sUmorthigh",  # 10% higher conditional mortality in unemployment 
9  => "sUmortlow",   # 10% lower conditional mortality in unemployment
10 => "productivity",# Two groups: One with low and another one with high productivity
11 => "TwoGroups",   # Two groups: low and high productivity, low and high income profile, low and high market transitions
12 => "TwoGroupsS2", # Two groups and S2: low and high productivity, low and high income profile, low and high market transitions
13 => "TwoGroupsS3", # Two groups and S3: low and high productivity, low and high income profile, low and high market transitions
14 => "TwoGroupsS4", # Two groups and S4: low and high productivity, low and high income profile, low and high market transitions
15 => "TwoGroupsS5", # Two groups and S5: low and high productivity, low and high income profile, low and high market transitions
16 => "TwoGroupsS6", # Two groups and S6: low and high productivity, low and high income profile, low and high market transitions
)


#------------ GENERAL VS PARTIAL EQUILIBRIUM ------------------

ModelVector = Dict{String,String}( 
"General"         => "G_",  # "G_"  General equilibrium
"Partial"         => "F_",  # "F_"  Partial equilibrium setting
"Partial_Partial" => "PP_", # "PP_" # Partial equilibrium setting plus fixed social contribution
)



