# Lifecycle model of risk taking on the job
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15845425.svg)](https://doi.org/10.5281/zenodo.15845425)
This model 


**Program simulations**
This program generates the economic results for the following set of simulations:

| Number | Column name   | Explanation                                                           |
|:-------|:--------------|:----------------------------------------------------------------------|
| 1      | benchmark     | Benchmark                                                             |
| 2      | hretage70     | Retirement age 70 rather than 65                                      |
| 3      | hseprate      | Separation rate of 4.5\% rather than 3.4\%                              |
| 4      | hunemben50    | Unemployment benefits replacement rate of 50\% rather than 40\%.        |
| 5      | hretben50     | Retirement benefits replacement rate of 50\% rather than 40\%           |
| 6      | sLEhigh       | Higher LE                                                             |
| 7      | sLElow        | Lower LE                                                              | 
| 8      | sUmorthigh    | 10\% higher conditional mortality in unemployment                      |
| 9      | sUmortlow     | 10\% lower conditional mortality in unemployment                       |
| 10     | productivity  | Two groups: One with low and another one with high productivity       |
| 11     | TwoGroups(1)  | Two groups: low and high productivity, low and high income profile,   |
|        |               | low and high market transitions                                       |
| 12     | TwoGroups(2)  | Two groups and S2: low and high productivity, low and high income     |
|        |               | profile, low and high market transitions                              |
| 13     | TwoGroups(3)  | Two groups and S3: low and high productivity, low and high income     |
|        |               | profile, low and high market transitions                              |
| 14     | TwoGroups(4)  | Two groups and S4: low and high productivity, low and high income     |
|        |               | profile, low and high market transitions                              |
| 15     | TwoGroups(5)  | Two groups and S5: low and high productivity, low and high income     |
|        |               | profile, low and high market transitions                              |
| 16     | TwoGroups(6)  | Two groups and S6: low and high productivity, low and high income     |
|        |               | profile, low and high market transitions                              |


**Program structure**

Use the main Jupyter notebook to generate the simulation results and obtain Figures 4,5,6,7 and Tables 2,3,4,5

```text
Model/
├── notebooks/                                              # Jupyter notebooks
|   ├── A_lifecycle_model_of_risk_taking_on_the_job.ipynb   # Working notebook
│   └── ModelFitting_Results.Rmd                            # Rmarkdown showing the main results (backup code/not used)
│
├── src/                                                    # Source code 
│   └── 00_main.jl                                          # Main module
│        ├── 01_dictionaries.jl                             # Defines the dictionaries for all the simulations
│        ├── 02a_param_struct.jl                            # Sets the struct for the parameters
│        ├── 02b_parameters.jl                              # Sets the parameter values
│        ├── 02c_parameter_functions.jl                     # Extra functions necessary for setting the individual parameters
│        ├── 03_init_parallel.jl                            # Parallelization function
│        ├── 04_functions_micro.jl                          # Microeconomic functions
│        ├── 05_equilibrium_prices.jl                       # Computes macroeconomic aggregates and find equilibirum prices
│        ├── 06_plots_benchmark.jl                          # Plots main results of the benchmark simulation
│        ├── 07_database.jl                                 # Generates the results database
│        ├── 08a_results_dataframe.jl                       # Compile all dataframes
│        └── 08b_results_tables_and_figures.jl              # Generates results for Figures 4,5,6,7 and Tables 2,3,4,5
│
├── replication files/                                      # R codes for the econometric estimations
│
├── results/                                                # Output datasets (all csv/rds files)
│
├── plots/                                                  # Output plots of the article
└── README.txt
```

