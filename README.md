## Welcome to the EcoEvoApps R package

Our goal with this project is to make a compilation of interactive ([shiny](https://shiny.rstudio.com/)) apps to help teach ecology and evolution models.

## Shiny apps

The package currently includes 11 shiny apps, the code for which is found in `inst/`:

1. Population growth in continuous time: https://ecoevoapps.shinyapps.io/single-population-continuous/  
Users can simulate the dynamics of continuously growing populations with either exponential or logistic growth. Users can optionally introduce a lag in the logistic growth model.  

2. Population growth in discrete time: https://ecoevoapps.shinyapps.io/single-population-discrete/  
Users can simulate the dynamics of a population with discrete exponential growth, or discrete growth with a carrying capacity. Users can choose between a standard discrete logistic model, the Beverton-Holt model, or the Ricker model.  

3. Stage-structred population growth: https://ecoevoapps.shinyapps.io/structured_population/  
This app simulates the dynamics of a stage-structured population. Users can set the survival and fecundity rates of populations with up to three life stages.  

4. Metapopulation dynamics model:  https://ecoevoapps.shinyapps.io/source-sink-dynamics/  
Users can explore the dynamics of a population that grows in a favorable habitat ("source"), but can also immigrate to a less suitable "sink" habitat. This is an implementation of the classic metapopulation model of Pulliam (1988).  

5. Offspring size-fitness tradeoff: https://ecoevoapps.shinyapps.io/smith_fretwell_app/  
Users can explore the life history tradeoff between making many small offspring with low survival, vs. fewer large offspring with higher survival rate. It implements the classic model of Smith and Fretwell (1974).  

6. Consumer-resource dynamics: https://ecoevoapps.shinyapps.io/consumer-resource-dynamics/   
Users can explore the dynamics between a consumer and resource. The consumer can either have a Type I or Type II functional response, and the resource can either experience exponential or logistic growth.  

7. Lotka-Volterra competition: https://ecoevoapps.shinyapps.io/lotka-volterra-competition/   
Users can explore the classic Lotka-Volterra competition model. The model can either be simulated in terms of absolute inter- and intra-specific competition coefficients (as advocated by Chesson (2000)), or in terms of the relative strength of intra vs. interspecific competition (as is presented in most texts).  

8. Competition for a biotic resource: https://ecoevoapps.shinyapps.io/biotic-resource-competition/  
Users can evaluate the outcome of competition between two species that compete for the same biotic resource (e.g. two predators that compete for the same prey species). This app can be used to illustrate how relative nonlinearities of competition can promote species coexistence. 

9. Competition for abiotic resources: https://ecoevoapps.shinyapps.io/abiotic_resource_competition/  
Users can evaluate the outcome of competition between two species that compete for the same abiotic resources (e.g. two plant species that compete for the same limiting resource). This app can be used to illustrate the classic R* rule of Tilman (1980). 

10. SIR dynamics models: https://ecoevoapps.shinyapps.io/infectious-disease-dynamics/  
Users can explore various "compartment models" of disease dynamics, which illustrate how various key parameters like disease transmission rate, survival, and recovery rate affect the spread of a disease through a population. This app can also be used to illustrate how vaccination alters the rate of disease spread.   

11. Island biogeography: https://ecoevoapps.shinyapps.io/island-biogeography/  
Users can evaluate how variation in island size and distance from a mainland source influence the equilibrium diversity of different islands in this app, which implements the classic models of Macarthur and Wilson (1964). 

## Installation and using EcoEvoApps directly from `R`

In addition to directly accessing the shiny apps at the links above, the package also includes a variety of functions with which users can directly simulate model dynamics through `R`, or can launch the shiny apps locally from their own machines. 

#### Installation

`ecoevoapps` can be installed directly from Gitlab (or its mirror on Github), as follows:

```
if (!requireNamespace("remotes", quietly = TRUE))
      install.packages('remotes')
remotes::install_gitlab("ecoevoapps/ecoevoapps")

# Or, install from github
# (This can be helpful if you plan on deploying the shiny apps to your own
# shinyapps.io account, as packages installed from gitlab don't automatically
# get installed during the shinyapps.io deployment.)
# remotes::install_github("gauravsk/ecoevoapps-mirror")
```

#### Launching apps from R 

The shiny apps in `ecoevoapps` can be launched directly from the R console using one of the `run_XXX()` functions used in the package. For example, the island biogeography package can be run using `ecoevoapps::shiny_island_biogeo()`.  The full list of such functions is:

```
"shiny_abiotic_comp"         "shiny_biotic_comp"          "shiny_consumer_resource"    "shiny_infectious_disease"  
"shiny_island_biogeo"        "shiny_lvcompetition"        "shiny_metapopulation"       "shiny_singlepop_continuous"
"shiny_singlepop_discrete"   "shiny_smith_fretwell"       "shiny_structured_pop"      
```

#### Running models from the R command line

The models listed above can also be run directly from the `R` command line using functions built into `ecoevoapps`. For example, the Lotka-Volterra competition model can be run using `run_lvcomp_model()`:

```
ecoevoapps::run_lvcomp_model(time = 100, params = c(K1 = 500, K2 = 500, a = .07, 
                                                      b = .07, r1 = .6, r2 = .8), 
                               init = c(N1 = 100, N2 = 50))
```

The following functions are available to run the models from `R`:

```
"run_beverton_holt_model"     "run_discrete_exponential"    "run_discrete_logistic_model" "run_exponential_model"      
"run_ibiogeo_model"           "run_infectiousdisease_model" "run_logistic_model"          "run_lvcomp_model"           
"run_predprey_model"          "run_ricker_model"           
```

## Contributing

*Text pending. In the mean while, if you'd like to contribute, please check out our [website](https://ecoevoapps.gitlab.io/contribute/)*
