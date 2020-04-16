## Interactive apps for theoretical ecology and evolution models

Our goal with this project is to make a compilation of interactive (shiny) apps to help teach ecology and evolution models.

**Priority models:**

1. single population continuous time (exponential + logistic growth) [Gaurav]
2. single population discrete time (geometric + logistic growth) [Gaurav]
3. (st)age structured discrete time [Marcel]
4. lotka-volterra competition [Kenji + Gaurav]
5. abiotic resource competition model [Kenji + Gaurav]
6. biotic resource competition/predator-prey model [Maddi + Rosa]
7. Nicholson-Bailey host-parasitoid [Rosa]
8. SIR [Maddi] [get someone from Jamie's lab? Andrew?]
9. levin's spatial model [Kenji]
10. island biogeography [Marcel]

(See end of this document for a comprehensive list of apps we have been thinking about)

### Overview of logistics

Our goal for now is to pick a list of ~10 models that we can target to finish somewhat quickly, and get a beta version of the package out by mid Summer.

To make a model:
	1. understand the biology (including model assumptions);  
 	2. write a brief explanation of this biology (maybe follow a template??);  
      	Template outline:  
      	1. Model parameters  
      	2. Model assumptions  
           	1. what happens if these are broken? e.g. introduce carrying capacity to exponential growth model  
      	3. Model equations  
      	4. Relevant analyses, e.g. equilibrium points for the populations   
      	5. References (including papers where this model was developed, papers where this model has been used well)  
 	3. write R-code to implement the model;  
 	4. develop shiny code to build off of the R code (including which plots to show for the model you are working on);   
 	5. share it with our group to get feedback, and later share it with others to get feedback.

write an R function to generate a model template(!)

### More comprehensive preliminary list of apps

Single population:   
- Exponential & logistic growth  
- Continuous time and discrete time  
- logistic growth with time lag  
- (st)age structured populations

Competition:  
- LV competition  
- Tilman R* stuff  
- Macarthur mechanistic competition model 

Consumer-resource  
- LV predator-prey  
  - various derivations of this, e.g. Macarthur-Rosenzwig, include Type II functional response, etc.  
- host-parasitoid interactions  
- temperature dependent interactions

Disease models  
- SIR, etc.  
  - modifications to this, e.g. SIRD  

Metapopulation models  
- patch occupancy  
- source-sink dynamics  
- competition colonization tradeoff

Other quantitative relationships   
- Species area curve  
- Species abundance distributions  
- island biogeography  
- Resource allocation tradeoff  
- Janzen-connell  
- optimal seed size  

Evolution  
- Hardy Weinberg  
- genetic drift  
- ESS in game theory  
- other eco-evo model  
- mark's model (trait-based speciation/extinction)   
- red queen  
- phylogenetic independent contrasts  

Behavior  
- switch point theorem  
- optimal foraging theory  

medium-term wishlist  
- integrating stochasticity  
- food web stuff  
- network stuff?  
