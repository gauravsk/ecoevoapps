### Island biogeography model

## Initial parameters
Sc = 100 # number of species in the continent
Si = 1 # initial number of species in the island
spp = paste0("sp.",1:Sc) # species names
Ni = 100 # carrying capacity of the island
spi = rep_len(sample(spp,Si),Ni) # species in the island
Nr = 1 # number of propagules in the rain
Sr = NA # number of species in the rain
Ex = 0.01 # extinction rate in the island

## Iterations
for(i in 2:1000){
  # Events
  rain = sample(spp,Nr,replace=T) # propagule rain
  spi = sample(spi,Ni*(1-Ex)) # extinction survivors
  rec = sample(rain,Ni-length(spi)) # recruits -> FIX: when rain is below the necessary to fill the gaps left by extinction
  spi = c(spi,rec) # species by the end of the process
  
  # Statistics
  Sr[i] = length(unique(rain)) # number of species in the rain
  Si[i] = length(unique(spi)) # number of species in the island
  }

plot(x=1:length(Si),y=Si,type="b")



## Complications
# Rescue effect
# Seed rain
# Dispersal limitation


















