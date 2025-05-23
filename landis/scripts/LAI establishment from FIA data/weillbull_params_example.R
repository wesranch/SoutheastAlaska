#LAI establishment curves

#This script has several steps:

# 1) import and wrangle FIA data for plots, trees, seedlings, and sitetrees
# 2) assemble FIA plots into age/species cohorts like LANDIS-II
# 3) estimate leaf area for plots using the same algorithm as NECN
# 4) get amount of regeneration per plot per species
# 5) fit distributions to regen~LAI values, with a few options

#libraries
library("tidyverse")
library("rFIA")
library("nls2")
library("minpack.lm")
library("sf")

#function to fit the weibull distribution curve to binned data using NLS
fit_weibull <- function(dat, lower = c(-Inf, -Inf, 0, 0), upper = c(Inf, Inf, Inf, 10), use_c_d = "both") {
  
  #you might have to play with these starting values
  pars <- expand.grid(a=seq(0.1,10, len=50), #shape
                      b=seq(0.1, 20, len=10), #scale
                      c = 0, #threshold parameter; this sets the "floor" for the curve,
                      # the y-intercept for shape parameter > 1
                      # or asymptote for low values of shape parameters
                      d=seq(0.1, 20, len = 20) #removed d parameter, which scales everything vertically
                      #it does allow better fit but harder to converge
  )
  
  
  # a floor could be set by replacing c with a constant, rather than 
  # estimating it, though this could cause issues with convergence.
  # first round to get approximate starting values
  if(use_c_d == "both") formula <- prop_present ~ (((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a)) + c) * d
  if(use_c_d == "c"){
    formula <- prop_present ~ (((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a)) + c)
    pars <- pars[,c(1:3)]
    lower = lower[c(1:3)]
    upper = upper[c(1:3)]
  } 
  if(use_c_d == "d") {
    formula <- prop_present ~ (((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a))) * d
    pars <- pars[,c(1,2,4)]
    lower = lower[c(1,2,4)]
    upper = upper[c(1,2,4)]
  }
  if(use_c_d == "neither"){
    formula <- prop_present ~ (((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a)))
    pars <- pars[,c(1,2)]
    lower = lower[c(1,2)]
    upper = upper[c(1,2)]
  }
  
  res <- nls2(formula, 
              data=dat,
              start=pars, 
              algorithm='brute-force',
              upper = upper,
              lower = lower,
              weights = dat$n_plots_bin)
  
  #get better estimates using minpack
  res1 <- nlsLM(formula, 
                data=dat,
                start=as.list(coef(res)),
                upper = upper,
                lower = lower,
                weights = dat$n_plots_bin)
  
  #sometimes c will be negative, which would allow seedlings to sometimes be negative.
  #If that happens, refit the model with c = 0
  if(use_c_d %in% c("both", "c") & coef(res1)[3] < 0) {
    res1 <- nlsLM(prop_present ~ ((a/b) * ((lai/b)^(a-1)) * exp(- (lai/b)^a)),
                  data=dat,
                  start=as.list(coef(res)[c(1,2)]))
  }
  
  
  return(res1)
}

#options
options(scipen = 999)

#should we restrict our analysis to plots that are near seedlings, near adult trees, or unrestricted?
#this is by far the slowest part of the script
range_method <- "seedling" #"seedling" or "adult"; other options skip this step and use all the data from the chosen states
#range buffer size -- how many meters can plots be from seedlings to count in the calculation?
range_buffer <- 10000
#should maximum suitability be set to 1 and everything scaled to match? This will (almost?)
# always increase total suitable light levels and thus regeneration
scale_max_p <- TRUE
#should the area under the curve be set to 1 for all species? This will cancel out
# differences in abundance/fecundity among species. Probably not recommended if you hae
# a lot of range-restricted species, unless you're using the range_method option above.
# But this is very much recommended if you're wanting to fit a true Weibull PDF where AUC = 1 
# (i.e. using use_c_d == "neither")
set_auc_to_1 <- FALSE
#what formula should we use? See above equation fit_weibull. For a regular Weibull PDF,
# set use_c_d = "neither", to only use the shape and scale parameters. Otherwise,
# you can use the c parameter to translate the whole curve up or the d parameter to stretch the
# whole curve. This gives more flexibility in the kinds of shapes that can be fit,
# and allows for the area under the curve to be greater than 1.

use_c_d <- "d" #what should be used if you need shape, scale, and y-int


#-------------------------------------------------------------------------------
# Step 1: wrangle data
# For this script, this has been preprocessed and limited to 20000 plots from MI and MN
# and we're only analyzing balsam fir and quaking aspen
#-------------------------------------------------------------------------------
#species reference data (example)
sp_ref <- read.csv("./example_data/REF_SPECIES.csv")
trees <- read.csv("./example_data/trees.csv")
plot <- read.csv("./example_data/plot.csv")
seedlings <- read.csv("./example_data/seedlings.csv")
sitetrees <- read.csv("./example_data/sitetrees.csv")

#for interior AK


#-------------------------------------------------------------------------------
# Step 2: Crosswalk LANDIS and FIA species codes
#-------------------------------------------------------------------------------

#this is the functional table and species table from your NECN project. We need
#them for a few things, like calculating LAI and crosswalking LANDIS-II species names
#and FIA SPCDs
func_table <- read.csv("./example_data/NECN_Functional_Table_inv_moisture.csv")
sp_table <- read.csv("./example_data/NECN_Spp_Table_inv_necn7.csv") %>%
  left_join(func_table, by = "FunctionalGroupIndex") 


# We need to assign each FIA species group a value for KLAI and MaximumLAI.
# You may have a lot of species in FIA that aren't in your LANDIS project; I'd suggest
# just coming up with a catchall functional group for those and putting all the 
# extra SPGRPCDs in there. Multiple SPGRPCDs might match each NECN functional group,
# and some FGs might not have a corresponding SPGRPCD if there are no FIA species 
# that match. That's fine.
spgrp_lai <- data.frame(SPGRPCD = unique(trees$SPGRPCD),
                        FunctionalGroupIndex = numeric(length(unique(trees$SPGRPCD)))) %>%
  arrange(SPGRPCD) %>%
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(5,4, 3, 2), 1, FunctionalGroupIndex))%>% #jack and red pine
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(6), 2, FunctionalGroupIndex))%>%   #balsam fir and white spruce
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(9), 3, FunctionalGroupIndex))%>%    #eastern white pine
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(36, 35), 4, FunctionalGroupIndex)) %>% #black spruce
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(7), 5, FunctionalGroupIndex)) %>% #tamarack, ceder
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(30, 31, 33, 42), 6, FunctionalGroupIndex)) %>% #northern hardwoods
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(37, 38, 41), 7, FunctionalGroupIndex)) %>% #aspen
  mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(25, 26, 28, 29, 27, 32, 43, 40, 39, 55), 8, FunctionalGroupIndex)) %>% #southern harwoods
  # mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(), 9, FunctionalGroupIndex)) %>% #wet broadleaf 
  # mutate(FunctionalGroupIndex = ifelse(SPGRPCD %in% c(), 10, FunctionalGroupIndex)) %>% #shrubs
  left_join(func_table %>% dplyr::select(FunctionalGroupIndex, KLAI, MaximumLAI))

#here, we're just doing ABBA and POTR, not all the species
spp_to_use_all <- c("ABBA", "POTR5") #sp_table$SpeciesCode

#get SPCD for each species. This will be different depending on how the species are named,
#but we want a crosswalk from the names used in LANDIS to FIA SPCD somehow
#In this project, I used the USDA PLANTS symbol, which is found in the FIA species reference table
# to crosswalk to FIA species code. We want a column of SpeciesCode that is your 
# LANDIS species identifier, and a column of SPCD that is the FIA species number

sp_ref$SpeciesCode <- sp_ref$SPECIES_SYMBOL
spp_crosswalk <- sp_ref[sp_ref$SPECIES_SYMBOL %in% spp_to_use_all, ] %>%
  dplyr::arrange(SPECIES_SYMBOL) %>%
  dplyr::select(SpeciesCode, SPCD)


# for seedlings, some species don't have enough to get good estimates. This table
# combines several SPCD for each species (e.g., uncommon Populus spp.) to 
# improve parameter estimates for rare species. You have to do this by hand for each
# species you're modeling. Here it's simple because I'm just using two common species
spp_crosswalk_combine <- tibble(SpeciesCode = spp_to_use_all,
                                SPCD = list(12, #Balsam fir
                                            746
                                ))

spp_to_use <- spp_crosswalk$SpeciesCode
spcd_to_use <- spp_crosswalk$SPCD
all_spcd <- unique(trees$SPCD)

#-------------------------------------------------------------------------------
# Step 3: Calculate per-tree LAI contribution and plot-level LAI
#-------------------------------------------------------------------------------

#I'm using the same logic that NECN does to estimate plot-level LAI, so that we
# don't have mismatches when we apply the parameters we're generating to an NECN model.
# So we need to get ages and biomass for cohorts, just like LANDIS-II.


#we need to make sure the trees have ages so we can bin them
enough_trees <- table(sitetrees$SPCD) %>% `[`(which(table(sitetrees$SPCD) > 100)) %>% names()

#make an age model for each species -- you could do this however you want; this is a quick and dirty
# cubic regression for each species, and lumping all rare species into one model
age_model <- lm(log(AGEDIA) ~ poly(log(DIA), 3)*as.factor(SPCD), 
                data = sitetrees[!is.na(sitetrees$DIA) & !is.na(sitetrees$AGEDIA) & sitetrees$SPCD %in% enough_trees, ])
age_model2 <- lm(log(AGEDIA) ~ poly(log(DIA), 3), 
                 data = sitetrees[!is.na(sitetrees$DIA) & !is.na(sitetrees$AGEDIA), ])

#asign ages to trees based on species and diameter
trees <- trees %>%
  mutate(PLOT.YEAR = paste(PLT_CN, INVYR, sep=".")) %>%
  right_join(., plots_to_use, by = c("PLT_CN" = "CN")) %>%
  dplyr::mutate(DIA_cm = DIA * 2.54,
                HT_m = HT / 3.3808) %>%
  dplyr::filter(STATUSCD == 1) %>%
  dplyr::left_join(spgrp_lai)

for(i in 1:length(all_spcd)){
  
  spcd = all_spcd[i]
  sp = spp_crosswalk[match(spcd, spp_crosswalk$SPCD), "SpeciesCode"]
  
  if(spcd %in% enough_trees){
    
    trees[trees$SPCD == spcd, "TOTAGE2"] <- exp(predict(age_model,
                                                        newdata = trees[trees$SPCD == spcd, ]))
    
  } else{
    trees[trees$SPCD == spcd, "TOTAGE2"] <- exp(predict(age_model2,
                                                        newdata = trees[trees$SPCD == spcd, ]))
    
  }
}

trees$age <- ifelse(is.na(trees$TOTAGE), trees$TOTAGE2, trees$TOTAGE)
trees$age <- ifelse(trees$age > 500, 500, trees$age)
breaks <- seq(0, max(trees$age, na.rm = TRUE) + (10 - max(trees$age, na.rm = TRUE) %% 10), by = 5)
trees$age_bin <- base::cut(trees$age, breaks = breaks, labels = breaks[-1], right = TRUE)

#use the NECN LAI calculation to get plot-level LAI
trees_bin <- trees %>%
  group_by(PLOT.YEAR, SPCD, age_bin) %>%
  summarise(cohort_biomass = sum(CARBON_AG/0.47, na.rm = TRUE) * 0.11, #sum biomass and convert to g m-2
            KLAI = KLAI[1],
            MaximumLAI = MaximumLAI[1]) %>%
  mutate(LAI_tree = MaximumLAI * cohort_biomass/(KLAI + cohort_biomass))

#calculate leaf area index per plot
plot_leaf_area <- trees_bin %>%
  group_by(PLOT.YEAR) %>%
  #m2 per tree * trees per acre * acre per m2 = meters squared leaf area per meter squared ground
  summarise(LAI = sum(LAI_tree)) %>%
  filter(!is.na(LAI) & !is.infinite(LAI)) %>%
  filter(LAI < 20)

hist(plot_leaf_area$LAI)

#-------------------------------------------------------------------------------
# Step 4: See where seedlings are present and which plots they could be present
#-------------------------------------------------------------------------------

## get seedlings
seedlings <- seedlings %>%
  mutate(PLOT.YEAR = paste(PLT_CN, INVYR, sep="."),
         TPA_UNADJ = ifelse(is.na(TPA_UNADJ), 0, TPA_UNADJ)) %>%
  filter(PLOT.YEAR %in% plot_leaf_area$PLOT.YEAR) %>% #filter out plots with bad LAI data, inappropriate COND, etc.
  # filter(TOTAGE <= 5) %>% #only useful in RMRS zone, and only collected fora  subset of trees
  group_by(PLOT.YEAR) %>%
  mutate(SEEDLING_COUNT = sum(TPA_UNADJ)) %>%
  slice_head(n = 1)


# Find how many seedlings are in each plot
# And find which plots are in the species range to use for future calculations
for (i in 1:nrow(spp_crosswalk)){
  range_method <- ifelse(is.na(range_method), "none", range_method)
  
  SPCD <- spp_crosswalk_combine[[i, "SPCD"]][[1]]
  
  seedling_sub <- seedlings[seedlings$SPCD %in% SPCD,]
  
  plot_sf <- plot  %>%
    sf::st_as_sf(coords = c("LON", "LAT")) %>%
    st_as_sf %>%
    sf::st_set_crs("EPSG:4326") %>%
    sf::st_transform("EPSG:5070") %>%
    mutate(PLOT.YEAR = paste(CN, INVYR, sep="."))
  
  if(range_method == "seedling"){
    
    seedling_sf <- left_join(seedling_sub, select(plot, CN, LAT, LON), by = c("PLT_CN" = "CN")) %>%
      sf::st_as_sf(coords = c("LON", "LAT"))%>%
      sf::st_set_crs("EPSG:4326") %>%
      sf::st_transform("EPSG:5070")
    
    #create a zone within buffer distance of seedlings
    seedling_buffer <- sf::st_buffer(seedling_sf, range_buffer) %>% 
      st_union() %>% 
      st_as_sf()
    plot_sf$in_range <- lengths(st_within(plot_sf, seedling_buffer))
    
  }else if(range_method == "adult"){
    plots_with_adults <- trees %>%
      filter(SPCD %in% SPCD) %>%
      filter(age > 20) %>% #pick an age at maturity to serve as a seed source
      `[`("PLT_CN") %>%
      unique()
    
    plot_adults <- plot[plot$CN %in% plots_with_adults$PLT_CN, ]  %>%
      sf::st_as_sf(coords = c("LON", "LAT")) %>%
      distinct(geometry) %>%
      st_as_sf %>%
      sf::st_set_crs("EPSG:4326") %>%
      sf::st_transform("EPSG:5070")
    
    plot_buffer <- sf::st_buffer(plot_adults, range_buffer) %>% 
      st_union() %>% 
      st_as_sf()
    plot_sf$in_range <- lengths(st_within(plot_sf, plot_buffer))
    
  }else{
    plot_sf$in_range <- 1
  }
  
  Table <- seedlings[seedlings$SPCD %in% SPCD,]
  
  if(nrow(Table) == 0) next #this can break the rest of the code, because further down expects a column for every species
  
  Sums <- aggregate(Table$TREECOUNT, by=list(PLOT.YEAR = Table$PLOT.YEAR), FUN=sum)
  Sums$x <- ifelse(is.na(Sums$x), 0, Sums$x)
  colnames(Sums) <- c("PLOT.YEAR", spp_to_use[i])
  
  #plots without seedlings get a 0, plots out of range get an NA
  plot_leaf_area <- left_join(plot_leaf_area, Sums, by = "PLOT.YEAR") %>%
    mutate(across(spp_to_use[i], ~ ifelse(is.na(.), 0, .))) %>%
    mutate(across(spp_to_use[i], ~ ifelse(PLOT.YEAR %in% filter(plot_sf, in_range == 1)$PLOT.YEAR,
                                          .,
                                          NA)))
  
  
}


spp_to_use2 <- spp_to_use[which(spp_to_use %in% names(plot_leaf_area))]

n_seedlings <- plot_leaf_area %>%
  tidyr::pivot_longer(cols = all_of(spp_to_use2),
                      names_to = "Species",
                      values_to = "Count") %>%
  group_by(Species) %>%
  summarize(total = sum(Count, na.rm = TRUE))

# write.csv(plot_leaf_area, file = paste("seedlings_with_leaf_area.csv", sep=""))

#-------------------------------------
# Finding proportion of each LAI bin with seedlings present

#make histogram for proportion
nBins <- 30
minLAI <- 0.05
plot_seedling_histogram <- plot_leaf_area %>%
  tidyr::pivot_longer(cols = all_of(spp_to_use2),
                      names_to = "Species",
                      values_to = "Count") %>%
  mutate(LAI = ifelse(LAI < minLAI, minLAI, LAI)) %>%
  mutate(lai_bin = base::cut(LAI, breaks = nBins),
         present = ifelse(Count>0, 1, 0)) %>%
  group_by(Species, lai_bin) %>%
  summarise(n_present = sum(present, na.rm = TRUE),
            n_plots_bin = sum(!is.na(present)),
            prop_present = n_present / n_plots_bin) %>%
  ungroup() %>%
  group_by(Species) %>%
  mutate(prop_present = ifelse(is.na(prop_present), 0, prop_present)) %>%
  mutate(lai = strsplit(as.character(lai_bin), split = ",") %>% #calculate the midpoint of the bin
           map(., .f = ~gsub("\\(|\\]", "", .)) %>%
           map(., .f = ~mean(as.numeric(.))) %>%
           unlist() %>%
           as.numeric())
if(set_auc_to_1) plot_seedling_histogram <- plot_seedling_histogram %>%
  mutate(prop_present = prop_present / sum(prop_present, na.rm = TRUE)) #proportion of plots in the bin with seedlings
if(scale_max_p) plot_seedling_histogram <- plot_seedling_histogram %>%
  mutate(prop_present = prop_present * (1/max(prop_present, na.rm = TRUE))) #set the maximum prop_present to 1 
#and scale everything to match

ggplot(plot_seedling_histogram, aes(x = lai, y = prop_present, color = Species)) +
  geom_line() +
  xlab(label = "Leaf Area Index") +
  ylab(label = "Proportion of plots with seedlings")


#set bounds to parameter estimates to send to the function that fits weibull parameters.
#bound the location parameter at 0 so that we don't get negative P_est.
#You could set other constraints if you're not getting realistic shapes
species_bounds <- tibble(Species = spp_to_use)
species_bounds$lower = list(c(-Inf, -Inf, 0, 0)) #lower and upper bounds for the shape, scale, location, and adjustment parameters
species_bounds$upper = list(c(Inf, Inf, Inf, 20))

#Only if needed: for some species, set the maximum shape parameter 
# to 1 to force a negative-exponential shape
species_bounds[species_bounds$Species %in% c("POTR5"), ]$upper <- list(c(1, Inf, Inf, 20))

plot_seedling_histogram <- left_join(plot_seedling_histogram, species_bounds)

#fit the models using function above, and pull out the shape and scale  
#parameters with broom::tidy()
weibull_models <- plot_seedling_histogram %>%
  mutate(PLOT.LAI = ifelse(lai < 0.1, 0.1, lai)) %>%
  ungroup() %>%
  group_by(Species) %>%
  ungroup() %>%
  dplyr::nest_by(Species) %>%
  mutate(model = list(fit_weibull(data, data$lower[[1]], data$upper[[1]], use_c_d = use_c_d))) %>%
  mutate(shape = broom::tidy(model) %>% pluck(., 2, 1),
         scale = broom::tidy(model) %>% pluck(., 2, 2),
         location = ifelse(use_c_d == "d", 0, broom::tidy(model) %>% pluck(., 2, 3, .default = 0)), #sometimes c doesn't exist; set it to 0 in that case
         adjust = broom::tidy(model) %>% pluck(., 2, ifelse(use_c_d == "d", 3, 4), .default = 1)) #sometimes d doesn't exist; set it to 0 in that case


#make a figure for each species with empirical data and distribution
#this is pretty gross looking but not that complicated really.
#We loop through each item of the list (.l), which loops us through each species,
#where we make a ggplot figure for each species
newdat <- list(lai = seq(0, 20, length.out = 100))
pmap(.l = list(dat = weibull_models$data, 
               sp = weibull_models$Species,
               mod = weibull_models$model),
     .f = function(dat, sp, mod){ 
       ggplot(data = dat, aes(x = lai, y = prop_present)) + 
         geom_point() + 
         ggtitle(label = sp) +
         geom_line(data = data.frame(pred = predict(mod, newdata = newdat),
                                     lai = newdat$lai),
                   aes(y = pred, x = lai))
     })


# write.csv(dplyr::select(weibull_models, Species, shape, scale, location, adjust), "weibull_establishment_params.csv")

