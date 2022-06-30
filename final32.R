##########################
### Load packages

fed_cap_packages = c(
  "tidyverse",
  "ctsem",
  "rstan"
)


package_loader = function(x) {
  
  ### Check if user has package, if not then install package
  eval(bquote(library(.(x), logical.return = TRUE))) == TRUE || install.packages(x)
  
  ### Load Packages
  eval(bquote(library(.(x))))
  
}

for (i in 1:length(fed_cap_packages)) {
  
  needed = fed_cap_packages[i]
  package_loader(needed)
  
}


###################################################
###################################################
########### Data Cleaning Script ##################
##### By: Chris Rea and C. Blain Morin ############
###################################################
###################################################

# This script downloads the raw data from dropbox
# and shows all transformations used to transform 
# the data for modelling. 

# The script can be used in other documents by 
# using the "source" function in other docs
# e.g., source("Data_Cleaning/data_cleaning_script.R")


### Load Raw Data
fed = read.csv("https://www.dropbox.com/s/wlhuq8mdyidxdk3/fed_agency_capacity_autonomy.csv?dl=1")


###########################################################
#############################################
### Identify and remove certain #############
###### top level agencies ###################
#############################################


### Make "top" level agency for appt and advs
### For example, if the agy_code SZ, this create and agy_full of SZ00
### Top level agencies end with 00
top.level = paste0(unique(fed$agy_code), "00")

### Select only these top level agencies
### Then create the top level appt, advs, and expert variables
appendage = fed %>%
  select(agy_code, AGYSUB, yr, appt_pct, advs_pct, expert_pct) %>%
  filter(AGYSUB %in% top.level) %>%
  select(-AGYSUB) %>%
  rename(top_appt_pct = appt_pct, 
         top_advs_pct = advs_pct,
         top_expert_pct = expert_pct)

### Attach these top levels to all other corresponding agencies
### I.e. attach to all agencies with a matching agy_code
### Joins by agy_code and year
fed = fed %>%
  left_join(appendage)


### Find all instances where a given two-letter agency code is used
### multiple times in a single year, which flags instances where an agency
### (usually, a department, like the Department of Agriculture) has sub-units.
### Then, append 00 to those two-letter codes to get a list of
### top-level AGYSUB codes that are used to drop agencies or departments that
### have sub-units.

drop.agy = fed %>%
  group_by(yr, agy_code) %>%
  summarise(n_in_code = n()) %>%
  filter(n_in_code > 1)

drop.agy = paste0(unique(drop.agy$agy_code), "00")

### Filter out top level agencies that have umbrella agencies
### Remove Dept of Defense and total and Net rows
fed = fed %>%
  filter(!AGYSUB %in% drop.agy) %>%
  filter(!agy_full == "Dept. of Defense") %>%
  filter(AGYSUB != "TOTL") %>% ### Removes Total Rows 
  filter(!grepl("NET", agy_full))



############################################################
########################################
### Add some data ######################
########################################

### Set minimum employee threshold
min_empl = 250

### Declare agency types to remove
not_agencies = c("National Defense",
                 "Interest")

### Grab agency names that meet the filtering conditions
temp = fed %>%
  filter(n >= min_empl) %>%
  filter(!agy_typ %in% not_agencies)

inc = unique(temp$agy_full)

### Filter using the inc agency names
### This step grabs agencies that *ever*
### have more than 250 employees
df = fed %>%
  filter(agy_full %in% inc)



###############################################################
##############################################
######### Data Transformations ###############
##############################################

dff = df %>%
  mutate(logn = log(n + 1)) %>%
  mutate(logb18 = log(b18 + 1)) %>%
  ### These next two lines correct for agencies with no "top level"
  mutate(top_appt_pct = ifelse(is.na(top_appt_pct), appt_pct, top_appt_pct)) %>%
  mutate(top_advs_pct = ifelse(is.na(top_advs_pct), advs_pct, top_advs_pct)) %>%
  mutate(log_appt_pct = log(appt_pct + 1)) %>%
  mutate(log_advs_pct = log(advs_pct + 1)) %>%
  mutate(log_top_appt_pct = log(top_appt_pct + 1)) %>%
  mutate(log_top_advs_pct = log(top_advs_pct + 1)) %>%
  mutate(log_b18_r = log(b18_r + 1))

### These are the regressor names used in the model
regressors = c("log_appt_pct",  "log_advs_pct", "log_top_appt_pct", "log_top_advs_pct", "log_b18_r",
               "logn", "logb18",
               "med_sal_", "LOSavg", "ma_pct")

### Temp for exmploratory

df3 = dff %>%
  select(all_of(regressors),
         yr,
         AGYSUB,
         agy_full,
         agy_typ,
         appt_pct,
         advs_pct,
         top_advs_pct,
         top_appt_pct,
         b18_r,
         n,
         b18) %>%
  drop_na()

df.clean = df3

### Select relevant columns and
### drop any row with missing data
dff = dff %>%
  select(all_of(regressors), yr, AGYSUB, agy_full, agy_typ) %>%
  mutate_at(all_of(regressors), scale) %>%
  # Dropping Na removes 55 observations
  drop_na() 

### Cleaner name
df.clean.scaled = dff

### Clean up work space
### only keep cleaned df
rm(list=setdiff(ls(), c("df.clean", "df.clean.scaled")))



regressors = c("log_appt_pct",  "log_advs_pct", "log_top_appt_pct", "log_top_advs_pct", "log_b18_r",
               "logn", "logb18",
               "med_sal_", "LOSavg", "ma_pct")

## Make Strings for the model
## based on the number of regressors
lambdas = c(1)
manifest = c(0)
for (i in 2:length(regressors)) {
  
  temp = paste0("lambda", i)
  temp2 = paste0("manifestmean", i)
  lambdas[i] = temp
  manifest[i] = temp2
}

df.clean.scaled$yr = df.clean.scaled$yr - 1972

lambdas = c(-1, "lam21", "lam31", "lam41", "lam51", 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 1, "lam72", 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 1, "lam93", "lam103")

manifestvar = data.frame(
  
  auto1 = c("var11", "cov21", "cov31", "cov41", "cov51", 0, 0, 0, 0, 0),
  auto2 = c(0, "var22", "cov32", "cov42", "cov52", 0, 0, 0, 0, 0),
  auto3 = c(0, 0, "var33", "cov43", "cov53", 0, 0, 0, 0, 0),
  auto4 = c(0, 0, 0, "var44", "cov54", 0, 0, 0, 0, 0),
  auto5 = c(0, 0, 0, 0, "var55", 0, 0, 0, 0, 0),
  reso1 = c(0, 0, 0, 0, 0, "var66", "cov76", 0, 0, 0),
  reso2 = c(0, 0, 0, 0, 0, 0, "var77", 0, 0, 0),
  expert1 = c(0, 0, 0, 0, 0, 0, 0, "var88", "cov98", "cov108"),
  expert2 = c(0, 0, 0, 0, 0, 0, 0, 0, "var99", "cov109"),
  expert3 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, "var1010")
  
  
)

manifestvar = as.matrix(manifestvar)

# The is the model specification
model = ctModel(type='stanct', 
                
                LAMBDA=matrix(lambdas,
                              nrow = length(regressors), 
                              ncol = 3), # Forcing the first factor load to be 1
                
                n.manifest=length(regressors),
                
                manifestNames = regressors,
                
                n.latent=3,
                
                T0MEANS = 0,
                
                latentNames=c('Autonomy', 'Resources', 'Expertise'),
                
                MANIFESTVAR = manifestvar,
                
                CINT = c('cint1', 'cint2', 'cint3'),
                
                id = "agy_full",
                
                time = "yr")

model = ctStanFit(datalong = df.clean.scaled,
                  ctstanmodel = model,
                  optimize = FALSE,
                  nopriors = FALSE,
                  iter = 5000,
                  chains = 4,
                  cores = 4, 
                  control = list(max_treedepth = 15, adapt_delta = .99))

save(model, file = "final32.Rda")