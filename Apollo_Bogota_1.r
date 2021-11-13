# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_Bogota_1",
  modelDescr ="Simple MNL model on mode choice RP data",
  indivID    ="ï..ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("Data selecta_Bog.csv",header=TRUE)

### Use only RP data
##database = subset(database,database$RP==1)

### Create new variable with average income
##database$mean_income = mean(database$income)

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(ICE=1, HEV=2, PHEV=3, BEV=4),
  avail        = list(ICE=database$Ava_ICE, HEV=database$Ava_HEV, PHEV=database$Ava_PHEV, BEV=database$Ava_BEV),
  choiceVar    = database$choice,
  explanators  = database[,c("Estrato","Hijos","Ingresos")]
  #,  rows         = database$income>30000
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_ICE   = 0,
              asc_HEV   = 0,
              asc_PHEV   = 0,
              asc_BEV  = 0,
              b_costo  = 0,
              b_impuesto  = 0,
              b_costoRecarga  = 0,
              b_autonomia = 0,
              b_costoKm  = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_ICE")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['ICE']]  = asc_ICE  + b_costo  * C_ICE   + b_impuesto * I_ICE   + b_costoRecarga * R_ICE  + b_autonomia * A_ICE  + b_costoKm * CKM_ICE
  V[['HEV']]  = asc_HEV  + b_costo  * C_HEV   + b_impuesto * I_HEV   + b_costoRecarga * R_HEV  + b_autonomia * A_HEV  + b_costoKm * CKM_HEV
  V[['PHEV']]  = asc_PHEV + b_costo  * C_PHEV  + b_impuesto * I_PHEV  + b_costoRecarga * R_PHEV + b_autonomia * A_PHEV + b_costoKm * CKM_PHEV
  V[['BEV']] = asc_BEV + b_costo   * C_BEV   + b_impuesto * I_BEV   + b_costoRecarga * R_BEV  + b_autonomia * A_BEV  + b_costoKm * CKM_BEV
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(ICE=1, HEV=2, PHEV=3, BEV=4), 
    avail         = list(ICE=Ava_ICE, HEV=Ava_HEV, PHEV=Ava_PHEV, BEV=Ava_BEV), 
    choiceVar     = choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)

  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)


