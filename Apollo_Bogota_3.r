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
  modelName  ="Apollo_Bogota_3",
  modelDescr ="MNL model with socio-demographics on mode choice SP data",
  indivID    ="ï..ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("Data selecta_Bog2.csv",header=TRUE)

### Use only SP data
#database = subset(database,database$SP==1)

### Create new variable with average income
#database$mean_income = mean(database$income)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_ICE = 0,
              asc_HEV = 0,
              asc_HEV_shift_Estrato1 = 0,
              asc_HEV_shift_Estrato2 = 0,
              asc_HEV_shift_Estrato3 = 0,
              asc_HEV_shift_Estrato4 = 0,
              asc_HEV_shift_Estrato5 = 0,
              asc_HEV_shift_Estrato6 = 0,
              asc_HEV_shift_Ingresos1 = 0,
              asc_HEV_shift_Ingresos2 = 0,
              asc_HEV_shift_Ingresos3 = 0,
              asc_HEV_shift_Ingresos4 = 0,
              asc_HEV_shift_Ingresos5 = 0,
              asc_HEV_shift_Ingresos6 = 0,
              asc_PHEV = 0,
              asc_PHEV_shift_Estrato1 = 0,
              asc_PHEV_shift_Estrato2 = 0,
              asc_PHEV_shift_Estrato3 = 0,
              asc_PHEV_shift_Estrato4 = 0,
              asc_PHEV_shift_Estrato5 = 0,
              asc_PHEV_shift_Estrato6 = 0,
              asc_PHEV_shift_Ingresos1 = 0,
              asc_PHEV_shift_Ingresos2 = 0,
              asc_PHEV_shift_Ingresos3 = 0,
              asc_PHEV_shift_Ingresos4 = 0,
              asc_PHEV_shift_Ingresos5 = 0,
              asc_PHEV_shift_Ingresos6 = 0,
              asc_BEV = 0,
              asc_BEV_shift_Estrato1 = 0,
              asc_BEV_shift_Estrato2 = 0,
              asc_BEV_shift_Estrato3 = 0,
              asc_BEV_shift_Estrato4 = 0,
              asc_BEV_shift_Estrato5 = 0,
              asc_BEV_shift_Estrato6 = 0,
              asc_BEV_shift_Ingresos1 = 0,
              asc_BEV_shift_Ingresos2 = 0,
              asc_BEV_shift_Ingresos3 = 0,
              asc_BEV_shift_Ingresos4 = 0,
              asc_BEV_shift_Ingresos5 = 0,
              asc_BEV_shift_Ingresos6 = 0,
              b_costo = 0,
              b_costo_shift_Estrato1 = 0,
              b_costo_shift_Estrato2 = 0,
              b_costo_shift_Estrato3 = 0,
              b_costo_shift_Estrato4 = 0,
              b_costo_shift_Estrato5 = 0,
              b_costo_shift_Estrato6 = 0,
              b_costo_shift_Ingresos1 = 0,
              b_costo_shift_Ingresos2 = 0,
              b_costo_shift_Ingresos3 = 0,
              b_costo_shift_Ingresos4 = 0,
              b_costo_shift_Ingresos5 = 0,
              b_costo_shift_Ingresos6 = 0,
              b_impuesto = 0,
              b_impuesto_shift_Estrato1 = 0,
              b_impuesto_shift_Estrato2 = 0,
              b_impuesto_shift_Estrato3 = 0,
              b_impuesto_shift_Estrato4 = 0,
              b_impuesto_shift_Estrato5 = 0,
              b_impuesto_shift_Estrato6 = 0,
              b_impuesto_shift_Ingresos1 = 0,
              b_impuesto_shift_Ingresos2 = 0,
              b_impuesto_shift_Ingresos3 = 0,
              b_impuesto_shift_Ingresos4 = 0,
              b_impuesto_shift_Ingresos5 = 0,
              b_impuesto_shift_Ingresos6 = 0,
              b_costoRecarga = 0,
              b_costoRecarga_shift_Estrato1 = 0,
              b_costoRecarga_shift_Estrato2 = 0,
              b_costoRecarga_shift_Estrato3 = 0,
              b_costoRecarga_shift_Estrato4 = 0,
              b_costoRecarga_shift_Estrato5 = 0,
              b_costoRecarga_shift_Estrato6 = 0,
              b_costoRecarga_shift_Ingresos1 = 0,
              b_costoRecarga_shift_Ingresos2 = 0,
              b_costoRecarga_shift_Ingresos3 = 0,
              b_costoRecarga_shift_Ingresos4 = 0,
              b_costoRecarga_shift_Ingresos5 = 0,
              b_costoRecarga_shift_Ingresos6 = 0,
              b_autonomia = 0,
              b_autonomia_shift_Hijos1 = 0,
              b_autonomia_shift_Hijos2 = 0,
              b_autonomia_shift_Hijos3 = 0,
              b_autonomia_shift_Hijos4 = 0,
              b_costoKm = 0,
              b_costoKm_shift_Estrato1 = 0,
              b_costoKm_shift_Estrato2 = 0,
              b_costoKm_shift_Estrato3 = 0,
              b_costoKm_shift_Estrato4 = 0,
              b_costoKm_shift_Estrato5 = 0,
              b_costoKm_shift_Estrato6 = 0,
              b_costoKm_shift_Ingresos1 = 0,
              b_costoKm_shift_Ingresos2 = 0,
              b_costoKm_shift_Ingresos3 = 0,
              b_costoKm_shift_Ingresos4 = 0,
              b_costoKm_shift_Ingresos5 = 0,
              b_costoKm_shift_Ingresos6 = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_ICE","asc_HEV_shift_Ingresos1","asc_PHEV_shift_Ingresos1","asc_BEV_shift_Ingresos1","b_costo_shift_Ingresos1",
                 "b_impuesto_shift_Ingresos1","b_costoRecarga_shift_Ingresos1","b_costoKm_shift_Ingresos1",
                 "asc_HEV_shift_Estrato1","asc_PHEV_shift_Estrato1","asc_BEV_shift_Estrato1",
                 "b_costo_shift_Estrato1","b_impuesto_shift_Estrato1","b_costoRecarga_shift_Estrato1",
                 "b_costoKm_shift_Estrato1","b_autonomia_shift_Hijos1")

### Read in starting values for at least some parameters from existing model output file
apollo_beta = apollo_readBeta(apollo_beta, apollo_fixed, "Apollo_Bogota_1", overwriteFixed=FALSE)

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
  
  ### Create alternative specific constants and coefficients using interactions with socio-demographics
  asc_HEV_value 	=	asc_HEV	  	+	asc_HEV_shift_Estrato1	*	Estrato1	+	asc_HEV_shift_Estrato2	*	Estrato2	+	asc_HEV_shift_Estrato3	*	Estrato3	+	asc_HEV_shift_Estrato4	*	Estrato4	+	asc_HEV_shift_Estrato5	*	Estrato5	+	asc_HEV_shift_Estrato6	*	Estrato6	+	asc_HEV_shift_Ingresos1 	*	Ingresos1	+	asc_HEV_shift_Ingresos2 	*	Ingresos2	+	asc_HEV_shift_Ingresos3 	*	Ingresos3	+	asc_HEV_shift_Ingresos4 	*	Ingresos4	+	asc_HEV_shift_Ingresos5 	*	Ingresos5	+	asc_HEV_shift_Ingresos6 	*	Ingresos6
  asc_PHEV_value	=	asc_PHEV		+	asc_PHEV_shift_Estrato1	*	Estrato1	+	asc_PHEV_shift_Estrato2	*	Estrato2	+	asc_PHEV_shift_Estrato3	*	Estrato3	+	asc_PHEV_shift_Estrato4	*	Estrato4	+	asc_PHEV_shift_Estrato5	*	Estrato5	+	asc_PHEV_shift_Estrato6	*	Estrato6	+	asc_PHEV_shift_Ingresos1	*	Ingresos1	+	asc_PHEV_shift_Ingresos2	*	Ingresos2	+	asc_PHEV_shift_Ingresos3	*	Ingresos3	+	asc_PHEV_shift_Ingresos4	*	Ingresos4	+	asc_PHEV_shift_Ingresos5	*	Ingresos5	+	asc_PHEV_shift_Ingresos6	*	Ingresos6
  asc_BEV_value 	=	asc_BEV	  	+	asc_BEV_shift_Estrato1	*	Estrato1	+	asc_BEV_shift_Estrato2	*	Estrato2	+	asc_BEV_shift_Estrato3	*	Estrato3	+	asc_BEV_shift_Estrato4	*	Estrato4	+	asc_BEV_shift_Estrato5	*	Estrato5	+	asc_BEV_shift_Estrato6	*	Estrato6	+	asc_BEV_shift_Ingresos1   *	Ingresos1	+	asc_BEV_shift_Ingresos2	  *	Ingresos2	+	asc_BEV_shift_Ingresos3 	*	Ingresos3	+	asc_BEV_shift_Ingresos4 	*	Ingresos4	+	asc_BEV_shift_Ingresos5 	*	Ingresos5	+	asc_BEV_shift_Ingresos6 	*	Ingresos6
  b_costo_value 	=	b_costo		+	b_costo_shift_Estrato1	*	Estrato1	+	b_costo_shift_Estrato2	*	Estrato2	+	b_costo_shift_Estrato3	*	Estrato3	+	b_costo_shift_Estrato4	*	Estrato4	+	b_costo_shift_Estrato5	*	Estrato5	+	b_costo_shift_Estrato6	*	Estrato6	+	b_costo_shift_Ingresos1 	*	Ingresos1	+	b_costo_shift_Ingresos2 	*	Ingresos2	+	b_costo_shift_Ingresos3 	*	Ingresos3	+	b_costo_shift_Ingresos4 	*	Ingresos4	+	b_costo_shift_Ingresos5 	*	Ingresos5	+	b_costo_shift_Ingresos6 	*	Ingresos6
  b_impuesto_value 	=	b_impuesto		+	b_impuesto_shift_Estrato1 	*	Estrato1	+	b_impuesto_shift_Estrato2 	*	Estrato2	+	b_impuesto_shift_Estrato3	  *	Estrato3	+	b_impuesto_shift_Estrato4	  *	Estrato4	+	b_impuesto_shift_Estrato5	  *	Estrato5	+	b_impuesto_shift_Estrato6	  *	Estrato6	+	b_impuesto_shift_Ingresos1	  *	Ingresos1	+	b_impuesto_shift_Ingresos2	  *	Ingresos2	+	b_impuesto_shift_Ingresos3	  *	Ingresos3	+	b_impuesto_shift_Ingresos4	  *	Ingresos4	+	b_impuesto_shift_Ingresos5	  *	Ingresos5	+	b_impuesto_shift_Ingresos6	  *	Ingresos6
  b_costoRecarga_value 	=	b_costoRecarga		+	b_costoRecarga_shift_Estrato1	*	Estrato1	+	b_costoRecarga_shift_Estrato2	*	Estrato2	+	b_costoRecarga_shift_Estrato3	*	Estrato3	+	b_costoRecarga_shift_Estrato4	*	Estrato4	+	b_costoRecarga_shift_Estrato5	*	Estrato5	+	b_costoRecarga_shift_Estrato6	*	Estrato6	+	b_costoRecarga_shift_Ingresos1	*	Ingresos1	+	b_costoRecarga_shift_Ingresos2	*	Ingresos2	+	b_costoRecarga_shift_Ingresos3	*	Ingresos3	+	b_costoRecarga_shift_Ingresos4	*	Ingresos4	+	b_costoRecarga_shift_Ingresos5	*	Ingresos5	+	b_costoRecarga_shift_Ingresos6	*	Ingresos6
  b_autonomia_value = b_autonomia + b_autonomia_shift_Hijos1 * Hijos1 + b_autonomia_shift_Hijos2 * Hijos2 + b_autonomia_shift_Hijos3 * Hijos3 + b_autonomia_shift_Hijos4 * Hijos4
  b_costoKm_value 	=	b_costoKm		+	b_costoKm_shift_Estrato1	*	Estrato1	+	b_costoKm_shift_Estrato2	*	Estrato2	+	b_costoKm_shift_Estrato3	*	Estrato3	+	b_costoKm_shift_Estrato4	*	Estrato4	+	b_costoKm_shift_Estrato5	*	Estrato5	+	b_costoKm_shift_Estrato6	*	Estrato6	+	b_costoKm_shift_Ingresos1 	*	Ingresos1	+	b_costoKm_shift_Ingresos2 	*	Ingresos2	+	b_costoKm_shift_Ingresos3 	*	Ingresos3	+	b_costoKm_shift_Ingresos4 	*	Ingresos4	+	b_costoKm_shift_Ingresos5 	*	Ingresos5	+	b_costoKm_shift_Ingresos6 	*	Ingresos6
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['ICE']]  = asc_ICE        + b_costo_value  * C_ICE + b_impuesto_value * I_ICE + b_costoRecarga_value * R_ICE + b_autonomia_value * A_ICE + b_costoKm_value * CKM_ICE
  V[['HEV']]  = asc_HEV_value  + b_costo_value  * C_HEV + b_impuesto_value * I_HEV + b_costoRecarga_value * R_HEV + b_autonomia_value * A_HEV + b_costoKm_value * CKM_HEV
  V[['PHEV']]  = asc_PHEV_value  + b_costo_value  * C_PHEV + b_impuesto_value * I_PHEV + b_costoRecarga_value * R_PHEV + b_autonomia_value * A_PHEV + b_costoKm_value * CKM_PHEV
  V[['BEV']] = asc_BEV_value + b_costo_value  * C_BEV + b_impuesto_value * I_BEV + b_costoRecarga_value * R_BEV + b_autonomia_value * A_BEV + b_costoKm_value * CKM_BEV
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(ICE=1, HEV=2, PHEV=3, BEV=4),
    avail        = list(ICE=Ava_ICE, HEV=Ava_HEV, PHEV=Ava_PHEV, BEV=Ava_BEV),
    choiceVar    = choice,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
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

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #
'
### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- LR TEST AGAINST SIMPLE MNL MODEL                           ----
# ----------------------------------------------------------------- #

apollo_lrTest("Apollo_Bogota_1", "Apollo_Bogota_3")
#apollo_lrTest("Apollo_example_2", model)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS AND ELASTICITY CALCULATIONS              ----
# ----------------------------------------------------------------- #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, apollo_probabilities, apollo_inputs, prediction_settings=list(runs=30))

### Now imagine the cost for BEV increases by 1%
database$cost_BEV = 1.01*database$cost_BEV
### Rerun predictions with the new data
predictions_new = apollo_prediction(model, apollo_probabilities, apollo_inputs)
### Return to original data
database$cost_BEV = 1/1.01*database$cost_BEV

### work with predictions at estimates
predictions_base=predictions_base[["at_estimates"]]
### Compute change in probabilities
change=(predictions_new-predictions_base)/predictions_base

### Not interested in chosen alternative now, so drop last column
change=change[,-ncol(change)]
### First two columns (change in ID and task) also not needed
change=change[,-c(1,2)]

### Look at first individual
change[database$ID==1,]
### And person 9, who has all 4 modes available
change[database$ID==9,]

### Summary of changes (possible presence of NAs for unavailable alternatives)
summary(change)

### Look at mean changes for subsets of the data, ignoring NAs
colMeans(change,na.rm=TRUE)
colMeans(subset(change,database$HEViness==1),na.rm=TRUE)
colMeans(subset(change,database$HEViness==0),na.rm=TRUE)
colMeans(subset(change,(database$income<quantile(database$income,0.25))),na.rm=TRUE)
colMeans(subset(change,(database$income>=quantile(database$income,0.25))|(database$income<=quantile(database$income,0.75))),na.rm=TRUE)
colMeans(subset(change,(database$income>quantile(database$income,0.75))),na.rm=TRUE)

### Compute own elasticity for BEV:
log(sum(predictions_new[,6])/sum(predictions_base[,6]))/log(1.01)

### Compute cross-elasticities for other modes
log(sum(predictions_new[,3])/sum(predictions_base[,3]))/log(1.01)
log(sum(predictions_new[,4])/sum(predictions_base[,4]))/log(1.01)
log(sum(predictions_new[,5])/sum(predictions_base[,5]))/log(1.01)



# ----------------------------------------------------------------- #
#---- RECOVERY OF SHARES FOR ALTERNATIVES IN DATABASE            ----
# ----------------------------------------------------------------- #

sharesTest_settings = list()
sharesTest_settings=list()
sharesTest_settings[["alternatives"]] = c(ICE=1, HEV=2, PHEV=3, BEV=4)
sharesTest_settings[["choiceVar"]]    = database$choice
sharesTest_settings[["subsamples"]] = list(HEViness=(database$HEViness==1),
                                           leisure=(database$HEViness==0))

apollo_sharesTest(model,apollo_probabilities,apollo_inputs,sharesTest_settings)

# ----------------------------------------------------------------- #
#---- MODEL PERFORMANCE IN SUBSETS OF DATABASE                   ----
# ----------------------------------------------------------------- #

fitsTest_settings = list()

fitsTest_settings[["subsamples"]] = list()
fitsTest_settings$subsamples[["HEViness"]] = database$HEViness==1
fitsTest_settings$subsamples[["leisure"]] = database$HEViness==0
apollo_fitsTest(model,apollo_probabilities,apollo_inputs,fitsTest_settings)

# ----------------------------------------------------------------- #
#---- FUNCTIONS OF MODEL PARAMETERS                              ----
# ----------------------------------------------------------------- #

deltaMethod_settings=list(operation="ratio", parName1="b_tt_ICE", parName2="b_cost")
apollo_deltaMethod(model, deltaMethod_settings)

deltaMethod_settings=list(operation="ratio", parName1="b_tt_ICE", parName2="b_cost", multPar1 = 60)
apollo_deltaMethod(model, deltaMethod_settings)

deltaMethod_settings=list(operation="diff", parName1="b_tt_ICE", parName2="b_tt_BEV")
apollo_deltaMethod(model, deltaMethod_settings)

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()
'
