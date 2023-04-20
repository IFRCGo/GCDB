# Extract Environment Variables
source('RCode/GetEnv.R')
# Download and install the necessary packages:
source('RCode/GetODDPackages.R')
# Extract model functions and priors
source('RCode/Model.R')
# Extract the model parameterisation algorithm, default = Adaptive MCMC
source('RCode/Method.R')

# This file holds all the required data - hazard, exposure, vulnerability, as well as event information and observed displacement estimates in the 'ODD' class format
ODDy<-readRDS(paste0(dir,"IIDIPUS_Input/ODDobjects/EQ20210814HTI_10919_example"))
# This is the model parameterisation, currently trained only on earthquakes on a global level
Omega<-readRDS(paste0(dir,"IIDIPUS_Results/Omega_v2_20210828.Rdata"))
# Test to see if the displacement prediction calculations are working
ODDy%<>%DispX(Omega = Omega,center = Model$center, BD_params = Model$BD_params, LL=F,Method = AlgoParams)

#@@@@@@@@@@@@@@@ TEST 1: DISPLACEMENT PREDICTIONS @@@@@@@@@@@@@@@#
if(!(sum(ODDy$Disp)>1e5 & sum(ODDy$Disp)<4e5)) { stop("Test 1 failed - displacement predictions with the example ODD object for HTI EQ 14-08-2021 is erroneous")
} else print("Test 1 complete - displacement predictions with the example ODD object for HTI EQ 14-08-2021 are reasonable")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

# Building damage assessment data from satellite image data (UNOSAT & Copernicus), in the 'BD' class format
BDy<-readRDS(paste0(dir,"IIDIPUS_Input/BDobjects/EQ20210814HTI_10919_example"))
# Test to see if the building damage prediction calculations are working
BDy%<>%BDX(Omega = Omega,Model = Model,LL=F,Method = AlgoParams)

#@@@@@@@@@@@@@@@ TEST 2: BUILDING DAMAGE PREDICTIONS @@@@@@@@@@@@@@@#
if(!(sum(BDy$ClassPred=="moderate")>800 & sum(BDy$ClassPred=="severe")>100)) { stop("Test 2 failed - building damage predictions with the example ODD object for HTI EQ 14-08-2021 is erroneous")
} else print("Test 2 complete - building damage predictions with the example ODD object for HTI EQ 14-08-2021 are reasonable")
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#


