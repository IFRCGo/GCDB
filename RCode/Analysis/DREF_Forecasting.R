# Create table of number of DREF's per country, then take the top-10

# Now plot seasonality for that country with respect to a given hazard, using DREF data

# Now using EM-DAT, Desinventar and IDMC data

# 

# Get the DREF data
DREF<-convGOApp_Monty()

tDREF_ev<-Monty_Ev2Tab(DREF)
tDREF_imp<-Monty_Imp2Tab(DREF)

# Filter out all years previous to 2008
DREF%<>%SplitMonty(DREF$event_Level$ID_linkage$event_ID[AsYear(DREF$event_Level$temporal$ev_sdate)>=2008])




# Time series analyis with irregular data
#     For out-of-sample validation, do this analysis with hold out of next 3 months over 1:8 years (thus 2016-2024)
#     Take the cumulative DREF funding value, per hazard, per sub-region, since 2008
#     Take the cumulative EM-DAT deaths, number of events, aid contributions and total cost, per hazard, per sub-region, since 2008
#     Do a simple regression of these variables
#     Check the residuals for each out-of-sample fold, comparing e.g. if predictions were made in 20016 to 2019 to 2021 and per season
#     Then apply autoregression on the residuals AFTER AGGREGATING EACH SUBREGION, since ...2018...? Plot the residuals
#     Do this for each country to see whether performance is comparable




# Convert into usable form... imp_tab and ev_tab



# Data wrangling
# Only DREF after 2008, IDMC since 2016 and EM-DAT since 2000
# 


# EM-DAT data, per country, per hazard in DREF hazards, per impact type,
#     make the monthly seasonality using Fourier Series and one yearly general trend
#     do this for a few countries in central Americas that have had rpevious DREFs
#     MAKE THE FUNCTION INDEPENDENT OF DATABASE/ORGANISATION

# What should I plot? 
#     DREF flood allocations, per sub-region (UN sub-regions), with respect to monthly seasonality
#     DREF cumulative funding per country, per year (all hazards)
#     DREF 1st fourier series component per country

# Predict DREF allocated budget per sub-region, cumulative per month
# Hurdle model based on EM-DAT fourier decomposition 