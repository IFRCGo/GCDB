source("./RCode/Setup/GetPackages.R")

folder<-"./CleanedData/MostlyImpactData/IDMC/"
# Create the folder for the data
dir.create(folder,showWarnings = F,recursive = T); 
# Download the data directly from IDMC
download.file("https://helix-tools-api.idmcdb.org/external-api/gidd/disasters/disaster-export/?iso3__in=&start_year=2000&end_year=2022&hazard_type__in=&client_id=IDMCWSHSOLO009&release_environment=RELEASE",
              paste0(folder,"GIDD-IDMC.xlsx"))
# Load data
GIDD<-readxl::read_xlsx(paste0(folder,"GIDD-IDMC.xlsx"))

