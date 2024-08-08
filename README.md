# ETL Monty 

## Montandon - Global Crisis Data Bank

Welcome to the repository for the realtime Extraction, Transformation and Loading (ETL) scripts of the Monty database. This repository mostly deals with extracting the data from all of the external databases, whereby it then processes and transforms this data into a common hazard, exposure, impact, and response taxonomy classification system and then pushes this data onto the Monty database. However, this repository also contains the analysis scripts that were required to provide the data for, for example, the World Disaster Report 2024 or the [@Monty dashboards](https://app.powerbi.com/view?r=eyJrIjoiODNhOTk5ZmUtMTJhYi00NDQwLTgzZjEtN2JiZTIzZmFhZDFmIiwidCI6ImEyYjUzYmU1LTczNGUtNGU2Yy1hYjBkLWQxODRmNjBmZDkxNyIsImMiOjh9). Here is where the magic happens!

## About Monty

Monty, an abbreviated name for the Montandon - Global Crisis Data Bank, is a database that brings in hazard and impact data for current, historical and forecasted disasters around the globe. By combining lots of different sources of information, Monty aims to fill-in-the-gaps and provide a more complete picture of disaster risk for the National Societies. For more information about the Montandon project, please check out [@this 5-minute video](https://www.youtube.com/watch?v=BEWxqYfrQek).

## Code Layout

Here we describe the layout of the code. The code base is located in the folder `RCode`. Inside this folder, you will find the following folders and files:

  - `Analysis`: this folder contains the scripts used to generate the analyses of the Monty database, including for the Monty dashboards and the World Disaster Report 2024.
  - `GCDB_Objects:` all scripts related to handling Monty objects can be found here. This includes transforming dataframes to the Monty data model and merging different Monty objects, for example.
  - `MainlyHazardData:` here is where we find the scripts that focus on extracting the hazard data (or, at least, data that speaks more about hazards than impacts). For example, the GDACS database, which also provides the GDACS-alertscore which is based on predicting mortality (thus also impact-related data).
  - `MainlyImpactData:` here is where we find the scripts that focus on extracting the impact data (or, at least, data that speaks more about impacts than hazards). For example, the EM-DAT database.
  - `Other:` this folder contains a collection of miscellaneous scripts that may or may not be needed anymore, but were at least very important in the development of the Monty data model and structure. For example, a script that extracts all of the currency codes via the ISO website.
  - `Setup:` this folder provides everything you need in order to get setup with using Monty, including installing all of the necessary R packages.
  - `Spatio-Infra-Political:` this folder contains scripts that access and download data related to spatio-political and spatio-infrastructural information, such as poltical administrative boundary datasets. Note that lots of these are required because different databases use different administrative boundaries.
  - `Main.R:` this file is where the principal code is that extracts the foundation layer of the Monty database. This file was used to get all of the historical data from all of the different databases, harmonise it and then build the first version of the Monty database.
  - `RealTime.R:` this file is where new and updated records are imported from the different databases, transformed and then pushed/merged onto the Monty database.

More information about each folder and files is detailed below.

### Analysis

This folder contains the scripts used to analyse the Monty data.

#### Key Files

  - `DREF_Forecasting.Rmd:` an RMarkdown script that allows you to extract the latest DREF+EA data and then forecast the next few months worth of allocations based upon yearly and seasonal trends in the different sub-regions, for each hazard.
  - `EAP_Country_Prioritisation_List.R:` this script allows the user to extract country rankings for different hazards with respect to certain metrics such as deaths and economic losses, based on the data that goes into the Monty dashboard (see below)
  - `Monty_Analysis:` this file is mostly outdated, but contains lots of the preliminary analyses made to calculate things like the impact-exceedance curves and the geospatial impact return period calculations. 
  - `Monty_Dashboard_Data.R:` all of the data that went into the [@Monty dashboards](https://app.powerbi.com/view?r=eyJrIjoiODNhOTk5ZmUtMTJhYi00NDQwLTgzZjEtN2JiZTIzZmFhZDFmIiwidCI6ImEyYjUzYmU1LTczNGUtNGU2Yy1hYjBkLWQxODRmNjBmZDkxNyIsImMiOjh9) comes from this file. That includes the impact return period analyses, the impact exceedance curves and the impact-based seasonality estimates.
  - `WDR_Analysis.R:` this script was developed in order to provide the data foundation for the World Disaster Report 2024, speak to Kirsten Hagon for more information.
  
### GCDB_Objects

This folder contains the main elements that define and manipulate Monty objects and instances.

#### Key Files

  - `MontanadonSchema_JSON.R:` this file was used to generate the Monty JSON schema and thus represents the full structure and provides the most direct description of the Monty data model. This file outputs the `Montandon_Schema_V1-00.json` data model file.
  - `MontyMethods.R:` this is possibly the most important file in the entire repository. It was made to be as general as possible such that any external database could be transformed into the Monty format using the functions in this file.
  - `CheckMonty.R:` this file contains functions to check that the format of a Monty instance conforms with what we expect and, if not, then corrects the issues.

### MainlyHazardData

This folder contains all of the scripts to access, download and transform all of the hazard-predominant databases.

#### Key Files

  - `GetDFO.R:` the Dartmouth Flood Observatory database
  - `GetDFS_Forecasts:` the DFS team have, for a few years now, extracted data from a few different hazard forecast organisations such as PDC DisasterAware, WFP ADAM. They extracted what they needed, transformed the data into a slightly different format and saved it out into a postgres database. This file extracts all of this data from the postgres post-processed database. Note that this work is half-complete (as of 8th August 2024) because a) PDC data wasn't wrangled correctly, b) WFP-ADAM have now completely changed their API and c) GDACS was already wrangled in GetGDACS.R.
  - `GetGDACS.R:` Global Disaster Alert and Coordination System database
  - `GetGFD.R:` Global Flood Database. This was deprioritised as it is never updated and approximately 40% of the events have been shown to have issues.
  - `GetGLIDEnumber.R:` the GLobal IDEntifier Number (GLIDE) database
  - `GetIBTrACS.R` International Best Track Archive for Climate Stewardship (IBTrACS). Note that this is incomplete, mostly because of the huge amount of RAM required to import the geospatial data. More efficient methods must be used in order to bring this data in
  - `GetUSGS.R` the United States Geological Survey shakemap database. Their API randomly cuts out and will almost always not return all of the data that fits inside of the query you provide, which I don't understand at all. Therefore, for the time being, we just bring in the tabular event information and not the shakemaps.

### MainlyImpactData

This folder contains all of the scripts to access, download and transform all of the impact-predominant databases

#### Key Files

  - `GetDesinventar.R:` the Desinventar database. Note that there are functions in this file that will also automatically extract the administrative boundary files of Desinventar, but substantive work needs to go in to cleaning many of them. This is because many countries have modified their administrative boundaries and when they update them in the Desinventar database there is no timestamp included...
  - `GetEMDAT.R:` the EMergency events DATabase (EM-DAT). Note that they currently (8th Aug 2024) use GAUL admin boundaries but will change to GADM later on this year. They also changed their data model early 2024 and their API twice in 2024
  - `GetGIDD.R` the Global Internal Displacement Database (GIDD). Note that they changed their datastructure around July 2024
  - `GetIFRCappeal.R:` this includes all the IFRC disaster event+impact related data, such as Emergency Appeals (EA), Disaster Response Emergency Fund (DREF) and Field Reports (FR). 
  - `GetReliefWeb.R:` the ReliefWeb data. Note that one current issue with this data is that ReliefWeb doesn't systematically provide any impact, hazard or exposure data, so it doesn't really fit very well in the Monty data model which doesn't have any database-specific information in the event field of the object, but in the hazard/impact/response fields...
  
### Other

These files are a bit of a random eclectic mix of different files that may or may not be useful anymore...

#### Key Files

  - `GetCurrencyCodes.R:` this extracts the ISO-standard list of currency codes and wrangles them into the Monty taxonomy. This should be repeated potentially once a year just to check if any new codes have come out.
  - `GetGoogleEarthEngineData.R:` this file is crucial to access the GAUL administrative boundary data which is used by EM-DAT in their geospatial tagging process. However, it seems like the key package that is required for these scripts to work, rgee, is no longer working, which seriously sucks...
  - `GetUnits.R:` this file extracts measurement units from existing databases and wrangles them into a format that is used by Monty.
  - `GetWorldBank.R:` extracts data from the World Bank API, such as population count or GDP-PPP data

### Setup

This folder is where all of the basic setup scripts are found. This is where the installation scripts are and some of the fundamental functions that are required to make Monty run.

#### Key Files

  - `AzureBlob.R:` scripts to push the data from R running on the realtime Monty server onto the blob storage server
  - `Functions.R:` general functions that are used throughout the entire repository. These general functions are not Monty specific at all
  - `GetEnv_Example.R:` this is an example of all the variables that the GetEnv.R file should include, which is the file that contains things like API keys which are linked to individual developers and not the Monty in general
  - `GetPackages.R:` this is the file that makes all the magic happen. It installs all the required packages and imports all of the files and functions that are required to make everything work. Very important file!
  - `InstallationChecks.R:` although currently very limited, this simple file will check that the basics of the Monty works on a new laptop/server/vm-instance.

### Spatio-Infra-Political

This section is all about political administrative boundaries and other geospatial infrastructural datasets.

#### Key Files

  - `GetAdminBoundaries.R:` this file automatically downloads and transforms almost all of the administrative boundary data that are used by all of the different databases that make up Monty. 
  - `GetOSM.R:` this file extracts the OpenStreetMap data, which also includes building and road data

## Installation

Please use the file `InstallationChecks.R` which can be found in the `Setup` folder. This has two lines to it: the first installs all of the correct packages and loads all of the scripts into the global environment, the second then tries to make a simple call to extract IFRC EA and DREF dataset and transform it into a Monty instance. Note that the first line should always work, no matter what. However, the second line is not always guaranteed to work as IFRC APIs are the data model of these datasets can be modified or updated and therefore this function may not work in future.

## DIY wrangling a new dataset into Monty

For this final section, an explanation is provided on how to extract future datasets such that they conform to the Monty data model. Generally speaking, this is straight-forward, but there are a few key moments whereby it is easy to make a mistake. This section will cover the following: converting the database hazard and impact taxonomies to conform with Monty and then the remainder of what is required in order to convert the rest of the database into a Monty object.

### Hazard taxonomy classification

### Impact taxonomy classification

### Converting to the Monty object data model
















