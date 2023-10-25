# For any tips on learning JSON Schemas, check this link out:
# https://json-schema.org/learn/glossary

# Get the RDLS JSON schema
RDLS<-jsonlite::fromJSON("https://docs.riskdatalibrary.org/en/0__2__0/rdls_schema.json")
# Get the taxonomy data
taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")

Monty<-RDLS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%% TOP-LEVEL CLASS STRUCTURE  %%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#@@@@@@@@@@@@@@@@@@@@ MONTANDON SCHEMA FUNDAMENTALS @@@@@@@@@@@@@@@@@@@@@#
Monty$`$id`<-"https://goadmin.ifrc.org/docs/Montandon/MontandonSchema.json"
Monty$title<-"Hazard-Impact Event Dataset - The Montandon Schema"
Monty$description<-"The data allows the user to assemble many different hazard-impact estimates,
potentially for different impact types, across different organisations and with spatio-temporal coverage, and more.
The structure and form of this dataset conforms with the Montandon Data Standards."

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% TOP-LEVEL PROPERTIES  %%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#@@@@@@@@@@@@@@@@@@@@@@@ EVENT-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$Event_Level<-
  list(
    title="Event-Level Metadata",
    description="This section describes all the event-level information, 
    without listing any information about impacts or the specific hazards involved.
    Note that an 'event' is defined as the occurrence of a single or multiple hazards.",
    `$ref`="#/$defs/Event_obj"
  )
#@@@@@@@@@@@@@@@@@@@@@@@ IMPACT-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$Impact<-list(
  title="Impact Data",
  description="'Disaster impact is the total effect, including negative effects 
  (e.g., economic losses) and positive effects (e.g., economic gains), 
  of a hazardous event or a disaster. The term includes economic, 
  human and environmental impacts, and may include death, injuries, disease and
  other negative effects on human physical, mental and social well-being'. 
  UNDRR - https://www.undrr.org/terminology/disaster",
  `$ref`="#/$defs/Impact_obj"
)
#@@@@@@@@@@@@@@@@@@@@@@@ HAZARD-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$Hazard<-list(
  title="Hazard Data",
  description="'A process, phenomenon or human activity that may cause loss of life, 
  injury or other health impacts, property damage, social and 
  economic disruption or environmental degradation.' 
  UNDRR - https://www.undrr.org/terminology/hazard",
  `$ref`="#/$defs/Hazard_obj"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%% CLASS DEFINITIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Monty$`$defs`<-list(
  #@@@@@@@@@@@@@@@@@@@ Event Object @@@@@@@@@@@@@@@@@@@@#
  Event_obj<-list(
    type="object",
    required=list("Event_ID","ISO3s","ev_sdate"),
    properties=list(
      Event_ID=list(
        title="Event ID",
        type="string",
        description="Event-level identifier code that is unique and distinct from that of any other hazard occurrence, 
      we generate this by condensing the abbreviated hazard title (e.g. EQ for earthquake), 
      the event start date, and the countries affected (separated by commas)",
        minLength=1,
      ),
      GLIDES=list(
        title=""
      ),
      ev_name_orig=list(
        title="Event Name (Original Language)",
        type="string",
        description="Name of the event in the original language of the information's source",
      ),
      ev_name_en=list(
        title="Event Name (English Language)",
        type="string",
        description="Name of the event in english",
      ),
      location=list(
        title="Event Location",
        type="string",
        description="Brief character description of where the event took place, this is unimportant but could be used to check the spatial-information of the data at a later stage"
      ),
      
      "location"="character", # general description of hazard location
      "ISO3"="character", # ISO3-codes
      "Continent"="character", # Local-continent
      "ev_sdate"="character", # Start date of the event or the impacting-hazard
      "ev_fdate"="character", # Finish date of the event or the impacting-hazard
      # Add triggering hazard details
      "prim_haz_Ab"="character", # Primary (triggering) hazard 2-letter abbreviation
      "prim_haz_type"="character", # Primary (triggering) hazard  type
      "prim_haz_cluster"="character", # Primary (triggering) hazard cluster 
      "prim_haz_spec"="character",
    ),
    minItems=1,
    uniqueItems=TRUE
  ),
  
  #@@@@@@@@@@@@@@@@@@@ Impact Object @@@@@@@@@@@@@@@@@@@@#
  Impact_obj=list(
    type="object",
    properties=list(),
    minItems=1,
    uniqueItems=TRUE
  ),
  
  #@@@@@@@@@@@@@@@@@@@ Hazard Object @@@@@@@@@@@@@@@@@@@@#
  Hazard_obj=list(
    type="object",
    properties=list(),
    uniqueItems=TRUE
  ),
  
  #@@@@@@@@@@@@@@@ Spatial Hazard Object @@@@@@@@@@@@@@@@#
  SpatialHazard_obj=list(),
  
  #@@@@@@@@@@@@@@@ Spatial Impact Object @@@@@@@@@@@@@@@@#
  SpatialImpact_obj=list(),
  
  #@@@@@@@@@@@@@@ ISO3 Country Code Object @@@@@@@@@@@@@@#
  ISO3_obj=list(),
  
  #@@@@@@@@@@@@@ Source Organisatin Object @@@@@@@@@@@@@@#
  SourceOrg_obj=list(),
  
  #@@@@@@@@@@@@@@ Measurement Units Object @@@@@@@@@@@@@@#
  Units=list()
)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% VALIDATION ROUTINES %%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Provided an example of the JSON metadata, we can test to see whether it is valid or not
jsonvalidate::json_validator(example, Monty)






