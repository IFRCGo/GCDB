# For any tips on learning JSON Schemas, check this link out:
# https://json-schema.org/learn/glossary

# Get the RDLS JSON schema
RDLS<-jsonlite::fromJSON("https://docs.riskdatalibrary.org/en/0__2__0/rdls_schema.json")
# Get the taxonomy data
taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
# Transfer over to Montandon class schema
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
  type="array",
  description="A collection of impact estimates",
  items=list(`$ref`="#/$defs/Impact_obj"),
  minItems=1,
  uniqueItems=TRUE
)
#@@@@@@@@@@@@@@@@@@@@@@@ HAZARD-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$Hazard<-list(
  title="Hazard Data",
  description="A collection of hazard severity estimates",
  items=list(`$ref`="#/$defs/Hazard_obj"),
  uniqueItems=TRUE
)
#@@@@@@@@@@@@@@@@@@@@@@ EXPOSURE-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@#
# Monty$properties$Exposure<-list(
#   title="Exposure Data",
#   description="",
#   `$ref`="#/$defs/Exposure_obj"
# )
#@@@@@@@@@@@@@@@@@@@ VULNERABILITY-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@#
# Monty$properties$Vulnerability<-list(
#   title="Vulnerability Data",
#   description="",
#   `$ref`="#/$defs/Vulnerability_obj"
# )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%% CLASS DEFINITIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Monty$`$defs`<-list(
  #@@@@@@@@@@@@@@@@@@@ Event Object @@@@@@@@@@@@@@@@@@@@#
  Event_obj<-list(
    type="object",
    required=list("event_ID","ISO3s","ev_sdate"),
    properties=list(
      event_ID=list(
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
      ISO3s=list(
        title="Country ISO3 Codes",
        description="List of countries affected, by ISO3 code.",
        type="array",
        items=list(`$ref`="#/$defs/ISO3_obj"),
        minItems=1,
        uniqueItems=TRUE
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
  ),
  
  #@@@@@@@@@@@@@@@@@@@ Impact Object @@@@@@@@@@@@@@@@@@@@#
  Impact_obj=list(
    type="object",
    description="'Disaster impact is the total effect, including negative effects 
    (e.g., economic losses) and positive effects (e.g., economic gains), 
    of a hazardous event or a disaster. The term includes economic, 
    human and environmental impacts, and may include death, injuries, disease and
    other negative effects on human physical, mental and social well-being'. 
    UNDRR - https://www.undrr.org/terminology/disaster",
    # the properties element has 6 sections: 
    # ID, impact value, impact categorisation, temporal, spatial and source information
    properties=list(
      ### ID ###
      ID_linkage=list(
        title="Impact ID and Links",
        description="Impact ID information and links to other object instances within an event.
        For example, linking to hazard IDs. 
        Note: no spatial IDs are linked here, see the spatial element.",
        type="array",
        properties=list(
          event_ID=list(
            title="Event ID",
            description="ID of the overall event that this impact estimate refers to",
            type="string",
            minLength=1
          ),
          imp_sub_ID=list(
            title="Impact Estimate ID",
            description="ID of each impact element, within the overall event",
            type="string",
            minLength=1
          ),
          haz_sub_ID=list(
            title="Hazard ID Link",
            description="ID of each hazard event data element, within the overall event",
            type="array",
            items=list(type="string",uniqueItems=TRUE)
          )
        ),
        required=list("event_ID","imp_sub_ID")
      ),
      ### Source Information ###
      source=list(
        title="Impact Data Source Information",
        description="Information regarding the source of the impact data",
        type="array",
        properties=list(
          imp_src_db=list(
            title="Impact Data Source Database",
            description="The database name from where the impact estimate was extracted",
            type="string",
            minLength=1
          ),
          imp_src_org=list(
            title="Impact Data Source Organisation",
            description="The organisation who published the impact estimate",
            type="string"
          ),
          imp_src_orgtype=list(
            title="Impact Data Source Organisation Type",
            description="The type of the organisation who published the impact estimate. 
            See the taxonomy for organisation type under 'organtypes'.",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="organtypes")%>%pull(name)%>%unique()%>%list()
          ),
          imp_src_URL=list(
            title="Impact Data Source URL",
            description="The website URL where the impact estimate was published",
            type="string",
            format="iri"
          )
        ),
        required=list("imp_src_db","imp_src_URL")
      ),
      ### Impact Value ###
      imp_estimate=list(
        title="Impact Value Data",
        description="Impact estimate value and unit information",
        type="array",
        properties=list(
          imp_value=list(
            title="Estimated Impact Value",
            description="The estimated value of the impact, as a number, without the units. 
            For example, for an estimate of 1000 people displaced, you would enter 1000.",
            type="number",
            minimum=0
          ),
          imp_type=list(
            title="Estimated Impact Value Type",
            description="The estimated value type of the impact. 
            For example, for an estimate of 1000 people displaced, 
            the value type is people displaced, thus you would enter 'imptypedisp'.
            Please use the ImpactInformationProfiles.csv file to help find the coded name of the impact type.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="imp_type")%>%pull(name)%>%unique()%>%list()
          ),
          imp_units=list(
            title="Impact Estimate Units",
            description="The units of the impact estimate. For example, 10 deaths 
            would be a count value, thus 'unitscountnum' should be used.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="measunits")%>%pull(name)%>%unique()%>%list()
          ),
          imp_unitdate=list(
            title="Impact Estimate Unit Date",
            description="If the impact estimate is a cost, please provide the date
            that the estimate was made on, to adjust for currency value and inflation.
            If no value is provided, imp_sdate will be used.",
            type="string",
            format="date"
          ),
          imp_est_type=list(
            title="Impact Estimate Data Type",
            description="What was the type of data source that was used to create 
            this impact estimate? Primary data? Secondary data? 
            Modelled data: was it estimated without any event-specific data?",
            type="array",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="est_type")%>%pull(name)%>%unique()%>%list()
          )
        ),
        required=list("imp_value","imp_type","imp_units","imp_est_type")
      ),
      ### Impact Categorisation ###
      imp_taxonomy=list(
        title="Impact Category",
        description="Description of the categorisation of the impact estimate
        with respect to the IFRC Impact Information Profiles (IIPs).",
        type="array",
        properties=list(
          imp_cats=list(
            title="Impact Category",
            description="The most concise categorisation of the impacts, into four 
            categories: population, environmental, infrastructure and economy.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="imp_cats")%>%pull(name)%>%unique()%>%list()
          ),
          imp_subcats=list(
            title="Impact Sub-Category",
            description="The sub-category of impact, which breaks down further from the 
            very condensed four categories into more descriptive sub-categories,
            but doesn't go all the way into the specific assets or population demographics
            that could have been impacted by the hazard.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="imp_subcats")%>%pull(name)%>%unique()%>%list()
          ),
          imp_det=list(
            title="Impact Detail",
            description="The sub-sub category of impact, which is the specific asset or 
            population demographic that has been impacted by the hazard.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=TRUE,
            enum=taxies%>%filter(list_name=="imp_det")%>%pull(name)%>%unique()%>%list()
          )
        )
      ),
      ### Temporal Information ###
      temporal=list(
        title="Time-Related Data",
        description="Data related to dates and time periods of the impact estimates",
        type="array",
        properties=list(
          imp_sdate=list(
            title="Impact Start Date",
            description="Starting date of the impact estimate. For example, the total cost of a disaster 
            and the number of people displaced is usually estimated between two dates. 
            The start date would be the earliest date that the impact estimate covers.
            Some estimates like the death toll might have been made within
            the first few days or weeks of the event, and thus reflects the impact 
            estimate based on preliminary data.",
            type="string",
            format="date"
          ),
          imp_fdate=list(
            title="Impact End Date",
            description="Final date of the impact estimate. For example, the total cost of a disaster 
            and the number of people displaced is usually estimated between two dates.
            The end date would be the latest date that the impact estimate covers.
            Some estimates like the death toll might have been made within
            the first few days or weeks of the event, and thus reflects the impact 
            estimate based on preliminary data. Note: if the impact estimate is a stock estimate,
            such that it makes a statement only about a given day (e.g. 'there were 
            1000 people still displaced on March 12th' instead of 'there were 1000
            people displaced between March 5th to April 5th'), then the end date is 
            the same as the start date (imp_sdate = imp_fdate).",
            type="string",
            format="date"
          )),
        required=list("imp_sdate","imp_fdate")
      ),
      ### Spatial Information ###
      spatial=list(
        title="Spatial-Related Data",
        description="Data related to the spatial coverage of the impact estimates",
        `$ref`="#/$defs/SpatialImpact_obj",
      )
    )
  ),
  
  #@@@@@@@@@@@@@@@@@@@ Hazard Object @@@@@@@@@@@@@@@@@@@@#
  Hazard_obj=list(
    type="object",
    description="'A process, phenomenon or human activity that may cause loss of life, 
    injury or other health impacts, property damage, social and 
    economic disruption or environmental degradation.' 
    UNDRR - https://www.undrr.org/terminology/hazard",
    properties=list(
      haz_sub_ID=list(
        title="Hazard Event ID",
        description="ID of each hazard event, within the overall event",
        type="string",
        minLength=1
      ),
    )
  ),
  
  #@@@@@@@@@@@@@@@ Spatial Hazard Object @@@@@@@@@@@@@@@@#
  SpatialHazard_obj=list(),
  
  #@@@@@@@@@@@@@@@ Spatial Impact Object @@@@@@@@@@@@@@@@#
  SpatialImpact_obj=list(
    title="Impact Spatial Data",
    description="All information about the impact-specific spatial data.",
    type="array",
    properties=list(
      file_loc=list(),
      col_ID=list(),
      src_info=list(),
      
      countries=list(
        title="Country ISO3 Codes",
        description="List of countries affected, by ISO3 code.",
        type="array",
        items=list(`$ref`="#/$defs/ISO3_obj"),
        minItems=1,
        uniqueItems=TRUE
      ),
      spatial_scale=list(
        title="Spatial Scale",
        description="The geographical scale of the data: sub-national, national or multi-national.",
        type="string",
        openCodelist=FALSE,
        enum=list("sub-national","national","multi-national")
      ),
      spatial_data=list(
        title="Impact Spatial Data Linkage",
        description="",
        type="array",
        items=list(`$ref`="#/$defs/SpatialImpact_obj"),
        minItems=1,
        uniqueItems=TRUE
      )
    ),
      
      data_type=list(
        imp_spat_res=list(
          title="Impact Spatial Resolution Value",
          description="The resolution of the given spatial dataset. 
            For example, spatial polygon data which is of administrative boundaries
            and at admin-level 2. You would enter '2' here.",
          type="numeric"
        ),
        imp_spat_resunits=list(
          title="Impact Spatial Data Type",
          description="The spatial data type of a given spatial dataset. 
            For example, spatial polygon data which is of administrative boundaries
            and at admin-level 2. You would enter 'adminlevel' here, which is the 
            correct coded name for administrative boundary spatial data.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=FALSE,
          enum=taxies%>%filter(list_name=="gridcov")%>%pull(name)%>%unique()%>%list()
        )
      )
    ),
    required=list("file_loc","col_ID","imp_spat_srcorg","imp_spat_URL","imp_spat_res",
                  "imp_spat_resunits","spatial_data"),
    minItems=1,
    uniqueItems=TRUE
  ),
  
  #@@@@@@@@@@@@@@ ISO3 Country Code Object @@@@@@@@@@@@@@#
  ISO3_obj=list(),
  
  #@@@@@@@@@@@@@ Source Organisation Object @@@@@@@@@@@@@@#
  SourceOrg_obj=list(),
  
  #@@@@@@@@@@@@@@ Measurement Units Object @@@@@@@@@@@@@@#
  Units=list()
)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% VALIDATION ROUTINES %%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Provided an example of the JSON metadata, we can test to see whether it is valid or not
jsonvalidate::json_validator(example, Monty)






