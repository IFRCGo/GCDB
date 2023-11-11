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
Monty$description<-"The data allows the user to assemble many different hazard-impact estimates, potentially for different impact types, across different organisations and with spatio-temporal coverage, and more. The structure and form of this dataset conforms with the Montandon Data Standards."
Monty$schema_version<-Monty_JSONschema
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% TOP-LEVEL PROPERTIES  %%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#@@@@@@@@@@@@@@@@@@@@@@@@ MONTY-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties<-list()

Monty$properties$monty_Info<-list(
  title="Monty Object Instance-Specific Information",
  description="Information about this Montandon object instance and general information on the data within this instance.",
  type="object",
  properties=list(
    class_version=list(
      title="Montandon Version",
      description="The version of the Montandon from which the data was extracted.",
      type="string",
      pattern="^Montandon V\\d\\.\\d\\d$"
    ),
    db_pubdate=list(
      title="Date of Publication",
      description="The date this data was published in the Montandon database.",
      `$ref`="#/$defs/Date_obj"
    ),
    helpful_resources=list(
      title="Helpful Monty-Related Resources",
      description="A few helpful resources that may help the user through their journey into Monty's world!",
      type="array",
      items=list(
        type="object",
        properties=list(
          title=list(type="string"),
          description=list(type="string"),
          URL=list(type="string",format="iri")
        )
      )
    )
  )
)
#@@@@@@@@@@@@@@@@@@@@@@@ EVENT-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$event_Level<-list(
  title="Event-Level Metadata",
  description="This section describes all the event-level information, without listing any information about impacts or the specific hazards involved. Note that an 'event' is defined as the occurrence of a single or multiple hazards.",
  type="array",
  items=list(`$ref`="#/$defs/Event_obj"),
  minItems=1,
  uniqueItems=TRUE
)
#@@@@@@@@@@@@@@@@@@@@@@@ IMPACT-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$impact_Data<-list(
  title="Impact Data",
  type="array",
  description="A collection of impact estimate data",
  items=list(`$ref`="#/$defs/Impact_obj"),
  minItems=1,
  uniqueItems=TRUE
)
#@@@@@@@@@@@@@@@@@@@@@@@ HAZARD-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$hazard_Data<-list(
  title="Hazard Data",
  description="A collection of hazard severity estimate data",
  type="array",
  items=list(`$ref`="#/$defs/Hazard_obj"),
  uniqueItems=TRUE
)
#@@@@@@@@@@@@@@@@@@@@@@ EXPOSURE-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@#
# Monty$properties$Exposure<-list(
# title="Exposure Data",
# description="A collection of exposure estimate data",
# items=list(`$ref`="#/$defs/Exposure_obj"),
# uniqueItems=TRUE
# )
#@@@@@@@@@@@@@@@@@@@ VULNERABILITY-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@#
# Monty$properties$Vulnerability<-list(
# title="Vulnerability Data",
# description="A collection of vulnerability estimate data",
# items=list(`$ref`="#/$defs/Vulnerability_obj"),
# uniqueItems=TRUE
# )

Monty$required<-c("monty_Info","event_Level","impact_Data","hazard_Data")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%% CLASS DEFINITIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
Monty$`$defs`<-list()
#@@@@@@@@@@@@@@@@@@@ Event Object @@@@@@@@@@@@@@@@@@@@#
Monty$`$defs`$Event_obj<-list(
  title="Event-Level Information",
  description="Information about the event, without taking into account any impact estimates or hazard data.",
  type="object",
  properties=list(
    ID_linkage=list(
      title="Event IDs and Names",
      description="Event ID information and names of the event in different languages.",
      type="object",
      properties=list(
        event_ID=list(
          title="Event ID",
          type="string",
          description="Event-level identifier code that is unique and distinct from that of any other hazard occurrence, we generate this by condensing the abbreviated hazard title (e.g. EQ for earthquake), the event start date, and the countries affected (separated by commas)",
          minLength=1
        ),
        ev_name=list(
          title="Event Name",
          type="string",
          description="Name of the event, for general use, in any given language."
        ),
        ev_name_lang=list(
          title="Event Name Language",
          type="string",
          description="Language the event name is provided in, values correspond to the ISO 639-2 (alpha-3b) standard language codes. Values were taken from https://datahub.io/core/language-codes/r/language-codes-full.csv",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=FALSE,
          enum=taxies%>%filter(list_name=="lang_ISO639-2_alpha3")%>%pull(name)%>%unique()%>%c()
        ),
        all_ext_IDs=list(
          title="External IDs",
          description="List of ID codes that are used by external organisations (outside of the Montandon) to uniquely identify the hazard event. For example, GLIDE numbers.",
          type="array",
          items=list(`$ref`="#/$defs/ExtIDs_obj"),
          uniqueItems=TRUE
        )
      ),
      required=list("event_ID")
    ),
    spatial=list(
      title="Event IDs and Names",
      description="Event ID information and names of the event in different languages.",
      type="object",
      properties=list(
        ISO3s=list(
          title="Country ISO3 Codes",
          description="List of countries affected, by ISO3 code.",
          type="array",
          items=list(`$ref`="#/$defs/ISO3_obj"),
          minItems=1,
          uniqueItems=TRUE
        ),
        gen_location=list(
          title="General Descriptive Event Location",
          type="string",
          description="Brief character description of where the event took place, this is unimportant but could be used to check the spatial-information of the data at a later stage"
        ),
        Regions=list(
          title="Continents or Regions",
          description="List of continents or regions affected",
          type="array",
          items=list(type="string"),
          minItems=1,
          uniqueItems=TRUE
        )
      ),
      required=list("ISO3s")
    ),
    temporal=list(
      title="Time-Related Data",
      description="Data related to dates and time periods of the hazards. Note that, for multiple hazards, the event start and end dates would be the minimum and the maximum dates over all of the hazards.",
      type="object",
      properties=list(
        ev_sdate=list(
          title="Event Start Date",
          description="Start date of the hazard event. For unclear start dates, please use the earliest plausible date.",
          `$ref`="#/$defs/Date_obj"
        ),
        ev_fdate=list(
          title="Event End Date",
          description="End date of the hazard event. For unclear end dates, please use the latest plausible date. Please note that this is when the hazard ended, not when the post-disaster recovery was achieved.",
          `$ref`="#/$defs/Date_obj"
        )
      ),
      required=list("ev_sdate","ev_fdate")
    ),
    principal_hazard=list(
      allOf=list("prpl_haz_spec"),
      title="Taxonomy of Principal Hazard",
      description="Classification of the principal hazard associated to the event, with respect to the UNDRR-ISC 2020 Hazard Information Profiles report. Note that we define 'principal' hazard here as not necessarily the first hazard to arrive, but the hazard that the event is most likely to be attributed to. For example, a cyclone event might result in flooding, heatwaves, thunderstorms, etc, but the principal event would remain a cyclone.",
      type="object",
      properties=list(
        prpl_haz_Ab=list(
          title="Abbreviated Hazard ID",
          description="Two-letter abbreviated ID for the principal hazard. For example, floods would be 'FL' and earthquakes 'EQ'. These abbreviations are pre-determined and standardised, and should be found online.",
          `$ref`="#/$defs/HazAb_obj"
        ), 
        prpl_haz_type=list(
          title="Principal Hazard Type",
          description="The most concise categorisation of the hazards, into eight categories. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
          `$ref`="#/$defs/HazType_obj"
        ),
        prpl_haz_cluster=list(
          title="Principal Hazard Cluster",
          description="The second layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
          `$ref`="#/$defs/HazCluster_obj"
        ),
        prpl_haz_spec=list(
          title="Principal Specific Hazard",
          description="The specific hazard is the final layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
          `$ref`="#/$defs/HazSpec_obj"
        )
      )
    )
  ),
  required=list("ID_linkage","spatial","temporal")
)

#@@@@@@@@@@@@@@@@@@@ Impact Object @@@@@@@@@@@@@@@@@@@@#
Monty$`$defs`$Impact_obj=list(
  title="Impact Object of Montandon",
  type="object",
  description="'Disaster impact is the total effect, including negative effects (e.g., economic losses) and positive effects (e.g., economic gains), of a hazardous event or a disaster. The term includes economic, human and environmental impacts, and may include death, injuries, disease and other negative effects on human physical, mental and social well-being'. UNDRR - https://www.undrr.org/terminology/disaster",
  # the properties element has 6 sections: 
  # ID, impact value, impact categorisation, temporal, spatial and source information
  properties=list(
    ### ID ###
    ID_linkage=list(
      title="Impact ID and Links",
      description="Impact ID information and links to other object instances within an event. For example, linking to hazard IDs. Note: no spatial IDs are linked here, see the spatial element.",
      type="object",
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
      type="object",
      properties=list(
        imp_src_db=list(
          title="Impact Data Source Database",
          description="The database name from where the impact estimate was extracted",
          type="string",
          minLength=1
        ),
        imp_src_dbdesc=list(
          title="Impact Data Source Database Description",
          description="A description of the database from where the impact estimate was extracted",
          type="string"
        ),
        imp_src_org=list(
          title="Impact Data Source Organisation",
          description="The organisation who published the impact estimate",
          type="string"
        ),
        imp_src_orgtype=list(
          title="Impact Data Source Organisation Type",
          description="The type of the organisation who published the impact estimate. See the taxonomy for organisation type under 'organtypes'.",
          `$ref`="#/$defs/SrcOrgType_obj"
        ),
        imp_src_orgatt=list(
          title="Source Organisation Attribution to Impact Data",
          description="The role of the source organisation to the impact data. For example, are they the data custiodians?",
          type="string"
        ),
        imp_src_URL=list(
          title="Impact Data Source URL",
          description="The website URL where the impact estimate was published",
          type="string",
          format="iri"
        ),
        imp_src_addinfo=list(
          title="Impact Data Source Additional Information",
          description="Any additional comments or information about the dataset or the source organisation.",
          type="string"
        ),
        imp_src_email=list(
          title="Impact Data Source Contact Email",
          description="Email address of the source organisation that hosts the impact data estimate.",
          type="string",
          format="email"
        ),
        imp_src_phone=list(
          title="Impact Data Source Contact Phone Number",
          description="Telephone number of the source organisation that hosts the impact data estimate. Note that this must be provided as a string, not as a numeric or integer.",
          type="string",
          pattern="^\\+(?:[0-9]?){6,14}[0-9]$",
          minLength=6,
          maxLength=12
        ),
        imp_src_lic=list(
          title="Impact Data License Information",
          description="Information concerning the license and proprietary information for the impact database.",
          type="string"
        )
      ),
      required=list("imp_src_db","imp_src_URL")
    ),
    ### Impact Value ###
    impact_estimate=list(
      title="Impact Value Data",
      description="Impact estimate value and unit information",
      type="object",
      properties=list(
        imp_value=list(
          title="Estimated Impact Value",
          description="The estimated value of the impact, as a number, without the units. For example, for an estimate of 1000 people displaced, you would enter 1000.",
          type="number",
          minimum=0
        ),
        imp_type=list(
          title="Estimated Impact Value Type",
          description="The estimated value type of the impact. For example, for an estimate of 1000 people displaced, the value type is people displaced, thus you would enter 'imptypedisp'. Please use the ImpactInformationProfiles.csv file to help find the coded name of the impact type.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=FALSE,
          enum=taxies%>%filter(list_name=="imp_type")%>%pull(name)%>%unique()%>%c()
        ),
        imp_units=list(
          title="Impact Estimate Units",
          description="The units of the impact estimate. For example, 10 deaths would be a count value, thus 'unitscountnum' should be used.",
          `$ref`="#/$defs/MeasUnits_obj"
        ),
        imp_unitdate=list(
          title="Impact Estimate Unit Date",
          description="If the impact estimate is a cost, please provide the date that the estimate was made on, to adjust for currency value and inflation. If no value is provided, imp_sdate will be used.",
          `$ref`="#/$defs/Date_obj"
        ),
        imp_est_type=list(
          title="Impact Estimate Data Type",
          description="What was the type of data source that was used to create this impact estimate? Primary data? Secondary data? Modelled data: was it estimated without any event-specific data?",
          `$ref`="#/$defs/EstType_obj"
        )
      ),
      required=list("imp_value","imp_type","imp_units","imp_est_type")
    ),
    ### Impact Categorisation ###
    impact_taxonomy=list(
      allOf=list("imp_det"),
      title="Impact Categorisation",
      description="Description of the categorisation of the impact estimate with respect to the IFRC Impact Information Profiles (IIPs).",
      type="object",
      properties=list(
        imp_cats=list(
          title="Impact Category",
          description="The most concise categorisation of the impacts, into four categories: population, environmental, infrastructure and economy.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=FALSE,
          enum=taxies%>%filter(list_name=="imp_cats")%>%pull(name)%>%unique()%>%c()
        ),
        imp_subcats=list(
          title="Impact Sub-Category",
          description="The sub-category of impact, which breaks down further from the very condensed four categories into more descriptive sub-categories, but doesn't go all the way into the specific assets or population demographics that could have been impacted by the hazard.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=FALSE,
          enum=taxies%>%filter(list_name=="imp_subcats")%>%pull(name)%>%unique()%>%c()
        ),
        imp_det=list(
          title="Impact Detail",
          description="The sub-sub category of impact, which is the specific asset or population demographic that has been impacted by the hazard.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=TRUE,
          enum=taxies%>%filter(list_name=="imp_det")%>%pull(name)%>%unique()%>%c()
        )
      ),
      required=list("imp_det")
    ),
    ### Temporal Information ###
    temporal=list(
      title="Time-Related Data",
      description="Data related to dates and time periods of the impact estimates",
      type="object",
      properties=list(
        imp_sdate=list(
          title="Impact Start Date",
          description="Starting date of the impact estimate. For example, the total cost of a disaster and the number of people displaced is usually estimated between two dates. The start date would be the earliest date that the impact estimate covers. Some estimates like the death toll might have been made within the first few days or weeks of the event, and thus reflects the impact estimate based on preliminary data.",
          `$ref`="#/$defs/Date_obj"
        ),
        imp_fdate=list(
          title="Impact End Date",
          description="Final date of the impact estimate. For example, the total cost of a disaster and the number of people displaced is usually estimated between two dates. The end date would be the latest date that the impact estimate covers. Some estimates like the death toll might have been made within the first few days or weeks of the event, and thus reflects the impact estimate based on preliminary data. Note: if the impact estimate is a stock estimate, such that it makes a statement only about a given day (e.g. 'there were 1000 people still displaced on March 12th' instead of 'there were 1000 people displaced between March 5th to April 5th'), then the end date is the same as the start date (imp_sdate = imp_fdate).",
          `$ref`="#/$defs/Date_obj"
        )),
      required=list("imp_sdate","imp_fdate")
    ),
    ### Spatial Information ###
    spatial=list(
      title="Spatial-Related Data",
      description="Data related to the spatial coverage of the impact estimates",
      type="array",
      items=list(`$ref`="#/$defs/SpatialImpact_obj")
    )
  ),
  required=list("ID_linkage","source","impact_estimate","impact_taxonomy","temporal","spatial")
)

#@@@@@@@@@@@@@@@@@@@ Hazard Object @@@@@@@@@@@@@@@@@@@@#
Monty$`$defs`$Hazard_obj=list(
  title="Hazard Object for Montandon",
  type="object",
  description="'A process, phenomenon or human activity that may cause loss of life, injury or other health impacts, property damage, social and economic disruption or environmental degradation.' UNDRR - https://www.undrr.org/terminology/hazard",
  properties=list(
    ### ID ###
    ID_linkage=list(
      title="Hazard and Event ID",
      description="Impact ID information and links to other object instances within an event. Note: no spatial IDs are linked here, see the spatial element.",
      type="object",
      properties=list(
        event_ID=list(
          title="Event ID",
          description="ID of the overall event that this hazard refers to",
          type="string",
          minLength=1
        ),
        haz_sub_ID=list(
          title="Hazard Event ID",
          description="ID of each hazard event, within the overall event",
          type="string",
          minLength=1
        ),
        haz_ext_IDs=list(
          title="External IDs",
          description="List of ID codes that are used by external organisations (outside of the Montandon) to uniquely identify the hazard event. For example, GLIDE numbers.",
          type="array",
          items=list(`$ref`="#/$defs/ExtIDs_obj"),
          uniqueItems=TRUE
        )
      ),
      required=list("event_ID","haz_sub_ID")
    ),
    ### Source Information ###
    source=list(
      title="Hazard Data Source Information",
      description="Information regarding the source of the impact data",
      type="object",
      properties=list(
        haz_src_db=list(
          title="Hazard Data Source Database",
          description="The database name from where the hazard data was extracted",
          type="string",
          minLength=1
        ),
        haz_src_org=list(
          title="Hazard Data Source Organisation",
          description="The organisation who published the hazard data",
          type="string"
        ),
        haz_src_orgtype=list(
          title="Hazard Data Source Organisation Type",
          description="The type of the organisation who published the hazard data. See the taxonomy for organisation type under 'organtypes'.",
          `$ref`="#/$defs/SrcOrgType_obj"
        ),
        haz_src_URL=list(
          title="Hazard Data Source URL",
          description="The website URL where the hazard data was published",
          type="string",
          format="iri"
        ),
        haz_src_lic=list(
          title="Hazard Data License Information",
          description="Information concerning the license and proprietary information for the hazard database.",
          type="string"
        ),
        haz_src_orgatt=list(
          title="Source Organisation Attribution to Hazard Data",
          description="The role of the source organisation to the hazard data. For example, are they the data custiodians?",
          type="string"
        )
      ),
      required=list("haz_src_db","haz_src_URL")
    ),
    hazard_detail=list(
      title="Basic Hazard Information",
      description="Basic information about the hazard, including things like the maximum magnitude/intensity/severity values and any related trigger/triggering/concurrent hazards (observed or even potential).",
      type="object",
      properties=list(
        haz_maxvalue=list(
          title="Maximum Hazard Severity Value",
          description="The estimated maximum hazard intensity/magnitude/severity value, as a number, without the units.",
          type="number",
          minimum=0
        ),
        haz_units=list(
          title="Maximum Hazard Severity Units",
          description="The units of the estimated hazard intensity/magnitude/severity.",
          `$ref`="#/$defs/MeasUnits_obj"
        ),
        haz_est_type=list(
          title="Hazard Data Estimate Type",
          description="What was the type of data source that was used to create this hazard intensity/magnitude/severity estimate? Primary data? Secondary data? Modelled data: was it estimated without any event-specific data?",
          `$ref`="#/$defs/EstType_obj"
        ),
        concur_haz=list(
          title="Concurrent Hazard Linkage",
          description="Linking the observed and potentially unobserved hazards linked to each specific hazard in the event.",
          type="array",
          items=list(
            title="Concurrent Hazard Linkage Object",
            description="Hazard linkage object",
            type="object",
            properties=list(
              haz_sub_ID=list(
                title="Monty-Based Hazard IDs",
                description="The ID of the specific hazard that is linked to this hazard instance entry, using the Montandon ID. If the hazard is not in Montandon, leave this blank.",
                type="string"
              ),
              linkhaz_ext_IDs=list(
                title="Linked External IDs",
                description="List of ID codes that are used by external organisations (outside of the Montandon) to uniquely identify the hazard event. For example, GLIDE numbers.",
                type="array",
                items=list(`$ref`="#/$defs/ExtIDs_obj"),
                uniqueItems=TRUE
              ),
              linkhaz_spec=list(
                title="Specific Hazard of Linked Hazard",
                description="The specific hazard is the final layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
                `$ref`="#/$defs/HazSpec_obj"
              ),
              linkhaz_occ_known=list(
                title="Is Linked Hazard Occurence Known or Potential",
                description="Was the linked hazard actually observed to have occurred with the main hazard, or is this link only a potential link. In the Montandon, we allow for hazards to be linked together by actual observed occurrences, or the possibility that the linked hazard occurred with the principal hazard. This is especially useful when handling hazards such as tropical cyclones, whereby more than half of all deaths from cyclones in the US were actually caused by inland flooding.",
                type="boolean"
              ),
              linkhaz_occ_prob=list(
                title="Probability of Occurrence of Linked Hazard",
                description="As a string, describe the probability of occurrence of the linked hazard with the main hazard. Numbers can also be used, but must be placed inside quotations.",
                type="string"
              ),
              linkhaz_occ_probdef=list(
                title="URL to Definition of Probability of Occurrence",
                description="Please provide a link to where the definition of occurrence probability is for the hazard relationship. For example, if the probability is 'high', where is 'high' defined?",
                type="string",
                format="iri"
              ),
              relationship=list(
                title="Relationship Between Hazards",
                type="string",
                description="The relationship between this linked hazard to the main hazard instance of this object. Therefore, if this hazard object instance is an earthquake, and the user is linking a landslide to the earthquake hazard object and data, then the relationship would be 'linkhaz-triggered-by-mainhaz', as the landslide way triggered by the earthquake. However, if the hazard object instance was a landslide, and the linked hazard was the earthquake data, then we would use 'mainhaz-triggers-linkhaz'. For hazards that do not necessarily trigger one-another, but occur together, then we use 'concurrent-hazards'. For example, thunderstorms can occur together with windstorms or cyclones, thus, we would use 'concurrent'. When the relationship between two hazards is complex, use 'complex-relationship'. For all other relationships, add as you want, but let us know what we're missing!.",
                enum=list("linkhaz-triggered-by-mainhaz","mainhaz-triggers-linkhaz","concurrent-hazards","complex-relationship"),
                openCodelist=TRUE
              )
            )
          )
        )
      )
    ),
    hazard_taxonomy=list(
      allOf=list("haz_spec"),
      title="Hazard Taxonomy",
      description="Classification of this specific hazard, with respect to the UNDRR-ISC 2020 Hazard Information Profiles report.",
      type="object",
      properties=list(
        haz_Ab=list(
          title="Abbreviated Hazard ID",
          description="Two-letter abbreviated ID for the hazard. For example, floods would be 'FL' and earthquakes 'EQ'. These abbreviations are pre-determined and standardised, and should be found online.",
          `$ref`="#/$defs/HazAb_obj"
        ), 
        haz_type=list(
          title="Hazard Type",
          description="The most concise categorisation of the hazards, into eight categories. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
          `$ref`="#/$defs/HazType_obj"
        ),
        haz_cluster=list(
          title="Hazard Cluster",
          description="The second layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
          `$ref`="#/$defs/HazCluster_obj"
        ),
        haz_spec=list(
          title="Specific Hazard",
          description="The specific hazard is the final layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
          `$ref`="#/$defs/HazSpec_obj"
        )
      )
    ),
    ### Temporal Information ###
    temporal=list(
      title="Time-Related Data",
      description="Data related to dates and time periods of the hazard",
      type="object",
      properties=list(
        haz_sdate=list(
          title="Hazard Start Date",
          description="Starting date of this specific hazard. For unclear start dates, please use the earliest plausible date.",
          `$ref`="#/$defs/Date_obj"
        ),
        haz_fdate=list(
          title="Hazard End Date",
          description="End date of this specific hazard. For unclear end dates, please use the latest plausible date. Please note that this is when the hazard ended, not when the post-disaster recovery was achieved.",
          `$ref`="#/$defs/Date_obj"
        )
      )
      # required=list("haz_sdate","haz_fdate")
    ),
    ### Spatial Information ###
    spatial=list(
      title="Spatial-Related Data",
      description="Data related to the spatial coverage of the hazard",
      type="array",
      items=list(`$ref`="#/$defs/SpatialHazard_obj")
    )
  ),
  required=list("ID_linkage","source","hazard_detail","hazard_taxonomy","temporal","spatial")
)

#@@@@@@@@@@@@@@@ Spatial Impact Object @@@@@@@@@@@@@@@@#
Monty$`$defs`$SpatialImpact_obj=list(
  title="Impact Spatial Data",
  description="All information about the impact-specific spatial data.",
  type="object",
  properties=list(
    ID_linkage=list(
      title="Spatial Impact Data IDs and Linkages",
      description="Information about the IDs and linkages associated to this spatial impact data",
      type="object",
      properties=list(
        imp_spat_ID=list(
          title="Impact Spatial ID",
          description="ID of the spatial impact data",
          type="string",
          minLength=1
        ),
        imp_spat_fileloc=list(
          title="Spatial Data File Location",
          description="Location of where the spatial data can be found. Note that this can be either a local folder or a direct URL to the source.",
          type="string",
          minLength=1
        ),
        imp_spat_colIDs=list(
          title="Spatial Data Column Indices",
          description="Indices of the specific spatial file columns which are referred to by the impact estimate",
          type="array",
          items=list(type="integer")
        ),
        imp_spat_rowIDs=list(
          title="Spatial Data Row Indices",
          description="Indices of the specific spatial file rows which are referred to by the impact estimate",
          type="array",
          items=list(type="integer")
        )
      ),
      required=list("imp_spat_ID","imp_spat_fileloc","imp_spat_colIDs","imp_spat_rowIDs")
    ),
    
    spatial_info=list(
      title="Spatial Data Information",
      description="Information that is directly related to the spatial data itself, such as the form: raster, point, line or polygon.",
      type="object",
      properties=list(
        imp_ISOs=list(
          title="Country ISO3 Codes",
          description="List of countries affected, by ISO3 code.",
          type="array",
          items=list(`$ref`="#/$defs/ISO3_obj"),
          minItems=1,
          uniqueItems=TRUE
        ),
        imp_spat_scale=list(
          title="Spatial Scale",
          description="The geographical scale of the data: sub-national, national or multi-national.",
          type="string",
          openCodelist=FALSE,
          enum=list("sub-national","national","multi-national")
        ),
        imp_spat_type=list(
          title="Impact Spatial Data Type",
          description="The spatial data type of a given spatial dataset. For example, spatial polygon data which is of administrative boundaries and at admin-level 2. You would enter 'adminlevel' here, which is the correct coded name for administrative boundary spatial data.",
          type="string",
          openCodelist=FALSE,
          enum=list("raster","points","lines","polygons")
        ),
        imp_spat_fileread=list(
          title="File Type and Software",
          description="Information on the specific spatial data file format, including providing an understanding of which specific software should be used to read the files.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=TRUE,
          enum=taxies%>%filter(list_name=="spatfilesoft")%>%pull(name)%>%unique()%>%c()
        ),
        imp_spat_res=list(
          title="Impact Spatial Resolution Value",
          description="The resolution of the given spatial dataset. For example, spatial polygon data which is of administrative boundaries and at admin-level 2. You would enter '2' here.",
          type="number"
        ),
        imp_spat_resunits=list(
          title="Impact Spatial Resolution Units",
          description="The units of the resolution of the given spatial dataset.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=TRUE,
          enum=taxies%>%filter(list_name=="gridcov")%>%pull(name)%>%unique()%>%c()
        ),
        imp_spat_crs=list(
          title="Impact Spatial Data Coordinate System",
          description="The spatial coordinate reference system code of the given spatial dataset, such as EPSG:4326. Note that this can either be in ESRI or EPSG format",
          type="string",
          pattern="^(ESRI:|EPSG:)[0-9]+$"
        )
      ),
      required=list("imp_ISOs","imp_spat_scale","imp_spat_type","imp_spat_res","imp_spat_resunits")
    ),
    source=list(
      title="Source Information of Impact Spatial Data",
      description="Information regarding the source of the spatial data of the impact estimate",
      type="object",
      properties=list(
        imp_spat_srcdb=list(
          title="Impact Spatial Data Source Database",
          description="The database name from where the spatial data of the impact estimate depends",
          type="string",
          minLength=1
        ),
        imp_spat_srcorg=list(
          title="Impact Spatial Data Source Organisation",
          description="The organisation who published the spatial data used by the impact estimate",
          type="string"
        ),
        imp_spat_srcorgtype=list(
          title="Impact Spatial Data Source Organisation Type",
          description="The type of the organisation who published the spatial data impact estimate. See the taxonomy for organisation type under 'organtypes'.",
          `$ref`="#/$defs/SrcOrgType_obj"
        ),
        imp_spat_URL=list(
          title="Impact Spatial Data Source URL",
          description="The website URL where the spatial data of the impact estimate can be found",
          type="string",
          format="iri"
        ),
        imp_spat_srclic=list(
          title="Impact Spatial Data License Information",
          description="Information concerning the license and proprietary information for the spatial impact data.",
          type="string"
        ),
        imp_spat_orgatt=list(
          title="Source Organisation Attribution to Spatial Impact Data",
          description="The role of the source organisation to the spatial impact data. For example, are they the data custodians?",
          type="string"
        )
      ),
      required=list("imp_spat_srcdb","imp_spat_URL")
    )
  ),
  required=list("ID_linkage","spatial_info","source"),
  minItems=1,
  uniqueItems=TRUE
)

#@@@@@@@@@@@@@@@ Spatial Hazard Object @@@@@@@@@@@@@@@@#
Monty$`$defs`$SpatialHazard_obj=list(
  title="Hazard Spatial Data",
  description="All information about the hazard-specific spatial data.",
  type="object",
  properties=list(
    ID_linkage=list(
      title="Spatial Hazard Data IDs and Linkages",
      description="Information about the IDs and linkages associated to this spatial hazard data",
      type="object",
      properties=list(
        haz_spat_ID=list(
          title="Hazard Spatial ID",
          description="ID of the spatial hazard data",
          type="string",
          minLength=1
        ),
        haz_spat_fileloc=list(
          title="Spatial Data File Location",
          description="Location of where the spatial data can be found. Note that this can be either a local folder or a direct URL to the source.",
          type="string",
          minLength=1
        ),
        haz_spat_colIDs=list(
          title="Spatial Data Column Indices",
          description="Indices of the specific spatial file columns which are referred to by the hazard data.",
          type="array",
          items=list(type="integer")
        ),
        haz_spat_rowIDs=list(
          title="Spatial Data Row Indices",
          description="Indices of the specific spatial file rows which are referred to by the hazard data",
          type="array",
          items=list(type="integer")
        )
      ),
      required=list("haz_spat_ID","haz_spat_fileloc","haz_spat_colIDs","haz_spat_rowIDs")
    ),
    spatial_info=list(
      title="Spatial Data Information",
      description="Information that is directly related to the spatial data itself, such as the countries associated to the area, or the data form: raster, point, line or polygon.",
      type="object",
      properties=list(
        haz_ISOs=list(
          title="Country ISO3 Codes",
          description="List of countries affected, by ISO3 code.",
          type="array",
          items=list(`$ref`="#/$defs/ISO3_obj"),
          minItems=1,
          uniqueItems=TRUE
        ),
        haz_spat_scale=list(
          title="Spatial Scale",
          description="The geographical scale of the data: sub-national, national or multi-national.",
          type="string",
          openCodelist=FALSE,
          enum=list("sub-national","national","multi-national")
        ),
        haz_spat_type=list(
          title="Hazard Spatial Data Type",
          description="The spatial data type of a given spatial dataset.",
          type="string",
          openCodelist=FALSE,
          enum=list("raster","points","lines","polygons")
        ),
        haz_spat_fileread=list(
          title="File Type and Software",
          description="Information on the specific spatial data file format, including providing an understanding of which specific software should be used to read the files.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=TRUE,
          enum=taxies%>%filter(list_name=="spatfilesoft")%>%pull(name)%>%unique()%>%c()
        ),
        haz_spat_res=list(
          title="Hazard Spatial Resolution Value",
          description="The resolution of the given spatial dataset.",
          type="number"
        ),
        haz_spat_resunits=list(
          title="Hazard Spatial Resolution Units",
          description="The units of the resolution of the given spatial dataset, such as arc-seconds or admin-level.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=TRUE,
          enum=taxies%>%filter(list_name=="gridcov")%>%pull(name)%>%unique()%>%c()
        ),
        haz_spat_crs=list(
          title="Hazard Spatial Data Coordinate System",
          description="The spatial coordinate reference system code of the given spatial dataset, such as EPSG-4326.",
          type="string",
          pattern="^(ESRI:|EPSG:)[0-9]+$"
        ),
        haz_spat_unit=list(
          # Get from WB RDLS
          title="Hazard Spatial Data Units",
          description="The units of the given spatial hazard dataset, given the IFRC units category found in the Impact Information Profiles tabular data. For example, the Richter scale could be used for earthquakes.",
          `$ref`="#/$defs/MeasUnits_obj"
        )
      ),
      required=list("haz_ISOs","haz_spat_scale","haz_spat_type","haz_spat_fileread","haz_spat_res","haz_spat_resunits")
    ),
    source=list(
      title="Source Information of Hazard Spatial Data",
      description="Information regarding the source of the spatial data of the hazard data",
      type="object",
      properties=list(
        haz_spat_srcdb=list(
          title="Hazard Spatial Data Source Database",
          description="The database name from where the spatial data of the hazard data depends",
          type="string",
          minLength=1
        ),
        haz_spat_srcorg=list(
          title="Hazard Spatial Data Source Organisation",
          description="The organisation who published the spatial data used by the hazard data",
          type="string"
        ),
        haz_spat_srcorgtype=list(
          title="Hazard Spatial Data Source Organisation Type",
          description="The type of the organisation who published the spatial data hazard data. See the taxonomy for organisation type under 'organtypes'.",
          `$ref`="#/$defs/SrcOrgType_obj"
        ),
        haz_spat_URL=list(
          title="Hazard Spatial Data Source URL",
          description="The website URL where the spatial data of the hazard data can be found",
          type="string",
          format="iri"
        ),
        haz_spat_srclic=list(
          title="Hazard Spatial Data License Information",
          description="Information concerning the license and proprietary information for the spatial hazard data.",
          type="string"
        ),
        haz_spat_orgatt=list(
          title="Source Organisation Attribution to Spatial Hazard Data",
          description="The role of the source organisation to the spatial hazard data. For example, are they the data custiodians?",
          type="string"
        )
      ),
      required=list("haz_spat_srcdb","haz_spat_URL")
    )
  ),
  required=list("ID_linkage","spatial_info","source"),
  minItems=1,
  uniqueItems=TRUE
)

#@@@@@@@@@@@@@@@@@@ Hazard Taxonomy Objects @@@@@@@@@@@@@@@@#
Monty$`$defs`$HazAb_obj=list(
  type="string",
  pattern="^[A-Z]{2}$"
)
Monty$`$defs`$HazType_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="hazardtypes")%>%pull(name)%>%unique()%>%c()
)
Monty$`$defs`$HazCluster_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="hazardsubtypes")%>%pull(name)%>%unique()%>%c()
)
Monty$`$defs`$HazSpec_obj=list(
  type="string",
  pattern="^[A-Z]{2}\\d{4}$",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(name)%>%unique()%>%c()
)

#@@@@@@@@@@@@@@ ISO3 Country Code Object @@@@@@@@@@@@@@#
Monty$`$defs`$ISO3_obj=list(
  title="ISO-3C Codes",
  description="3-letter abbreviated country codes (as characters), with respect to the ISO 3166 standard.",
  type="string",
  pattern="^[A-Z]{3}$",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="Country")%>%pull(name)%>%unique()%>%c()
)

#@@@@@@@@@@@@@@@@@ External Event ID @@@@@@@@@@@@@@@@@@#
Monty$`$defs`$ExtIDs_obj=list(
  title="External ID Codes",
  description="Any IDs, such as the GLIDE number ID, that are linked to this specific event",
  allOf=list("Ext_IDs"),
  type="object",
  properties=list(
    Ext_ID=list(type="string"),
    ID_srcdb=list(type="string"),
    ID_srcorg=list(type="string")
  ),
  minLength=1
)

#@@@@@@@@@@@@@@@@@@@@@@@ Misc @@@@@@@@@@@@@@@@@@@@@@@@#
Monty$`$defs`$Date_obj=list(
  type="string",
  format="date",
  pattern="^[012]{1}\\d{3}-[01]{1}\\d{1}-[0123]{1}\\d{1}$"
)
Monty$`$defs`$SrcOrgType_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="organtypes")%>%pull(name)%>%unique()%>%c()
)
Monty$`$defs`$EstType_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="est_type")%>%pull(name)%>%unique()%>%c()
)
Monty$`$defs`$MeasUnits_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="measunits")%>%pull(name)%>%unique()%>%c()
)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@ CONVERT TO JSON @@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
Monty_JSONtext<-jsonlite::toJSON(Monty,pretty = F,auto_unbox=T)

#@@@@@@@@@@@@@@@ CONSTRAINED TAXONOMIES @@@@@@@@@@@@@@@#
# Constrain the different taxonomies
### Hazard taxonomy ###
HIPtext<-taxies%>%filter(list_name=="hazardsubsubtypes")%>%group_by(name)%>%
  reframe(linkie=link_group,
          texter=paste0('{"if":{"properties": {"haz_spec": { "const": "',name,
                        '" }}},"then": {"properties": {"haz_cluster": { "const": "',linkie,
                        '" },"haz_type": { "const": "',taxies%>%filter(list_name=="hazardsubtypes" & name==linkie)%>%pull(link_group),
                        '" }}}}'))%>%
  pull(texter)%>%paste0(collapse = ",")
HIPtext<-paste0('"allOf": [',HIPtext,']')
# Replace this text in JSON
Monty_JSONtext<-str_replace_all(str_trim(Monty_JSONtext),
                                '\"allOf\":\\[\"haz_spec\"\\]', 
                                HIPtext)
### Impact taxonomy ###
IIPtext<-taxies%>%filter(list_name=="imp_det")%>%group_by(name)%>%
  reframe(linkie=link_group,
          texter=paste0('{"if":{"properties": {"imp_det": { "const": "',name,
                        '" }}},"then": {"properties": {"imp_subcats": { "const": "',linkie,
                        '" },"imp_cats": { "const": "',taxies%>%filter(list_name=="imp_subcats" & name==linkie)%>%pull(link_group),
                        '" }}}}'))%>%
  pull(texter)%>%paste0(collapse = ",")
IIPtext<-paste0('"allOf": [',IIPtext,']')
# Replace this text in JSON
Monty_JSONtext<-str_replace_all(str_trim(Monty_JSONtext),
                                '\"allOf\":\\[\"imp_det\"\\]', 
                                IIPtext)
### Principal hazard taxonomy ###
PHIPtext<-taxies%>%filter(list_name=="hazardsubsubtypes")%>%group_by(name)%>%
  reframe(linkie=link_group,
          texter=paste0('{"if":{"properties": {"prpl_haz_spec": { "const": "',name,
                        '" }}},"then": {"properties": {"prpl_haz_cluster": { "const": "',linkie,
                        '" },"prpl_haz_type": { "const": "',taxies%>%filter(list_name=="hazardsubtypes" & name==linkie)%>%pull(link_group),
                        '" }}}}'))%>%
  pull(texter)%>%paste0(collapse = ",")
PHIPtext<-paste0('"allOf": [',PHIPtext,']')
# Replace this text in JSON
Monty_JSONtext<-str_replace_all(str_trim(Monty_JSONtext),
                '\"allOf\":\\[\"prpl_haz_spec\"\\]', 
                PHIPtext)
### Constrain the GLIDE numbers ###
Monty_JSONtext<-str_replace_all(str_trim(Monty_JSONtext),
                                '\"allOf\":\\[\"Ext_IDs\"\\]', 
                                '"allOf": [{"if":{"properties": {"ID_srcdb": { "const": "GLIDE" }}},"then": {"properties": {"Ext_ID": { "type": "string", "pattern": "^[A-Z]{2}-\\d{4}-\\d{6}-[A-Z]{3}$", "uniqueItems": true }}}}]')
#@@@@@@@@@@@@@@ UNCONSTRAINED TAXONOMIES @@@@@@@@@@@@@@#
Monty$`$defs`$Event_obj$properties$principal_hazard<-
  Monty$`$defs`$Event_obj$properties$principal_hazard[2:length(Monty$`$defs`$Event_obj$properties$principal_hazard)]
Monty$`$defs`$Hazard_obj$properties$hazard_taxonomy<-
  Monty$`$defs`$Hazard_obj$properties$hazard_taxonomy[2:length(Monty$`$defs`$Hazard_obj$properties$hazard_taxonomy)]
Monty$`$defs`$Impact_obj$properties$impact_taxonomy<-
  Monty$`$defs`$Impact_obj$properties$impact_taxonomy[2:length(Monty$`$defs`$Impact_obj$properties$impact_taxonomy)]
Monty$`$defs`$ExtIDs_obj$items<-
  Monty$`$defs`$ExtIDs_obj$items[2:length(Monty$`$defs`$ExtIDs_obj$items)]
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@ SAVE OUT JSON @@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# Constrained version
write(Monty_JSONtext,"./Taxonomies/Montandon_Schema_V1-00.json")
# Unconstrained version
write(jsonlite::toJSON(Monty,pretty = T,auto_unbox=T),
      "./Taxonomies/Montandon_Schema_V1-00_unconstrained.json")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@ READ IN EXAMPLE @@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
ex_Monty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% VALIDATION ROUTINES %%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Provided an example of the JSON metadata, we can test to see whether it is valid or not
# jsonvalidate::json_validator(example, Monty)






