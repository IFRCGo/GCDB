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
    title="Event-Level Information",
    description="Information about the event, without taking into account any impact estimates 
    or hazard data.",
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
            description="Event-level identifier code that is unique and distinct from that of any other hazard occurrence, 
            we generate this by condensing the abbreviated hazard title (e.g. EQ for earthquake), 
            the event start date, and the countries affected (separated by commas)",
            minLength=1),
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
          GLIDES=list(
            title="Linked GLIDE Numbers",
            type="array",
            description="International GLIDE numbers used for interoperable identification",
            items=list(
              type="string",
              pattern="^[A-Z]{2}-\d{4}-\d{6}-[A-Z]{3}$",
              uniqueItems=TRUE
            )
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
          location=list(
            title="Descriptive Event Location",
            type="string",
            description="Brief character description of where the event took place, this is unimportant but could be used to check the spatial-information of the data at a later stage"
          ),
          Continents=list(
            title="Continents or Regions",
            description="List of continents or regions affected",
            type="array",
            minItems=1,
            uniqueItems=TRUE
          )
        ),
        required=list("ISO3s")
      ),
      temporal=list(
        title="Time-Related Data",
        description="Data related to dates and time periods of the hazards. 
        Note that, for multiple hazards, the event start and end dates would be
        the minimum and the maximum dates over all of the hazards.",
        type="object",
        properties=list(
          ev_sdate=list(
            title="Event Start Date",
            description="Start date of the hazard event. For unclear start dates, 
            please use the earliest plausible date.",
            type="string",
            format="date"
          ),
          ev_fdate=list(
            title="Event End Date",
            description="End date of the hazard event. For unclear end dates, 
            please use the latest plausible date. Please note that this is when the
            hazard ended, not when the post-disaster recovery was achieved.",
            type="string",
            format="date"
          )),
        required=list("ev_sdate","ev_fdate")
      ),
      principal_hazard=list(
        title="Taxonomy of Principal Hazard",
        description="Classification of the principal hazard associated to the event, 
        with respect to the UNDRR-ISC 2020 Hazard Information Profiles report.
        Note that we define 'principal' hazard here as not necessarily the first 
        hazard to arrive, but the hazard that the event is most likely to be 
        attributed to. For example, a cyclone event might result in flooding, 
        heatwaves, thunderstorms, etc, but the principal event would remain a cyclone.",
        type="object",
        properties=list(
          prpl_haz_Ab=list(
            title="Abbreviated Hazard ID",
            description="Two-letter abbreviated ID for the principal hazard. For example, 
            floods would be 'FL' and earthquakes 'EQ'. These abbreviations are 
            pre-determined and standardised, and should be found online.",
            type="string",
            pattern="^[A-Z]{2}$"
          ), 
          prpl_haz_type=list(
            title="Principal Hazard Type",
            description="The most concise categorisation of the hazards, into eight 
            categories. Please see the UNDRR-ISC 2020 Hazard Information Profiles report
            for more information.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="hazardtypes")%>%pull(name)%>%unique()%>%list()
          ),
          prpl_haz_cluster=list(
            title="Principal Hazard Cluster",
            description="The second layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report
            for more information.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="hazardsubtypes")%>%pull(name)%>%unique()%>%list()
          ),
          prpl_haz_spec=list(
            title="Principal Specific Hazard",
            description="The specific hazard is the final layer in the categorisation of the hazards. 
            Please see the UNDRR-ISC 2020 Hazard Information Profiles report
            for more information.",
            type="string",
            pattern="^[A-Z]{2}\d{4}$",
            codelist="ImpactInformationProfiles.csv",
            minLength=1,
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(name)%>%unique()%>%list()
          )
        )
      )
    ),
    required=list("ID_linkage","spatial","temporal")
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
          ),
          linked_GLIDES=list(
            title="Linked GLIDE Numbers",
            type="array",
            description="International GLIDE numbers used for interoperable identification",
            items=list(
              type="string",
              pattern="^[A-Z]{2}-\d{4}-\d{6}-[A-Z]{3}$",
              uniqueItems=TRUE
            )
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
      impact_estimate=list(
        title="Impact Value Data",
        description="Impact estimate value and unit information",
        type="object",
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
      impact_taxonomy=list(
        title="Impact Categorisation",
        description="Description of the categorisation of the impact estimate
        with respect to the IFRC Impact Information Profiles (IIPs).",
        type="object",
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
        type="array",
        items=list(`$ref`="#/$defs/SpatialImpact_obj")
      )
    ),
    required=list("ID_linkage","source","impact_estimate","impact_taxonomy","temporal","spatial")
  ),
  
  #@@@@@@@@@@@@@@@@@@@ Hazard Object @@@@@@@@@@@@@@@@@@@@#
  Hazard_obj=list(
    type="object",
    description="'A process, phenomenon or human activity that may cause loss of life, 
    injury or other health impacts, property damage, social and 
    economic disruption or environmental degradation.' 
    UNDRR - https://www.undrr.org/terminology/hazard",
    properties=list(
      ### ID ###
      ID_linkage=list(
        title="Hazard and Event ID",
        description="Impact ID information and links to other object instances within an event.
          Note: no spatial IDs are linked here, see the spatial element.",
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
          GLIDE=list(
            title="Linked GLIDE Number",
            type="string",
            description="International GLIDE numbers used for interoperable identification",
            pattern="^[A-Z]{2}-\d{4}-\d{6}-[A-Z]{3}$",
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
            description="The type of the organisation who published the hazard data. 
            See the taxonomy for organisation type under 'organtypes'.",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="organtypes")%>%pull(name)%>%unique()%>%list()
          ),
          haz_src_URL=list(
            title="Hazard Data Source URL",
            description="The website URL where the hazard data was published",
            type="string",
            format="iri"
          )
        ),
        required=list("haz_src_db","haz_src_URL")
      ),
      hazard_detail=list(
        title="Basic Hazard Information",
        description="Basic information about the hazard, including things like 
        the maximum magnitude/intensity/severity values and any related 
        trigger/triggering/concurrent hazards (observed or even potential).",
        type="object",
        properties=list(
          haz_maxvalue=list(
            title="Maximum Hazard Severity Value",
            description="The estimated maximum hazard intensity/magnitude/severity value, 
            as a number, without the units.",
            type="number",
            minimum=0
          ),
          haz_units=list(
            title="Maximum Hazard Severity Units",
            description="The units of the estimated hazard intensity/magnitude/severity.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="measunits")%>%pull(name)%>%unique()%>%list()
          ),
          haz_est_type=list(
            title="Hazard Data Estimate Type",
            description="What was the type of data source that was used to create 
            this hazard intensity/magnitude/severity estimate? Primary data? Secondary data? 
            Modelled data: was it estimated without any event-specific data?",
            type="array",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="est_type")%>%pull(name)%>%unique()%>%list()
          ),
          concur_haz=list(
            title="Concurrent Hazard Linkage",
            description="Linking the observed and potentially unobserved hazards linked to
            each specific hazard in the event.",
            type="object",
            properties=list(
              
              
              
              
              
              
              
              
              # haz_sub_ID of linked hazard (NA if empty)
              
              # GLIDE number of linked hazard
              
              # Role - triggered by, trigger, concurrent, all, other 
              
              # 
              
              
              
              
              
              
              
              
              
            )
          )
        )
      ),
      hazard_taxonomy=list(
        title="Hazard Taxonomy",
        description="Classification of this specific hazard, 
        with respect to the UNDRR-ISC 2020 Hazard Information Profiles report.",
        type="object",
        properties=list(
          haz_Ab=list(
            title="Abbreviated Hazard ID",
            description="Two-letter abbreviated ID for the hazard. For example, 
            floods would be 'FL' and earthquakes 'EQ'. These abbreviations are 
            pre-determined and standardised, and should be found online.",
            type="string",
            pattern="^[A-Z]{2}$"
          ), 
          haz_type=list(
            title="Hazard Type",
            description="The most concise categorisation of the hazards, into eight 
            categories. Please see the UNDRR-ISC 2020 Hazard Information Profiles report
            for more information.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="hazardtypes")%>%pull(name)%>%unique()%>%list()
          ),
          haz_cluster=list(
            title="Hazard Cluster",
            description="The second layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report
            for more information.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="hazardsubtypes")%>%pull(name)%>%unique()%>%list()
          ),
          haz_spec=list(
            title="Specific Hazard",
            description="The specific hazard is the final layer in the categorisation of the hazards. 
            Please see the UNDRR-ISC 2020 Hazard Information Profiles report
            for more information.",
            type="string",
            pattern="^[A-Z]{2}\d{4}$",
            codelist="ImpactInformationProfiles.csv",
            minLength=1,
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(name)%>%unique()%>%list()
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
              description="Starting date of this specific hazard. For unclear start dates, 
              please use the earliest plausible date.",
              type="string",
              format="date"
            ),
            haz_fdate=list(
              title="Hazard End Date",
              description="End date of this specific hazard. For unclear end dates, 
              please use the latest plausible date. Please note that this is when the
              hazard ended, not when the post-disaster recovery was achieved.",
              type="string",
              format="date"
            )),
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
    ),
    
    #@@@@@@@@@@@@@@@ Spatial Hazard Object @@@@@@@@@@@@@@@@#
    SpatialHazard_obj=list(
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
              description="Location of where the spatial data can be found. 
              Note that this can be either a local folder or a direct URL to the source.",
              type="string",
              minLength=1
            ),
            haz_spat_colIDs=list(
              title="Spatial Data Column Indices",
              description="Indices of the specific spatial file columns which are 
              referred to by the hazard data.",
              type="array",
              items=list(type="integer")
            ),
            haz_spat_rowIDs=list(
              title="Spatial Data Row Indices",
              description="Indices of the specific spatial file rows which are 
              referred to by the hazard data",
              type="array",
              items=list(type="integer")
            )
          ),
          required=list("haz_spat_ID","haz_spat_fileloc","haz_spat_colIDs","haz_spat_rowIDs")
        ),
        spatial_info=list(
          title="Spatial Data Information",
          description="Information that is directly related to the spatial data itself, 
          such as the countries associated to the area, or the data form: raster, point, line or polygon.",
          type="object",
          properties=list(
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
            haz_spat_type=list(
              title="Hazard Spatial Data Type",
              description="The spatial data type of a given spatial dataset.",
              type="string",
              openCodelist=FALSE,
              enum=list("raster","points","lines","polygons")
            ),
            haz_spat_res=list(
              title="Hazard Spatial Resolution Value",
              description="The resolution of the given spatial dataset.",
              type="numeric"
            ),
            haz_spat_resunits=list(
              title="Hazard Spatial Resolution Units",
              description="The units of the resolution of the given spatial dataset.",
              type="string",
              codelist="ImpactInformationProfiles.csv",
              openCodelist=TRUE,
              enum=taxies%>%filter(list_name=="gridcov")%>%pull(name)%>%unique()%>%list()
            ),
            
            
            
            
            
            
            
            
            
            
            haz_spat_unit=list(
              # Get from WB RDLS
              title="Hazard Spatial Data Units",
              description="The units of the given spatial hazard dataset, given the IFRC 
              units category found in the Impact Information Profiles tabular data.",
              type="string",
              codelist="ImpactInformationProfiles.csv",
              openCodelist=TRUE,
              enum=taxies%>%filter(list_name=="measunits")%>%pull(name)%>%unique()%>%list()
            ),
            
            
            
            
            
            
            
            
            
            
            
            
            haz_spat_esttype=list(
              title="Hazard Data Type",
              description="What was the type of data source that was used to create 
              the hazard data? Primary data? Secondary data? 
              Modelled data: was it estimated without any event-specific data?",
              type="array",
              codelist="ImpactInformationProfiles.csv",
              openCodelist=FALSE,
              enum=taxies%>%filter(list_name=="est_type")%>%pull(name)%>%unique()%>%list()
            )
          ),
          required=list("countries","spatial_scale","haz_spat_type","haz_spat_res","haz_spat_resunits")
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
              description="The type of the organisation who published the spatial data hazard data. 
              See the taxonomy for organisation type under 'organtypes'.",
              codelist="ImpactInformationProfiles.csv",
              openCodelist=FALSE,
              enum=taxies%>%filter(list_name=="organtypes")%>%pull(name)%>%unique()%>%list()
            ),
            haz_spat_URL=list(
              title="Hazard Spatial Data Source URL",
              description="The website URL where the spatial data of the hazard data can be found",
              type="string",
              format="iri"
            )
          ),
          required=list("haz_spat_srcdb","haz_spat_URL")
        )
      ),
      required=list("ID_linkage","spatial_info","source"),
      minItems=1,
      uniqueItems=TRUE
    ),
    
    #@@@@@@@@@@@@@@@ Spatial Impact Object @@@@@@@@@@@@@@@@#
    SpatialImpact_obj=list(
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
              description="Location of where the spatial data can be found. 
              Note that this can be either a local folder or a direct URL to the source.",
              type="string",
              minLength=1
            ),
            imp_spat_colIDs=list(
              title="Spatial Data Column Indices",
              description="Indices of the specific spatial file columns which are 
              referred to by the impact estimate",
              type="array",
              items=list(type="integer")
            ),
            imp_spat_rowIDs=list(
              title="Spatial Data Row Indices",
              description="Indices of the specific spatial file rows which are 
              referred to by the impact estimate",
              type="array",
              items=list(type="integer")
            )
          ),
          required=list("imp_spat_ID","imp_spat_fileloc","imp_spat_colIDs","imp_spat_rowIDs")
        ),
        
        spatial_info=list(
          title="Spatial Data Information",
          description="Information that is directly related to the spatial data itself, 
          such as the form: raster, point, line or polygon.",
          type="object",
          properties=list(
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
            imp_spat_type=list(
              title="Impact Spatial Data Type",
              description="The spatial data type of a given spatial dataset. 
              For example, spatial polygon data which is of administrative boundaries
              and at admin-level 2. You would enter 'adminlevel' here, which is the 
              correct coded name for administrative boundary spatial data.",
              type="string",
              openCodelist=FALSE,
              enum=list("raster","points","lines","polygons")
            ),
            imp_spat_res=list(
              title="Impact Spatial Resolution Value",
              description="The resolution of the given spatial dataset. 
              For example, spatial polygon data which is of administrative boundaries
              and at admin-level 2. You would enter '2' here.",
              type="numeric"
            ),
            imp_spat_resunits=list(
              title="Impact Spatial Resolution Units",
              description="The units of the resolution of the given spatial dataset.",
              type="string",
              codelist="ImpactInformationProfiles.csv",
              openCodelist=TRUE,
              enum=taxies%>%filter(list_name=="gridcov")%>%pull(name)%>%unique()%>%list()
            )
          ),
          required=list("countries","spatial_scale","imp_spat_type","imp_spat_res","imp_spat_resunits")
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
              description="The type of the organisation who published the spatial data impact estimate. 
              See the taxonomy for organisation type under 'organtypes'.",
              codelist="ImpactInformationProfiles.csv",
              openCodelist=FALSE,
              enum=taxies%>%filter(list_name=="organtypes")%>%pull(name)%>%unique()%>%list()
            ),
            imp_spat_URL=list(
              title="Impact Spatial Data Source URL",
              description="The website URL where the spatial data of the impact estimate can be found",
              type="string",
              format="iri"
            )
          ),
          required=list("imp_spat_srcdb","imp_spat_URL")
        )
      ),
      required=list("ID_linkage","spatial_info","source"),
      minItems=1,
      uniqueItems=TRUE
    ),
    
    #@@@@@@@@@@@@@@ ISO3 Country Code Object @@@@@@@@@@@@@@#
    ISO3_obj=list(
      title="ISO-3C Codes",
      description="3-letter abbreviated country codes (as characters), 
      with respect to the ISO 3166 standard.",
      type="string",
      pattern="^[A-Z]{2}$",
      codelist="ImpactInformationProfiles.csv",
      openCodelist=FALSE,
      enum=taxies%>%filter(list_name=="Country")%>%pull(name)%>%unique()%>%list()
    )
  )
)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% VALIDATION ROUTINES %%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Provided an example of the JSON metadata, we can test to see whether it is valid or not
jsonvalidate::json_validator(example, Monty)






