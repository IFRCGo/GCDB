source("./RCode/Setup/GetPackages.R")
# For any tips on learning JSON Schemas, check this link out:
# https://json-schema.org/learn/glossary
# Get the RDLS JSON schema
RDLS<-jsonlite::fromJSON("https://docs.riskdatalibrary.org/en/0__2__0/rdls_schema.json")





stop("change _fileread to _format, check _spat_fileloc and _spat_URL")





# Get the taxonomy data
taxies<-openxlsx::read.xlsx("./ImpactInformationProfiles.xlsx")
# Country codes and associated regions
counties<-openxlsx::read.xlsx("./Taxonomies/IsoContinentRegion.xlsx")%>%
    filter(!is.na(Country))%>%mutate(Continent=convIso3Continent_alt(ISO.Code))
# Make sure the extra countries not present here are included
tmp<-data.frame(ISO.Code=(taxies%>%filter(list_name=="Country")%>%pull(name))[!taxies%>%filter(list_name=="Country")%>%pull(name)%in%counties$ISO.Code])%>%
                mutate(Country=convIso3Country(ISO.Code),continent=convIso3Continent_alt(ISO.Code))
# Add them
othcounties<-counties%>%filter(ISO.Code=="damndaniel"); othcounties[1:nrow(tmp),]<-NA_character_
counties%<>%rbind(othcounties%>%mutate(ISO.Code=tmp$ISO.Code,Country=tmp$Country,Continent=tmp$continent))%>%
  filter(!duplicated(ISO.Code)); rm(tmp,othcounties)
counties%<>%dplyr::select(ISO.Code,Country,UN.Region,World.Bank.Regions,Continent,UN.Sub.Region,World.Bank.Income.Groups)
colnames(counties)[1]<-"ISO3"
# Create the data frames of the taxonomies to be saved out later
exp_class<-data.frame(
  exp_spec_code=taxies%>%filter(list_name=="exp_specs")%>%pull(name),
  exp_spec_lab=taxies%>%filter(list_name=="exp_specs")%>%pull(label),
  exp_subcat_code=taxies%>%filter(list_name=="exp_specs")%>%pull(link_group),
  exp_subcat_lab=left_join(taxies[taxies$list_name=="exp_specs",2:4],
                   taxies[taxies$list_name=="exp_subcats",2:4],
                   by=c("link_group"="name"))%>%pull(label.y),
  exp_cat_code=taxies%>%filter(list_name=="exp_specs")%>%pull(link_maingroup),
  exp_cat_lab=left_join(left_join(taxies[taxies$list_name=="exp_specs",2:4],
                             taxies[taxies$list_name=="exp_subcats",2:4],
                             by=c("link_group"="name")),
                   taxies[taxies$list_name=="exp_cats",2:3],
                   by=c("link_group.y"="name"))%>%pull(label)
)
imp_class<-data.frame(
  imp_type_code=taxies%>%filter(list_name=="imp_type")%>%pull(name),
  imp_type_lab=taxies%>%filter(list_name=="imp_type")%>%pull(label)
)
haz_class<-data.frame(
  haz_spec_code=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(name),
  haz_spec_lab=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(label),
  haz_cluster_code=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(link_group),
  haz_cluster_lab=left_join(taxies[taxies$list_name=="hazardsubsubtypes",2:4],
                   taxies[taxies$list_name=="hazardsubtypes",2:4],
                   by=c("link_group"="name"))%>%pull(label.y),
  haz_type_code=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(link_maingroup),
  haz_type_lab=left_join(left_join(taxies[taxies$list_name=="hazardsubsubtypes",2:4],
                             taxies[taxies$list_name=="hazardsubtypes",2:4],
                             by=c("link_group"="name")),
                   taxies[taxies$list_name=="hazardtypes",2:3],
                   by=c("link_group.y"="name"))%>%pull(label)  
)
units_info<-data.frame(
  unit_codes=taxies%>%filter(list_name=="measunits")%>%pull(name)%>%na.omit(),
  units_lab=taxies%>%filter(list_name=="measunits")%>%pull(label)%>%na.omit(),
  unit_groups_code=taxies%>%filter(list_name=="measunits")%>%pull(link_group)%>%na.omit()
)
units_info%<>%left_join(taxies%>%
                          filter(list_name=="measunitgroups")%>%
                          dplyr::select(2:3)%>%
                          rename(unit_groups_code=name,unit_groups_lab=label),
                        by="unit_groups_code")
est_type<-taxies%>%filter(list_name=="est_type")%>%dplyr::select(2:3)%>%
  rename(est_type_code=name,est_type_lab=label)
spatial_coverage<-taxies%>%filter(list_name=="spatialcoverage")%>%
  dplyr::select(2:3)%>%rename(spat_cov_code=name,spat_cov_lab=label)

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
#@@@@@@@@@@@@@@@@@@@@@@ RESPONSE-LEVEL PROPERTIES @@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$response_Data<-list(
  title="Response Data",
  description="A collection of data on the response/interventions to the hazard event",
  type="array",
  items=list(`$ref`="#/$defs/Response_obj"),
  uniqueItems=TRUE
)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ TAXONOMIES @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
Monty$properties$taxonomies<-list(
  title="Montandon Taxonomy Codes",
  description="The full list of required taxonomy codes necessary to interpret the data",
  type="object",
  properties=list(
    ISO_info=list(
      title="Country and Region Information",
      description="Information concerning the country, such as ISO3C code, country name, region, continent, etc.",
      type="array",
      items=list(
        type="object",
        properties=list(
          ISO3=list(
            title="ISO3 Character Code",
            description="The character-based three letter country code with respect to the alpha-3 ISO 3166-1 standard",
            type="string",
            codelist="IsoContinentRegion.csv",
            openCodelist=FALSE,
            enum=counties$ISO3%>%na.omit()%>%unique()
          ),
          country=list(
            title="Country Name",
            description="The country name",
            type="string",
            codelist="IsoContinentRegion.csv",
            openCodelist=FALSE,
            enum=counties$Country%>%na.omit()%>%unique()
          ),
          region_UN=list(
            title="UN-Defined Region",
            description="The region, as defined by the UN",
            type="string",
            codelist="IsoContinentRegion.csv",
            openCodelist=FALSE,
            enum=counties$UN.Region%>%na.omit()%>%unique()
          ),
          region_WB=list(
            title="World Bank-Defined Region",
            description="The region, as defined by the World Bank",
            type="string",
            codelist="IsoContinentRegion.csv",
            openCodelist=FALSE,
            enum=counties$World.Bank.Regions%>%na.omit()%>%unique()
          ),
          continent=list(
            title="Continent",
            description="The continent",
            type="string",
            codelist="IsoContinentRegion.csv",
            openCodelist=FALSE,
            enum=counties$Continent%>%na.omit()%>%unique()
          ),
          subregion_UN=list(
            title="UN-Defined Sub-Region",
            description="The sub-region, as defined by the UN",
            type="string",
            codelist="IsoContinentRegion.csv",
            openCodelist=FALSE,
            enum=counties$UN.Sub.Region%>%na.omit()%>%unique()
          ),
          income_WB=list(
            title="Country Income Group",
            description="The country income group, as defined by the World Bank",
            type="string",
            codelist="IsoContinentRegion.csv",
            openCodelist=FALSE,
            enum=counties$World.Bank.Income.Groups%>%na.omit()%>%unique()
          )
        )
      )
    ),
    exp_class=list(
      title="Exposure Taxonomy",
      description="The taxonomy of the different exposure categorisations",
      type="array",
      items=list(
        type="object",
        properties=list(
          exp_spec_code=list(
            title="Exposure Detail Code",
            description="The code of the exposure detail",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=exp_class$exp_spec_code%>%na.omit()%>%unique()
          ),
          exp_spec_lab=list(
            title="Exposure Detail Label",
            description="The label or full name of the exposure detail",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=exp_class$exp_spec_lab%>%na.omit()%>%unique()
          ),
          exp_subcat_code=list(
            title="Exposure Sub-Category Code",
            description="The code for the exposure sub-category",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=exp_class$exp_subcat_code%>%na.omit()%>%unique()
          ),
          exp_subcat_lab=list(
            title="Exposure Sub-Category Label",
            description="The label or full name of the exposure sub-category",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=exp_class$exp_subcat_lab%>%na.omit()%>%unique()
          ),
          exp_cat_code=list(
            title="Exposure Category Code",
            description="The code of the exposure category",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=exp_class$exp_cat_code%>%na.omit()%>%unique()
          ),
          exp_cat_lab=list(
            title="Exposure Category Label",
            description="The label or full name of the exposure category",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=exp_class$exp_cat_lab%>%na.omit()%>%unique()
          )
        )
      )
    ),
    imp_class=list(
      title="Impact Taxonomy",
      description="The taxonomy of the different impact and loss categorisations",
      type="array",
      items=list(
        type="object",
        properties=list(
          imp_type_code=list(
            title="Impact Type Code",
            description="The code of the impact type",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=imp_class$imp_type_code%>%na.omit()%>%unique()
          ),
          imp_type_lab=list(
            title="Impact Type Label",
            description="The label or full name of the impact type",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=imp_class$imp_type_lab%>%na.omit()%>%unique()
          )
        )
      )
    ),
    haz_class=list(
      title="Hazard Taxonomy",
      description="The taxonomy of the different hazard categorisations",
      type="array",
      items=list(
        type="object",
        properties=list(
          haz_spec_code=list(
            title="Specific Hazard Code",
            description="The coded name of the specific hazard",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=haz_class$haz_spec_code%>%na.omit()%>%unique()
          ),
          haz_spec_lab=list(
            title="Specific Hazard Name",
            description="The name of the specific hazard",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=haz_class$haz_spec_lab%>%na.omit()%>%unique()
          ),
          haz_cluster_code=list(
            title="Hazard Cluster Code",
            description="The coded name of the hazard cluster",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=haz_class$haz_cluster_code%>%na.omit()%>%unique()
          ),
          haz_cluster_lab=list(
            title="Hazard Cluster Name",
            description="The name of the hazard cluster",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=haz_class$haz_cluster_lab%>%na.omit()%>%unique()
          ),
          haz_type_code=list(
            title="Hazard Type Code",
            description="The coded name of the hazard type",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=haz_class$haz_type_code%>%na.omit()%>%unique()
          ),
          haz_type_lab=list(
            title="Hazard Type Label",
            description="The name of the hazard type",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=haz_class$haz_type_lab%>%na.omit()%>%unique()
          )
        )
      )
    ),
    src_info=list(
      title="Source Information",
      description="The list of all source information relevant to this specific dataset.",
      type="array",
      items=list(
        type="object",
        properties=list(
          src_org_code=list(
            title="Organisation Coded Name",
            description="The organisation coded name.",
            type="string"
          ), 
          src_org_lab=list(
            title="Organisation Label Name",
            description="The organisation name, in the format '<name> (<acronym>)'.",
            type="string"
          ), 
          src_org_typecode=list(
            title="Organisation Type Code",
            description="The code name of the organisation type. See the taxonomy for organisation type under 'organtypes'.",
            `$ref`="#/$defs/SrcOrgType_obj"
          ),
          src_org_typelab=list(
            title="Organisation Type Label",
            description="The label name of the organisation type. See the taxonomy for organisation type under 'organtypes'.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="organtypes")%>%pull(label)%>%na.omit()%>%unique()
          ),
          src_org_email=list(
            title="Impact Data Source Contact Email",
            description="Email address of the source organisation that hosts the impact data estimate.",
            type="string",
            format="email"
          ),
          src_db_code=list(
            title="Database Coded Name",
            description="The coded name of the database.",
            type="string"
          ),
          src_db_lab=list(
            title="Database Label Name",
            description="The label name of the database.",
            type="string"
          ),
          src_db_attr=list(
            title="Organisation Attribution to Database",
            description="The attribution of the database to the source organisation. For example, the curator.",
            type="string"
          ),
          src_db_lic=list(
            title="Database License Information",
            description="Information concerning the license and proprietary information for the database.",
            type="string"
          ),
          src_db_URL=list(
            title="Impact Data Source URL",
            description="The website URL where the impact estimate was published",
            type="string",
            format="iri"
          ),
          src_addinfo=list(
            title="Impact Data Source Additional Information",
            description="Any additional comments or information about the dataset or the source organisation.",
            type="string"
          )
        )
      )
    ),
    units_info=list(
      title="Units Taxonomy",
      description="The full list of taxonomies of the units.",
      type="array",
      items=list(
        type="object",
        properties=list(
          unit_codes=list(
            title="Unit Code",
            description="The coded name of the unit.",
            `$ref`="#/$defs/MeasUnits_obj"
          ), 
          units_lab=list(
            title="Unit Name",
            description="The label name of the unit.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=TRUE,
            enum=units_info$units_lab
          ),
          unit_groups_code=list(
            title="Unit Group Code",
            description="The code of the hierarchical group that the unit belongs to.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=TRUE,
            enum=units_info$unit_groups_code
          ),
          unit_groups_lab=list(
            title="Unit Group Label",
            description="The name of the hierarchical group that the unit belongs to.",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=TRUE,
            enum=units_info$unit_groups_lab
          )
        )
      )
    ),
    est_type=list(
      title="Estimation Type",
      description="The full list of taxonomies of the estimation type.",
      type="array",
      items=list(
        type="object",
        properties=list(
          est_type_code=list(
            title="Estimate Type Code",
            description="The codes that describe the estimate types",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="est_type")%>%pull(name)%>%unique()%>%na.omit()
          ),
          est_type_lab=list(
            title="Estimate Type Label",
            description="The label that describes the estimate types",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="est_type")%>%pull(label)%>%unique()%>%na.omit()
          )
        )
      )
    ),
    spatial_coverage=list(
      title="Spatial Coverage Taxonomy",
      description="The full list of taxonomies of the spatial coverage.",
      type="array",
      items=list(
        type="object",
        properties=list(
          spat_cov_code=list(
            title="General Spatial Coverage Codes",
            description="The codes for the most succinct grouping of spatial coverage data, including polygon, point and line data",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="spatialcoverage")%>%pull(name)%>%unique()%>%na.omit()
          ),
          spat_cov_lab=list(
            title="General Spatial Coverage Names",
            description="The labels for the most succinct grouping of spatial coverage data, including polygon, point and line data",
            type="string",
            codelist="ImpactInformationProfiles.csv",
            openCodelist=FALSE,
            enum=taxies%>%filter(list_name=="spatialcoverage")%>%pull(label)%>%unique()%>%na.omit()
          )
        )
      )
    )
  )
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

Monty$required<-c("monty_Info","event_Level","impact_Data","hazard_Data","response_Data")

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
        ev_ISO3s=list(
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
        )
      ),
      required=list("ev_ISO3s")
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
    allhaz_class=list(
      title="Taxonomy of All Hazards",
      description="Classification of the hazards associated to the event, with respect to the UNDRR-ISC 2020 Hazard Information Profiles report.",
      type="array",
      items=list(
        type="object",
        properties=list(
          all_hazs_Ab=list(
            title="Abbreviated Hazard ID",
            description="Two-letter abbreviated ID for the hazards. For example, floods would be 'FL' and earthquakes 'EQ'. These abbreviations are pre-determined and standardised, and should be found online.",
            `$ref`="#/$defs/HazAb_obj"
          ),
          all_hazs_spec=list(
            title="All Specific Hazards",
            description="The specific hazards is the final layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
            `$ref`="#/$defs/HazSpec_obj"
          )
        )
      ),
      required=list("all_hazs_Ab","all_hazs_spec")
    )
  ),
  required=list("ID_linkage","spatial","temporal","allhaz_class")
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
        imp_src_org=list(
          title="Impact Data Source Organisation",
          description="The organisation who published the impact estimate",
          type="string"
        ),
        imp_src_URL=list(
          title="Impact Data Source URL",
          description="The website URL where the impact data was published",
          type="string",
          format="iri"
        )
      ),
      required=list("imp_src_db","imp_src_URL")
    ),
    ### Impact Value ###
    impact_detail=list(
      title="Impact Value Data",
      description="Impact estimate value and unit information, including the exposure classification of what was impacted.",
      type="object",
      properties=list(
        exp_spec=list(
          title="Impact Detail",
          description="The sub-sub category of impact, which is the specific asset or population demographic that has been impacted by the hazard.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=TRUE,
          enum=taxies%>%filter(list_name=="exp_specs")%>%pull(name)%>%unique()%>%na.omit()
        ),
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
          enum=taxies%>%filter(list_name=="imp_type")%>%pull(name)%>%unique()%>%na.omit()
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
      required=list("exp_spec","imp_value","imp_type","imp_units","imp_est_type")
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
  required=list("ID_linkage","source","impact_detail","temporal","spatial")
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
      description="Basic information about the hazard, including things like the maximum magnitude/intensity/severity values and any related trigger/triggering/concurrent hazards (observed or even potential).",
      type="object",
      properties=list(
        haz_Ab=list(
          title="Abbreviated Hazard ID",
          description="Two-letter abbreviated ID for the hazard. For example, floods would be 'FL' and earthquakes 'EQ'. These abbreviations are pre-determined and standardised, and should be found online.",
          `$ref`="#/$defs/HazAb_obj"
        ), 
        haz_spec=list(
          title="Specific Hazard",
          description="The specific hazard is the final layer in the categorisation of the hazards. Please see the UNDRR-ISC 2020 Hazard Information Profiles report for more information.",
          `$ref`="#/$defs/HazSpec_obj"
        ),
        haz_maxvalue=list(
          title="Maximum Hazard Severity Value",
          description="The estimated maximum hazard intensity/magnitude/severity value, as a number, without the units.",
          type="number",
          minimum=0
        ),
        haz_maxunits=list(
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
  required=list("ID_linkage","source","hazard_detail","temporal","spatial")
)





























#@@@@@@@@@@@@@@@@@@ Response Object @@@@@@@@@@@@@@@@@@@#
Monty$`$defs`$Response_obj=list(
  title="Response Object of Montandon",
  type="object",
  description="Responses and interventions to the disaster, which aim to reduce the impact or speed up recovery.",
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
        res_sub_ID=list(
          title="Response ID",
          description="ID of each response element, within the overall event",
          type="string",
          minLength=1
        ),
        imp_sub_ID=list(
          title="Impact Estimate ID",
          description="ID of each impact element, within the overall event",
          type="string"
        ),
        haz_sub_ID=list(
          title="Hazard ID Link",
          description="ID of each hazard event data element, within the overall event",
          type="array",
          items=list(type="string",uniqueItems=TRUE)
        )
      ),
      required=list("event_ID","res_sub_ID")
    )
  ),
  required=list("ID_linkage")
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
        imp_spat_colname=list(
          title="Spatial Data Column Indices",
          description="Indices of the specific spatial file columns which are referred to by the impact estimate",
          type="array",
          items=list(type="string")
        ),
        imp_spat_rowname=list(
          title="Spatial Data Row Indices",
          description="Indices of the specific spatial file rows which are referred to by the impact estimate",
          type="array",
          items=list(type="string")
        )
      ),
      required=list("imp_spat_ID","imp_spat_fileloc","imp_spat_colname","imp_spat_rowname")
    ),
    
    spatial_info=list(
      title="Spatial Data Information",
      description="Information that is directly related to the spatial data itself, such as the form: raster, point, line or polygon.",
      type="object",
      properties=list(
        imp_ISO3s=list(
          title="Country ISO3 Codes",
          description="List of countries affected, by ISO3 code.",
          type="array",
          items=list(`$ref`="#/$defs/ISO3_obj"),
          minItems=1,
          uniqueItems=TRUE
        ),
        imp_spat_covcode=list(
          title="Impact Spatial Data Type",
          description="The spatial data type of a given spatial dataset. For example, spatial polygon data which is of administrative boundaries and at admin-level 2. You would enter 'adminlevel' here, which is the correct coded name for administrative boundary spatial data.",
          type="string",
          openCodelist=FALSE,
          enum=spatial_coverage$spat_cov_code
        ),
        imp_spat_fileread=list(
          title="File Type and Software",
          description="Information on the specific spatial data file format, including providing an understanding of which specific software should be used to read the files.",
          type="string",
          codelist="ImpactInformationProfiles.csv",
          openCodelist=TRUE,
          enum=taxies%>%filter(list_name=="spatfilesoft")%>%pull(name)%>%unique()%>%na.omit()
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
          enum=taxies%>%filter(list_name=="measunits" & link_group=="unitsspatial")%>%pull(name)%>%unique()%>%na.omit()
        ),
        imp_spat_crs=list(
          title="Impact Spatial Data Coordinate System",
          description="The spatial coordinate reference system code of the given spatial dataset, such as EPSG:4326. Note that this can either be in ESRI or EPSG format",
          type="string",
          pattern="^(ESRI:|EPSG:)[0-9]+$"
        )
      ),
      required=list("imp_ISO3s","imp_spat_covcode","imp_spat_res","imp_spat_resunits")
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
        haz_spat_colname=list(
          title="Spatial Data Column Indices",
          description="Indices of the specific spatial file columns which are referred to by the hazard data.",
          type="array",
          items=list(type="string")
        ),
        haz_spat_rowname=list(
          title="Spatial Data Row Indices",
          description="Indices of the specific spatial file rows which are referred to by the hazard data",
          type="array",
          items=list(type="string")
        )
      ),
      required=list("haz_spat_ID","haz_spat_fileloc","haz_spat_colname","haz_spat_rowname")
    ),
    spatial_info=list(
      title="Spatial Data Information",
      description="Information that is directly related to the spatial data itself, such as the countries associated to the area, or the data form: raster, point, line or polygon.",
      type="object",
      properties=list(
        haz_ISO3s=list(
          title="Country ISO3 Codes",
          description="List of countries affected, by ISO3 code.",
          type="array",
          items=list(`$ref`="#/$defs/ISO3_obj"),
          minItems=1,
          uniqueItems=TRUE
        ),
        haz_spat_covcode=list(
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
          enum=taxies%>%filter(list_name=="spatfilesoft")%>%pull(name)%>%unique()%>%na.omit()
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
          enum=taxies%>%filter(list_name=="measunits" & link_group=="unitsspatial")%>%pull(name)%>%unique()%>%na.omit()
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
      required=list("haz_ISO3s","haz_spat_covcode","haz_spat_fileread","haz_spat_res","haz_spat_resunits")
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
  enum=taxies%>%filter(list_name=="hazardtypes")%>%pull(name)%>%unique()%>%na.omit()
)
Monty$`$defs`$HazCluster_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="hazardsubtypes")%>%pull(name)%>%unique()%>%na.omit()
)
Monty$`$defs`$HazSpec_obj=list(
  type="string",
  pattern="^[A-Z]{2}\\d{4}$",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="hazardsubsubtypes")%>%pull(name)%>%unique()%>%na.omit()
)

#@@@@@@@@@@@@@@ ISO3 Country Code Object @@@@@@@@@@@@@@#
Monty$`$defs`$ISO3_obj=list(
  title="ISO-3C Codes",
  description="3-letter abbreviated country codes (as characters), with respect to the ISO 3166 standard.",
  type="string",
  pattern="^[A-Z]{3}$",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="Country")%>%pull(name)%>%unique()%>%na.omit()
)

#@@@@@@@@@@@@@@@@@ External Event ID @@@@@@@@@@@@@@@@@@#
Monty$`$defs`$ExtIDs_obj=list(
  allOf=list("Ext_IDs"),
  title="External ID Codes",
  description="Any IDs, such as the GLIDE number ID, that are linked to this specific event",
  type="object",
  properties=list(
    ext_ID=list(type="string"),
    ext_ID_db=list(type="string"),
    ext_ID_org=list(type="string")
  ),
  minLength=1
)

#@@@@@@@@@@@@@@@@@@@@@@@ Misc @@@@@@@@@@@@@@@@@@@@@@@@#
Monty$`$defs`$Date_obj=list(
  title="Date in UTC+00:00",
  type="string",
  format="date",
  pattern="^[012]{1}\\d{3}-[01]{1}\\d{1}-[0123]{1}\\d{1}$"
)
Monty$`$defs`$SrcOrgType_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="organtypes")%>%pull(name)%>%unique()%>%na.omit()
)
Monty$`$defs`$EstType_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=taxies%>%filter(list_name=="est_type")%>%pull(name)%>%unique()%>%na.omit()
)
Monty$`$defs`$MeasUnits_obj=list(
  type="string",
  codelist="ImpactInformationProfiles.csv",
  openCodelist=FALSE,
  enum=units_info$unit_codes
)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@ CONVERT TO JSON @@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
Monty_JSONtext<-jsonlite::toJSON(Monty,pretty = F,auto_unbox=T)

#@@@@@@@@@@@@@@@ CONSTRAINED TAXONOMIES @@@@@@@@@@@@@@@#
### Constrain the GLIDE numbers ###
Monty_JSONtext<-str_replace_all(str_trim(Monty_JSONtext),
                                '\"allOf\":\\[\"Ext_IDs\"\\]', 
                                '"allOf": [{"if":{"properties": {"ext_ID_db": { "const": "GLIDE" }}},"then": {"properties": {"Ext_ID": { "type": "string", "pattern": "^[A-Z]{2}-\\d{4}-\\d{6}-[A-Z]{3}$", "uniqueItems": true }}}}]')
#@@@@@@@@@@@@@@ UNCONSTRAINED TAXONOMIES @@@@@@@@@@@@@@#
Monty$`$defs`$ExtIDs_obj<-
  Monty$`$defs`$ExtIDs_obj[2:length(Monty$`$defs`$ExtIDs_obj)]

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@ SAVE OUT JSON @@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# Constrained version
write(Monty_JSONtext,"./Taxonomies/Montandon_Schema_V1-00.json")
# Unconstrained version
write(jsonlite::toJSON(Monty,pretty = T,auto_unbox=T),
      "./Taxonomies/Montandon_Schema_V1-00_unconstrained.json")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@ GENERATE EXAMPLE @@@@@@@@@@@@@@@@@@#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
# Read it in
ex_Monty<-jsonlite::fromJSON("./Taxonomies/Montandon_JSON-Example.json")
# Modify the specified object class structure version
ex_Monty$monty_Info$class_version<-Monty_JSONschema
# Date this template was generated
ex_Monty$monty_Info$db_pubdate<-Sys.Date()
# Ensure all the taxonomy data is present (except src_info which is added at the data-level)
ex_Monty$taxonomies<-list(ISO_info=counties,
                          exp_class=exp_class,
                          imp_class=imp_class,
                          haz_class=haz_class,
                          src_info=ex_Monty$taxonomies$src_info,
                          units_info=units_info,
                          est_type=est_type,
                          spatial_coverage=spatial_coverage)
# Helpful resources for the user on the Montandon database
ex_Monty$monty_Info$helpful_resources<-data.frame(
  title=c("Global Crisis Data Bank - Concept Note","COP28 'All About That Base' Report"),
  description=c("The original concept note of the Global Crisis Data Bank, which we also refer to as the Montandon database. This document provides an overview on the database and it's application within IFRC.",
                "A detailed report introducing the Montandon database and demonstrating some of its use-cases, by applying it to provide evidence to support the hypothesis that climate change is not just enhancing the frequency and intensity of climate- and weather-related hazards, but also their impacts. You can find the report in the provided URL, navigate to the section 'Other GO Resources' and you'll see the link to the report, titled COP28 'All About That Base' Report"),
  URL=c("https://www.undrr.org/media/83713/","https://go.ifrc.org/resources")
)
# Write out the modified example
write(jsonlite::toJSON(ex_Monty,pretty = T,auto_unbox=T),
      "./Taxonomies/Montandon_JSON-Example.json")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%% VALIDATION ROUTINES %%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Provided an example of the JSON metadata, we can test to see whether it is valid or not
# jsonvalidate::json_validator(example, Monty)
stop("Add longitude and latitude point of event")


