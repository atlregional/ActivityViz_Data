##################################################################################
# Create data for ARC Visulization using ActivityViz
# Author: Aditya Gore
##################################################################################

#### Sections
# 1. Load/Install required packages
# 2. Define Constants
# 3. Load required databases
# 4. Create output data
# 4a. Executive Summary Scenario
# 4b. Passive Data Scenario
# 4c. Travel Survey Scenario
# 5. Write output data


### Load/Install required packages ###############################################
##################################################################################
library(data.table)
library(jsonlite)
library(stringr)
library(sf)
library(geojsonsf)


### Define Constants #############################################################
##################################################################################

# Input files
data_dir   = file.path(getwd(), "data")

person_file      = file.path(data_dir, "abm_per_arcga.csv")
trip_file        = file.path(data_dir, "abm_trips_arcga.csv")
per_trip_dd_file = file.path(data_dir, "abm_dd_arcga.csv")
od_file          = file.path(data_dir, "od_20200228_ARCGA_DraftSubmitted_EXPANDED_20200615.csv")
od_dd_file       = file.path(data_dir, "od_data_dictionary.csv")
superdistricts_file = file.path(getwd(), "superdistricts.json")
zone_file = file.path(getwd(), "ZoneShape.GeoJSON")
county_file = file.path(getwd(), "cb_2015_us_county_500k_GEORGIA.json")

# Output files
os_output_dir = file.path(getwd(), "OnboardSurvey")

# Onboard Survey

### Load required datasets #######################################################
##################################################################################

if(!file.exists("../abmod.RData")){
  person_dt    = fread(person_file)
  trip_dt      = fread(trip_file)
  abmdd_dt     = fread(per_trip_dd_file)
  od_dt        = fread(od_file)
  od_dd_dt     = fread(od_dd_file)
  save(person_dt, trip_dt, abmdd_dt, od_dt, od_dd_dt,
       file = "abmod.RData")
} else {
  load("../abmod.RData")
}

# Create taz to superdistrict crosswalk for Fulton and DeKalb county
superdistricts_sf = geojson_sf(superdistricts_file)
zone_json = fromJSON(zone_file)
zone_json$features$geometry$coordinates = lapply(zone_json$features$geometry$coordinates,
                                                 function(x) {class(x)="numeric";x})
zone_json_str = toJSON(zone_json)
zone_json_str = sub("\"type\":\\[\"FeatureCollection\"\\],", "\"type\":\"FeatureCollection\",", zone_json_str)
zone_sf = geojson_sf(zone_json_str)

# Create taz to superdistrict crosswalk for Fulton and DeKalb county
zone_sd_sf = st_join(zone_sf, superdistricts_sf, suffix=c("_TAZ", "_SD"))
zone_sd_dt = data.table(zone_sd_sf)
zone_sd_dt = zone_sd_dt[county %in% c("Fulton", "DeKalb")]
zone_sd_dt = zone_sd_dt[zone_sd_dt[,.I[1],.(id_TAZ)][,V1]]

# Read the county file to filter out counties that are not part of the geography
county_sf = geojson_sf(county_file)

### Create output data ###########################################################
##################################################################################

### Onboard Survey ###############################################################
##################################################################################

output_ls = list()

# Demographic Data
# Age
# Check the field name
# od_dd_dt[grepl("Age", DESCRIPTION, ignore.case = TRUE)]
od_dt[AGE=="43997", AGE:="6-15"]
age_dt = od_dt[SUBMITTAL!="Dummy",.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                                    CHART_TYPE = "PARTICIPANT AGE"), by = .(AGE, AGE.Code.)]
setorder(age_dt, "AGE.Code.")
age_dt[, `AGE.Code.`:=NULL]
output_ls[["age_dt"]] = age_dt

# Household size
# Check the field name
# od_dd_dt[grepl("household", DESCRIPTION, ignore.case = TRUE)]
hhsize_dt = od_dt[SUBMITTAL!="Dummy",.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                                       CHART_TYPE = "HOUSEHOLD SIZE"), by = .(HH_SIZE, `HH_SIZE.Code.`)]
setorder(hhsize_dt, "HH_SIZE.Code.")
hhsize_dt[, `HH_SIZE.Code.`:=NULL]
output_ls[["hhsize_dt"]] = hhsize_dt

# Number employed in household
# Check the field name
# od_dd_dt[grepl("household", DESCRIPTION, ignore.case = TRUE)]
employed_hh_dt = od_dt[SUBMITTAL!="Dummy",
                       .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                         CHART_TYPE = "EMPLOYED IN HH"), by = .(EMPLOYED_IN_HH, `EMPLOYED_IN_HH.Code.`)]
setorder(employed_hh_dt, "EMPLOYED_IN_HH.Code.")
employed_hh_dt[, `EMPLOYED_IN_HH.Code.`:=NULL]
output_ls[["employed_hh_dt"]] = employed_hh_dt

# Household Income
# Check the field name
# od_dd_dt[grepl("household", DESCRIPTION, ignore.case = TRUE)]
income_dt = od_dt[SUBMITTAL!="Dummy",.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                                       CHART_TYPE = "HOUSEHOLD INCOME"), 
                  by = .(INCOME, `INCOME.Code.`)]
setorder(income_dt, "INCOME.Code.")
income_dt[, `INCOME.Code.`:=NULL]
output_ls[["income_dt"]] = income_dt

# Gender
# Check the field name
# od_dd_dt[grepl("gender", DESCRIPTION, ignore.case = TRUE)]
gender_dt = od_dt[SUBMITTAL!="Dummy",.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                                       CHART_TYPE = "PARTICIPANT GENDER"), 
                  by = .(GENDER_INFO, `GENDER_INFO.Code.`)]
setorder(gender_dt, "GENDER_INFO.Code.")
gender_dt[, `GENDER_INFO.Code.`:=NULL]
output_ls[["gender_dt"]] = gender_dt

# Race/Ethnicity
# Check the field name
# od_dd_dt[grepl("race", `FIELD NAME`, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
race_dt = od_dt[SUBMITTAL!="Dummy",.SD,.SDcols=c("ID", "Updated_UNLINKED_WGHT_FCTR",
                                                 setdiff(gsub("\\s|\\[|\\]","\\.",
                                                   od_dd_dt[grepl("race", `FIELD NAME`, ignore.case = TRUE),
                                                          `FIELD NAME`]),
                                                   "Race_Combined"))]
race_long_dt = melt.data.table(race_dt, id.vars = c("ID", "Updated_UNLINKED_WGHT_FCTR"))
race_long_dt =  race_long_dt[!value %in% c("", "No")]
race_long_dt[variable == "RACE..1.",     RACE:="American Indian / Alaska Native"]
race_long_dt[variable == "RACE..2.",     RACE:="Black/African American"]
race_long_dt[variable == "RACE..3.",     RACE:="Asian"]
race_long_dt[variable == "RACE..4.",     RACE:="White / Caucasian"]
race_long_dt[variable == "RACE..5.",     RACE:="Native Hawaiian / Pacific Islander"]
race_long_dt[variable == "RACE..Other.", RACE:="Other"]
race_long_dt[,RACE:=ifelse(.N>1,
                           ifelse(any(RACE=="Other"), "Other", "Mixed Race"), RACE),.(ID)]
race_long_dt = race_long_dt[,.N,.(ID,RACE)][,N:=NULL][]
race_long_dt = race_long_dt[race_dt[,.(ID, Updated_UNLINKED_WGHT_FCTR)],on=.(ID)]
race_long_dt[is.na(RACE), RACE:= "Not Provided"]
race_dt = race_long_dt[,.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                          CHART_TYPE = "PARTICIPANT RACE"),.(RACE)]
output_ls[["race_dt"]] = race_dt

# Number of vehicles in the household
# Check the field name
# od_dd_dt[grepl("household", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
num_veh_dt = od_dt[SUBMITTAL!="Dummy",.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                                        CHART_TYPE = "HOUSEHOLD VEH"), 
                   by = .(COUNT_VH_HH, `COUNT_VH_HH.Code.`)]
setorder(num_veh_dt, "COUNT_VH_HH.Code.")
num_veh_dt[, `COUNT_VH_HH.Code.`:=NULL]
output_ls[["num_veh_dt"]] = num_veh_dt

# If household vehicle could be used for the trip
# Check the field name
# od_dd_dt[grepl("household", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
used_veh_dt = od_dt[SUBMITTAL!="Dummy",
                    .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                      CHART_TYPE = "VEH USED"), by = .(USED_VEH_TRIP, `USED_VEH_TRIP.Code.`)]
setorder(used_veh_dt, "USED_VEH_TRIP.Code.")
used_veh_dt[, `USED_VEH_TRIP.Code.`:=NULL]
used_veh_dt[USED_VEH_TRIP=="",USED_VEH_TRIP:="ZERO VEH HH"]
output_ls[["used_veh_dt"]] = used_veh_dt

# Has a driver's license
# Check the field name
# od_dd_dt[grepl("license", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
have_dl_dt = od_dt[SUBMITTAL!="Dummy",.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                                        CHART_TYPE = "DRIVERS LICENSE"), by = .(AGE, `AGE.Code.`,
                                                                                HAVE_DL, `HAVE_DL.Code.`)]
setorder(have_dl_dt, "AGE.Code.", "HAVE_DL.Code.")
have_dl_dt[, `AGE.Code.`:=NULL]
have_dl_dt[, `HAVE_DL.Code.`:=NULL]
output_ls[["have_dl_dt"]] = have_dl_dt

# # Register to Win
# # Check the field name
# od_dd_dt[grepl("text", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
# have_dl_dt = od_dt[!is.na(`HAVE_DL.Code.`) & `HAVE_DL.Code.`!= "",.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2)), by = .(HAVE_DL, `HAVE_DL.Code.`)]
# setorder(have_dl_dt, "HAVE_DL.Code.")
# have_dl_dt[, `HAVE_DL.Code.`:=NULL]

# # Texting
# # Check the field name
# od_dd_dt[grepl("text", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
# have_dl_dt = od_dt[!is.na(`HAVE_DL.Code.`) & `HAVE_DL.Code.`!= "",.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2)), by = .(HAVE_DL, `HAVE_DL.Code.`)]
# setorder(have_dl_dt, "HAVE_DL.Code.")
# have_dl_dt[, `HAVE_DL.Code.`:=NULL]

# Transfer from and transfer to
# Check the field name
# od_dd_dt[grepl("transfer", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
transfers_od_dt = od_dt[SUBMITTAL!="Dummy",
                        .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                          CHART_TYPE = "TRANSFER FROM_TO"), 
                        by = .(PREV_TRANSFERS, `PREV_TRANSFERS.Code.`,
                               NEXT_TRANSFERS, `NEXT_TRANSFERS.Code.`)]
setorder(transfers_od_dt, "PREV_TRANSFERS.Code.", "NEXT_TRANSFERS.Code.")
transfers_od_dt[, TRANSFERS:=paste0(`PREV_TRANSFERS.Code.`, " Before - ", `NEXT_TRANSFERS.Code.`, " After")]
transfers_od_dt = transfers_od_dt[,.(TRANSFERS, COUNT, CHART_TYPE)]
output_ls[["transfers_od_dt"]] = transfers_od_dt

# Origin Transport mode
# Check the field name
# od_dd_dt[grepl("origin", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
origin_transport_dt = od_dt[SUBMITTAL!="Dummy",
                            .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                              CHART_TYPE = "ORIGIN TRANSPORT MODE"), by = .(ORIGIN_TRANSPORT, `ORIGIN_TRANSPORT.Code.`)]
setorder(origin_transport_dt, "ORIGIN_TRANSPORT.Code.")
origin_transport_dt[, `ORIGIN_TRANSPORT.Code.`:=NULL]
output_ls[["origin_transport_dt"]] = origin_transport_dt

# Origin Place Type
# Check the field name
# od_dd_dt[grepl("origin", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
origin_place_type_dt = od_dt[SUBMITTAL!="Dummy",
                             .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                               CHART_TYPE = "ORIGIN PLACE TYPE"), by = .(ORIGIN_PLACE_TYPE, `ORIGIN_PLACE_TYPE.Code.`)]
setorder(origin_place_type_dt, "ORIGIN_PLACE_TYPE.Code.")
origin_place_type_dt[, `ORIGIN_PLACE_TYPE.Code.`:=NULL]
output_ls[["origin_place_type_dt"]] = origin_place_type_dt


# Resident or Visitor
# Check the field name
# od_dd_dt[grepl("resident", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
res_vis_dt = od_dt[SUBMITTAL!="Dummy",
                   .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                     CHART_TYPE = "RESIDENT_VISITOR"), by = .(RESIDENT_OR_VISITOR, `RESIDENT_OR_VISITOR.Code.`)]
setorder(res_vis_dt, "RESIDENT_OR_VISITOR.Code.")
res_vis_dt[, `RESIDENT_OR_VISITOR.Code.`:=NULL]
output_ls[["res_vis_dt"]] = res_vis_dt

# Trip in OPPO DIR
# Check the field name
# od_dd_dt[grepl("trip", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
trip_oppo_dt = od_dt[SUBMITTAL!="Dummy",
                     .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                       CHART_TYPE = "TRIP IN OPPO DIR"), by = .(TRIP_IN_OPPO_DIR, `TRIP_IN_OPPO_DIR.Code.`)]
setorder(trip_oppo_dt, "TRIP_IN_OPPO_DIR.Code.")
trip_oppo_dt[, `TRIP_IN_OPPO_DIR.Code.`:=NULL]
output_ls[["trip_oppo_dt"]] = trip_oppo_dt

# Trip in OPPO DIR Time
# Check the field name
# od_dd_dt[grepl("trip", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
trip_oppo_time_dt = od_dt[SUBMITTAL!="Dummy",
                          .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                            CHART_TYPE = "TRIP IN OPPO DIR TIME"), by = .(TRIP_IN_OPPO_DIR, `TRIP_IN_OPPO_DIR.Code.`,
                                                                          OPPO_DIR_TRIP_TIME, `OPPO_DIR_TRIP_TIME.Code.`)]
trip_oppo_time_dt = trip_oppo_time_dt[OPPO_DIR_TRIP_TIME!=""]
# trip_oppo_time_dt[OPPO_DIR_TRIP_TIME=="", OPPO_DIR_TRIP_TIME:="Bus not used"]
setorder(trip_oppo_time_dt, "OPPO_DIR_TRIP_TIME.Code.")
trip_oppo_time_dt[, TRIP_IN_OPPO_DIR:=NULL]
trip_oppo_time_dt[, `TRIP_IN_OPPO_DIR.Code.`:=NULL]
trip_oppo_time_dt[, `OPPO_DIR_TRIP_TIME.Code.`:=NULL]
output_ls[["trip_oppo_time_dt"]] = trip_oppo_time_dt

# Employment status
# Check the field name
# od_dd_dt[grepl("employment", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
emp_status_dt = od_dt[SUBMITTAL!="Dummy",
                      # !is.na(`EMPLOYMENT_STATUS.Code.`) & `EMPLOYMENT_STATUS.Code.`!= "",
                      .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                        CHART_TYPE = "EMPLOYMENT STATUS"), by = .(EMPLOYMENT_STATUS, `EMPLOYMENT_STATUS.Code.`)]
setorder(emp_status_dt, "EMPLOYMENT_STATUS.Code.")
emp_status_dt[, `EMPLOYMENT_STATUS.Code.`:=NULL]
output_ls[["emp_status_dt"]] = emp_status_dt

# Student status
# Check the field name
# od_dd_dt[grepl("student", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
student_status_dt = od_dt[SUBMITTAL!="Dummy",
                          # !is.na(`STUDENT_STATUS.Code.`) & `STUDENT_STATUS`!= "",
                          .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                            CHART_TYPE = "STUDENT STATUS"), by = .(STUDENT_STATUS, `STUDENT_STATUS.Code.`)]
student_status_dt[is.na(STUDENT_STATUS.Code.) & `STUDENT_STATUS`== "", STUDENT_STATUS:="Missing"]
setorder(student_status_dt, "STUDENT_STATUS.Code.")
student_status_dt[, `STUDENT_STATUS.Code.`:=NULL]
output_ls[["student_status_dt"]] = student_status_dt

# Home language is other
# Check the field name
# od_dd_dt[grepl("lang", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
home_lang_other_dt = od_dt[SUBMITTAL!="Dummy",
                           # !is.na(`HOME_LANG_OTHER.Code.`) & `HOME_LANG_OTHER.Code.`!= "",
                           .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                             CHART_TYPE = "HOME LANGUAGE OTHER"), by = .(HOME_LANG_OTHER, `HOME_LANG_OTHER.Code.`)]
home_lang_other_dt[is.na(HOME_LANG_OTHER.Code.) & `HOME_LANG_OTHER`== "", HOME_LANG_OTHER:="Missing"]
setorder(home_lang_other_dt, "HOME_LANG_OTHER.Code.")
home_lang_other_dt[, `HOME_LANG_OTHER.Code.`:=NULL]
output_ls[["home_lang_other_dt"]] = home_lang_other_dt

# Home other language
# Check the field name
# od_dd_dt[grepl("lang", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
home_other_lang_dt = od_dt[SUBMITTAL!="Dummy" & HOME_LANG_OTHER.Code.==1,
                           # !is.na(`HOME_OTHER_LANG.Code.`) & `HOME_OTHER_LANG`!= "",
                           .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                             CHART_TYPE = "HOME OTHER LANGUAGE"), by = .(HOME_OTHER_LANG, `HOME_OTHER_LANG.Code.`)]
home_other_lang_dt[is.na(HOME_OTHER_LANG.Code.) & `HOME_OTHER_LANG`== "", HOME_OTHER_LANG:="Missing"]
setorder(home_other_lang_dt, "HOME_OTHER_LANG.Code.")
home_other_lang_dt[, `HOME_OTHER_LANG.Code.`:=NULL]
output_ls[["home_other_lang_dt"]] = home_other_lang_dt

# English ability
# Check the field name
# od_dd_dt[grepl("lang", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
eng_ability_dt = od_dt[SUBMITTAL!="Dummy" & HOME_LANG_OTHER.Code.==1,
                       # !is.na(`ENGLISH_ABILITY.Code.`) & `ENGLISH_ABILITY.Code.`!= "",
                       .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                         CHART_TYPE = "ENGLISH ABILITY"), by = .(ENGLISH_ABILITY, `ENGLISH_ABILITY.Code.`)]
setorder(eng_ability_dt, "ENGLISH_ABILITY.Code.")
eng_ability_dt[, `ENGLISH_ABILITY.Code.`:=NULL]
output_ls[["eng_ability_dt"]] = eng_ability_dt

# Survey Language
# Check the field name
# od_dd_dt[grepl("lang", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
survey_lang_dt = od_dt[SUBMITTAL!="Dummy",
                       # !is.na(`SURVEY_LANGUAGE.Code.`) & `SURVEY_LANGUAGE.Code.`!= "",
                       .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                         CHART_TYPE = "SURVEY LANGUAGE"), by = .(SURVEY_LANGUAGE, `SURVEY_LANGUAGE.Code.`)]
survey_lang_dt[,SURVEY_LANGUAGE:=toupper(SURVEY_LANGUAGE)]
setorder(survey_lang_dt, "SURVEY_LANGUAGE.Code.")
survey_lang_dt[, `SURVEY_LANGUAGE.Code.`:=NULL]
output_ls[["survey_lang_dt"]] = survey_lang_dt

# Fare Method Frequency
# Check the field name
# od_dd_dt[grepl("fare", `FIELD NAME`, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
fare_labels = od_dd_dt[grepl("fare_method", `FIELD NAME`, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
fare_labels[,LABEL:=gsub("If resp.* used (.*) to pay.*", "\\1", DESCRIPTION)]
fare_labels[,FIELDNAME:=gsub("\\s|\\[|\\]","\\.",`FIELD NAME`)]
setkey(fare_labels, FIELDNAME)
fare_method_dt = od_dt[,.SD,.SDcols=c("ID", "Updated_UNLINKED_WGHT_FCTR", 
                                      grep("FARE_METHOD", names(od_dt), value = TRUE))]
fare_method_long_dt = melt.data.table(fare_method_dt, id.vars = c("ID", "Updated_UNLINKED_WGHT_FCTR"))
fare_method_long_dt = fare_method_long_dt[!value %in% c("")]
fare_method_long_dt[,FARE:=fare_labels[.(variable),.(LABEL)]]
fare_method_dt = fare_method_long_dt[,.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                                        CHART_TYPE = "FARE METHOD"), by = .(FARE, RESPONSE=value)]
fare_method_dt = fare_method_dt[RESPONSE %in% c("Yes", "No")]
output_ls[["fare_method_dt"]] = fare_method_dt


# Use services
# Check the field name
# od_dd_dt[grepl("use", `FIELD NAME`, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
service_labels = od_dd_dt[grepl("USE_SERVICES_SE_MI", `FIELD NAME`, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
service_labels[,LABEL:=gsub("If resp.* use (.*) in the.*", "\\1", DESCRIPTION)]
service_labels[,FIELDNAME:=gsub("\\s|\\[|\\]","\\.",`FIELD NAME`)]
setkey(service_labels, FIELDNAME)
use_service_dt = od_dt[,.SD,.SDcols=c("ID", "Updated_UNLINKED_WGHT_FCTR", grep("USE_SERVICES_SE_MI", names(od_dt), value = TRUE))]
use_service_long_dt = melt.data.table(use_service_dt, id.vars = c("ID", "Updated_UNLINKED_WGHT_FCTR"))
use_service_long_dt = use_service_long_dt[!value %in% c("")]
use_service_long_dt[,SERVICE:=service_labels[.(variable),.(LABEL)]]
use_service_dt = use_service_long_dt[,.(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                                        CHART_TYPE = "USE SERVICES"), by = .(SERVICE, RESPONSE=value)]
use_service_dt = use_service_dt[RESPONSE %in% c("Yes", "No")]
output_ls[["use_service_dt"]] = use_service_dt

# Type of Fare
# Check the field name
# od_dd_dt[grepl("fare", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
type_fare_dt = od_dt[SUBMITTAL!="Dummy" & !is.na(`TYPE_OF_FARE.Code.`),
                     #!is.na(`TYPE_OF_FARE.Code.`) & `TYPE_OF_FARE.Code.`!= "",
                     .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                       CHART_TYPE = "TYPE OF FARE"), by = .(TYPE_OF_FARE, `TYPE_OF_FARE.Code.`)]
setorder(type_fare_dt, "TYPE_OF_FARE.Code.")
type_fare_dt[, `TYPE_OF_FARE.Code.`:=NULL]
output_ls[["type_fare_dt"]] = type_fare_dt

# Used Breeze Card
# Check the field name
# od_dd_dt[grepl("Breeze", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
used_breeze_card_dt = od_dt[SUBMITTAL!="Dummy" & !is.na(`USED_BREEZE_CARD.Code.`),
                            #!is.na(`USED_BREEZE_CARD.Code.`) & `USED_BREEZE_CARD.Code.`!= "",
                            .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                              CHART_TYPE = "USED BREEZE CARD"), by = .(USED_BREEZE_CARD, `USED_BREEZE_CARD.Code.`)]
setorder(used_breeze_card_dt, "USED_BREEZE_CARD.Code.")
used_breeze_card_dt[, `USED_BREEZE_CARD.Code.`:=NULL]
output_ls[["used_breeze_card_dt"]] = used_breeze_card_dt


# How no bus
# Check the field name
# od_dd_dt[grepl("bus", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
how_no_bus_dt = od_dt[SUBMITTAL!="Dummy" & !is.na(`HOW_NO_BUS.Code.`),
                      #!is.na(`HOW_NO_BUS.Code.`) & `HOW_NO_BUS.Code.`!= "",
                      .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                        CHART_TYPE = "HOW NO BUS"), by = .(HOW_NO_BUS, `HOW_NO_BUS.Code.`)]
setorder(how_no_bus_dt, "HOW_NO_BUS.Code.")
how_no_bus_dt[, `HOW_NO_BUS.Code.`:=NULL]
output_ls[["how_no_bus_dt"]] = how_no_bus_dt

# Transit use freq
# Check the field name
# od_dd_dt[grepl("transit", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
transit_use_freq_dt = od_dt[SUBMITTAL!="Dummy" & !is.na(`TRANSIT_USE_FREQ.Code.`),
                            #!is.na(`TRANSIT_USE_FREQ.Code.`) & `TRANSIT_USE_FREQ.Code.`!= "",
                            .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                              CHART_TYPE = "TRANSIT USE FREQ"), by = .(TRANSIT_USE_FREQ, `TRANSIT_USE_FREQ.Code.`)]
setorder(transit_use_freq_dt, "TRANSIT_USE_FREQ.Code.")
transit_use_freq_dt[, `TRANSIT_USE_FREQ.Code.`:=NULL]
output_ls[["transit_use_freq_dt"]] = transit_use_freq_dt

# HHPL Travel with you
# Check the field name
# od_dd_dt[grepl("how many", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
hhpl_dt = od_dt[SUBMITTAL!="Dummy" & !is.na(`HHPPL_TRAVEL_WITHYOU.Code.`),
                #!is.na(`HHPPL_TRAVEL_WITHYOU.Code.`) & `HHPPL_TRAVEL_WITHYOU.Code.`!= "",
                .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                  CHART_TYPE = "HHPL TRAVEL WITH YOU"), by = .(HHPPL_TRAVEL_WITHYOU, `HHPPL_TRAVEL_WITHYOU.Code.`)]
setorder(hhpl_dt, "HHPPL_TRAVEL_WITHYOU.Code.")
hhpl_dt[, `HHPPL_TRAVEL_WITHYOU.Code.`:=NULL]
output_ls[["hhpl_dt"]] = hhpl_dt

# Did you go to work
# Check the field name
# od_dd_dt[grepl("work", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
work_map = c(
  "Yes since left home" = "Yes, headed to work",
  "Yes before returning home" = "Yes, headed to home",
  "No" = "No"
)
did_y_work_dt = od_dt[SUBMITTAL!="Dummy" & !is.na(`DID_YOU_GO_2_WORK.Code.`),
                      #!is.na(`DID_YOU_GO_2_WORK.Code.`) & `DID_YOU_GO_2_WORK.Code.`!= "",
                      .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                        CHART_TYPE = "GO TO WORK"), by = .(DID_YOU_GO_2_WORK, `DID_YOU_GO_2_WORK.Code.`)]
did_y_work_dt[,DID_YOU_GO_2_WORK:=work_map[DID_YOU_GO_2_WORK] ]
setorder(did_y_work_dt, "DID_YOU_GO_2_WORK.Code.")
did_y_work_dt[, `DID_YOU_GO_2_WORK.Code.`:=NULL]
output_ls[["did_y_work_dt"]] = did_y_work_dt

# Did you go to school
# Check the field name
school_map = c(
  "Yes since left home" = "Yes, headed to school",
  "Yes before returning home" = "Yes, headed to home",
  "No" = "No"
)
# od_dd_dt[grepl("school", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
did_y_school_dt = od_dt[SUBMITTAL!="Dummy" & !is.na(`DID_YOU_GO_TO_SCHOOL.Code.`),
                        #!is.na(`DID_YOU_GO_TO_SCHOOL.Code.`) & `DID_YOU_GO_TO_SCHOOL.Code.`!= "",
                        .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                          CHART_TYPE = "GO TO SCHOOL"), by = .(DID_YOU_GO_TO_SCHOOL, `DID_YOU_GO_TO_SCHOOL.Code.`)]
did_y_school_dt[,DID_YOU_GO_TO_SCHOOL:=school_map[DID_YOU_GO_TO_SCHOOL] ]
setorder(did_y_school_dt, "DID_YOU_GO_TO_SCHOOL.Code.")
did_y_school_dt[, `DID_YOU_GO_TO_SCHOOL.Code.`:=NULL]
output_ls[["did_y_school_dt"]] = did_y_school_dt


# Survey Route by Time period
# od_dd_dt[grepl("route", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
# od_dd_dt[grepl("time", DESCRIPTION, ignore.case = TRUE), .(`FIELD NAME` , DESCRIPTION)]
route_period_dt = od_dt[SUBMITTAL!="Dummy" & !is.na(`ROUTE_SURVEYED.Code.`)&!is.na(`TIME_ON.Code.`),
                        #!is.na(`ROUTE_SURVEYED.Code.`)&!is.na(`TIME_ON.Code.`),
                        .(COUNT = round(sum(Updated_UNLINKED_WGHT_FCTR),2),
                          CHART_TYPE = "ROUTE SURVEYED BY TIME PERIOD"), 
                        by = .(ROUTE_SURVEYED, `ROUTE_SURVEYED.Code.`,
                               TIME_ON, `TIME_ON.Code.`)]
# setorder(route_period_dt, "ROUTE_SURVEYED.Code.", "TIME_ON.Code.")
route_period_dt[,TIME_ON:=paste0(`TIME_ON.Code.`, " ", TIME_ON)]
route_period_dt[,`ROUTE_SURVEYED.Code.`:=NULL]
route_period_dt[,`TIME_ON.Code.`:=NULL]
route_period_dt[,ROUTE_SURVEYED_LABEL:=gsub("\\s-.*$", "", ROUTE_SURVEYED)]
route_period_dt = route_period_dt[,.(COUNT=sum(COUNT), CHART_TYPE=unique(CHART_TYPE)),
                                  by = .(ROUTE_SURVEYED=ROUTE_SURVEYED_LABEL,
                                         TIME_ON)]
setorder(route_period_dt, -COUNT)
output_ls[["route_period_dt"]] = route_period_dt

# Trip OD for barchartmap
# ROUTECode for Marta service area starts with "MRT"
trip_origin_bar_dt = od_dt[SUBMITTAL!="Dummy" & grepl("MRT", ROUTECode) #& DESTIN_COUNTY %in% c("DeKalb County",
                                                                        #                       "Fulton County"),
                       ,.(ODTYPE = "ORIGIN", 
                         QUANTITY=round(sum(Updated_UNLINKED_WGHT_FCTR), 2)),
                       by = .(ZONE = ORIGIN_ADDRESS..MTAZ20., COUNTY = ORIGIN_COUNTY)]


trip_destination_bar_dt = od_dt[SUBMITTAL!="Dummy" & grepl("MRT", ROUTECode) #& ORIGIN_COUNTY %in% c("DeKalb County",
                                                                             #                       "Fulton County"),
                           ,.(ODTYPE = "DESTINATION", 
                             QUANTITY=round(sum(Updated_UNLINKED_WGHT_FCTR), 2)),
                           by = .(ZONE = DESTIN_ADDRESS..MTAZ20., COUNTY = DESTIN_COUNTY)]

trip_od_bar_dt = rbindlist(list(trip_origin_bar_dt, trip_destination_bar_dt), use.names = TRUE)
trip_od_bar_dt[,COUNTY:=gsub(" County", "", COUNTY)]
trip_od_bar_dt = trip_od_bar_dt[COUNTY %in% c("DeKalb", "Fulton")]
trip_od_bar_dt[,ZONE:=as.integer(ZONE)]
trip_od_bar_dt[,QUANTITY:=sum(QUANTITY),.(ZONE,ODTYPE)]
trip_od_bar_dt = trip_od_bar_dt[trip_od_bar_dt[,.I[1],.(ZONE,ODTYPE)][,V1]]
trip_od_bar_dt = trip_od_bar_dt[QUANTITY > 0]
trip_od_bar_dt = merge(trip_od_bar_dt,zone_sd_dt[county %in% c("Fulton", "DeKalb"),
                                                 .(ZONE=id_TAZ, id_SD, NAME)], by="ZONE", 
                       allow.cartesian = TRUE)#, all=TRUE)
# trip_od_bar_dt[,keep:=sum(QUANTITY, na.rm = TRUE) > 0, .(NAME)]
# trip_od_bar_dt = trip_od_bar_dt[keep==TRUE][,keep:=NULL][]
# trip_od_bar_dt[is.na(QUANTITY), QUANTITY:=0]
# trip_od_bar_dt[is.na(ODTYPE), ODTYPE:="ORIGIN"]
# trip_od_bar_dt[,QUANTITY:=ceiling(QUANTITY)]
# trip_od_bar_dt[,QUANTITY:=Prop*QUANTITY]
trip_od_bar_dt = trip_od_bar_dt[, .(QUANTITY=sum(QUANTITY), CHART_TYPE="TRIP ORIGIN OR DESTINATION"),
                                .(DISTRICT=NAME, ODTYPE)]
setorder(trip_od_bar_dt, DISTRICT, -ODTYPE)

output_ls[["trip_od_bar_dt"]] = trip_od_bar_dt



# Trip flows for chord diagram
# ROUTECode for Marta service area starts with "MRT"
trip_flow_dt = od_dt[SUBMITTAL!="Dummy" & grepl("MRT", ROUTECode),
                     .(Trips=round(sum(Updated_UNLINKED_WGHT_FCTR), 2)),
                     by = .(FROM = as.integer(ORIGIN_ADDRESS..MTAZ20.), TO = as.integer(DESTIN_ADDRESS..MTAZ20.))]
trip_flow_dt = trip_flow_dt[!(is.na(FROM)|is.na(TO))]
trip_flow_dt = merge(trip_flow_dt,zone_sd_dt[,.(FROM=id_TAZ, NAME, county)], by="FROM", 
                     allow.cartesian = TRUE, all = TRUE)
# trip_flow_dt[,Trips:=Trips*Prop]
trip_flow_dt = trip_flow_dt[!is.na(NAME)]
trip_flow_dt[!county %in% c("DeKalb", "Fulton"), Trips:=0]
trip_flow_dt[,FROM:=NAME]
trip_flow_dt[,c("NAME", "county"):=NULL]
trip_flow_dt = merge(trip_flow_dt,zone_sd_dt[,.(TO=id_TAZ, NAME, county)], by="TO", 
                     allow.cartesian = TRUE, all = TRUE)
# trip_flow_dt[,Trips:=Trips*Prop]
trip_flow_dt = trip_flow_dt[!is.na(NAME)]
trip_flow_dt = trip_flow_dt[!is.na(FROM)]
trip_flow_dt[!county %in% c("DeKalb", "Fulton"), Trips:=0]
trip_flow_dt[,TO:=NAME]
trip_flow_dt[,c("NAME","county"):=NULL]
trip_flow_dt = trip_flow_dt[,.(Trips=round(sum(Trips),2)),.(FROM,TO)]
setkey(trip_flow_dt, FROM, TO)
sel_district = sort(unique(c(trip_flow_dt$FROM, trip_flow_dt$TO)))
trip_flow_dt = trip_flow_dt[CJ(FROM=sel_district, TO=sel_district, unique = TRUE)]
trip_flow_dt[is.na(Trips), Trips:=0]
output_ls[["trip_flow_dt"]] = trip_flow_dt

# Trip flows for chord diagram by County
trip_flow_county_dt = od_dt[SUBMITTAL!="Dummy" & grepl("MRT", ROUTECode),
                     .(Trips=round(sum(Updated_UNLINKED_WGHT_FCTR), 2)),
                     by = .(FROM = ORIGIN_COUNTY, TO = DESTIN_COUNTY)]
trip_flow_county_dt[,FROM:=gsub(" County", "", FROM)]
trip_flow_county_dt[,TO:=gsub(" County", "", TO)]
trip_flow_county_dt = trip_flow_county_dt[FROM %in% county_sf$NAME & TO %in% county_sf$NAME]
setcolorder(trip_flow_county_dt, c("FROM", "TO", "Trips"))
# The chart does not properly display if the first county from ordered list is not in FROM column
setkey(trip_flow_county_dt, FROM, TO)
sel_county = sort(unique(c(trip_flow_county_dt$FROM, trip_flow_county_dt$TO)))
trip_flow_county_dt = trip_flow_county_dt[CJ(FROM=sel_county, TO=sel_county, unique = TRUE)]
setkey(trip_flow_county_dt, FROM, TO)
trip_flow_county_dt[is.na(Trips), Trips:=0]
# trip_flow_county_dt[1,Trips:=ifelse(is.na(Trips), 1e-2, Trips)]
# trip_flow_county_dt = trip_flow_county_dt[!is.na(Trips)]
output_ls[["trip_flow_county_dt"]] = trip_flow_county_dt

# Create zone filter files
sd_filter_dt = zone_sd_dt[,.(.N, value=1),.(id=id_SD,NAME,NAME2=NAME)]
sd_dt = data.table(superdistricts_sf)[NAME %in% trip_flow_dt$FROM,.(id,NAME)]
sd_dt[,NAME2:=NAME]
sd_dt[,value:=0]
sd_dt[sd_filter_dt, value:=i.value,on=.(NAME, NAME2)]
superdistricts_dt = dcast(sd_dt, id+NAME~NAME2, value.var = "value", fill = 0)
output_ls[["superdistricts_dt"]] = superdistricts_dt

# Create desirelines districts
desirelines_sf = superdistricts_sf[superdistricts_sf$id %in% zone_sd_dt$id_SD, ]
desirelines_sf = st_centroid(desirelines_sf)
linecomb = expand.grid(ind=seq_len(nrow(desirelines_sf)),ind2=seq_len(nrow(desirelines_sf)))
linecomb = linecomb[linecomb$ind!=linecomb$ind2,]
all_lines_sf_sf = suppressWarnings(suppressMessages(t(mapply(function(ind1, ind2) {st_cast(
  st_union(desirelines_sf[ind1,], desirelines_sf[ind2,]),
  "LINESTRING")}, linecomb$ind, linecomb$ind2))))
all_lines_sf_df = data.frame(all_lines_sf_sf)
all_lines_sf_df$geometry = unlist(all_lines_sf_df$geometry, recursive = FALSE)
all_lines_sf_df = st_as_sf(all_lines_sf_df)
all_lines_sf_df$o = as.integer(unlist(all_lines_sf_df$id))
all_lines_sf_df$d = as.integer(unlist(all_lines_sf_df$id.1))
newdesirelines_sf = all_lines_sf_df[,c("o", "d", "geometry")]
st_write(newdesirelines_sf, "obs_desirelines.GeoJSON", driver = "GeoJSON", delete_dsn = TRUE)
st_write(superdistricts_sf[superdistricts_sf$id %in% newdesirelines_sf$o,c("id", "NAME", "geometry")],
         "superdistricts.GeoJSON", driver = "GeoJSON", delete_dsn = TRUE)

# Create desirelines counties
desirelines_sf = county_sf[county_sf$NAME %in% trip_flow_county_dt$FROM,]
desirelines_sf = st_centroid(desirelines_sf)
desirelines_sf$id = as.integer(paste0(desirelines_sf$STATEFP, desirelines_sf$COUNTYFP))
linecomb = expand.grid(ind=seq_len(nrow(desirelines_sf)),ind2=seq_len(nrow(desirelines_sf)))
linecomb = linecomb[linecomb$ind!=linecomb$ind2,]
all_lines_sf_sf = suppressWarnings(suppressMessages(t(mapply(function(ind1, ind2) {st_cast(
  st_union(desirelines_sf[ind1,], desirelines_sf[ind2,]),
  "LINESTRING")}, linecomb$ind, linecomb$ind2))))
all_lines_sf_df = data.frame(all_lines_sf_sf)
all_lines_sf_df$geometry = unlist(all_lines_sf_df$geometry, recursive = FALSE)
all_lines_sf_df = st_as_sf(all_lines_sf_df)
all_lines_sf_df$o = as.integer(unlist(all_lines_sf_df$id))
all_lines_sf_df$d = as.integer(unlist(all_lines_sf_df$id.1))
newdesirelines_sf = all_lines_sf_df[,c("o", "d", "geometry")]
st_write(newdesirelines_sf, "obs_county_desirelines.GeoJSON", driver = "GeoJSON", delete_dsn = TRUE)

county_sf$id = as.integer(paste0(county_sf$STATEFP, county_sf$COUNTYFP))
st_write(county_sf[county_sf$id %in% newdesirelines_sf$o,c("id", "NAME", "geometry")],
         "obs_county.GeoJSON", driver = "GeoJSON", delete_dsn = TRUE)

lapply(output_ls, function(x) {
  if(ncol(x) == 3 & !any(names(x) %in% c("FROM", "TO"))) {
    x[,GROUP:="Expanded Response"]
    setcolorder(x, c("GROUP"))
  }
  TRUE
})

### Write output data ############################################################
##################################################################################

## Onboard Survey
# Snapshot
for(table_name in names(output_ls)){
  filename = file.path(os_output_dir, paste0(gsub("_dt", "", table_name), ".csv"))
  fwrite(output_ls[[table_name]],
         file = filename)
  cat(basename(filename), "\n")
}



# Function to reformat DESCRIPTION
formatDescription = function(x){
  paste0(paste0("<br>", strsplit(x, split = "\\s?\n\\s?")[[1]], "</br>"),collapse = "")
}

# # Trip OD
# fwrite(overall_trip_dt,         file = trip_od_overall_file_d)
# 
# ## Passive Data
# # Geojson files
# st_write(overall_sf,         driver = "GeoJSON", dsn = overall_geo_file,         delete_dsn = TRUE)
# st_write(hillsborough_sf,    driver = "GeoJSON", dsn = hillsborough_geo_file,    delete_dsn = TRUE)
# st_write(pinellas_sf,        driver = "GeoJSON", dsn = pinellas_geo_file,        delete_dsn = TRUE)
# st_write(pasco_sf,           driver = "GeoJSON", dsn = pasco_geo_file,           delete_dsn = TRUE)
# st_write(hernando_citrus_sf, driver = "GeoJSON", dsn = hernando_citrus_geo_file, delete_dsn = TRUE)
# 
# # Trip OD
# 
# ## Travel Survey
# # Trip OD
# fwrite(overall_trip_dt,         file = trip_od_overall_file)
# fwrite(hillsborough_trip_dt,    file = trip_od_hillsborough_file)
# fwrite(pinellas_trip_dt,        file = trip_od_pinellas_file)
# fwrite(pasco_trip_dt,           file = trip_od_pasco_file)
# fwrite(hernando_citrus_trip_dt, file = trip_od_hernando_citrus_file)
# 
# # Trips Mode
# fwrite(trips_mode, file = trip_mode_file)
# # Seasonal trips mode
# fwrite(seasonal_trips_mode, file = seasonal_trip_mode_file)
# # University trips mode
# fwrite(uni_trips_mode, file = uni_trip_mode_file)
# # Trips Zone
# fwrite(trip_zone, file = trip_zone_file)
# # Day Pattern
# fwrite(day_pattern_dt, file = day_pattern_file)
# # Time Use
# fwrite(time_use_dt, file = time_use_file)


