##################################################################################
# Create data for Tampa Bay Passive Data Visulization using ActivityViz
# Author: Aditya Gore
##################################################################################

#### Sections
# 1. Load/Install required packages
# 2. Define Constants
# 3. Load required databases
# 4. Create output data
# 4a. Passive Data Scenario
# 4b. Executive Summary Scenario
# 5. Write output data


### Load/Install required packages ###############################################
##################################################################################
library(data.table)
library(jsonlite)
library(stringr)
library(sf)
library(geojsonsf)
library(omxr)


### Define Constants #############################################################
##################################################################################

# Input files
data_dir   = getwd()

survey_trip_od = file.path(data_dir, "Executive_Summary", "trip_od_overall.csv")
trip_ii_file      = file.path(data_dir, "Tampa_Internals.omx")
trip_ie_file      = file.path(data_dir, "Tampa.omx")

# Geography input files
taz_file       = file.path(getwd(), "tampa2020.json")
county_file    = file.path(getwd(), "counties.json")
agg_file       = file.path(getwd(), "tampa.csv")

# Output files
es_output_dir = file.path(getwd(), "Executive_Summary")
pd_output_dir = file.path(getwd(), "Passive_Data")

# Executive Summary
es_trip_daily_od_overall_file = file.path(es_output_dir, "trip_od_overall.csv") # Daily overall passive data OD charts

# Passive_Data
pd_trip_daily_od_overall_file         = file.path(pd_output_dir, 
                                                  "trip_daily_pod_overall.csv")         # Daily overall passive data OD charts
pd_trip_daily_od_hillsborough_file    = file.path(pd_output_dir, 
                                                  "trip_daily_pod_hillsborough.csv")    # Daily hillsborough passive data OD charts
pd_trip_daily_od_pinellas_file        = file.path(pd_output_dir, 
                                                  "trip_daily_pod_pinellas.csv")        # Daily pinellas passive data OD charts
pd_trip_daily_od_pasco_file           = file.path(pd_output_dir, 
                                                  "trip_daily_pod_pasco.csv")           # Daily pasco passive data OD charts
pd_trip_daily_od_hernando_citrus_file = file.path(pd_output_dir, 
                                                  "trip_daily_pod_hernando_citrus.csv") # Daily hernando_citrus passive data OD charts

pd_trip_res_vis_od_overall_file         = file.path(pd_output_dir, 
                                                  "trip_res_vis_pod_overall.csv")         # Residents/Visitors overall passive data OD charts
pd_trip_res_vis_od_hillsborough_file    = file.path(pd_output_dir, 
                                                  "trip_res_vis_pod_hillsborough.csv")    # Residents/Visitors hillsborough passive data OD charts
pd_trip_res_vis_od_pinellas_file        = file.path(pd_output_dir, 
                                                  "trip_res_vis_pod_pinellas.csv")        # Residents/Visitors pinellas passive data OD charts
pd_trip_res_vis_od_pasco_file           = file.path(pd_output_dir, 
                                                  "trip_res_vis_pod_pasco.csv")           # Residents/Visitors pasco passive data OD charts
pd_trip_res_vis_od_hernando_citrus_file = file.path(pd_output_dir, 
                                                  "trip_res_vis_pod_hernando_citrus.csv") # Residents/Visitors hernando_citrus passive data OD charts



### Load required datasets #######################################################
##################################################################################

# if(!file.exists("../hts_data.RData")){
#   household_dt = fread(household_file)
#   person_dt    = fread(person_file)
#   day_dt       = fread(day_file)
#   trip_dt      = fread(trip_ii_file)
#   save(household_dt, person_dt, day_dt, trip_dt,
#        file = "hts_data.RData")
# } else {
#   load("../hts_data.RData")
# }

trip_ii_dt = setDT(read_all_omx(trip_ii_file))
trip_taz = read_lookup(trip_ii_file, "Rows")$Lookup
trip_ii_dt[,":="(oTAZ=trip_taz[origin], dTAZ=trip_taz[destination])]
trip_ie_dt = setDT(read_all_omx(trip_ie_file))
trip_taz = read_lookup(trip_ie_file, "Rows")$Lookup
trip_ie_dt[,":="(oTAZ=trip_taz[origin], dTAZ=trip_taz[destination])]
trip_ie_dt[,Residents:=Residents_AM+Residents_MD+Residents_NT+Residents_PM]
trip_ie_dt[,Visitors:=Visitors_AM+Visitors_MD+Visitors_NT+Visitors_PM]
# trip_dt = merge(trip_ii_dt, trip_ie_dt, by=c("origin", "destination",
#                                              "oTAZ", "dTAZ"),
#                 suffixes = c("_II", "_IE"))
trip_ii_dt[,Type:="Internal"]
trip_ie_dt[,Type:="External"]
trip_dt = rbindlist(list(trip_ii_dt, trip_ie_dt), use.names = TRUE, fill = TRUE)
trip_od_survey = fread(file = survey_trip_od)

taz_ls = fromJSON(taz_file)
county_ls = fromJSON(county_file)
taz_sf = geojson_sf(taz_file)
taz_dt = data.table(taz_sf)
agg_dt = fread(agg_file)
agg_dt[,HILLSBOROUGH_LBL_3:=gsub("\\&|\\/","",HILLSBOROUGH_LBL_3)]
agg_dt[,HILLSBOROUGH_LBL_3:=gsub("_"," ",HILLSBOROUGH_LBL_3)]
# agg_dt[,HILLSBOROUGH_LBL_3:=gsub("\\s+","_",HILLSBOROUGH_LBL_3)]
agg_dt[,PINELLAS_LBL:=gsub("\\&|\\/","",PINELLAS_LBL)]
agg_dt[,PINELLAS_LBL:=gsub("_"," ",PINELLAS_LBL)]
# agg_dt[,PINELLAS_LBL:=gsub("\\s+","_",PINELLAS_LBL)]
agg_dt[,PASCO_LBL:=gsub("\\&|\\/","",PASCO_LBL)]
agg_dt[,PASCO_LBL:=gsub("_"," ",PASCO_LBL)]
# agg_dt[,PASCO_LBL:=gsub("\\s+","_",PASCO_LBL)]
agg_dt[,HERNANDO_CITRUS_LBL_2:=gsub("\\&|\\/","",HERNANDO_CITRUS_LBL_2)]
agg_dt[,HERNANDO_CITRUS_LBL_2:=gsub("_"," ",HERNANDO_CITRUS_LBL_2)]
# agg_dt[,HERNANDO_CITRUS_LBL_2:=gsub("\\s+","_",HERNANDO_CITRUS_LBL_2)]
agg_dt[,D7_ALL_LBL:=gsub("\\&|\\/","",D7_ALL_LBL)]
agg_dt[,D7_ALL_LBL:=gsub("_"," ",D7_ALL_LBL)]
# agg_dt[,D7_ALL_LBL:=gsub("\\s+","_",D7_ALL_LBL)]
agg_sf = st_as_sf(merge(as.data.frame(agg_dt), taz_sf, by.x="TAZ", by.y="id",all.x = TRUE))



### Create output data ###########################################################
##################################################################################

### Passive Data Scenario ########################################################
##################################################################################

# Chord Diagram
trip_dt[trip_dt<0] = 0
# trip_ii_dt[,Residents:=Residents_AM+Residents_MD+Residents_NT+Residents_PM]
# trip_ii_dt[,Visitors:=Visitors_AM+Visitors_MD+Visitors_NT+Visitors_PM]
# trip_ii_dt[,Residents:=Residents_AM+Residents_MD+Residents_OP+Residents_PM]
# trip_ii_dt[,Visitors:=Visitors_AM+Visitors_MD+Visitors_OP+Visitors_PM]

trip_est_dt = trip_dt[,.(OTAZ = oTAZ, DTAZ = dTAZ, Type, Daily, Residents, Visitors)]
trip_est_dt =  merge(trip_est_dt, agg_dt[,.(TAZ, DISTRICT, HILLSBOROUGH_LBL_3, PINELLAS_LBL, PASCO_LBL,
                                            HERNANDO_CITRUS_LBL_2, D7_ALL_LBL, 
                                            COUNTY_NAME=`COUNTY NAME`)],
                     by.x = "OTAZ", by.y="TAZ", all.x = TRUE)
trip_est_dt =  merge(trip_est_dt, agg_dt[,.(TAZ, DISTRICT, HILLSBOROUGH_LBL_3, PINELLAS_LBL, PASCO_LBL,
                                            HERNANDO_CITRUS_LBL_2, D7_ALL_LBL, 
                                            COUNTY_NAME=`COUNTY NAME`)],
                     by.x = "DTAZ", by.y="TAZ", all.x = TRUE,
                     suffixes = c("_O", "_D"))
label_names = paste0(rep(c("D7_ALL_LBL", "HILLSBOROUGH_LBL_3", "PINELLAS_LBL", 
                "PASCO_LBL", "HERNANDO_CITRUS_LBL_2"),each=2), rep(c("_O", "_D"),5))
trip_est_dt[Type=="External", c(label_names):=lapply(.SD,function(x) ifelse(is.na(x),"External", x)),
            .SDcols = label_names]
trip_est_dt = trip_est_dt[Daily > 0]
# Overall
## Daily
daily_overall_trip_dt         = trip_est_dt[,.(TRIPS = sum(Daily)),.(FROM = D7_ALL_LBL_O,
                                                                     TO = D7_ALL_LBL_D)]
daily_overall_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                            TO   = ifelse(is.na(TO), "External", TO))]
setkey(daily_overall_trip_dt, FROM, TO)
daily_overall_trip_dt = daily_overall_trip_dt[CJ(c(agg_dt$D7_ALL_LBL,"External"),
                                                 c(agg_dt$D7_ALL_LBL,"External"), unique = TRUE)]
daily_overall_trip_dt[is.na(TRIPS), TRIPS := 0]
daily_overall_trip_dt[, TRIPS:=round(TRIPS, 0)]
# daily_overall_trip_dt = daily_overall_trip_dt[TRIPS!=0]
## Residents/Visitors
res_vis_overall_trip_dt         = trip_est_dt[,.(`RESIDENT TRIPS` = round(sum(Residents), 0),
                                                   `VISITOR TRIPS` = round(sum(Visitors), 0)),.(FROM = D7_ALL_LBL_O,
                                                                     TO = D7_ALL_LBL_D)]
res_vis_overall_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                              TO   = ifelse(is.na(TO), "External", TO))]

# Hillsborough
## Daily
daily_hillsborough_trip_dt         = trip_est_dt[grepl("Hillsborough", COUNTY_NAME_O)|
                                              grepl("Hillsborough", COUNTY_NAME_D),
                                              .(TRIPS = sum(Daily)),
                                              .(FROM = HILLSBOROUGH_LBL_3_O,
                                                TO = HILLSBOROUGH_LBL_3_D)]
daily_hillsborough_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                            TO   = ifelse(is.na(TO), "External", TO))]
setkey(daily_hillsborough_trip_dt, FROM, TO)
HILLSBOROUGH_LBL_3 = c(agg_dt[!is.na(HILLSBOROUGH_LBL_3), unique(HILLSBOROUGH_LBL_3)], "External")
daily_hillsborough_trip_dt = daily_hillsborough_trip_dt[CJ(c(HILLSBOROUGH_LBL_3),
                                                           c(HILLSBOROUGH_LBL_3),
                                                           unique = TRUE)]
daily_hillsborough_trip_dt[is.na(TRIPS), TRIPS:=0]
daily_hillsborough_trip_dt[, TRIPS:=round(TRIPS, 0)]
# daily_hillsborough_trip_dt = daily_hillsborough_trip_dt[TRIPS>0]
## Residents/Visitors
res_vis_hillsborough_trip_dt         = trip_est_dt[grepl("Hillsborough", COUNTY_NAME_O)|
                                                  grepl("Hillsborough", COUNTY_NAME_D),
                                                  .(`RESIDENT TRIPS` = round(sum(Residents), 0),
                                                    `VISITOR TRIPS` = round(sum(Visitors), 0)),
                                                  .(FROM = HILLSBOROUGH_LBL_3_O,
                                                    TO = HILLSBOROUGH_LBL_3_D)]
res_vis_hillsborough_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                                TO   = ifelse(is.na(TO), "External", TO))]
# Pinellas
## Daily
daily_pinellas_trip_dt         = trip_est_dt[grepl("Pinellas", COUNTY_NAME_O)|
                                                   grepl("Pinellas", COUNTY_NAME_D),.(TRIPS = sum(Daily)),
                                             .(FROM = PINELLAS_LBL_O,
                                               TO = PINELLAS_LBL_D)]
daily_pinellas_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                                 TO   = ifelse(is.na(TO), "External", TO))]
setkey(daily_pinellas_trip_dt, FROM, TO)
PINELLAS_LBL = c(agg_dt[!is.na(PINELLAS_LBL), unique(PINELLAS_LBL)], "External")
daily_pinellas_trip_dt = daily_pinellas_trip_dt[CJ(c(PINELLAS_LBL),
                                                   c(PINELLAS_LBL),
                                                   unique = TRUE)]
daily_pinellas_trip_dt[is.na(TRIPS), TRIPS:=0]
daily_pinellas_trip_dt[, TRIPS:=round(TRIPS, 0)]
## Residents/Visitors
res_vis_pinellas_trip_dt         = trip_est_dt[grepl("Pinellas", COUNTY_NAME_O)|
                                                       grepl("Pinellas", COUNTY_NAME_D),
                                                 .(`RESIDENT TRIPS` = round(sum(Residents), 0),
                                                   `VISITOR TRIPS` = round(sum(Visitors), 0)),
                                                 .(FROM = PINELLAS_LBL_O,
                                                   TO = PINELLAS_LBL_D)]
res_vis_pinellas_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                                     TO   = ifelse(is.na(TO), "External", TO))]
# Pasco
## Daily
daily_pasco_trip_dt         = trip_est_dt[grepl("Pasco", COUNTY_NAME_O)|
                                               grepl("Pasco", COUNTY_NAME_D),.(TRIPS = sum(Daily)),
                                             .(FROM = PASCO_LBL_O,
                                               TO = PASCO_LBL_D)]
daily_pasco_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                             TO   = ifelse(is.na(TO), "External", TO))]
setkey(daily_pasco_trip_dt, FROM, TO)
PASCO_LBL = c(agg_dt[!is.na(PASCO_LBL), unique(PASCO_LBL)], "External")
daily_pasco_trip_dt = daily_pasco_trip_dt[CJ(c(PASCO_LBL),
                                             c(PASCO_LBL),
                                             unique = TRUE)]
daily_pasco_trip_dt[is.na(TRIPS), TRIPS:=0]
daily_pasco_trip_dt[, TRIPS:=round(TRIPS, 0)]
## Residents/Visitors
res_vis_pasco_trip_dt         = trip_est_dt[grepl("Pasco", COUNTY_NAME_O)|
                                                   grepl("Pasco", COUNTY_NAME_D),
                                                 .(`RESIDENT TRIPS` = round(sum(Residents), 0),
                                                   `VISITOR TRIPS` = round(sum(Visitors), 0)),
                                                 .(FROM = PASCO_LBL_O,
                                                   TO = PASCO_LBL_D)]
res_vis_pasco_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                                 TO   = ifelse(is.na(TO), "External", TO))]
# Hernando/Citrus
## Daily
daily_hernando_citrus_trip_dt         = trip_est_dt[grepl("Hernando", COUNTY_NAME_O)|
                                                      grepl("Hernando", COUNTY_NAME_D)|
                                                      grepl("Citrus", COUNTY_NAME_O)|
                                                      grepl("Citrus", COUNTY_NAME_D),.(TRIPS = sum(Daily)),
                                          .(FROM = HERNANDO_CITRUS_LBL_2_O,
                                            TO = HERNANDO_CITRUS_LBL_2_D)]
daily_hernando_citrus_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                          TO   = ifelse(is.na(TO), "External", TO))]
setkey(daily_hernando_citrus_trip_dt, FROM, TO)
HERNANDO_CITRUS_LBL_2 = c(agg_dt[!is.na(HERNANDO_CITRUS_LBL_2), unique(HERNANDO_CITRUS_LBL_2)], "External")
daily_hernando_citrus_trip_dt = daily_hernando_citrus_trip_dt[CJ(c(HERNANDO_CITRUS_LBL_2),
                                                                 c(HERNANDO_CITRUS_LBL_2),
                                                                 unique = TRUE)]
daily_hernando_citrus_trip_dt[is.na(TRIPS), TRIPS:=0]
daily_hernando_citrus_trip_dt[, TRIPS:=round(TRIPS, 0)]
## Residents/Visitors
res_vis_hernando_citrus_trip_dt         = trip_est_dt[grepl("Hernando", COUNTY_NAME_O)|
                                                          grepl("Hernando", COUNTY_NAME_D)|
                                                          grepl("Citrus", COUNTY_NAME_O)|
                                                          grepl("Citrus", COUNTY_NAME_D),
                                              .(`RESIDENT TRIPS` = round(sum(Residents), 0),
                                                `VISITOR TRIPS` = round(sum(Visitors), 0)),
                                              .(FROM = HERNANDO_CITRUS_LBL_2_O,
                                                TO = HERNANDO_CITRUS_LBL_2_D)]
res_vis_hernando_citrus_trip_dt[,":="(FROM = ifelse(is.na(FROM), "External", FROM),
                              TO   = ifelse(is.na(TO), "External", TO))]


### Executive Summary ############################################################
##################################################################################
# Trip OD
es_trip_overall_od = merge(res_vis_overall_trip_dt[,.(FROM, TO, TRIPS = `RESIDENT TRIPS`)],
                           trip_od_survey,
                           by = c("FROM", "TO"),
                           all = TRUE,
                           suffixes = c("_PASSIVE", "_SURVEY"))
es_trip_overall_od = es_trip_overall_od[!(FROM == "External" & TO == "External")]
setnames(es_trip_overall_od, names(es_trip_overall_od), 
         gsub("TRIPS_","",names(es_trip_overall_od)))
es_trip_overall_od[is.na(PASSIVE), PASSIVE:=0]
es_trip_overall_od[is.na(SURVEY), SURVEY:=0]

### Write output data ############################################################
##################################################################################

## Passive Data
# Trip OD
fwrite(daily_overall_trip_dt,           file = pd_trip_daily_od_overall_file)
fwrite(daily_hillsborough_trip_dt,      file = pd_trip_daily_od_hillsborough_file)
fwrite(daily_pinellas_trip_dt,          file = pd_trip_daily_od_pinellas_file)
fwrite(daily_pasco_trip_dt,             file = pd_trip_daily_od_pasco_file)
fwrite(daily_hernando_citrus_trip_dt,   file = pd_trip_daily_od_hernando_citrus_file)

fwrite(res_vis_overall_trip_dt,         file = pd_trip_res_vis_od_overall_file)
fwrite(res_vis_hillsborough_trip_dt,    file = pd_trip_res_vis_od_hillsborough_file)
fwrite(res_vis_pinellas_trip_dt,        file = pd_trip_res_vis_od_pinellas_file)
fwrite(res_vis_pasco_trip_dt,           file = pd_trip_res_vis_od_pasco_file)
fwrite(res_vis_hernando_citrus_trip_dt, file = pd_trip_res_vis_od_hernando_citrus_file)

## Executive Summary
# Trip OD
fwrite(es_trip_overall_od,              file = es_trip_daily_od_overall_file)



