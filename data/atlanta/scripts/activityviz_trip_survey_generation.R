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
library(stringr)


### Define Constants #############################################################
##################################################################################

# Input files
data_dir   = file.path(getwd(), "data")

person_file      = file.path(data_dir, "abm_per_arcga.csv")
trip_file        = file.path(data_dir, "abm_trips_arcga.csv")
per_trip_dd_file = file.path(data_dir, "abm_dd_arcga.csv")
od_file          = file.path(data_dir, "od_20200228_arcga.csv")
od_dd_file       = file.path(data_dir, "od_data_dictionary.csv")

# Geography input files
# taz_file       = file.path(getwd(), "tampa2020.json")
# county_file    = file.path(getwd(), "counties.json")
# agg_file       = file.path(getwd(), "tampa.csv")

# Output files
ts_output_dir = file.path(getwd(), "TourSummary")



### Load required datasets #######################################################
##################################################################################

if(!file.exists("../atlanta/abmod.RData")){
  person_dt    = fread(person_file)
  trip_dt      = fread(trip_file)
  abmdd_dt     = fread(per_trip_dd_file)
  od_dt        = fread(od_file)
  od_dd_dt     = fread(od_dd_file)
  save(person_dt, trip_dt, abmdd_dt, od_dt, od_dd_dt,
       file = "abmod.RData")
} else {
  load("../atlanta/abmod.RData")
}

# taz_ls = fromJSON(taz_file)
# county_ls = fromJSON(county_file)
# taz_sf = geojson_sf(taz_file)
# taz_dt = data.table(taz_sf)
# agg_dt = fread(agg_file)
# agg_dt[,HILLSBOROUGH_LBL_3:=gsub("\\&|\\/","",HILLSBOROUGH_LBL_3)]
# agg_dt[,HILLSBOROUGH_LBL_3:=gsub("\\s+","_",HILLSBOROUGH_LBL_3)]
# agg_dt[,PINELLAS_LBL:=gsub("\\&|\\/","",PINELLAS_LBL)]
# agg_dt[,PINELLAS_LBL:=gsub("\\s+","_",PINELLAS_LBL)]
# agg_dt[,PASCO_LBL:=gsub("\\&|\\/","",PASCO_LBL)]
# agg_dt[,PASCO_LBL:=gsub("\\s+","_",PASCO_LBL)]
# agg_dt[,HERNANDO_CITRUS_LBL_2:=gsub("\\&|\\/","",HERNANDO_CITRUS_LBL_2)]
# agg_dt[,HERNANDO_CITRUS_LBL_2:=gsub("\\s+","_",HERNANDO_CITRUS_LBL_2)]
# agg_sf = st_as_sf(merge(as.data.frame(agg_dt), taz_sf, by.x="TAZ", by.y="id",all.x = TRUE))
# 


### Create output data ###########################################################
##################################################################################

# Create districts.json file for OD chord chart
# # Overall
# overall_sf = dplyr::summarise(dplyr::group_by(agg_sf, D7_ALL_LBL))
# names(overall_sf) = c("NAME", "geometry")
# 
# # Hillsborough
# hillsborough_sf = dplyr::summarise(dplyr::group_by(agg_sf, HILLSBOROUGH_LBL_3))
# names(hillsborough_sf) = c("NAME", "geometry")
# 
# # Pinellas
# pinellas_sf = dplyr::summarise(dplyr::group_by(agg_sf, PINELLAS_LBL))
# names(pinellas_sf) = c("NAME", "geometry")
# 
# # Pasco
# pasco_sf = dplyr::summarise(dplyr::group_by(agg_sf, PASCO_LBL))
# names(pasco_sf) = c("NAME", "geometry")
# 
# # Hernando/Citrus
# hernando_citrus_sf = dplyr::summarise(dplyr::group_by(agg_sf, HERNANDO_CITRUS_LBL_2))
# names(hernando_citrus_sf) = c("NAME", "geometry")


### Onboard Survey ###############################################################
##################################################################################

output_ls = list()

# Filter trips that start and end at home
trip_dt[,start_end_home:=any(base_at_home==1) & any(islast_return_home==1),.(per_id)]
trip_dt[is.na(start_end_home), start_end_home:=FALSE]
# trip_hh_dt = trip_dt[start_end_home==TRUE]
# Activity Recoding
activity_codes = abmdd_dt[item == "activity_type", .(code, value)]
activity_codes[,activity_location:=as.character(NA)]
## Identify Home
activity_codes[grepl("Home", value, ignore.case = TRUE), activity_location:="HOME"]
## Identify Work
activity_codes[grepl("Work", value, ignore.case = TRUE) & is.na(activity_location), 
               activity_location:="WORK"]
## Identify School
activity_codes[grepl("School | Class", value, ignore.case = TRUE) & is.na(activity_location), 
               activity_location:="SCHOOL"]
## Identify Change Modes
activity_codes[grepl("Mode | Transfer", value, ignore.case = TRUE) & is.na(activity_location), 
               activity_location:="MODE_CHANGE"]
## Identify Other
activity_codes[is.na(activity_location), 
               activity_location:="OTHER"]

# Create a tour file
tour_dt = trip_dt[,.N,.(per_id, tour_id = gsub("_\\d{2}$", "", sort_id),
                           travel_date, home_based=start_end_home)][,N:=NULL][]
# Code anchor code
trip_dt[trip_number==0, activity_type:=ifelse(is.na(activity_type), base_at_home, activity_type)]
setkey(activity_codes, code)
trip_dt[,activity_location:=activity_codes[.(activity_type), activity_location]]
trip_dt[start_end_home==FALSE,start_end_home:=activity_location[.N]=="HOME" & activity_location[1]=="HOME",.(per_id)]
anchor_code_dt = trip_dt[,.(activity_code = paste0(c(substr(activity_location,1,1)),
                                                      collapse = "")),.(per_id, home_based=start_end_home)]
anchor_code_dt[home_based==TRUE & !grepl("H$", activity_code), activity_code:=paste0(activity_code,"H")]
anchor_code_dt[home_based==TRUE & !grepl("^H", activity_code), activity_code:=paste0("H", activity_code)]
setkey(anchor_code_dt, per_id)
setkey(tour_dt, per_id)
tour_dt[anchor_code_dt,activity_code:=i.activity_code]
tour_dt[anchor_code_dt,home_based:=i.home_based]

# Code mode
mode_codes_dt = abmdd_dt[item=="travel_mode", .(code, value)]
mode_codes_dt[,mode_code:=as.character(NA)]
mode_codes_dt[code==1, mode_code:="DRIVE"]
mode_codes_dt[code==4, mode_code:="TRANSIT"]
mode_codes_dt[code==9, mode_code:="WALK"]
mode_codes_dt[is.na(mode_code), mode_code:="OTHER"]

setkey(mode_codes_dt, code)
trip_dt[,mode_code:=mode_codes_dt[.(travel_mode), mode_code]]
trip_dt[,new_mode_code:=shift(mode_code,fill = mode_code[2], type = "lead"),.(per_id)]

tour_mode_code_dt = trip_dt[,.(mode_code=paste0(c(substr(new_mode_code,1,1)),
                                                   collapse = "")),.(per_id)]
setkey(tour_mode_code_dt, per_id)

tour_dt[tour_mode_code_dt,mode_code:=i.mode_code]
tour_dt[nchar(activity_code)==nchar(mode_code), mode_code:=gsub(".$","",mode_code)]
tour_dt[(nchar(activity_code)-1)!=nchar(mode_code)]
tour_dt[activity_code=="H" & mode_code=="NA", mode_code:=""]
tour_dt[mode_code=="NA", mode_code:=""]

# Count tour stops
tour_dt[,nstops:=as.integer(str_count(activity_code,"O"))]

# Segment Tours
# 1. Drive Transit
tour_dt[,drive_transit:=str_detect(mode_code,"(DT)|(TD)")]
# 2. Just Walk or Just Transit
tour_dt[,walk_transit_only:=FALSE]
tour_dt[drive_transit==FALSE & mode_code!="", walk_transit_only:=str_detect(mode_code,"D|O", negate = TRUE)]
# 3. Not-Transit
tour_dt[,non_transit:=str_detect(mode_code, "T", negate = TRUE)]
# 4. Has Work
tour_dt[,has_work:=str_detect(activity_code, "W", negate = FALSE)]
# 5. Has School
tour_dt[,has_school:=str_detect(activity_code, "S", negate = FALSE)]
# 6. Has Work and School
tour_dt[,has_work_school:=has_work & has_school]
# 7. Has Neither
tour_dt[,has_neither:=!(has_work | has_school)]
# 8. Overall
tour_dt[,overall:=TRUE]

# Tour Time
trip_dt[,departure_time:=as.POSIXct(dept_time, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")]
trip_dt[is.na(departure_time),departure_time:=as.POSIXct(dept_time, format="%Y-%m-%dT%H:%M:%S.:00Z", tz="UTC")]
trip_dt[,arrival_time2:=as.POSIXct(arrival_time, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")]
trip_dt[is.na(arrival_time2),arrival_time2:=as.POSIXct(arrival_time, format="%Y-%m-%dT%H:%M:%S.:00Z", tz="UTC")]
tour_time_dt = trip_dt[,.(otime = min(departure_time, na.rm = TRUE),
                             dtime = max(departure_time, na.rm = TRUE)),
                          .(per_id)]
setkey(tour_time_dt, per_id)
tour_dt[tour_time_dt, ":="(otime=i.otime,
                           dtime=i.dtime)]
tour_dt[,home_all_day:=FALSE]
tour_dt[activity_code=="H", home_all_day:=TRUE]
tour_dt[,one_place_all_day:=FALSE]
tour_dt[nchar(activity_code)==1, one_place_all_day:=TRUE]
tour_dt[,tourduration:=difftime(dtime,otime,units = "hours")]

# Count number of tours
ntours_dt = trip_dt[,.(ntours=sum(activity_location=="HOME")-1L*all(is.na(islast_return_home))),
                    .(per_id, 
                      start_end_home)]
ntours_dt[start_end_home == FALSE, ntours:=0L]
# ntours_dt[,.N,.(start_end_home, ntours)][order(start_end_home, ntours)]
setkey(ntours_dt, per_id)
tour_dt[ntours_dt, ntours:=i.ntours]
nstops_labels = c("0No Stops", "1 Stop", "2 Stops", "3 Stops", "4+ Stops")
tour_dt[ntours==0, nstops:=0L]
tour_dt[,nstops_label:=nstops_labels[pmin(nstops,4)+1L]]
ntour_labels = c("0No Tours", "1 Tour", "2 Tours", "3 Tours", "4+ Tours")
tour_dt[ntours>3, ntours:=4L]
tour_dt[,ntours:=ntour_labels[ntours+1L]]

# Household Vehicles
# person_dt[,.N,.(count_vh_hh)]
setkey(person_dt, per_id)
hh_veh_labels = c("0 VEH HH", "1+ VEH HH")
tour_dt[person_dt,HH_VEH:=hh_veh_labels[pmin(i.count_vh_hh,1)+1L]]

# Can Use Household Vehicles
# person_dt[,.N,.(count_vh_hh)]
setkey(person_dt, per_id)
can_use_hh_veh_labels = c("1"="YES",
                          "2"="NO",
                          "98"="DON'T KNOW",
                          "99"="REFUSE",
                          "NA"="RESPONSE UNAVAILABLE")
tour_dt[person_dt,CAN_USE_HHVEH:=can_use_hh_veh_labels[as.character(i.can_use_hhveh)]]
tour_dt[HH_VEH=="0 VEH HH", CAN_USE_HHVEH:="ZERO VEH HH"]
tour_dt[is.na(CAN_USE_HHVEH), CAN_USE_HHVEH:="RESPONSE UNAVAILABLE"]

# Household Size
hh_size_labels = c("1 PERSON",
                   "2 PERSONS",
                   "3 PERSONS",
                   "4 PERSONS",
                   "5 PERSONS",
                   "6+ PERSONS")
tour_dt[person_dt,HH_SIZE:=hh_size_labels[pmin(i.hh_size,6)]]

# Gender
gender_labels = c("MALE", "FEMALE", "OTHER")
tour_dt[person_dt,GENDER:=gender_labels[i.gender]]

# Age
age_labels = c("UNDER 6", "6-15", "16-17", "18-24", "25-34", "35-44", "45-54",
               "55-64", "65-75", "76 AND OLDER")
tour_dt[person_dt,AGE:=age_labels[i.age]]

# Income
# writeClipboard(paste0("c(",
#        paste0(
#          paste0("\"", abmdd_dt[item=="income",code], "\"", " = ", "\"", abmdd_dt[item=="income", value]), "\"",
#          collapse = ",\n"), 
#        ")"))
income_labels = c("1" = "Below $5,000",
                  "2" = "$5,000 - $9,999",
                  "3" = "$10,000 - $19,999",
                  "4" = "$20,000 - $29,999",
                  "5" = "$30,000 - $39,999",
                  "6" = "$40,000 - $49,999",
                  "7" = "$50,000 - $59,999",
                  "8" = "$60,000 - $74,999",
                  "9" = "$75,000 - $99,999",
                  "10" = "$100,000 - $119,999",
                  "11" = "$120,000 - $149,999",
                  "12" = "More than $150,000",
                  "98" = "No answer")

tour_dt[person_dt,INCOME:=income_labels[as.character(i.income)]]

output_ls[["tour_dt"]] = tour_dt

### Create Summaries #############################################################
##################################################################################

summary_ls = list()

# Home based Tours
home_based_dt = tour_dt[,.(COUNT=.N,CHART="SURVEYED BY NUMBER OF TOURS"),.(NTOURS = ntours)]
home_based_dt[,GROUP:= "PERSONS"]
setorder(home_based_dt, NTOURS)
setcolorder(home_based_dt, "GROUP")
summary_ls[["home_based_dt"]] = home_based_dt

# Number of Stops
stops_dt = tour_dt[,.(COUNT=.N, CHART="NUMBER OF STOPS"),.(NTOURS = ntours, NSTOPS=nstops_label)]
setorder(stops_dt, NTOURS, NSTOPS)
# stops_dt = stops_dt[order(match(NTOURS, ntour_labels), STOPS)]
summary_ls[["stops_dt"]] = stops_dt

# Drive Transit only
drive_transit_dt = tour_dt[,.(COUNT=.N, CHART="DRIVE TRANSIT ONLY"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `DRIVE TRANSIT`=as.character(drive_transit))]
setorder(drive_transit_dt, `TOURS AND STOPS`, `DRIVE TRANSIT`)
# drive_transit_dt = drive_transit_dt[order(match(NTOURS, ntour_labels), `DRIVE TRANSIT`)]
summary_ls[["drive_transit_dt"]] = drive_transit_dt

# Walk Transit only
walk_transit_dt = tour_dt[,.(COUNT=.N, CHART="WALK TRANSIT ONLY"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `WALK TRANSIT`=as.character(walk_transit_only))]
setorder(walk_transit_dt, `TOURS AND STOPS`, `WALK TRANSIT`)
summary_ls[["walk_transit_dt"]] = walk_transit_dt

# Non Transit only
non_transit_dt = tour_dt[,.(COUNT=.N, CHART="NON TRANSIT"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `NON TRANSIT`=as.character(non_transit))]
setorder(non_transit_dt, `TOURS AND STOPS`, `NON TRANSIT`)
summary_ls[["non_transit_dt"]] = non_transit_dt

# Has Work
has_work_dt = tour_dt[,.(COUNT=.N, CHART="HAS WORK"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `HAS WORK`=as.character(has_work))]
setorder(has_work_dt, `TOURS AND STOPS`, `HAS WORK`)
summary_ls[["has_work_dt"]] = has_work_dt

# Has School
has_school_dt = tour_dt[,.(COUNT=.N, CHART="HAS SCHOOL"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `HAS SCHOOL`=as.character(has_school))]
setorder(has_school_dt, `TOURS AND STOPS`, `HAS SCHOOL`)
summary_ls[["has_school_dt"]] = has_school_dt

# Has Work and school
has_work_school_dt = tour_dt[,.(COUNT=.N, CHART="HAS WORK AND SCHOOL"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `HAS WORK SCHOOL`=as.character(has_work_school))]
setorder(has_work_school_dt, `TOURS AND STOPS`, `HAS WORK SCHOOL`)
summary_ls[["has_work_school_dt"]] = has_work_school_dt

# Has Neither
has_neither_dt = tour_dt[,.(COUNT=.N, CHART="HAS NEITHER"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `HAS NEITHER`=as.character(has_neither))]
setorder(has_neither_dt, `TOURS AND STOPS`, `HAS NEITHER`)
summary_ls[["has_neither_dt"]] = has_neither_dt

# OVERALL
overall_dt = tour_dt[,.(COUNT=.N, CHART="OVERALL"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `OVERALL`=as.character(overall))]
overall_dt[,OVERALL:="PARTICIPANTS"]
setorder(overall_dt, `TOURS AND STOPS`, `OVERALL`)
summary_ls[["overall_dt"]] = overall_dt

# Add Demographic data
# Tours by household cars
hh_veh_dt = tour_dt[,.(COUNT=.N, CHART="HOUSEHOLD VEHICLES"),
                    .(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), 
                      `HOUSEHOLD VEH`=HH_VEH)]
setorder(hh_veh_dt, `TOURS AND STOPS`, `HOUSEHOLD VEH`)
summary_ls[["hh_veh_dt"]] = hh_veh_dt

# Tours by availability of household vehicle
can_use_hhveh_dt = tour_dt[,.(COUNT=.N, CHART="HOUSEHOLD VEHICLES AVAILABILITY"),
                           .(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), 
                             `HOUSEHOLD VEH AVAILABLE`=CAN_USE_HHVEH)]
setorder(can_use_hhveh_dt, `TOURS AND STOPS`, `HOUSEHOLD VEH AVAILABLE`)
can_use_hhveh_dt = can_use_hhveh_dt[order(`TOURS AND STOPS`, 
                                          match(`HOUSEHOLD VEH AVAILABLE`, can_use_hh_veh_labels))]
summary_ls[["can_use_hhveh_dt"]] = can_use_hhveh_dt

# Tour by household size
hh_size_dt = tour_dt[, .(COUNT = .N, CHART = "HOUSEHOLD SIZE"),
                     .(`TOURS AND STOPS` = paste0(ntours, ", ", nstops_label),
                       `HOUSEHOLD SIZE` = HH_SIZE)]
setorder(hh_size_dt, `TOURS AND STOPS`, `HOUSEHOLD SIZE`)
summary_ls[["hh_size_dt"]] = hh_size_dt

# Tour by gender
gender_dt = tour_dt[, .(COUNT = .N, CHART = "GENDER"),
                    .(`TOURS AND STOPS` = paste0(ntours, ", ", nstops_label),
                      GENDER)]
# setorder(gender_dt, `TOURS AND STOPS`, GENDER)
gender_dt = gender_dt[order(`TOURS AND STOPS`,
                            match(GENDER, gender_labels))]
summary_ls[["gender_dt"]] = gender_dt

# tour by age
age_dt = tour_dt[, .(COUNT = .N, CHART = "AGE"),
                 .(`TOURS AND STOPS` = paste0(ntours, ", ", nstops_label),
                   AGE)]
# setorder(age_dt, `TOURS AND STOPS`, AGE)
age_dt = age_dt[order(`TOURS AND STOPS`,
                      match(AGE, age_labels))]
summary_ls[["age_dt"]] = age_dt

# Tour by household income
income_dt = tour_dt[, .(COUNT = .N, CHART = "INCOME"),
                 .(`TOURS AND STOPS` = paste0(ntours, ", ", nstops_label),
                   INCOME)]
income_dt = income_dt[order(`TOURS AND STOPS`,
                            match(INCOME, income_labels))]
summary_ls[["income_dt"]] = income_dt

# home_based_labels = c("NOT HOME BASED", "HOME BASED")
lapply(summary_ls, function(temp_dt){
  if("NTOURS" %in% names(temp_dt)){
    temp_dt[,NTOURS:=gsub("^0", "", NTOURS)]
    cat("Tours Updated", "\n")
  }
  if("TOURS AND STOPS" %in% names(temp_dt)){
    temp_dt[,`TOURS AND STOPS`:=gsub("0", "", `TOURS AND STOPS`)]
    cat("Stops Updated", "\n")
  }
  if("NSTOPS" %in% names(temp_dt)){
    temp_dt[,NSTOPS:=gsub("0", "", NSTOPS)]
    cat("Stops Updated", "\n")
  }
  invisible(TRUE)
})

lapply(output_ls, function(temp_dt){
  if("NTOURS" %in% names(temp_dt)){
    temp_dt[,NTOURS:=gsub("^0", "", NTOURS)]
    cat("Tours Updated", "\n")
  }
  if("NSTOPS" %in% names(temp_dt)){
    temp_dt[,NSTOPS:=gsub("^0", "", NSTOPS)]
    cat("Stops Updated", "\n")
  }
  invisible(TRUE)
})


### Write output data ############################################################
##################################################################################

# Write Tour file to data folder
fwrite(output_ls$tour_dt, file = "data/tour.csv")

# Write the summaries
lapply(names(summary_ls), function(x){
  fwrite(summary_ls[[x]], file = file.path(ts_output_dir, gsub("_dt",".csv",x)))
  invisible(TRUE)
})


