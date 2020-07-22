##################################################################################
# Create data for ARC Visulization using ActivityViz
# Author: Aditya Gore
##################################################################################

#### Sections
# 1. Load/Install required packages
# 2. Define Constants
# 3. Load required databases
# 4. Create output data
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
od_file          = file.path(data_dir, "od_20200228_ARCGA_DraftSubmitted_EXPANDED_20200615.csv")
od_dd_file       = file.path(data_dir, "od_data_dictionary.csv")
superdistricts_file = file.path(getwd(), "superdistricts.json")
zone_file = file.path(getwd(), "ZoneShape.GeoJSON")
county_file = file.path(getwd(), "cb_2015_us_county_500k_GEORGIA.json")

# Output files
ts_output_dir = file.path(getwd(), "TourSummary")



### Load required datasets #######################################################
##################################################################################

# if(!file.exists("../atlanta/abmod.RData")){
  person_dt    = fread(person_file)
  trip_dt      = fread(trip_file)
  abmdd_dt     = fread(per_trip_dd_file)
  od_dt        = fread(od_file)
  od_dd_dt     = fread(od_dd_file)
  # save(person_dt, trip_dt, abmdd_dt, od_dt, od_dd_dt,
  #      file = "abmod.RData")
# } else {
#   load("../atlanta/abmod.RData")
# }

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


### One Day Tour Survey ##########################################################
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

# Code origin and destination TAZ, County
trip_dt[, destination_taz:=location_mtaz20]
trip_dt[, origin_taz:=shift(location_mtaz20),.(per_id)]

trip_dt[, destination_county:=location_county]
trip_dt[, origin_county:=shift(location_county),.(per_id)]

# Trip OD Bar Chart
trip_origin_bar_dt = trip_dt[trip_number > 0 & start_end_home==TRUE, .(ODTYPE = "ORIGIN",
                                                                       COUNT=.N), .(ZONE = origin_taz)]
trip_destination_bar_dt = trip_dt[trip_number > 0 & start_end_home==TRUE, .(ODTYPE = "DESTINATION",
                                                                       COUNT=.N), .(ZONE = destination_taz)]
trip_od_bar_dt = rbindlist(list(trip_origin_bar_dt, trip_destination_bar_dt),
                           use.names = TRUE)
trip_od_bar_dt[,ZONE:=as.integer(ZONE)]
trip_od_bar_dt = trip_od_bar_dt[!is.na(ZONE)]
trip_od_bar_dt[zone_sd_dt,DISTRICT:=i.NAME,on=.(ZONE=id_TAZ)]
trip_od_bar_dt = trip_od_bar_dt[!is.na(DISTRICT)]
setcolorder(trip_od_bar_dt, c("ZONE", "DISTRICT", "ODTYPE", "COUNT"))
trip_od_bar_dt = trip_od_bar_dt[, .(QUANTITY=sum(COUNT), CHART_TYPE="TRIP ORIGIN OR DESTINATION"),
                                .(DISTRICT, ODTYPE)]
setorder(trip_od_bar_dt, DISTRICT, -ODTYPE)
output_ls[["trip_od_bar_dt"]] = trip_od_bar_dt

# Trip flows for chord diagram
# final_agency should be Marta
trip_flow_dt = trip_dt[grepl("MARTA", final_agency, ignore.case = TRUE) & start_end_home==TRUE & 
                         trip_number > 0,
                     .(Trips=.N),
                     by = .(FROM = as.integer(origin_taz), TO = as.integer(destination_taz))]
trip_flow_dt = trip_flow_dt[!(is.na(FROM)|is.na(TO))]
trip_flow_dt = merge(trip_flow_dt,zone_sd_dt[,.(FROM=id_TAZ, NAME)], by="FROM")
trip_flow_dt[,FROM:=NAME]
trip_flow_dt[,c("NAME"):=NULL]
trip_flow_dt = merge(trip_flow_dt,zone_sd_dt[,.(TO=id_TAZ, NAME)], by="TO")
trip_flow_dt[,TO:=NAME]
trip_flow_dt[,c("NAME"):=NULL]
trip_flow_dt = trip_flow_dt[,.(Trips=sum(Trips)),.(FROM,TO)]
setkey(trip_flow_dt, FROM, TO)
trip_flow_dt = trip_flow_dt[CJ(FROM, TO, unique = TRUE)]
trip_flow_dt[is.na(Trips), Trips:=0]
output_ls[["trip_flow_dt"]] = trip_flow_dt

# Trip flows for chord diagram by County
trip_flow_county_dt = trip_dt[grepl("MARTA", final_agency, ignore.case = TRUE) & start_end_home==TRUE & 
                                trip_number > 0,
                            .(Trips=.N),
                            by = .(FROM = origin_county, TO = destination_county)]
trip_flow_county_dt = trip_flow_county_dt[FROM %in% county_sf$NAME & TO %in% county_sf$NAME]
setkey(trip_flow_county_dt, FROM, TO)
sel_county = sort(unique(c(trip_flow_county_dt$FROM, trip_flow_county_dt$TO)))
trip_flow_county_dt = trip_flow_county_dt[CJ(FROM=sel_county, TO=sel_county, unique = TRUE)]
setkey(trip_flow_county_dt, FROM, TO)
# trip_flow_county_dt = trip_flow_county_dt[CJ(FROM,TO, unique = TRUE)]
trip_flow_county_dt[is.na(Trips), Trips:=0]
trip_flow_county_dt[,FROM:=gsub(" County", "", FROM)]
trip_flow_county_dt[,TO:=gsub(" County", "", TO)]
output_ls[["trip_flow_county_dt"]] = trip_flow_county_dt

# # Create zone filter files
# sd_filter_dt = zone_sd_dt[,.(.N, value=1),.(id=id_SD,NAME,NAME2=NAME)]
# sd_dt = data.table(superdistricts_sf)[NAME %in% trip_flow_dt$FROM,.(id,NAME)]
# sd_dt[,NAME2:=NAME]
# sd_dt[,value:=0]
# sd_dt[sd_filter_dt, value:=i.value,on=.(NAME, NAME2)]
# superdistricts_dt = dcast(sd_dt, id+NAME~NAME2, value.var = "value", fill = 0)
# output_ls[["superdistricts_dt"]] = superdistricts_dt
# 
# # Create desirelines
# desirelines_sf = superdistricts_sf[superdistricts_sf$id %in% zone_sd_dt$id_SD, ]
# desirelines_sf = st_centroid(desirelines_sf)
# linecomb = expand.grid(id=seq_len(nrow(desirelines_sf)),id2=seq_len(nrow(desirelines_sf)))
# linecomb = linecomb[linecomb$id!=linecomb$id2,]
# all_lines_sf_sf = suppressWarnings(suppressMessages(t(mapply(function(id1, id2) {st_cast(
#   st_union(desirelines_sf[id1,], desirelines_sf[id2,]),
#   "LINESTRING")}, linecomb$id, linecomb$id2))))
# all_lines_sf_df = data.frame(all_lines_sf_sf)
# all_lines_sf_df$geometry = unlist(all_lines_sf_df$geometry, recursive = FALSE)
# all_lines_sf_df = st_as_sf(all_lines_sf_df)
# all_lines_sf_df$o = as.integer(unlist(all_lines_sf_df$id))
# all_lines_sf_df$d = as.integer(unlist(all_lines_sf_df$id.1))
# newdesirelines_sf = all_lines_sf_df[,c("o", "d", "geometry")]
# st_write(newdesirelines_sf, "odts_desirelines.GeoJSON", driver = "GeoJSON", delete_dsn = TRUE)




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
st_write(newdesirelines_sf, "odts_county_desirelines.GeoJSON", driver = "GeoJSON", delete_dsn = TRUE)

county_sf$id = as.integer(paste0(county_sf$STATEFP, county_sf$COUNTYFP))
st_write(county_sf[county_sf$id %in% newdesirelines_sf$o,c("id", "NAME", "geometry")],
         "odts_county.GeoJSON", driver = "GeoJSON", delete_dsn = TRUE)

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
# income_labels = c("1" = "Below $5,000",
#                   "2" = "$5,000 - $9,999",
#                   "3" = "$10,000 - $19,999",
#                   "4" = "$20,000 - $29,999",
#                   "5" = "$30,000 - $39,999",
#                   "6" = "$40,000 - $49,999",
#                   "7" = "$50,000 - $59,999",
#                   "8" = "$60,000 - $74,999",
#                   "9" = "$75,000 - $99,999",
#                   "10" = "$100,000 - $119,999",
#                   "11" = "$120,000 - $149,999",
#                   "12" = "More than $150,000",
#                   "98" = "No answer")
income_labels = c("1" =  "$0 - $19,999",
                  "2" =  "$0 - $19,999",
                  "3" =  "$0 - $19,999",
                  "4" =  "$20,000 - $59,999",
                  "5" =  "$20,000 - $59,999",
                  "6" =  "$20,000 - $59,999",
                  "7" =  "$20,000 - $59,999",
                  "8" =  "$60,000 - $119,999",
                  "9" =  "$60,000 - $119,999",
                  "10" = "$60,000 - $119,999",
                  "11" = "More than $120,000",
                  "12" = "More than $120,000",
                  "98" = "No answer")

tour_dt[person_dt,INCOME:=income_labels[as.character(i.income)]]

output_ls[["tour_dt"]] = tour_dt

### Create Summaries #############################################################
##################################################################################

summary_ls = list()
true_false_labels = c(
  "FALSE" = "No",
  "TRUE"  = "Yes"
)

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
drive_transit_dt[,`DRIVE TRANSIT`:=true_false_labels[`DRIVE TRANSIT`]]
# drive_transit_dt = drive_transit_dt[order(match(NTOURS, ntour_labels), `DRIVE TRANSIT`)]
summary_ls[["drive_transit_dt"]] = drive_transit_dt

# Walk Transit only
walk_transit_dt = tour_dt[,.(COUNT=.N, CHART="WALK TRANSIT ONLY"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `WALK TRANSIT`=as.character(walk_transit_only))]
setorder(walk_transit_dt, `TOURS AND STOPS`, `WALK TRANSIT`)
walk_transit_dt[,`WALK TRANSIT`:=true_false_labels[`WALK TRANSIT`]]
summary_ls[["walk_transit_dt"]] = walk_transit_dt

# Non Transit only
non_transit_dt = tour_dt[,.(COUNT=.N, CHART="NON TRANSIT"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `NON TRANSIT`=as.character(non_transit))]
setorder(non_transit_dt, `TOURS AND STOPS`, `NON TRANSIT`)
non_transit_dt[,`NON TRANSIT`:=true_false_labels[`NON TRANSIT`]]
summary_ls[["non_transit_dt"]] = non_transit_dt

# Has Work
has_work_dt = tour_dt[,.(COUNT=.N, CHART="HAS WORK"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `HAS WORK`=as.character(has_work))]
setorder(has_work_dt, `TOURS AND STOPS`, `HAS WORK`)
has_work_dt[,`HAS WORK`:=true_false_labels[`HAS WORK`]]
summary_ls[["has_work_dt"]] = has_work_dt

# Has School
has_school_dt = tour_dt[,.(COUNT=.N, CHART="HAS SCHOOL"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `HAS SCHOOL`=as.character(has_school))]
setorder(has_school_dt, `TOURS AND STOPS`, `HAS SCHOOL`)
has_school_dt[,`HAS SCHOOL`:=true_false_labels[`HAS SCHOOL`]]
summary_ls[["has_school_dt"]] = has_school_dt

# Has Work and school
has_work_school_dt = tour_dt[,.(COUNT=.N, CHART="HAS WORK AND SCHOOL"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `HAS WORK SCHOOL`=as.character(has_work_school))]
setorder(has_work_school_dt, `TOURS AND STOPS`, `HAS WORK SCHOOL`)
has_work_school_dt[,`HAS WORK SCHOOL`:=true_false_labels[`HAS WORK SCHOOL`]]
summary_ls[["has_work_school_dt"]] = has_work_school_dt

# Has Neither
has_neither_dt = tour_dt[,.(COUNT=.N, CHART="HAS NEITHER"),.(`TOURS AND STOPS` = paste0(ntours, ", ",nstops_label), `HAS NEITHER`=as.character(has_neither))]
setorder(has_neither_dt, `TOURS AND STOPS`, `HAS NEITHER`)
has_neither_dt[,`HAS NEITHER`:=true_false_labels[`HAS NEITHER`]]
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

for(table_name in setdiff(names(output_ls), "tour_dt")){
  filename = file.path(ts_output_dir, paste0(gsub("_dt", "", table_name), ".csv"))
  fwrite(output_ls[[table_name]],
         file = filename)
  cat(basename(filename), "\n")
}

