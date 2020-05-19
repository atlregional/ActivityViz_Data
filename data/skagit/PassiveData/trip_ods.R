
# process skagit data for chords
# Ben Stabler, ben.stabler@rsginc.com, 05/19/20

trips_filename = "C:/projects/skagit/spaghetti_cut/output_csvs/trips.csv"
zone_filename = "C:/projects/skagit/spaghetti_cut/shapefiles/tract_aggregate_zones_wgs84.dbf"
zone_filename_with_names = "C:/projects/skagit/spaghetti_cut/shapefiles/tract_aggregate_zones_wgs84_names.dbf"
trips_od_filename = "C:/projects/skagit/spaghetti_cut/output_csvs/trips_od.csv"
counties_filename = "C:/projects/skagit/spaghetti_cut/output_csvs/counties.csv"

library(foreign)

# read trips
trips = read.csv(trips_filename)
trips_od = table(trips$start_zone_id, trips$stop_zone_id)
trips_od_df = as.data.frame.matrix(trips_od)
trips_od_matrix = as.matrix(trips_od_df)

# read zones
zone_names = read.dbf(zone_filename)
zone_names$name = paste0(zone_names$County, ".", zone_names$Agg_Num)
rownames(trips_od_matrix) = c("External",zone_names$name)
colnames(trips_od_matrix) = c("External",zone_names$name)

# create counties file
trips_od_matrix[] = 0
diag(trips_od_matrix) = 1
counties = data.frame("ID"=c(0, zone_names$seq_id), "COUNTY"=c("External", zone_names$name), trips_od_matrix)

#create trips od table
trips_od_from_to = as.data.frame(trips_od)
colnames(trips_od_from_to) = c("FROM","TO","FLOWS")
trips_od_from_to$FROM = zone_names$name[match(trips_od_from_to$FROM, zone_names$seq_id)]
trips_od_from_to$FROM[is.na(trips_od_from_to$FROM)] = "External"
trips_od_from_to$TO = zone_names$name[match(trips_od_from_to$TO, zone_names$seq_id)]
trips_od_from_to$TO[is.na(trips_od_from_to$TO)] = "External"

#write outputs
write.dbf(zone_names, zone_filename_with_names) # includes names so when mapshaper creates geojson it has a names attribute
write.csv(trips_od_from_to, trips_od_filename, row.names=F, quote=F)
write.csv(counties, counties_filename, row.names=F, quote=F)

#manually set external to external to 0 for now