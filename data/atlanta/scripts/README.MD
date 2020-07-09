# Example ARC Scripts Folder

## Modeling Scenarios

These scripts convert the ARC model outputs to the input formats expected by the tool.  These are not used in anyway by the system.  
1. convertWKTtoGEOJSON.R - R script to convert the old ABMVIZ wellknowntext format TAZ polygons to GeoJSON.  [Mapshaper](http://www.mapshaper.org) is a better alternative.
4. BarChartAndMapExample.sql - SQL script to query the ABMVIZ DB to produce the bar chart and map visual demo data set
5. TimeUseExample.sql - SQL script to query the ABMVIZ DB to produce the time use visual demo data set
6. 3DAnimatedMapExample.sql - SQL script to query the ABMVIZ DB to produce the 3D animated map demo data set
7. TreeMapExample.sql - SQL script to query the ABMVIZ DB to produce the tree map visual demo data set
8. RadarChartsExample.sql - SQL script to query the ABMVIZ DB to produce the radar charts visual demo data set
9. BarChartExample.sql - SQL script to query the ABMVIZ DB to produce the bar chart visual demo data set

To create the ARC demo data from SQL Server, do the following:
1. Open SQL server management studio
2. Open one of the example SQL query scripts such as BarChartAndMapExample.sql
3. Set the user - [ATLANTAREGION\TAMConsult] - and schema (i.e. scenario) - BS10 - to query
4. Execute the script
5. Right click in the upper left corner of the results table and select Copy with Headers
6. Paste the result in Excel and save as a CSV file into the relevant data\scenario folder

## Survey Scenarios

The following scripts process ARC survey data and produce outputs shown in the survey scenarios of the ARC visualizer

1. `onboard_survey_data.R` - R script that processes *Onboard Survey Data* and develops outputs to the input formats expected by the tool. The script carries out following operations:
   - Load the packages required to process data.
   - Read survey data, data dictionary and geospatial files.
   - Create dataset to be used by the visualizer by processing survey data.
   - Write the dataset in the `OnboardSurvey` folder.
2. `one_day_tour_data.R` - R script that processes *One-Day Tour Survey Data* and develops outputs to the inputs formats expected by the tool. The script carries out following operations:
   - Load the packages required to process data.
   - Read survey data, data dictionary and geospatial files.
   - Transform trip data to tour data by recoding activity location to identify home and then identifying trips that start and end at home. 
   - Append additional variables to tour data to simplify creation of dataset for visualizer.
   - Create dataset to be used by the visualizer by processing tour data.
   - Write the dataset in the `TourSummary` folder.