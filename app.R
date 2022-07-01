library(data.table)
library(tidyverse)
library(readxl)
library(geojsonsf)
library(sf)
library(s2)
s2_available <- !inherits(try(sf_use_s2(TRUE), silent = TRUE), "try-error")
library(tmaptools)
library(classInt)
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)
library(textclean)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leafem)
library(fontawesome)
library(shinyjs)

# Import Census data using tigris instead?

# Import map data and shape files

s1701_poverty_tracts <-
  fread(
    "ACSST5Y2019.S1701_data_with_overlays_2021-03-11T131230.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!Population for whom poverty status is determined",
      "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level"
    )
  )

eligible_sites <- fread("eligible_sites.csv") #update file

sites <- read_excel("Site_Export.xlsx", sheet = "Site Data") #update file

pa <- read_excel("Program_Activities_Export.xlsx", sheet = "Program Activity Data") #update file

ia <- read_excel("Indirect_Activity_Export.xlsx", sheet = "Indirect Activity Data") #update file
ia_ic <- read_excel("Indirect_Activity_Export.xlsx", sheet = "Intervention Channels")

pse <- read_excel("PSE_Site_Activity_Export.xlsx", sheet = "PSE Data") #update file

coa <- read_excel("Coalition_Export.xlsx", sheet = "Coalition Data") #update file
coa_members <- read_excel("Coalition_Export.xlsx", sheet = "Members")

part <- read_excel("Partnership_Export.xlsx", sheet = "Partnership Data") #update file

il_tracts_sf <- st_read("./cb_2019_17_tract_500k")

# Import community profile data

s1701_poverty_counties <-
  fread(
    "ACSST5Y2019.S1701_data_with_overlays_2021-11-08T111346.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!Population for whom poverty status is determined",
      "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over",
      "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male",
      "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)"
    )
  )

s1701_poverty_places <-
  fread(
    "ACSST5Y2019.S1701_data_with_overlays_2021-04-28T154515.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!Population for whom poverty status is determined",
      "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
      "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over",
      "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male",
      "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
      "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
      "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)"
    )
  )

s2201_snap_counties <-
  fread(
    "ACSST5Y2019.S2201_data_with_overlays_2021-12-16T152331.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Households receiving food stamps/SNAP!!Households",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!White alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Black or African American alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!American Indian and Alaska Native alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Asian alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Native Hawaiian and Other Pacific Islander alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Some other race alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Two or more races",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Hispanic or Latino origin (of any race)"
    )
  )

s2201_snap_places <-
  fread(
    "ACSST5Y2019.S2201_data_with_overlays_2021-05-04T143832.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Households receiving food stamps/SNAP!!Households",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!White alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Black or African American alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!American Indian and Alaska Native alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Asian alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Native Hawaiian and Other Pacific Islander alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Some other race alone",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Two or more races",
      "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Hispanic or Latino origin (of any race)"
    )
  )

s1602_lep_counties <-
  fread(
    "ACSST5Y2019.S1602_data_with_overlays_2021-12-16T145907.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!All households",
      "Estimate!!Limited English-speaking households!!All households",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Spanish",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other Indo-European languages",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Asian and Pacific Island languages",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other languages"
    )
  )

s1602_lep_places <-
  fread(
    "ACSST5Y2019.S1602_data_with_overlays_2021-05-05T160010.csv",
    skip = 1,
    select = c(
      "id",
      "Geographic Area Name",
      "Estimate!!Total!!All households",
      "Estimate!!Limited English-speaking households!!All households",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Spanish",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other Indo-European languages",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Asian and Pacific Island languages",
      "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other languages"
    )
  )

adult_obesity <-
  fread(
    "2022 County Health Rankings Illinois Data - v1.csv",
    skip = 1,
    select = c("FIPS",
               "State",
               "County",
               "% Adults with Obesity")
  )

food_insecurity <- fread("MMG2021_2019Data_ToShare.csv")

places_counties <- fread("IL Places-Counties.csv")

# Community Networks Map

## SNAP Eligible Individuals/% Tracts Layer

s1701_poverty_tracts <-
  s1701_poverty_tracts %>% rename(
    AFFGEOID = "id",
    total_population = "Estimate!!Total!!Population for whom poverty status is determined",
    individuals_income_below_185_percent_poverty_level = "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level",
  ) %>%
  separate("Geographic Area Name",
           c("census_tract", "county", "state"),
           sep = ", ")

s1701_poverty_tracts$county <- gsub(" County", "", s1701_poverty_tracts$county, fixed = TRUE)

c_map_poverty <- s1701_poverty_tracts[,
                                      c(
                                        "AFFGEOID",
                                        "census_tract",
                                        "county",
                                        "state",
                                        "total_population",
                                        "individuals_income_below_185_percent_poverty_level"
                                      )]

c_map_poverty$census_tract <- gsub("Census Tract ", "", c_map_poverty$census_tract, fixed = TRUE)


c_map_poverty$snap_eligibility_percent <-
  round(
    100 * (
      c_map_poverty$individuals_income_below_185_percent_poverty_level / c_map_poverty$total_population
    )
  )

il_tracts_sf_merged <- merge(il_tracts_sf, c_map_poverty, by = "AFFGEOID") #missing two tracts?

# PEARS sites Datacleaning

program_area_counts <- function(df, fields, module) {
  out_df <-
    df[df$program_area %in% c("SNAP-Ed", "EFNEP") & # program_area column must be present
         !grepl("TEST", df[[fields[2]]]), fields] # second field must be name
  out_df <-
    full_join(out_df[out_df$program_area == "SNAP-Ed", ] %>% count(site_id, name = paste0("snap_ed_", module)),
              out_df[out_df$program_area == "EFNEP", ] %>% count(site_id, name =  paste0("efnep_", module)),
              by = "site_id")
  out_df[[module]] <-
    rowSums(out_df[, c(paste0("snap_ed_", module), paste0("efnep_", module))],  na.rm = TRUE)
  out_df
}

pa2 <- rename(pa, "program_area" = "program_areas")

sites_pa <-
  program_area_counts(pa2,
                      c("program_id", "name", "program_area", "site_id"),
                      "program_activities")

ia_ic2 <-
  left_join(ia, ia_ic[c("activity_id", "channel_id", "site_id")], by = "activity_id") %>%
  filter(!is.na(site_id)) %>%
  distinct(activity_id, title, program_area, site_id)
sites_ia <-
  program_area_counts(ia_ic2,
                      c("activity_id", "title", "program_area", "site_id"),
                      "indirect_activities")

sites_pse <-
  program_area_counts(pse,
                      c("pse_id", "name", "program_area", "site_id"),
                      "pse_site_activities")

coa_members2 <-
  left_join(coa, coa_members[c("coalition_id", "member_id", "site_id")], by = "coalition_id") %>%
  filter(!is.na(site_id)) %>%
  distinct(coalition_id, coalition_name, program_area, site_id)
sites_coa <-
  program_area_counts(coa_members2,
                      c("coalition_id", "coalition_name", "program_area", "site_id"),
                      "coalitions")

sites_part <-
  program_area_counts(
    part,
    c(
      "partnership_id",
      "partnership_name",
      "program_area",
      "site_id"
    ),
    "partnerships"
  )

site_programming <-
  sites[c("site_id",
          "site_name",
          "city__county",
          "latitude",
          "longitude")] %>%
  left_join(sites_pa, by = "site_id") %>%
  left_join(sites_ia, by = "site_id") %>%
  left_join(sites_pse, by = "site_id") %>%
  left_join(sites_coa, by = "site_id") %>%
  left_join(sites_part, by = "site_id") %>%
  filter(
    !is.na(program_activities) |
      !is.na(pse_site_activities) |
      !is.na(coalitions) | !is.na(partnerships)
  )

cols <- colnames(site_programming)

# create function
program_bool <- function(df, program) {
  #program: snake case character value for program variable
  out_df <- df
  cols <- colnames(out_df)
  program_cols <- cols[grepl(paste0("^", program), cols)]
  out_df[program] <-rowSums(out_df[, program_cols],  na.rm = TRUE)
  out_df[program] <- ifelse(out_df[program] > 0, "Yes", "No")
  out_df
}

site_programming <- program_bool(site_programming, "snap_ed")
site_programming <- program_bool(site_programming, "efnep")

site_programming <-
  site_programming[, c(
    "site_id",
    "site_name",
    "city__county",
    "latitude",
    "longitude",
    "snap_ed",
    "efnep",
    "program_activities",
    "indirect_activities",
    "pse_site_activities",
    "coalitions",
    "partnerships"
  )]

# FoodFinder Sites

food_finder_key <- "food_finder_api_key"

food_finder_sites <-
  geojson_sf(
    paste0(
      "https://api-v2-prod-dot-foodfinder-183216.uc.r.appspot.com/partners/providers?key=",
      food_finder_key,
      "&limit=100000&offset=1&format=1&min_lat=36.73359537141243&max_lat=42.81674576662397&min_lon=-91.91983678325361&max_lon=-87.36749173318934"
    )
  )
# separate out query parameters, paste() strings

food_finder_sites <-
  food_finder_sites %>%  extract(
    geometry,
    into = c('latitude', 'longitude'),
    '\\((.*),(.*)\\)',
    conv = T
  )

food_finder_sites <-
  select(as.data.frame(food_finder_sites), -geometry)

food_finder_sites$site_type <-
  mgsub(
    food_finder_sites$filter_id,
    seq(1, 6),
    c(
      "School & Summer Meals",
      "Farmers Markets",
      "Food Pantry/Meal Site",
      "Grocery Stores",
      "Senior Food Resources",
      "SNAP & WIC Offices"
    )
  )

food_finder_sites <-
  food_finder_sites %>% filter(site_type == "Food Pantry/Meal Site") %>% select(c(
    "name",
    "address",
    "city",
    "state",
    "zip",
    "latitude",
    "longitude",
    "site_type"
  ))
  
food_finder_sites <-
  food_finder_sites  %>% rename(
    "site_name" = "name",
    "site_address" = "address",
    "site_city" = "city",
    "site_state" = "state",
    "site_zip" = "zip"
  )

eligible_sites <- rbind(eligible_sites, food_finder_sites)

# Leaflet Map Visuals

pal1 <-
  colorNumeric("Blues", domain = il_tracts_sf_merged$snap_eligibility_percent)
pal2 <-
  colorNumeric("Reds",
               domain = il_tracts_sf_merged$individuals_income_below_185_percent_poverty_level)

popup_sb1 <- paste0("County: \n", as.character(il_tracts_sf_merged$county),
                    "</br/> Census Tract: \n", as.character(il_tracts_sf_merged$census_tract),
                    "</br/> # of Individuals Below 185% FPL: \n", as.character(il_tracts_sf_merged$individuals_income_below_185_percent_poverty_level),
                    "</br/> % of Individuals Below 185% FPL: \n", as.character(il_tracts_sf_merged$snap_eligibility_percent))

# legend html generator:
marker_legend_html <- function(icon_set) {
  # container div:
  legend_html <-
    "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:1px; margin: 0;'> Site Legend </h4>"

  n <- 1
  # add each icon for font-awesome icons icons:
  for (icon in icon_set) {
    if (icon[["library"]] == "fa") {
      legend_html<- paste0(legend_html, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 0px; margin-top: 11px; 'class= 'fa fa-",icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 5px; display: inline-block; ' >", names(icon_set)[n] ,"</p>",
                          "</div>")
    }
    n <- n + 1
  }
  paste0(legend_html, "</div>")
}

# Create icon_set using for loop?

icon_set <- awesomeIconList(
  "INEP Program Site" = makeAwesomeIcon(icon = "circle", markerColor = "green", library = "fa"),
  "FCRC" = makeAwesomeIcon(icon = "users", markerColor = "purple", library = "fa"),
  "FQHC" = makeAwesomeIcon(icon = "medkit", markerColor = "red", library = "fa"),
  "Food Pantry/Meal Site" = makeAwesomeIcon(icon = "apple", markerColor = "beige", library = "fa"),
  "Head Start Center" = makeAwesomeIcon(icon = "child", markerColor = "darkred", library = "fa"),
  "Emergency Shelter" = makeAwesomeIcon(icon = "home", markerColor = "cadetblue", library = "fa"),  
  "Eligible School" = makeAwesomeIcon(text = fa("school"), markerColor = "orange", library = "fa"),  
  "WIC Office" = makeAwesomeIcon(icon = "female", markerColor = "blue", library = "fa")
)

## Community Profile

clean_census_data <-
  function(census_data,
           rename_cols,
           rename_values,
           geography,
           pivot_cols_prefix,
           pivot_col,
           values_to,
           ethnicity = FALSE,
           ethnic_col,
           total_col) {

  # census_data: df of ACS table
  # rename_cols: vector of column names that should be renamed
  # rename_values: list of strings for column name string replacement
  # geography inputs: "counties", "places"
  # pivot_cols_prefix: A regular expression used to select variables to pivot and remove matching text from the start of each variable name
  # pivot_col: A string specifying the name of the column to create from the data stored in the column names selected from pivot_cols_prefix
  # values_to: A string specifying the name of the column to create from the data stored in cell values
  # Ethnicity: logical, whether or not non-hispanic or latino ethnicity needs to be calculated
  # ethnic_col: A string specifying the name of the column for hispanic/latino origin (optional unless ethnicity == TRUE)
  # total_col: string value of the renamed variable for the total value of the pivoted columns (optional unless ethnicity == TRUE)


  # verify dependencies are installed/loaded:
  # dplyr::rename_all
  # textclean::mgsub
  # tidyr::separate
  # tidyr::pivot_longer
  # dplyr::left_join
  # dplyr::rename
  # dplyr::filter
  # stringr::str_to_title



  # verify proper inputs
  # census_data must contain required variables for geography value

  # return new df
  out_df <- census_data
  
  out_df <- out_df %>% rename_at(all_of(rename_cols), ~rename_values)
  
  out_df <-
    out_df %>% rename(geographic_area_name = "Geographic Area Name")
  
  if (geography == "counties") {
    
    out_df$geographic_area_name <-
      gsub(" County", "", out_df$geographic_area_name)
    out_df  <-
      out_df  %>% separate("geographic_area_name", c("county", "state"), sep = ", ")
    
  } else {
    
    out_df$geographic_area_name <-
      mgsub(
        out_df$geographic_area_name,
        c(
          " city, Illinois",
          " CDP, Illinois",
          " village, Illinois",
          " town, Illinois"
        ),
        ""
      )
    
    out_df$geographic_area_name  <-
      gsub("De Pue", "DePue", out_df$geographic_area_name, fixed = TRUE) #still necessary?
    out_df <-
      out_df %>% filter(id != "1600000US1728950") #still necessary?
    
  }
  
  # Calculate ethnicity complement
  if (ethnicity == TRUE) {
    no_ethnic_col <- paste(pivot_cols_prefix, "no_", gsub(pivot_cols_prefix, "", ethnic_col), sep = "")
    out_df[[no_ethnic_col]] <- out_df[[total_col]] - out_df[[ethnic_col]]
  }
  
  out_df <- out_df %>%
    pivot_longer(
      cols = starts_with(pivot_cols_prefix),
      names_to = pivot_col,
      names_prefix = pivot_cols_prefix,
      values_to = values_to,
      values_drop_na = TRUE
    )
  
  out_df[[pivot_col]] <- gsub("_", " ", out_df[[pivot_col]]) %>% str_to_title()
  out_df
}

# SNAP Recipient Households by Race/Ethnicity

rename_cols <- c(
  "Estimate!!Households receiving food stamps/SNAP!!Households",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!White alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Black or African American alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!American Indian and Alaska Native alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Asian alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Native Hawaiian and Other Pacific Islander alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Some other race alone",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Two or more races",
  "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Hispanic or Latino origin (of any race)"
)

rename_values <- c(
  "total_snap_recipient_households",
  "snap_recipient_households_white",
  "snap_recipient_households_black_or_african_american",
  "snap_recipient_households_american_indian_and_alaska_native",
  "snap_recipient_households_asian",
  "snap_recipient_households_native_hawaiian_and_other_pacific_islander",
  "snap_recipient_households_some_other_race",
  "snap_recipient_households_two_or_more_races",
  "snap_recipient_households_hispanic_or_latino_origin"
)

pivot_col <- "demo"
pivot_cols_prefix <- "snap_recipient_households_"
values_to <- "snap_recipient_households"
ethnic_col <- "snap_recipient_households_hispanic_or_latino_origin"
total_col <- "total_snap_recipient_households"

snap_recipient_households_demo_counties <-
  clean_census_data(
    s2201_snap_counties,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

snap_recipient_households_demo_cities <-
  clean_census_data(
    s2201_snap_places,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

# LEP Households by Language

rename_cols <- c(
  "Estimate!!Total!!All households",
  "Estimate!!Limited English-speaking households!!All households",
  "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Spanish",
  "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other Indo-European languages",
  "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Asian and Pacific Island languages",
  "Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other languages"
)

rename_values <- c(
  "total_households",
  "total_lep_households",
  "lep_households_speaking_spanish",
  "lep_households_speaking_other_indo_european_languages",
  "lep_households_speaking_asian_and_pacific_island_languages",
  "lep_households_speaking_other_languages"
)	

pivot_col <- "language"
pivot_cols_prefix <- "lep_households_speaking_"
values_to <- "households"

lep_households_counties <-
  clean_census_data(
    s1602_lep_counties,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

lep_households_cities <-
  clean_census_data(
    s1602_lep_places,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

# Function for joining poverty status data:

join_poverty_status <- function(total_pop_df, below_poverty_df, x) {
  # x: name of the categorical variable
  
  total_pop_df_c <- total_pop_df
  below_poverty_df_c <- below_poverty_df
  
  total_pop_df_c$poverty_status <- "Total Population"
  below_poverty_df_c$poverty_status <- "Below 100% Poverty Level"
  
  out_df <- rbind(total_pop_df_c, below_poverty_df_c)
  out_df[[x]] <- out_df[[x]] %>% str_to_title()
  out_df
}

#Poverty Status of Individuals by Age

cols <- c(
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
  "Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over"
)

census_data <- s1701_poverty_counties[, ..cols]

rename_cols <- tail(cols, 6)

rename_values <- c(
  "total_population",
  "total_population_age_under_5_years",
  "total_population_age_5_to_17_years",
  "total_population_age_18_to_34_years",
  "total_population_age_35_to_64_years",
  "total_population_age_65_years_and_over"
)	

pivot_col <- "age"
pivot_cols_prefix <- "total_population_age_"
values_to <- "individuals"

poverty_individuals_age_counties1 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

census_data <- s1701_poverty_places[, ..cols]

poverty_individuals_age_cities1 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

cols <- c(
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over"
)

census_data <- s1701_poverty_counties[, ..cols]

rename_cols <- tail(cols, 6)

rename_values <- c(
  "total_population",
  "below_poverty_level_under_5_years",
  "below_poverty_level_5_to_17_years",
  "below_poverty_level_18_to_34_years",
  "below_poverty_level_35_to_64_years",
  "below_poverty_level_65_years_and_over"
)	

pivot_cols_prefix <- "below_poverty_level_"

poverty_individuals_age_counties2 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

census_data <- s1701_poverty_places[, ..cols]

poverty_individuals_age_cities2 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

poverty_individuals_age_counties <-
  join_poverty_status(poverty_individuals_age_counties1,
                      poverty_individuals_age_counties2,
                      "age")
poverty_individuals_age_counties$age <-
  mgsub(
    poverty_individuals_age_counties$age,
    unique(poverty_individuals_age_counties$age),
    c("Under 5", "5 To 17", "18 To 34", "35 To 64", "65 And Over")
  )

poverty_individuals_age_cities <-
  join_poverty_status(poverty_individuals_age_cities1,
                      poverty_individuals_age_cities2,
                      "age")
poverty_individuals_age_cities$age <-
  mgsub(
    poverty_individuals_age_cities$age,
    unique(poverty_individuals_age_cities$age),
    c("Under 5", "5 To 17", "18 To 34", "35 To 64", "65 And Over")
  )

#Poverty Status of Individuals by Sex

cols <- c(
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male",
  "Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female"
)

census_data <- s1701_poverty_counties[, ..cols]

rename_cols <- tail(cols, 3)

rename_values <- c(
  "total_population",
  "total_population_sex_male",
  "total_population_sex_female"
)	

pivot_col <- "sex"
pivot_cols_prefix <- "total_population_sex_"
values_to <- "individuals"

poverty_individuals_sex_counties1 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

census_data <- s1701_poverty_places[, ..cols]

poverty_individuals_sex_cities1 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

cols <- c(
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female"
)

census_data <- s1701_poverty_counties[, ..cols]

rename_cols <- tail(cols, 3)

rename_values <- c(
  "total_population",
  "below_poverty_level_male",
  "below_poverty_level_female"
)	

pivot_cols_prefix <- "below_poverty_level_"

poverty_individuals_sex_counties2 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

census_data <- s1701_poverty_places[, ..cols]

poverty_individuals_sex_cities2 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = FALSE
  )

poverty_individuals_sex_counties <-
  join_poverty_status(poverty_individuals_sex_counties1,
                      poverty_individuals_sex_counties2,
                      "sex")
poverty_individuals_sex_cities <-
  join_poverty_status(poverty_individuals_sex_cities1,
                      poverty_individuals_sex_cities2,
                      "sex")

#Poverty Status of Individuals by Race/Ethnicity

cols <- c(
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
  "Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)"
)

census_data <- s1701_poverty_counties[, ..cols]

rename_cols <- tail(cols, 9)

rename_values <- c(
  "total_population",
  "total_population_demo_white",
  "total_population_demo_black_or_african_american",
  "total_population_demo_american_indian_and_alaska_native",
  "total_population_demo_asian",
  "total_population_demo_native_hawaiian_and_other_pacific_islander",
  "total_population_demo_some_other_race",
  "total_population_demo_two_or_more_races",
  "total_population_demo_hispanic_or_latino_origin"
)	

pivot_col <- "demo"
pivot_cols_prefix <- "total_population_demo_"
values_to <- "individuals"
ethnic_col <- "total_population_demo_hispanic_or_latino_origin"
total_col <- "total_population"

poverty_individuals_demo_counties1 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

census_data <- s1701_poverty_places[, ..cols]

poverty_individuals_demo_cities1 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

cols <- c(
  "id",
  "Geographic Area Name",
  "Estimate!!Total!!Population for whom poverty status is determined",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races",
  "Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)"
)

census_data <- s1701_poverty_counties[, ..cols]

rename_cols <- tail(cols, 10)

rename_values <- c(
  "total_population",
  "total_population_below_poverty_level",
  "below_poverty_level_white",
  "below_poverty_level_black_or_african_american",
  "below_poverty_level_american_indian_and_alaska_native",
  "below_poverty_level_asian",
  "below_poverty_level_native_hawaiian_and_other_pacific_islander",
  "below_poverty_level_some_other_race",
  "below_poverty_level_two_or_more_races",
  "below_poverty_level_hispanic_or_latino_origin"
)	

pivot_cols_prefix <- "below_poverty_level_"
ethnic_col <- "below_poverty_level_hispanic_or_latino_origin"
total_col <- "total_population_below_poverty_level"

poverty_individuals_demo_counties2 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "counties",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

census_data <- s1701_poverty_places[, ..cols]

poverty_individuals_demo_cities2 <-
  clean_census_data(
    census_data,
    rename_cols,
    rename_values,
    geography = "places",
    pivot_cols_prefix,
    pivot_col,
    values_to,
    ethnicity = TRUE,
    ethnic_col,
    total_col
  )

poverty_individuals_demo_counties2 <-
  poverty_individuals_demo_counties2 %>% select(-total_population_below_poverty_level)
poverty_individuals_demo_cities2 <-
  poverty_individuals_demo_cities2 %>% select(-total_population_below_poverty_level)

poverty_individuals_demo_counties <-
  join_poverty_status(poverty_individuals_demo_counties1,
                      poverty_individuals_demo_counties2,
                      "demo")
poverty_individuals_demo_cities <-
  join_poverty_status(poverty_individuals_demo_cities1,
                      poverty_individuals_demo_cities2,
                      "demo")

# SNAP-Ed Eligibility

snap_ed_eligibility_counties <-
  c_map_poverty[, c(
    "county",
    "total_population",
    "individuals_income_below_185_percent_poverty_level"
  )] %>%
  group_by(county) %>%
  summarise(
    total_population = sum(total_population),
    individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)
  ) %>%
  ungroup()

snap_ed_eligibility_counties$state <- "Illinois"

snap_ed_eligibility_cities <-
  unique(poverty_individuals_age_cities[, c("id", "geographic_area_name", "total_population")]) %>%
  left_join(s1701_poverty_places[, c(
    "id",
    "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level"
  )], by = "id") %>%
  rename(individuals_income_below_185_percent_poverty_level = "Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level")

# Adult Obesity

adult_obesity <- adult_obesity %>% rename(fips = "FIPS",
                                          state = "State",
                                          county = "County",
                                          percent_adults_obesity = "% Adults with Obesity")

# Food Insecurity

food_insecurity <- food_insecurity %>% rename(county = "County, State",
                                              food_insecurity_rate = "2019 Food Insecurity Rate",
                                              food_insecure_persons = "# of Food Insecure Persons in 2019",
                                              child_food_insecurity_rate = "2019 Child food insecurity rate",
                                              food_insecure_children = "# of Food Insecure Children in 2019")

food_insecurity$county <- gsub(" County, Illinois", "", food_insecurity$county)

## Shiny App

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    
    tags$style(
      ".block {
      margin: 10px;
      min-height: 200px;
    }
    
    span, a {
    }
    "
    ),
    
    shinyjs::useShinyjs(),
    fluidRow(
      tabBox(
        title = NULL,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "850px", width = 12, 
        tabPanel("Community Programming Map", 
                 fluidRow(
                   column(9, 
                          leafletOutput("map", height = 800),
                   ),
                   column(3,
                          h3("Site Filters"),
                          pickerInput("programInput", "INEP Program Type",
                                      choices = c("SNAP-Ed", "EFNEP"),
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE)),
                          pickerInput("eligiblesiteInput", "Community Site Type",
                                      choices = levels(as.factor(eligible_sites$site_type)),
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE)),
                          HTML(marker_legend_html(icon_set = icon_set)),
                   )
                 )
        ),
        tabPanel("Community Profile",
                 fluidRow(
                   column(10,
                          fluidRow(
                            column(3,
                                   h4("Income Statistics"),
                                   htmlOutput("text1")
                            ),
                            column(4,
                                   h4("Food Insecurity"),
                                   fluidRow(
                                     column(6, htmlOutput("text4")),
                                     column(6, htmlOutput("text5"))
                                   )
                            ),
                            column(2,
                                   h4("Adult Obesity"),
                                   htmlOutput("text3")
                            ),
                            
                            column(3,
                                   h4("Limited English Proficiency"),
                                   htmlOutput("text2")
                            ),
                          ),
                          fluidRow(
                            fluidRow(
                              column(4, plotlyOutput("plot1")),
                              column(8, plotlyOutput("plot2")),
                            )
                          ),
                          fluidRow(
                            column(7, plotlyOutput("plot3")),
                            column(5, plotlyOutput("plot4")),
                          ),
                          fluidRow(
                            column(5, plotlyOutput("plot5")),
                            column(3, plotlyOutput("plot6")),                            
                            column(4, plotlyOutput("plot7")),
                          ),
                   ),
                   column(2,
                          h3("Community Profile Filters"),
                          
                          pickerInput("countyInput", "County",
                                      choices = levels(as.factor(snap_recipient_households_demo_counties$county)),
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = pickerOptions(liveSearch = T, maxOptions = 1)), #select all > NULL
                          
                          pickerInput("cityInput", "City",
                                      choices = levels(as.factor(snap_recipient_households_demo_cities$geographic_area_name)),
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = pickerOptions(liveSearch = T, maxOptions = 1)),
                          
                          actionButton("reset_input", "Clear All Filters"),
                          h5("Metrics for Illinois displayed by default."), 
                   ),
                   
                 ),
        ),
        
        tabPanel("External Links & Sources",
                 div(
                   class = "block",
                   HTML('
                               <h4><b>External Links</b></h4>
                               
                               <i>The following data sources are used as supplementary resources for INEP strategic planning.
                               <br>Please reach out to the <a href = "mailto: uie-fcsevaluation@illinois.edu">INEP Evaluation Team </a>
                               if you have suggestions for additonal data sources.</i><br>
                               
                               <li> <a href=https://snaped.engagementnetwork.org/assessment/ target="_blank">SNAP-Ed Assessment</a></li>
                               <li> <a href=http://www.countyhealthrankings.org/ target="_blank">County Health Rankings</a></li>
                               <li> <a href=https://www.ers.usda.gov/data-products/food-access-research-atlas/go-to-the-atlas/ target="_blank">Food Access Research Atlas</a></li>
                               <li> <a href=https://www.ers.usda.gov/data-products/food-environment-atlas/go-to-the-atlas/ target="_blank">Food Environment Atlas</a></li>
                               <li> <a href=https://engagementnetwork.org/map-room/?action=tool_map&tool=footprint target="_blank">CARES Engagement Network (Vulnerable populations footprint)</a></li>
                               <li> <a href=https://www.communitycommons.org/ target="_blank">Community Commons</a></li>
                               <li> <a href=https://insight.livestories.com/s/v2/win-measures/2fda874f-6683-49bd-adb2-22f6f3c5a718/ target="_blank">Well Being in the Nation Measurement Framework</a></li>
                               <li> <a href=https://map.feedingamerica.org/ target="_blank">Feeding America Map the Meal Gap</a></li>
                               <li> <a href=https://iys.cprd.illinois.edu/results/county target="_blank">Illinois Youth Survey</a></li>
                               <li> <a href=https://eat-move-save.extension.illinois.edu/ target="_blank">Find Food IL Community Food Map</a></li>
                               
                               <h4><b>Sources</b></h4>
                               <h5><b>Income Statistics</b></h5>
                               <p>Figures for Income Statistics (individuals with income at or below 185% of the Federal Poverty Level) are provided by the 2019 American Community Survey 5-year estimates.
                               <br><a href=https://www.census.gov/programs-surveys/acs/about.html target="_blank">About the American Community Survey</a></br></p>
                               
                               <h5><b>INEP Program Sites</b></h5>
                               <p>INEP Program Sites for the 2021 report year are sourced from PEARS.
                               <br><a href=https://www.k-state.edu/oeie/pears/ target="_blank">Office of Educational Innovation and Evaluation (OEIE) - PEARS</a></br></p>
                               
                               <h5><b>Community Sites</b></h5>
                               <p>The following sources are utilized for community sites:<br>
                               
                               <li> Family Community Resource Centers (FCRC) and Women, Infants, and Children (WIC) Offices - <a href=https://www.dhs.state.il.us/page.aspx?module=12 target="_blank">IDHS Office Locator</a>
                               <li> Federally Qualified Health Centers (FQHC) - <a href=https://data.hrsa.gov/geo target="_blank">HRSA Data by Geography</a>
                               <li> Food Pantries & Meal Sites - <a href=https://eat-move-save.extension.illinois.edu/ target="_blank">Find Food IL Community Food Map</a>
                               <li> Head Start Centers - <a href=https://eclkc.ohs.acf.hhs.gov/center-locator target="_blank">Head Start Center Locator</a>
                               <li> Eligible Schools - <a href=https://www.isbe.net/Pages/Nutrition-Data-Analytics-Maps.aspx target="_blank">ISBE Free and Reduced-Price Meal Eligibility Data</a>
                               <li> Emergency Shelters - <a href=http://www.illinoisfoodbanks.org/sites.asp target="_blank">Illinois Pantries, Soup Kitchens and Emergency Shelters</a>
                               
                               <h5><b>Food Insecurity</b></h5>
                               <p>Feeding America\'s <i>Map the Meal Gap</i> study provides annual estimates of Food Insecurity rates.
                               <br><a href=https://www.feedingamerica.org/research/map-the-meal-gap/overall-executive-summary target="_blank">Feeding America | Food Insecurity Report Briefs</a></br></p>
                               
                               <h5><b>Adult Obesity</b></h5>
                               <p>Adult Obesity Rate is calculated by County Health Rankings and based on responses to the Behavioral Risk Factor Surveillance Survey (BRFSS). 
                               <br><a href=https://www.countyhealthrankings.org/app/illinois/2022/measure/factors/11/description target="_blank">County Health Rankings | Adult Obesity</a></br></p>
                               
                               <h5><b>Demographics</b></h5>
                               <p>Demographic measures including Limited English Proficiency, Poverty Status, and SNAP Recipient Households are provided by the 2019 American Community Survey 5-year estimates.
                               <br><a href=https://www.census.gov/programs-surveys/acs/about.html target="_blank">About the American Community Survey</a></br></p>
                               ')
                 )
        )
      )
    ),
  ),
)



server <- function(input, output, session) {
  
  site_programming.reactive <-
    reactive({
      if (!is.null(input$programInput)) {
        input_cols <-
          mgsub(
            input$programInput,
            c(
              "SNAP-Ed",
              "EFNEP"
            ),
            c(
              "snap_ed",
              "efnep"
            )
          )
        
        ind_filters <- c()
        
        for (i in 1:length(input_cols)) {
          ind_filters[i] <- paste0(input_cols[i], " == 'Yes'")
        }
        
        filters <- paste(ind_filters, collapse = " | ")
        
        site_programming <-
          filter(site_programming, eval(parse(text = filters)))
      } else {
        site_programming 
      }
      
    })
  
  eligible_sites.reactive <-
    reactive({
      if (!is.null(input$eligiblesiteInput)) {
        eligible_sites %>% filter(site_type %in% input$eligiblesiteInput)
        
      } else {
        eligible_sites
      }
      
    })   
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("CartoDB.Voyager",
                       options = providerTileOptions(updateWhenZooming = FALSE,      # map won"t update tiles until zoom is done
                                                     updateWhenIdle = FALSE)) %>%
      setView(-89.241943359375, 40.17047886718109, zoom = 7) %>%
      addLayersControl(
        overlayGroups = c(
          "Number of Individuals below 185% FPL",
          "Percent Individuals below 185% FPL",
          "INEP Program Sites",
          "Community Sites"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
      addPolygons(
        data = il_tracts_sf_merged,
        fillColor = ~ pal1(il_tracts_sf_merged$snap_eligibility_percent),
        fillOpacity = 0.5,
        weight = 0.9,
        smoothFactor = 0.2,
        stroke = TRUE,
        color = "white",
        popup = ~ popup_sb1,
        group = "Percent Individuals below 185% FPL"
      ) %>%
      addPolygons(
        data = il_tracts_sf_merged,
        fillColor = ~ pal2(
          il_tracts_sf_merged$individuals_income_below_185_percent_poverty_level
        ),
        fillOpacity = 0.5,
        weight = 0.9,
        smoothFactor = 0.2,
        stroke = TRUE,
        color = "white",
        popup = ~ popup_sb1,
        group = "Number of Individuals below 185% FPL"
      ) %>%
      addLegend(
        pal = pal1,
        values = il_tracts_sf_merged$snap_eligibility_percent,
        position = "bottomright",
        title = "Percent Individuals below 185% FPL",
        group = "Percent Individuals below 185% FPL"
      ) %>%
      addLegend(
        pal = pal2,
        values = il_tracts_sf_merged$individuals_income_below_185_percent_poverty_level,
        position = "bottomright",
        title = "Number of Individuals below 185% FPL",
        group = "Number of Individuals below 185% FPL"
      ) %>% hideGroup(c("Number of Individuals below 185% FPL", "Community Sites"))
  })
  
  
  observe({
    
    popup3 <- paste0("INEP Site: ", as.character(site_programming.reactive()$site_name),
                     "</br/> SNAP-Ed Program: ", as.character(site_programming.reactive()$snap_ed),
                     "</br/> EFNEP Program: ", as.character(site_programming.reactive()$efnep)#,
                     # "</br/> Program Activities: ", as.character(site_programming.reactive()$program_activities), #if null, skip line
                     # "</br/> Indirect Activities: ", as.character(site_programming.reactive()$indirect_activities),
                     # "</br/> PSE Site Activities: ", as.character(site_programming.reactive()$pse_site_activities),
                     # "</br/> Coalitions: ", as.character(site_programming.reactive()$coalitions),
                     # "</br/> Partnerships: ", as.character(site_programming.reactive()$partnerships)
                     )
    
    leafletProxy("map", data = site_programming.reactive()) %>%
      clearGroup(group = "INEP Program Sites") %>%
      addAwesomeMarkers(lng = ~longitude,
                        lat = ~latitude,
                        icon = ~icon_set["INEP Program Site"],
                        popup = ~popup3,
                        label =  paste0("", as.character(site_programming.reactive()$site_name)),
                        group = "INEP Program Sites",
                        clusterOptions = markerClusterOptions(
                          disableClusteringAtZoom = 14,
                          spiderfyOnMaxZoom = FALSE,
                          showCoverageOnHover = FALSE
                        ))
    
  })
  
  observe({
    
    leafletProxy("map") %>%
      clearGroup(group = "Community Sites")
    
    leafletProxy("map") %>%
      addAwesomeMarkers(data = eligible_sites.reactive(),
                        lng = ~longitude,
                        lat = ~latitude,
                        icon = ~icon_set[site_type],
                        popup = ~paste0(site_name,
                                      "</br/>", site_address,
                                      "</br/>", site_city, ", ", site_state, " ", site_zip),
                        label = ~as.character(site_name),
                        group = "Community Sites",
                        clusterOptions = markerClusterOptions(
                          disableClusteringAtZoom = 14,
                          spiderfyOnMaxZoom = FALSE,
                          showCoverageOnHover = FALSE
                        ))
  })        
  
  # Community Profile
  
  observeEvent(
    # define pickerinputs to be observed
    c(
      input$countyInput
    ),
    {
      if (!is.null(input$countyInput)) {
        places_counties2 <- places_counties %>%
          filter(COUNTY %in% input$countyInput)
        
        updatePickerInput(
          session,
          "cityInput",
          choices = levels(factor(places_counties2$PLACE)),
          selected = NULL
        )               
      } else {
        updatePickerInput(
          session,
          "countyInput",
          choices = levels(factor(snap_recipient_households_demo_counties$county)),
          selected = NULL
        )
        updatePickerInput(
          session,
          "cityInput",
          choices = levels(factor(snap_recipient_households_demo_cities$geographic_area_name)),
          selected = NULL
        )
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = F
  )
  
  plot_reactive <- function(counties_df, cities_df, group_var, summarise_var) {
    reactive({
      
      if (!is.null(input$countyInput) & is.null(input$cityInput)) {
        
        counties_df %>% filter(county %in% input$countyInput)
        
      } else if (!is.null(input$cityInput)) {
        
        cities_df %>% filter(geographic_area_name %in% input$cityInput)
        
      } else {
        
        counties_df %>%
          group_by(across({{ group_var }})) %>%
          summarise(across({{ summarise_var }}, sum)) %>%
          ungroup()
        
      }
      
    })
  }
  
  poverty_individuals_sex.reactive <-
    plot_reactive(
      poverty_individuals_sex_counties,
      poverty_individuals_sex_cities,
      c(state, sex, poverty_status),
      c(individuals, total_population)
    )
  
  poverty_individuals_age.reactive <-
    plot_reactive(
      poverty_individuals_age_counties,
      poverty_individuals_age_cities,
      c(state, age, poverty_status),
      c(individuals, total_population)
    )
  
  poverty_individuals_demo.reactive <-
    plot_reactive(
      poverty_individuals_demo_counties,
      poverty_individuals_demo_cities,
      c(state, demo, poverty_status),
      c(individuals, total_population)
    )
  
  snap_recipient_households_demo.reactive <-
    plot_reactive(
      snap_recipient_households_demo_counties,
      snap_recipient_households_demo_cities,
      c(state, demo),
      c(snap_recipient_households, total_snap_recipient_households)
    )
  
  lep_households.reactive <-
    plot_reactive(
      lep_households_counties,
      lep_households_cities,
      c(state, language),
      c(households, total_lep_households, total_households)
    )
  
  snap_ed_eligibility.reactive <-
    plot_reactive(
      snap_ed_eligibility_counties,
      snap_ed_eligibility_cities,
      state,
      c(total_population, individuals_income_below_185_percent_poverty_level)
    )
  
  # Function for plot template
  plot_template <-
    function(reactive_data,
             demographic = FALSE,
             ind_var,
             dep_var,
             total,
             grouped = FALSE,
             group_var,
             group_name,
             title,
             labels_map = waiver(),
             hide_traces = c(),
             legend = TRUE) {
      # title: character
      # input arg for hiding legends
      
    data <- reactive_data
    
    if (demographic == "race") {
      data <- data %>%
        filter(!{{ ind_var }} %in% c("Hispanic Or Latino Origin", "No Hispanic Or Latino Origin"))
    } else if (demographic == "ethnicity") {
      data <- data %>%
        filter({{ ind_var }} %in% c("Hispanic Or Latino Origin", "No Hispanic Or Latino Origin"))
    }	
    
    data <- data %>%
      mutate(percent = paste0(as.character(round((100 * {{ dep_var }} / {{ total }}), digits = 1)), "%"))
    
    if (grouped == TRUE) {
      ggplotly(ggplot(data, aes({{ ind_var }}, y = {{ dep_var }}, fill = factor({{ group_var }}))) +
                        geom_col(position = "dodge") +
                        geom_text(aes(label = percent, y = {{ dep_var }} + 0.02 * max({{ dep_var }})), position = position_dodge(width = 0.9)) + #dodge width as input var?
                        theme(axis.text.x = element_text(angle = 45, hjust = 1),
                          axis.title.x = element_blank(), axis.title.y = element_blank(),
                          plot.title = element_text(hjust = 0.5)) +
                        ggtitle(title) +
                        scale_fill_discrete(name = group_name) +
                        scale_x_discrete(labels = labels_map) +
                        scale_y_continuous(labels = scales::comma), tooltip = c("y"))  %>%
                 style(hoverinfo = "none", traces = hide_traces) %>%
             layout(showlegend = legend)
    } else {
      ggplotly(ggplot(data, aes(x = {{ ind_var }}, y = {{ dep_var }}, fill = {{ ind_var }})) +
                 geom_col(show.legend = FALSE) +
                 geom_text(aes(label=percent, y = {{ dep_var }}+0.02*max({{ dep_var }})), position = position_dodge(width = 0.6)) +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1),
                       axis.title.x = element_blank(), axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5)) +
                 ggtitle(title) +
                 scale_x_discrete(labels = labels_map) +
                 scale_y_continuous(labels = scales::comma), tooltip = c("y")) %>%
        layout(showlegend = FALSE) %>% #input argument
        style(hoverinfo = "none", traces = hide_traces) #https://stackoverflow.com/questions/51004936/ggplotly-only-return-tooltip-hover-text-on-certain-geom-objects
      # Hide traces programmatically?
    }       
  }
  
  output$plot1 <- renderPlotly({
    plot_template(
      poverty_individuals_sex.reactive(),
      ind_var = sex,
      dep_var = individuals,
      grouped = TRUE,
      group_var = poverty_status, 
      group_name = "Poverty Status",
      total = total_population,
      title = "Poverty Status of Individuals by Sex",
      hide_traces = c(3, 4),
      legend = FALSE
    )
  })
  
  output$plot2 <- renderPlotly({
    age_level_order <-
      factor(
        poverty_individuals_age.reactive()$age,
        level = c("Under 5", "5 To 17", "18 To 34", "35 To 64", "65 And Over")
      )
    
    plot_template(
      poverty_individuals_age.reactive(),
      ind_var = age_level_order,
      dep_var = individuals,
      grouped = TRUE,
      group_var = poverty_status, #factor?
      group_name = "Poverty Status",
      total = total_population,
      title = "Poverty Status of Individuals by Age",
      hide_traces = c(3, 4)
    )
  })
  
  labels_map <- c("American Indian And Alaska Native" = "American Indian\nAnd Alaska Native",
                  "Black Or African American" = "Black Or\nAfrican American",
                  "Native Hawaiian And Other Pacific Islander" = "Native Hawaiian\nAnd Other\nPacific Islander",
                  "Some Other Race" = "Some Other\nRace",
                  "Two Or More Races" = "Two Or More\nRaces"
  )
  
  output$plot3 <- renderPlotly({
    plot_template(
      poverty_individuals_demo.reactive(),
      demographic = "race",
      ind_var = demo,
      dep_var = individuals,
      grouped = TRUE,
      group_var = poverty_status,
      group_name = "Poverty Status",
      total = total_population,
      title = "Poverty Status of Individuals by Race",
      labels_map = labels_map,
      legend = FALSE,
      hide_traces = c(3, 4)
    )
  })
  
  output$plot4 <- renderPlotly({
    plot_template(
      poverty_individuals_demo.reactive(),
      demographic = "ethnicity",
      ind_var = demo,
      dep_var = individuals,
      grouped = TRUE,
      group_var = poverty_status,
      group_name = "Poverty Status",
      total = total_population,
      title = "Poverty Status of Individuals by Ethnicity",
      labels_map = c("Hispanic Or Latino Origin" = "Hispanic Or\nLatino Origin",
                     "No Hispanic Or Latino Origin" = "No Hispanic Or\nLatino Origin"),
      hide_traces = c(3, 4)
    )
  })
  
  output$plot5 <- renderPlotly({
    plot_template(
      snap_recipient_households_demo.reactive(),
      demographic = "race",
      ind_var = demo,
      dep_var = snap_recipient_households,
      group_var = demo,
      total = total_snap_recipient_households,
      title = "SNAP Households by Race",
      labels_map = labels_map,
      hide_traces = c(8, 9, 10, 11, 12, 13, 14)
    )
  })
    
  output$plot6 <- renderPlotly({
    plot_template(
      snap_recipient_households_demo.reactive(),
      demographic = "ethnicity",
      ind_var = demo,
      dep_var = snap_recipient_households,
      group_var = demo,
      total = total_snap_recipient_households,
      title = "SNAP Households by Ethnicity",
      labels_map = c(
        "Hispanic Or Latino Origin" = "Hispanic Or\nLatino Origin",
        "No Hispanic Or Latino Origin" = "No Hispanic Or\nLatino Origin"
      ),
      hide_traces = c(3, 4)
    )
  })

  output$plot7 <- renderPlotly({
    plot_template(
      lep_households.reactive(),
      ind_var = language,
      dep_var = households,
      group_var = language,
      total = total_lep_households,
      title = "LEP Households by Language",
      labels_map = c(
        "Asian And Pacific Island Languages" = "Asian And\nPacific Island\nLanguages",
        "Other Indo European Languages" = "Other\nIndo European\nLanguages"
      ),
      hide_traces = c(5, 6, 7, 8)
    )
  })
  
  output$text1 <- renderUI({
    HTML(
      paste("<b>Individuals Earning Below 185% Poverty Level:</b>",
            as.character(format(snap_ed_eligibility.reactive()$individuals_income_below_185_percent_poverty_level, big.mark=",")),
            "<b>Percentage of Individuals Earning Below 185% FPL:</b>",
            paste0(as.character(round(100 * snap_ed_eligibility.reactive()$individuals_income_below_185_percent_poverty_level/
                                        snap_ed_eligibility.reactive()$total_population, digits = 1)), "%"), sep = "<br/>")
    )
  })
  
  output$text2 <- renderUI({
    HTML(
      paste("<b>LEP Households:</b>",
            as.character(format(unique(lep_households.reactive()$total_lep_households), big.mark=",")),
            "<b>Percentage of LEP Households:</b>",
            paste0(as.character(unique(round(100 * lep_households.reactive()$total_lep_households/
                                               lep_households.reactive()$total_households, digits = 1))), "%"), sep = "<br/>")
    )
  })
    
  adult_obesity.reactive <-
    reactive({
      
      if (!is.null(input$countyInput)) {
        
        adult_obesity %>% filter(county %in% input$countyInput)
        
      } else {
        
        adult_obesity %>% filter(county == "")
        
      }
      
    })
  
  observe({
    
    if ((!is.null(input$countyInput) & is.null(input$cityInput)) |
        (is.null(input$cityInput))) {
      
      output$text3 <- renderUI({
        HTML(
          paste("<b>Adult Obesity Rate:</b>",
                paste0(as.character(adult_obesity.reactive()$percent_adults_obesity), "%"), sep = "<br/>")
        )
      })
      
    } else {
      output$text3 <- renderUI({
        HTML("This metric is only available by County/State.")
      })
    }
    
  })
  
  food_insecurity.reactive <-
    reactive({
      
      if (!is.null(input$countyInput)) {
        
        food_insecurity %>% filter(county %in% input$countyInput)
        
      } else {
        
        food_insecurity %>% filter(county == "Illinois")
        
      }
      
    })
  
  observe({
    
    if ((!is.null(input$countyInput) & is.null(input$cityInput)) |
        (is.null(input$cityInput))) {
      
      output$text4 <- renderUI({
        HTML(
          paste("<b>2019 Food Insecurity Rate:</b>",
                as.character(food_insecurity.reactive()$food_insecurity_rate),
                "<b>Food Insecure Persons in 2019:</b>",
                as.character(food_insecurity.reactive()$food_insecure_persons), sep = "<br/>")
          
          
        )
      })
      
      output$text5 <- renderUI({
        HTML(
          paste("<b>2019 Child Food Insecurity Rate:</b>",
                as.character(food_insecurity.reactive()$child_food_insecurity_rate),
                "<b>Food Insecure Children in 2019:</b>",
                as.character(food_insecurity.reactive()$food_insecure_children), sep = "<br/>")
          
          
        )
      })
      
    } else {
      output$text4 <- renderUI({
        HTML("This metric is only available by County/State.")
      })
      output$text5 <- renderUI({
        HTML("This metric is only available by County/State.")
      })
    }
    
  })
  
  observeEvent(input$reset_input, {
    #ids > vector > if notnull() > reset()
    shinyjs::reset("countyInput")
    shinyjs::reset("cityInput")
  })
  
}

shinyApp(ui, server)