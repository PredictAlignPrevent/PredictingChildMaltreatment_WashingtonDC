##################################################################
#### Create Variables
##################################################################

library("sf")            # Spatial data objects and methods
library("tidyverse")     # data manipulation framework
library("geojsonsf")
library("lubridate")


##1.1 Global Variables
base_dir = "C:/projects/PAP_Wash_DC"

#read in neighborhood file to filter out those points outside study areas
nbr <- geojson_sf("https://opendata.arcgis.com/datasets/f6c703ebe2534fc3800609a07bad8f5b_17.geojson") %>%
  st_transform(102685) %>% 
  dplyr::select(NAME, NBH_NAMES, geometry)

##### 2.1 Load Data #####
files <-list.files(file.path(base_dir,"/2019_11_11_dataFrom_DC/Predict_Data/"), pattern = "*\\.xls$|*\\.csv$")
var_list <- vector(mode = "list")
var_names <- NULL
for(i in seq_along(files)){
  filename <- str_sub(files[i], start = 1, end = -5)
  sf_i <- tryCatch({
    if(tools::file_ext(files[i]) == "xls"){
      dat <- readxl::read_xls(file.path(base_dir,"/2019_11_11_dataFrom_DC/Predict_Data/",files[i])) 
    } else if(tools::file_ext(files[i]) == "csv"){
      dat <- read.csv(file.path(base_dir,"/2019_11_11_dataFrom_DC/Predict_Data/",files[i])) 
    }
    
    dat %>%
      filter(!is.na(X) | !is.na(Y)) %>%
      st_as_sf(., coords = c("Y", "X"), crs = 4326) %>% 
      st_transform(102685) %>% 
      .[st_union(nbr),]
    
  }, error = function(e){
    cat(filename, "error = ",e$message,"\n")
    return(e)
  }
  )
  if(!inherits(sf_i, "error")){
    var_list[[length(var_list)+1]] <- sf_i
    var_names[length(var_list)] <- filename
  }
}

# Libraries

libraries <- geojson_sf("https://opendata.arcgis.com/datasets/cab0eaaad4e242c18a36422c3323e6ac_4.geojson") %>% 
  st_transform(102685) %>% 
  dplyr::select(NAME, ADDRESS, geometry) %>% 
  .[st_union(nbr),]

var_list[[length(var_list)+1]] <- libraries

# Parks

Parks <- geojson_sf("https://opendata.arcgis.com/datasets/287eaa2ecbff4d699762bbc6795ffdca_9.geojson") %>% 
  st_transform(102685) %>% 
  dplyr::select(NAME, ADDRESS, geometry) %>% 
  st_centroid(.) %>% 
  .[st_union(nbr),]

var_list[[length(var_list)+1]] <- Parks

# Liquor Licenses

Liquor_Licenses <- geojson_sf("https://opendata.arcgis.com/datasets/cabe9dcef0b344518c7fae1a3def7de1_5.geojson") %>% 
  st_transform(102685) %>% 
  dplyr::select(TRADE_NAME, TYPE, ADDRESS, geometry) %>% 
  mutate(TYPE = case_when(TYPE == "Tavern" ~ "Bar",
                          str_detect(TYPE, "Retail") | TYPE == "Wholesaler" ~ "Retailer",
                          TYPE == "Restaurant" ~ "Restaurant")) %>% 
  filter(TYPE %in% c("Bar", "Restaurant", "Retailer")) %>% 
  .[st_union(nbr),] %>% 
  unique()

var_list[[length(var_list)+1]] <- Liquor_Licenses

# Fire Stations

Fire_Stations <- geojson_sf("https://opendata.arcgis.com/datasets/05d048a0aa4845c6a0912f3a9f216992_6.geojson") %>% 
  st_transform(102685) %>% 
  dplyr::select(NAME, ADDRESS, geometry) %>% 
  .[st_union(nbr),]

var_list[[length(var_list)+1]] <- Fire_Stations


# Public Housing

PublicHousing_Areas <- geojson_sf("https://opendata.arcgis.com/datasets/7f40eee5afaa4210959c2a55328a0cab_15.geojson") %>% 
  st_transform(102685) %>% 
  dplyr::select(NAME, ADDRESS, geometry) %>% 
  st_centroid(.) %>% 
  .[st_union(nbr),]

var_list[[length(var_list)+1]] <- PublicHousing_Areas

names(var_list) <- c(var_names, "Libraries", "Parks", "Liquor_Licenses", "Fire_Stations", "Public_Housing")

####### 2.2 Protective Variables #####
protective_class <- c("Child_Development_Centers", "Food_Bank", "Homeless_Services", 
                      "Homeless_Shelters", "Playgrounds", "Police_Stations", "Rec_Centers", 
                      "ReligiousOrgs", "Schools", "SNAP", "WIC", "Fire_Stations", "Parks", 
                      "Libraries")

protective_vars <- list()
for(i in seq_along(protective_class)){
  dat <- var_list[[protective_class[i]]] %>%
    mutate(feature_name = protective_class[i],
           class = "protective") %>%
    dplyr::select(feature_name, class)
  protective_vars[[i]] <- dat
}

Businesses_protective <- var_list[["BusinessProject"]] %>%
  filter(Category == "Protective") %>%
  mutate(feature_name = "BusinessProject",
         class = "protective") %>%
  dplyr::select(feature_name, class)

protective_vars[[length(protective_vars)+1]] <- Businesses_protective
protective_vars <- do.call(rbind, protective_vars)

var_list[["Protective"]] <- protective_vars


####### 2.3 Risk variables #####
risk_class <- c("Check_Cashing", "BusStops", "NonDepository_Banks", "Liquor_Licenses", "Public_Housing")
risk_vars <- list()

#get the risk variables that will not change

for(i in seq_along(risk_class)){
  dat <- var_list[[risk_class[i]]] %>%
    mutate(feature_name = risk_class[i],
           class = "risk") %>%
    dplyr::select(feature_name, class)
  risk_vars[[i]] <- dat
}

Business_risk <- var_list[["BusinessProject"]] %>%
  filter(Category == "Risk") %>%
  mutate(feature_name = "BusinessProject",
         class = "risk") %>%
  dplyr::select(feature_name, class)
risk_vars[[length(risk_vars)+1]] <- Business_risk

#need to make risk variables for 2017/18 and 2019 because 311 calls and crimes will have differences
#putting the risk variables that will not change in each dataset

risk_vars_1718 <- risk_vars
risk_vars_19 <- risk_vars

# separating crime and 311 calls by year 
#crime
CrimeData_risk_1718 <- var_list[["Crime_2017.19"]] %>%
  mutate(REPORT_DAT = as.character(REPORT_DAT),
         REPORT_DAT = lubridate::mdy(REPORT_DAT)) %>% 
  filter(REPORT_DAT >= "2017-1-1" & REPORT_DAT <= "2018-12-31") %>% 
  mutate(feature_name = "CrimeData_1718",
         class = "risk") %>%
  dplyr::select(feature_name, class)
risk_vars_1718[[length(risk_vars_1718)+1]] <- CrimeData_risk_1718

CrimeData_risk_19 <- var_list[["Crime_2017.19"]] %>%
  mutate(REPORT_DAT = as.character(REPORT_DAT),
         REPORT_DAT = lubridate::mdy(REPORT_DAT)) %>% 
  filter(REPORT_DAT >= "2019-1-1" & REPORT_DAT <= "2019-12-31") %>% 
  mutate(feature_name = "CrimeData_19",
         class = "risk") %>%
  dplyr::select(feature_name, class)
risk_vars_19[[length(risk_vars_19)+1]] <- CrimeData_risk_19

#311 calls
calls311_risk_1718 <- var_list[["311Calls_2017.19"]] %>%
  mutate(ADDDATE = as.character(ADDDATE),
         ADDDATE = lubridate::mdy(ADDDATE)) %>% 
  filter(ADDDATE >= "2017-1-1" & ADDDATE <= "2018-12-31") %>%
  filter(!(SERVICECODEDESCRIPTION %in% c('Dead Animal Collection','Rodent Inspection and Treatment'))) %>%
  mutate(feature_name = "311Calls",
         class = "risk") %>%
  dplyr::select(feature_name, class)
risk_vars_1718[[length(risk_vars_1718)+1]] <- calls311_risk_1718

calls311_risk_19 <- var_list[["311Calls_2017.19"]] %>%
  mutate(ADDDATE = as.character(ADDDATE),
         ADDDATE = lubridate::mdy(ADDDATE)) %>% 
  filter(ADDDATE >= "2019-1-1" & ADDDATE <= "2019-12-31") %>% 
  filter(!(SERVICECODEDESCRIPTION %in% c('Dead Animal Collection','Rodent Inspection and Treatment'))) %>%
  mutate(feature_name = "311Calls",
         class = "risk") %>%
  dplyr::select(feature_name, class)
risk_vars_19[[length(risk_vars_19)+1]] <- calls311_risk_19

risk_vars_1718 <- do.call(rbind, risk_vars_1718)
risk_vars_19 <- do.call(rbind, risk_vars_19)

#add to the variable list 
var_list[["Risk_1718"]] <- risk_vars_1718
var_list[["Risk_19"]] <- risk_vars_19

#add crime and 311 calls to var_list for 2017/18 and 2019
CrimeData_1718 <- var_list[["Crime_2017.19"]] %>%
  mutate(REPORT_DAT = as.character(REPORT_DAT),
         REPORT_DAT = lubridate::mdy(REPORT_DAT)) %>% 
  filter(REPORT_DAT >= "2017-1-1" & REPORT_DAT <= "2018-12-31")

CrimeData_19 <- var_list[["Crime_2017.19"]] %>%
  mutate(REPORT_DAT = as.character(REPORT_DAT),
         REPORT_DAT = lubridate::mdy(REPORT_DAT)) %>% 
  filter(REPORT_DAT >= "2019-1-1" & REPORT_DAT <= "2019-12-31")

var_list[["Crime_1718"]] <- CrimeData_1718
var_list[["Crime_19"]] <- CrimeData_19

calls311_1718 <- var_list[["311Calls_2017.19"]] %>%
  mutate(ADDDATE = as.character(ADDDATE),
         ADDDATE = lubridate::mdy(ADDDATE)) %>% 
  filter(ADDDATE >= "2017-1-1" & ADDDATE <= "2018-12-31") %>%
  filter(!(SERVICECODEDESCRIPTION %in% c('Dead Animal Collection','Rodent Inspection and Treatment'))) 

calls311_19 <- var_list[["311Calls_2017.19"]] %>%
  mutate(ADDDATE = as.character(ADDDATE),
         ADDDATE = lubridate::mdy(ADDDATE)) %>% 
  filter(ADDDATE >= "2019-1-1" & ADDDATE <= "2019-12-31") %>% 
  filter(!(SERVICECODEDESCRIPTION %in% c('Dead Animal Collection','Rodent Inspection and Treatment')))

var_list[["311_1718"]] <- calls311_1718
var_list[["311_19"]] <- calls311_19

###### getting individual features #####
#separate out each type of crime, business, and 311 calls

# Violations Individual (min n = 100)
violations_list_1718 <- get_individual_features(var_list[["311_1718"]], "SERVICECODEDESCRIPTION", "VIO.1718", 100)
violations_list_19 <- get_individual_features(var_list[["311_19"]], "SERVICECODEDESCRIPTION", "VIO.19", 100)
var_list <- c(var_list, violations_list_1718)
var_list <- c(var_list, violations_list_19)

# CrimeData Individual (min n = 100)
cimedata_list_1718 <- get_individual_features(var_list[["Crime_1718"]], "OFFENSE", "CRIME.1718", 100)
cimedata_list_19 <- get_individual_features(var_list[["Crime_19"]], "OFFENSE", "CRIME.19", 100)
var_list <- c(var_list, cimedata_list_1718)
var_list <- c(var_list, cimedata_list_19)

# BusinessProject Individual (min n = 10)
businessproject_list <- get_individual_features(var_list[["BusinessProject"]], "SELECTED_SIC_DESC", "BUSI", 10)
var_list <- c(var_list, businessproject_list)


###### clean up var_list ####

#filtering the screened in cases to remove duplicates
ScreenedIn_Cases <- var_list[["ScreenedIn_Cases"]] %>%
  group_by(ID, REFERRAL_DATE) %>% 
  mutate(n = row_number()) %>% 
  filter(n == 1) %>% 
  dplyr::select(-n) %>% 
  ungroup()
var_list[["ScreenedIn_Cases"]] <- ScreenedIn_Cases


#remove unwanted variables
var_list <- var_list[-c(1, 6)]

