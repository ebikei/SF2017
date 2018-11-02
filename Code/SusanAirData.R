x=c('tidyverse', 'reshape2', 'readxl')
lapply(x,require,character.only=T)


## AirNOW
temp.location = read.csv('https://haze.airfire.org/bluesky-daily/output/temp_susan/HAQAST/ObservationalDataAnalysis/monitoring_data_csv/2017_airnow.csv')
location.airnow = filter(temp.df, row_number() <58) %>%
	select(monitorID, longitude, latitude, agencyName) %>%
	mutate(agencyName = as.character(agencyName), monitorID = as.character(monitorID),  longitude = as.numeric(as.character(longitude)), latitude = as.numeric(as.character(latitude)))
temp.airdata = read.csv('https://haze.airfire.org/bluesky-daily/output/temp_susan/HAQAST/ObservationalDataAnalysis/monitoring_data_csv/2017_airnow.csv', skip=58)
airdata.airnow  = melt(temp.airdata, id.vars = c('datetime')) %>%
	mutate(monitorID = substr(variable, 2, 13)) %>%
	select(-variable) %>%
	inner_join(location.airnow, by = 'monitorID')

# AirSIS
temp.locationsis = read.csv('https://haze.airfire.org/bluesky-daily/output/temp_susan/HAQAST/ObservationalDataAnalysis/monitoring_data_csv/2017_airsis.csv')
location.airsis = filter(temp.locationsis, row_number() <28) %>%
	mutate(monitorID = as.character(monitorID), longitude = as.numeric(as.character(longitude)), latitude = as.numeric(as.character(latitude)), agencyName = as.character(monitorType)) %>%
	select(monitorID, longitude, latitude, agencyName) 
temp.airdata = read.csv('https://haze.airfire.org/bluesky-daily/output/temp_susan/HAQAST/ObservationalDataAnalysis/monitoring_data_csv/2017_airsis.csv', skip=28)
airdata.airsis  = melt(temp.airdata, id.vars = c('datetime')) %>%
	mutate(monitorID = as.character(variable)) %>%
	inner_join(location.airsis, by = 'monitorID') %>%
	select(-variable)

# EPA Data (notte downloaded date maybe too early?, and PM2.5 values in October is misssing)
temp.locationepa = read.csv('https://haze.airfire.org/bluesky-daily/output/temp_susan/HAQAST/ObservationalDataAnalysis/monitoring_data_csv/2017_epa_frm.csv')
location.airepa = filter(temp.locationepa,  row_number() <31) %>%
	mutate(monitorID = as.character(monitorID), longitude = as.numeric(as.character(longitude)), latitude = as.numeric(as.character(latitude)), agencyName = as.character(pwfslDataIngestSource)) %>%
	select(monitorID, longitude, latitude, agencyName) 
temp.airdata = read.csv('https://haze.airfire.org/bluesky-daily/output/temp_susan/HAQAST/ObservationalDataAnalysis/monitoring_data_csv/2017_epa_frm.csv', skip=31)
airdata.airepa  = melt(temp.airdata, id.vars = c('datetime')) %>%
	mutate(monitorID = substr(variable, 2, 13)) %>%
	inner_join(location.airepa, by = 'monitorID') %>%
	select(-variable)

df = rbind(airdata.airnow, airdata.airsis, airdata.airepa) %>%
	mutate(Date = as.Date(as.character(datetime), "%Y-%m-%d")) %>%
	select(Date, monitorID, value, longitude, latitude, agencyName) %>%
	arrange(Date, value, monitorID) %>%
	filter(!is.na(value), Date %in% seq(as.Date("2017-10-01"),as.Date('2017-10-31'), by = '1 day'))

Monitor = df %>% 
	distinct(monitorID, .keep_all = TRUE)