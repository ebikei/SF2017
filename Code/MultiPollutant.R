x<-c("tidyverse")
lapply(x, require, character.only=T)

setwd('F:\\Research\\AirTrend\\TrendGIS\\Data')

load("CO_Data_20171026.RData") #CO_AQS
load("NO2_Data_20171026.RData") #NO2_AQS
load("O3_Data_20160120.RData") #O3_AQS
load("PM10_Data_20160120.RData") #PM10_AQS
load("PM25_Data_20160120.RData") #PM25_AQS
load("SO2_Data_20160120.RData") #SO2_AQS

countieslist = c('001', '013', '041','055', '075', '081','085', '095', '097')
CO = filter(CO_AQS, substr(FIPSPOC, 1, 2) == '06', year(Date) == 2017, month(Date) == 10, substr(FIPSPOC, 3, 5) %in% countieslist)
NO2 = filter(NO2_AQS, substr(FIPSPOC, 1, 2) == '06', year(Date) == 2017, month(Date) == 10, substr(FIPSPOC, 3, 5) %in% countieslist)
O3 = filter(O3_AQS, substr(FIPSPOC, 1, 2) == '06', year(Date) == 2017, month(Date) == 10, substr(FIPSPOC, 3, 5) %in% countieslist)
SO2 = filter(SO2_AQS, substr(FIPSPOC, 1, 2) == '06', year(Date) == 2017, month(Date) == 10, substr(FIPSPOC, 3, 5) %in% countieslist)
PM10 = filter(PM10_AQS, substr(FIPSPOC, 1, 2) == '06', year(Date) == 2017, month(Date) == 10, substr(FIPSPOC, 3, 5) %in% countieslist)
PM25 = filter(PM25_AQS, substr(FIPSPOC, 1, 2) == '06', year(Date) == 2017, month(Date) == 10, substr(FIPSPOC, 3, 5) %in% countieslist)

df = full_join(CO, NO2, by = c('Date', 'FIPSPOC')) %>%
	full_join(O3, by = c('Date', 'FIPSPOC')) %>%
	full_join(SO2, by = c('Date', 'FIPSPOC')) %>%
	full_join(PM10, by = c('Date', 'FIPSPOC')) %>%
	full_join(PM25, by = c('Date', 'FIPSPOC')) %>%
	mutate(PMC_Value = PM10_Value - PM25_Value)

df2 = gather(df, Pollutant, Value, CO_Value:PMC_Value) %>%
	mutate(FIPS = substr(FIPSPOC, 1, 9)) %>%
	group_by(FIPS, Date, Pollutant) %>%
	summarize(Value = mean(Value, na.rm = TRUE)) %>%
	ungroup() %>%
	filter(!is.na(Value)) %>%
	arrange(FIPS, Pollutant, Date)


filter(df2, Pollutant == 'PMC_Value', !is.na(Value)) %>%
	ggplot(aes(x = Date, y = Value, color = FIPS)) + 
	    geom_line(lwd = 1.2)+
	    geom_point() +
        geom_vline(xintercept = c(as.Date("2017-10-01"), as.Date("2017-10-31")), color = 'red', lwd = 0.8, lty =3) +
	    xlab('Date')+
	    ggtitle('Value Trend')+
#	    geom_vline(xintercept = 35)+
#	    geom_vline(xintercept = 15, lty =2)+
	    theme_bw()+
	    theme(plot.title = element_text(hjust = 0.5))
#	    annotate("text", x = c(21, 41.5), y = 30, label = c("NAAQS\n (Annual)", "NAAQS\n (24h)"))


