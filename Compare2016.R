x=c('tidyverse', 'readxl', 'reshape2', 'gghighlight')
lapply(x,require,character.only=T)

df = read_excel("C:/Users/ebike/Google Drive/Research/WildFire2017/Preliminary County weighted pm orig.xlsx", sheet = "county weighted pm", skip =2)
newcolnames = c('County', paste0('October', c(3:20)))
names(df) = newcolnames

df2 = filter(df, !is.na(County), !is.na(October3)) %>%
    melt(id.vars = c('County')) %>%
    mutate(Date = as.Date(paste0('2017', variable), format = '%Y%B%d')) %>%
    select(County, Date, PM25 = value) %>%
    arrange(County,  Date ) %>%
    mutate(FireDays = ifelse(Date %in% seq(as.Date("2017-10-09"),as.Date('2017-10-17'), by = '1 day'), 1, 0))


df.2016 = read_excel("C:/Users/ebike/Google Drive/Research/WildFire2017/county population-weighted pm2.5 exposures 2016.xlsx", sheet = "county weighted pm 2016", skip =2)
newcolnames = c('County', paste0('Day', seq(as.Date("2016-01-01"),as.Date('2016-12-31'), by = '1 day')))
names(df.2016) = newcolnames
                
df2.2016 = df.2016 %>% 
    filter(!is.na(County), !is.na(`Day2016-01-01`)) %>% 
    melt(id.vars = c('County')) %>% 
    mutate(Date = as.Date(substr(variable, 4, 13))) %>% 
    filter(Date %in% seq(as.Date("2016-10-09"),as.Date('2016-10-17'), by = '1 day')) %>% 
    mutate(FireDays = 2) %>% 
    select(County, Date, PM25 = value, FireDays)

df3 = rbind(df2, df2.2016) 

df3 %>% 
    group_by(FireDays) %>% 
    summarize(MeanPM25 = mean(PM25), SEPM25 = sd(PM25))

ggplot(df3, aes(x = PM25)) + 
    geom_histogram(aes(x = PM25, fill= factor(FireDays)), binwidth = 5, color = 'black')+
    xlab('PM2.5 Value')+
    ggtitle('Histogram: PM2.5 Value')+
    geom_vline(xintercept = 35)+
    geom_vline(xintercept = 15, lty =2)+
    scale_fill_manual(values=c("light blue", "red", "Green"), name="",labels=c("Non-Fire Days", "Fire Days", "Same Period in 2016"))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text", x = c(22.5, 42.5), y = 30, label = c("NAAQS\n (Annual)", "NAAQS\n (24h)"))

    