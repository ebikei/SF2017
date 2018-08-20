x=c('tidyverse', 'readxl', 'reshape2')
lapply(x,require,character.only=T)

df = read_excel("C:/Users/ebike/Google Drive/Research/WildFire2017/Preliminary hourly pm2.5 2017 by site.xlsx", 
                sheet = 'hourly pm2.5 (ug per m3)', skip =1, 
                col_types = c("date", rep('numeric', 61)))
oldnames = names(df)
newcolnames = c('Date', 'Hour', paste0('Monitor', oldnames[3:62]))
names(df) = newcolnames
df$Date = as.Date(df$Date)
df2 = df %>% 
    melt(id.vars = c('Date', 'Hour')) %>% 
    rename(Monitor = variable, PM25 = value ) %>% 
    filter(!is.na(PM25)) %>% 
    arrange(Monitor, Date, Hour) %>% 
    filter(Date %in% seq(as.Date("2017-10-03"),as.Date('2017-10-20'), by = '1 day')) %>% 
    mutate(FireDays = ifelse(Date %in% seq(as.Date("2017-10-09"),as.Date('2017-10-17'), by = '1 day'), 1, 0))



ggplot(df2, aes(x = PM25)) + 
    geom_histogram(aes(x = PM25, fill= factor(FireDays)), binwidth = 5, color = 'black')+
    xlab('PM2.5 Value')+
    ggtitle('Histogram: PM2.5 Value')+
    geom_vline(xintercept = 35)+
    geom_vline(xintercept = 15, lty =2)+
    scale_fill_manual(values=c("light blue", "red"), name="",labels=c("Non-Fire Days", "Fire Days"))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text", x = c(22.5, 42.5), y = 30, label = c("NAAQS\n (Annual)", "NAAQS\n (24h)"))


df3 = df2 %>% 
    filter(PM25 >100) 
table(df3$Monitor) %>% 
    data.frame() %>% 
    arrange(Freq)


df2 %>% 
    select(Monitor == 'Monitor06-055-0003') %>% 
    ggplot()