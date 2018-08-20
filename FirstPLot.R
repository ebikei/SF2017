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

ggplot(df2, aes(x = PM25 )) + 
    geom_histogram(binwidth = 5, fill="light blue", color = 'black')+
    xlab('PM2.5 Value')+
    ggtitle('Histogram: PM2.5 Value')+
    geom_vline(xintercept = 35)+
    geom_vline(xintercept = 15, lty =2)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))+
    annotate("text", x = c(21, 41.5), y = 30, label = c("NAAQS\n (Annual)", "NAAQS\n (24h)"))

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

ggplot(df2, aes(x = Date, y = PM25, color = County)) +
    geom_line(lwd=1.2)+
    geom_vline(xintercept = c(as.Date("2017-10-09"), as.Date("2017-10-17")), color = 'red', lwd = 0.8, lty =3)+
    geom_hline(yintercept = 35)+
    geom_hline(yintercept = 15, lty =2)+
    ggtitle('PM2.5 Value Trend by County')+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_date(date_labels="%b%d",date_breaks  ="1 day")+
    annotate("text", x = as.Date("2017-10-04"), y = c(20.5, 38.5), label = c("NAAQS (Annual)", "NAAQS (24h)"))

ggplot(df2) +
    geom_line(aes(x = Date, y = PM25, color = County)) +
    gghighlight(max(PM25) > 75)+
    geom_hline(yintercept = 35)+
    theme_bw()

ggplot(df2, aes(x = Date, y = PM25)) +
    geom_line(lwd=1)+
    geom_rect(data=df2,aes(xmin=as.Date("2017-10-09"), xmax=as.Date("2017-10-17"), ymin = -Inf, ymax = Inf,fill= 'Oct 9th - 17th'), alpha=0.02)+
    facet_wrap(~County)+
    geom_hline(yintercept = 35)+
    geom_hline(yintercept = 15, lty =2)+
    theme_bw()+
    scale_x_date(date_labels="%b%d",date_breaks  ="3 day")+
    theme(strip.text = element_text(colour = 'black'))+
    scale_fill_manual('Wildfire Period',  values = 'Pink',  guide = guide_legend(override.aes = list(alpha = 0.99))) 
