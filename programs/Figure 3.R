##Figure 4: associations between different metrics: Outbreak attack rate, cfr, outbreak size, outbreak duration and outbreak threshold for admin2 outbreaks####
library(dplyr)
library(ggplot2)
library(stringr)
threshold_data1=read.csv("reference_data/outbreak_threshold_data_new_definition1.csv")
threshold_data1$spatial_scale=as.character(threshold_data1$spatial_scale)
threshold_data1[which(threshold_data1$spatial_scale=="First administrative level"),]$spatial_scale="admin1"
threshold_data1[which(threshold_data1$spatial_scale=="Second administrative level"),]$spatial_scale="admin2"
threshold_data1[which(threshold_data1$spatial_scale=="Third administrative level"),]$spatial_scale="admin3"

##time to outbreak peak
peak_data=read.csv('reference_data/outbreak_time_to_peak_new_definition1.csv')
peak_data$duration=peak_data$duration/7
peak_data$total_suspected_cases=round(peak_data$total_suspected_cases,0)
peak_data$total_deaths=round(peak_data$total_deaths,0)
threshold_data2=merge(threshold_data1,
                      peak_data,
                      by=c('location','outbreak_number','start_weekday','country','spatial_scale','temporal_scale','population','total_suspected_cases','total_deaths','duration'))

threshold_data2$pop_cat=ifelse(threshold_data2$population<=10000,"<10,000",
                               ifelse(threshold_data2$population<=100000,"10,000-100,000",
                                      ifelse(threshold_data2$population<=1000000,"100,000-1,000,000",">1,000,000")))
threshold_data2$pop_cat=factor(threshold_data2$pop_cat,
                               levels = c('<10,000',
                                          "10,000-100,000",
                                          "100,000-1,000,000",
                                          ">1,000,000"))

threshold_data2$population_density=threshold_data2$population/threshold_data2$area#area km*km
threshold_data2$rural_urban=NA
threshold_data2$rural_urban=ifelse(threshold_data2$population_density>=1000,'Urban settings','Rural settings')

threshold_data3=threshold_data2
threshold_data3=rbind(threshold_data2,threshold_data2)
threshold_data3[1:(nrow(threshold_data3)/2),]$rural_urban='All settings'
threshold_data3$rural_urban=factor(threshold_data3$rural_urban,
                                   levels = c('All settings',
                                              'Rural settings',
                                              'Urban settings'))

threshold_data3$confirmed_prop=100*threshold_data3$total_confirmed_cases/threshold_data3$total_suspected_cases
threshold_data3$confirmed_attackrate=1000*threshold_data3$total_confirmed_cases/threshold_data3$population
threshold_data3$spatial_scale=ifelse(threshold_data3$spatial_scale=='admin1','First-level administrative units',
                                     ifelse(threshold_data3$spatial_scale=='admin2','Second-level administrative units','Third-level administrative units'))

threshold_data4=threshold_data3[which(threshold_data3$spatial_scale=='Second-level administrative units'&!threshold_data3$rural_urban=='All settings'),]

tr_outbreaksize=ggplot(data=threshold_data4)+
  geom_point(aes(x=threshold,y=total_suspected_cases,color=rural_urban),size=5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  geom_smooth(aes(x=threshold,y=total_suspected_cases))+
  scale_x_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  scale_y_log10(breaks = c(10,100,1000,10000),
                labels = c(10,100,1000,10000))+
  labs(x='Weekly incidence per 100,000 people\n',
       y="Outbreak size",
       title='G'
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_text(size=40,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 40,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 40,face="bold"),
    axis.title.y = ggplot2::element_text(size=40,face="bold"),
    plot.title = ggplot2::element_text(size=40,face="bold"),
    legend.title = ggplot2::element_text( size = 40,face='bold'),
    legend.position = 'none',
    strip.text = element_text(size = 44,face='bold'),
    legend.text = element_text(size = 40)
  )+
  guides(color=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(2, 2, 2, 2, "cm"))
tr_outbreaksize
tr_outbreaksize=ggExtra::ggMarginal(tr_outbreaksize,type="histogram",fill="gray")

tr_ar=ggplot(data=threshold_data4)+
  geom_point(aes(x=threshold,y=attack_rate,color=rural_urban),size=5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  geom_smooth(aes(x=threshold,y=attack_rate))+
  scale_x_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  labs(x='Weekly incidence per 100,000 people\n',
       y="Attack rate per 1,000 people",
       title='B'
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_text(size=40,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 40,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 40,face="bold"),
    axis.title.y = ggplot2::element_text(size=40,face="bold"),
    plot.title = ggplot2::element_text(size=40,face="bold"),
    legend.title = ggplot2::element_text( size = 40,face='bold'),
    legend.position = 'none',
    strip.text = element_text(size = 44,face='bold'),
    legend.text = element_text(size = 40)
  )+
  guides(color=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(2, 2, 2, 2, "cm"))
tr_ar
tr_ar=ggExtra::ggMarginal(tr_ar,type="histogram",fill="gray")

r0_ar=ggplot(data=threshold_data4)+
  geom_point(aes(x=mean_R0,y=attack_rate,color=rural_urban),size=5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  geom_smooth(aes(x=mean_R0,y=attack_rate))+
  scale_x_continuous(breaks = c(1,2,3),
                     labels=c(1,2,3))+
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  labs(y='Attack rate per 1,000 people',
       x='Mean reproductive number during the first epidemic week\n',
       title='C'
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_text(size=40,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 40,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 40,face="bold"),
    axis.title.y = ggplot2::element_text(size=40,face="bold"),
    plot.title = ggplot2::element_text(size=40,face="bold"),
    legend.title = ggplot2::element_text( size = 40,face='bold'),
    legend.position = 'none',
    strip.text = element_text(size = 44,face='bold'),
    legend.text = element_text(size = 40)
  )+
  guides(color=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(2, 2, 2, 2, "cm"))
r0_ar
r0_ar=ggExtra::ggMarginal(r0_ar,type="histogram",fill="gray")

ar_cfr=ggplot(data=threshold_data4)+
  geom_point(aes(y=attack_rate,x=cfr,color=rural_urban),size=5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  geom_smooth(aes(y=attack_rate,x=cfr))+
  scale_x_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+  
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  labs(y='Attack rate per 1,000 people',
       x='CFR (%)',
       title='F'
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_text(size=40,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 40,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 40,face="bold"),
    axis.title.y = ggplot2::element_text(size=40,face="bold"),
    plot.title = ggplot2::element_text(size=40,face="bold"),
    legend.title = ggplot2::element_text( size = 40,face='bold'),
    legend.position = 'none',
    strip.text = element_text(size = 44,face='bold'),
    legend.text = element_text(size = 40)
  )+
  guides(color=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(2, 2, 2, 2, "cm"))
ar_cfr
ar_cfr=ggExtra::ggMarginal(ar_cfr,type="histogram",fill="gray")

ar_duration=ggplot(data=threshold_data4)+
  geom_point(aes(y=attack_rate,x=duration,color=rural_urban),size=5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  geom_smooth(aes(y=attack_rate,x=duration))+
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  labs(y='Attack rate per 1,000 people',
       x='Outbreak Duration (week)',
       title='E'
       #title="Outbreaks reported in regions with population between 100,000-1,000,000"
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_text(size=40,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 40,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 40,face="bold"),
    axis.title.y = ggplot2::element_text(size=40,face="bold"),
    plot.title = ggplot2::element_text(size=40,face="bold"),
    legend.title = ggplot2::element_text( size = 40,face='bold'),
    legend.position = 'none',
    strip.text = element_text(size = 44,face='bold'),
    legend.text = element_text(size = 40)
  )+
  guides(color=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(2, 2, 2, 2, "cm"))
ar_duration
ar_duration=ggExtra::ggMarginal(ar_duration,type="histogram",fill="gray")

ar_peak=ggplot(data=threshold_data4)+
  geom_point(aes(y=attack_rate,x=time_to_peak..weeks.,color=rural_urban),size=5)+
  geom_smooth(aes(y=attack_rate,x=time_to_peak..weeks.))+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  labs(y='Attack rate per 1,000 people',
       x='Time to outbreak peak (week)',
       title='D'
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_text(size=40,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 40,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 40,face="bold"),
    axis.title.y = ggplot2::element_text(size=40,face="bold"),
    plot.title = ggplot2::element_text(size=40,face="bold"),
    legend.title = ggplot2::element_text( size = 40,face='bold'),
    legend.position = 'none',
    strip.text = element_text(size = 44,face='bold'),
    legend.text = element_text(size = 40)
  )+
  guides(color=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(2, 2, 2, 2, "cm"))
ar_peak
ar_peak=ggExtra::ggMarginal(ar_peak,type="histogram",fill="gray")

peak_duration=ggplot(data=threshold_data4)+
  geom_point(aes(x=time_to_peak..weeks.,y=duration,color=rural_urban),size=5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  geom_smooth(aes(x=time_to_peak..weeks.,y=duration))+
  labs(y='Outbreak duration (week)',
       x='Time to outbreak peak (week)\n',
       title='A'
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_text(size=40,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 40,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 40,face="bold"),
    axis.title.y = ggplot2::element_text(size=40,face="bold"),
    plot.title = ggplot2::element_text(size=40,face="bold"),
    legend.title = ggplot2::element_text( size = 40,face='bold'),
    legend.position = 'none',
    strip.text = element_text(size = 44,face='bold'),
    legend.text = element_text(size = 40)
  )+
  guides(color=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(2, 2, 2, 2, "cm"))
peak_duration
peak_duration=ggExtra::ggMarginal(peak_duration,type="histogram",fill="gray")

legend=cowplot::get_legend(ggplot(data=threshold_data4)+
                             geom_point(aes(x=time_to_peak..weeks.,y=duration,color=rural_urban),size=5)+
                             scale_color_manual(values=c("#E69F00", "#56B4E9"))+
                             geom_smooth(aes(x=time_to_peak..weeks.,y=duration))+
                             ggplot2::theme(
                               legend.title = ggplot2::element_text( size = 40,face='bold'),
                               legend.position = "bottom",
                               strip.text = element_text(size = 44,face='bold'),
                               legend.text = element_text(size = 40))+
                             guides(color=guide_legend(title="Rural or urban settings")))

jpeg("reference_data/figure 4 admin2 with distribution.jpg",width=4000,height=2000,quality = 10000)
gridExtra::grid.arrange(gridExtra::arrangeGrob(peak_duration,tr_ar,r0_ar,ar_peak,ar_duration,ar_cfr,nrow = 2,ncol=3),legend,nrow=2,heights=c(5,1))
dev.off()