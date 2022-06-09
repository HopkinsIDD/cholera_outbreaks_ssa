##Figure 3: associations between different metrics: Outbreak attack rate, cfr, outbreak size, outbreak duration and outbreak threshold for admin2 outbreaks####
library(dplyr)
library(ggplot2)
library(stringr)
outbreak_data=read.csv("reference_data/outbreak_data.csv")

fig3_data=rbind(outbreak_data,outbreak_data)
fig3_data[1:(nrow(fig3_data)/2),]$rural_urban='All settings'
fig3_data$rural_urban=factor(fig3_data$rural_urban,
                             levels = c('All settings',
                                        'Rural settings',
                                        'Urban settings'))

fig3_data$confirmed_prop=100*fig3_data$total_confirmed_cases/fig3_data$total_suspected_cases
fig3_data$confirmed_attackrate=1000*fig3_data$total_confirmed_cases/fig3_data$population
fig3_data$spatial_scale=ifelse(fig3_data$spatial_scale=='admin1','First-level administrative units',
                                     ifelse(fig3_data$spatial_scale=='admin2','Second-level administrative units','Third-level administrative units'))

fig3_data=fig3_data[which(fig3_data$spatial_scale=='Second-level administrative units'&!fig3_data$rural_urban=='All settings'),]

tr_outbreaksize=ggplot(data=fig3_data)+
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

tr_ar=ggplot(data=fig3_data)+
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

r0_ar=ggplot(data=fig3_data)+
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

ar_cfr=ggplot(data=fig3_data)+
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

ar_duration=ggplot(data=fig3_data)+
  geom_point(aes(y=attack_rate,x=duration,color=rural_urban),size=5)+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  geom_smooth(aes(y=attack_rate,x=duration))+
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  labs(y='Attack rate per 1,000 people',
       x='Outbreak Duration (week)',
       title='E'
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

ar_peak=ggplot(data=fig3_data)+
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

peak_duration=ggplot(data=fig3_data)+
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

legend=cowplot::get_legend(ggplot(data=fig3_data)+
                             geom_point(aes(x=time_to_peak..weeks.,y=duration,color=rural_urban),size=5)+
                             scale_color_manual(values=c("#E69F00", "#56B4E9"))+
                             geom_smooth(aes(x=time_to_peak..weeks.,y=duration))+
                             ggplot2::theme(
                               legend.title = ggplot2::element_text( size = 40,face='bold'),
                               legend.position = "bottom",
                               strip.text = element_text(size = 44,face='bold'),
                               legend.text = element_text(size = 40))+
                             guides(color=guide_legend(title="Rural or urban settings")))

jpeg("reference_data/figure 3 admin2 with distribution.jpg",width=4000,height=2000,quality = 10000)
gridExtra::grid.arrange(gridExtra::arrangeGrob(peak_duration,tr_ar,r0_ar,ar_peak,ar_duration,ar_cfr,nrow = 2,ncol=3),legend,nrow=2,heights=c(5,1))
dev.off()