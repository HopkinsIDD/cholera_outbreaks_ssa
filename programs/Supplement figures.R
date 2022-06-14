## Supplement figures
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

#Supplement figures 3-13
tr=ggplot(fig3_data, aes(x=spatial_scale, y=threshold)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nOutbreak threshold per 100,000 people',
    x=x_title)+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
tr

ar=ggplot(fig3_data, aes(x=spatial_scale, y=attack_rate)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nAttack rate per 1,000 people',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
ar

death=ggplot(fig3_data, aes(x=spatial_scale, y=total_deaths)) +
  scale_y_log10(breaks = c(1,10,100),
                labels = c(1,10,100))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nNumber of cholera-associated deaths',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
death

cfr=ggplot(fig3_data, aes(x=spatial_scale, y=cfr)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10),
                labels = c(0.01,0.1,1,10))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nCFR (%)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
cfr

r0=ggplot(fig3_data, aes(x=spatial_scale, y=mean_R0)) +
  scale_y_log10(breaks = c(1,1.5,2,2.5,3),
                labels=c(1,1.5,2,2.5,3))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\n\n Average reproductive number \n during the first epidemic week',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
r0


cases=ggplot(fig3_data, aes(x=spatial_scale, y=total_suspected_cases)) +
  scale_y_log10(breaks = c(10,100,1000,10000),
                labels= c(10,100,1000,10000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0))+
  theme()+
  ggplot2::labs(
    y='\nNumber of suspected cases',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
cases

duration=ggplot(fig3_data, aes(x=spatial_scale, y=duration)) +
  scale_y_log10(breaks = c(4,8,16,32,64,128),
                labels = c(4,8,16,32,64,128))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nOutbreak duration(week)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
duration

peak=ggplot(fig3_data, aes(x=spatial_scale, y=time_to_peak..weeks.)) +
  scale_y_log10(breaks = c(4,8,16,32),
                labels =  c(4,8,16,32))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nTime to outbreak peak (week)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
peak

#Supplement figures 14-25
cat='spatial_scale'#pop_cat/spatial_scale
if(cat=='pop_cat'){
  x_title='\nPopulation'
}else if(cat=='spatial_scale'){
  x_title='\nAdministrative units'
} 

tr=ggplot(fig3_data, aes(x=spatial_scale, y=threshold, fill=rural_urban)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nOutbreak threshold per 100,000 people',
    x=x_title)+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
tr

ar=ggplot(fig3_data, aes(x=spatial_scale, y=attack_rate, fill=rural_urban)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nAttack rate per 1,000 people',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
ar

death=ggplot(fig3_data, aes(x=spatial_scale, y=total_deaths, fill=rural_urban)) +
  scale_y_log10(breaks = c(1,10,100),
                labels = c(1,10,100))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nNumber of cholera-associated deaths',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
death

cfr=ggplot(fig3_data, aes(x=spatial_scale, y=cfr, fill=rural_urban)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10),
                labels = c(0.01,0.1,1,10))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nCFR (%)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
cfr

r0=ggplot(fig3_data, aes(x=spatial_scale, y=mean_R0, fill=rural_urban)) +
  scale_y_log10(breaks = c(1,1.5,2,2.5,3),
                labels=c(1,1.5,2,2.5,3))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\n\n Average reproductive number \n during the first epidemic week',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
r0


cases=ggplot(fig3_data, aes(x=spatial_scale, y=total_suspected_cases, fill=rural_urban)) +
  scale_y_log10(breaks = c(10,100,1000,10000),
                labels= c(10,100,1000,10000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nNumber of suspected cases',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
cases

duration=ggplot(fig3_data, aes(x=spatial_scale, y=duration , fill=rural_urban)) +
  scale_y_log10(breaks = c(4,8,16,32,64,128),
                labels = c(4,8,16,32,64,128))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nOutbreak duration(week)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
duration

peak=ggplot(fig3_data, aes(x=spatial_scale, y=time_to_peak..weeks. , fill=rural_urban)) +
  scale_y_log10(breaks = c(4,8,16,32),
                labels =  c(4,8,16,32))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nTime to outbreak peak (week)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
peak


#Supplement figures 27-38
cat='pop_cat'#pop_cat/spatial_scale
if(cat=='pop_cat'){
  x_title='\nPopulation'
}else if(cat=='spatial_scale'){
  x_title='\nAdministrative units'
} 

tr=ggplot(fig3_data, aes(x=pop_cat, y=threshold)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nOutbreak threshold per 100,000 people',
    x=x_title)+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
tr

ar=ggplot(fig3_data, aes(x=pop_cat, y=attack_rate)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nAttack rate per 1,000 people',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
ar

death=ggplot(fig3_data, aes(x=pop_cat, y=total_deaths)) +
  scale_y_log10(breaks = c(1,10,100),
                labels = c(1,10,100))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nNumber of cholera-associated deaths',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
death

cfr=ggplot(fig3_data, aes(x=pop_cat, y=cfr)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10),
                labels = c(0.01,0.1,1,10))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nCFR (%)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
cfr

r0=ggplot(fig3_data, aes(x=pop_cat, y=mean_R0)) +
  scale_y_log10(breaks = c(1,1.5,2,2.5,3),
                labels=c(1,1.5,2,2.5,3))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\n\n Average reproductive number \n during the first epidemic week',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
r0


cases=ggplot(fig3_data, aes(x=pop_cat, y=total_suspected_cases)) +
  scale_y_log10(breaks = c(10,100,1000,10000),
                labels= c(10,100,1000,10000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0))+
  theme()+
  ggplot2::labs(
    y='\nNumber of suspected cases',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
cases

duration=ggplot(fig3_data, aes(x=pop_cat, y=duration)) +
  scale_y_log10(breaks = c(4,8,16,32,64,128),
                labels = c(4,8,16,32,64,128))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nOutbreak duration(week)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
duration

peak=ggplot(fig3_data, aes(x=pop_cat, y=time_to_peak..weeks.)) +
  scale_y_log10(breaks = c(4,8,16,32),
                labels =  c(4,8,16,32))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point()+
  theme()+
  ggplot2::labs(
    y='\nTime to outbreak peak (week)',
    x=x_title
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
peak


## Supplement figures 39-74
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
main_outbreak<-fig3_data
main_outbreak$definition='new_definition'

sensitivity_outbreak<-read.csv("reference_data/outbreak_data_sensitivity_analysis.csv")
sensitivity_outbreak1=sensitivity_outbreak
sensitivity_outbreak1$rural_urban='All settings'
sensitivity_outbreak=rbind(sensitivity_outbreak,sensitivity_outbreak1)

sensitivity_outbreak$spatial_scale=
  factor(sensitivity_outbreak$spatial_scale,levels=c('admin1','admin2','admin3','admin4 or lower'))
levels(sensitivity_outbreak$spatial_scale)=c(
  'First-level administrative units',
  'Second-level administrative units',
  'Third-level administrative units',
  'Forth-level administrative units')

main_outbreak=main_outbreak[,colnames(main_outbreak)%in%colnames(sensitivity_outbreak)]
main_outbreak$location_period_id=as.character(main_outbreak$location_period_id)
outbreak=bind_rows(main_outbreak,sensitivity_outbreak)

outbreak$definition=ifelse(
  outbreak$definition=='old_definition',
  'Average weekly cases \nduring the first three epidemic weeks',
  'Average weekly incidence \nper 100,000 people'
)
outbreak$confirmed_prop=100*outbreak$total_confirmed_cases/outbreak$total_suspected_cases
outbreak$confirmed_attackrate=1000*outbreak$total_confirmed_cases/outbreak$population

#subset the admin units
outbreak=outbreak[which(outbreak$spatial_scale=="Third-level administrative units"),]

#distributions of all coviariates
tr=ggplot(outbreak, aes(x=definition, y=threshold, fill=rural_urban)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nOutbreak threshold per 100,000 people',
    x="\nOutbreak thresholds"
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
tr

ar=ggplot(outbreak, aes(x=definition, y=attack_rate, fill=rural_urban)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10,100,1000),
                labels = c(0.01,0.1,1,10,100,1000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nAttack rate per 1,000 people',
    x="\nOutbreak thresholds"
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
ar

cfr=ggplot(outbreak, aes(x=definition, y=cfr, fill=rural_urban)) +
  scale_y_log10(breaks = c(0.01,0.1,1,10),
                labels = c(0.01,0.1,1,10))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nCFR (%)',
    x="\nOutbreak thresholds"
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))

cfr

r0=ggplot(outbreak, aes(x=definition, y=mean_R0, fill=rural_urban)) +
  scale_y_log10(breaks = c(1,1.5,2,2.5,3),
                labels=c(1,1.5,2,2.5,3))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\n\n Average reproductive number \n during the first epidemic week',
    x="\nOutbreak thresholds"
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
r0

cases=ggplot(outbreak, aes(x=definition, y=total_suspected_cases, fill=rural_urban)) +
  scale_y_log10(breaks = c(10,100,1000,10000),
                labels= c(10,100,1000,10000))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nNumber of suspected cases',
    x="\nOutbreak thresholds"
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))

cases

duration=ggplot(outbreak, aes(x=definition, y=duration , fill=rural_urban)) +
  scale_y_log10(breaks = c(4,8,16,32,64),
                labels =  c(4,8,16,32,64))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nOutbreak duration(week)',
    x="\nOutbreak thresholds"
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))

duration

peak=ggplot(outbreak, aes(x=definition, y=time_to_peak..weeks. , fill=rural_urban)) +
  scale_y_log10(breaks = c(4,8,16,32,64),
                labels = c(4,8,16,32,64))+
  geom_violin(width=0.6,position=position_dodge(0.7))+
  geom_point(position = position_jitterdodge(jitter.width=0,dodge.width=0.7))+
  scale_fill_manual(values=c( "#56B4E9","#999999", "#E69F00"))+
  theme()+
  ggplot2::labs(
    y='\nTime to outbreak peak (week)',
    x="\nOutbreak thresholds"
  )+
  theme_classic()+
  ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "transparent"),
    panel.grid.major.x = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.x = ggplot2::element_blank(), 
    panel.grid.major.y = ggplot2::element_blank(), # get rid of major grid
    panel.grid.minor.y = ggplot2::element_blank(), 
    axis.title.x =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.x =   ggplot2::element_text(size = 18,face="bold"),
    axis.title.y =   ggplot2::element_text(size = 22,face="bold"),
    axis.text.y =   ggplot2::element_text(size = 18,face="bold"),
    plot.title =   ggplot2::element_text(size = 22,face="bold"),
    legend.title = ggplot2::element_text( size=18,face='bold'),
    strip.text = element_text(size=22,face='bold'),
    legend.text = element_text(size=18)
  )+
  guides(fill=guide_legend(title="Rural or urban settings"))+
  ggplot2::theme(plot.margin = margin(1, 1, 1, 1, "cm"))
peak

##Supplement figure 5
dup_outbreak_data<-read.csv("reference_data/Supplement_figure_75_77.csv")
dup_outbreak_data$TL<-as.Date(dup_outbreak_data$TL,origin="1996-01-01")
dup_outbreak_data$TR<-as.Date(dup_outbreak_data$TR,origin="1996-01-01")

selected_dup_outbreak_data=dup_outbreak_data[which(dup_outbreak_data$dup_id==66),]
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::tigray::northwesterntigray"),]$location='Northwestern tigray in tigray region in Ethopia'
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::tigray::northwesterntigray::asgedetsimbila"),]$location='Asgede Tsimbla in North Western tigray in tigray region in Ethopia'
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::tigray::northwesterntigray::laelayadiyabo"),]$location="Laelay Adiyabo in North Western tigray in tigray region in Ethopia"
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::tigray::northwesterntigray::tahitayadiyabo"),]$location="Tahitay Adiyabo in North Western tigray in tigray region in Ethopia"
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::tigray::northwesterntigray::tahitaykoraro"),]$location="Tahitay Koraro in North Western tigray in tigray region in Ethopia"
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::tigray::northwesterntigray::tselemti"),]$location="Tselemti in North Western tigray in tigray region in Ethopia"

selected_dup_outbreak_data=dup_outbreak_data[which(dup_outbreak_data$dup_id==74),]
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::amhara::southgondar"),]$location='South Gondar in Amhara region in Ethopia'
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::amhara::southgondar::dera"),]$location='Dera in South Gondar in Amhara region in Ethopia'
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::eth::amhara::southgondar::semada"),]$location="Semada in South Gondar in Amhara region in Ethopia"

selected_dup_outbreak_data=dup_outbreak_data[which(dup_outbreak_data$dup_id==12),]
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::cod::nordkivu::karisimbihealthdistrict"),]$location='Karisimbi Health District in Nord-Kivu region in Democratic Republic of Congo'
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::cod::nordkivu::karisimbihealthdistrict::katoyi"),]$location='Katoyi in Karisimbi Health District in Nord-Kivu region in Democratic Republic of Congo'
selected_dup_outbreak_data[which(selected_dup_outbreak_data$location=="afr::cod::nordkivu::karisimbihealthdistrict::majengo"),]$location="Majengo in Karisimbi Health District in Nord-Kivu region in Democratic Republic of Congo"

for (id in unique(selected_dup_outbreak_data[which(!selected_dup_outbreak_data$dup_id%in%selected_dup_outbreak_data[which(selected_dup_outbreak_data$spatial_scale=='admin1'),]$dup_id),]$dup_id)) {
  single_selected_dup_outbreak_data=selected_dup_outbreak_data[selected_dup_outbreak_data$dup_id==id,]
  single_selected_dup_outbreak_data$location=as.factor(single_selected_dup_outbreak_data$location)
  density_ridge_data=as.data.frame(lapply(single_selected_dup_outbreak_data, rep, single_selected_dup_outbreak_data$sCh))
  
  if(unique(single_selected_dup_outbreak_data$temporal_scale=='daily')){
    ytitle='Reported suspected cases per day'
    bar_length=1
  }else{
    ytitle='Reported suspected cases per week'
    bar_length=7
  }
  
  date_limits=c(min(single_selected_dup_outbreak_data$TL),max(single_selected_dup_outbreak_data$TL))
  
  y_limits=c(0,max(single_selected_dup_outbreak_data$sCh))
  y_breaks=round(seq(y_limits[1],y_limits[2],length=5),0)
  
  figure1=
    ggplot2::ggplot() + 
    ggplot2::geom_bar(data=single_selected_dup_outbreak_data[which(single_selected_dup_outbreak_data$spatial_scale=='admin2'),],ggplot2::aes(x=TL,y=sCh,fill=location),width=bar_length,stat="identity", colour='black')+
    scale_x_date(date_labels = "%Y-%m",limits = date_limits,breaks= "1 month")+
    scale_y_continuous(limits=y_limits,breaks=y_breaks)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.key = ggplot2::element_blank(), 
                   legend.background = ggplot2::element_rect(colour = 'black', fill = 'white'), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_blank(),
                   legend.text=element_text(size=30),
                   plot.title=element_text(size=60),
                   axis.title = element_text(size=40),
                   axis.text = element_text(size=40))+
    labs(title="Second-level administrative unit outbreak",
         y=ytitle,x='Time')
  
  
  figure2=
    ggplot2::ggplot() + 
    ggplot2::geom_bar(data=single_selected_dup_outbreak_data[which(single_selected_dup_outbreak_data$spatial_scale=='admin3'),],ggplot2::aes(x=TL,y=sCh,width=date_range,fill=location),stat="identity", colour='black')+
    scale_x_date(date_labels = "%Y-%m",limits = date_limits,breaks= "1 month")+
    scale_y_continuous(limits = y_limits,breaks=y_breaks)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.key = ggplot2::element_blank(), 
                   legend.background = ggplot2::element_rect(colour = 'black', fill = 'white'), 
                   legend.position = "bottom", 
                   legend.title = ggplot2::element_blank(),
                   legend.text=element_text(size=30),
                   plot.title=element_text(size=60),
                   axis.title = element_text(size=40),
                   axis.text = element_text(size=40))+
    labs(title="Third-level administrative unit outbreak",
         y=ytitle,x='Time')+
    guides(fill=guide_legend(nrow=2,byrow=T))
  gridExtra::grid.arrange(figure1,figure2,ncol=1)
}
