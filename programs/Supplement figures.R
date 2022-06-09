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

r0=ggplot(fig3_data, aes(x=pop_cat, y=mean_R0, fill=rural_urban)) +
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