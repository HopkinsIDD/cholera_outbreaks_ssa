##Figure 3: heatmap####
library(ggplot2)
library(tidyverse)

significant_outbreaks=subset(read.csv('reference_data/outbreak_time_to_peak_new_definition1.csv',encoding="UTF-8"),EpiPeriod=='complete' &!spatial_scale=="country")

heatmap_data=data.frame(
  country=sort(rep(sort(unique(significant_outbreaks$country)),12*(2020-2010+1))),
  year=rep(2010:2020,12*length(unique(significant_outbreaks$country))),
  month=ifelse(rep(1:12,length(unique(significant_outbreaks$country))*(2020-2010+1))>9,
               rep(1:12,length(unique(significant_outbreaks$country))*(2020-2010+1)),
               paste0('0',rep(1:12,length(unique(significant_outbreaks$country))*(2020-2010+1)))),
  day='01',
  has_data='no',
  outbreak_sCh=0,
  outbreak_deaths=NA,
  outbreak_cCh=NA,
  outbreak_duration=0,
  total_pop=0,
  outbreak_pop=0,
  outbreak_pop_censored=0,
  outbreak_number=0,
  outbreak_sl=NA
)

heatmap_data=tidyr::unite(heatmap_data,
                          'year_month',
                          year:day,
                          sep='-')
heatmap_data$year_month=format(lubridate::ymd(heatmap_data$year_month),'%Y-%m')

heatmap_data=read.csv('reference_data/heatmap_data_new_definition1.csv')
heatmap_data$year=as.integer(substr(heatmap_data$year_month,1,4))
heatmap_data=heatmap_data[which(
  heatmap_data$year<=2019|
    (heatmap_data$year==2020 & heatmap_data$year_month=='2020-01')
),]

heatmap_data$has_data=ifelse(heatmap_data$outbreak_sCh>0,'outbreak time period',heatmap_data$has_data)

heatmap_data$has_data=ifelse(heatmap_data$has_data=='no','no data',
                             ifelse(heatmap_data$has_data=='yes','non-outbreak time period',heatmap_data$has_data))
heatmap_data$has_data=factor(heatmap_data$has_data,
                             levels=c('no data',
                                      'non-outbreak time period',
                                      'outbreak time period'))
location_period_pop=read.csv('reference_data/country_location_period_pop.csv')

##match the country-level population to the heatmap_data
heatmap_data$year=lubridate::year(as.Date(paste0(heatmap_data$year_month,'-01')))
heatmap_data1=merge(heatmap_data,location_period_pop,by=c('country','year'),all.x = T)

nrow(heatmap_data)==nrow(heatmap_data1)

heatmap_data1$outbreak_population_proportion=100*heatmap_data1$outbreak_pop/heatmap_data1$population
heatmap_data1$outbreak_population_proportion_censored=100*heatmap_data1$outbreak_pop_censored/heatmap_data1$population

heatmap_data1[which(heatmap_data1$has_data=='no data'),]$outbreak_population_proportion=NA

heatmap_data1[which(heatmap_data1$has_data=='outbreak time period'&
                      heatmap_data1$outbreak_population_proportion==0),]$outbreak_population_proportion=NA

heatmap_data1[which(heatmap_data1$has_data=='non-outbreak time period'&
                      heatmap_data1$outbreak_population_proportion==0),]$outbreak_population_proportion=NA

heatmap_data1$`Missing Shapefiles`=NA
heatmap_data1[which(heatmap_data1$has_data=='outbreak time period'&
                      is.na(heatmap_data1$outbreak_population_proportion)),]$`Missing Shapefiles`='Yes'
heatmap_data1[which(heatmap_data1$has_data=='outbreak time period'&
                      is.na(heatmap_data1$outbreak_population_proportion)==F),]$`Missing Shapefiles`='No'

heatmap_data1$country=dplyr::case_when(
  heatmap_data1$country=='AGO'~'Angola',
  heatmap_data1$country=="BDI"~'Burundi',
  heatmap_data1$country=="BEN"~'Benin',
  heatmap_data1$country=="CAF"~'Central African Republic',
  heatmap_data1$country=="CIV"~'Ivory Coast',
  heatmap_data1$country=="CMR"~'Cameroon',
  heatmap_data1$country=="COD"~'Democratic Republic of the Congo',
  heatmap_data1$country=="COG"~'Republic of the Congo',
  heatmap_data1$country=="ETH"~'Ethiopia',
  heatmap_data1$country=="GHA"~'Ghana',
  heatmap_data1$country=="GIN"~'Guinea',
  heatmap_data1$country=="GNB"~'Guinea-Bissau',
  heatmap_data1$country=="KEN"~'Kenya',
  heatmap_data1$country=="LBR"~'Liberia',
  heatmap_data1$country=="MLI"~'Mali',
  heatmap_data1$country=="MOZ"~'Mozambique',
  heatmap_data1$country=="MWI"~'Malawi',
  heatmap_data1$country=="NAM"~'Namibia',
  heatmap_data1$country=="NER"~'Niger',
  heatmap_data1$country=="NGA"~'Nigeria',
  heatmap_data1$country=="SDN"~'Sudan',
  heatmap_data1$country=="SLE"~'Sierra Leone',
  heatmap_data1$country=="SOM"~'Somalia',
  heatmap_data1$country=="SSD"~'South Sudan',
  heatmap_data1$country=="TCD"~'Chad',
  heatmap_data1$country=="TGO"~'Togo',
  heatmap_data1$country=="TZA_mainland"~'Tanzania Mainland',
  heatmap_data1$country=="TZA_zanzibar"~'Tanzania Zanzibar',
  heatmap_data1$country=="TZA"~'Tanzania',
  heatmap_data1$country=="UGA"~'Uganda',
  heatmap_data1$country=="ZMB"~'Zambia',
  heatmap_data1$country=="ZWE"~'Zimbabwe'
)

dates=seq(as.Date('2010-01-01'),as.Date('2020-01-31'),by='12 months')
heatmap_data1$new_year_month=as.Date(paste0(
  heatmap_data1$year_month,'-01'
))

#add annual rectangular
heatmap_data1$group=1

add_rec=data.frame()
for (country in unique(heatmap_data1$country)) {
  tmp_add_rec=heatmap_data1[which(heatmap_data1$country==country),][1:11,]
  tmp_add_rec$new_year_month=dates+165
  tmp_add_rec$group=2
  add_rec=rbind(add_rec,tmp_add_rec)
}

heatmap_data1=rbind(heatmap_data1,add_rec)
heatmap_data1=heatmap_data1[with(heatmap_data1,order(heatmap_data1$outbreak_pop_censored)),]

heatmap_data2=heatmap_data1%>%
  group_by(country)%>%
  summarise(
    total_coverage=length(outbreak_population_proportion_censored[is.na(outbreak_population_proportion_censored)==F]),
    total_outbreak=length(outbreak_population_proportion_censored[is.na(outbreak_population_proportion_censored)==F&outbreak_population_proportion>0])
    
  )
country_list=unique(heatmap_data2[with(heatmap_data2,order(heatmap_data2$total_outbreak,decreasing = T)),]$country)

ggplot(data=heatmap_data1[heatmap_data1$group==1,],aes(x=new_year_month, y=country)) +
  geom_tile(aes(fill=outbreak_population_proportion_censored,
                color=outbreak_population_proportion_censored)
  )+
  ggplot2::scale_fill_continuous(trans='log10',
                                 high="#56B1F7", low="#132B43",
                                 name='Proportion of population \nliving in regions with outbreaks (%)',
                                 na.value="white"
  )+
  ggplot2::scale_color_continuous(trans='log10',
                                  high="#56B1F7", low="#132B43",
                                  na.value="white",
                                  guide=FALSE
  )+
  ggplot2::geom_tile(data=heatmap_data1[which(heatmap_data1$group==1 &
                                                (heatmap_data1$has_data=='non-outbreak time period'|
                                                   (heatmap_data1$has_data=='outbreak time period'& heatmap_data1$outbreak_population_proportion_censored==0)|
                                                   (heatmap_data1$has_data=='outbreak time period'& is.na(heatmap_data1$outbreak_population_proportion_censored)))
  ),],aes(x=new_year_month, y=country),fill='grey',color='grey'
  )+
  geom_tile(
    data=heatmap_data1[heatmap_data1$group==2,],
    aes(x=new_year_month, y=country),alpha=0,color='black'
  )+
  ggplot2::theme(plot.title =  ggplot2::element_text(hjust = 0.5))+
  ggplot2::theme(panel.grid.major =  ggplot2::element_blank(), 
                 panel.background =  ggplot2::element_blank()
  )+
  ggplot2::labs(
    y='',
    x='')+
  theme(
    plot.title = element_text( size=18, face="bold.italic"),
    axis.title.x = element_text( size=18, face="bold"),
    axis.title.y = element_text(size=24, face="bold"),
    axis.text.x = element_text(size=20,face="bold"),
    axis.text.y=element_text(size=24,face="bold"),
    legend.text = element_text(size=16,face="bold"),
    legend.title = element_text(size=18, face='bold'),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.width = unit(1, "cm")
  )+
  guides(fill = guide_colourbar(
    title.vjust=3,
    ticks.colour = "black",
    frame.colour = "black",
    reverse=FALSE,
    order = 1)
  )+
  guides(shape=guide_legend(
    title.vjust = 3,
    order = 2)
  )+
  scale_x_continuous(expand = c(0,0),
                     breaks= as.Date(dates),
                     labels=format(seq(as.Date('2010-01-01'),as.Date('2020-12-31'),by='12 months'),'%Y-%m')
  )+
  scale_y_discrete(limits = rev(sort(country_list)))