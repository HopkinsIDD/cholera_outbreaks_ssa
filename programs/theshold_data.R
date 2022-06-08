##Figure 4: associations between different metrics: Outbreak attack rate, cfr, outbreak size, outbreak duration and outbreak threshold for admin2 outbreaks####
library(dplyr)
library(ggplot2)
library(stringr)

all_outbreak_data=readRDS('reference_data/outbreak_data.rds')
all_outbreak_data$location=str_replace_all(all_outbreak_data$location,"afr::tza::mainland::","afr::tza::")
all_outbreak_data$location=str_replace_all(all_outbreak_data$location,"afr::tza::zanzibar::","afr::tza::")
tza=all_outbreak_data[which(all_outbreak_data$country=="TZA"),]

#subset weekly and daily data
all_outbreak_data=all_outbreak_data[which(all_outbreak_data$temporal_scale=='weekly'|all_outbreak_data$temporal_scale=='daily'),]
all_outbreak_data$year=lubridate::year(all_outbreak_data$TL)
all_outbreak_data$location_period_id=as.integer(all_outbreak_data$location_period_id)


threshold=data.frame()
for (country_iso in sort(unique(all_outbreak_data$country))[1:length(sort(unique(all_outbreak_data$country)))]) {
  #country_iso='TZA_zanzibar'
  minimum_consecutive_reports=3
  threshold_dependent_reports=3
  outbreak_size=0
  
  daily_aggregated_weekly_changing_threshold_filepath=paste0('generated_data/Country/',country_iso,'/',country_iso,'_outbreak_list_weekly_changing_threshold_daily_aggregated_new_definition1_',minimum_consecutive_reports,'_',threshold_dependent_reports,'.rds')
  #daily_aggregated_weekly_changing_threshold_filepath=paste0('generated_data/Country/',country_iso,'/',country_iso,'_outbreak_list_weekly_changing_threshold_daily_aggregated_',minimum_consecutive_reports,'_',threshold_dependent_reports,'.rds')
  
  if(file.exists(daily_aggregated_weekly_changing_threshold_filepath)){
    weekly_daily_outbreak_list=readRDS(file = daily_aggregated_weekly_changing_threshold_filepath)
    if(length(weekly_daily_outbreak_list)>0){
      for (i in 1:length(weekly_daily_outbreak_list)) {
        
        tmp=data.frame(weekly_daily_outbreak_list[[i]])%>%
          subset(EpiPeriod=='complete')
        tmp=tmp[,colnames(tmp)%in%c("outbreak_number","location","country","spatial_scale","threshold","start_weekday","TL","TR","population","sCh",'deaths'),]%>%
          distinct(outbreak_number,location,country,spatial_scale,threshold,start_weekday,TL,TR,population,sCh,deaths)
        
        threshold=rbind(threshold,tmp)
        
      }
      
    }
  }
  print(country_iso)
}

threshold_data=threshold%>%
  subset(!spatial_scale=='country')%>%
  group_by(location,outbreak_number,start_weekday,country,spatial_scale)%>%
  summarise(
    duration=as.integer(max(TR)-min(TL)+1)/7,
    threshold=threshold*100000,
    population=min(population),
    total_suspected_cases=round(sum(sCh),0),
    attack_rate=1000*total_suspected_cases/population,
    total_deaths=round(sum(deaths),0),
    cfr=100*total_deaths/total_suspected_cases)
threshold_data=distinct(threshold_data)
nrow(all_outbreaks_R)#from reproductive number R script
all_outbreaks_R=distinct(all_outbreaks_R)
threshold_data1=merge(distinct(threshold_data[,colnames(threshold_data)%in%c('total_suspected_cases','total_deaths','duration','location','outbreak_number','start_weekday','threshold','attack_rate','cfr')]),all_outbreaks_R[,!colnames(all_outbreaks_R)%in%c('total_sCh')],by=c('outbreak_number','location','start_weekday'))
