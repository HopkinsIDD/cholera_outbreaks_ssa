library(viridisLite)
library(ggplot2)

##Figure 1 Spatial distribution of outbreaks reported at sub-national administrative units
afr_shp=sf::st_read('reference/AfricaShapefiles/afr_g2014_2013_0.shp')#from open AFrica
total_shp=sf::st_read('reference_data/AfricaShapefiles/total_shp_0427.shp')

colnames(total_shp)[1]="lctn_pr"
significant_outbreaks=subset(read.csv('reference_data/outbreak_time_to_peak_new_definition1.csv',encoding="UTF-8"),
                             EpiPeriod=='complete' &
                               !spatial_scale=="country"
)
all(significant_outbreaks$location_period_id%in%total_shp$lctn_pr)


##change tza mailand from admin1 to admin0
significant_outbreaks=significant_outbreaks%>%subset(!location=="afr::tza::mainland")
significant_outbreaks[which(stringr::str_detect(significant_outbreaks$location,"afr::tza::mainland")&significant_outbreaks$spatial_scale=="admin2"),]$spatial_scale="admin1"
significant_outbreaks[which(stringr::str_detect(significant_outbreaks$location,"afr::tza::mainland")&significant_outbreaks$spatial_scale=="admin3"),]$spatial_scale="admin2"

##piped locations shps
significant_outbreaks$spatial_scale=as.character(significant_outbreaks$spatial_scale)
total_shp$spatial_level=NA
total_shp$outbreak='No'
piped_location=significant_outbreaks[which(grepl("\\|",significant_outbreaks$location_period_id)),]

for (piped_loc in unique(piped_location$location_period_id)){
  component_loc=unlist(strsplit(piped_loc,split = '\\|'))
  aggregated_shp=total_shp[total_shp$lctn_pr%in%component_loc,]$geometry
  valid_shp1=sf::st_cast(aggregated_shp,'POLYGON')
  final_shp=sf::st_as_sf(sf::st_union(valid_shp1))
  final_shp$geometry=final_shp$x
  sf::st_geometry(final_shp)='geometry'
  final_shp$x=piped_loc
  colnames(final_shp)[1]='lctn_pr'
  final_shp$spatial_level=unique(significant_outbreaks[which(significant_outbreaks$location_period_id==piped_loc),]$spatial_scale)
  final_shp$outbreak="Yes"
  total_shp=rbind(total_shp,final_shp)
}

#non-piped locations
for (lp_id in unique(significant_outbreaks[which(significant_outbreaks$location_period_id%in%unique(total_shp$lctn_pr)),]$location_period_id)) {
  total_shp[which(total_shp$lctn_pr==lp_id),]$spatial_level=unique(significant_outbreaks[which(significant_outbreaks$location_period_id==lp_id),]$spatial_scale)
  total_shp[which(total_shp$lctn_pr==lp_id),]$outbreak='Yes'
}
table(total_shp$outbreak)

total_shp$spatial_level=factor(total_shp$spatial_level,levels = c('country','admin1','admin2','admin3'))
country_total_shp=total_shp[which(total_shp$spatial_level=='country'),]

admin1_piped_shp=total_shp[which(total_shp$spatial_level=='admin1'&
                                   !total_shp$spatial_level=='unknown'&
                                   grepl('\\|',total_shp$lctn_pr)),]
admin1_non_piped_shp=total_shp[which(total_shp$spatial_level=='admin1'&
                                       !total_shp$spatial_level=='unknown'&
                                       !grepl('\\|',total_shp$lctn_pr)),]
admin2_piped_shp=total_shp[which(total_shp$spatial_level=='admin2'&
                                   !total_shp$spatial_level=='unknown'&
                                   grepl('\\|',total_shp$lctn_pr)),]
admin2_non_piped_shp=total_shp[which(total_shp$spatial_level=='admin2'&
                                       !total_shp$spatial_level=='unknown'&
                                       !grepl('\\|',total_shp$lctn_pr)),]
admin3_piped_shp=total_shp[which(total_shp$spatial_level=='admin3'&
                                   !total_shp$spatial_level=='unknown'&
                                   grepl('\\|',total_shp$lctn_pr)),]
admin3_non_piped_shp=total_shp[which(total_shp$spatial_level=='admin3'&
                                       !total_shp$spatial_level=='unknown'&
                                       !grepl('\\|',total_shp$lctn_pr)),]

#select admin3 eth outbreaks
eth_admin3_shp=unique(significant_outbreaks[which(significant_outbreaks$spatial_scale=="admin3"&significant_outbreaks$country=="ETH"),]$location_period)
afr_country_shp=sf::st_read("generated_data/population/country_shps.shp")

ggplot() +
  geom_sf(data=afr_shp,color='black',fill='white')+
  geom_sf(data=admin1_piped_shp[admin1_piped_shp$outbreak=='Yes',],aes(fill=spatial_level)
  )+
  geom_sf(data=admin1_non_piped_shp[admin1_non_piped_shp$outbreak=='Yes',],aes(fill=spatial_level)
  )+
  geom_sf(data=admin2_piped_shp[admin2_piped_shp$outbreak=='Yes',],aes(fill=spatial_level)
  )+
  geom_sf(data=admin2_non_piped_shp[admin2_non_piped_shp$outbreak=='Yes',],aes(fill=spatial_level)
  )+
  geom_sf(data=admin3_piped_shp[admin3_piped_shp$outbreak=='Yes',],aes(fill=spatial_level)
  )+
  geom_sf(data=admin3_non_piped_shp[admin3_non_piped_shp$outbreak=='Yes',],aes(fill=spatial_level)
  )+
  geom_sf(data=afr_country_shp,color='black',fill=NA)+
  geom_point(
    data=admin3_non_piped_shp[admin3_non_piped_shp$outbreak=='Yes'&!admin3_non_piped_shp$lctn_pr%in%eth_admin3_shp,],
    aes(geometry = geometry),
    size=0.01,
    stat = "sf_coordinates"
  )+
  ggthemes::theme_map(base_size=18)+
  labs(fill="Spatial distribution of outbreaks \nreported at sub-national administrative units")+
  scale_fill_manual(values=c(
    'admin1'=viridisLite::cividis(10)[10],
    'admin2'=viridisLite::cividis(10)[8],
    'admin3'=viridisLite::cividis(10)[5]),
    breaks = c(
      'admin1','admin2','admin3'),
    labels=c(
      'First-level administrative units',
      'Second-level administrative units',
      'Third-level administrative units'
    ))+
  ggplot2::theme(
    panel.grid.minor =  ggplot2::element_blank(),
    panel.background =  ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.line =  ggplot2::element_blank(),
    axis.ticks.x  = element_blank(),
    legend.text=element_text(size=18,face="bold"),
    legend.title = element_text(size=20,face="bold"),
    legend.margin=margin(4, 0, 0, 0)
  )
