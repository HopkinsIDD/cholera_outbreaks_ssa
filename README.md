# cholera_outbreaks_ssa
This repository provides the data and source code for the following paper: Qulu Zheng, Francisco J Luquero, Iza Ciglenecki, Joseph F. Wamala, Abdinasir Abubakar, Placide Welo, Mukemil Hussen, Mesfin Wossen, Sebastian Yennan, Alama Keita, Justin Lessler, Andrew S. Azman, Elizabeth C. Lee. "Cholera outbreaks in sub-Saharan Africa during 2010-2019: A Descriptive Analysis." International Journal of Infectious Diseases. (DOI: 10.1016/j.ijid.2022.05.039)

## Data (reference_data/)
1. **Summarized characteristics of cholera outbreaks from 2010 through 2019: reference_data/outbreak_data.csv**
* _location_: the name of the location where outbreak cases were reported and the name is made up of WHO region, country, and administrative units seperated by "::".
* _outbreak_number_: the uid of outbreak occurred in a specific region.
* _start_weekday_: the start weekday of the reported cases.
* _duration_: the duration of the outbreak in weeks.
* _threshold_: the outbreak threshold per week.
* _total_suspected_cases_: the total number of suspected cholera cases in an outbreak. 
* _attack_rate_: the attach rate of an outbreak per 1,000 people.
* _total_deaths_: the total number of cholera-associated deaths in an outbreak. NA means death data were not reported.
* _cfr_: the case fatality rate of an outbreak.
* _temporal_scale_: the temporal scale of the time series data (weekly data).
* _country_: the country where the outbreak was reported.
* _population_: the total population in the region where the outbreak was reported and in the year when the outbreak was detected.
* _area_: the area of the region where the outbreak was reported.
* _mean_R0_: the average of the instantaneous reproductive estimates over the first week of an outbreak.
* _population_density_: the number of population per km2 in the region where the outbreak was reported.
* _rural_urban_: urban regions are places where population density is equal to or higher than 1000 habitants per km2 and rural regions are places where population density is below 1000 habitants per km2.
* _pop_cat_: regions are grouped into four categories based on the population size, including "population < 1,000,000", "population 100,000-1,000,000", "population 10,000-100,000", and "population<10,000".
* _who_region_: The WHO region where the outbreak was reported.
* _start_date_: the start date of the outbreak.
* _end_date_: the end date of the outbreak.
* _total_confirmed_cases_: the total number of confirmed cholera cases reported in an outbreak. NA means that no confirmed case data was reported.
* _spatial_scale_: the spatial scale of the outbreak, including the first administrative unit ("admin1"), the second administrative unit ("admin2"), and the third administrative unit ("admin3"). 
* _location_period_id_: the location period id that are used to link to the shapefiles in global cholera incidence database.
* _time_to_peak..weeks._: the number of weeks between the start of an outbreak and the week with the most reported cases
**2. Summarized characteristics of cholera outbreaks from 2010 through 2019 in the sensitivity analysis: reference_data/outbreak_data_sensitivity_analysis.csv**
* column names are the same as above. 
**3. Processed monthly time series of population data from 2010 through 2019: reference_data/heatmap_data.csv**
* country: the ISO 3166 country codes.
* year_month: 
6. Processed location and population data: reference_data/country_location_period_pop.csv. 
7. Processed data for supplement figures 75_77: reference_data/Supplement_figure_75_77.csv

## Code (programs/)
This folder contains scripts that can generate the figrues in the main text or supplementary material. 
