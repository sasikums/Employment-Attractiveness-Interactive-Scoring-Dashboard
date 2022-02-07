
#setwd('C:/Users/shreedharsasikumar/Desktop/SR Data Catalog/Zipscores/')


included_states=c('MI','WI','IN','OH')
state_string=paste0(included_states,collapse = ",")
update_sr_data_func =function(){
  setwd('C:/Users/shreedharsasikumar/Desktop/SR Data Catalog/McDIL')
  source('SR_Functions_list.R')
  
  
  
  setwd('C:/Users/shreedharsasikumar/Desktop/SR Data Catalog/Zipscores/')
  #Zip Map Data
  load('US_Zip_Shapes.rdata') #data frame:zip_map_data
  
  all_zip_metrics=readRDS(file ="Zip Code Latest Metrics.rds")%>%
    filter(state_abbreviation %in% included_states)
  
  state_full_string=read.csv('us_state_fips.csv',
                             stringsAsFactors = FALSE)%>%
    filter(state_abbv %in% included_states)%>%
    select(state_full_name)
  state_string=paste0(included_states,collapse = ",")
  
  covid_growth=readRDS(file ="Current Covid Growth Rate by County.rds")%>%
    filter(state_abbreviation %in% included_states)
  
  
  setwd('C:/Users/shreedharsasikumar/Desktop/SR Data Catalog/App_MI/')
  saveRDS(all_zip_metrics,paste0("Zip Code Latest Metrics",
                                 state_string,".rds"))
  saveRDS(covid_growth,paste0("Current Covid Growth Rate by County",
                               state_string,".rds"))
  
 
  
  relv_zip_map_data=zip_map_data %>%
    inner_join(state_full_string)
  
  saveRDS(relv_zip_map_data,paste0("Relv State Zip Shapes ",
                                 state_string,".rds"))
    
}
#update_sr_data_func()
# Zip Map Data ------------------------------------------------------------

#Zip Map Data
zip_map_data=readRDS(file =paste0("Relv State Zip Shapes ",
                                  state_string,".rds"))
state_map_data=read.csv('US State Shape La-Lons.csv',
                        stringsAsFactors  = FALSE)%>%
  mutate(state_full_name=str_to_title(region))

# All Zip Metrics ---------------------------------------------------------
all_zip_metrics=readRDS(file =paste0("Zip Code Latest Metrics",
                                     state_string,".rds"))


covid_growth=readRDS(file =paste0("Current Covid Growth Rate by County",
                                  state_string,".rds"))

consol_states=unique(all_zip_metrics$state_full_name)
selected_cats=distinct(all_zip_metrics,Topic,Name)
consol_topics=c('All',unique(all_zip_metrics$Topic))
consol_metrics=unique(all_zip_metrics$Name)
