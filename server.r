server <- function(input, output, session) {
  
#get shapes for relevant state
  
  rel_zip_map_data_func=reactive({
    relv_data=zip_map_data %>%
      filter(state_full_name %in% input$selected_states)
    return(relv_data)
  })
  
  rel_covid_map_data_func=reactive({
    relv_data=covid_growth %>%
      filter(state_full_name %in% input$selected_states)%>%
      mutate(group=1,
             Covid.Avg.Daily.Growth.Rate=round(Covid.Avg.Daily.Growth.Rate*100,
                                               2))
    
    if(input$covid_overlay_select=="National Hotspots"){
      relv_data=relv_data %>%
        filter(National_Hotspot=="Yes")
    }
    
    if(input$covid_overlay_select=="In-State Hotspots"){
      relv_data=relv_data %>%
        filter(State_Hotspot=="Yes")
    }
    
    return(relv_data)
  })
  
  rel_state_map_data_func=reactive({
    relv_data=state_map_data %>%
      filter(state_full_name %in% input$selected_states)
    return(relv_data)
  })
  

# Selectors for Variable Weights ------------------------------------------

  
  
  output$topic_metric_selector_1=renderUI({
    
    
    if(input$consol_composite_topic1!="All"){
      relv_data=selected_cats %>%
        filter(Topic==input$consol_composite_topic1)
      
      cat_metrics=unique(relv_data$Name)
    }
    else{
      cat_metrics=consol_metrics
    }
    
    menu=selectInput("consol_composite_metric1",
                     label="Select Variable to Score by",
                     choices=cat_metrics,
                     selected="County Unemployment Rate",
                     selectize = TRUE)
    return(menu)
    
  })
  
  output$topic_metric_selector_2=renderUI({
    
    
    if(input$consol_composite_topic2!="All"){
      relv_data=selected_cats %>%
        filter(Topic==input$consol_composite_topic2)
      
      cat_metrics=unique(relv_data$Name)
    }
    else{
      cat_metrics=consol_metrics
    }
    
    menu=selectInput("consol_composite_metric2",
                     label="Select Variable to Score by",
                     choices=cat_metrics,
                     selected="State Unemployment Initial Claims; 4 Week Avg. Growth Rate",
                     selectize = TRUE)
    return(menu)
    
  })
  
  output$topic_metric_selector_3=renderUI({
    
    
    if(input$consol_composite_topic3!="All"){
      relv_data=selected_cats %>%
        filter(Topic==input$consol_composite_topic3)
      
      cat_metrics=unique(relv_data$Name)
    }
    else{
      cat_metrics=consol_metrics
    }
    
    menu=selectInput("consol_composite_metric3",
                     label="Select Variable to Score by",
                     choices=cat_metrics,
                     selected="State Unemployment Insurance Amount",
                     selectize = TRUE)
    return(menu)
    
  })
  
  output$topic_metric_selector_4=renderUI({
    
    
    if(input$consol_composite_topic4!="All"){
      relv_data=selected_cats %>%
        filter(Topic==input$consol_composite_topic4)
      
      cat_metrics=unique(relv_data$Name)
    }
    else{
      cat_metrics=consol_metrics
    }
    
    menu=selectInput("consol_composite_metric4",
                     label="Select Variable to Score by",
                     choices=cat_metrics,
                     selected="Covid.Per1000.Deaths",
                     selectize = TRUE)
    return(menu)
    
  })
  
  output$topic_metric_selector_5=renderUI({
    
    
    if(input$consol_composite_topic5!="All"){
      relv_data=selected_cats %>%
        filter(Topic==input$consol_composite_topic5)
      
      cat_metrics=unique(relv_data$Name)
    }
    else{
      cat_metrics=consol_metrics
    }
    
    menu=selectInput("consol_composite_metric5",
                     label="Select Variable to Score by",
                     choices=cat_metrics,
                     selected="Food services and drinking places:National Percentile Rank",
                     selectize = TRUE)
    return(menu)
    
  })
  
  output$topic_metric_selector_6=renderUI({
    
    
    if(input$consol_composite_topic6!="All"){
      relv_data=selected_cats %>%
        filter(Topic==input$consol_composite_topic6)
      
      cat_metrics=unique(relv_data$Name)
    }
    else{
      cat_metrics=consol_metrics
    }
    
    menu=selectInput("consol_composite_metric6",
                     label="Select Variable to Score by",
                     choices=cat_metrics,
                     selected="State Percentage of Business Decreasing Employees",
                     selectize = TRUE)
    return(menu)
    
  })
  output$topic_metric_selector_7=renderUI({
    
    
    if(input$consol_composite_topic7!="All"){
      relv_data=selected_cats %>%
        filter(Topic==input$consol_composite_topic7)
      
      cat_metrics=unique(relv_data$Name)
    }
    else{
      cat_metrics=consol_metrics
    }
    
    menu=selectInput("consol_composite_metric7",
                     label="Select Variable to Score by",
                     choices=cat_metrics,
                     selected="Zip Implied Hourly Rate",
                     selectize = TRUE)
    return(menu)
    
  })
  
  output$topic_metric_selector_8=renderUI({
    
    
    if(input$consol_composite_topic8!="All"){
      relv_data=selected_cats %>%
        filter(Topic==input$consol_composite_topic8)
      
      cat_metrics=unique(relv_data$Name)
    }
    else{
      cat_metrics=consol_metrics
    }
    
    menu=selectInput("consol_composite_metric8",
                     label="Select Variable to Score by",
                     choices=cat_metrics,
                     selected="Zip Population with College Degrees",
                     selectize = TRUE)
    return(menu)
    
  })
  
  # Zip Heat Composite Calculations ----------------------------------------------------
  
  consol_composite_params_func=reactive({
    composite_params=data.frame(Name=c(input$consol_composite_metric1,
                                       input$consol_composite_metric2,
                                       input$consol_composite_metric3,
                                       input$consol_composite_metric4,
                                       input$consol_composite_metric5,
                                       input$consol_composite_metric6,
                                       input$consol_composite_metric7,
                                       input$consol_composite_metric8),
                                Direction=c(input$consol_composite_direction1,
                                            input$consol_composite_direction2,
                                            input$consol_composite_direction3,
                                            input$consol_composite_direction4,
                                            input$consol_composite_direction5,
                                            input$consol_composite_direction6,
                                            input$consol_composite_direction7,
                                            input$consol_composite_direction8),
                                Weight=c(input$consol_composite_weight1,
                                         input$consol_composite_weight2,
                                         input$consol_composite_weight3,
                                         input$consol_composite_weight4,
                                         input$consol_composite_weight5,
                                         input$consol_composite_weight6,
                                         input$consol_composite_weight7,
                                         input$consol_composite_weight8))
    composite_params=mutate(composite_params,Weight=as.numeric(Weight))
    #write.csv(composite_params,'Composite Paramters.csv',
     #         row.names = FALSE)
    return(composite_params)
  })
  
  consol_composite_calc_data=reactive({
    composite_params=consol_composite_params_func()
    #print(composite_params)
    comp_data= all_zip_metrics %>%
      inner_join(composite_params, by = "Name")%>%
      filter(state_full_name %in% input$selected_states)
    
    #write.csv(comp_data,'Composite Parameters Data.csv',
     #         row.names = FALSE)
    return(comp_data)
  })
  
  consol_composite_calc_data_wide=reactive({
    relv_data=consol_composite_calc_data()%>%
      filter(Weight>0)%>%
      select(zip_name,Name,value)%>%
      mutate(value=round(value,2))%>%
      reshape2::dcast(zip_name~Name,value.var = "value",
                      fun.aggregate = mean)
    #write.csv(relv_data,'Composite Parameters Data Wide.csv',
     #         row.names = FALSE)
    return(relv_data)
  })
  consol_composite_calc=reactive({
    comp_data= consol_composite_calc_data() %>%
      mutate(Percentile=if_else(Direction=="Positive",Percentile,
                                1-Percentile))%>%
      mutate(Score=Percentile*Weight)%>%
      group_by(zip_code,
               zip_name,
               state_county_fips,
               state_county_name,
               state_full_name)%>%
      arrange(zip_code,
              zip_name,
              state_county_fips,
              state_county_name,
              state_full_name)%>%
      summarise(Sum_Score=sum(Score,na.rm = TRUE),
                Sum_Weight=sum(Weight,na.rm = TRUE),
                Count=n())%>%
      ungroup()%>%
      mutate(Name="Composite Risk Score",
             Percentile=Sum_Score/Sum_Weight,
             value=round(Sum_Score/Sum_Weight,3)*(Count/8),
             Topic="User Created")%>%
      arrange(-value)
    #write.csv(comp_data,'Composite Parameters Calculation.csv',
     #         row.names = FALSE)
    #comb_data=bind_rows(comp_data,
    #                   filter(consol_county_data,Name!="Composite Risk Score"))
    
    return(comp_data)
  })
  

# Top and Bottom 10 Zips --------------------------------------------------

  top_10_zips = reactive({
    relv_data=consol_composite_calc()%>%
      arrange(-value)
    
    relv_data=head(relv_data,10)
    return(relv_data)
  })
  
  output$zip_top_list= renderGvis({
    
    relv_data=top_10_zips()
    
    bar_title=paste0("Top 10 Zip-Codes by Composite Score")
    
    bar=gvisBarChart(relv_data,xvar='zip_name',
                     yvar='value',options=list(title=bar_title,
                                               height=360,
                                               hAxis='{minValue:0, maxValue:1}'))
    
    return(bar)
  })
  
  bottom_10_zips = reactive({
    relv_data=consol_composite_calc()%>%
      arrange(value)
    
    relv_data=head(relv_data,10)
    return(relv_data)
  })
  
  output$zip_bottom_list= renderGvis({
    
    relv_data=bottom_10_zips()
    
    bar_title=paste0("Bottom 10 Zip-Codes by Composite Score")
    
    bar=gvisBarChart(relv_data,xvar='zip_name',
                     yvar='value',options=list(title=bar_title,
                                               height=360,
                                               hAxis='{minValue:0, maxValue:1}'))
    
    return(bar)
  })
  
  sel_metrics_zip_data=reactive({
    
    relv_zips=top_10_zips()%>%
      select(zip_name,`Composite Score`=value)
    if(input$zip_sel_metric_top_bottom=="Bottom 10"){
      relv_zips=bottom_10_zips()%>%
        select(zip_name,`Composite Score`=value)
    }
    relv_data= consol_composite_calc_data_wide()%>%
      inner_join(relv_zips,by="zip_name")
    
    return(relv_data)
  })
  #Download handler
  output$downloadmetriclist= downloadHandler(
    filename = function() {
      paste0(input$zip_sel_metric_top_bottom,
             ' Zip Code Metrics ',
             paste0(input$selected_states,collapse = ","),
             ".csv")
    },
    content = function(file) {
      write.csv(sel_metrics_zip_data() , file, row.names = FALSE)
    }
  ) 
  output$sel_metrics_table=renderPlotly({
    temp=sel_metrics_zip_data()%>%
      arrange(-`Composite Score`)
    
    fig <- plot_ly(
      type = 'table',
      header = list(
        values = c(names(temp)[1:length(names(temp))]),
        align = c('left', rep('center', length(names(temp))-2)),
        line = list(color = '#506784'),
        fill = list(color = '#119DFF'),
        font = list(color = 'white', size = 10)
      ),
      cells = list(
        values = rbind(
          t(as.matrix(unname(temp)))
        ),
        line = list(color = '#506784'),
        fill = list(color = c('#25FEFD', 'white')),
        align = c('left', 'center'),
        font = list(color = c('#506784'), size = 10)
      ))
    
    return(fig)
  })  

  
  output$consol_zip_heat_map=renderPlot({
    map_data=rel_zip_map_data_func() %>%
      left_join(consol_composite_calc(), 
                by = c("zip_code", "state_full_name", "state_county_fips"))
    
    relv_state_map_data=rel_state_map_data_func()
    
    # write.csv(map_data,'County Risk Score.csv',
    #          row.names = FALSE)
    #print(head(map_data))
    
    p=ggplot(map_data,aes(x=long, y=lat, group=group))+ 
      geom_polygon(color="darkblue", 
                   aes(fill=Percentile), size = .1 , alpha = .5) +
      geom_polygon(data=relv_state_map_data, 
                   aes(x=long, y=lat, group=group),
                   color="black", fill="lightblue",  size = 1, alpha = .3)+
      ggtitle(paste0(""))+
      scale_fill_continuous(name="Composite Score", 
                            low = "red", high = "green",
                            na.value = "grey50")+
      theme_void()
    
    if(input$covid_overlay_select!="No Covid Overlay"){
      p=p+ geom_point(data=rel_covid_map_data_func(),
                      aes(x = county_lon, y = county_lat, 
                          size = Covid.Avg.Daily.Growth.Rate),
                      color="purple")
    }
    
    return(p)
  })
}#end of server