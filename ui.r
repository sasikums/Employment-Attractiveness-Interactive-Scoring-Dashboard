
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(stringr)
library(googleVis)
library(zoo)
library(data.table)
library(lubridate)
library(viridis)

source('helper_functions.r')


ui <- dashboardPage(
  dashboardHeader(title="Midwest Employment"),
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Find Zip Codes to Hire", 
               tabName = "nat_zip"
      ),
      selectInput("selected_states",
                  label="Select States to Zoom in on",
                  choices=consol_states,
                  selected = consol_states,
                  multiple = TRUE,
                  selectize = TRUE)
    )#end of sidebarMenu
  ),#end of dashsise
  dashboardBody(
    tabItems(
      tabItem('nat_zip',
              fluidRow(
                box(width = 8,
                    status = "info", solidHeader = TRUE,
                    title = "Zip-Code Level Heat Map of Composite Score for Selected States (Green is attractive areas for hiring)",
                    plotOutput('consol_zip_heat_map'),
                    radioButtons("covid_overlay_select",
                                 "Select to show Covid Hotspots (by Daily Case Growth):",
                                 choices = c('No Covid Overlay','National Hotspots',
                                             'In-State Hotspots','All Counties'),
                                 selected = "National Hotspots",
                                 inline = TRUE),
                    p("Please allow a few seconds for the map & charts to load."),
                    p("Please note calculations will take a few seconds to update as new inputs/weights are selected.")
                    
                    
                ),#end of box
                tabBox(width=4,
                       title="Summary View of Top/Bottom 10 Zip Codes",
                       
                       tabPanel("Top 10 Zip Codes",
                                htmlOutput('zip_top_list'),
                                p("Please allow a few seconds for the map & charts to load."),
                                p("Please note calculations will take a few seconds to update as new inputs/weights are selected.")
                                
                       ),
                       tabPanel("Bottom 10 Zip Codes",
                                htmlOutput('zip_bottom_list'),
                                p("Please note calculations will take a few seconds to update as new inputs/weights are selected.")
                                ),
                       tabPanel("Selected Metrics Table",
                              radioButtons("zip_sel_metric_top_bottom",
                                           "Select Top/Bottom 10 Zip Codes",
                                           choices = c('Top 10','Bottom 10'),
                                           inline = TRUE),
                               plotlyOutput('sel_metrics_table'),
                              p("Please note calculations will take a few seconds to update as new inputs/weights are selected."),
                              downloadButton("downloadmetriclist", 
                                             "Download Displayed Table as CSV")
                              )#end of tabpanel
                )#end of box
              ),#end of fluidrow,
              fluidRow(
                box(width=12,    title = strong("How to use this dashboard"),
                    background = "light-blue",
                    "The heat map shows a composite score of which zip-codes
                    across the selected states offer the best prospects of finding
                    a person likely to accept an employment offer, with green zip-codes
                    indicating that a person is likelier to take a job offer than someone
                    from a red zip-code. Note this data and scores are presented for sample illustrative purposes only
                    and represents a small portion of signals that could be leveraged to answer this question. Data is shown
                    as is at the source.",
                    tags$br(), 
                    "Use the drop-downs in each dialog box below to choose among a number
                    of signal variables that could impact a person's likelihood to accept
                    a job such as prevailing unemployment in the area, who likely employers
                    are in the area & whether they are affected by Covid, the local state
                    policies, Covid case growth etc.",
                    tags$br(),
                    "As a user selects variables, the composite score updates based on the
                    selected variables, weight and direction. This in turn updates the map 
                    as well as the Top 10/Bottom 10 Zip-Codes. Note the weight must be set to 
                    greater than 0 to be used in the calculation and displayed in the tables.",
                    tags$br(),
                    "Use the charts and tables to the right to view additional details about the
                    Top/Bottom 10 zip-codes by composite score. You can also change the
                    states used in the ranking clicking on the menu-bar, top-left, next to the title.",
                    tags$br(),
                    "Adjust the selected fields, customize the weights and 
                    change the assumed direction of the signal on employment
                    acceptance prospect to customize your score."
                )#end of box
              ),#end of row
             
              fluidRow(
                box(width=3,status = "info", solidHeader = TRUE,
                    title = "Select Variable, Weight & Direction",
                    selectInput("consol_composite_topic5",
                                label="Select Topic",
                                selected = "Business Concentation by Industry",
                                choices=consol_topics,selectize = TRUE),
                    uiOutput('topic_metric_selector_5'),
                    selectInput("consol_composite_direction5",
                                label="Set Direction",
                                selected="Positive",
                                choices=c('Positive',
                                          'Negative'),selectize = TRUE),
                    selectInput("consol_composite_weight5",
                                label="Set Relative Weight (1-10)",
                                selected=5,
                                choices=c(0,1,2,3,4,5,6,7,8,9,10),selectize = TRUE)
                    
                ),#end of box
                box(width=3,status = "info", solidHeader = TRUE,
                    title = "Select Variable, Weight & Direction",
                    selectInput("consol_composite_topic6",
                                label="Select Topic",
                                selected = "Small Business Sentiment",
                                choices=consol_topics,selectize = TRUE),
                    uiOutput('topic_metric_selector_6'),
                    selectInput("consol_composite_direction6",
                                label="Set Direction",
                                selected="Positive",
                                choices=c('Positive',
                                          'Negative'),selectize = TRUE),
                    selectInput("consol_composite_weight6",
                                label="Set Relative Weight (1-10)",
                                selected=2,
                                choices=c(0,1,2,3,4,5,6,7,8,9,10),selectize = TRUE)
                    
                ),#end of box
                box(width=3,status = "info", solidHeader = TRUE,
                    title = "Select Variable, Weight & Direction",
                    selectInput("consol_composite_topic7",
                                label="Select Topic",
                                selected = "Zip Code Population",
                                choices=consol_topics,selectize = TRUE),
                    uiOutput('topic_metric_selector_7'),
                    selectInput("consol_composite_direction7",
                                label="Set Direction",
                                selected="Negative",
                                choices=c('Positive',
                                          'Negative'),selectize = TRUE),
                    selectInput("consol_composite_weight7",
                                label="Set Relative Weight (1-10)",
                                selected=6,
                                choices=c(0,1,2,3,4,5,6,7,8,9,10),selectize = TRUE)
                    
                ),#end of box
                box(width=3,status = "info", solidHeader = TRUE,
                    title = "Select Variable, Weight & Direction",
                    selectInput("consol_composite_topic8",
                                label="Select Topic",
                                selected = "Zip Code Population",
                                choices=consol_topics,selectize = TRUE),
                    uiOutput('topic_metric_selector_8'),
                    selectInput("consol_composite_direction8",
                                label="Set Direction",
                                selected="Negative",
                                choices=c('Positive',
                                          'Negative'),selectize = TRUE),
                    selectInput("consol_composite_weight8",
                                label="Set Relative Weight (1-10)",
                                selected=5,
                                choices=c(0,1,2,3,4,5,6,7,8,9,10),selectize = TRUE)
                    
                )#end of box
              ),#end of fluidRow
              fluidRow(
                box(width=3,status = "info", solidHeader = TRUE,
                    title = "Select Variable, Weight & Direction",
                    selectInput("consol_composite_topic1",
                                label="Select Topic",
                                selected = "Monthly County Unemployment",
                                choices=consol_topics,selectize = TRUE),
                    uiOutput('topic_metric_selector_1'),
                    selectInput("consol_composite_direction1",
                                label="Set Direction",
                                selected="Positive",
                                choices=c('Positive',
                                          'Negative'),selectize = TRUE),
                    selectInput("consol_composite_weight1",
                                label="Set Relative Weight (1-10)",
                                selected=8,
                                choices=c(0,1,2,3,4,5,6,7,8,9,10),selectize = TRUE)
                    
                ),#end of box
                box(width=3,status = "info", solidHeader = TRUE,
                    title = "Select Variable, Weight & Direction",
                    selectInput("consol_composite_topic2",
                                label="Select Topic",
                                selected =  "	Weekly State Employment Insurance",
                                choices=consol_topics,selectize = TRUE),
                    uiOutput('topic_metric_selector_2'),
                    selectInput("consol_composite_direction2",
                                label="Set Direction",
                                selected="Positive",
                                choices=c('Positive',
                                          'Negative'),selectize = TRUE),
                    selectInput("consol_composite_weight2",
                                label="Set Relative Weight (1-10)",
                                selected=2,
                                choices=c(0,1,2,3,4,5,6,7,8,9,10),selectize = TRUE)
                    
                ),#end of box
                box(width=3,status = "info", solidHeader = TRUE,
                    title = "Select Variable, Weight & Direction",
                    selectInput("consol_composite_topic3",
                                label="Select Topic",
                                selected = "State Covid Policies",
                                choices=consol_topics,selectize = TRUE),
                    uiOutput('topic_metric_selector_3'),
                    selectInput("consol_composite_direction3",
                                label="Set Direction",
                                selected="Negative",
                                choices=c('Positive',
                                          'Negative'),selectize = TRUE),
                    selectInput("consol_composite_weight3",
                                label="Set Relative Weight (1-10)",
                                selected=2,
                                choices=c(0,1,2,3,4,5,6,7,8,9,10),selectize = TRUE)
                    
                ),#end of box
                box(width=3,status = "info", solidHeader = TRUE,
                    title = "Select Variable, Weight & Direction",
                    selectInput("consol_composite_topic4",
                                label="Select Topic",
                                selected = "Covid Incidence",
                                choices=consol_topics,selectize = TRUE),
                    uiOutput('topic_metric_selector_4'),
                    selectInput("consol_composite_direction4",
                                label="Set Direction",
                                selected="Negative",
                                choices=c('Positive',
                                          'Negative'),selectize = TRUE),
                    selectInput("consol_composite_weight4",
                                label="Set Relative Weight (1-10)",
                                selected=3,
                                choices=c(0,1,2,3,4,5,6,7,8,9,10),selectize = TRUE)
                    
                )
              ),#end of fluidRow
              fluidRow(
                p("Sources: US Census Bureau, USAFacts, US Department of Labor, KPMG Signals Repository")
              )
      )#end of tab
    )#end of of tab items
  )#end of dash body
)# end of dash page
