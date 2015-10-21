library(leaflet)
library(RColorBrewer)
#read in csv file
data = read.csv("data/esfu_app_data.csv", stringsAsFactors = FALSE, fileEncoding='latin1')
data = data[order(data$Country),]

shinyUI(fluidPage(theme = "bootstrap.css",
  #css to hide error messages
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # place Aeras images
  img(src = "aeras_logo.jpg", height = 40, width = 400, align = "middle", hspace = 400, vspace = 10),
  h4("AERAS ESFU DATA", align = "center"),
  
  # add button to download all data  
  tags$div(downloadButton('download_full_data', 'Download All Data'), style = "margin-left: 100px;"),
  hr(),
  actionButton('Reset_button', 'Reset'),
  br(),
 
  sidebarLayout(
            sidebarPanel(
            
              tabsetPanel(
                tabPanel('View Data',
          # add option to select countries of interest
           selectInput("country_selection", "Select Countries", choices = c("All Countries",unique(data$Country)), multiple = TRUE, selected = "All Countries"),             
             uiOutput('site'),
             uiOutput("datatype"),           
             uiOutput('sex'),
             uiOutput('pop'),
             uiOutput('tb_type'),
             sliderInput("Age_range", label = "Age Range", min = 0, 
                         max = 100, value = c(0, 100)),
            checkboxInput("age_checkbox", label = "Exclude data for all ages [0 - 100]", value = FALSE),
             dateRangeInput("Date_range", label = "Date range", start = "1900", format = "yyyy")
             
              ),
          
                tabPanel('Search Sites', 
                         selectInput("country_selection2", "Select Countries", choices = c("All Countries",unique(data$Country)), multiple = TRUE, selected = "All Countries"),
                         uiOutput('data_typing2'),
                         numericInput('search_num1', "Data Min (%)", value = 0),
                         numericInput('search_num2', "Data Max (%)", value = 0),
                         radioButtons("sex2", "Gender", unique(data$Gender)),
                         uiOutput('pop2'),
                         sliderInput("Age_range2", label = "Age Range", min = 0, 
                                     max = 100, value = c(0, 100)),
                         checkboxInput("age_checkbox2", label = "Exclude data for all ages [0 - 100]", value = FALSE),
                         dateRangeInput("Date_range2", label = "Date range", start = "1900", format = "yyyy")                         
                )         
              )
           ),
          
          mainPanel(
            conditionalPanel(
              condition = "input.data_type != 'Select DataType'",
              htmlOutput('data_type_info'),
              tableOutput('summary_table'),
              
              ### style tables
              tags$style(type="text/css", "#summary_table table {border: medium solid #3366CC; font-size: 15px; font-family: 'Verdana'; border-collapse: collapse; text-align: center; margin-left: auto; margin-right: auto;}"),
              tags$style(type="text/css", "#summary_table tr {border: thin solid #3366CC; text-align: center;}"),
              tags$style(type="text/css", "#summary_table td {border: thin solid; font-family: 'Verdana'; text-align: center;}"),
              
              tags$style(type="text/css", "#summary_table th {text-align: center;}"),
                            
              tags$style(type="text/css", "#summary_table td:first-child {font-weight: bolder; font-family: 'Verdana'}"),
              tags$style(type="text/css", "#summary_table tr:first-child {font-weight: bold; background-color: #0066CC; color: white; font-family: 'Verdana';}"),
                
              tags$style(type="text/css", "#data_table table {border: medium solid #3366CC; font-size: 15px; font-family: 'Verdana'; border-collapse: collapse; text-align: center; margin-left: auto; margin-right: auto;}"),        
              
              br(),
              hr(),
              htmlOutput('info_for_datatable'),
              dataTableOutput('data_table'),
              
              br(),
              leafletOutput('testplot'),
              hr(),
              downloadButton('download_subset_data', 'Download Spreadsheet of Selected Data')
            ),
            
            conditionalPanel(
              condition = "input.data_type2 != 'Select DataType' &&  input.data_type == 'Select DataType' && (typeof input.data_type2 !== 'undefined')",
             
              htmlOutput('data_type_info2'),
              htmlOutput("heading_summary2"),
              tableOutput('summary2'),
              hr(),
              htmlOutput('info_for_datatable2'),
              dataTableOutput('data_table2'),
              ### style tables
              tags$style(type="text/css", "#summary2 table {border: medium solid #009900; font-size: 15px; font-family: 'Verdana'; border-collapse: collapse; text-align: center; margin-left: auto; margin-right: auto;}"),
              tags$style(type="text/css", "#summary2 tr {border: thin solid #009900; text-align: center;}"),
              tags$style(type="text/css", "#summary2 td {border: thin solid; text-align: center;}"),
              
              
              tags$style(type="text/css", "#summary2 th {font-weight: bold; background-color: #009900; color: white; text-align: center;}"),
              
              
              tags$style(type="text/css", "#data_table2 table {border: medium solid #009900; font-size: 15px; font-family: 'Verdana'; border-collapse: collapse; text-align: center; margin-left: auto; margin-right: auto;}"),
              downloadButton('download_subset_data2', 'Download Spreadsheet of Selected Data')
            )
          )        
          )                                   
  )  
)

