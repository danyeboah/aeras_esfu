#read in leaflet library
library(leaflet)

# read in sp package
library(sp)

#read in csv file
data_server = read.csv("data/esfu_app_data.csv", stringsAsFactors = FALSE, fileEncoding='latin1')
data_server = data_server[order(data_server$Organization_Site),]
data_server$data <- data_server$Percent * 100

# read in maps data
x = readRDS('data/ZAF_adm1.rds')
x@data$density = c(1,2,3,4,5,6,7,8,9,10)


shinyServer(function(input, output, session) {
  ###################################################### subsetting data section
  # select rows of chosen countries
  country_subset = reactive({
    if (input$country_selection != 'All Countries') {
      data = data_server[data_server$Country %in% input$country_selection,] 
      
    }
    else {
      data = data_server
    }
    return(data)
  })
  
  # select rows of chosen sites
  site_subset = reactive({
    data = country_subset()
    if (input$site_selection != 'All Sites in Selected Countries') {
      data = data[data$Organization_Site %in% input$site_selection,] 
    }
    return(data)
  })
  
  # select rows of chosen sites and datatype 
  site_data_subset = reactive({
    data = site_subset() 
    data = data[data$Data_type == input$data_type,]
  })
  
  # select rows of chosen sites, datatype and sex
  site_data_sex_subset = reactive({
    data = site_data_subset()
    data = data[data$Gender == input$sex,]
  })
  
  # select rows of chosen sites, datatype, sex, population
  site_data_sex_pop_subset = reactive({
    data = site_data_sex_subset()
    
    # subset based on population
    if (!"All available" %in% input$population) {
      
      for (i in 1:length(input$population)) {
        data = data[(data$Population1 == input$population[i]) | (data$Population2 == input$population[i]) | (data$Population3 == input$population[i]),]
      } 
    }
    
    return(data)
    
  })
  
  # select rows of chosen countries, sites, datatype,sex, population, date and age
  site_data_sex_others_subset = reactive({
    data = site_data_sex_pop_subset()
    
    # subset based on tb type
    if (!"All available" %in% input$tb_type) {
      
      for (i in 1:length(input$tb_type)) {
        data = data[(data$TB_Type1 == input$tb_type[i]) | (data$TB_Type2 == input$tb_type[i]) | (data$TB_Type3 == input$tb_type[i]) | (data$TB_Type4 == input$tb_type[i]) ,]
      } 
    }
  
    #subset based on age selection
    data = data[(data$Age_range_low <= input$Age_range[2] & data$Age_range_high >= input$Age_range[1]),]
    if (input$age_checkbox == TRUE)
    {
      data = data[data$Age_range_low > 0 & data$Age_range_high < 100,]
    }
    
    #subset based on year
    data = data[(as.numeric(substring(data$Data_Year_Min,1,4)) <= as.numeric(format(input$Date_range[2], "%Y"))) & (as.numeric( substring(data$Data_Year_Max,1,4)) >= as.numeric(format(input$Date_range[1], "%Y"))),]
    
    return(data)
  })
  
  
  ######################################################## selecting dynamic data
 # select site based on country selection
  output$site = renderUI({
    datatype_of_interest = country_subset()
    datatype_of_interest = datatype_of_interest[order(datatype_of_interest$Organization_Site),]
    datatype_of_interest = datatype_of_interest$Organization_Site
    datatype_of_interest = c("All Sites in Selected Countries",datatype_of_interest)
    selectInput("site_selection", "Select Sites", choices = datatype_of_interest, multiple = TRUE, selected = "All Sites in Selected Countries")
  })
  
  # select data type based on site selection
  output$datatype = renderUI({
    datatype_of_interest = unique(site_subset()$Data_type);
    datatype_of_interest = c("Select DataType", datatype_of_interest)
    selectInput("data_type", "Data Type",datatype_of_interest, selected = "Select DataType")
  })
  
  # select sex based on site and datatype selection
  output$sex = renderUI({
    datatype_of_interest = unique(site_data_subset()$Gender);
    selectInput("sex", "Gender",datatype_of_interest)
  })
  
  # select population based on site, datatype and sex selection
  output$pop = renderUI({
    datatype_of_interest = c("All available",unique(site_data_sex_subset()$Population1), unique(site_data_sex_subset()$Population2), unique(site_data_sex_subset()$Population3));
    selectInput("population", "Select Population of Interest", datatype_of_interest, selected = "All available", multiple = TRUE)
    })
  
  # select tb_type based on site, datatype, sex and population
  output$tb_type = renderUI({
    datatype_of_interest = c("All available",unique(site_data_sex_pop_subset()$TB_Type1), unique(site_data_sex_pop_subset()$TB_Type2), unique(site_data_sex_pop_subset()$TB_Type3), unique(site_data_sex_pop_subset()$TB_Type4));
    selectInput("tb_type", "Select TB type", datatype_of_interest, selected = "All available", multiple = TRUE)
 })
 

######################################################## create data to output
  # Heading(data-type)
  output$data_type_info = renderUI({
    tags$h3(paste("Summary Table for ", toString(input$data_type)), style ="text-align: center; color : #3366CC");
  })

  # obtain aggregate average
  aggregate_data_pertinent = reactive({
    data_mean = aggregate(site_data_sex_others_subset()$Percent * 100, by = list(site_data_sex_others_subset()$Organization_Site), FUN = function(x) { return(round(mean(x),2)) }) 
    data_sd = aggregate(site_data_sex_others_subset()$Percent * 100, by = list(site_data_sex_others_subset()$Organization_Site),  FUN = function(x) {return(round(sd(x),2))  } ) 
    data_min_max = aggregate(site_data_sex_others_subset()$Percent * 100, by = list(site_data_sex_others_subset()$Organization_Site),  FUN = function(x) {return(paste(min(x), '-', max(x)))} )   
    
    age_min_max = aggregate(site_data_sex_others_subset()$Age_range_low ~ site_data_sex_others_subset()$Age_range_high, by = list(site_data_sex_others_subset()$Organization_Site),  FUN = function(x, y) {return(paste(min(x), '-', max(y)))} )   
    year_min_max = aggregate(as.numeric(substring(site_data_sex_others_subset()$Data_Year_Min,1,4)) ~ as.numeric(substring(site_data_sex_others_subset()$Data_Year_Max,1,4)), by = list(site_data_sex_others_subset()$Organization_Site),  FUN = function(x, y) {return(paste(min(x), '-', max(y)))} )   
    
    data = merge(data_mean,data_sd, by = c("Group.1"))
    data = merge(data,data_min_max, by = c("Group.1"))
    data = merge(data,age_min_max, by = c("Group.1"))
    data = merge(data,year_min_max, by = c("Group.1"))
  
   colnames(data) = c('Site Name', 'Average', 'Standard Deviation(%)','Range(%)', 'Age Range (Years)', 'Date Range')  
   return(data)
   })

  ### output summary table
 output$summary_table = renderTable({
      data = aggregate_data_pertinent()
      data = data[order(-data$Average),]
      colnames(data) = c('Site Name', 'Average(%)', 'Standard Deviation(%)','Range(%)', 'Age Range (Years)', 'Date Range')  
      
      return(data)
  }, include.rownames = FALSE)



################################################# output original table of selection
variables = c('Organization_Site','Country','Age_range_low','Age_range_high','data','Comments','Data_source')
headers = c('Site', 'Country','Min Age', 'Max Age','Data (%)', 'Comments','Data Source')

  output$info_for_datatable = renderUI({
    tags$h3(paste("Data Table for", toString(input$data_type)), style ="text-align: center; color : #3366CC");
  })

  output$data_table = renderDataTable({
    data = site_data_sex_others_subset()
    data = data[variables]
 
    colnames(data) = headers
    return(data)
  }, options = list(columnDefs = list(list(targets = c(1:7) - 1, searchable = FALSE))))
  

############################################## output download and reset buttons
  # handler to allow downloading chosen data
  output$download_subset_data = downloadHandler (
    filename = function() {paste(toString(input$site_selection),"_",toString(input$data_type),"_",toString(input$sex),"_",toString(input$population),".csv")},
    content = function(file) {
      write.table(aggregate_data_pertinent(), file, sep = ",", row.names = FALSE)
      write.table(' ', file,col.names = FALSE, row.names = FALSE, append=TRUE)  
      write.table(site_data_sex_others_subset(), file, sep = ",",append=TRUE, row.names = FALSE)  
    } 
  )
  
  # download full data
  output$download_full_data = downloadHandler (
    filename = "aerasesfu_data.csv",
    content = function(file) {
      write.csv(data_server,file)
    } 
  )

# reset button  
  resetting = observeEvent(input$Reset_button, {
    updateSelectInput(session, "country_selection",selected = "All Countries")
    updateSelectInput(session, "country_selection2",selected = "All Countries")
    
    updateSelectInput(session, "site_selection",selected = "All Sites in Selected Countries")
     
    updateSelectInput(session, "data_type",selected = "Select DataType")
    updateSelectInput(session, "data_type2",selected = "Select DataType")
    
    updateSelectInput(session, "population",selected = "All available")
    updateSelectInput(session, "population2",selected = "All available")
    
    updateSelectInput(session, "tb_type",selected = "All available")
    updateSelectInput(session, "tb_type",selected = "All available")
    
    updateSliderInput(session, "Age_range", value = c(0,100))
    updateSliderInput(session, "Age_range2", value = c(0,100))
    
    updateDateRangeInput(session, "Date_range", start = "1970-01-01", end = Sys.Date())
    updateDateRangeInput(session, "Date_range2", start = "1970-01-01", end = Sys.Date())
    
    updateCheckboxInput(session, "age_checkbox", value = 0)
    updateCheckboxInput(session, "age_checkbox2", value = 0)
    
    updateNumericInput(session, "search_num1", value = 0)
    updateNumericInput(session, "search_num2", value = 0)
  
  })
  
#################################################################### second tab

output$data_type_info2 = renderUI({
  tags$h3(paste('Summary Table for ', toString(input$data_type2)), style ="text-align: center; color : green");
})

output$info_for_datatable2 = renderUI({
  tags$h3(paste("Data Table for", toString(input$data_type2)), style ="text-align: center; color : green");
})

####### subset data to provide options for the population input
calc_subset_for_popselection = reactive ({
  
  data = data_server[(data_server$Data_type == input$data_type2) & (data_server$Gender == input$sex2), ]
  if (input$country_selection2 != "All Countries"){
    data = data[data$Country == input$country_selection2,]
  }
  data = data[(as.numeric(substring(data$Data_Year_Min,1,4)) <= as.numeric(format(input$Date_range2[2], "%Y"))) & (as.numeric( substring(data$Data_Year_Max,1,4)) >= as.numeric(format(input$Date_range2[1], "%Y"))),]
  data = data[(data$Age_range_low <= input$Age_range2[2] & data$Age_range_high >= input$Age_range2[1]),]

  return(data)
 })

#########################select controls
### select population based on other selected factors
## also includes a function to change datatype of first tab to "select datatype" in order to toggle views
output$pop2 = renderUI({
  if (input$data_type2 != "Select DataType"){
    updateSelectInput(session, "data_type",selected = "Select DataType")  
  }
  
  datatype_of_interest = c("All available",unique(calc_subset_for_popselection()$Population1),unique(calc_subset_for_popselection()$Population2), unique(calc_subset_for_popselection()$Population3));
  selectInput("population2", "Select Population of Interest", datatype_of_interest, selected = "All available", multiple = TRUE)
})

# select datatype based on selected countries
output$data_typing2 = renderUI({
  data = data_server
  if (input$country_selection2 != "All Countries"){
    data = data[data$Country == input$country_selection2,]
  }
  selectInput("data_type2", "Data Type",choices = c('Select DataType', unique(data$Data_type)), selected = 'Select DataType')
})

################ calculations for outputting data
# subset to provide data that will be used for calculations to check overlap of input range
calc_subset = reactive({
  data = calc_subset_for_popselection()
  
 # subset based on population
 if (!"All available" %in% input$population2) {
   for (i in 1:length(input$population2)) {
     data = data[(data$Population1 == input$population2[i]) | (data$Population2 == input$population2[i]) | (data$Population3 == input$population2[i]),]
   } 
 }
 
  # exclude 0 - 100 if checkbox is checked
  if (input$age_checkbox2 == TRUE)
  {
    data = data[data$Age_range_low > 0 & data$Age_range_high < 100,]
  }
  
  return(data)
})


background_data_calc = reactive({
  data_mean = aggregate(calc_subset()$Percent * 100, by = list(calc_subset()$Organization_Site), FUN = function(x) { return(round(mean(x),2)) }) 
  data_mean = data_mean[(data_mean$x >= input$search_num1) & (data_mean$x <= input$search_num2),]
 
  return(data_mean)
})

output$summary2 = renderTable({
  data = background_data_calc()
  data = data[order(-data$x),]
  colnames(data) = c('Site', 'Average(%)')
  return(data)
},include.rownames = FALSE)

output$data_table2 = renderDataTable({
  data = calc_subset()
  data = data[data$Organization_Site %in% background_data_calc()$Group.1,]
  data = data[variables]
  colnames(data) = headers
 
  return(data)
}, options = list(columnDefs = list(list(targets = c(1:7) - 1, searchable = FALSE))))

#handler to download data on page
output$download_subset_data2 = downloadHandler (
  filename = function() {paste(toString(input$country_selection2),"_",toString(input$data_type2),"_",toString(input$sex2),"_",toString(input$population2),".csv")},
  content = function(file) {
    write.table(background_data_calc(), file, sep = ",", row.names = FALSE)
    write.table(' ', file,col.names = FALSE, row.names = FALSE, append=TRUE)  
    write.table(calc_subset(), file, sep = ",",append=TRUE, row.names = FALSE)  
  } 
)
############################################################# map stuff
# plot

# next step
# figure out how to add polygon with style using function
style = function(m) {
  style_list = c(m, color = "#2ca25f", fill = FALSE)
  return(style_list)
}

getColor = function(density){
  if (density < 2){
    return("#800026")
  }
  else if (density< 5){
    return ("#BD0026")
  }
  else if (density < 8) {
    return("#FC4E2A")
  }
  else {
    return("#FED976")
  }
}

output$testplot = renderLeaflet({
  m = leaflet(x)
  m = addTiles(m)
 
  m = addPolygons(m, color = "#000", opacity = 0.8, fillOpacity = 0.2, fillColor = getColor(x@data$density))
  
 # m = clearBounds(m)
  
  #m = addMarkers(m, lng=174.768, lat=-36.852, popup="The birthplace of R")
  m   
})


  # map stuff
  # get data rows for selected city and selected datatype for map
#  data_subset_city = reactive({
#    city = unique(data_server[data_server$Organization..Site. == toString(input$Siteselector1),]$City);
#    data_server[(data_server$City == city) & (data_server$Data.type == input$data_type1),];
#  })
  
  # obtain aggregate data
  # obtain aggregate average
#  aggregate_data_mean = reactive({
#    data_to_work_on = data_subset_city();
#    aggregate(data_to_work_on$PercentData, by = list(data_to_work_on$Organization..Site., data_to_work_on$Male, data_to_work_on$Female), FUN = mean);
#  })

# get data rows for selection  ##### use this for reference
#data_subset= reactive({
  # subset based on datatype and sex choice
 # type_sex = data_server[(data_server$Data_type == toString(input$data_type) & data_server$Gender == toString(input$sex)),]
  
  #subset based on age selection
  #age = type_sex[(type_sex$Age_range_low <= input$Age_range[2] & type_sex$Age_range_high >= input$Age_range[1]),]
  
  #######################################
  #subset based on year
  # date = age[(as.numeric(age$Data_Year_Min) <= as.numeric(format(input$Date_range[2], "%Y"))) & (as.numeric(age$Data_Year_Max) >= as.numeric(format(input$Date_range[1], "%Y"))),]
  
  #subset based on population
  ##### ask Esther and add code  
  
  # subset based on site
  #site = age[age$Organization_Site %in% input$site_selection,]
#})

# keeping for review
#output$mean = renderUI({
#  if (nrow(data_subset()) != 0) {
#    message = paste("mean: ",signif(data_of_interest_mean(), digits = 3), "%");
#    return (tags$p(message, style = "text-align: center; font-size: 18px; color : orange;"));
#  }
#  else {
#    return(tags$p('Mean not available for selection', style = "font-size: 18px; color: red"))
#  }
#})

#output$test = renderTable({
 #data = calc_subset()
 #data = bac
# data = data[data$Organization_Site %in% ]
#})

#output$test = renderText({
# input$data_type2
#  })

  
  
})
