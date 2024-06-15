library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(janitor)
library(readxl)
library(reactable)
library(shinydashboard)
library(highcharter)
library(countrycode)

seed <- as.numeric(commandArgs(trailingOnly = T))

hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

ui <- dashboardPage(skin = 'purple',
  dashboardHeader(title = 'COVID Dashboard'),
  dashboardSidebar(sidebarMenu(id = 'tabs',
                               menuItem('Overview', tabName = 'all', icon = icon('map')),
                               menuItem('Charts', tabName = 'chart', icon = icon('chart-simple'))),
                   pickerInput("regions",
                               HTML("<b>Regions</b>"),
                               choices  = NULL,
                               selected = NULL,
                               options = pickerOptions(
                                 actionsBox = TRUE, 
                                 liveSearch = TRUE,
                                 size = 10,
                                 container = "body",
                                 selectedTextFormat = "count > 1"),
                               multiple = T),
                   pickerInput("countries",
                               HTML("<b>Countries</b>"),
                               choices  = NULL,
                               selected = NULL,
                               options = pickerOptions(
                                 actionsBox = TRUE, 
                                 liveSearch = TRUE,
                                 size = 10,
                                 container = "body",
                                 selectedTextFormat = "count > 1"),
                               multiple = T),
                   actionButton('apply', 'Apply', width = '87%')),
  dashboardBody(
    tabItems(
      tabItem('all',
              fluidRow(
                infoBoxOutput('totalDeaths'),
                infoBoxOutput('totalRecovered'),
                infoBoxOutput('totalConfirmed')),
              fluidRow(
                box(width = 12,
                    highchartOutput('covidMap')),
                box(width = 12,
                    reactableOutput('allTable')))),
      tabItem('chart',
              renderHighchart('allChart'))
    )
  )
)

server <- function(input, output, session) {
  
  dat <- read.csv('data from kaggle.com') %>%
    clean_names() %>%
    select(!contains(c('new', 'last', 'x1'))) %>%
    mutate(`% Death` = round(deaths/confirmed*100,2),
           `% Recovered` = round(recovered/confirmed*100,2),
           `% Active` = round(active/confirmed*100,2),
           country_code = countrycode(country_region, origin = 'country.name', destination = 'iso2c')) %>%
    arrange(desc(confirmed)) %>%
    select(!deaths_100_recovered) %>%
    rename(`WHO Region` = who_region, 'Confirmed' = confirmed, 'Deaths' = deaths, 'Recovered' = recovered, 'Active' = active, `Deaths Per 100 Cases` = deaths_100_cases,
           `Recovered Per 100 Cases` = recovered_100_cases, `Country Region` = 'country_region')
  
  updatePickerInput(session, 'regions', choices = unique(dat$`WHO Region`), selected = unique(dat$`WHO Region`))
  
  observe({
    
    opts <- dat %>%
      filter(`WHO Region` %in% input$regions)
    updatePickerInput(session, 'countries', choices = unique(opts$`Country Region`), selected = unique(opts$`Country Region`))
    
  })
  
  datDf <- eventReactive(input$apply, {
    
    if(input$apply == 0){
      
      dat
      
    } else {
      
      dat %>%
        filter(`WHO Region` %in% input$regions,
               `Country Region` %in% input$countries)
      
    }
    
  }, ignoreNULL = F)
  
  output$covidMap <- renderHighchart({
    
    temp <- datDf() %>%
      rename('country' = `Country Region`)
    
    hcmap(
      map = "custom/world-highres3",
      data = temp,
      joinBy = c('iso-a2', 'country_code'),
      value = 'Confirmed',
      dataLabels = list(enabled = TRUE, pointFormat = '{point.name}')) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_tooltip(pointFormat = '<span style="color:{point.color}">\u25CF</span> {point.name}: {point.value}', headerFormat = NULL)
    
  })
  
  output$allChart <- renderHighchart({

    temp <- datDf() %>%
      group_by(`WHO Region`) %>%
      summarise(total = mean(`% Death`)) %>%
      mutate(total = round(total, 2)) %>%
      arrange(desc(total))

    hchart(temp, 'column', hcaes(x = `WHO Region`, y = total)) %>%
      hc_title(text = 'Death Rates', align = 'center') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_tooltip(pointFormat = '<span style="color:{point.color}">\u25CF</span> {point.WHO Region}: <b>{point.total}%</b>', headerFormat = NULL)

  })
  
  output$allTable <- renderReactable({
    
    temp <- datDf()
    
    reactable(dat, groupBy = 'WHO Region',
              defaultColDef = colDef(format = colFormat(separators = T)),
              columns = list(
                Confirmed = colDef(aggregate = 'sum'),
                Deaths = colDef(aggregate = 'sum'),
                Recovered = colDef(aggregate = 'sum'),
                Active = colDef(aggregate = 'sum'),
                `Deaths Per 100 Cases` = colDef(aggregate = 'mean', format = colFormat(digits = 2), style = JS("function(rowInfo){
                                                                                                               if(rowInfo.aggregated == true){
                                                                                                                return{fontWeight: 'bold'}
                                                                                                               }}")),
                `Recovered Per 100 Cases` = colDef(aggregate = 'mean', format = colFormat(digits = 2)),
                `% Death` = colDef(aggregate = 'mean', format = colFormat(digits = 2, suffix = '%')),
                `% Recovered` = colDef(aggregate = 'mean', format = colFormat(digits = 2, suffix = '%')),
                `% Active` = colDef(aggregate = 'mean', format = colFormat(digits = 2, suffix = '%'))))
    
  })
  
  output$totalDeaths <- renderInfoBox({
    
    temp <- dat %>%
      adorn_totals('row') %>%
      arrange(desc(Deaths)) %>%
      select(Deaths) %>%
      head(1)
    
    infoBox(title = 'Total COVID World Deaths', color = 'purple',
            paste0(formatC(as.numeric(temp), format = 'd', big.mark = ',')), width = 4, icon = icon('biohazard'), fill = T)
    
  })
  
  output$totalRecovered <- renderInfoBox({
    
    temp <- dat %>%
      adorn_totals('row') %>%
      arrange(desc(Recovered)) %>%
      select(Recovered) %>%
      head(1)
    
    infoBox(title = 'Total COVID World Recoveries', color = 'purple',
            paste0(formatC(as.numeric(temp), format = 'd', big.mark = ',')), width = 4, icon = icon('star-of-life'), fill = T)
    
  })
  
  output$totalConfirmed <- renderInfoBox({
    
    temp <- dat %>%
      adorn_totals('row') %>%
      arrange(desc(Confirmed)) %>%
      select(Confirmed) %>%
      head(1)
    
    infoBox(title = 'Total COVID World Recoveries', color = 'purple',
            paste0(formatC(as.numeric(temp), format = 'd', big.mark = ',')), width = 4, icon = icon('check'), fill = T)
    
  })
  
  session$onSessionEnded(function() {
    quit()
  })
  
}

shinyApp(ui, server)

#run this once so get the exe for the dashboard
#shinyShortcut::shinyShortcut()