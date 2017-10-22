
library(shiny)
library(leaflet)

                    
fluidPage(
  
  checkboxGroupInput("mapOption",
                     "Choose which data to add",
                     choices = c("Missing Children Data" = "miss",
                                 "Incidents Data" = "inci"),
                     selected = c("miss","inci")),
  
  leafletOutput("mymap"),
  
  selectInput("state", "Sort by state", choices = sort(c(missing$`Missing State`)),
              multiple = TRUE),
  
  selectInput("city", "Sort by city", choices = sort(c(missing$`Missing City`,
                                                       attempts$`Incident City`)),
              multiple = TRUE),
  
  selectInput("zip", "Sort by zip", choices = sort(c(missing$ZIP,
                                                       attempts$ZIP)),
              multiple = TRUE),
  
  selectInput("caseID", "Sort by case id", choices = sort(c(missing$`Case Number`,
                                                     attempts$`Case Number`)),
              multiple = TRUE)
  
  )