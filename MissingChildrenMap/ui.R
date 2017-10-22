
library(shiny)
library(shinydashboard)
library(leaflet)


shinyUI(
  dashboardPage(
    dashboardHeader(title = "Header 1"),
    dashboardSidebar(collapsed = FALSE,
                     sidebarMenu(
                       menuItem("Drop Down 1", icon = icon("chevron-right"), startExpanded = TRUE,
                                checkboxGroupInput("mapOption",
                                                   "Choose which data to add",
                                                   choices = c("Missing Children Data" = "miss",
                                                               "Incidents Data" = "inci"),
                                                   selected = c("miss","inci"))  
                       )
                     ),
                     sidebarMenu(
                       menuItem("Drop Down 2", icon = icon("chevron-right"), startExpanded = TRUE,
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
                     )
    ),
    dashboardBody(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("mymap")
    )
  )
)