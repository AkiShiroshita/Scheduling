# libraries ----

suppressPackageStartupMessages({
  library(readr)
  library(shiny)
  library(googlesheets4)
  library(shinydashboard)
  library(shinyjs)
  library(ggplot2)
  library(dplyr)
  library(googledrive)
  library(DT)
})
gs4_auth(cache = ".secrets", email = "akihirokun8@gmail.com")

# setup ----

# functions ----

## google sheet---

# outpatient

SHEET_out <-
  "https://docs.google.com/spreadsheets/d/18UV22-0k5sbWOUQdRWU-E1vRW7R3xLcpp3lsFICg6yM/edit#gid=0"
# inpatient
SHEET_in <-
  "https://docs.google.com/spreadsheets/d/1uLBJ-0l9g-1EpC8tiPHjBY9F64qtYXK-b48XdC6UpHw/edit#gid=0"

# user interface ----

skin_color <- "green"

## tabs ----

# outpatient

outpatient_tab <- tabItem(
  tabName = "outpatient_tab",
  box(
    width = 12,
    title = "Name",
    collapsible = TRUE,
    solidHeader = TRUE,
    textInput(
      "name_out",
      "What is your name?",
      value = "",
      placeholder = "Your full name (Akihiro Shiroshita)"
    )
    ),
  box(
    width = 12,
    title = "Month",
    collapsible = TRUE,
    solidHeader = TRUE,
    radioButtons(
    "month_out",
    "What month is your preference for?",
    choices = c("January",
                  "February",
                  "March",
                  "April",
                  "May",
                  "June",
                  "July",
                  "August",
                  "September",
                  "October",
                  "November",
                  "December"),
    selected = character(0)
    )
    ),
  box(
    width = 12,
    title = "Days",
    collapsible = TRUE,
    solidHeader = TRUE,
    textInput(
      "dates_out",
      "List your unavailable dates?",
      value = "",
      placeholder = "11,12,15,16"
    )
    ),
    
    actionButton("preference_submit_out", "Submit"),
    textOutput("response_out")
)

# inpatient

inpatient_tab <- tabItem(
  tabName = "inpatient_tab",
  box(
    width = 12,
    title = "Name",
    collapsible = TRUE,
    solidHeader = TRUE,
    textInput(
      "name_in",
      "What is your name?",
      value = "",
      placeholder = "Your full name (Akihiro Shiroshita)"
    )
  ),
  box(
    width = 12,
    title = "Month",
    collapsible = TRUE,
    solidHeader = TRUE,
    radioButtons(
      "month_in",
      "What month is your preference for?",
      choices = c("January",
                  "February",
                  "March",
                  "April",
                  "May",
                  "June",
                  "July",
                  "August",
                  "September",
                  "October",
                  "November",
                  "December"),
      selected = character(0)
    )
  ),
  box(
    width = 12,
    title = "Days",
    collapsible = TRUE,
    solidHeader = TRUE,
    textInput(
      "dates_in",
      "List your unavailable dates?",
      value = "",
      placeholder = "11,12,15,16"
    )
  ),
  
  actionButton("preference_submit_in", "Submit"),
  textOutput("response_in")
)

## UI ----
ui <- dashboardPage(
  skin = skin_color,
  dashboardHeader(title = "Web page for shift scheduling", 
                  titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
  ),
  dashboardSidebar(
    # https://fontawesome.com/icons?d=gallery&m=free
    sidebarMenu(
      id = "tabs",
      menuItem("Outpatient", tabName = "outpatient_tab", icon = icon("clinic-medical")),
      menuItem("Inpatient", tabName = "inpatient_tab", icon = icon("hospital-user"))
    )),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      # links to files in www/
      tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"), 
      tags$link(rel = "stylesheet", type = "text/css", href = "radio-table.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), 
      tags$script(src = "radio-table.js"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      outpatient_tab,
      inpatient_tab
    )
  )
)

# server ----
server <- function(input, output, session) {
  
  append_data_out <- reactive({
    data.frame(name = input$name_out,
               month = input$month_out,
               date1 = input$dates_out)}) 
  
  append_data_in <- reactive({
    data.frame(name = input$name_in,
               month = input$month_in,
               date2 = input$dates_in)}) 
  
  observeEvent(input$preference_submit_out, {
    googlesheets4::sheet_append(SHEET_out, data = append_data_out(), sheet = 1)
    output$response_out <- renderText("Thank you!")
  })
  
  observeEvent(input$preference_submit_in, {
    googlesheets4::sheet_append(SHEET_in, data = append_data_in(), sheet = 1)
    output$response_in <- renderText("Thank you!")
  })
  
}

shinyApp(ui, server)