#----------------------#
#        ui.R          #
#----------------------#

#library(shinydashboard)
library(rsconnect)
library(shiny)
library(DT)
library(readxl)
library(bs4Dash)
library(shinyjs)
library(stringr)
library(readxl)

#Change

#---------------
#     Header
#---------------

header <- dashboardHeader(
  title = "Quality Checks"
  )

#---------------
#     Sidebar
#---------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Homepage", tabName = "home", icon = icon("home", lib = "font-awesome")),
    menuItem("Upload ALS File", tabName = "uploadals", icon = icon("table",lib = "font-awesome")),
    menuItem("Fields Table", tabName = "fieldstables", icon = icon("table",lib = "font-awesome")),
    #menuItem("CRFDraft Table", tabName = "crftables", icon = icon("table",lib = "font-awesome")),
    menuItem("Forms Table", tabName = "formstables", icon = icon("table",lib = "font-awesome"))
  )
)

#---------------
#     Body
#---------------

body <- dashboardBody(
  
  #includeCSS("custom.css"),
  #tags$style(HTML(table_css)),
  
  tabItems(
    #Homepage Tab Content
    tabItem(tabName = "home",
            h2("Homepage Content"),
            br(),
              fluidRow(
                bs4ValueBoxOutput("numberBox"),
                bs4ValueBoxOutput("finishedBox"),
                bs4ValueBoxOutput("progressBox")
              ),
            h3("Tasks"),
            box(status = "info",width = 12, collapsible = FALSE,
                maximizable = TRUE,style = "height:500px; overflow-y: scroll;overflow-y: scroll;",
            fluidRow(
              column(8,uiOutput("accordion")),
              column(8,uiOutput("accordionlinknext")),
              column(8,uiOutput("accordioncf")),
              column(8,uiOutput("accordionsubjectcf")),
              column(8,uiOutput("accordionaecf")),
              column(8,uiOutput("accordiondatadict")),
              column(8,uiOutput("accordionsuppterm")),
              column(8,uiOutput("accordionquerync")),
              column(8,uiOutput("accordionspecify")),
              column(8,uiOutput("accordionallowadd")),
              column(8,uiOutput("accordionbreaksignature")),
              column(8,uiOutput("accordionSAE_Always")),
              column(8,uiOutput("accordionDays")),
              column(8,uiOutput("accordionVerify")),
              column(8,uiOutput("accordionsignaturereq")),
              column(8,uiOutput("accordionstandardrange")),
              column(8,uiOutput("accordionlabcf")),
              column(8,uiOutput("accordionobslab")),
              column(8,uiOutput("accordionunsched")),
              column(8,uiOutput("accordioncffolder")),
              column(8,uiOutput("accordiondictterm"))
           
            )
            
              
            )
            
            ),
    tabItem(tabName = "uploadals",
            uiOutput("uploadxls")
            ),
    tabItem(tabName = "fieldstables",
            h2("Fields Table"),
              DT::dataTableOutput("fields")
                ),
    # tabItem(tabName = "crftables",
    #         h2("CRFDraft Table"),
    #         DT::dataTableOutput("crfdraft")
    #         ),
    tabItem(tabName = "formstables",
            h2("Forms Table"),
            DT::dataTableOutput("forms"))
  )
)

#---------------
#     Dashboard
#---------------

ui <- dashboardPage(header,sidebar,body)



