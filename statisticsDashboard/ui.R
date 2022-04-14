library(shinydashboard)
library(shiny)
library(tidyverse)
library(ggtext)

header <- dashboardHeader(
  title = "Statistics Visualizations"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Normal Distribution",
             tabName = "zDist"),
    menuItem("Student's t Distribution",
             tabName = "tDist"),
    menuItem("Chi Squared Distribution",
             tabName="chiDist"),
    menuItem("F Distribution",
             tabName ="fDist")
  )
  )
  
  
body <-  dashboardBody(
  tabItems(
    tabItem(tabName = "zDist",
            fluidRow(
              column(width = 4,
                     box( title = "Plot", width = NULL,
                sliderInput("zConfInt", "Confidence Interval", min = 0.5, max = 1, value = 0.95, step = 0.01),
                radioButtons("zTail","Select tails", 
                             choices = list("Two Tail" = 0,"Left Tail" = 1,"Right Tail" = 2)))
                ),
              column( width = 8,
                      box(titile = "Inputs", width = NULL,
                plotOutput("zDistPlot"))
                )
              )),
    
    tabItem(tabName = "tDist",
            fluidRow(
              column(width = 4,
                     box( title = "Plot", width = NULL,
                          sliderInput("tConfInt", "Confidence Interval", min = 0.5, max = 1, value = 0.95, step = 0.01),
                          numericInput("tdf","Degrees of Freedom", min = 1, step = 1, value = 5),
                          radioButtons("tTail","Select tails", 
                                       choices = list("Two Tail" = 0,"Left Tail" = 1,"Right Tail" = 2)))
                     ),
              column(width = 8,
                      box(titile = "Inputs", width = NULL,
                          plotOutput("tDistPlot"))
                     )
              )),
    
    tabItem(tabName = "chiDist",
            fluidRow(
              column(width = 4,
                     box(title = "Input",
                         width = NULL,
                         sliderInput("chiConfInt", "Confidence Interval", min = 0.5, max = 1, value = 0.95, step = 0.01),
                         numericInput("chidf","Degrees of Freedom", min = 1, step = 1, value = 5))
                     ),
                     column( width = 8,
                             box(titile = "Inputs", width = NULL,
                                 plotOutput("chiDistPlot")
                             ))
            )),
    tabItem(tabName = "fDist",
            fluidRow(
              column(width = 4,
                     box(title = "Input",
                         width = NULL,
                         sliderInput("fConfInt", "Confidence Interval", min = 0.5, max = 1, value = 0.95, step = 0.01),
                         numericInput("fdf1","Degrees of Freedom", min = 1, step = 1, value = 20),
                         numericInput("fdf2","Degrees of Freedom", min = 1, step = 1, value = 20))
              ),
              column( width = 8,
                      box(titile = "Inputs", width = NULL,
                          plotOutput("fDistPlot")
                      ))
            ))
    ))



ui <- dashboardPage(header,sidebar,body)          

