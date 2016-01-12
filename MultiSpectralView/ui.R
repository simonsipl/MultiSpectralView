
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(
  
  navbarPage("Navigation",
             tabPanel("Spectral",
                      sidebarLayout(position ="left",
                                    sidebarPanel( 
                                      helpText("Choose Fluorochromes"),
                                      uiOutput("fluoro"),
                                      
                                      
                                      helpText("Choose Excitation Filter"),
                                      uiOutput("filterex"),
                                      
                                      helpText("Choose Emission Filter"),
                                      uiOutput("filterem"),
                                      
                                      
                                      helpText("Choose Light Source"),
                                      uiOutput("lamp")
                                    ),
                                    mainPanel(
                                      
                                      plotOutput('plot'),
                                      tableOutput('filterTable'),
                                      plotOutput('newPlot')
                                      
                                    )
                      )
                      
             ),
             tabPanel("Upload",
                      sidebarLayout(position="left",
                                    sidebarPanel(
                                      fileInput('file1', 'Choose CSV File',
                                                accept=c('text/csv', 
                                                         'text/comma-separated-values,text/plain', 
                                                         '.csv')),
                                      tags$hr(),
                                      checkboxInput('header', 'Header', TRUE),
                                      radioButtons('sep', 'Separator',
                                                   c(Comma=',',
                                                     Semicolon=';',
                                                     Tab='\t'),
                                                   ','),
                                      radioButtons('quote', 'Quote',
                                                   c(None='',
                                                     'Double Quote'='"',
                                                     'Single Quote'="'"),
                                                   '"'),
                                      br(),
                                      radioButtons('wave', 'Wave',
                                                   c(
                                                     Lamp = 'lamp',
                                                     Fluorochromes = 'Fluorochromes',
                                                     Filter = 'Filter'
                                                     
                                                   )
                                      ),
                                      uiOutput('filtertype'),
                                      
                                      p("Choose what kind of data you want to add, then click a button below"),
                                      br(),
                                      
                                      actionButton("saveButton","Add!")
                                      
                                    ),
                                    mainPanel(
                                      plotOutput("spectra"),
                                      verbatimTextOutput("nText"),
                                      tableOutput('contents')
                                      
                                    )
                                    
                                    
                      )
             )
             
             
  ))



