library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Modeling and Inference for Interrupted Time Series Data"),
  hr(),
  h4("1. Data Description"),
  
  fluidRow(
    column(4, wellPanel(
      
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
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
                   '"')
      
    )),
    
    column(8, wellPanel(
      plotOutput("plot")
    ))
    
  ),
  
  
  
  fluidRow(
    column(4, sliderInput("Hospital_num",  "Micro System Number:", min = 1, max = 12, value = 3)),
    column(4, textInput("nameHospital",  "Micro System Name:", value = "Choose Micro System")),
    column(4, h4("  "), actionButton("simulation", "Confirm your micro system choice",style='float:center; padding:15px; font-size:120%'))
  ),
  
  fluidRow(
    column(6, numericInput("month", "Choose starting month:", 1)),
    column(6, numericInput("year", "Choose starting year:", 2008))
    
  ),
  fluidRow(
    column(4, numericInput("t0", "Theoretical executive time point (TET):", 31)),
    column(4, numericInput("L1", "Choose candidate before TET:", 5)),
    column(4, numericInput("L2", "Choose candidate after TET:", 5))
  ),
  fluidRow(
    column(5,h4("")),
    column(6, actionButton("plotData", " Plot Data ",style='float:center; padding:15px; font-size:120%'))
  ),
  
  hr(),
  
  h4("2. Statistical Model for Interrupted Time Series Data"),
  
  
  
  fluidRow(
    column(5,h4("")),
    column(6, actionButton("analyze", "Analyze Data", style='color:red; padding:20px; font-size:120%'))
    #column(6, downloadButton("exportResults", "Export Regression Results"))
  ),
  
  hr(),
  
  fluidRow(
    column(6, wellPanel(plotOutput("plotLogLikelihood"))),
    column(6, wellPanel(plotOutput("plotEstimateLines")))
  ),
  
  fluidRow(
    column(6, wellPanel(plotOutput("resid1"))),
    column(6, wellPanel(plotOutput("resid2")))
  ),
  
  fluidRow(
    column(6, wellPanel(plotOutput("ACF1"))),
    column(6, wellPanel(plotOutput("ACF2")))
  ),
  
  fluidRow(
    
    column(6, h4("PRE"),wellPanel( tableOutput('table1'))),
    
    column(6, h4("POST"),wellPanel( tableOutput('table2')))
  ),
  
  
  
  hr(),
  
  h4("3. Inferences"),
  
  fluidRow(
    
    #column(6, h4("Change in Intercept"), actionButton("showChangeInt", "Show"), wellPanel( tableOutput('tablechangeInt'))),
    
    column(6, h4("Change in Slope"), actionButton("showChangeSlope", "Show"), wellPanel( tableOutput('tableChangeSlope'))),
    
    column(6, h4("Change in Noise Variance"), actionButton("showChangeWN", "Show"),wellPanel( tableOutput('tableChangeWN')))
  ),
  
  fluidRow(
    column(6, h4("Change in Level"), actionButton("showChangeLevel", "Show"),wellPanel( tableOutput('tableChangeLevel'))),
    
    column(6, h4("Change in AR Coefficient"), actionButton("showChangeAR", "Show"),wellPanel(tableOutput('tableChangeAR')))
  ),
  
  hr(),
  
  h4("4. Export Results"),
  fluidRow(
    column(4, downloadButton("exportResults", "Export Regression Line Results in Part 2")),
    column(4, downloadButton("exportAnalysis", "Export Analysis Results in Part 2")),
    column(4, downloadButton("exportInference", "Export Inferences in Part 3"))
  ),
  
  hr()
  # end shinyUI  
))
