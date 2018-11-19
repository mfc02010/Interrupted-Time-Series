library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Robust-ITS Modeling and Inference for Interrupted Time Series Data"),
  hr(),
  h4("1. Data Description"),
  
  fluidRow(
    column(10, wellPanel(
      
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      tags$em("Uploaded data must be in a *.csv file, contain only three columns (date, micro system, and one outcome of interest), 
              be collected monthly, and include at most 12 micro systems (units, clusters, organizations, etc.).", style="font-size:13px; color:blue"),
      tags$br(),
      tags$em("For more than 12 units or more than one outcome, separate the data into files with no more than 12 units and one outcome.
              An example of the data structure accepted is provided in the manual.", style="font-size:13px; color:blue"),
      tags$br(),
      tags$em("The manual can be downloaded at:", style="font-size:13px; color:blue"),
      tags$body(
        a("Robust-ITS Manual", target="blank", href= "https://www.dropbox.com/s/te5gy1cojtovevo/Manual3.pdf?dl=1", style="font-size:13px")),
      tags$br(),
      tags$em("DISCLAIMER: The authors and their organizations assume no liability for the use of and results obtained from this toolbox. Upload data at your own risk; we do not guarantee the safety of uploaded data. To safeguard data, contact the authors to obtain files to run the toolbox on your local machine.", style="font-size:12px; color:red"),
      tags$html(
         ),
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
      
    ))
  ),

  
  fluidRow(
    column(4, sliderInput("Hospital_num",  "Micro System Number:", min = 1, max = 12, value = 3)),
    column(4, textInput("nameHospital",  "Micro System Name:", value = "Choose Micro System")),
    column(4, h4("  "), actionButton("simulation", "Confirm your micro system choice",style='float:center; padding:15px; font-size:120%'))
  ),
 
  fluidRow(
    column(4, numericInput("month", "Choose starting month:", 1)),
    column(4, numericInput("year", "Choose starting year:", 2008))
    
  ),
  fluidRow(
    column(4, numericInput("t0", "Theoretical executive time point (TET):", 31)),
    column(4, numericInput("L1", "Choose candidate before TET:", 5)),
    column(4, numericInput("L2", "Choose candidate after TET:", 3))
  ),
  fluidRow(
    column(4,h4("")),
    column(4, actionButton("plotData", " Plot Data ",style='float:center; padding:15px; font-size:120%'))
  ),
  h4(" "),
   fluidRow(
    column(1,h4("")),
    column(10, align="center", wellPanel(
      tags$br(),
      plotOutput("plot")
    ))
  ),
  
  br(),
  hr(),
  
  h4("2. Statistical Model for Interrupted Time Series Data"),

  fluidRow(
    column(5,h4("")),
    column(6, actionButton("analyze", "Analyze Data", style='color:red; padding:20px; font-size:120%'))
    #column(6, downloadButton("exportResults", "Export Regression Results"))
  ),
  
  br(),
  
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
    
    column(6, h4("Change in Level"), actionButton("showChangeLevel", "Show"),wellPanel( tableOutput('tableChangeLevel')))
    
  ),
  
  fluidRow(
    column(6, h4("Change in AR Coefficient"), actionButton("showChangeAR", "Show"),wellPanel(tableOutput('tableChangeAR'))),
    
    column(6, h4("Change in Noise Variance"), actionButton("showChangeWN", "Show"),wellPanel( tableOutput('tableChangeWN')))
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
