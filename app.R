## ZERO Differential Expression statistics and plots
## 
## Created by the CCI Bioinformatics team (Nisitha Jayatilleke, Pooja Venkat and Chelsea Mayoh)
## Date: 15/07/2019
## Last updated: 07/07/2020

# Import relevant libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(Rtsne)
library(ggplot2)
library(plotly)
library(tidyr)
library(RColorBrewer)

# Increase max file size
options(shiny.maxRequestSize = 500*1024^2)

# File directory
dirLoc <- getwd()

# Define UI for application
ui <- fluidPage(
  # Set shiny theme
  theme = shinytheme("flatly"),
  # Initialise ShinyJS
  useShinyjs(),
  # Navbar initialisation
  navbarPage(
    id = "tabs", 
    title = "ZERO Viewer",
    ################################
    # DE and table view -- Nisitha #
    ################################
    tabPanel(
      "Table Statistics",
      # Select mode for application
      fluidRow(
        column(
          12,
          radioButtons(
            inputId = "selectOffline",
            label = h4("Select mode:"),
            choices = list("Online" = "online", "Offline" = "offline")
          )
        )
      ),
      # Select sample to view data
      fluidRow(
        column(
          6,
          uiOutput(
            "sampleSelect"
          )
        ),
        column(
          6,
          uiOutput(
            "tableSelect"
          )
        )
      ),
      # Select patient metadata and TPM counts files
      fluidRow(
        column(
          6,
          uiOutput(
            "TPMcounts"
          )
        ),
        column(
          6,
          uiOutput(
            "patientMetadata"
          )
        )
      ),
      # Select extra histology TPMs
      fluidRow(
        column(
          12,
          selectizeInput(
            inputId = "histologySelect",
            label = h4("Select histology groups to add TPM:"),
            choices = NULL,
            multiple = T
          )
        )
      ),
      # Select genes to view
      fluidRow(
        column(
          12,
          radioButtons(
            inputId = "selectGeneInput",
            label = h4("Gene filter:"),
            choices = list("All genes" = "all", "Specified genes" = "specific", "Gene list" = "list")
          )
        )
      ),
      # Break
      fluidRow(
        br()
      ),
      # Allow selection of genes based on method
      fluidRow(
        column(
          12,
          uiOutput("selectGenes")
        )
      ),
      # Print table preview
      fluidRow(
        mainPanel(
          DTOutput("tablePreview")
        )
      ),
      # Download table button
      fluidRow(
        column(
          12,
          downloadButton("tableDownload")
        )
      ),
      # Break
      fluidRow(
        br()
      )
    ),
    ############################
    # Expression Plot -- Pooja #
    ############################ 
    tabPanel(
      "Expression Plot",
      fluidRow(
        column(
          12,
          radioButtons(
            inputId = "Select_plotfile_dot", 
            label = h4("Select file mode:"),
            choices = list("Online" = "online_plot", "Offline" = "offline_plot")
          )
        )
      ),
      fluidRow(
        column(
          6,
          uiOutput("dotTPM_file")
        ),
        column(
          6,
          uiOutput("dotPatient_file")
        )
      ),
      fluidRow(
        column(
          6,
          selectizeInput(
            inputId = "selectPatient_dot", 
            label = h4("Select Patient ID:"), 
            choices = NULL, 
            multiple = F
          )
        ),  
        column(
          6,
          selectizeInput(
            inputId = "selectgenes2_dot", 
            label = h4("Select gene to plot:"), 
            choices = NULL, 
            multiple = F
          )
        )
      ),
      fluidRow(
        column(
          6,
          selectizeInput(
            inputId = "selectPatient_dot_offline", 
            label = h4("Select Patient ID:"), 
            choices = NULL, 
            multiple = F
          )
        ),  
        column(
          6,
          selectizeInput(
            inputId = "selectgenes2_dot_offline", 
            label = h4("Select gene to plot:"), 
            choices = NULL, 
            multiple = F
          )
        )
      ),
      fluidRow(
        column(
          12, 
          plotlyOutput("plotarea_dot", height = "800px", width = "900px")
        )
      ),
      # Download plot button
      fluidRow(
        column(
          12,
          downloadButton("ExpressionDownload_dot", label = "Download as .png")
        )
      )
    ),
    ########################
    # tSNE plot -- Nisitha #
    ########################
    tabPanel(
      "tSNE Plot",
      # Select mode for application
      fluidRow(
        column(
          12,
          radioButtons(
            inputId = "tSNEselectOffline",
            label = h4("Select mode:"),
            choices = list("Online" = "online", "Offline" = "offline")
          )
        )
      ),
      fluidRow(
        column(
          6,
          radioButtons(
            inputId = "tSNEcolourSelect",
            label = "Select colour table input method:",
            choices = list(
              "Colour by specified categories" = "list",
              "Custom user input" = "user"
            )
          )
        ),
        column(
          6,
          radioButtons(
            inputId = "tSNEbroadCategories",
            label = "Select colouring catergory:",
            choices = list(
              "Colour by Diagnosis" = "specific",
              "Colour by Cancer Category" = "broad"
            )
          )
        )
      ),
      # Select upload of TPM counts, Patient diagnosis files
      fluidRow(
        column(
          4,
          uiOutput("tSNETPMCounts")
        ),
        column(
          4,
          uiOutput("tSNEPatientMetadata")
        ),
        column(
          4,
          uiOutput("tSNEColourTable")
        )
      ),
      # Select sample for plotting 
      fluidRow(
        column(
          12,
          uiOutput("tSNEsampleSelect")
        )
      ),
      # Output tSNE plotly
      fluidRow(
        column(
          12,
          plotlyOutput("tSNEPlot", height = "800px", width = "1200px")
        )
      ),
      # Download tSNE plot button
      fluidRow(
        column(
          12,
          downloadButton("tSNEDownload", label = "Download as .png")
        )
      )
    ),
    #############################
    # Signature plot -- Nisitha #
    #############################
    tabPanel(
      "Signature Plot",
      # Select mode for application
      fluidRow(
        column(
          12,
          radioButtons(
            inputId = "SigPlotSelectOffline",
            label = h4("Select mode:"),
            choices = list("Online" = "online", "Offline" = "offline")
          )
        )
      ),
      # Select upload of TPM counts, Patient diagnosis and gene signature list files
      fluidRow(
        column(
          4,
          uiOutput("SigPlotTPMCounts")
        ),
        column(
          4,
          uiOutput("SigPlotPatientMetadata")
        ),
        column(
          4,
          uiOutput("SigPlotSignatureGenes")
        )
      ),
      # Select sample and signature for plotting
      fluidRow(
        column(
          6,
          uiOutput("SigPlotSampleSelect")
        ),
        column(
          6,
          uiOutput("SigPlotSignatureFile")
        )
      ),
      # Output signature plotly
      fluidRow(
        column(
          12,
          plotlyOutput("SigPlot", height = "800px", width = "1200px")
        )
      ),
      # Download signature plot button
      fluidRow(
        column(
          12,
          downloadButton("SigPlotDownload", label = "Download as .png")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Use shinyjs
  useShinyjs()
  
  # Import table view script
  source("scripts/tableView.R", local = T)
  
  # Import expression plot script
  source("scripts/ExpressionPlot_shiny.R", local = T)
  
  # Import tSNE plot script
  source("scripts/tSNEPlot_shiny.R", local = T)
  
  # Import signautre script
  source("scripts/SignaturePlot_shiny.R", local = T)
  
  # Stop application (required for RInno)
  if(!interactive()){
    session$onSessionEnded(function(){
      stopApp()
      q("no")
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

