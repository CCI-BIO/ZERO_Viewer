## tSNEPlot_shiny.R
## Server logic for creating a tSNE plot
##
## Created by Nisitha Jayatilleke
## Date: 18/07/2019
## Last updated: 04/08/2020

# Initialise variable 
tsne <<-  NULL

# Initialise widgets
observeEvent(
  c(input$tSNEselectOffline),
  {
    shinyjs::disable("tSNEDownload")
    
    # Hide plot
    shinyjs::hide("tSNEPlot")
    
    # Hide and reset widgets
    shinyjs::hide("tSNEColourSelect")
    shinyjs::hide("tSNEsampleSelect")
    shinyjs::hide("tSNEperplexitySelect")
    shinyjs::hide("tSNECategoryColourSelect")
    shinyjs::hide("tSNEColourTable")
    
    output$tSNEColourSelect <- renderUI({
      selectInput(
        inputId = "tSNEColourSelect2", 
        label = NULL,
        choices = "NA"
      )
    })
    output$tSNEsampleSelect <- renderUI({
      selectInput(
        inputId = "tSNEsampleSelect2", 
        label = NULL,
        choices = "NA"
      )
    })
    output$tSNEperplexitySelect <- renderUI({
      selectInput(
        inputId = "tSNEperplexitySelect2", 
        label = NULL,
        choices = "NA"
      )
    })
    output$tSNECategoryColourSelect <- renderUI({
      selectInput(
        inputId = "tSNECategoryColourSelect2", 
        label = NULL,
        choices = "NA"
      )
    })
    output$tSNEColourTable <- renderUI({
      selectInput(
        inputId = "tSNEColourTable2", 
        label = NULL,
        choices = "NA"
      )
    })
    
    # Online widgets
    if(input$tSNEselectOffline == "online"){
      shinyjs::show("tSNEbroadCategories")
      output$tSNETPMCounts <- renderUI({
        selectInput(
          inputId = "tSNETPMCounts2", 
          label = NULL,
          choices = "NA"
        )
      })
      output$tSNEPatientMetadata <- renderUI({
        selectInput(
          inputId = "tSNEPatientMetadata2", 
          label = NULL,
          choices = "NA"
        )
      })
      shinyjs::hide("tSNETPMCounts")
      shinyjs::hide("tSNEPatientMetadata")
    } else if(input$tSNEselectOffline == "offline"){
      # Offline widgets
      output$tSNETPMCounts <- renderUI({
        fileInput(
          inputId = "tSNETPMCounts2",
          label = h4("Upload TPM counts file:")
        )
      })
      output$tSNEPatientMetadata <- renderUI({
        fileInput(
          inputId = "tSNEPatientMetadata2",
          label = h4("Upload sample metadata file:")
        )
      })
      shinyjs::show("tSNETPMCounts")
      shinyjs::show("tSNEPatientMetadata")
    }
  }
)

# Online mode
observeEvent(
  c(input$tSNEselectOffline),
  {
    if(input$tSNEselectOffline == "online"){
      tryCatch(
        expr = {
          # Create variable for tSNE patient metadata
          tSNEpatientMetadata <<- reactive({
            tryCatch(
              expr = {
                check_tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
                colnames(check_tpm) <- gsub(pattern="\\.",replacement="-",colnames(check_tpm))
                metadata <- read.delim(paste(dirLoc, "Patients_Diagnosis.txt", sep = ""), header = T, stringsAsFactors = F)
                metadata <- metadata[which(metadata[,1] %in% colnames(check_tpm)),]
                return(metadata)
              },
              error = function(e){
                return(NULL)
              }
            )
          })
          # Create variable for tSNE patient tpm counts
          tSNETPM <<- reactive({
            tryCatch(
              expr = {
                tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
                return(tpm)
              },
              error = function(e){
                return(NULL)
              }
            )
          })
        },
        error = function(e){
          tSNEpatientMetadata <<- reactive({return(NULL)})
          tSNETPM <<- reactive({return(NULL)})
          return(NULL)
        }
      )
    }
  }
)

# Offline mode
observeEvent(
  c(input$tSNEselectOffline, input$tSNETPMCounts2, input$tSNEPatientMetadata2),
  {
    if(input$tSNEselectOffline == "offline"){
      tSNEpatientMetadata <<- reactive({return(NULL)})
      tSNETPM <<- reactive({return(NULL)})
      inFile1 <- input[["tSNETPMCounts2"]]
      if(is.null(inFile1) || inFile1 == "NA"){
        tSNETPM <<- reactive({return(NULL)})
        return(NULL)
      } else {
        tryCatch(
          expr = {
            # Create variable for tSNE patient tpm counts
            tSNETPM <<- reactive({
              tpm <- read.delim(inFile1$datapath, sep = "\t", header = T, row.names = 1)
              return(tpm)
            })
          },
          error = function(e){
            tSNETPM <<- reactive({return(NULL)})
            return(NULL)
          }
        )
      }
      inFile2 <- input[["tSNEPatientMetadata2"]]
      if(is.null(inFile2)){
        return(NULL)
      } else {
        tryCatch(
          expr = {
            # Create variable for tSNE patient metadata
            tSNEpatientMetadata <<- reactive({
              check_tpm <- read.delim(inFile1$datapath, sep = "\t", header = T, row.names = 1)
              colnames(check_tpm) <- gsub(pattern="\\.",replacement="-",colnames(check_tpm))
              metadata <- read.delim(inFile2$datapath, header = T, stringsAsFactors = F)
              metadata <- metadata[which(metadata[,1] %in% colnames(check_tpm)),]
              return(metadata)
            })
          },
          error = function(e){
            tSNEpatientMetadata <<- reactive({return(NULL)})
            tSNEpatientMetadataAlt <<- reactive({return(NULL)})
          }
        )
      }
    }
  }
)

# Populate colour selection variables 
observeEvent(
  c(input$tSNEselectOffline, input$tSNETPMCounts2, input$tSNEPatientMetadata2),
  {
    tryCatch(
      expr = {
        if(!is.null(tSNETPM()) & !is.null(tSNEpatientMetadata())){
          shinyjs::show("tSNEColourSelect")
          shinyjs::show("tSNEsampleSelect")
          shinyjs::show("tSNEperplexitySelect")
          output$tSNEColourSelect <- renderUI({
            radioButtons(
              inputId = "tSNEColourSelect2",
              label = h4("Select colouring method:"),
              choices = list(
                "Colour by metadata categories" = "list",
                "Custom colouring" = "user"
              )
            )
          })
          sampleMetadata <- tSNEpatientMetadata()
          # sampleMetadata[,1] <- gsub(pattern = "-", replacement = ".", x = sampleMetadata[,1])
          output$tSNEsampleSelect <- renderUI({
            sampleSelectList <- sampleMetadata[,1]
            names(sampleSelectList) <- sampleSelectList
            selectizeInput(
              inputId = "tSNEsampleSelect2",
              label = h4("Select sample ID to highlight:"),
              choices = c("None", sampleSelectList),
              multiple = F,
              selected = "None"
            )
          })
          output$tSNEperplexitySelect <- renderUI({
            numericInput(
              inputId = "tSNEperplexitySelect2", 
              label = h4("Select perplexity for tSNE:"), 
              value = 20, 
              min = 1, 
              step = 1
            )
          })
        }
      },
      error = function(e){
        return(NULL)
      }
    )
  }
)

observeEvent(
  c(input$tSNEselectOffline, input$tSNEColourSelect2),
  {
    shinyjs::hide("tSNECategoryColourSelect")
    shinyjs::hide("tSNEColourTable")
    if(!is.null(input$tSNEColourSelect2)){
      if(input$tSNEColourSelect2 != "NA"){
        if(input$tSNEColourSelect2 == "list"){
          output$tSNECategoryColourSelect <- renderUI({
            selectInput(
              inputId = "tSNECategoryColourSelect2",
              label = h4("Select colouring catergory:"),
              choices = NULL, 
              multiple = F
            )
          })
          shinyjs::show("tSNECategoryColourSelect")
          shinyjs::hide("tSNEColourTable")
          sampleMetadata <- tSNEpatientMetadata()
          output$tSNECategoryColourSelect <- renderUI({
            selectInput(
              inputId = "tSNECategoryColourSelect2",
              label = h4("Select colouring catergory:"),
              choices = colnames(sampleMetadata)[-1], 
              multiple = F
            )
          })
        } else if(input$tSNEColourSelect2 == "user"){
          shinyjs::hide("tSNECategoryColourSelect")
          shinyjs::show("tSNEColourTable")
          output$tSNEColourTable <- renderUI({
            fileInput(
              inputId = "tSNEColourTable2",
              label = h4("Upload custom sample metadata file:")
            )
          })
        }
      }
    }
  }
)

# Create log table for rtsne 
observeEvent(
  c(input$tSNEselectOffline, input$tSNETPMCounts2, input$tSNEPatientMetadata2),
  {
    tryCatch(
      expr = {
        if(!is.null(tSNETPM()) & !is.null(tSNEpatientMetadata())){
          # Create TPM object
          tSNETpmObject <<- reactive({
            tpm <- tSNETPM()
            tpm <- tpm[,-1]
            colnames(tpm) <- gsub(pattern = "\\.", replacement = "-", colnames(tpm))
            expTpm <- t(tpm)
            expTpm[expTpm == 0] <- 0.0001
            logTPM <- log2(expTpm)
            return(logTPM)
          })
        }
      },
      error = function(e){
        tSNETpmObject <<- reactive({return(NULL)})
        return(NULL)
      }
    )
  }
)

observeEvent(
  c(input$tSNEperplexitySelect2),
  {
    if(!is.null(tSNETPM()) & !is.null(tSNEpatientMetadata()) & !is.null(input$tSNEperplexitySelect2)){
      tsne <<- NULL
      if(!is.null(tSNETpmObject())){
        tryCatch(
          expr = {
            # Create tsne output
            perplexity <- input$tSNEperplexitySelect2
            logTPM <- tSNETpmObject()
            set.seed(1234)
            tsne <<- Rtsne(logTPM, dims = 2, perplexity = perplexity, verbose = TRUE, max_iter = 500, check_duplicates = F)
          },
          error = function(e){
            ####### ADD ERROR MESSAGE FOR LOW PERPLEXITY HERE #######
            return(NULL)
          }
        )
      }
    }
  }
)

# Plot trigger
observeEvent(
  c(input$tSNEselectOffline, input$tSNEColourSelect2, input$tSNECategoryColourSelect2, input$tSNEperplexitySelect2, input$tSNEsampleSelect2, input$tSNEColourTable2),
  {
    if(!is.null(tSNETPM()) & !is.null(tSNEpatientMetadata()) & !is.null(input$tSNEperplexitySelect2) & !is.null(tsne) & !is.null(input$tSNECategoryColourSelect2)){
      if(input$tSNEColourSelect2 == "list"){
        if(input$tSNECategoryColourSelect2 != "NA"){
          if(input$tSNECategoryColourSelect2 != ""){
            sampleMetadata <- tSNEpatientMetadata()
            categoryToColour <- input$tSNECategoryColourSelect2
            groupList <- sampleMetadata[,categoryToColour]
            
            if(input$tSNEselectOffline == "online"){
              colourTable <- read.delim(paste(dirLoc, "cancertype_colours.txt", sep = ""), header = T, stringsAsFactors = F)
              colourTable <- colourTable[which(colourTable[,1] %in% unique(groupList)),]
              colourVector <- colourTable[,2]
              names(colourVector) <- colourTable[,1]
            } else if(input$tSNEselectOffline == "offline"){
              numColours <- length(unique(groupList))
              colourTable <- matrix(ncol = 2, nrow = numColours)
              colourTable[,1] <- unique(groupList)
              getPalette <- colorRampPalette(brewer.pal(n = 9, name = "Set1"))
              colourTable[,2] <- getPalette(numColours)
              colourTable <- as.data.frame(colourTable)
              colourVector <- colourTable[,2]
              names(colourVector) <- colourTable[,1]
            }
            
            tsne_points <<- as.data.frame(tsne$Y)
            
            logTable <- tSNETpmObject()
            
            tryCatch(
              expr = {
                tsne_points <<- cbind(tsne_points, SampleID = rownames(logTable))
                m <- match(sampleMetadata[,1], tsne_points$SampleID)
                tsne_points <<- cbind(tsne_points, sampleMetadata[m,-1])
                
                # Create ggplot string
                stringToParse <- "ggplot(tsne_points, aes(x = V1, y = V2, SampleID = SampleID"
                for(i in 1:ncol(sampleMetadata[m,-1])){
                  stringToParse <- paste(stringToParse, ", ", colnames(sampleMetadata[m,-1])[i], " = ", colnames(sampleMetadata[m,-1])[i], sep = "")
                }
                stringToParse <- paste(stringToParse, ")) + geom_point(aes(colour = ", categoryToColour, ")) + scale_colour_manual(values = colourVector) + labs(colour = 'Cancer Type') + theme_classic()", sep = "")
                
                p <- eval(parse(text = stringToParse))
                
                filename <- "tsne.png"
                
                id <- input$tSNEsampleSelect2
                if(id != "None"){
                  p <- p + geom_point(data = tsne_points[tsne_points$SampleID == id,], size = 3, shape = 8)
                  filename <- paste("tsne_", id, "_highlighted.png", sep = "")
                }
                
                # Create download handler
                output$tSNEDownload <- downloadHandler(
                  filename = function(){filename},
                  content = function(file){ggsave(filename = file, plot = p, device = "png", width = 8)}
                )
                # Enable download button
                shinyjs::enable("tSNEDownload")
                
                # Plotly output
                output$tSNEPlot <- renderPlotly({
                  ggplotly(p)
                })
                shinyjs::show("tSNEPlot")
              },
              error = function(e){
                return(NULL)
              }
            )
          }
        }
      } else if(input$tSNEColourSelect2 == "user"){

        inFile3 <- input[["tSNEColourTable2"]]
        if(is.null(inFile3)){
          return(NULL)
        } else {
          tryCatch(
            expr = {
              # Create variable for tSNE colour table
              tSNEcolourTable <<- read.delim(inFile3$datapath, header = T, stringsAsFactors = F)
            },
            error = function(e){
              tSNEcolourTable <<- NULL
            }
          )
        }
        if(!is.null(tSNEcolourTable)){
          tryCatch(
            expr = {
              sampleMetadata <- tSNEpatientMetadata()
              
              colourTable <- tSNEcolourTable
              numColours <- length(unique(colourTable[,2]))
              getPalette <- colorRampPalette(brewer.pal(n = 9, name = "Paired"))
              colourVector <- getPalette(numColours)
              colourVector <- c(colourVector, "#a9a9a9")
              names(colourVector) <- c(unique(colourTable[,2]), "Other")
              
              print(colourVector)
              
              tsne_points <<- as.data.frame(tsne$Y)
              
              logTable <- tSNETpmObject()
              
              tsne_points <<- cbind(tsne_points, SampleID = rownames(logTable))
              tsne_points <<- merge(x = tsne_points, y = sampleMetadata, by.x = "SampleID", by.y = c(1))
              
              tsne_points <<- cbind(tsne_points, SampleID = rownames(logTable))
              tsne_points <<- merge(x = tsne_points, y = sampleMetadata, by.x = "SampleID", by.y = c(1))
              
              # Create ggplot string
              stringToParse <- "ggplot(tsne_points, aes(x = V1, y = V2, SampleID = SampleID"
              for(i in 1:ncol(sampleMetadata[m,-1])){
                stringToParse <- paste(stringToParse, ", ", colnames(sampleMetadata[m,-1])[i], " = ", colnames(sampleMetadata[m,-1])[i], sep = "")
              }
              stringToParse <- paste(stringToParse, ")) + geom_point(aes(colour = Groupings)) + scale_colour_manual(values = colourVector) + labs(colour = 'Cancer Type') + theme_classic()", sep = "")
              
              p <- eval(parse(text = stringToParse))
              
              filename <- "tsne.png"
              
              id <- input$tSNEsampleSelect2
              if(id != "None"){
                p <- p + geom_point(data = tsne_points[tsne_points$SampleID == id,], size = 3, shape = 8)
                filename <- paste("tsne_", id, "_highlighted.png", sep = "")
              }
              
              # Create download handler
              output$tSNEDownload <- downloadHandler(
                filename = function(){filename},
                content = function(file){ggsave(filename = file, plot = p, device = "png", width = 8)}
              )
              # Enable download button
              shinyjs::enable("tSNEDownload")
              
              # Plotly output
              output$tSNEPlot <- renderPlotly({
                ggplotly(p)
              })
              shinyjs::show("tSNEPlot")
            },
            error = function(e){
              return(NULL)
            }
          )
        }
      }
    }
  }
)
