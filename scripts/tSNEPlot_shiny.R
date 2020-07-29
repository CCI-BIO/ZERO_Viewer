## tSNEPlot_shiny.R
## Server logic for creating a tSNE plot
##
## Created by Nisitha Jayatilleke
## Date: 18/07/2019
## Last updated: 29/07/2020

# Initialise widgets
observeEvent(
  c(input$tSNEselectOffline, input$tSNEcolourSelect),
  {
    shinyjs::disable("tSNEDownload")
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
      
      if(input$tSNEcolourSelect == "list"){
        output$tSNEColourTable <- renderUI({
          selectInput(
            inputId = "tSNEColourTable2", 
            label = NULL,
            choices = "NA"
          )
        })
        shinyjs::hide("tSNEColourTable")
        shinyjs::show("tSNEbroadCategories")
      } else if(input$tSNEcolourSelect == "user"){
        shinyjs::hide("tSNEbroadCategories")
        output$tSNEColourTable <- renderUI({
          fileInput(
            inputId = "tSNEColourTable2",
            label = h4("Upload custom patient diagnosis file:")
          )
        })
        shinyjs::show("tSNEColourTable")
      }
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
          label = h4("Upload patient diagnosis file:")
        )
      })
      if(input$tSNEcolourSelect == "list"){
        shinyjs::show("tSNEbroadCategories")
        output$tSNEColourTable <- renderUI({
          fileInput(
            inputId = "tSNEColourTable2",
            label = h4("Upload cancer type colours file:")
          )
        })
      } else if(input$tSNEcolourSelect == "user"){
        shinyjs::hide("tSNEbroadCategories")
        output$tSNEColourTable <- renderUI({
          fileInput(
            inputId = "tSNEColourTable2",
            label = h4("Upload custom patient diagnosis file:")
          )
        })
      }
      shinyjs::show("tSNETPMCounts")
      shinyjs::show("tSNEPatientMetadata")
      shinyjs::show("tSNEColourTable")
    }
  }
)

# Online mode
observeEvent(
  c(input$tSNEselectOffline, input$tSNEcolourSelect, input$tSNEColourTable2),
  {
    if(input$tSNEselectOffline == "online"){
      tryCatch(
        expr = {
          # Create variable for tSNE patient metadata
          tSNEpatientMetadata <<- reactive({
            check_tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
            metadata <- read.delim(paste(dirLoc, "Patients_Diagnosis.txt", sep = ""), header = T, stringsAsFactors = F)
            if(nrow(metadata) > (ncol(check_tpm)-1)){
              metadata <- metadata[1:(ncol(check_tpm)-1),]
            }
            return(metadata)
          })
          # Create variable for tSNE patient metadata (alternative format)
          tSNEpatientMetadataAlt <<- reactive({
            check_tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
            metadata <- read.delim(paste(dirLoc, "Patients_Diagnosis.txt", sep = ""), sep = "\t", header = F, row.names = 1)
            if(nrow(metadata) > ncol(check_tpm)){
              metadata <- metadata[1:(ncol(check_tpm)),]
            }
            return(metadata)
          })
          # Create variable for tSNE patient tpm counts
          tSNETPM <<- reactive({
            tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
            return(tpm)
          })
          if(input$tSNEcolourSelect == "list"){
            tSNEcolourTable <<- reactive({
              colourTable <- read.delim("data_files/cancertype_colours.txt", header = T, stringsAsFactors = F)
              return(colourTable)
            })
            shinyjs::show("tSNEbroadCategories")
          } else if(input$tSNEcolourSelect == "user"){
            shinyjs::hide("tSNEbroadCategories")
            inFile3 <- input[["tSNEColourTable2"]]
            if(is.null(inFile3)){
              return(NULL)
            } else {
              tryCatch(
                expr = {
                  # Create variable for tSNE colour table
                  tSNEcolourTable <<- reactive({
                    colourTable <- read.delim(inFile3$datapath, header = T, stringsAsFactors = F)
                    return(colourTable)
                  })
                },
                error = function(e){
                  tSNEcolourTable <<- reactive({return(NULL)})
                }
              )
            }
          }
        },
        error = function(e){
          tSNEpatientMetadata <<- reactive({return(NULL)})
          tSNEpatientMetadataAlt <<- reactive({return(NULL)})
          tSNETPM <<- reactive({return(NULL)})
          tSNEcolourTable <<- reactive({return(NULL)})
          return(NULL)
        }
      )
    }
  }
)

# Offline mode
observeEvent(
  c(input$tSNEselectOffline, input$tSNETPMCounts2, input$tSNEPatientMetadata2, input$tSNEcolourSelect, input$tSNEColourTable2),
  {
    if(input$tSNEselectOffline == "offline"){
      tSNEpatientMetadata <<- reactive({return(NULL)})
      tSNEpatientMetadataAlt <<- reactive({return(NULL)})
      tSNEcolourTable <<- reactive({return(NULL)})
      tSNETPM <<- reactive({return(NULL)})
      inFile1 <- input[["tSNETPMCounts2"]]
      if(is.null(inFile1)){
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
              metadata <- read.delim(inFile2$datapath, header = T, stringsAsFactors = F)
              if(nrow(metadata) > (ncol(check_tpm)-1)){
                metadata <- metadata[1:(ncol(check_tpm)-1),]
              }
              return(metadata)
            })
            # Create variable for tSNE patient metadata (alternative format)
            tSNEpatientMetadataAlt <<- reactive({
              check_tpm <- read.delim(inFile1$datapath, sep = "\t", header = T, row.names = 1)
              metadata <- read.delim(inFile2$datapath, sep = "\t", header = F, row.names = 1)
              if(nrow(metadata) > ncol(check_tpm)){
                metadata <- metadata[1:(ncol(check_tpm)),]
              }
              return(metadata)
            })
          },
          error = function(e){
            tSNEpatientMetadata <<- reactive({return(NULL)})
            tSNEpatientMetadataAlt <<- reactive({return(NULL)})
          }
        )
      }
      inFile3 <- input[["tSNEColourTable2"]]
      if(is.null(inFile3)){
        return(NULL)
      } else {
        tryCatch(
          expr = {
            # Create variable for tSNE colour table
            tSNEcolourTable <<- reactive({
              colourTable <- read.delim(inFile3$datapath, header = T, stringsAsFactors = F)
              return(colourTable)
            })
          },
          error = function(e){
            tSNEcolourTable <<- reactive({return(NULL)})
          }
        )
      }
    }
  }
)

# Plot online trigger
observeEvent(
  c(input$tSNEselectOffline, input$tSNEcolourSelect, input$tSNEColourTable2, input$tSNEbroadCategories, input$tSNEperplexitySelect),
  {
    if(input$tSNEselectOffline == "online"){
      # Select sample for plotting
      output$tSNEsampleSelect <- renderUI({
        patientMetadata <- tSNEpatientMetadata()
        sampleSelectList <- patientMetadata$Patient.ID
        names(sampleSelectList) <- sampleSelectList
        selectizeInput(
          inputId = "tSNEsampleSelect2",
          label = h4("Select patient ID:"),
          choices = c("None", sampleSelectList),
          multiple = F,
          selected = "None"
        )
      })

      # Create TPM object
      tSNETpmObject <- reactive({
        tpm <- tSNETPM()
        tpm <- tpm[,-1]
        colnames(tpm) <- gsub(pattern = "\\.", replacement = "-", colnames(tpm))
        expTpm <- t(tpm)
        expTpm[expTpm == 0] <- 0.0001
        logTPM <- log2(expTpm)
        return(logTPM)
      })

      # Create tsne output
      tSNEobject <- reactive({
        perplexity <- input$tSNEperplexitySelect
        logTPM <- tSNETpmObject()
        set.seed(1234)
        tsne <- Rtsne(logTPM, dims = 2, perplexity = perplexity, verbose = TRUE, max_iter = 500, check_duplicates = F)
        return(tsne)
      })
      
      if(input$tSNEcolourSelect == "list"){
        # Construct tSNE plot with plotly
        output$tSNEPlot <- renderPlotly({
          id <- input$tSNEsampleSelect2
          if(!is.null(id)){
            if(input$tSNEbroadCategories == "specific"){
              cncInfo <- tSNEpatientMetadataAlt()
              cncInfo <- t(cncInfo)
              
              cncType <- cncInfo[1,-1]
              cncType2 <- cncInfo[3,-1]
              
              logTPM <- tSNETpmObject()
              
              tsne <- tSNEobject()
              tsne_points <- as.data.frame(tsne$Y)
              tsne_points <- cbind(tsne_points, Diagnosis = as.vector(cncType), CancerCategory = as.vector(cncType2), SampleID = rownames(logTPM))
              
              colourTable <- tSNEcolourTable()
              #colourTable <- colourTable[-c((nrow(colourTable)-3):nrow(colourTable)),]
              colourTable <- colourTable[which(colourTable$cancertype %in% cncType),]
              CancerColours <- colourTable$cols
              names(CancerColours) <- unique(colourTable$cancertype)
              
              # Highlight patient id
              suppressWarnings({
                p <- ggplot(tsne_points, aes(x = V1, y = V2, SampleID = SampleID, CancerCategory = CancerCategory)) +
                  geom_point(aes(colour = Diagnosis)) +
                  scale_colour_manual(values = CancerColours) +
                  labs(colour = "Cancer Type") +
                  theme_classic()
                filename <- "tsne.png"
                if(id != "None"){
                  p <- p + geom_point(data = tsne_points[tsne_points$SampleID == id,], size = 3, shape = 8)
                  filename <- paste("tsne_", id, "_highlighted.png", sep = "")
                }
              })
            } else if(input$tSNEbroadCategories == "broad"){
              cncInfo <- tSNEpatientMetadataAlt()
              cncInfo <- t(cncInfo)
              
              cncType <- cncInfo[1,-1]
              cncType2 <- cncInfo[3,-1]
              
              logTPM <- tSNETpmObject()
              
              tsne <- tSNEobject()
              tsne_points <- as.data.frame(tsne$Y)
              tsne_points <- cbind(tsne_points, Diagnosis = as.vector(cncType), CancerCategory = as.vector(cncType2), SampleID = rownames(logTPM))
              
              colourTable <- tSNEcolourTable()
              #colourTable <- colourTable[c((nrow(colourTable)-3):nrow(colourTable)),]
              colourTable <- colourTable[which(colourTable$cancertype %in% cncType),]
              CancerColours <- colourTable$cols
              names(CancerColours) <- unique(colourTable$cancertype)
              
              # Highlight patient id
              suppressWarnings({
                p <- ggplot(tsne_points, aes(x = V1, y = V2, SampleID = SampleID, Diagnosis = Diagnosis)) +
                  geom_point(aes(colour = CancerCategory)) +
                  scale_colour_manual(values = CancerColours) +
                  labs(colour = "Cancer Type") +
                  theme_classic()
                filename <- "tsne.png"
                if(id != "None"){
                  p <- p + geom_point(data = tsne_points[tsne_points$SampleID == id,], size = 3, shape = 8)
                  filename <- paste("tsne_", id, "_highlighted.png", sep = "")
                }
              })
            }
            
            # Create download handler
            output$tSNEDownload <- downloadHandler(
              filename = function(){filename},
              content = function(file){ggsave(filename = file, plot = p, device = "png", width = 8)}
            )
            # Enable download button
            shinyjs::enable("tSNEDownload")
            
            # Plotly output
            ggplotly(
              print(p),
              tooltip = c("Diagnosis", "CancerCategory", "SampleID")
            )
          }
        })
      } else if(input$tSNEcolourSelect == "user"){
        inFile3 <- input[["tSNEColourTable2"]]
        if(is.null(inFile3)){
          return(NULL)
        } else {
          # Construct tSNE plot with plotly
          output$tSNEPlot <- renderPlotly({
            id <- input$tSNEsampleSelect2
            if(!is.null(id)){
              cncInfo <- tSNEpatientMetadataAlt()
              cncInfo <- t(cncInfo)
              cncType <- cncInfo[1,-1]
              cncType2 <- cncInfo[3,-1]
              logTPM <- tSNETpmObject()
              colourTable <- tSNEcolourTable()
              samplesToColour <- colourTable$Patient
              uniqueColours <- unique(colourTable$Group)
              numCols <- length(uniqueColours)
              cols <- brewer.pal(n = 9, name = "Set1")
              cols <- cols[1:numCols]
              cols <- c(cols, "#a9a9a9")
              names(cols) <- c(uniqueColours, "Other")
              sampleList <- rownames(logTPM)
              for(i in 1:length(sampleList)){
                if(sampleList[i] %in% samplesToColour){
                  sampleList[i] <- colourTable[which(colourTable$Patient == sampleList[i]),"Group"]
                } else {
                  sampleList[i] <- "Other"
                }
              }
              names(sampleList) <- rownames(logTPM)
              tsne <- tSNEobject()
              tsne_points <- as.data.frame(tsne$Y)
              tsne_points <- cbind(tsne_points, Diagnosis = as.vector(cncType), CancerCategory = as.vector(cncType2), SampleID = rownames(logTPM), Group = sampleList)
              
              # Highlight patient id
              suppressWarnings({
                p <- ggplot(tsne_points, aes(x = V1, y = V2, SampleID = SampleID, Diagnosis = Diagnosis, CancerCategory = CancerCategory)) +
                  geom_point(data = subset(tsne_points, Group == 'Other'), aes(colour = Group)) +
                  geom_point(aes(colour = Group)) +
                  scale_colour_manual(values = cols) +
                  labs(colour = "Cancer Type") +
                  theme_classic()
                filename <- "tsne.png"
                if(id != "None"){
                  p <- p + geom_point(data = tsne_points[tsne_points$SampleID == id,], size = 3, shape = 8)
                  filename <- paste("tsne_", id, "_highlighted.png", sep = "")
                }
              })
              
              # Create download handler
              output$tSNEDownload <- downloadHandler(
                filename = function(){filename},
                content = function(file){ggsave(filename = file, plot = p, device = "png", width = 8)}
              )
              # Enable download button
              shinyjs::enable("tSNEDownload")
              
              # Plotly output
              ggplotly(
                print(p),
                tooltip = c("Diagnosis", "CancerCategory", "SampleID")
              )
            }
          })
        }
      }
    }
  }
)

# Plot offline trigger
observeEvent(
  c(input$tSNEselectOffline, input$tSNETPMCounts2, input$tSNEPatientMetadata2, input$tSNEcolourSelect, input$tSNEColourTable2, input$tSNEbroadCategories, input$tSNEperplexitySelect),
  {
    if(input$tSNEselectOffline == "offline"){
      # Select sample for plotting
      output$tSNEsampleSelect <- renderUI({
        patientMetadata <- tSNEpatientMetadata()
        sampleSelectList <- patientMetadata$Patient.ID
        names(sampleSelectList) <- sampleSelectList
        selectizeInput(
          inputId = "tSNEsampleSelect2",
          label = h4("Select patient ID:"),
          choices = c("None", sampleSelectList),
          multiple = F,
          selected = "None"
        )
      })

      # Create TPM object
      tSNETpmObject <- reactive({
        tpm <- tSNETPM()
        tpm <- tpm[,-1]
        colnames(tpm) <- gsub(pattern = "\\.", replacement = "-", colnames(tpm))
        expTpm <- t(tpm)
        expTpm[expTpm == 0] <- 0.0001
        logTPM <- log2(expTpm)
        return(logTPM)
      })

      # Create tsne output
      tSNEobject <- reactive({
        perplexity <- input$tSNEperplexitySelect
        logTPM <- tSNETpmObject()
        set.seed(1234)
        tsne <- Rtsne(logTPM, dims = 2, perplexity = perplexity, verbose = TRUE, max_iter = 500, check_duplicates = F)
        return(tsne)
      })

      if(input$tSNEcolourSelect == "list"){
        # Construct tSNE plot with plotly
        output$tSNEPlot <- renderPlotly({
          id <- input$tSNEsampleSelect2
          if(!is.null(id)){
            if(input$tSNEbroadCategories == "specific"){
              cncInfo <- tSNEpatientMetadataAlt()
              cncInfo <- t(cncInfo)
              
              cncType <- cncInfo[1,-1]
              cncType2 <- cncInfo[3,-1]
              
              logTPM <- tSNETpmObject()
              
              tsne <- tSNEobject()
              tsne_points <- as.data.frame(tsne$Y)
              tsne_points <- cbind(tsne_points, Diagnosis = as.vector(cncType), CancerCategory = as.vector(cncType2), SampleID = rownames(logTPM))
              
              colourTable <- tSNEcolourTable()
              #colourTable <- colourTable[-c((nrow(colourTable)-3):nrow(colourTable)),]
              colourTable <- colourTable[which(colourTable$cancertype %in% cncType),]
              CancerColours <- colourTable$cols
              names(CancerColours) <- unique(colourTable$cancertype)
              
              # Highlight patient id
              suppressWarnings({
                p <- ggplot(tsne_points, aes(x = V1, y = V2, SampleID = SampleID, CancerCategory = CancerCategory)) +
                  geom_point(aes(colour = Diagnosis)) +
                  scale_colour_manual(values = CancerColours) +
                  labs(colour = "Cancer Type") +
                  theme_classic()
                filename <- "tsne.png"
                if(id != "None"){
                  p <- p + geom_point(data = tsne_points[tsne_points$SampleID == id,], size = 3, shape = 8)
                  filename <- paste("tsne_", id, "_highlighted.png", sep = "")
                }
              })
            } else if(input$tSNEbroadCategories == "broad"){
              cncInfo <- tSNEpatientMetadataAlt()
              cncInfo <- t(cncInfo)
              
              cncType <- cncInfo[1,-1]
              cncType2 <- cncInfo[3,-1]
              
              logTPM <- tSNETpmObject()
              
              tsne <- tSNEobject()
              tsne_points <- as.data.frame(tsne$Y)
              tsne_points <- cbind(tsne_points, Diagnosis = as.vector(cncType), CancerCategory = as.vector(cncType2), SampleID = rownames(logTPM))
              
              colourTable <- tSNEcolourTable()
              #colourTable <- colourTable[c((nrow(colourTable)-3):nrow(colourTable)),]
              colourTable <- colourTable[which(colourTable$cancertype %in% cncType2),]
              CancerColours <- colourTable$cols
              names(CancerColours) <- unique(colourTable$cancertype)
              
              # Highlight patient id
              suppressWarnings({
                p <- ggplot(tsne_points, aes(x = V1, y = V2, SampleID = SampleID, Diagnosis = Diagnosis)) +
                  geom_point(aes(colour = CancerCategory)) +
                  scale_colour_manual(values = CancerColours) +
                  labs(colour = "Cancer Type") +
                  theme_classic()
                filename <- "tsne.png"
                if(id != "None"){
                  p <- p + geom_point(data = tsne_points[tsne_points$SampleID == id,], size = 3, shape = 8)
                  filename <- paste("tsne_", id, "_highlighted.png", sep = "")
                }
              })
            }
            
            # Create download handler
            output$tSNEDownload <- downloadHandler(
              filename = function(){filename},
              content = function(file){ggsave(filename = file, plot = p, device = "png", width = 8)}
            )
            # Enable download button
            shinyjs::enable("tSNEDownload")
            
            # Plotly output
            ggplotly(
              print(p),
              tooltip = c("Diagnosis", "CancerCategory", "SampleID")
            )
          }
        })
      } else if(input$tSNEcolourSelect == "user"){
        inFile3 <- input[["tSNEColourTable2"]]
        if(is.null(inFile3)){
          return(NULL)
        } else {
          # Construct tSNE plot with plotly
          output$tSNEPlot <- renderPlotly({
            id <- input$tSNEsampleSelect2
            if(!is.null(id)){
              cncInfo <- tSNEpatientMetadataAlt()
              cncInfo <- t(cncInfo)
              cncType <- cncInfo[1,-1]
              cncType2 <- cncInfo[3,-1]
              logTPM <- tSNETpmObject()
              colourTable <- tSNEcolourTable()
              samplesToColour <- colourTable$Patient
              uniqueColours <- unique(colourTable$Group)
              numCols <- length(uniqueColours)
              cols <- brewer.pal(n = 9, name = "Set1")
              cols <- cols[1:numCols]
              cols <- c(cols, "#a9a9a9")
              names(cols) <- c(uniqueColours, "Other")
              sampleList <- rownames(logTPM)
              for(i in 1:length(sampleList)){
                if(sampleList[i] %in% samplesToColour){
                  sampleList[i] <- colourTable[which(colourTable$Patient == sampleList[i]),"Group"]
                } else {
                  sampleList[i] <- "Other"
                }
              }
              names(sampleList) <- rownames(logTPM)
              tsne <- tSNEobject()
              tsne_points <- as.data.frame(tsne$Y)
              tsne_points <- cbind(tsne_points, Diagnosis = as.vector(cncType), CancerCategory = as.vector(cncType2), SampleID = rownames(logTPM), Group = sampleList)
              
              # Highlight patient id
              suppressWarnings({
                p <- ggplot(tsne_points, aes(x = V1, y = V2, SampleID = SampleID, Diagnosis = Diagnosis, CancerCategory = CancerCategory)) +
                  geom_point(data = subset(tsne_points, Group == 'Other'), aes(colour = Group)) +
                  geom_point(aes(colour = Group)) +
                  scale_colour_manual(values = cols) +
                  labs(colour = "Cancer Type") +
                  theme_classic()
                filename <- "tsne.png"
                if(id != "None"){
                  p <- p + geom_point(data = tsne_points[tsne_points$SampleID == id,], size = 3, shape = 8)
                  filename <- paste("tsne_", id, "_highlighted.png", sep = "")
                }
              })
              
              # Create download handler
              output$tSNEDownload <- downloadHandler(
                filename = function(){filename},
                content = function(file){ggsave(filename = file, plot = p, device = "png", width = 8)}
              )
              # Enable download button
              shinyjs::enable("tSNEDownload")
              
              # Plotly output
              ggplotly(
                print(p),
                tooltip = c("Diagnosis", "CancerCategory", "SampleID")
              )
            }
          })
        }
      }
    }
  }
)
