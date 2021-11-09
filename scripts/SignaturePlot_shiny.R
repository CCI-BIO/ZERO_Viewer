## SignaturePlot_shiny.R
## Server logic for signature plot generation
##
## Created by Nisitha Jayatilleke
## Date: 23/07/2019
## Last updated: 04/08/2020

# Initiliase widgets
observeEvent(
  c(input$SigPlotSelectOffline),
  {
    shinyjs::disable("SigPlotDownload")
    # Online widgets
    if(input$SigPlotSelectOffline == "online"){
      # TPM count table
      output$SigPlotTPMCounts <- renderUI({
        selectInput(
          inputId = "SigPlotTPMCounts2", 
          label = NULL,
          choices = "NA"
        )
      })
      # Metadata table
      output$SigPlotPatientMetadata <- renderUI({
        selectInput(
          inputId = "SigPlotPatientMetadata2", 
          label = NULL,
          choices = "NA"
        )
      })
      # Signature file for offline (turn off)
      output$SigPlotSignatureGenes <- renderUI({
        selectInput(
          inputId = "SigPlotSignatureGenes2", 
          label = NULL,
          choices = "NA"
        )
      })
      # Signature file for online (turn on)
      output$SigPlotSignatureFile <- renderUI({
        fileList <- list.files(path = "data_files/", pattern = "expression_profile_genes.txt", full.names = F)
        names(fileList) <- fileList
        selectizeInput(
          inputId = "SigPlotSignatureFile2",
          label = HTML(paste(h4("Select signature to plot:"), '\n', h5("NOTE: signature files must have the suffix '_expression_profile_genes.txt'"))),
          choices = fileList,
          multiple = F
        )
      })
      shinyjs::hide("SigPlotTPMCounts")
      shinyjs::hide("SigPlotPatientMetadata")
      shinyjs::hide("SigPlotSignatureGenes")
      shinyjs::show("SigPlotSignatureFile")
    }
    # Offline widgets
    if(input$SigPlotSelectOffline == "offline"){
      # TPM count table
      output$SigPlotTPMCounts <- renderUI({
        fileInput(
          inputId = "SigPlotTPMCounts2",
          label = h4("Upload TPM counts file:")
        )
      })
      # Metadata table
      output$SigPlotPatientMetadata <- renderUI({
        fileInput(
          inputId = "SigPlotPatientMetadata2",
          label = h4("Upload sample metadata file:")
        )
      })
      # Signature file for offline (turn on)
      output$SigPlotSignatureGenes <- renderUI({
        fileInput(
          inputId = "SigPlotSignatureGenes2",
          label = h4("Upload gene signature file:")
        )
      })
      # Signature file for online (turn off)
      output$SigPlotSignatureFile <- renderUI({
        fileInput(
          inputId = "SigPlotSignatureFile2",
          label = NULL
        )
      })
      shinyjs::show("SigPlotTPMCounts")
      shinyjs::show("SigPlotPatientMetadata")
      shinyjs::show("SigPlotSignatureGenes")
      shinyjs::hide("SigPlotSignatureFile")
    }
  }
)

# Online mode
observeEvent(
  c(input$SigPlotSelectOffline),
  {
    if(input$SigPlotSelectOffline == "online"){
      tryCatch(
        expr = {
          # Create variable for signature plot patient metadata
          shinyjs::disable("SigPlotSelectOffline")
          SigPlotPatientMetadata <<- reactive({
            check_tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
            colnames(check_tpm) <- gsub(pattern="\\.",replacement="-",colnames(check_tpm))
            metadata <- read.delim(paste(dirLoc, "Patients_Diagnosis.txt", sep = ""), header = T, stringsAsFactors = F)
            metadata <- metadata[which(metadata[,1] %in% colnames(check_tpm)),]
            # if(nrow(metadata) > (ncol(check_tpm)-1)){
            #   metadata <- metadata[1:(ncol(check_tpm)-1),]
            # }
            return(metadata)
          })
          # Create variable for signature plot patient tpm counts
          SigPlotTPM <<- reactive({
            tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
            return(tpm)
          })
          # Create variable for signature genes 
          SigPlotSignatureGenes <<- reactive({
            genes <- read.delim(paste("data_files/", input$SigPlotSignatureFile2, sep = ""), sep = "\t", header = T)
            return(genes)
          })
        },
        error = function(e){
          SigPlotPatientMetadata <<- reactive({return(NULL)})
          SigPlotTPM <<- reactive({return(NULL)})
          SigPlotSignatureGenes <<- reactive({return(NULL)})
          shinyjs::enable("SigPlotSelectOffline")
          return(NULL)
        }
      )
    }
  }
)

# Offline mode
observeEvent(
  c(input$SigPlotSelectOffline, input$SigPlotTPMCounts2, input$SigPlotPatientMetadata2, input$SigPlotSignatureGenes2),
  {
    if(input$SigPlotSelectOffline == "offline"){
      SigPlotPatientMetadata <<- reactive({return(NULL)})
      SigPlotTPM <<- reactive({return(NULL)})
      SigPlotSignatureGenes <<- reactive({return(NULL)})
      inFile1 <- input[["SigPlotTPMCounts2"]]
      if(is.null(inFile1)){
        return(NULL)
      } else {
        tryCatch(
          expr = {
            # Create variable for signature plot tpm counts
            SigPlotTPM <<- reactive({
              tpm <- read.delim(inFile1$datapath, sep = "\t", header = T, row.names = 1)
              return(tpm)
            })
          },
          error = function(e){
            SigPlotTPM <<- reactive({return(NULL)})
            return(NULL)
          }
        )
      }
      inFile2 <- input[["SigPlotPatientMetadata2"]]
      if(is.null(inFile2)){
        return(NULL)
      } else {
        tryCatch(
          expr = {
            # Create variable for signature plot patient metadata
            SigPlotPatientMetadata <<- reactive({
              check_tpm <- read.delim(inFile1$datapath, sep = "\t", header = T, row.names = 1)
              colnames(check_tpm) <- gsub(pattern="\\.",replacement="-",colnames(check_tpm))
              metadata <- read.delim(inFile2$datapath, header = T, stringsAsFactors = F)
              metadata <- metadata[which(metadata[,1] %in% colnames(check_tpm)),]
              # if(nrow(metadata) > (ncol(check_tpm)-1)){
              #   metadata <- metadata[1:(ncol(check_tpm)-1),]
              # }
              return(metadata)
            })
          },
          error = function(e){
            SigPlotPatientMetadata <<- reactive({return(NULL)})
          }
        )
      }
      inFile3 <- input[["SigPlotSignatureGenes2"]]
      if(is.null(inFile3)){
        return(NULL)
      } else {
        tryCatch(
          expr = {
            # Create variable for signature plot genes
            SigPlotSignatureGenes <<- reactive({
              sig <- read.delim(inFile3$datapath, header = T, stringsAsFactors = F)
              return(sig)
            })
          },
          error = function(e){
            SigPlotSignatureGenes <<- reactive({return(NULL)})
          }
        )
      }
    }
  }
)

# Plot online trigger
observeEvent(
  c(input$SigPlotSelectOffline, input$SigPlotSignatureFile2),
  {
    tryCatch(
      expr = {
        if(input$SigPlotSelectOffline == "online"){
          # Select sample for plotting
          output$SigPlotSampleSelect <- renderUI({
            patientMetadata <- SigPlotPatientMetadata()
            sampleSelectList <- patientMetadata[,1]
            names(sampleSelectList) <- sampleSelectList
            selectizeInput(
              inputId = "SigPlotSampleSelect2",
              label = h4("Select sample ID:"),
              choices = sampleSelectList,
              multiple = F
            )
          })
          # Create TPM object
          SigPlotTpmObject <- reactive({
            tpm <- SigPlotTPM()
            tpm <- tpm[,-1]
            colnames(tpm) <- gsub(pattern = "\\.", replacement = "-", colnames(tpm))
            tpm[tpm == 0] <- 0.0001
            logTPM <- log10(tpm)
            return(logTPM)
          })
          # Construct signature plot with plotly
          output$SigPlot <- renderPlotly({
            tryCatch(
              expr = {
                test <- SigPlotTpmObject()
                sig <- SigPlotSignatureGenes()
                
                test <- test[which(rownames(test) %in% sig$AltGene),]
                test_means <- rowMeans(test)
                test_medians <- apply(test, 1, function(x) median(x))
                test <- cbind(test, test_means, test_medians, "gene_id" = rownames(test))
                
                patientToTest <- input$SigPlotSampleSelect2
                patientTPM <- test[,c("gene_id", patientToTest, "test_means", "test_medians"), drop = F]
                colnames(patientTPM)[which(colnames(patientTPM) == "test_means")] <- "Mean"
                colnames(patientTPM)[which(colnames(patientTPM) == "test_medians")] <- "Median"
                colnames(patientTPM)[which(colnames(patientTPM) == "gene_id")] <- "Gene"
                colnames(patientTPM)[which(colnames(patientTPM) == patientToTest)] <- "Sample.Value"
                patientTPM <- cbind(patientTPM, Sample.ID = c(rep(patientToTest, nrow(patientTPM))))
                n <- nrow(patientTPM)
                
                # Reorder the genes based on patient TPM
                gene_table <- patientTPM$Sample.Value
                names(gene_table) <- patientTPM$Gene
                gene_levels <- names(gene_table)[order(gene_table, decreasing = T)]
                Gene.Name <- names(gene_table)[order(gene_table, decreasing = T)]
                Mean.Value <- patientTPM[order(patientTPM$Sample.Value, decreasing = T),]$Mean
                Median.Value <- patientTPM[order(patientTPM$Sample.Value, decreasing = T),]$Median
                patientTPM$Gene <- factor(patientTPM$Gene, levels = gene_levels)
                
                # Create plot
                suppressWarnings(
                  p <- ggplot(data = patientTPM, mapping = aes(x = Gene, y = Sample.Value)) + 
                    scale_x_discrete() +
                    geom_segment(data = patientTPM, mapping = aes(x = as.numeric(0.5:(0.5+n-1)), xend = as.numeric(1.5:(1.5+n-1)), y = Mean.Value, yend = Mean.Value, value = Mean.Value, gene = Gene.Name, col = "Mean"), size = 1) +
                    geom_segment(data = patientTPM, mapping = aes(x = as.numeric(0.5:(0.5+n-1)), xend = as.numeric(1.5:(1.5+n-1)), y = Median.Value, yend = Median.Value, value = Median.Value, gene = Gene.Name, col = "Median"), size = 1) +
                    geom_point(data = patientTPM, aes(col = "Sample TPM", sampleID = Sample.ID, value = Sample.Value, gene = Gene), size = 1.5, show.legend = T) +
                    scale_color_manual(name = "TPM Statistics", values = c("brown1", "cornflowerblue", "black")) +
                    ylab("log10(TPM)") +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))
                )
                
                # Create download handler
                output$SigPlotDownload <- downloadHandler(
                  filename = function(){paste(patientToTest, "_signature_plot.png", sep = "")},
                  content = function(file){ggsave(filename = file, plot = (p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))), device = "png", width = 8)}
                )
                
                # Enable download button
                shinyjs::enable("SigPlotDownload")
                shinyjs::enable("SigPlotSelectOffline")
                
                # Create plotly version for display
                ggplotly(p, tooltip = c("sampleID", "value", "gene"))
                
              },
              error = function(e){
                shinyjs::enable("SigPlotSelectOffline")
                return(NULL)
              },
              warning = function(e){
                shinyjs::enable("SigPlotSelectOffline")
                return(NULL)
              }
            )
          })
        }
      },
      error = function(e){
        shinyjs::enable("SigPlotSelectOffline")
        return(NULL)
      }
    )
  }
)

# Plot offline trigger
observeEvent(
  c(input$SigPlotSelectOffline, input$SigPlotTPMCounts2, input$SigPlotPatientMetadata2, input$SigPlotSignatureGenes2),
  {
    tryCatch(
      expr = {
        if(input$SigPlotSelectOffline == "offline"){
          # Select sample for plotting
          output$SigPlotSampleSelect <- renderUI({
            patientMetadata <- SigPlotPatientMetadata()
            sampleSelectList <- patientMetadata[,1]
            names(sampleSelectList) <- sampleSelectList
            selectizeInput(
              inputId = "SigPlotSampleSelect2",
              label = h4("Select sample ID:"),
              choices = sampleSelectList,
              multiple = F
            )
          })
          
          # Create TPM object
          SigPlotTpmObject <- reactive({
            tpm <- SigPlotTPM()
            tpm <- tpm[,-1]
            colnames(tpm) <- gsub(pattern = "\\.", replacement = "-", colnames(tpm))
            tpm[tpm == 0] <- 0.0001
            logTPM <- log10(tpm)
            return(logTPM)
          })
          
          # Construct signature plot with plotly
          output$SigPlot <- renderPlotly({
            tryCatch(
              expr = {
                test <- SigPlotTpmObject()
                sig <- SigPlotSignatureGenes()
                
                test <- test[which(rownames(test) %in% sig$AltGene),]
                test_means <- rowMeans(test)
                test_medians <- apply(test, 1, function(x) median(x))
                test <- cbind(test, test_means, test_medians, "gene_id" = rownames(test))
                
                patientToTest <- input$SigPlotSampleSelect2
                patientTPM <- test[,c("gene_id", patientToTest, "test_means", "test_medians"), drop = F]
                colnames(patientTPM)[which(colnames(patientTPM) == "test_means")] <- "Mean"
                colnames(patientTPM)[which(colnames(patientTPM) == "test_medians")] <- "Median"
                colnames(patientTPM)[which(colnames(patientTPM) == "gene_id")] <- "Gene"
                colnames(patientTPM)[which(colnames(patientTPM) == patientToTest)] <- "Sample.Value"
                patientTPM <- cbind(patientTPM, Sample.ID = c(rep(patientToTest, nrow(patientTPM))))
                n <- nrow(patientTPM)
                
                # Reorder the genes based on patient TPM
                gene_table <- patientTPM$Sample.Value
                names(gene_table) <- patientTPM$Gene
                gene_levels <- names(gene_table)[order(gene_table, decreasing = T)]
                Gene.Name <- names(gene_table)[order(gene_table, decreasing = T)]
                Mean.Value <- patientTPM[order(patientTPM$Sample.Value, decreasing = T),]$Mean
                Median.Value <- patientTPM[order(patientTPM$Sample.Value, decreasing = T),]$Median
                patientTPM$Gene <- factor(patientTPM$Gene, levels = gene_levels)
                
                # Create plot
                suppressWarnings(
                  p <- ggplot(data = patientTPM, mapping = aes(x = Gene, y = Sample.Value)) + 
                    scale_x_discrete() +
                    geom_segment(data = patientTPM, mapping = aes(x = as.numeric(0.5:(0.5+n-1)), xend = as.numeric(1.5:(1.5+n-1)), y = Mean.Value, yend = Mean.Value, value = Mean.Value, gene = Gene.Name, col = "Mean"), size = 1) +
                    geom_segment(data = patientTPM, mapping = aes(x = as.numeric(0.5:(0.5+n-1)), xend = as.numeric(1.5:(1.5+n-1)), y = Median.Value, yend = Median.Value, value = Median.Value, gene = Gene.Name, col = "Median"), size = 1) +
                    geom_point(data = patientTPM, aes(col = "Sample TPM", sampleID = Sample.ID, value = Sample.Value, gene = Gene), size = 1.5, show.legend = T) +
                    scale_color_manual(name = "TPM Statistics", values = c("brown1", "cornflowerblue", "black")) +
                    ylab("log10(TPM)") +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))
                )
                
                # Create download handler
                output$SigPlotDownload <- downloadHandler(
                  filename = function(){paste(patientToTest, "_signature_plot.png", sep = "")},
                  content = function(file){ggsave(filename = file, plot = (p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))), device = "png", width = 8)}
                )
                
                # Enable download button
                shinyjs::enable("SigPlotDownload")
                
                # Create plotly version for display
                ggplotly(print(p), tooltip = c("sampleID", "value", "gene"))
                
              },
              error = function(e){
                return(NULL)
              },
              warning = function(w){
                return(NULL)
              }
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

