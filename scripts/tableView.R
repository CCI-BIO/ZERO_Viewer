## tableView.R
## Server logic for viewing and manipulating tables
##
## Created by Nisitha Jayatilleke
## Date: 17/07/2019
## Last updated: 07/07/2020

# Online or Offline mode
observe({
  # Online mode
  if(input$selectOffline == "online"){
    # Select sample for table view
    output$sampleSelect <- renderUI({
      tryCatch(
        expr = {
          patientMetadata <- read.delim(paste(dirLoc, "Patients_Diagnosis.txt", sep = ""), header = T, stringsAsFactors = F)
          print(patientMetadata)
          sampleSelectList <- patientMetadata[,1]
          names(sampleSelectList) <- sampleSelectList
          selectizeInput(
            inputId = "sampleSelect2",
            label = h4("Select sample ID:"),
            choices = sampleSelectList,
            multiple = F,
            selected = NULL
          )
        },
        error = function(e){
          return(NULL)
        }
      )
    })
    
    # Select table to view
    output$tableSelect <- renderUI({
      tryCatch(
        expr = {
          sampleToView <- input$sampleSelect2
          if(length(grep(pattern = "P00", x = sampleToView)) > 0){
            sampleToViewNoSecondary <- gsub(pattern = "-.*", replacement = "", x = sampleToView)
          } else {
            sampleToViewNoSecondary <- sampleToView
          }
          fileList <- list.files(path = paste(dirLoc, sampleToViewNoSecondary, "/", sep = ""), full.names = F)
          fileList <- fileList[grep(pattern = "-FC", x = fileList)]  # Remove extra - after FC
          selectizeInput(
            inputId = "tableSelect2",
            label = h4("Select table to view:"),
            choices = fileList,
            multiple = F,
            selected = NULL
           )
        },
        error = function(e){
          return(NULL)
        }
      )
    })
    
    # Hide offline inputs
    output$patientMetadata <- renderUI({NULL})
    output$TPMcounts <- renderUI({NULL})
    
  } else if(input$selectOffline == "offline"){
    # Offline mode
    # Select file for table view
    output$sampleSelect <- renderUI({
      fileInput(
        inputId = "tableFile",
        label = h4("Upload table file:")
      )
    })
    # Select patient diagnosis file
    output$patientMetadata <- renderUI({
      fileInput(
        inputId = "patientMetadata2",
        label = h4("Upload sample metadata file:")
      )
    })
    # Select TPM data table
    output$TPMcounts <- renderUI({
      fileInput(
        inputId = "TPMcounts2",
        label = h4("Upload TPM counts file:")
      )
    })
    # Set table select to NULL
    output$tableSelect <- renderUI({NULL})
  }
})

# Select genes based on specified method
output$selectGenes <- renderUI({
  if(input$selectGeneInput == "all"){
    return(NULL)
  } else if(input$selectGeneInput == "specific"){
    selectizeInput(
      inputId = "geneSelectSpecifc",
      label = h4("Select genes to show:"),
      choices = NULL,
      multiple = T
    )
  } else if(input$selectGeneInput == "list"){
    fileInput(
      inputId = "geneListFile",
      label = h4("Upload gene list:")
    )
  }
})

# Select groupings 
observeEvent(
  c(input$selectOffline, input$sampleSelect2, input$tableSelect2, input$selectGeneInput, input$patientMetadata2, input$TPMcounts2),
  {
    tryCatch(
      expr = {
        if(input$selectOffline == "offline"){
          inFile2 <- input[["patientMetadata2"]]
          if(is.null(inFile2)){
            return(NULL)
          } else {
            patientMetadata <- read.delim(inFile2$datapath, header = T, stringsAsFactors = F)
          }
        } else if(input$selectOffline == "online"){
          patientMetadata <- read.delim(paste(dirLoc, "/Patients_Diagnosis.txt", sep = ""), header = T, stringsAsFactors = F)
        }
        output$categoryChoice <- renderUI({
          selectInput(
            inputId = "categoryChoice2",
            label = h4("Select groupings:"), 
            choices = colnames(patientMetadata)[-1], 
            multiple = F
          )
        })
      },
      error = function(e){
        return(NULL)
      }
    )
  }
)

# Select specific groups to summarise
observeEvent(
  c(input$categoryChoice2),
  {
    tryCatch(
      expr = {
        if(!is.null(input$categoryChoice2)){
          if(input$selectOffline == "offline"){
            inFile2 <- input[["patientMetadata2"]]
            if(is.null(inFile2)){
              return(NULL)
            } else {
              patientMetadata <- read.delim(inFile2$datapath, header = T, stringsAsFactors = F)
            }
          } else if(input$selectOffline == "online"){
            patientMetadata <- read.delim(paste(dirLoc, "/Patients_Diagnosis.txt", sep = ""), header = T, stringsAsFactors = F)
          }
          output$histologySelect <- renderUI({
            selectInput(
              inputId = "histologySelect2",
              label = h4("Select specific groupings:"), 
              choices = unique(patientMetadata[,input$categoryChoice2]), 
              multiple = T
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

# Server side selectize input for genes
observeEvent(
  c(input$selectOffline, input$sampleSelect2, input$tableSelect2, input$selectGeneInput, input$patientMetadata2, input$TPMcounts2),
  {
    tryCatch(
      expr = {
        if(input$selectOffline == "offline"){
          inFile <- input[["tableFile"]]
          if(is.null(inFile)){
            return(NULL)
          } else {
            tableToView <- read.delim(inFile$datapath, header = T, stringsAsFactors = F)
            geneList <- as.character(tableToView$gene_id)
            updateSelectizeInput(
              session = session,
              inputId = "geneSelectSpecifc",
              choices = geneList,
              selected = NULL,
              server = T
            )
          }
        } else if(input$selectOffline == "online"){
          sampleToView <- input$sampleSelect2
          if(length(grep(pattern = "P00", x = sampleToView)) > 0){
            sampleToViewNoSecondary <- gsub(pattern = "-.*", replacement = "", x = sampleToView)
          } else {
            sampleToViewNoSecondary <- sampleToView
          }
          tableToView <- read.delim(paste(dirLoc, sampleToViewNoSecondary, "/", input$tableSelect2, sep = ""), header = T, stringsAsFactors = F)
          geneList <- as.character(tableToView$gene_id)
          updateSelectizeInput(
            session = session,
            inputId = "geneSelectSpecifc",
            choices = geneList,
            selected = NULL,
            server = T
          )
        }
      },
      error = function(e){
        return(NULL)
      }
    )
  }
)

# Render table for viewing
output$tablePreview <- renderDT(
  {
    tryCatch(
      expr = {
        if(input$selectOffline == "offline"){
          inFile <- input[["tableFile"]]
          if(is.null(inFile)){
            return(NULL)
          } else {
            tableToView <- read.delim(inFile$datapath, header = T, stringsAsFactors = F)
          }
        }
        if(input$selectOffline == "online"){
          sampleToView <- input$sampleSelect2
          if(length(grep(pattern = "P00", x = sampleToView)) > 0){
            sampleToViewNoSecondary <- gsub(pattern = "-.*", replacement = "", x = sampleToView)
          } else {
            sampleToViewNoSecondary <- sampleToView
          }
          tableToView <- read.delim(paste(dirLoc, sampleToViewNoSecondary, "/", input$tableSelect2, sep = ""), header = T, stringsAsFactors = F)
        }
        ## LEGACY CODE - FOR ZERO FILES ##
        #if("MeanTPM" %in% colnames(tableToView)){
        #  tableToView <- tableToView[,c("gene_id", "FC", "zscoreMean", "MeanTPM", "MedianTPM")]
        #} else {
        #  tableToView <- tableToView[,c("gene_id", "FC", "zscoreMean", "MedianTPM")]
        #}
        if(input$selectGeneInput == "specific"){
          tableToView <- tableToView[which(tableToView$gene_id %in% input$geneSelectSpecifc),]
        }
        if(input$selectGeneInput == "list"){
          inFile <- input[["geneListFile"]]
          if(is.null(inFile)){
            geneList <- NULL
          } else {
            geneList <- readLines(inFile$datapath)
          }
          tableToView <- tableToView[which(tableToView$gene_id %in% geneList),]
        }
        if(!is.null(input$histologySelect2)){
          if(input$selectOffline == "offline"){
            inFile2 <- input[["patientMetadata2"]]
            if(is.null(inFile2)){
              return(NULL)
            } else {
              patientMetadata <- read.delim(inFile2$datapath, header = T, stringsAsFactors = F)
            }
            inFile3 <- input[["TPMcounts2"]]
            if(is.null(inFile3)){
              return(NULL)
            } else {
              TPMcounts <- read.delim(inFile3$datapath, header = T, stringsAsFactors = F)
              TPMcounts <- TPMcounts[which(TPMcounts$gene_id %in% tableToView$gene_id),]
              rownames(TPMcounts) <- TPMcounts$gene_id
              TPMcounts <- TPMcounts[,-c(1:2)]
            }
          } else if(input$selectOffline == "online"){
            patientMetadata <- read.delim(paste(dirLoc, "/Patients_Diagnosis.txt", sep = ""), header = T, stringsAsFactors = F)
            TPMcounts <- read.delim(paste(dirLoc, "/GeneExpression_TPM_Counts.txt", sep = ""), header = T, stringsAsFactors = F)
            TPMcounts <- TPMcounts[which(TPMcounts$gene_id %in% tableToView$gene_id),]
            rownames(TPMcounts) <- TPMcounts$gene_id
            TPMcounts <- TPMcounts[,-c(1:2)]
          }
          selectHist <- input$histologySelect2
          for(i in 1:length(selectHist)){
            histToCalc <- selectHist[i]
            patientMetadataSubset <- patientMetadata[which(patientMetadata[,input$categoryChoice2] %in% histToCalc),1]
            TPMcountssubset <- TPMcounts[, which(colnames(TPMcounts) %in% patientMetadataSubset),drop=F]
            TPMmean <- rowMeans(TPMcountssubset)
            TPMmedian <- apply(TPMcountssubset, 1, function(x) median(x))
            TPMStats <- cbind(TPMmean, TPMmedian)
            rownames(TPMStats) <- rownames(TPMcountssubset)
            colnames(TPMStats) <- c(paste(histToCalc, "_MeanTPM", sep = ""), paste(histToCalc, "_MedianTPM", sep = ""))
            tableToView <- merge(x = tableToView, y = TPMStats, by.x = "gene_id", by.y = "row.names")
          }
        }
        output$tableDownload <- downloadHandler(
          filename = function(){paste("filtered_table.txt", sep = "")},
          content = function(file){write.table(tableToView, file, row.names = F, sep = "\t", quote = F)}
        )
        return(tableToView)
      },
      error = function(e){
        return(NULL)
      }
    )
  }, 
  options = list(pageLength = 25)
)
