## violinPlot_shiny.R
## Server logic for violin plot generation
##
## Created by Nisitha Jayatilleke
## Date: 18/08/2020
## Last updated: 18/08/2020

# Initiliase widgets
observeEvent(
  c(input$VioPlotSelectOffline),
  {
    shinyjs::disable("VioPlotDownload")
    # Online widgets
    if(input$VioPlotSelectOffline == "online"){
      # TPM count table
      output$VioPlotTPMCounts <- renderUI({
        selectInput(
          inputId = "VioPlotTPMCounts2", 
          label = NULL,
          choices = "NA"
        )
      })
      # Metadata table
      output$VioPlotPatientMetadata <- renderUI({
        selectInput(
          inputId = "VioPlotPatientMetadata2", 
          label = NULL,
          choices = "NA"
        )
      })
      shinyjs::hide("VioPlotTPMCounts")
      shinyjs::hide("VioPlotPatientMetadata")
    }
    # Offline widgets
    if(input$VioPlotSelectOffline == "offline"){
      # TPM count table
      output$VioPlotTPMCounts <- renderUI({
        fileInput(
          inputId = "VioPlotTPMCounts2",
          label = h4("Upload TPM counts file:")
        )
      })
      # Metadata table
      output$VioPlotPatientMetadata <- renderUI({
        fileInput(
          inputId = "VioPlotPatientMetadata2",
          label = h4("Upload sample metadata file:")
        )
      })
      shinyjs::show("VioPlotTPMCounts")
      shinyjs::show("VioPlotPatientMetadata")
    }
  }
)

# Online mode
observeEvent(
  c(input$VioPlotSelectOffline),
  {
    if(input$VioPlotSelectOffline == "online"){
      tryCatch(
        expr = {
          # Create variable for violin plot patient metadata
          shinyjs::disable("VioPlotSelectOffline")
          VioPlotPatientMetadata <<- reactive({
            check_tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
            colnames(check_tpm) <- gsub(pattern="\\.",replacement="-",colnames(check_tpm))
            metadata <- read.delim(paste(dirLoc, "Patients_Diagnosis.txt", sep = ""), header = T, stringsAsFactors = F)
            metadata <- metadata[which(metadata$Patient.ID %in% colnames(check_tpm)),]
            # if(nrow(metadata) > (ncol(check_tpm)-1)){
            #   metadata <- metadata[1:(ncol(check_tpm)-1),]
            # }
            return(metadata)
          })
          # Create variable for violin plot patient tpm counts
          VioPlotTPM <<- reactive({
            tpm <- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
            return(tpm)
          })
        },
        error = function(e){
          shinyjs::enable("VioPlotSelectOffline")
          VioPlotPatientMetadata <<- reactive({return(NULL)})
          VioPlotTPM <<- reactive({return(NULL)})
          return(NULL)
        }
      )
    }
  }
)

# Offline mode
observeEvent(
  c(input$VioPlotSelectOffline, input$VioPlotTPMCounts2, input$VioPlotPatientMetadata2),
  {
    if(input$VioPlotSelectOffline == "offline"){
      VioPlotPatientMetadata <<- reactive({return(NULL)})
      VioPlotTPM <<- reactive({return(NULL)})
      inFile1 <- input[["VioPlotTPMCounts2"]]
      if(is.null(inFile1)){
        return(NULL)
      } else {
        tryCatch(
          expr = {
            # Create variable for violin plot tpm counts
            VioPlotTPM <<- reactive({
              tpm <- read.delim(inFile1$datapath, sep = "\t", header = T, row.names = 1)
              return(tpm)
            })
          },
          error = function(e){
            VioPlotTPM <<- reactive({return(NULL)})
            return(NULL)
          }
        )
      }
      inFile2 <- input[["VioPlotPatientMetadata2"]]
      if(is.null(inFile2)){
        return(NULL)
      } else {
        tryCatch(
          expr = {
            # Create variable for violin plot patient metadata
            VioPlotPatientMetadata <<- reactive({
              check_tpm <- read.delim(inFile1$datapath, sep = "\t", header = T, row.names = 1)
              colnames(check_tpm) <- gsub(pattern="\\.",replacement="-",colnames(check_tpm))
              metadata <- read.delim(inFile2$datapath, header = T, stringsAsFactors = F)
              metadata <- metadata[which(metadata$Patient.ID %in% colnames(check_tpm)),]
              # if(nrow(metadata) > (ncol(check_tpm)-1)){
              #   metadata <- metadata[1:(ncol(check_tpm)-1),]
              # }
              return(metadata)
            })
          },
          error = function(e){
            VioPlotPatientMetadata <<- reactive({return(NULL)})
          }
        )
      }
    }
  }
)

# Populate gene and category selection lists
observeEvent(
  c(input$VioPlotSelectOffline, input$VioPlotTPMCounts2, input$VioPlotPatientMetadata2),
  {
    shinyjs::hide("VioPlotGeneSelect")
    shinyjs::hide("VioPlotCategorySelect")
    shinyjs::hide("VioPlotSelectTPMScale")
    tryCatch(
      expr = {
        if(!is.null(VioPlotTPM()) & !is.null(VioPlotPatientMetadata())){
          tpm <- VioPlotTPM()
          updateSelectizeInput(
            session = session,
            inputId = "VioPlotGeneSelect",
            choices = rownames(tpm),
            server = T
          )
          metadata <- VioPlotPatientMetadata()
          output$VioPlotCategorySelect <- renderUI({
            selectizeInput(
              inputId = "VioPlotCategorySelect2",
              label = h4("Select category to plot:"),
              choices = colnames(metadata)[-1],
              multiple = F
              # options = list(maxOptions = 30000)
            )
          })
          output$VioPlotSelectTPMScale <- renderUI({
            radioButtons(
              inputId = "VioPlotSelectTPMScale2",
              label = h4("Select TPM scaling:"), 
              choices = list("TPM" = "tpm", "Log TPM" = "logtpm")
            )
          })
          shinyjs::show("VioPlotGeneSelect")
          shinyjs::show("VioPlotCategorySelect")
          shinyjs::show("VioPlotSelectTPMScale")
        }
        shinyjs::enable("VioPlotSelectOffline")
      },
      error = function(e){
        shinyjs::enable("VioPlotSelectOffline")
        return(NULL)
      }
    )
  }
)

# Populate specific category select
observeEvent(
  c(input$VioPlotCategorySelect2),
  {
    tryCatch(
      expr = {
        if(!is.null(VioPlotTPM()) & !is.null(VioPlotPatientMetadata())){
          metadata <- VioPlotPatientMetadata()
          columnToSelect <- input$VioPlotCategorySelect2
          groups <- metadata[,columnToSelect]
          output$VioPlotSpecificCategorySelect <- renderUI({
            selectInput(
              inputId = "VioPlotSpecificCategorySelect2",
              label = h4("Select specific labels to plot:"), 
              choices = groups, 
              selected = groups, 
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

# Plot trigger
observeEvent(
  c(input$VioPlotSelectOffline, input$VioPlotTPMCounts2, input$VioPlotPatientMetadata2, input$VioPlotGeneSelect, input$VioPlotCategorySelect2, input$VioPlotSelectTPMScale2, input$VioPlotSpecificCategorySelect2),
  {
    if(input$VioPlotSelectOffline == "offline" | input$VioPlotSelectOffline == "online"){
      # Construct violin plot with plotly
      output$VioPlot <- renderPlotly({
        tryCatch(
        expr = {
            if(!is.null(VioPlotTPM())){
              tpm <- VioPlotTPM()
              geneToTest <- input$VioPlotGeneSelect
              tpmSubset <- tpm[which(rownames(tpm) %in% geneToTest),,drop=F]
              tpmSubset <- tpmSubset[,-which(colnames(tpmSubset) %in% "transcript_id.s."),drop=F]
              metadata <- VioPlotPatientMetadata()
              metadata[,1] <- gsub(pattern = "-", replacement = ".", metadata[,1])
              
              # Create data frame
              df <- matrix(nrow = ncol(tpmSubset), ncol = 3)
              colnames(df) <- c("sample", "value", "group")
              
              categoryToTest <- input$VioPlotCategorySelect2
              column_idx <- which(colnames(metadata) %in% categoryToTest)
              for(i in 1:ncol(tpmSubset)){
                df[i,1] <- colnames(tpmSubset)[i]
                df[i,2] <- tpmSubset[1,i]
                df[i,3] <- metadata[which(metadata[,1] == colnames(tpmSubset)[i]), column_idx]
              }
              df <- as.data.frame(df)
              df$sample <- as.character(df$sample)
              df$value <- as.numeric(df$value)
              df$group <- as.character(df$group)
              
              df <- df[which(df$group %in% input$VioPlotSpecificCategorySelect2),]
              
              if(input$VioPlotSelectTPMScale2 == "tpm"){
                # Create plot
                p1 <- ggplot(data = df, mapping = aes(x = group, y = value, fill = group)) + 
                  geom_violin(alpha = 0.5) +
                  theme_classic() +
                  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), plot.title = element_text(hjust = 0.5)) +
                  xlab("") + ylab("TPM") + 
                  ggtitle(paste("Gene expression for ", geneToTest, sep = ""))
                
                p2 <- ggplot(data = df, mapping = aes(x = group, y = value, fill = group)) + 
                  geom_violin(alpha = 0.5) +
                  theme_classic() +
                  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 15), plot.title = element_text(size = 17, hjust = 0.5)) +
                  geom_boxplot(inherit.aes = F, mapping = aes(x = group, y = value), width = 0.1, fill = "gray62", outlier.alpha = 0.5) + 
                  stat_boxplot(geom = "errorbar", width = 0.1) +
                  xlab("") + ylab("TPM") + 
                  ggtitle(paste("Gene expression for ", geneToTest, sep = ""))
                
              } else if(input$VioPlotSelectTPMScale2 == "logtpm"){
                df$value[df$value == 0] <- 0.0001
                df$value <- log(df$value)
                
                # Create plot
                p1 <- ggplot(data = df, mapping = aes(x = group, y = value, fill = group)) + 
                  geom_violin(alpha = 0.5) +
                  theme_classic() +
                  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), plot.title = element_text(hjust = 0.5)) +
                  xlab("") + ylab("log TPM") + 
                  ggtitle(paste("Gene expression for ", geneToTest, sep = ""))
                
                p2 <- ggplot(data = df, mapping = aes(x = group, y = value, fill = group)) + 
                  geom_violin(alpha = 0.5) +
                  theme_classic() +
                  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 15), plot.title = element_text(size = 17, hjust = 0.5)) +
                  geom_boxplot(inherit.aes = F, mapping = aes(x = group, y = value), width = 0.1, fill = "gray62", outlier.alpha = 0.5) + 
                  stat_boxplot(geom = "errorbar", width = 0.1) +
                  xlab("") + ylab("log TPM") + 
                  ggtitle(paste("Gene expression for ", geneToTest, sep = ""))
              }
              
              # Create download handler
              output$VioPlotDownload <- downloadHandler(
                filename = function(){paste(geneToTest, "_violin_plot.png", sep = "")},
                content = function(file){ggsave(filename = file, plot = (p2), device = "png", width = 20, height = 12, dpi = 150)}
              )
              
              # Enable download button
              shinyjs::enable("VioPlotDownload")
              
              # Create plotly version for display
              ggplotly(p1)
              
            }
        },
        error = function(e){
          return(NULL)
        }
        )
      })
    }
  }
)