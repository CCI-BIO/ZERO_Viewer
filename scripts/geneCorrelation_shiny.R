## violinPlot_shiny.R
## Server logic for gene correlation plot generation
##
## Created by Nisitha Jayatilleke
## Date: 18/08/2020
## Last updated: 18/08/2020

# Initiliase widgets
observeEvent(
  c(input$GeneCorSelectOffline),
  {
    # Online widgets
    if(input$GeneCorSelectOffline == "online"){
      # TPM count table
      output$GeneCorTPMCounts <- renderUI({
        selectInput(
          inputId = "GeneCorTPMCounts2", 
          label = NULL,
          choices = "NA"
        )
      })
      shinyjs::hide("GeneCorTPMCounts")
    }
    # Offline widgets
    if(input$GeneCorSelectOffline == "offline"){
      # TPM count table
      output$GeneCorTPMCounts <- renderUI({
        fileInput(
          inputId = "GeneCorTPMCounts2",
          label = h4("Upload TPM counts file:")
        )
      })
      shinyjs::show("GeneCorTPMCounts")
    }
  }
)

# Create widgets for selecting genes to correlate
observeEvent(
  c(input$GeneCorSelectOffline, input$GeneCorTPMCounts2),
  {
    if(input$GeneCorSelectOffline == "online"){
      tryCatch(
        expr = {
          tpm_table <<- read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""), sep = "\t", header = T, row.names = 1)
          geneList <- rownames(tpm_table)
        },
        error = function(e){
          tpm_table <<- NULL
          geneList <<- NULL
        }
      )
    } else if (input$GeneCorSelectOffline == "offline"){
      inFile <- input[["GeneCorTPMCounts2"]]
      if(is.null(inFile)){
        return(NULL)
      } else {
        tryCatch(
          expr = {
            tpm_table <<- read.delim(inFile$datapath, sep = "\t", header = T, row.names = 1)
            geneList <- rownames(tpm_table)
          },
          error = function(e){
            tpm_table <<- NULL
            geneList <<- NULL
          }
        )
      }
    }
    if(!is.null(geneList)){
      updateSelectizeInput(
        session = session, 
        inputId = "GeneCorFirstGene", 
        choices = geneList, 
        server = T
      )
      updateSelectizeInput(
        session = session, 
        inputId = "GeneCorSecondGene", 
        choices = geneList, 
        server = T
      )
      output$GeneCorSelectMethod <- renderUI({
        selectInput(
          inputId = "GeneCorSelectMethod2",
          label = h4("Select correlation method:"),
          choices = list(
            "Pearson" = "pearson",
            "Kendall" = "kendall",
            "Spearman" = "spearman"
          ),
          multiple = F
        )
      })
    }
  }
)

observeEvent(
  c(input$GeneCorFirstGene, input$GeneCorSecondGene, input$GeneCorSelectMethod2),
  {
    tryCatch(
      expr = {
        if(!is.null(input$GeneCorFirstGene) & !is.null(input$GeneCorSecondGene)){
          modified_table <- tpm_table[,-which(colnames(tpm_table) %in% "transcript_id.s.")]
          tpm1 <- modified_table[which(rownames(modified_table) == input$GeneCorFirstGene),]
          tpm2 <- modified_table[which(rownames(modified_table) == input$GeneCorSecondGene),]
          tpm1[tpm1 == 0] <- 0.0001
          tpm2[tpm2 == 0] <- 0.0001
          tpm1 <- log(tpm1)
          tpm2 <- log(tpm2)
          
          tpm1 <- as.numeric(tpm1)
          tpm2 <- as.numeric(tpm2)
          
          correlation <- cor.test(tpm1, tpm2, method = input$GeneCorSelectMethod2)
          pval <- as.numeric(correlation$p.value)
          correlation_score <- as.numeric(correlation$estimate)
          
          if(input$GeneCorSelectMethod2 == "pearson"){
            corMethod <- "Pearson's"
          } else if(input$GeneCorSelectMethod2 == "kendall"){
            corMethod <- "Kendall's"
          } else if(input$GeneCorSelectMethod2 == "spearman"){
            corMethod <- "Spearman's"
          }
          
          output$GeneCorValues <- renderText({
            paste(corMethod, " r = ", round(correlation_score, digits = 2), ", p = ", format(pval, digits = 3, scientific = T), sep = "")
          })
          
          df <- cbind(tpm1, tpm2)
          df <- as.data.frame(df)
          df$Sample.ID <- colnames(modified_table)
          print(df)
          p <- ggplot(data = df, mapping = aes(x = tpm1, y = tpm2, Sample.ID = Sample.ID)) +
            geom_point() +
            geom_smooth(inherit.aes = F, data = df, mapping = aes(x = tpm1, y = tpm2), method = "lm") +
            xlab(paste(input$GeneCorFirstGene, " log(TPM)", sep = "")) +
            ylab(paste(input$GeneCorSecondGene, " log(TPM)", sep = ""))
          
          # Create download handler
          output$GeneCorPlotDownload <- downloadHandler(
            filename = function(){paste(input$GeneCorFirstGene, "_", input$GeneCorSecondGene, "_correlation_plot.png", sep = "")},
            content = function(file){ggsave(filename = file, plot = (p + ggtitle(paste(corMethod, " r = ", round(correlation_score, digits = 2), ", p = ", format(pval, digits = 3, scientific = T), sep = "")) + theme(plot.title = element_text(size = 17))), device = "png", width = 10, height = 6, dpi = 150)}
          )
          
          # Enable download button
          shinyjs::enable("VioPlotDownload")
          
          # Plotly plot
          output$GeneCorPlot <- renderPlotly({
            ggplotly(p)
          })
        }
      },
      error = function(e){
        return(NULL)
      }
    )
  }
)