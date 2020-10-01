## ExpressionPlot_shiny.R
## Server logic for creating an expression plot
##
## Created by Pooja Venkat
## Date: 16/07/2019
## Last updated: 04/08/2020

observeEvent(c(input$Select_plotfile_dot),{ 
  tryCatch(
    expr = {
      #check <<- 0
      shinyjs::disable("ExpressionDownload_dot")
      if(input$Select_plotfile_dot == "online_plot"){
        output$plotarea_dot <- renderPlotly({NULL})
        
        #Disable Offline options
        shinyjs::hide("dotTPM_file")
        shinyjs::hide("dotPatient_file")
        shinyjs::hide("selectPatient_dot_offline")
        shinyjs::hide("selectgenes2_dot_offline")
        
        #In case of a Switch from offline to online, able online options
        shinyjs::show("selectPatient_dot")
        shinyjs::show("selectgenes2_dot")
        
        #Reading the files directly
        tpm_online<<-read.delim(paste(dirLoc, "GeneExpression_TPM_Counts.txt", sep = ""),sep="\t",header=T, stringsAsFactors = F)
        colnames(tpm_online)<<-gsub(pattern="\\.",replacement="-",colnames(tpm_online))
        cncInfo_online<<-read.delim(paste(dirLoc,"Patients_Diagnosis.txt",sep=""),sep="\t",header=F,row.names=1)
        cncInfo_online<<-cncInfo_online[c(1,which(rownames(cncInfo_online) %in% colnames(tpm_online))),]
        cncInfo_1_online<<-read.delim(paste(dirLoc,"Patients_Diagnosis.txt",sep=""),sep="\t",header=T)
        cncInfo_1_online<<-cncInfo_1_online[which(cncInfo_1_online[,1] %in% colnames(tpm_online)),]
        
        # Check if file exists
        if(is.null(tpm_online) & is.null(cncInfo_1_online)){
          return(NULL)
        } else {
          #generating drop down lists for gene IDs and Patient names 
          ##Tried decreasing the number of list here for it to load faster
          genes_list <- tpm_online$gene_id
          updateSelectizeInput(session = session, inputId = "selectgenes2_dot", choices = genes_list, server = T)
          patient_id <- cncInfo_1_online[,1]
          updateSelectizeInput(session = session, inputId = "selectPatient_dot", choices = patient_id, server = T )
        }
      } else if (input$Select_plotfile_dot == "offline_plot"){
        output$plotarea_dot <- renderPlotly({NULL})
        
        #Able Offline options
        shinyjs::show("dotTPM_file")
        shinyjs::show("dotPatient_file")
        
        #Disable online options
        shinyjs::hide("selectPatient_dot")
        shinyjs::hide("selectgenes2_dot")
        
        #check<<-0
        #UI for file inputs
        output$dotTPM_file <- renderUI({fileInput(inputId = "dotTPMCounts", label = h4("Upload TPM counts file:"))})
        output$dotPatient_file <- renderUI({fileInput(inputId = "dotPatientmetadata", label = h4("Upload sample metadata file:"))})
      }
    },
    error = function(e){
      return(NULL)
    }
  )
})

#Read and extract from user uploaded file 
observeEvent(c(input$Select_plotfile_dot, input$dotTPMCounts, input$dotPatientmetadata ),{
  if(input$Select_plotfile_dot == "offline_plot"){
    #reading files
    inFile_dot <- input[["dotTPMCounts"]]
    inFile_dot1 <- input[["dotPatientmetadata"]]
    
    #Able gene list and Patient id selection
    shinyjs::show("selectgenes2_dot_offline")
    shinyjs::show("selectPatient_dot_offline")
    
    #Check if file exists
    if(is.null(inFile_dot) ){
      return(NULL)
    } else {
      # If exists check if read.table results in error
      tryCatch(expr = {
        #global variable for user uploaded file
        tpm_offline <<- read.delim(inFile_dot$datapath, header = TRUE, sep = "\t", stringsAsFactors = F)
        colnames(tpm_offline)<<-gsub(pattern="\\.",replacement="-",colnames(tpm_offline))
        genes_list_offline <- tpm_offline$gene_id
        updateSelectizeInput(session = session,inputId = "selectgenes2_dot_offline", choices = genes_list_offline,server=T)
      },
      error = function(e){
        # If error in read.table output NULL 
        return(NULL)
      })
    }
    #Check if file exists
    if(is.null(inFile_dot1)){
      return(NULL)
    } else {
      tryCatch(expr = {
        #global variable for user uploaded file
        #for plot generation
        cncInfo_offline<<-read.delim(inFile_dot1$datapath,sep="\t", header=F,row.names=1)
        
        #extract Patient ids
        cncInfo_1_offline<<-read.delim(inFile_dot1$datapath,sep="\t",header=T)
        patient_id_offline <- cncInfo_1_offline[,1]
        
        updateSelectizeInput(session = session,inputId = "selectPatient_dot_offline", choices = patient_id_offline,server=T )
        
      }, 
      error = function(e){
      # If error in read.table output NULL 
      return(NULL)
      })
    }
    ##Check if patients IDs correlate in both tpm and patient diagnosis file
    if(!is.null(tpm_offline) & !is.null(cncInfo_offline) & !is.null(cncInfo_1_offline) ){
      cncInfo_offline<<-cncInfo_offline[c(1,which(rownames(cncInfo_offline) %in% colnames(tpm_offline))),]
      cncInfo_1_offline<<-cncInfo_1_offline[which(cncInfo_1_offline[,1] %in% colnames(tpm_offline)),]
      patient_id_offline <- cncInfo_1_offline[,1]
      
      updateSelectizeInput(session = session,inputId = "selectPatient_dot_offline", choices = patient_id_offline,server=T )
      
    }
  }
})

#function to plot. id = input$selectPatient, geneList = input$selectgenes, tpm = gene file, cncInfo = Patient meta data, cncInfo_1 = Patient meta data w/ header = T
Expression_Dotplot <- function(id, geneList, tpm, cncInfo, cncInfo_1){
  cncInfo<-t(cncInfo)
  patLoc<-which(colnames(tpm)==id)
  patLocCnc<-which(colnames(cncInfo)==id)
  cncType<-cncInfo[1,patLocCnc]
  patLoc<-patLoc-2 #to take into account gene_id and transcript_id columns - change if this changes
  patLocCnc<-patLocCnc-1 #to take into account category naming
  typeCnc<-cncInfo[1,2:ncol(cncInfo)]
  grpall<-numeric(length(typeCnc))
  numPat<-typeCnc[patLocCnc]
  ##Number of patients with the same cancer type
  numCnc <- length(which(typeCnc %in% numPat))
  for(i in 1:length(typeCnc)){
    if(i==patLocCnc){grpall[i]<-2}else if(typeCnc[i]==numPat & numCnc!=1){grpall[i]<-3}else{grpall[i]<-1}
  }
  groups<-character(length(grpall))
  i=1
  forceId<-paste("z",id,sep="") #to place id alphabetically last to ensure in final grouping for plotting
  #forceId<-id
  for(i in 1:length(grpall)){
    if(grpall[i]==2){groups[i]<-forceId}else if(grpall[i]==3){groups[i]<-as.character(numPat)}else{groups[i]<-"all"}
  }
  if(length(geneList)>0){
    gene<-geneList
    index<-which(tpm$gene_id %in% gene)
    geneInfo<-tpm[index,]
    geneInfo<-geneInfo[,-1:-2]
    temp<-t(geneInfo)
    name<-c(rep(gene,ncol(geneInfo)))
    grpdat<-data.frame(TPM=as.numeric(temp),name=name,groups=groups)
    grpdat <- cbind(grpdat, cncInfo_1)
    maxVal<-max(geneInfo[1,])+1
    suppressWarnings({
      if(numCnc == 1){
        ##if a single sample is present within a  cancer type
        pl<-ggplot(grpdat,aes(name,TPM))+
          geom_hline(aes(yintercept=median(grpdat$TPM)), size = 0.1)+
          geom_dotplot(binaxis="y",stackdir="center",aes(fill=groups),binwidth=maxVal/60,dotsize=1.2, width = 0.8, alpha = 0.8)+
          scale_fill_manual(name="",labels=c("Cohort",as.character(id)),values=c("black","red"))+
          theme_classic()+theme(axis.text.x = element_text(size=25),axis.title.y=element_text(size=25))+
          labs(y="TPM",x="")
      }else{
        pl<-ggplot(grpdat,aes(name,TPM))+
        geom_hline(aes(yintercept=median(grpdat$TPM)), size = 0.1)+
        geom_dotplot(binaxis="y",stackdir="center",aes(fill=groups),binwidth=maxVal/60,dotsize=1.2, width = 0.8, alpha = 0.8)+
        scale_fill_manual(name="",labels=c("Cohort",as.character(cncType),as.character(id)),values=c("black","green","red"))+
        theme_classic()+theme(axis.text.x = element_text(size=25),axis.title.y=element_text(size=25))+
        labs(y="TPM",x="")
      }
    filename <- paste("Gene_", gene, "-rnaseq_personalised.png", sep = "")
    })
    
    #Create download handler
    output$ExpressionDownload_dot <- downloadHandler(
      filename = function(){filename},
      content = function(file){ggsave(filename = file, plot = pl, device = "png", width = 8)}
    )
    shinyjs::enable("ExpressionDownload_dot")
    grpdat$TPM <- round(grpdat$TPM, 3)
    
    #Creating bin manually
    breaks <- seq(0, maxVal, by=maxVal/60)
    bins <- cut(grpdat$TPM, breaks, right = TRUE, include.lowest = T)
    grpdat_1 <- cbind(grpdat, as.character(bins), as.character(bins))
    colnames(grpdat_1)[8] <- "bins"
    colnames(grpdat_1)[9] <- "bins1"
    b <- as.data.frame(summary(bins))
    b <- cbind(b, rownames(b))
    colnames(b) <- c("frequency", "bins")
    b$frequency <- as.numeric(b$frequency)
    
    #Retrive bin limits
    grpdat_1$bins1 <- gsub('[(]',"", grpdat_1$bins1)
    grpdat_1$bins1 <- gsub('[[]',"", grpdat_1$bins1)
    grpdat_1$bins1 <- gsub('[]]',"", grpdat_1$bins1)
    grpdat_1 <- separate(grpdat_1, bins1,c("low","up"),sep="([\\,])")
    grpdat_1$up <- as.numeric(grpdat_1$up)
    grpdat_1$low <- as.numeric(grpdat_1$low)
    grpdat_1$mid <- ((grpdat_1$up - grpdat_1$low)/2 + grpdat_1$low)
    
    #To plot inorder  
    b$factor <- c(1:60)
    gr <- merge(grpdat_1, b, by="bins" )
    gr$factor <- as.factor(gr$factor)
    gr$factor <- factor(gr$factor, levels = as.character(c(1:60)))
    b$bins <- as.character(b$bins)
    gr$bins <- as.character(gr$bins)
    gr$groups <- as.character(gr$groups)
    gr$colour[gr$groups=="all"] <- "black"
    gr$colour[grep("^z", gr$groups)] <- "red"
    gr$colour[gr$groups!="all" & !(grepl("^z", gr$groups))] <- "green"
    gr$groups[grep("^z", gr$groups)] <- as.character(gr[grep("^z", gr$groups),1])
    gr$groups[gr$groups=="all"] <- "Cohort"
    colourGroup <- gr$colour
    names(colourGroup) <- gr$groups
    gr$size[gr$colour=="black"] <- 2
    gr$size[gr$colour=="green"] <- 3
    gr$size[gr$colour=="red"] <- 4
    sizeGroup <- gr$size
    names(sizeGroup) <- gr$groups
    gr$shape[gr$colour=="black"] <- 16
    gr$shape[gr$colour=="green"] <- 15
    gr$shape[gr$colour=="red"] <- 17
    shapeGroup <- gr$shape
    names(shapeGroup) <- gr$groups
    
    #p <- "ggplot(data=gr, aes(name, mid, colour=groups, Patient_Id= Patient.ID, TPM =TPM, Diagnosis =Diagnosis, size=groups, shape=groups))+ geom_hline(aes(yintercept=median(gr$TPM)))+labs(y='TPM',x='')+scale_size_manual(values=c(2,3,4), name='', labels=c(as.character(q),as.character(cncType),as.character(id)))+scale_shape_manual(name='', labels=c(as.character(q),as.character(cncType),as.character(id)), values=c(16,17,15))+scale_colour_manual(name='',labels=c('Cohort',as.character(cncType),id),values=c('black','green','red'))"
    p <- "ggplot(data=gr, aes(name, mid, colour=groups, Patient_Id=Patient.ID, TPM=TPM, Diagnosis=Diagnosis, size=groups, shape=groups))+ geom_hline(aes(yintercept=median(gr$TPM)))+labs(y='TPM',x='')+scale_colour_manual(values=colourGroup)+scale_size_manual(values=sizeGroup)+scale_shape_manual(values=shapeGroup)"
    
    bin_value<- list()
    for (x in 1:nrow(b)) {
      if (b$frequency[x] > 50){
        bin_value[[x]] = which(gr$bins == b$bins[x])
        p <-paste(p,"+geom_point(data = gr[bin_value[[", x,"]],], position = position_jitter(width=0.5, height =0), aes(name,mid))", sep="" ) 
      } else if(b$frequency[x] <=50 & b$frequency[x] >=10 ){
        bin_value[[x]] = which(gr$bins == b$bins[x])
        p <-paste(p,"+geom_point(data = gr[bin_value[[", x,"]],], position = position_jitter(width=0.3, height =0), aes(name,mid))", sep="" ) 
      } else if(b$frequency[x] <10 & b$frequency[x] >=2){
        bin_value[[x]] = which(gr$bins == b$bins[x])
        p <-paste(p,"+geom_point(data = gr[bin_value[[", x,"]],], position = position_jitter(width=0.1, height =0), aes(name,mid))", sep="" ) 
      } else if(b$frequency[x] ==1){
        bin_value[[x]] = which(gr$bins == b$bins[x])
        p <-paste(p,"+geom_point(data = gr[bin_value[[", x,"]],], aes(name,mid))", sep="" ) 
      }
    }
    q <- "Cohort"
    #p <- paste(p, "+scale_colour_manual(labels=c('Cohort',as.character(cncType),id),values=c('black','green','red'))")
    p <- paste(p)
    #ggplotly(print(eval(parse(text = p))), tooltip=c("Patient_Id","TPM", "Diagnosis"))
    tooltip <- c("Patient_Id","TPM", "Diagnosis")
    set.seed(1234) ## ADDED TO KEEP CONSISTENT PLOTS WHEN CHANGING PATIENT OR GENE
    output$plotarea_dot <- renderPlotly({
      ggplotly(print(eval(parse(text = p))), tooltip=c("Patient_Id","TPM", "Diagnosis"))
    })
  }
  #return(pl, filename, p, tooltip)
}

#online Plot
observeEvent(c(input$selectgenes2_dot, input$selectPatient_dot,input$Select_plotfile_dot ),{
  if(input$Select_plotfile_dot =="online_plot" ){
    if(nchar(input$selectgenes2_dot) > 0 & nchar(input$selectPatient_dot) > 0){
      if ((ncol(tpm_online) - 2) > nrow(cncInfo_1_online)){
        tpm_online <<- tpm_online[,1:(nrow(cncInfo_1_online)+2)]
      }else if((ncol(tpm_online) - 2) < nrow(cncInfo_1_online)){
        cncInfo_1_online <<- cncInfo_1_online[1:(ncol(tpm_online)-2),]
        cncInfo_online <<- cncInfo_online[1:(ncol(tpm_online)-1),]
      }
      #Expression_Dotplot(id, geneList, tpm, cncInfo, cncInfo_1)
      #id = input$selectPatient, geneList = input$selectgenes, tpm = gene file, cncInfo = Patient meta data, cncInfo_1 = Patient meta data w/ header = T
      Expression_Dotplot(input$selectPatient_dot,input$selectgenes2_dot,tpm_online, cncInfo_online, cncInfo_1_online)
    }
  }
})

#offline plot
observeEvent(c(input$selectgenes2_dot_offline, input$selectPatient_dot_offline,input$Select_plotfile_dot ),{
   if (input$Select_plotfile_dot =="offline_plot"){
    if(nchar(input$selectgenes2_dot_offline) > 0 & nchar(input$selectPatient_dot_offline) > 0){
      if ((ncol(tpm_offline) - 2) > nrow(cncInfo_1_offline)){
        tpm_offline <<- tpm_offline[,1:(nrow(cncInfo_1_offline)+2)]
      }else if((ncol(tpm_offline) - 2) < nrow(cncInfo_1_offline)){
        cncInfo_1_offline <<- cncInfo_1_offline[1:(ncol(tpm_offline)-2),]
        cncInfo_offline <<- cncInfo_offline[1:(ncol(tpm_offline)-1),]
      }
      #Expression_Dotplot(id, geneList, tpm, cncInfo, cncInfo_1)
      #id = input$selectPatient, geneList = input$selectgenes, tpm = gene file, cncInfo = Patient meta data, cncInfo_1 = Patient meta data w/ header = T
      Expression_Dotplot(input$selectPatient_dot_offline,input$selectgenes2_dot_offline,tpm_offline, cncInfo_offline, cncInfo_1_offline)
    }
  }
})
