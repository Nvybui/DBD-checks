#----------------------#
#      server.R        #
#----------------------#


#Change
options(shiny.maxRequestSize = 50*1024^2) 

server <- function(input,output){
  
  crfdraftvar <- 0
  donevar <- 0
  
  
  
  output$uploadxls <- renderMenu({
    
    fluidRow(
      column(width = 3,fileInput("file1", "Choose ALS File",
                                 multiple = TRUE,
                                 accept = c("text/xls",".xls"))
      ),
    )
  })
  
  output$numberBox <- renderValueBox({
    valueBox(
      value = 0, subtitle = "All Tasks", icon = icon("list"),
      color = "info",
      gradient = TRUE,
      footer = div("Click here"),
      elevation = 2
    )
  })
  
  output$finishedBox <- renderValueBox({
    valueBox(
      value = 0, "Tasks Done", icon = icon("check"),
      color = "lightblue",
      footer = div("Click here"),
      elevation = 2
    )
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      "0%", "Progress", icon = icon("bar-chart-o"),
      color = "teal",
      footer = div("Click here"),
      elevation = 2
    )
  })
  
  ##############** Accordion UI **################### 
  
  output$accordion <- renderUI({
    addPopover(
      id = "accordion",
      options = list(
        content = "Logic: ALS file tab 'CRF Draft', column: 'DefaultMatrixOID' = 'SCREEN'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion1",
      accordionItem(
        title = "DefaultMatrixOID = Screen",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML("<b>CRF draft Settings: Default Matrix</b><br>
          Confirm Default matrix is 'Screening' unless otherwise specified by TDM "
          )
      )
    ))
  })

  
  output$accordionlinknext <- renderUI({
    addPopover(
      id = "accordionlinknext",
      options = list(
        content = "Logic: ALS file tab 'Forms', column: 'ConfirmationStyle' = 'LinkNext'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion2",
      accordionItem(
        title = "ConfirmationStyle = LinkNext",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML("<b>Form attributes: Redirect</b><br>
          In the 'Forms' Tab, Column 'ConfirmationStyle' should always be 'LinkNext'.
          There might be trial specific exceptions. Please check with the TDM, if needed."
          )
        )
      )
    )
    
  })
  
  output$accordioncf <- renderUI({
    addPopover(
      id = "accordioncf",
      options = list(
        content = "Logic: Tab 'CustomFunctions' column 'FunctionName' = 'DA_IRT_submit_blank_TECHF_RAVEDT', search string
        should contain 'DA_IRT' / 'blank TECHF' / 'RAVEDT'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion3",
      accordionItem(
        title = "Check if CF is implemented in the trial",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("<b>Custom Function: DA_IRT_submit_blank_TECHF_RAVEDT_</b><br>For IRT trials: Check that this CF is implemented in the trial."))
        ) 
      )
    )
    
  })
  
  output$accordionsubjectcf <- renderUI({
    addPopover(
      id = "accordionsubjectcf",
      options = list(
        content = "Logic: Tab 'CustomFunctions' column 'FunctionName' = 'SUBJ_subject_status', search string
        should contain 'Subject' / 'Status' / 'SUBJ'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion4",
      accordionItem(
        title = "Check if Subject Status CF is implemented in the trial",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check that the CF that defines a <b>stable</b> subject status hierarchy is implemented in the trial."))
        ) 
      )
    )
    
  })
  
  output$accordionaecf <- renderUI({
    addPopover(
      id = "accordionaecf",
      options = list(
        content = "Logic: Tab 'CustomFunctions' column 'FunctionName' = 'AE_submit_Empty_DEC_ASERFLAG', search string
        should contain 'AE_submit' / 'Empty_DEC' / 'ASERFLAG'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion5",
      accordionItem(
        title = "Check if AE Empty DEC_ASERFLAG CF is implemented in the trial",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check that the CF <b>AE_xxx_submit_Empty_DEC_ASERFLAG</b> is implemented in the trial."))
        ) 
      )
    )
    
  })
  
  output$accordiondatadict <- renderUI({
    addPopover(
      id = "accordiondatadict",
      options = list(
        content = "Logic: Tab 'Fields', column 'CodingDictionary' and 'DataDictionaryName' should not be entered in the same row.",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion6",
      accordionItem(
        title = "Check if DataDictionary is empty when a CodingDictionary exists and vice-versa",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("If a CodingDictionary exists, an additional data dictionary should not be
                      assigned to the same field as due to a bug in RAVE, the coded terms do not appear in
                      the data extracts."))
        ) 
      )
    )
    
  })
  
  output$accordionsuppterm <- renderUI({
    addPopover(
      id = "accordionsuppterm",
      options = list(
        content = "Logic: Tab 'CoderSupplementalTerms', column 'SupplementalTerm' should be in this order:
        1. CMROUTE, 2. CMINDC, 3.CMINDCD1, 4.CMINDCD2, 5.CMINDCD3, 6.CMINDCD4, 7.CMINDCD5.",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion7",
      accordionItem(
        title = "Check order of Supplemental terms in CM form(s))",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Confirm that the order of the supplemental terms on CM form(s) is as follows:
                      <br> 1. CMROUTE, 2. CMINDC, 3. CMINDCD1, 4. CMINDCD2, 5. CMINDCD3, 6. CMINDCD4, 7.CMINDCD5"))
        ) 
      )
    )
    
  })
  
  output$accordionquerync <- renderUI({
    addPopover(
      id = "accordionquerync",
      options = list(
        content = "Logic: Tab 'Fields', column 'DataFormat' entries contain 'DD' or start with '$' and entry is not blank then 
        entry in column 'QueryNonConformance' = 'FALSE'.",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion8",
      accordionItem(
        title = "QueryNonConformance is ticked for all numeric and date fields",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Confirm that the flag for auto-query for non-conformant data is ticked for all <b>numeric</b>
                      and <b>date fields</b> (Metadata best practise)."))
        ) 
      )
    )
    
  })
 
  output$accordionspecify <- renderUI({
    addPopover(
    id = "accordionspecify",
    options = list(
      content = "Logic: ALS file tab 'Forms', column: 'ConfirmationStyle' = 'LinkNext'",
      placement = "right",
      trigger = "hover"
    )
  )
    accordion(
      id = "accordion9",
      accordionItem(
        title = "DataDictionaryEntries (Specify = TRUE) compared to DataDictionaryName (QNC = TRUE)",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check if fields with 'Data Dictionaries' with Specify option = TRUE have the 
                      QueryNonConformance ticked in the 'Fields' Tab"))
        ) 
      )
    )
    
  })
  
  output$accordionallowadd <- renderUI({
      addPopover(
        id = "accordionallowadd",
        options = list(
          content = "Logic: Tab 'Matrices', if column 'Addable' = FALSE, entry in column 'Maximum' should be 1. If column 'Addable' is TRUE
          column 'Maximum' should be 1. Note: The Screening matrix is an exception.",
          placement = "right",
          trigger = "hover"
        )
      )
    accordion(
      id = "accordion10",
      accordionItem(
        title = "Setting of 'Allow Add'",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check if in Tab 'Matrices', if column 'Addable' = FALSE, entry
                      in column 'Maximum' = 1 OR if column 'Addable' has the entry TRUE, entry in column 
                      'Maximum = 200. Note: The Screening matrix is an exception."))
        ) 
      )
    )
    
  })
  
  output$accordionbreaksignature <- renderUI({
    addPopover(
      id = "accordionbreaksignature",
      options = list(
        content = "Logic: Tab 'Fields', if column 'DefaultValue' is entered and column 'DoesNotBreakSignature' = TRUE then 'EntryRestrictions'
        should contain 'Investigator' and 'Coordinator'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion11",
      accordionItem(
        title = "'DoesNotBreakSignature' column blank for defaulted values",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check if in Tab 'Fields' in column 'DefaultValue' entry is not blank and column
                      'DoesNotBreakSignature' = 'TRUE' and column 'EntryRestrictions' contains
                      'Investigator' or 'Coordinator'"))
        ) 
      )
    )
    
  })
  
  output$accordionSAE_Always <- renderUI({
    addPopover(
      id = "accordionSAE_Always",
      options = list(
        content = "Logic: FormOID in tab 'DataDictionaryEntries' and column 'DataDictionaryName' = zSAE_ALWAYS should match the FormOID in 
        tab 'Forms'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion12",
      accordionItem(
        title = "zSAE_ALWAYS dictionary included and FormOID adapted accordingly",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check that zSAE_ALWAYS dictionary is included in the 'DataDictionaryName' column in
                      the 'DataDictionaryEntries' tab and FormOID in the 'Forms' tab is matching."))
        ) 
      )
    )
    
  })
  
  output$accordionDays <- renderUI({
    addPopover(
      id = "accordionDays",
      options = list(
        content = "Logic: Tab 'CFolder' column 'CloseDays' and column 'AccessDays' should be empty.",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion13",
      accordionItem(
        title = "AccessDays and CloseDays not defined",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check that 'CloseDays' and 'AccessDays' are not defined in the 'Folder' Tab."))
        ) 
      )
    )
    
  })

  output$accordionVerify <- renderUI({
    addPopover(
      id = "accordionVerify",
      options = list(
        content = "Logic: Tab 'Fields' if column 'EntryRestrictions' does not contain 'Investigator' and 'Coordinator' and 
        column 'EntryRestrictions is entered and column 'isVisible = TRUE then the respective FormOID in the tab 'Forms' should 
        have the column 'isSignatureRequired' = TRUE.",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion14",
      accordionItem(
        title = "Verification/SourceDocument flag",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check that each field that is enterable by the site needs to be ticked as 'requires verification'."))
        ) 
      )
    )
    
  })
  
  output$accordionsignaturereq <- renderUI({
    addPopover(
      id = "accordionsignaturereq",
      options = list(
        content = "Logic: Tab 'Fields' if column 'EntryRestrictions' does not contain 'Investigator' and 'Coordinator' and 
        column 'EntryRestrictions is entered and column 'DoesNotBreakSignature = FALSE and column 'isVisible = TRUE,
        then column 'SourceDocument' should be TRUE.",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion15",
      accordionItem(
        title = "Not enterable fields ticked as 'Does not participate in Signature'",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check that each field that is enterable by the site has in the form 
                      tab the field 'isSignatureRequired ticked as TRUE'."))
        ) 
      )
    )
    
  })
  
  output$accordionstandardrange <- renderUI({
    addPopover(
      id = "accordionstandardrange",
      options = list(
        content = "Logic: Tab 'LabVariableMappings' if column GlobalVariableOID = 'Age' and FieldOID = 'LBAGE' 
        then LocationMethod should be 'OnLabPage'. If column GlobalVariableOID = 'SEX' and FieldOID = 'SEX',
        then LocationMethod should be 'EarliestDate'.",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion16",
      accordionItem(
        title = "Range Type is set to 'Standard'",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Confirm that Range Type is set to 'Standard' and that the mapping for SEX and AGE is as follows:<br>
                       1) SEX mapping is entered (Location = earliest date) <br>
                       2) AGE mapping is entered (Location = On lab page)"))
        ) 
      )
    )
    
  })
  
  output$accordionlabcf <- renderUI({
    addPopover(
      id = "accordionlabcf",
      options = list(
        content = "Logic: Tab 'CustomFunctions' and column 'FunctionName' should be 'LB_LOCAL_01_001'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion17",
      accordionItem(
        title = "cf LB_LOCAL_01_001 is implemented",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check if cf LB_LOCAL_01_001 is implemented."))
        ) 
      )
    )
    
  })
  
  output$accordionobslab <- renderUI({
    addPopover(
      id = "accordionobslab",
      options = list(
        content = "Logic: All variables in tab 'Fields' and column 'FormOID' that start with 'LB' and column 'FieldOID' = LBDAT should
        have column 'CanSetDataPageDate' set to TRUE",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion18",
      accordionItem(
        title = "Observation Date LBDAT",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Check if Observation Date of Form is ticked for LBDAT fields.")))))
  })
  
  output$accordionunsched <- renderUI({
    addPopover(
      id = "accordionunsched",
      options = list(
        content = "Logic: Tab that contains 'UNSCH' and column 'Matrix:UNSCH' = 'UNSCHED_01_001' and column 'UNSCHED' should be 'X'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion19",
      accordionItem(
        title = "Selection form in Unscheduled visit",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Confirm that only the selection form is assigned to the unscheduled visit folder and that the
                      selection form is setup according to the specifications.")))))
  })
  
  output$accordioncffolder <- renderUI({
    addPopover(
      id = "accordioncffolder",
      options = list(
        content = "Logic: Tab 'CustomFunctions' and column 'FunctionName' should be 'CF_Foldername_V1'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion20",
      accordionItem(
        title = "CF_Foldername_V1",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Confirm that CF_Foldername_V1 is implemented in the study.")))))
  })
  
  output$accordiondictterm <- renderUI({
    addPopover(
      id = "accordiondictterm",
      options = list(
        content = "Logic: Tab 'CustomFunctions' and column 'FunctionName' should be 'SET_DEC_DICTTERM'",
        placement = "right",
        trigger = "hover"
      )
    )
    accordion(
      id = "accordion21",
      accordionItem(
        title = "SET_DEC_DICTTERM",
        status = "gray",
        collapsed = TRUE,
        tags$div(
          class = "accordion-body",
          HTML(paste0("Confirm that SET_DEC_DICTTERM is implemented in the study.")))))
  })
  
  ########################################################################
  
  multiplesheets <- function(table){
    
    #getting info about all excel sheets
    
    #find all tabs that contain "unsch" in the excelfile
    unschd_names <- excel_sheets(table)
    unschdsheets <- unschd_names[str_detect(unschd_names,"UNSCH")]
    
    sheetnames <- c("CRFDraft","Forms","Fields","CustomFunctions","CoderSupplementalTerms","DataDictionaryEntries","Matrices","Folders","LabVariableMappings",unschdsheets)
    
    # Create empty list to store sheet data
    sheet_data <- list()
    
    # Loop over sheet names and read in data
    for(i in seq_along(sheetnames)){
      sheet_data[[i]] <- read_excel(table, sheet = sheetnames[i])
    }
    
    
    # sheets <-excel_sheets(table)[1:27] #getting the column names
    # tibble <- lapply(sheets, function(x) read_excel(table, sheet = x))
    xlstable <- lapply(sheet_data,as.data.frame)
    
    #assigning names to sheets
    names(xlstable) <- sheetnames
    return(xlstable)
    
  }
  
  ##############** InputFile **###################
  observeEvent(input$file1,{
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    #read in excel file and remove NAs
    
    alldf <- multiplesheets(file$datapath)
    
    ##############** Table **###################
    # Find the index of the "IsVisualVerify" column
    index <- which(colnames(alldf$Fields) == "IsVisualVerify")
    
    #Remove all columns after the "IsVisualVerify" column
    generaldf <- alldf$Fields[,1:index]
    #Convert char to factor for the filter function
    #generaldf[sapply(generaldf,is.character)] <- lapply(generaldf[sapply(generaldf,is.character)],as.factor)
    generaldf <- as.data.frame(unclass(generaldf), stringsAsFactors = TRUE)
 
    
    # Folder df
    folderdf <- alldf$Folders
    
    #CustomFunctions df
    cfdf <- alldf$CustomFunctions
    
    #CoderSupplementalTerms df
    suppdf <- alldf$CoderSupplementalTerms
    
    #CRFDraft df
    crfindex <- which(colnames(alldf$CRFDraft) == "SourceUrlId")
    crfdraftdf <- alldf$CRFDraft[1,]
    crfdraftdf <- crfdraftdf[1:crfindex]
    
    #DataDictionaryEntries df
    datadictentriesindex <- which(colnames(alldf$DataDictionaryEntries) == "Specify")
    datadictentriesdf <- alldf$DataDictionaryEntries[1:datadictentriesindex]
    
    
    #Forms df
    formsindex <- which(colnames(alldf$Forms) == "SourceUrlId")
    formsdf <- alldf$Forms[,1:formsindex]
    
    #Convert char to factor for the filter function
    formsdf <- as.data.frame(unclass(formsdf), stringsAsFactors = TRUE)
    
    #Matrices df
    matricesdf <- alldf$Matrices
    
    #LabVariableMappings df
    lvmindex <- which(colnames(alldf$LabVariableMappings) == "LocationMethod")
    lvmdf <- alldf$LabVariableMappings[,1:lvmindex]
    
    #Unscheduled df, find the index of unsch in alldf
    unschd_index <- which(grepl("UNSCH",names(alldf)))
    
    unschddf <- alldf[[unschd_index]]
    
    output$forms <- DT::renderDataTable({
      DT::datatable(formsdf,
                    style = 'bootstrap4', 
                    # extensions = c("Buttons","ColReorder","FixedColumns"),
                    # selection = "single",
                    filter = list(position = "top", clear = FALSE, plain = FALSE),
                    options = list(scrollX = TRUE,
                                   scrollY = "500px",
                                   paging = TRUE,
                                   pageLength = 12
                    )
      )
    })
    
    # output$crfdraft <- DT::renderDataTable({
    #   DT::datatable(crfdraftdf,
    #                 style = 'bootstrap4', 
    #                 # extensions = c("Buttons","ColReorder","FixedColumns"),
    #                 # selection = "single",
    #                 filter = list(position = "top", clear = FALSE, plain = FALSE),
    #                 options = list(scrollX = TRUE,
    #                                scrollY = "500px",
    #                                paging = TRUE,
    #                                pageLength = 12
    #                 )
    #   )
    # })
    
    output$fields <- DT::renderDataTable({
      DT::datatable(generaldf,
                    style = 'bootstrap4', 
                    #class = 'table-bordered table-condensed hover',
                    #caption =  htmltools::tags$caption(h3("ALS Table"), style="color:blue"),
                    extensions = c("Buttons","ColReorder","FixedColumns"),
                    selection = "none",
                    escape = FALSE,
                    filter = list(position = "top", clear = FALSE, plain = FALSE),
                    options = list(scrollX = TRUE,
                                   scrollY = "500px",
                                   paging = TRUE,
                                   pageLength = 12,
                                   server = FALSE,
                                   autoWidth = TRUE,
                                   scrollCollapse = TRUE,
                                   #fixedColumns = list(leftColumns = 3),
                                   colReorder = list(realtime = FALSE),
                                   dom = 'Bfrtip',
                                   buttons = list("csv","excel","pdf",
                                                  list(extend = 'colvis',columns = c(3:index))
                                   )))
      
    })
    
    
    #install.packages('dplyr', repos='https://cloud.r-project.org/')
 
    ##############** Accordion 21 **##################
    
    crfdraftvar <<- crfdraftvar + 1
    
    if(any(cfdf$FunctionName == "SET_DEC_DICTTERM")){
      cfdictterm <- paste0("<b>There is no issue for this check. SET_DEC_DICTTERM is implemented in the study.</b>")
    } else {
      cfdictterm  <- paste0("<b>SET_DEC_DICTTERM is not implemented in the study. Please check.</b>")
    }
    
    updatecfdictterm <- function(status){
      if (any(cfdf$FunctionName == "SET_DEC_DICTTERM")){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatecfdictterm <- updatecfdictterm("initial")
    
    output$accordiondictterm <- renderUI({
      accordion(
        id = "accordion21",
        accordionItem(
          title = "SET_DEC_DICTTERM",
          status = updatecfdictterm$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Confirm that SET_DEC_DICTTERM is implemented in the study.<br>",cfdictterm)))))
    })
   
    ##############** Accordion 20 **##################
    crfdraftvar <<- crfdraftvar + 1
    
    if(any(cfdf$FunctionName == "CF_Foldername_V1")){
      cffoldername <- paste0("<b>There is no issue for this check. CF_Foldername_V1 is implemented in the study.</b>")
    } else {
      cffoldername <- paste0("<b>CF_Foldername_V1 is not implemented in the study. Please check.</b>")
    }
    
    updatecffolder<- function(status){
      if (any(cfdf$FunctionName == "CF_Foldername_V1")){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatecffolder <- updatecffolder("initial")
    
    output$accordioncffolder <- renderUI({
      accordion(
        id = "accordion20",
        accordionItem(
          title = "CF_Foldername_V1",
          status = updatecffolder$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Confirm that CF_Foldername_V1 is implemented in the study.<br>",cffoldername)))))
    })
    
    ##############** Accordion 19 **##################
    
    crfdraftvar <<- crfdraftvar + 1
    
    unschdmatrix <- subset(unschddf, unschddf$`Matrix: UNSCH` == "UNSCHED_01_001" & unschddf$UNSCHED == "X")
    
    if(nrow(unschdmatrix) > 0){
      unschdname <- paste0("<b>There is no issue for this check.</b>")
    } else {
      unschdname <- paste0("<b>Matrix: UNSCH = UNSCHED_01_001 is not marked with an X in column 'UNSCHED'.")
    }
    
    updateunschd <- function(status){
      if (nrow(unschdmatrix) > 0){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updateunschd <- updateunschd("initial")
    
    output$accordionunsched <- renderUI({
      accordion(
        id = "accordion19",
        accordionItem(
          title = "Selection form in Unscheduled visit",
          status = updateunschd$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Confirm that only the selection form is assigned to the unscheduled visit folder and that the
                      selection form is setup according to the specifications.<br>",unschdname)))))
    })
   
    ##############** Accordion 18 **##################
    
    crfdraftvar <<- crfdraftvar + 1
    
    lbdat <- subset(generaldf, generaldf$FieldOID == "LBDAT" & generaldf$CanSetDataPageDate == "FALSE"
                    & grepl("LB",generaldf$FormOID))
    
    if(all(is.na(lvmdf))){
      
      updatelbdate <- function(status){
        donevar <<- donevar + 1
        status = "success"
        return(list(status = status, donevar = donevar))
      } 
      updatelbdate <- updatelbdate("initial")
      
      output$accordionobslab <- renderUI({
        accordion(
          id = "accordion18",
          accordionItem(
            title = "Observation Date LBDAT",
            status = "success",
            collapsed = TRUE,
            tags$div(
              class = "accordion-body",
              HTML(paste0("Check if Observation Date of Form is ticked for LBDAT fields.<br>
                          <b> According to the data, local lab is not used in this study. Therefore this check is not applicable.
                          </b>")))))
      })
      
      
    } else {
      
      if(nrow(lbdat) > 0){
        lbdatname <- paste0("<b>There may be ", nrow(lbdat), " entrie(s) where CanSetDataPage Date = FALSE. Please check.</b>")
      } else {
        lbdatname <- paste0("<b>There is no issue for this check.</b>")
      }
      
      updatelbdate <- function(status){
        if (nrow(lbdat) <= 0){
          donevar <<- donevar + 1
          status = "success"
        }
        else {
          #donevar <<- donevar
          status = "danger"
        }
        return(list(status = status, donevar = donevar))
      }
      
      updatelbdate <- updatelbdate("initial")
      
      output$accordionobslab <- renderUI({
        accordion(
          id = "accordion18",
          accordionItem(
            title = "Observation Date LBDAT",
            status = updatelbdate$status,
            collapsed = TRUE,
            tags$div(
              class = "accordion-body",
              HTML(paste0("Check if Observation Date of Form is ticked for LBDAT fields.<br>",lbdatname)))))
      })
    }
    
    ##############** Accordion 17 **##################
    
    crfdraftvar <<- crfdraftvar + 1
    
    if(all(is.na(lvmdf))){
      
      updatecflab <- function(status){
        donevar <<- donevar + 1
        status = "success"
        return(list(status = status, donevar = donevar))
      }
      
      updatecflab <- updatecflab("initial")
      
      output$accordionlabcf <- renderUI({
        accordion(
          id = "accordion17",
          accordionItem(
            title = "cf LB_LOCAL_01_001 is implemented",
            status = "success",
            collapsed = TRUE,
            tags$div(
              class = "accordion-body",
              HTML(paste0("Check if cf LB_LOCAL_01_001 is implemented.<br>
                          <b> According to the data, local lab is not used in this study. Therefore this check is not applicable.
                          </b>")
                   )
              )
            )
          )
      })
      
    } else {
  
      if(any(cfdf$FunctionName == "LB_LOCAL_01_001") == TRUE){
        cflabname <- paste0("<b>The cf LB_LOCAL_01_001 is implemented in the study.</b>")
      }
      else {
        cflabname <- paste0("<b>cf LB_LOCAL_01_001 can not be found in the file. Please check.</b>")
      }
   
      updatecflab <- function(status){
        if (any(cfdf$FunctionName == "LB_LOCAL_01_001") == TRUE){
          donevar <<- donevar + 1
          status = "success"
        }
        else {
          #donevar <<- donevar
          status = "danger"
        }
        return(list(status = status, donevar = donevar))
      }
      
      updatecflab <- updatecflab("initial")
      
      output$accordionlabcf <- renderUI({
        accordion(
          id = "accordion17",
          accordionItem(
            title = "cf LB_LOCAL_01_001 is implemented",
            status = updatecflab$status,
            collapsed = TRUE,
            tags$div(
              class = "accordion-body",
              HTML(paste0("Check if cf LB_LOCAL_01_001 is implemented.<br>", cflabname)
                   )
              )
            )
          )
      })
    }
   
    ##############** Accordion 16 **##################
    crfdraftvar <<- crfdraftvar + 1
    
    if(all(is.na(lvmdf))){
      
      updatelvm <- function(status){
        donevar <<- donevar + 1
        status = "success"
        
        return(list(status = status, donevar = donevar))
      }
      
      updatelvm <- updatelvm("initial")
      
      output$accordionstandardrange <- renderUI({
        accordion(
          id = "accordion16",
          accordionItem(
            title = "Range Type is set to 'Standard'",
            status = "success",
            collapsed = TRUE,
            tags$div(
              class = "accordion-body",
              HTML(paste0("Confirm that Range Type is set to 'Standard' and that the mapping for SEX and AGE is as follows:<br>
                       1) SEX mapping is entered (Location = earliest date) <br>
                       2) AGE mapping is entered (Location = On lab page) <br>
                          <b> According to the data, local lab is not used in this study. Therefore this check is not applicable.
                          </b>")))))
      })
      
    } else {
      lvmage <- subset(lvmdf,lvmdf$GlobalVariableOID == "Age" & lvmdf$FieldOID == "LBAGE" & lvmdf$LocationMethod == "OnLabPage")
      lvmsex <- subset(lvmdf, lvmdf$GlobalVariableOID == "Sex" & lvmdf$FieldOID == "SEX" & lvmdf$LocationMethod == "EarliestDate")
      
      if(nrow(lvmage) <= 0 & nrow(lvmsex) <= 0){
        lvmname <- paste0("<b>There may be an issue with the FieldOID or LocationMethod for both Sex and Age. Please check.</b>")
      } else if (nrow(lvmage) > 0 & nrow(lvmsex) <= 0){
        lvmname <- paste0("<b>There may be an issue with the FieldOID or LocationMethod for Sex. Please check.</b>")
      } else if (nrow(lvmage) <= 0 & nrow(lvmsex) > 0){
        lvmname <- paste0("<b>There may be an issue with the FieldOID or LocationMethod for Age. Please check.</b>")
      } else {
        lvmname <- paste0("<b>There is no issue for this check.</b>")
      }
      
      updatelvm <- function(status){
        if (nrow(lvmage) > 0 & nrow(lvmsex) > 0){
          donevar <<- donevar + 1
          status = "success"
        }
        else {
          #donevar <<- donevar
          status = "danger"
        }
        return(list(status = status, donevar = donevar))
      }
      
      updatelvm <- updatelvm("initial")
      
      output$accordionstandardrange <- renderUI({
        accordion(
          id = "accordion16",
          accordionItem(
            title = "Range Type is set to 'Standard'",
            status = updatelvm$status,
            collapsed = TRUE,
            tags$div(
              class = "accordion-body",
              HTML(paste0("Confirm that Range Type is set to 'Standard' and that the mapping for SEX and AGE is as follows:<br>
                       1) SEX mapping is entered (Location = earliest date) <br>
                       2) AGE mapping is entered (Location = On lab page) <br>",lvmname))
            )
          )
        )
      })
    }
  
    ###############** Accordion 15 **##################
    
    crfdraftvar <<- crfdraftvar + 1
    
    sigreqfields <- subset(generaldf,!grepl("Investigator",generaldf$EntryRestrictions) &
                             !grepl("Coordinator",generaldf$EntryRestrictions) & !is.na(generaldf$EntryRestrictions) &
                             generaldf$IsVisible == TRUE, select = FormOID)
    sigreqforms <- subset(formsdf,formsdf$IsSignatureRequired == FALSE, select = OID)
    
    any(sigreqfields$FormOID %in% sigreqforms$OID)
    sigreqfields$FormOID[which(sigreqfields$FormOID %in% sigreqforms$OID)]
    
    
    if(any(sigreqfields$FormOID %in% sigreqforms$OID) == TRUE){
      
      updatesigreq <- function(status){
        donevar <<- donevar
        status = "danger"
        return(list(status = status, donevar = donevar))
      }
      
      updatesigreq <- updatesigreq("initial")
      sigreqname <- paste0("<b> There may be a problem with the FormOID(s): ",
                           paste(sigreqfields$FormOID[which(sigreqfields$FormOID %in% sigreqforms$OID)],
                                 collapse = ","), " ,please check.<b/>")
    } else {
      sigreqname <- paste0("<b>There are no issues for this check.<b/>")
    }
    
    updatesigreq <- function(status){
      if (any(sigreqfields$FormOID %in% sigreqforms$OID) == FALSE){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatesigreq <- updatesigreq("initial")
    
    output$accordionsignaturereq <- renderUI({
      accordion(
        id = "accordion15",
        accordionItem(
          title = "Not enterable fields ticked as 'Does not participate in Signature'",
          status = updatesigreq$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check that each field that is enterable by the site has in the form
                      tab the field 'isSignatureRequired ticked as TRUE'.<br>",sigreqname))
          )
        )
      )
    })
    
    ###############** Accordion 14 **##################
    crfdraftvar <<- crfdraftvar + 1
    
    sourceverify <- subset(generaldf,!grepl("Investigator",generaldf$EntryRestrictions) &
                             !grepl("Coordinator",generaldf$EntryRestrictions) & !is.na(generaldf$EntryRestrictions) &
                             generaldf$DoesNotBreakSignature == "FALSE" & generaldf$IsVisible == "TRUE" &
                             generaldf$SourceDocument == "FALSE")
    
   
    if(nrow(sourceverify) > 0){
      subset_sourceverify <- head(sourceverify[, 1:2], n = 5)
      result <- apply(subset_sourceverify, 1, function(row) paste(row, collapse = ","))
      resultstring <- paste(result,collapse = "<br>")
      sourceverifyname <- paste0("<b> There are ", nrow(sourceverify), " findings for this issue. Please check.
                                 <br> FormOID | FieldOID (for the first 5 rows):<br>",resultstring)
      
      
    } else {
      sourceverifyname <- paste0("<b>There are no issues for this check.<b/>")
    }
    
    updatesourceverify<- function(status){
      if (nrow(sourceverify) <= 0){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatesourceverify <- updatesourceverify("initial")
    
    output$accordionVerify <- renderUI({
      accordion(
        id = "accordion14",
        accordionItem(
          title = "Verification/SourceDocument flag",
          status = updatesourceverify$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check that each field that is enterable by the site needs to be ticked as
                        'requires verification'.<br>",paste(sourceverifyname,collapse = ",")))
          )
        )
      )
    })
    
    ###############** Accordion 13 **##################
    
    crfdraftvar <<- crfdraftvar + 1
    
    closedays <- subset(folderdf,!is.na(folderdf$CloseDays))
    accessdays <- subset(folderdf,!is.na(folderdf$AccessDays))
    
    if(nrow(closedays) > 0 & nrow(accessdays) > 0){
      
      daysname <- paste0("<b>AccessDays and CloseDays are defined (not null). Please check.<b/>")
      
    } else if(nrow(closedays) > 0 & nrow(accessdays) <= 0){
      
      daysname <- paste0("<b>AccessDays is not defined but CloseDays is defined (not null). Please check.<b/>")
      
    } else if(nrow(closedays) <= 0 & nrow(accessdays) > 0){
      
      daysname <-  paste0("<b>CloseDays is not defined but AccessDays is defined (not null). Please check.<b/>")
      
    } else {
      daysname <- paste0("<b>There are no issues for this check. CloseDays and AccessDays are both not defined.</b>")
    }
    
    updatedays<- function(status){
      if (nrow(closedays) <= 0 & nrow(accessdays) <= 0){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar
        status = "danger"
      }
      
      return(list(status = status, donevar = donevar))
    }
    
    updatedays <- updatedays("initial")
    
    output$accordionDays <- renderUI({
      accordion(
        id = "accordion13",
        accordionItem(
          title = "AccessDays and CloseDays not defined",
          status = updatedays$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check that 'CloseDays' and 'AccessDays' are not defined in the 'Folder' Tab.<br>",daysname))
          )
        )
      )
    })
    
    ###############** Accordion 12 **################### 
    crfdraftvar <<- crfdraftvar + 1
   
    dict <- any(datadictentriesdf$DataDictionaryName == "zSAE_ALWAYS")
    if(!is.na(dict)){
     
       datadictsaeDM <- subset(datadictentriesdf,datadictentriesdf$DataDictionaryName == "zSAE_ALWAYS"
                               & grepl("DM",datadictentriesdf$CodedData))
      saeformOIDDM <- subset(formsdf,grepl(substr(datadictsaeDM$CodedData[1],5,13),formsdf$OID))
      datadictsaeAE <- subset(datadictentriesdf,datadictentriesdf$DataDictionaryName == "zSAE_ALWAYS"
                              & grepl("AE",datadictentriesdf$CodedData))
      saeformOIDAE <- subset(formsdf,grepl(substr(datadictsaeAE$CodedData[1],4,12),formsdf$OID))
      
      if(nrow(saeformOIDDM) > 0 & nrow(saeformOIDAE) > 0){
        
        datadictsaename <- paste0("<b>There are no issues for this check. The dictionary is included and the
                               FormOIDs in the CodedData column in the DataDictionaryEntries tab
                               and FormOID in the Forms tab for DM and AE are matching.</b>")
      } else if (nrow(saeformOIDDM) <= 0 & nrow(saeformOIDAE) >0){
        datadictsaename <- paste0("<b>The dictionary is included but the FormOID in the Forms tab for DM is not matching with the
                               FormOID in the CodedData column in the DataDictionaryEntries tab.</b><br>
                               FormOID: ",paste(substring(datadictsaeDM$CodedData[1],5,13),collapse = ","))
        
      } else if (nrow(saeformOIDDM) > 0 & nrow(saeformOIDAE) <=0){
        
        datadictsaename <- paste0("<b>The dictionary is included but the FormOID in the Forms tab for AE is not matching with the
                                  FormOID in the CodedData column in the DataDictionaryEntries tab.</b><br>
                                  FormOID: ",paste(substring(datadictsaeAE$CodedData[1],4,12),collapse = ","))
      } else {
        datadictsaename <- paste0("<b>The dictionary is included but the FormOIDs in the CodedData column in the
                                  DataDictionaryEntries tab and FormOID in the Forms tab for DM and AE are not matching.<br>
                                  FormOID(s): ",paste(substring(datadictsaeDM$CodedData[1],5,13),collapse = ",")
                                  ,",",paste(substring(datadictsaeAE$CodedData[1],4,12),collapse = ","),"</b>")
      }
      
      updatedatadictsae <- function(status){
        if (nrow(saeformOIDDM) > 0 & nrow(saeformOIDAE) > 0){
          donevar <<- donevar + 1
          status = "success"
        }
        else {
          status = "danger"
        }
        return(list(status = status, donevar = donevar))
      }
    }
    else {
      updatedatadictsae <- function(status){
        #donevar <<- donevar 
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
      datadictsaename <- paste0("<b>The dictionary zSAE_ALWAYS is missing. Please check.</b>")
    }
    
    updatedatadictsae <- updatedatadictsae("initial")
    
    output$accordionSAE_Always <- renderUI({
      accordion(
        id = "accordion12",
        accordionItem(
          title = "zSAE_ALWAYS dictionary included and FormOID adapted accordingly",
          status = updatedatadictsae$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check that zSAE_ALWAYS dictionary is included in the 'DataDictionaryName' column in
                      the 'DataDictionaryEntries' tab and FormOID in the 'Forms' tab is matching.<br>",datadictsaename))
          )
        )
      )
    })
 
    ###############** Accordion 11 **################### 
    
    crfdraftvar <<- crfdraftvar + 1
    
    breakfalse <- subset(generaldf,!is.na(generaldf$DefaultValue) & generaldf$DoesNotBreakSignature != TRUE,
                         select = c(FormOID,FieldOID,DefaultValue,DoesNotBreakSignature))
    
    # entryrestrictfalse <- subset(generaldf,!is.na(generaldf$DefaultValue) & generaldf$DoesNotBreakSignature != TRUE
    #                              & (!grepl("Investigator",generaldf$EntryRestrictions)|
    #                                !grepl("Coordinator",generaldf$EntryRestrictions)),
    #                              select = c(FormOID,FieldOID,EntryRestrictions))
    
    
    if(nrow(breakfalse) > 0) {
      breakname <- paste0("<b> There are/is ", nrow(breakfalse), " result(s) where DefaultValue is entered but DoesNotBreakSignature
                           is not TRUE.<br> FormOID: ",paste(breakfalse$FormOID,collapse = ","),
                          "<br>FieldOID: ",paste(breakfalse$FieldOID,collapse = ","))
    } else {
      breakname <- paste0("<b>There are no issues for this check. All entries where the DefaultValue is entered have the
                           DoesNotBreakSignature ticked as TRUE.</b>")
    }
    
    updatebreaksignature <- function(status){
      if (nrow(breakfalse) <= 0){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar 
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatebreaksignature <- updatebreaksignature("initial")
    
    output$accordionbreaksignature <- renderUI({
      accordion(
        id = "accordion11",
        accordionItem(
          title = "'DoesNotBreakSignature' column blank for defaulted values",
          status = updatebreaksignature$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check if in Tab 'Fields' in column 'DefaultValue' entry is not blank and column
                      'DoesNotBreakSignature' = 'TRUE'.<br>", breakname))
          )
        )
      )
    })
    
    ##############** Accordion 10 **################### 
    
    crfdraftvar <<- crfdraftvar + 1
    
    matricefalse <- matricesdf[matricesdf$Addable == FALSE & matricesdf$Maximum != 1,]
    matricetrue <- matricesdf[matricesdf$Addable == TRUE & matricesdf$Maximum != 200,]
    matricename <- ""

    if (nrow(matricefalse) > 0) {
      matricename <- paste0("<b>There are/is ", nrow(matricefalse), " result(s) where Addable = FALSE and Maximum is not 1. <br> Matrix/Matrices: ", toString(matricefalse$MatrixName),"</b>")
    }
    
    if (nrow(matricetrue) > 0) {
      separator <- ifelse(matricename == "", "", "<br> and ")
      matricename <- paste0("<b>",matricename, separator, "There are/is ", nrow(matricetrue), " result(s) where Addable = TRUE and Maximum is not 200. <br> Matrix/Matrices: ", toString(matricetrue$MatrixName),"</b>")
    }
    
    if (matricename == "") {
      matricename <- "<b>There are no issues for this check.</b>"
    }
    
    updatematrices <- function(status){
      if (nrow(matricefalse) <= 0 & nrow(matricetrue) <= 0){
        donevar <<- donevar + 1
        status = "success"
      }
      else if(nrow(matricefalse) == 1 & nrow(matricetrue) <= 0 & grepl("Screening",toString(matricefalse$MatrixName)) == TRUE){
        donevar <<- donevar + 1
        status = "success"  
      }
      else {
        #donevar <<- donevar 
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatematrices <- updatematrices("initial")
    
    output$accordionallowadd <- renderUI({
      accordion(
        id = "accordion10",
        accordionItem(
          title = "Setting of 'Allow Add'",
          status = updatematrices$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check if in Tab 'Matrices', if column 'Addable' = FALSE, entry
                      in column 'Maximum' = 1 OR if column 'Addable' has the entry TRUE, entry in column
                      'Maximum = 200<br>", matricename))
          )
        )
      )
    })

    ##############** Accordion 9 **###################
    crfdraftvar <<- crfdraftvar + 1
    
    
    datadictentries <- subset(datadictentriesdf, Specify == TRUE, select = DataDictionaryName)
    fieldsdatadict <- subset(generaldf,QueryNonConformance == FALSE, select = DataDictionaryName)
    fieldsdatadict <- unique(na.omit(fieldsdatadict))
    
    #Find common variables in both dataframes
    common <- intersect(fieldsdatadict$DataDictionaryName,datadictentries$DataDictionaryName)
    
    #Check if there are any, if yes print them out
    if(length(common) > 0){
      datadictname <- paste0("<b>There are DataDictionaryName entries where 'QueryNonConformance' is ticked as
                             FALSE but have 'Specify' ticked as TRUE in the DataDictionaryEntries Tab: ",
                             paste(common,collapse = ","),"</b>")
    }
    
    else {
      datadictname <- paste0("<b>All DataDictionaryName entries are ticked correctly where the 'Specify' option
                             is ticked as TRUE.</b>")
    }
    
    updatedatadictentries <- function(status){
      if (length(common) == 0){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar 
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatedatadictentries <- updatedatadictentries("initial")
    
    output$accordionspecify <- renderUI({
      accordion(
        id = "accordion9",
        accordionItem(
          title = "DataDictionaryEntries (Specify = TRUE) compared to DataDictionaryName (QNC = TRUE)",
          status = updatedatadictentries$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check if fields with 'Data Dictionaries' with Specify option = TRUE have the
                      QueryNonConformance ticked in the 'Fields' Tab<br>",datadictname)),
          )
        )
      )
    })
    
    ##############** Accordion 8 **###################
    
    crfdraftvar <<- crfdraftvar + 1
    
    #Subset dataframe to only variables that dont start with $,HH and yy, and have the QueryNonConformance
    #ticked as FALSE
    qnc_df <- subset(generaldf, grepl("^[^$|HH|yy]", DataFormat) & QueryNonConformance == FALSE, select = VariableOID)
    
    if (nrow(qnc_df) < 1){
      qnc_var <- paste0("QueryNonConformance is ticked for all numeric and date fields.")
    }
    else {
      qnc_var <- paste0("<b>QueryNonConformance is not ticked for the following variable(s): ",toString(qnc_df$VariableOID),"</b>")
    }
    
    updateqnc <- function(status){
      if (nrow(qnc_df) < 1){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar 
        status = "danger"
      }
      
      return(list(status = status, donevar = donevar))
    }
    
    updateqnc <- updateqnc("initial")
    
    output$accordionquerync <- renderUI({
      accordion(
        id = "accordion8",
        accordionItem(
          title = "QueryNonConformance is ticked for all numeric and date fields",
          status = updateqnc$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Confirm that the flag for auto-query for non-conformant data is ticked for all <b>numeric</b>
                      and <b>date fields</b> (Metadata best practise).<br>", qnc_var))
          )
        )
      )
      
    })
    
    
    ##############** Accordion 7 **###################
    
    crfdraftvar <<- crfdraftvar + 1
    
    #Define the correct order of the SupplementalTerm order
    correct_order_supp <- c("CMROUTE","CMINDC","CMINDID1","CMINDID2","CMINDID3","CMINDID4","CMINDID5")
    
    #check if the order matches the file
    supp_match <- all(match(suppdf$SupplementalTerm[1:7],correct_order_supp) == 1:length(correct_order_supp))
    
    if(supp_match == TRUE){
      suppvar <- paste0("<b>The order of the supplemental terms are correct.</b>")
    }
    else{
      #which() function is used to find the index of the first variable that is not in the correct order
      first_wrong_var <- which(match(suppdf$SupplementalTerm[1:7], correct_order_supp) != 1:length(correct_order_supp))[1]
      suppvar <- paste0("<b>The variable '", suppdf$SupplementalTerm[first_wrong_var], "' is not in the correct order.</b>")
      
    }
    
    updatesupp <- function(status){
      if (supp_match == TRUE){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar 
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatesupp <- updatesupp("initial")
    
    output$accordionsuppterm <- renderUI({
      accordion(
        id = "accordion7",
        accordionItem(
          title = "Check order of Supplemental terms in CM form(s))",
          status = updatesupp$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Confirm that the order of the supplemental terms on CM form(s) is as follows:
                      <br> 1. CMROUTE, 2. CMINDC, 3. CMINDCD1, 4. CMINDCD2, 5. CMINDCD3, 6. CMINDCD4, 7.CMINDCD5<br>",
                        suppvar))
          )
        )
      )
    })
    
    ##############** Accordion 6 **###################
    
    crfdraftvar <<- crfdraftvar + 1
    
    dict_var <- any(!is.na(generaldf$CodingDictionary) & !is.na(generaldf$DataDictionaryName))
    
    if(dict_var == FALSE){
      dictvar <- paste0("There are no entries where both DataDictionaryName and CodingDictionary have values.")
    }
    else{
      dictname <- generaldf$DataDictionaryName[which
                                              (!is.na(generaldf$CodingDictionary) & !is.na(generaldf$DataDictionaryName))]
      dictvar <- paste0("<b> There are entries where both DataDictionaryName and CodingDictionary have values.<br>
                        DataDictionaryName Variable(s): ",toString(dictname), "</b>.")
    }
    
    updatedict <- function(status){
      if (dict_var == FALSE){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar 
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatedict <- updatedict("initial")
    
    output$accordiondatadict <- renderUI({
      accordion(
        id = "accordion6",
        accordionItem(
          title = "Check if DataDictionary is empty when a CodingDictionary exists",
          status = updatedict$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("If a CodingDictionary exists, an additional data dictionary should not be
                      assigned to the same field as due to a bug in RAVE, the coded terms do not appear in
                      the data extracts.<br>",dictvar))
          )
        )
      )
    })
    
    ##############** Accordion 5 **###################
    
    crfdraftvar <<- crfdraftvar + 1
    
    ae_var <- any(grepl("AE",cfdf$FunctionName) & grepl("submit", cfdf$FunctionName) &
                    grepl("Empty_DEC", cfdf$FunctionName) & grepl("ASERFLAG", cfdf$FunctionName))
    
    if(ae_var == TRUE){
      ae_name <- cfdf$FunctionName[which(grepl("AE",cfdf$FunctionName) & grepl("submit", cfdf$FunctionName) &
                                  grepl("Empty_DEC", cfdf$FunctionName) & grepl("ASERFLAG", cfdf$FunctionName))]
      cfaevar <- tags$div(
        HTML(paste0("The Custom Function is implemented in the study with the name: <b>",ae_name,"<b>."))
      )
    }
    
    else {
      cfaevar <- paste("No CF could be found in the trial.")
    }
    
    updateaecf <- function(status){
      if (ae_var == TRUE){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
       #donevar <<- donevar + 1
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updateaecf <- updateaecf("initial")
    
    output$accordionaecf <- renderUI({
      accordion(
        id = "accordion5",
        accordionItem(
          title = "Check if AE Empty DEC_ASERFLAG CF is implemented in the trial",
          status = updateaecf$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check that the CF <b>AE_xxx_submit_Empty_DEC_ASERFLAG</b> is implemented in the trial.<br>",cfaevar))
          )
        )
      )
    })
    
    ##############** Accordion 4 **###################
    
    crfdraftvar <<- crfdraftvar + 1
    
    subj_var <- any(grepl("subject",cfdf$FunctionName) & grepl("status", cfdf$FunctionName) &
                      grepl("SUBJ", cfdf$FunctionName))
    
    if(subj_var == TRUE){
      subj_name <- cfdf$FunctionName[which(grepl("subject",cfdf$FunctionName) & grepl("status", cfdf$FunctionName) &
                                             grepl("SUBJ", cfdf$FunctionName))]
      cfsubjvar <- tags$div(
        HTML(paste0("The Custom Function is implemented in the study with the name:
                  <b>", subj_name,"</b>."))
      )
    }
    else {
      cfsubjvar <- paste("No CF could be found in the trial.")
    }
    
    updatesubjcf <- function(status){
      if (subj_var == TRUE){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        #donevar <<- donevar
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatesubjcf <- updatesubjcf("initial")
    
    output$accordionsubjectcf <- renderUI({
      accordion(
        id = "accordion4",
        accordionItem(
          title = "Check if Subject Status CF is implemented in the trial",
          status = updatesubjcf$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("Check that the CF that defines a <b>stable</b> subject status hierarchy
                        is implemented in the trial.",cfsubjvar))
          )
        )
      )
      
    })
    
    
    ##############** Accordion 3 **###################
    
    crfdraftvar <<- crfdraftvar + 1
    
    has_var <- any(grepl("DA_IRT", cfdf$FunctionName) & grepl("blank_TECHF", cfdf$FunctionName) &
                     grepl("RAVEDT", cfdf$FunctionName))
    
    if (has_var == TRUE){
      var_name <- cfdf$FunctionName[which(grepl("DA_IRT", cfdf$FunctionName)
                                          & grepl("RAVEDT", cfdf$FunctionName) & grepl("blank_TECHF", cfdf$FunctionName))]
      cfvar <- tags$div(
        HTML(paste0("The Custom Function is implemented in the study with the name:
                  <b>", var_name,"</b>."))
      )
      
    }
    else {
      cfvar <- paste("No CF could be found in the trial. Ignore this error if the study does not use IRT.")
    }
    
    updatecf <- function(status){
      if (has_var == TRUE){
        donevar <<- donevar + 1
        status = "success"
      }
      else {
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatecf <- updatecf("initial")
    
    output$accordioncf <- renderUI({
      accordion(
        id = "accordion3",
        accordionItem(
          title = "Check if CF is implemented in the trial",
          status = updatecf$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("<b>Custom Function: DA_IRT_submit_blank_TECHF_RAVEDT_</b><br>For IRT trials:
                        Check that this CF is implemented in the trial.<br>", cfvar
            ))
          )
        )
      )
    })

    ##############** Accordion 2 **###################
   
    crfdraftvar <<- crfdraftvar+1
    
    updatelinknext <- function(status){
      
      allvaluesmatch <- any(grepl("LinkNext",formsdf$ConfirmationStyle))
      nonmatchingindices <- which(formsdf$ConfirmationStyle != "LinkNext")
      nonmatchingvalues <- formsdf$ConfirmationStyle[nonmatchingindices]
      
      
      if(allvaluesmatch == TRUE){
        donevar <<- donevar + 1
        status = "success"
        linknextvar <- paste0("<b>The Confirmationstyle 'LinkNext' is available.</b>")
        
      } else {
        status = "danger"
        linknextvar <- paste0("<b><span style='color:red;'>For this file, the ConfirmationStyle is: ", 
                               unique(nonmatchingvalues),"</span></b>")
      }
      return(list(status = status,nonmatchingvalues = nonmatchingvalues, donevar = donevar, linknextvar = linknextvar))
    }
    
    updatelinknext <- updatelinknext("initial")
    
    output$accordionlinknext <- renderUI({
      accordion(
        id = "accordion2",
        accordionItem(
          title = "ConfirmationStyle = LinkNext",
          status = updatelinknext$status,
          collapsed = TRUE,
          tags$div(
            class = "accordion-body",
            HTML(paste0("In the 'Forms' Tab, Column 'ConfirmationStyle' should always be 'LinkNext'.
          There might be trial specific exceptions. Please check with the TDM, if needed.<br>",
                        updatelinknext$linknextvar))
          )
        )
      )
    })
    

    ##############** Accordion 1 **###################
    
    crfdraftvar <<- crfdraftvar + 1
    
    output$accordion <- renderUI({
      accordion(
        id = "accordion1",
        accordionItem(
          title = "DefaultMatrixOID = Screen",
          status = updatestatus$status,
          collapsed = TRUE,
          "Confirm Default matrix is 'Screening' unless otherwise specified by TDM "
        )
      )
    })
    
    updatestatus <- function(status) {
      
      if (crfdraftdf$DefaultMatrixOID == "SCREEN") {
        donevar <<- donevar + 1
        status = "success"
      } else {
        #donevar <<- donevar
        status = "danger"
      }
      return(list(status = status, donevar = donevar))
    }
    
    updatestatus <- updatestatus("initial")
    
    ##############** Update outputs **###################
    
    output$numberBox <- renderValueBox({
      valueBox(
        value = crfdraftvar, subtitle = "All Tasks", icon = icon("list"),
        color = "info",
        gradient = TRUE,
        footer = div("Click here"),
        elevation = 2
        
      )
    })
    
    
    # Add the results of all checks to this list
    check_results <- list(updatestatus,updatelinknext,updatecf,updatesubjcf,updateaecf,updatedict,updatesupp,updateqnc,
                          updatedatadictentries,updatematrices,updatebreaksignature,updatedatadictsae,updatedays,
                          updatesourceverify,updateunschd,updatecffolder,updatecfdictterm,updatecflab,updatesigreq,
                          updatelvm,updatelbdate)
    
    
    
    # Take the highest donevar 
    total_donevar <- max(sapply(check_results, function(x) x$donevar))
    
    output$finishedBox <- renderValueBox({
      valueBox(
        value = total_donevar, "Tasks Done", icon = icon("check"),
        color = "lightblue",
        footer = div("Click here"),
        elevation = 2
      )
    })
    
    progress <- round(total_donevar/crfdraftvar*100,0)
    printprogress <- paste0(progress,"%")
    output$progressBox <- renderValueBox({
      valueBox(
        printprogress, "Progress", icon = icon("bar-chart-o"),
        color = "teal",
        footer = div("Click here"),
        elevation = 2
      )
    })
    
    
    #################################################################################
    #To-Do List
    #Confirm flag auto-query: exclude DEC and SRF, alle Felder wo die drei Rollen Investigator, Coordinator die werden nicht von der site ausgefuellt
    
  })
  
  
}