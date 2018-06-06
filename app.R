library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(tidyr)
library(rsconnect)
library(readxl)
library(lubridate)
library(directlabels)
library(DT)
library(plotly)
library(tidyselect)

pw <- "10CABpw"
#setwd("C:/Users/marc.a.eskew.mil/Documents/R/Aircraft Hours/CABpage/CABpage")
ui <- dashboardPage(skin="yellow",
  #### Dashboard Header ####             
  dashboardHeader(title="10th CAB Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Navigation Links",
               menuSubItem("Current Aircraft Status",tabName="current",icon=icon("info")),
               menuSubItem("Readiness & Maint. Historics",tabName = "historics",icon=icon("graph")),
               menuSubItem("Trend Analysis",tabName = "trends",icon=icon("calendar-check")),
               menuSubItem("Aircraft Analysis",tabName = "aircraft",icon=icon("helicopter")),
               menuSubItem("Monthly Reports",tabName = "month",icon=icon("calendar")),
               menuSubItem("DSR Upload (Admin)",tabName = "upload",icon=icon("arrow-circle-up"))
      ),
      menuItem("Customization Tools", startExpanded = TRUE,
               radioButtons("category","Type Category",
                            choices = c("Unit"="UNIT","MDS"="MDS"),selected="MDS"),
               checkboxInput("dcofilter","Include UAS?",value=TRUE),
               uiOutput("unitSelect"),
               uiOutput("dateslider"),
               sliderInput("binrange","Bin Range",min=1,max=100,value=30)
      )
    )
  ),
  #### Dashboard Body ####
  dashboardBody(
    tabItems(
      tabItem(tabName = "current",
              fluidRow(
                column(3,box(title="Summary Stats",
                             textOutput("curtabdate"),
                             valueBoxOutput("totalACbox",width=NULL),
                             valueBoxOutput("totalRTLbox",width=NULL),
                             valueBoxOutput("totalpercentbox",width=NULL),width=NULL)),
                column(9,tabBox(
                  title="10th CAB Status",id="tabset1",
                  tabPanel(title="Ready to Fly",plotOutput("rtfnow")),
                  tabPanel(title="Maintenance Status",plotOutput("maintnow")),
                  tabPanel(title="Hours to Phase",plotlyOutput("htpnow")),
                  tabPanel(title="Location",tableOutput("locnow")),
                  width=NULL
                )
                )
              )
      ),
      tabItem(tabName = "historics",
              fluidRow(
                column(6,tabBox(width=NULL,
                  title="RTL/Maint Historics",id="tabset2",
                  tabPanel(title="RTL Historics",plotOutput("rtfhistory")),
                  tabPanel(title="Maint. Historics",
                           plotOutput("mainthistory"),
                           uiOutput("maintchecker"))
                )
                ),
                column(6,box(width=NULL,
                  title="RTL Boxplot",plotlyOutput("rtfboxplot")
                )
                )
              ),
              fluidRow(
                column(6,box(width=NULL,
                  title="Hours Flown",plotOutput("hourbin")
                )
                ),
                column(6,box(width=NULL,
                  title="Maintenance Events",plotOutput("eventbin")
                ))
              )
              
      ),
      tabItem(
              fluidRow(
                column(4,
                       fluidRow(
                         box(
                           fileInput("file1", "Select CAB Status Report",
                                   multiple = FALSE,
                                   accept = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                              ".xlsx")),
                            dateInput("reportdate","Status Report Date",startview = today()),width=NULL
                                 )
                         ),
                        fluidRow(
                          box(
                            passwordInput("password","Password: "),
                            textOutput("pass"),
                            fluidRow(
                              actionButton("executeUpload","Submit Document"),
                              downloadButton("downloadLog","Download Log")
                              ),width=NULL
                          )
                        )
                       ),
              column(8,box("Upload Preview",DTOutput("previewtable"),width=NULL))),
              fluidRow(
                column(12,box(title="Current Log Data",DTOutput("logdata"),width=NULL))
                
      ),tabName = "upload")
    )
  )
)

  ###### Server Function ######
server <- function(input, output,session) {

  #### Data set up ####
  load("aclog.rda")
  d2 <- AClog


  logdata <- reactive({
    
    dfilter <- input$dcofilter
    selectfilter <- input$unitSelectCall
    categoryfilter <- isolate(input$category)

    if(isTRUE(dfilter)){
      if(is.null(input$unitSelectCall)){
        AClog1 <- AClog
      }
      else {
        AClog1 <- AClog %>%
          dplyr::filter(MDS %in% c(input$unitSelectCall) | UNIT %in% c(input$unitSelectCall))
      }
    }
    
    else {
      if(is.null(input$unitSelectCall)){
        AClog1 <- AClog %>%
          filter(UNIT != "D Co. 10 GE")
      }
      else {
        AClog1 <- AClog %>%
          dplyr::filter(MDS %in% c(input$unitSelectCall) | UNIT %in% c(input$unitSelectCall)) %>%
          filter(UNIT != "D Co. 10 GE")
      }
    }
  })
  
  logdata1 <- reactive ({
    if(is.null(input$statusselect)) {
      AClog2 <- logdata()
    }
    else {
      AClog2 <- logdata() %>%
        filter(A.C.STATUS %in% c(input$statusselect))
    }
  })
  
  logevents <- reactive({
    eventdf <- logdata() %>%
      group_by(SERIAL.NUMBER) %>%
      mutate(Maint.Change=if_else(as.character(A.C.STATUS)!=as.character(lag(A.C.STATUS)),TRUE,FALSE)) 
    
    eventdf$Maint.Change[is.na(eventdf$Maint.Change)] <- TRUE
    
    eventdf <- eventdf %>%
      filter(Maint.Change == TRUE) %>%
      mutate(days = lead(DATE)-DATE) %>%
      arrange(SERIAL.NUMBER,DATE)
  })


  #### Dynmaic Input Render ####
  
  output$dateslider <- renderUI({
    dateiso <- isolate(logdata()$DATE)
    sliderInput("daterange","Date Range",min = min(dateiso),max=max(dateiso),
                value=c(min(dateiso),max(dateiso)))
  })
  
  output$unitSelect <- renderUI({
    unitiso <- isolate(logdata())
    unitiso <- unitiso %>%
      select_(input$category) %>%
      distinct()

    selectInput("unitSelectCall",paste(input$category),c(unitiso),multiple = TRUE)
  })
  
  output$maintchecker <- renderUI({
    maintiso <- isolate(logdata())
    maintiso <- maintiso %>%
      select(A.C.STATUS) %>%
      distinct()
    
    selectInput("statusselect","A.C. Status",c(maintiso),multiple=TRUE)
  })
  
  #### Data input tab ####
output$logdata <- renderDT({
  req(USER$logged == TRUE)
  datatable(logdata(),selection='none',editable=TRUE,filter="top",options=list(scrollX=TRUE))
})

  proxy <- dataTableProxy("logdata")

  observeEvent(input$logdata_cell_edit,{
    info <- input$logdata_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    d2[i,j] <<- DT::coerceValue(v,d2[i,j])
    replaceData(proxy,d2,resetPaging = FALSE)
    AClog <<- d2
    save(AClog,file="aclog.rda")
  })

  output$previewtable <- renderDT({

    req(input$file1)

    df1 <- read_excel(input$file1$datapath, sheet="1-10 ARB")
    df2 <- read_excel(input$file1$datapath, sheet="2-10 ASSAULT")
    df3 <- read_excel(input$file1$datapath, sheet="3-10 GSAB")
    df4 <- read_excel(input$file1$datapath, sheet="D Co. 10 GE")
    
    df3 <- select(df3, -X__6)

    colnames(df1) <- df1[1,]
    colnames(df2) <- df2[1,]
    colnames(df3) <- df3[1,]
    colnames(df4) <- colnames(df1)
    df1 <- df1[,1:11]
    df2 <- df2[,1:11]
    df3 <- df3[,1:11]
    df4 <- df4[,1:11]
    df1$UNIT <- "1-10 ARB"
    df2$UNIT <- "2-10 ASSAULT"
    df3$UNIT <- "3-10 GSAB"
    df4$UNIT <- "D Co. 10 GE"
    
    
    dfcombine <- bind_rows(df1,df2)
    dfcombine <- bind_rows(dfcombine,df3)
    dfcombine <- bind_rows(dfcombine,df4)

    
    valid_column_names <- make.names(names=names(dfcombine),unique=TRUE,allow_=TRUE)
    names(dfcombine) <- valid_column_names
    
    dfcombine[dfcombine==0] <- NA
    
    dfformat <- dfcombine %>%
      #rename(UNIT =X1.10.ARB) %>%
      tidyr::fill(MDS) %>%
      dplyr::select(UNIT,MDS, `SERIAL.NUMBER`, RTL,`ACFT.HOURS`,`A.C.STATUS`,`HRS.TO.PHASE`,LOC,ECD,REMARKS) %>% 
      filter(!is.na(`SERIAL.NUMBER`)) %>%
      filter(MDS!="MDS") %>%
      mutate(DATE=input$reportdate)
    
    datatable(dfformat,filter="top",options=list(pageLength=5,scrollX=TRUE))
  
  })
  
  USER <- reactiveValues(logged = FALSE)
  
  output$pass <- renderText({
    if (USER$logged == FALSE) {
      if(input$password > 0){
        password <- isolate(input$password)
        if(password == pw) {
          USER$logged <- TRUE
        }
        else {
          "Password incorrect!"
        }
      }
    }
  })

  observeEvent(input$executeUpload,{

    req(input$file1)
    req(USER$logged == TRUE)
    logdata()

    df1 <- read_excel(input$file1$datapath, sheet="1-10 ARB")
    df2 <- read_excel(input$file1$datapath, sheet="2-10 ASSAULT")
    df3 <- read_excel(input$file1$datapath, sheet="3-10 GSAB")
    df4 <- read_excel(input$file1$datapath, sheet="D Co. 10 GE")

    df3 <- select(df3, -X__6)
    source("dataupload.r",local=TRUE)
    save(AClog,file="aclog.rda")
  })
  
  output$downloadLog <- downloadHandler(
    filename = function() {
      paste("10CABLog ",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(logdata(), file, row.names = FALSE)
    }
  )
  
  ###### Current Stats Tab ######
  
  #### RTL Box ####
  rtldata <- reactive({
    logdata() %>%
      filter(DATE == max(DATE)) %>%
      group_by_(input$category,"RTL") %>%
      summarise(Number=n()) %>%
      arrange(desc(RTL))
  })
  
  output$rtfnow <- renderPlot({
    req(input$category)
    plotrtl <- ggplot(rtldata(),aes_string(x=input$category,y="Number"))
    plotrtl + geom_bar(stat="identity",aes(fill=RTL)) + geom_text(aes(label=Number),size=7,position=position_stack(vjust=.4))

  })
  
  #### Maint Status Box ####
  
  maintdata <- reactive({
    logdata() %>%
      filter(DATE==max(DATE)) %>%
      group_by_(input$category,"A.C.STATUS") %>%
      summarise(Number = n()) %>%
      mutate(freq = Number / sum(Number))
  })
  
  output$maintnow <- renderPlot({
    req(input$category)
    statusbin <- ggplot(maintdata(),aes_string(x=input$category,y="A.C.STATUS"))
    statusbin + geom_bin2d(aes(fill=freq)) + geom_text(aes(label=scales::percent(freq)),size=7)
  })

  #### Hours to Phase Box ####
  
  hoursdata <- reactive({
    logdata() %>%
      filter(DATE==max(DATE)) %>%
      group_by_(input$category) %>%
      filter(!is.na(HRS.TO.PHASE))
  })
  
  output$htpnow <- renderPlotly({
    req(input$category)
    htpplot <- ggplot(hoursdata(),aes(x=HRS.TO.PHASE))
    htpplotly <- htpplot + geom_histogram(binwidth = input$binrange,aes_string(fill=input$category)) + 
      stat_bin(binwidth=input$binrange,aes(label=..count..),geom = "text",vjust=-.4)
    ggplotly(htpplotly)
  })
  
  
  #### Value Boxes ####
  output$totalACbox <- renderValueBox({
    req(input$category)
    valueBox(sum(rtldata()$Number),"Total AC",icon=icon("plus"),color="blue")
  })
 
  totalRTL <- reactive({
    rtldata() %>%
    filter(RTL=="Y")
  })
   
  output$totalRTLbox <- renderValueBox({
    req(input$category)
    
    valueBox(sum(totalRTL()$Number),"Total RTL",icon=icon("thumbs-up"),color="green")
  })
  
  output$totalpercentbox <- renderValueBox({
    req(input$category)
    valueBox(scales::percent(sum(totalRTL()$Number)/sum(rtldata()$Number)),"Percent RTL",icon=icon("percent"),
             color="green")
  })
  
  output$curtabdate <- renderText({
    paste("As of: ", max(logdata()$DATE))
  })
  
  #### Location Box ####
  
  locationdata <- reactive({
    locdata <- logdata() %>%
      filter(DATE==max(DATE)) %>%
      group_by_(input$category,"LOC") %>%
      summarise(Number = as.integer(n())) %>%
      spread_(input$category,"Number") %>%
      mutate(Total = as.integer(rowSums(.[-1],na.rm=TRUE)))
     })
  ?mutate
  

  output$locnow <- renderTable({
    locationdata()
  })
  
  
  
  
  ##### Readiness and Maint Historics Tab #####
  #### RTF Historics Plot ####
  rtfhistorydata <- reactive({
    logdata() %>%
      filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
      group_by_(input$category,"DATE","RTL") %>%
      summarise(Number=n()) %>%
      spread(RTL,Number) %>%
      mutate(Percent = Y / (N+Y))
  })
  
  output$rtfhistory <- renderPlot({
    req(input$daterange)

    rtfhistoryplot <- ggplot(rtfhistorydata(),aes(x=DATE,y=Percent))
    rtfhistoryplot + geom_line(aes_string(color=input$category),size=1.5)
  })

  
  #### Maintenance Status Historics Plot ####

  mainthistorydata <- reactive({
    logdata1() %>%
      filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
      group_by_(input$category,"DATE","A.C.STATUS") %>%
      summarise(Number=n()) %>%
      arrange(A.C.STATUS)
  })

  output$mainthistory <- renderPlot({
    req(input$daterange)

    rtfhistoryplot <- ggplot(mainthistorydata(),aes(x=DATE,y=Number))
    rtfhistoryplot + geom_line(aes_string(linetype="A.C.STATUS",color=input$category),size=1)
  })
  
  
  #### RTF Box Plots ####
  rtfboxdata <- reactive({
    rtfbox <- logdata() %>%
      filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
      group_by_("DATE","RTL") %>%
      summarise(Number=n()) %>%
      spread(RTL,Number) 
    
    cuts <- seq(min(rtfbox$DATE),max(rtfbox$DATE),by=1)
    cutperiod <- paste0(input$binrange," days")
    
    rtfbox <- data.frame(rtfbox,numfactor = cut.Date(rtfbox$DATE,breaks = cutperiod))

    
    # rtfbox <- rtfbox %>%
    #   left_join(cuts,by="DATE") %>%
    #   fill(numfactor)
    rtfbox
  })

  output$rtfboxplot <- renderPlotly({
    avgplot <- ggplot(rtfboxdata())
    avgplot1 <- avgplot + geom_boxplot(aes(x=numfactor,y=Y,group=numfactor))
    ggplotly(avgplot1)
    })

  
  #### Hours Analysis ####
  hourbindata <- reactive({
    hourbindf <- as.data.frame(logdata())
    
    cuts <- seq(min(hourbindf$DATE),max(hourbindf$DATE),by=1)
    cutperiod <- paste0(input$binrange," days")
    
    hourbindf <- data.frame(hourbindf,numfactor = cut.Date(hourbindf$DATE,breaks = cutperiod))
    

    hourbindf <- hourbindf %>%
      dplyr::filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
      dplyr::group_by_("SERIAL.NUMBER") %>%
      dplyr::arrange_(input$category,"SERIAL.NUMBER","DATE") %>%
      dplyr::mutate(DAY.HOURS = ACFT.HOURS - lag(ACFT.HOURS,default=first(ACFT.HOURS))) %>%
      dplyr::group_by_(input$category,"numfactor") %>%
      dplyr::summarise(Hours = sum(DAY.HOURS))

  hourbindf
  })
  
  output$hourbin <- renderPlot({
    req(input$daterange)
    
    hourbinplot <- ggplot(hourbindata(),aes(x=numfactor,y=Hours))
    hourbinplot + geom_bar(stat="identity",aes_string(fill=input$category))
  })
  
  #### Maintenance Event Setup ####
  
  
  dfevents <- reactive({
    dfevents1 <- logdata() %>%
      group_by(SERIAL.NUMBER) %>%
      mutate(Maint.Change=if_else(as.character(A.C.STATUS)!=as.character(lag(A.C.STATUS)),TRUE,FALSE)) 
    
    dfevents1$Maint.Change[is.na(dfevents1$Maint.Change)] <- TRUE
    dfevents1
  })
  
 eventbindata <- reactive({
   
    eventbindf <- as.data.frame(dfevents())
    
    cuts <- seq(min(eventbindf$DATE),max(eventbindf$DATE),by=1)
    cutperiod <- paste0(input$binrange," days")

    eventbindf <- data.frame(eventbindf,numfactor = cut.Date(eventbindf$DATE,breaks = cutperiod))
    
    eventbindf <- eventbindf %>%
      dplyr::filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
      dplyr::group_by_("SERIAL.NUMBER") %>%
      dplyr::arrange_(input$category,"SERIAL.NUMBER","DATE") %>%
      dplyr::group_by_(input$category,"numfactor") %>%
      dplyr::filter(Maint.Change==TRUE) %>%
      dplyr::filter(DATE!=min(dfevents()$DATE)) %>%
      dplyr::summarise(Events=n())

    eventbindf
  })
    
 output$eventbin <- renderPlot({
   req(input$daterange)
   
   eventbinplot <- ggplot(eventbindata(),aes(x=numfactor,y=Events))
   eventbinplot + geom_bar(stat="identity",aes_string(fill=input$category))
 })
 
 }
# Run the application 
shinyApp(ui,server)
#runApp(shinyApp(ui,server), host="0.0.0.0", port=3168)


