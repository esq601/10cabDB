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
library(ggthemes)
library(plotly)
library(tidyselect)

pw <- "10CABpw"
#setwd("C:/Users/marc.a.eskew.mil/Documents/R/Aircraft Hours/CABpage/CABpage")
ui <- dashboardPage(skin="yellow",
  #### Dashboard Header ####             
  dashboardHeader(title="10th CAB Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Current Aircraft Status",tabName="current",icon=icon("info")),
      menuItem("Readiness & Maint. Historics",tabName = "historics",icon=icon("calendar",lib="glyphicon")),
      menuItem("Trend Analysis",tabName = "trends",icon=icon("signal",lib="glyphicon")),
      # menuItem("Aircraft Analysis",tabName = "aircraft",icon=icon("paperclip")),
      # menuItem("Monthly Reports",tabName = "month",icon=icon("calendar")),
      menuItem("DSR Upload (Admin)",tabName = "upload",icon=icon("arrow-circle-up")),
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
    tags$head(
      tags$link(rel="stylesheet",type="text/css",href="custom.css")
    ),
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
                  tabPanel(title="Hours to Phase",plotlyOutput("htpnow"),br(),
                           "Utilize 'Bin Range' slider to adjust hour bins"),
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
                  tabPanel(title="RTL Historics",plotOutput("rtfhistory"),
                           checkboxInput("smoothfilter","Show Smooth Line?",value=TRUE),
                           sliderInput("smoothrange","Smoothness",min=1,max=100,value=50)
                  ),
                  tabPanel(title="Maint. Historics",
                           plotOutput("mainthistory"),
                           uiOutput("maintchecker"))
                )
                ),
                column(6,box(width=NULL,
                  title="RTL Boxplot",footer="Boxplots display the median number of AC RTL for the time period. 
                  The middle 50% of the data is inside the box, and outlier spread on the whiskers.",
                   plotlyOutput("rtfboxplot")
                )
                )
              ),
              fluidRow(
                column(6,box(width=NULL,
                  title="Hours Flown",footer="Displays the total number of hours flown during set date bin",
                  plotOutput("hourbin")
                )
                ),
                column(6,box(width=NULL,
                  title="Maintenance Events",footer="Displays the total number of AC that go from RTL status to non-RTL status during date bin.",
                  plotlyOutput("eventbin")
                ))
              )
              
      ),
      tabItem(
        tabName = "trends",
        column(9,
               fluidRow(
                 column(6,
                        box(width=NULL,
                            title="Maintenance Event Ranks",
                            plotOutput("eventrank"),br(),
                            "Top 15 Aircraft by number of times transferring to non-RTL.")),
                 column(6,
                        box(width=NULL,
                            title="Down Time Ranks",
                            plotOutput("downtimerank"),br(),
                            "Top 15 Aircraft by number of days non-RTL.")),
                 column(12,tabBox(width=NULL,
                                 title="Up time Analysis",id="tabset3",
                                 tabPanel(title="Days Analysis",plotOutput("updowndays"),br(),
                                          "This chart displays a density plot of days between an aircraft going RTL to non-RTL"),
                                 tabPanel(title="ACFT Hours Analysis",plotOutput("updownhours"),br(),
                                          "This chart displays a density plot of the number of flight hours between an aircraft going RTL to non-RTL")
                 )
                 )
               )
        ),
        column(3,
               fluidRow(
                 tabBox(width=NULL,
                        title="Total Up Time Analysis",id="tabset4",
                        tabPanel(title="Day",plotOutput("updowndaystotal",height=800),br(),
                                 "This chart displays the difference by aircraft from the mean number of days to a downtime event."),
                        tabPanel(title="Hour",plotOutput("updownhourstotal",height=800),br(),
                                 "This chart displays the difference by aircraft from the mean number of ACFT Hrs. to a downtime event.")
                 )
               )
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
    showNotification("Upload Complete!")
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
    statusbin + geom_bin2d(aes(fill=freq)) + geom_text(aes(label=scales::percent(freq)),size=7)  + scale_fill_distiller(palette="YlOrBr",direction = 1)
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
      stat_bin(binwidth=input$binrange,aes(label=..count..),geom = "text",vjust=-.4)  + theme_pander() + scale_fill_pander()
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
    rtfhistoryplot1 <- rtfhistoryplot + geom_line(aes_string(color=input$category),size=1)  + 
      theme_pander() + scale_color_pander()
    
    if(input$smoothfilter) {
      rtfhistoryplot1 <- rtfhistoryplot1 + geom_smooth(span=(.01*input$smoothrange),size=1.5,se=FALSE)
    }
    
    rtfhistoryplot1
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
    rtfhistoryplot + geom_line(aes_string(linetype="A.C.STATUS",color=input$category),size=1)  + theme_pander() + scale_color_pander()
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
    req(input$category)
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
    hourbinplot + geom_bar(stat="identity",aes_string(fill=input$category)) + theme_pander() + scale_fill_pander()
  })
  
  #### Maintenance Event Setup ####
  
  
  dfevents <- reactive({
    dfevents1 <- logdata() %>%
      group_by(SERIAL.NUMBER) %>%
      arrange(SERIAL.NUMBER,DATE) %>%
      mutate(A.C.DOWN=if_else((as.character(RTL)=="N" & as.character(lag(RTL))=="Y") ,TRUE,FALSE)) %>%
      mutate(A.C.UP=if_else((as.character(RTL)=="Y" & as.character(lag(RTL))=="N") ,TRUE,FALSE))
    
    dfevents1$A.C.DOWN <- replace_na(dfevents1$A.C.DOWN,FALSE)
    dfevents1$A.C.UP <- replace_na(dfevents1$A.C.UP,FALSE)
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
      dplyr::filter(A.C.DOWN==TRUE) %>%
      dplyr::filter(DATE!=min(dfevents()$DATE)) %>%
      dplyr::summarise(Events=n())

    eventbindf
  })
    
 output$eventbin <- renderPlotly({
   req(input$daterange)
   
   eventbinplot <- ggplot(eventbindata(),aes(x=numfactor,y=Events))
   eventbinplotly <- eventbinplot + geom_bar(stat="identity",aes_string(fill=input$category))  + theme_pander() + scale_fill_pander()
   ggplotly(eventbinplotly)
 })
 
 #### Aircraft Event Ranking ####
 
 eventrankdata <- reactive({
   
   eventrankdf <- dfevents() %>%
     dplyr::filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
     dplyr::arrange_(input$category,"SERIAL.NUMBER","DATE") %>%
     dplyr::group_by_(input$category,"SERIAL.NUMBER") %>%
     dplyr::filter(A.C.DOWN==TRUE) %>%
     dplyr::summarise(Events=n()) %>%
     ungroup() %>%
     dplyr::arrange(desc(Events)) %>%
     slice(1:15)

   eventrankdf
 })
 
 output$eventrank <- renderPlot({
   req(input$daterange)
   
   eventrankplot <- ggplot(eventrankdata(),aes(x=reorder(SERIAL.NUMBER,Events),y=Events))
   eventrankplot + geom_bar(stat="identity",aes_string(fill=input$category))  +
     theme_pander() + scale_fill_pander() +xlab("Serial Number")+ coord_flip() 

 })
 
 #### Aircraft Down Time ####
 
 downtimedata <- reactive({
   
   downtimedf <- dfevents() %>%
     dplyr::filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
     dplyr::arrange_(input$category,"SERIAL.NUMBER","DATE") %>%
     dplyr::group_by_(input$category,"SERIAL.NUMBER") %>%
     dplyr::filter(RTL=="N") %>%
     dplyr::summarise(`Days Down`=n()) %>%
     ungroup() %>%
     dplyr::arrange(desc(`Days Down`)) %>%
     slice(1:15)
   
 })
 
 output$downtimerank <- renderPlot({
   req(input$daterange)
   
   downtimerankplot <- ggplot(downtimedata(),aes(x=reorder(SERIAL.NUMBER,`Days Down`),y=`Days Down`))
   downtimerankplot + geom_bar(stat="identity",aes_string(fill=input$category))+xlab("Serial Number") +
     coord_flip()  + theme_pander() + scale_fill_pander()
   
 })
 
 #### Downtime Analysis ####
 
 updowndata <- reactive({
   logdata() %>%
     dplyr::filter(DATE >= input$daterange[1] & DATE <= input$daterange[2]) %>%
     group_by(SERIAL.NUMBER) %>%
     arrange(SERIAL.NUMBER,DATE) %>%
     mutate(A.C.DOWN=if_else((as.character(RTL)=="N" & as.character(lag(RTL))=="Y") ,TRUE,FALSE)) %>%
     mutate(A.C.UP=if_else((as.character(RTL)=="Y" & as.character(lag(RTL))=="N") ,TRUE,FALSE)) %>%
     filter(A.C.UP==TRUE | A.C.DOWN==TRUE) %>%
     mutate(GoodHours=if_else((A.C.DOWN==TRUE) & lag(A.C.UP==TRUE),ACFT.HOURS-lag(ACFT.HOURS),0)) %>%
     mutate(GoodDays=if_else((A.C.DOWN==TRUE) & lag(A.C.UP==TRUE),DATE-lag(DATE),0)) %>%
     mutate(badDays=if_else((A.C.UP==TRUE) & lag(A.C.DOWN==TRUE),DATE-lag(DATE),0)) %>%
     mutate(badHours=if_else((A.C.UP==TRUE) & lag(A.C.DOWN==TRUE),ACFT.HOURS-lag(ACFT.HOURS),0))
 })
 
 updowntotaldata <- reactive({
   updowndata() %>%
     filter(badDays > 0) %>%
     group_by_(input$category,"SERIAL.NUMBER") %>%
     summarise(TotalDays = sum(badDays),TotalHours=sum(badHours)) %>%
     filter(TotalDays>0 | TotalHours>0) %>%
     ungroup() %>%
     mutate(DaysDiff = round(TotalDays-mean(na.omit(TotalDays)),digits=1)) %>%
     mutate(AboveBelow = if_else(DaysDiff>=0,TRUE,FALSE)) %>%
     mutate(HoursDiff = round(TotalHours-mean(na.omit(TotalHours)),digits=1)) %>%
     mutate(AboveBelowh = if_else(HoursDiff>=0,TRUE,FALSE))

 })
 
 updowngooddata <- reactive({
   updowndata() %>%
     filter(GoodDays >0)
 })
 
 output$updowndays <- renderPlot({
   req(input$daterange)
   
   updowndaysplot <- ggplot(updowngooddata(),aes(x=GoodDays))
   updowndaysplot + geom_density(aes_string(fill=input$category),alpha=.4) + 
     xlab("Days between Up and Down Status")  + theme_pander() + scale_fill_pander()
   
 })
 
 output$updownhours <- renderPlot({
   updownhoursplot <- ggplot(updowngooddata(),aes(x=GoodHours))
   updownhoursplot + geom_density(aes_string(fill=input$category),alpha=.4) + 
     xlab("ACFT Hours between Up and Down Status") + theme_pander() + scale_fill_pander()
 })
 
 output$updowndaystotal <- renderPlot({
   ggplot(updowntotaldata(),aes(x=reorder(SERIAL.NUMBER,DaysDiff),y=DaysDiff,label=DaysDiff)) +
     geom_segment(aes(y=0,x=reorder(SERIAL.NUMBER,DaysDiff),yend=DaysDiff,xend=reorder(SERIAL.NUMBER,DaysDiff)),color="black") +
     geom_point(stat='identity',aes(color=AboveBelow),  size=10)  +
     geom_text(color="white", size=4) +
     scale_color_manual(name="Average to Down Status", 
                        labels = c("Below Average", "Above Average"), 
                        values = c("TRUE"="#00ba38", "FALSE"="#f8766d")) +
     labs(title=paste0("Mean Days to Downtime: ",round(mean(na.omit(updowntotaldata()$TotalDays)),1))) + 
     xlab("Serial Number") +
     ylab("Mean Days Between Downtime") +
     theme(legend.position = "none") +
     coord_flip()
 })
 
 output$updownhourstotal <- renderPlot({
   dfupdown<- updowntotaldata() %>%
     filter(!is.na(HoursDiff))
   
   ggplot(dfupdown,aes(x=reorder(SERIAL.NUMBER,HoursDiff),y=HoursDiff,label=HoursDiff)) +
     geom_segment(aes(y=0,x=reorder(SERIAL.NUMBER,HoursDiff),yend=HoursDiff,xend=reorder(SERIAL.NUMBER,HoursDiff)),color="black") +
     geom_point(stat='identity',aes(color=AboveBelowh),  size=10) +
     geom_text(color="white", size=4) +
     scale_color_manual(name="Average ACFT Hrs. to Down Status", 
                        labels = c("Below Average", "Above Average"), 
                        values = c("TRUE"="#00ba38", "FALSE"="#f8766d")) +
     labs(title=paste0("Mean Hours to Downtime: ",round(mean(na.omit(dfupdown$TotalHours)),1))) + 
     xlab("Serial Number") +
     ylab("Mean ACFT Hrs. Between Downtime") +
     theme(legend.position = "none") +
     coord_flip()
 })
 
 
 #### Individual Aircraft Area!! ####
 
 
 
 }
# Run the application 
shinyApp(ui,server)
#runApp(shinyApp(ui,server), host="0.0.0.0", port=3168)


