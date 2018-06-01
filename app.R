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
                column(12,tabBox(
                  title="RTL/Maint Historics",id="tabset2",
                  tabPanel(title="RTL Historics",plotOutput("rtfhistory")),
                  tabPanel(title="Maint. Historics",
                           plotOutput("mainthistory"),
                           uiOutput("maintchecker")),
                  tabPanel(title="RTL Boxplot",plotlyOutput("rtfboxplot")),
                  width=NULL
                )
                )
              ),
              fluidRow(
                column(4,box(title="Hours by Month",plotOutput("monthhour"),width=NULL)),
                column(4,box(title="Cumulative Hours",plotOutput("cumplot"),width=NULL)),
                column(4,box(title="Whisker Plots",plotOutput("avghour"),width=NULL))
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
    # df1 <- isolate(input$category)
    # print(rtfhistorydata())

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

    print(mainthistorydata())

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
    
    cuts <- as.data.frame(seq(min(rtfbox$DATE),max(rtfbox$DATE),by=input$binrange))
    colnames(cuts) <- "DATE"
    cuts$numfactor <- as.factor(cuts$DATE)
    
    rtfbox <- as.data.frame(rtfbox)
    
    rtfbox <- rtfbox %>%
      left_join(cuts,by="DATE") %>%
      fill(numfactor)
    print(cuts)
    print(rtfbox)
    rtfbox
  })
  # means <- aggregate(Hours ~ Aircraft,rtfhistorydata(),mean)
  # means$Hours <- round(means$Hours,digits=1)
  output$rtfboxplot <- renderPlotly({
    avgplot <- ggplot(rtfboxdata())
    avgplot1 <- avgplot + geom_boxplot(aes(x=numfactor,y=Y,group=numfactor))#+ stat_summary(fun.y=median, color="darkred", geom="point", 
    ggplotly(avgplot1)#                                      shape=18, size=3,show.legend = FALSE)  
    # geom_text(data = means, aes(label = Hours, y = Hours + 0.08))
    })

 }
# Run the application 
shinyApp(ui,server)

