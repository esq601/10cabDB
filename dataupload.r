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
#dfcombine <- as.data.frame(dfcombine)
#str(df4)
#colnames(dfcombine) <- dfcombine[1,]

#df1 <- df1[-1,]
#df1 <- df1[,1:11]
#str(dfcombine)

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

dfformat$UNIT <- as.factor(dfformat$UNIT)
dfformat$MDS <- as.factor(dfformat$MDS)
dfformat$SERIAL.NUMBER <- as.factor(dfformat$SERIAL.NUMBER)
dfformat$RTL <- as.factor(toupper(dfformat$RTL))
dfformat$A.C.STATUS <- as.factor(dfformat$A.C.STATUS)
dfformat$LOC <- as.factor(dfformat$LOC)
dfformat$ACFT.HOURS <- as.numeric(dfformat$ACFT.HOURS)
dfformat$HRS.TO.PHASE <- as.numeric(dfformat$HRS.TO.PHASE)
dfformat$REMARKS <- as.character(dfformat$REMARKS)
dfformat$ECD <- as.numeric(dfformat$ECD)
dfformat$ECD <-as_date(dfformat$ECD)
dfformat$ECD <- dfformat$ECD - years(70) - days(1)

dfformat <- as_tibble(dfformat)


# dfold <- read.csv(input$file2$datapath)
# 
# 
# dfold$MDS <- as.factor(dfold$MDS)
# dfold$SERIAL.NUMBER <- as.factor(dfold$SERIAL.NUMBER)
# dfold$RTL <- as.factor(toupper(dfold$RTL))
# dfold$A.C.STATUS <- as.factor(dfold$A.C.STATUS)
# dfold$LOC <- as.factor(dfold$LOC)
# dfold$ACFT.HOURS <- as.numeric(dfold$ACFT.HOURS)
# dfold$DATE <- as_date(dfold$DATE)
# dfold$ECD <-as_date(dfold$ECD)
# dfold$REMARKS <- as.character(dfold$REMARKS)

dfold <- as_tibble(AClog)

#dfold <- rename(dfold, UNIT=ï..UNIT)
dfold[dfold==0] <- NA

if (max(dfformat$DATE) == max(dfold$DATE)) {
  dfnew <- dplyr::setdiff(dfformat,dfold)
  
  if (nrow(dfnew)>0) {
    for (i in 1:nrow(dfnew)){
      dfsubmit <- dfformat %>%
        filter(SERIAL.NUMBER != as.character(dfnew$SERIAL.NUMBER[i])) 
    }
    dfsubmit <- dfsubmit %>%
      bind_rows(dfnew)
  } else{
    dfsubmit <- dfold
  }
  
} else {
  dfsubmit <- bind_rows(dfold,dfformat)
}

AClog <- dfsubmit
