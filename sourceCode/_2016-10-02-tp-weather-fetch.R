library(RCurl)
library(XML)
library(httr)
library(scales)

#-----------------------------------------------#
# 1 time testing...
# 先設定好站點編號
stationID <- '466930'

# 撈取日報(逐小時的紀錄)
# 先設定好欄位名稱，撈取日期
tbNames <- c('ObsTime','StnPres','SeaPres','Temperature','TdDewPoint','RH','WS','WD','WSGust','WDGust','Precp','PrecpHour','SunShine','GloblRad','Visb')
fdate <- '2016-09-01'

cwbURL <- sprintf('http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=%s&stname=%s&datepicker=%s', stationID, '%25E8%2587%25BA%25E5%258C%2597', fdate)
weatherDate <- cwbURL %>% 
  getURL() %>% 
  htmlParse(., encoding = 'utf8', asText = TRUE) %>% 
  readHTMLTable(header = TRUE) %$%
  MyTable %>% 
  tbl_dt()

weatherDate %<>% filter(row.names(weatherDate) != 1)
weatherDate %<>% mutate_each(funs(as.numeric(as.character(.)))) %>% arrange(V1) 
names(weatherDate) <- tbNames
weatherDate %<>% mutate(station = '竹子湖', cdate = fdate, ctime = (as.POSIXct(fdate)+hours(ObsTime-1)))


# 撈取月報(逐日的紀錄)
# 先設定好欄位名稱，撈取日期
tbNames <- c('ObsTime','StnPres','SeaPres','StnPresMax','StnPresMaxTime','StnPresMin','StnPresMinTime','Temperature','TMax','TMaxTime','TMin','TMinTime','TdDewPoint','RH','RHMin','RHMinTime','WS','WD','WSGust','WDGust','WGustTime','Precp','PrecpHour','PrecpMax10','PrecpMax10Time','PrecpHrMax','PrecpHrMaxTime','SunShine','SunShineRate','GloblRad','VisbMean','EvapA')
fmonth <- '2016-09'

cwbURL <- sprintf('http://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=%s&stname=%s&datepicker=%s', stationID, '%25E7%25AB%25B9%25E5%25AD%2590%25E6%25B9%2596', fmonth)
weatherMonth <- cwbURL %>% 
  getURL() %>% 
  htmlParse(., encoding = 'utf8', asText = TRUE) %>% 
  readHTMLTable(header = TRUE) %$%
  MyTable %>% 
  tbl_dt()

weatherMonth %<>% filter(row.names(weatherMonth) != 1)
weatherMonth %<>% mutate_each(funs(as.numeric(as.character(.)))) %>% arrange(V1) 
names(weatherMonth) <- tbNames
weatherMonth %<>% mutate(station = '竹子湖', cdate = as.Date(str_c(fmonth, ObsTime, sep = '-')))


#-----------------------------------------------#
# 2016-09的三個氣象觀測站的日報
# loop 2016-09-01~2016-09-30 & 3 stations
stationID <- c('466880','466930','467440')
stationName <- c('板橋','竹子湖','高雄')
dateList <- seq.Date(from = as.Date('2016-09-01'), to = as.Date('2016-09-30'), by = 'days')
tbNames <- c('ObsTime','StnPres','SeaPres','Temperature','TdDewPoint','RH','WS','WD','WSGust','WDGust','Precp','PrecpHour','SunShine','GloblRad','Visb')


weatherDate3 <- NULL
for (i in 1:3){
  t0 <- Sys.time()

  weatherDate2 <- NULL
  for (j in 1:length(dateList)) {
    t00 <- Sys.time()
    Sys.sleep(time = runif(n=1, min=1, max=3))
    
    cwbURL <- sprintf('http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=%s&stname=%s&datepicker=%s', stationID[i], '%25E8%2587%25BA%25E5%258C%2597', dateList[j])
    tmpTb <- cwbURL %>% 
      getURL() %>% 
      htmlParse(., encoding = 'utf8', asText = TRUE) %>% 
      readHTMLTable(header = TRUE) %$%
      MyTable %>% 
      tbl_dt()
    
    tmpTb %<>% filter(row.names(tmpTb) != 1)
    tmpTb %<>% mutate_each(funs(as.numeric(as.character(.)))) %>% arrange(V1) 
    names(tmpTb) <- tbNames
    tmpTb %<>% mutate(station = stationName[i], cdate = dateList[j])
    
    weatherDate2 %<>% bind_rows(tmpTb)
    sprintf('stationName = %s, FetchDate = %s , time duration = %s min', stationName[i], dateList[j], difftime(Sys.time(), t00, units = 'mins')) %>% print()
  }
  
weatherDate3 %<>% bind_rows(weatherDate2)
sprintf('stationName = %s is done!!, time duration = %s min', stationName[i], difftime(Sys.time(), t0, units = 'mins')) %>% print()
}

# weatherDate3 %<>% mutate(ctime = as.POSIXct(cdate + hours(ObsTime-1)))
# write.table(weatherDate3, 'd:/weatherDate3.txt', row.names = FALSE, fileEncoding = 'UTF-8', sep = ',')
# weatherDate3 <- read.csv('d:/weatherDate3.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE, stringsAsFactors = FALSE) %>% tbl_dt()
weatherDate3 %<>% distinct()


#-----------------------------------------------#
# 2016-09的三個氣象觀測站的月報
# loop 3 stations
stationID <- c('466880','466930','467440')
stationName <- c('板橋','竹子湖','高雄')
tbNames <- c('ObsTime','StnPres','SeaPres','StnPresMax','StnPresMaxTime','StnPresMin','StnPresMinTime','Temperature','TMax','TMaxTime','TMin','TMinTime','TdDewPoint','RH','RHMin','RHMinTime','WS','WD','WSGust','WDGust','WGustTime','Precp','PrecpHour','PrecpMax10','PrecpMax10Time','PrecpHrMax','PrecpHrMaxTime','SunShine','SunShineRate','GloblRad','VisbMean','EvapA')
fmonth <- '2016-09'

weatherMonth2 <- NULL
for (i in 1:3){
  t0 <- Sys.time()
  
  cwbURL <- sprintf('http://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=%s&stname=%s&datepicker=%s', stationID[i], '%25E7%25AB%25B9%25E5%25AD%2590%25E6%25B9%2596', fmonth)
  weatherMonth <- cwbURL %>% 
    getURL() %>% 
    htmlParse(., encoding = 'utf8', asText = TRUE) %>% 
    readHTMLTable(header = TRUE) %$%
    MyTable %>% 
    tbl_dt()
  
  weatherMonth %<>% filter(row.names(weatherMonth) != 1)
  weatherMonth %<>% mutate_each(funs(as.numeric(as.character(.)))) %>% arrange(V1) 
  names(weatherMonth) <- tbNames
  weatherMonth %<>% mutate(station = stationName[i], cdate = as.Date(str_c(fmonth, ObsTime, sep = '-')))
  
  weatherMonth2 %<>% bind_rows(weatherMonth)
  sprintf('stationName = %s is done!!, time duration = %s min', stationName[i], difftime(Sys.time(), t0, units = 'mins')) %>% print()
}

# weatherMonth2
# write.table(weatherMonth2, 'd:/weatherMonth2.txt', row.names = FALSE, fileEncoding = 'UTF-8', sep = ',')
# weatherMonth2 <- read.csv('d:/weatherMonth2.txt', fileEncoding = 'UTF-8', sep = ',', header = TRUE, stringsAsFactors = FALSE) %>% tbl_dt()
# weatherMonth2 %<>% mutate(cdate = as.Date(cdate)) 

#-----------------------------------------------#
# Visualization
# 逐日記錄
weatherMonth2 %>% select(station, cdate, WS, Precp, PrecpHour) %>% 
  mutate(RainStrength = Precp/(PrecpHour/24)) %>% 
  select(-PrecpHour) %>% 
  gather(key = 'vType', value = 'value', -c(1,2)) %>% 
  mutate(value = ifelse(is.na(value), 0, value), cdate = as.Date(cdate)) %>% {
  flabels <- c('Precp' = '降水量', 'RainStrength' = '降水強度', 'WS' = '風速')  

    ggplot(data = ., aes(x = cdate, y = value, group = station, colour = station)) +
    geom_line(size = 1.5) + 
    labs(x = '', y = '', title = '2016-09 逐日氣象資料') +
    facet_grid(vType~., scales = 'free_y', labeller = as_labeller(flabels)) +
    theme_bw() +
    theme(strip.text = element_text(size = 10, face = 'bold'),
          title = element_text(size = 10, face = 'bold'),
          legend.text = element_text(size = 10, face = 'bold')) +
    annotate("rect", xmin=as.Date('2016-09-14'), xmax=as.Date('2016-09-15'), ymin=0, ymax=Inf, alpha=0.2, fill="red") +
    annotate("rect", xmin=as.Date('2016-09-27'), xmax=as.Date('2016-09-28'), ymin=0, ymax=Inf, alpha=0.2, fill="red") +
    annotate('text', label = '莫蘭蒂颱風', x = as.Date('2016-09-14'), y = Inf, vjust = 2, hjust = 1) +
    annotate('text', label = '梅姬颱風', x = as.Date('2016-09-27'), y = Inf, vjust = 2, hjust = 1) +
    geom_hline(data = data.frame(vType = 'WS'), aes(yintercept = 5), linetype = 2, colour = 'grey50')
}


# 逐小時記錄，板橋，梅姬颱風前後兩天?
weatherDate3 %>% 
  filter(cdate >= as.Date('2016-09-26') & cdate <= as.Date('2016-09-29')) %>% 
  select(station, ctime, Precp, WS) %>% 
  gather(key = 'vType', value = 'value', -c(1,2)) %>%
  mutate(value = ifelse(is.na(value), 0, value), ctime = as.POSIXct(strptime(ctime, format = '%Y-%m-%d %H:%M:%S'))) %>% 
  arrange(station, vType, ctime) %>% {
   
    flabels <- c('Precp' = '降水量', 'WS' = '風速')
    lim <- as.POSIXct(c(strptime('2016-09-26 00:00:00', format = '%Y-%m-%d %H:%M:%S'), strptime('2016-09-30 00:00:00', format = '%Y-%m-%d %H:%M:%S')))
    
    ggplot(data = ., aes(x = ctime, y = value, group = station, colour = station)) +
      geom_line(size = 1.5) + 
      labs(x = '', y = '', title = '2016-09-26~2016-09-29 逐時氣象資料') +
      facet_grid(vType~., scales = 'free_y', labeller = as_labeller(flabels)) +
      theme_bw() +
      theme(strip.text = element_text(size = 10, face = 'bold'),
            title = element_text(size = 10, face = 'bold'),
            legend.text = element_text(size = 10, face = 'bold'),
            axis.text.x = element_text(angle = 90)) +
      geom_hline(data = data.frame(vType = 'WS'), aes(yintercept = 5), linetype = 2, colour = 'grey50') +
      annotate("rect", xmin = as.POSIXct(strptime('2016-09-27', format = '%Y-%m-%d')), xmax = as.POSIXct(strptime('2016-09-29', format = '%Y-%m-%d')), ymin=0, ymax=Inf, alpha=0.2, fill="red") +
      scale_x_datetime(limits = lim, breaks = date_breaks("6 hour"), labels = date_format("%m/%d %H:%M", tz = 'Asia/Taipei')) +
      annotate('text', label = '颱風假:09-27~09-28', x = as.POSIXct(strptime('2016-09-29', format = '%Y-%m-%d')), y = Inf, vjust = 2, hjust = 1)
  }
  
# 當x軸是時間類型時，調整scale就要考慮到tz的設定，不然可能會呈現錯誤的時間，無法達到我們要的效果









#-----------------------------------------------#
# loop...
dList <- seq.Date(as.Date('2016-04-21'),as.Date('2016-09-06'), by = 'day')
wData <- NULL
for (i in 1:length(dList)){
  Sys.sleep(time = runif(n=1, min=1, max=5))
  
  myURL <- str_c('http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=466920&stname=%25E8%2587%25BA%25E5%258C%2597&datepicker=', dList[i])
  myHTML <- myURL %>% getURL() %>% htmlParse(., encoding = 'utf8', asText = TRUE)
  
  tmpData <- readHTMLTable(myHTML, header = TRUE) %$% MyTable %>% tbl_dt()
  tbNames <- tmpData[1, ] %>% unlist() %>% as.character()
  
  tmpData %<>% .[-1,]
  tmpData %<>% sapply(., as.character) %>% data.frame(stringsAsFactors = FALSE)
  tmpData %<>% sapply(., as.numeric) %>% tbl_dt() %>% arrange(V1)
  tmpData %<>% mutate(cdate = dList[i])
  
  wData %<>% bind_rows(tmpData)
}

wDataNames <- tbNames


mList <- seq.Date(as.Date('2016-04-01'),as.Date('2016-09-01'), by = 'month') %>% str_sub(1, 7)
wDayData <- NULL
for (i in 1:length(dList)){
  Sys.sleep(time = runif(n=1, min=1, max=5))
  
  myURL <- str_c('http://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=466920&stname=%25E8%2587%25BA%25E5%258C%2597&datepicker=', mList[i])
  myHTML <- myURL %>% getURL() %>% htmlParse(., encoding = 'utf8', asText = TRUE)
  
  tmpData <- readHTMLTable(myHTML, header = TRUE) %$% MyTable %>% tbl_dt()
  tbNames <- tmpData[1, ] %>% unlist() %>% as.character()
  
  tmpData %<>% .[-1,]
  tmpData %<>% sapply(., as.character) %>% data.frame(stringsAsFactors = FALSE)
  tmpData %<>% sapply(., as.numeric) %>% tbl_dt() %>% arrange(V1)
  tmpData %<>% mutate(cdate = mList[i])
  
  wDayData %<>% bind_rows(tmpData)
}

wDayDataNames <- tbNames
wDayData %<>% mutate(cdate2 = as.Date(str_c(cdate, V1, sep = '-')))


#-----------------------------------------------#
# 氣溫(℃)Temperature
# 降水時數(hr)PrecpHour
# 日照率(%)SunShineRate
weather <- wDayData %>% select(cdate2, V8, V23, V29)
names(weather) <- c('cdate','Temperature','PrecpHour','SunShineRate')
weather %<>% mutate(PrecpHour = (PrecpHour/24)*100)
weather %<>% filter(cdate >= as.Date('2016-04-21') & cdate <= as.Date('2016-09-06'))


tmp <- Lar02Day %>% filter(status == '進店人流') %>% group_by()
tmp %<>% left_join(weather, by = 'cdate', copy = TRUE)


tmp %>% select(cdate, cnt, SunShineRate) %>% gather(key = 'vType', value = 'value', -cdate) %>% {
  ggplot(data = ., aes(x = cdate, y = value)) + 
    geom_line() +
    facet_grid(vType ~ ., scales = 'free_y')
}








