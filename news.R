setwd("\\Users\\MOLAP\\Desktop\\DWBI")
#according to the report, two waves of research (November & December 2017 and March & April 2018) were conducted. Some of the respondents were asked face to face and some of them were surveyed online. It means that no dates should be excluded(like Christmas), because someone could potentially fill in the survey online on any date.


data1=as.data.frame(list(ID=1:61))

#This function will generate a uniform sample of dates from 
#within a designated start and end date:

generate.date=function(start.day,end.day,data){   
  size=dim(data)[1]    
  days=seq.Date(as.Date(start.day),as.Date(end.day),by="day")  
  #pick.day=runif(size,1,length(days))  
  #date=days[pick.day]  
}

#This will create a new column within your data frame called date:

data1$date=generate.date("2017-11-01","2017-12-31",data)

#and this will order your data frame by date:

data1=data1[order(data1$date),]
#the  dates for the first wave of the research are ready!
#now on to the second wave
data2=as.data.frame(list(ID=1:61))

#This function will generate a uniform sample of dates from 
#within a designated start and end date:

generate.date1=function(start.day,end.day,data2){   
  size=dim(data2)[1]    
  days=seq.Date(as.Date(start.day),as.Date(end.day),by="day")  
  #pick.day=runif(size,1,length(days))  
  #date=days[pick.day]  
}

#This will create a new column within your data frame called date:

data2$date=generate.date1("2018-03-01","2018-04-30",data2)

#and this will order your data frame by date:

data2=data2[order(data2$date),]

#https://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates

dates3 <- rbind(data1, data2)
rm(data1, data2)
dates3$Year = dates3$date
dates3$Year <- as.Date(dates3$Year)
dates3$Year <- format(as.Date(dates3$Year, format="%d/%m/%Y"),"%Y")
dates3$Month = dates3$date
dates3$Month <- format(as.Date(dates3$Month, format="%d/%m/%Y"),"%m")
dates3$DayofWeek = dates3$date
dates3$DayofWeek <- weekdays(as.Date(dates3$DayofWeek, format="%d/%m/%Y"))
x <- c("Wednesday","Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday")

dates3$WeekOfSurvey = dates3$DayofWeek
dates3$WeekOfSurvey <- cumsum(dates3$WeekOfSurvey == "Sunday") + (dates3$WeekOfSurvey[1] != "Sunday")
dates3$WeekOfSurvey <- c(1, head(dates3$WeekOfSurvey, dim(dates3)[1] - 1))

dates3 <- dates3[, c(2, 3, 4, 6, 5)]


news <- read.csv(url("http://static.ofcom.org.uk/static/models/news-consumption-2018-data.csv"), skip=1, header=TRUE, sep=",")
news[3025:3070] <- list(NULL) #removing crossbreak columns that we will not use 
news[3010:3018] <- list(NULL) #removing crossbreak columns we are not going to use
news[2970:3002] <- list(NULL) #removing questions about daily limitations of the respondents and some other generic questions
news[2961:2968] <- list(NULL) #removing information about the readers we will not use
news[2783:2959] <- list(NULL) #removing information that is not relevant (i.e. concerns other sources of news or newspapers that are not included in the research)
news[2778:2781] <- list(NULL) #removing info about newspapers not relevant to this research
news[2738:2769] <- list(NULL) #removing info about newspapers not relevant to this research
news[2735] <- NULL
news[2681:2724] <- list(NULL) #removing clolumns that have no connection to newspapers
news[2666:2678] <- list(NULL) #removing info about newspapers not relevant to this research
news[2579:2656] <- list(NULL) #removing info not relevant to the chosen newspapers
news[2515:2578] <- list(NULL) #removing info not relevant to the chosen newspapers
news[2510:2513] <- list(NULL) #removing info not relevant to our newspapers
news[2501] <- NULL
news[2486:2498] <-list(NULL) #removing unwanted newspapers
news[2443:2476] <- list(NULL) #removing info about TV and irrelevant newspapers
news[2441] <- NULL
news[2428:2439] <-list(NULL) #irrelevant info
news[2427] <-NULL
news[2425] <-NULL
news[2395:2423] <-list(NULL)
news[2393] <-NULL
news[2262:2391] <-list(NULL) #newspapers or news sources we are not interested in
news[2238:2245] <-list(NULL) #news source we are not interested in
news[1986:2165] <-list(NULL) #info about news sources we are not interested in
news[1954:1969] <-list(NULL)
news[1922:1945] <-list(NULL)
news[1685:1849] <-list(NULL)
news[1637:1672] <-list(NULL)
news[1633:1634] <-list(NULL)
news[1629:1631] <-list(NULL)
news[1614:1619] <-list(NULL)
news[1610:1611] <-list(NULL)
news[1606:1607] <-list(NULL)
news[1015:1596] <-list(NULL)
news[812:1002] <- list(NULL)
news[809] <-NULL
news[508:798] <-list(NULL)
news[503:506] <-list(NULL)
news[361:494] <-list(NULL)
news[357:360] <-list(NULL)
news[337:345] <-list(NULL)
news[333:335] <- list(NULL)
news[196:323] <-list(NULL)
news[188:193] <-list(NULL)
news[184:186] <-list(NULL)
news[83:174] <-list(NULL)
news[81] <- NULL
news[68:80] <-list(NULL)
news[58:66] <-list(NULL)
news[24:56] <-list(NULL)
news <- news[-c(4619), ] #the last row was empty! 
#now it's a bit easier to work with this dataset - I removed all the info that is not relevant.Now let's have a look at what we have. The dataset is still quite big, so maybe there is a better way to organise it?
#let's see what we need to keep for the readers dimention. There is a chance we can combine or remove some columns.
news$Crossbreak...Working <- as.character(news$Crossbreak...Working)
news$Crossbreak...Working[news$Crossbreak...Working == "0"] <- "Not working"
news$Crossbreak...Working[news$Crossbreak...Working == ""] <- "Not working"
news$Crossbreak...Working <- as.factor(news$Crossbreak...Working)
news[10:11] <-list(NULL)
news[379] <- NULL
news$Crossbreak...16.to.24 <- as.character(news$Crossbreak...16.to.24)
news$Crossbreak...25.to.34 <- as.character(news$Crossbreak...25.to.34)
news$Crossbreak...35.to.44 <- as.character(news$Crossbreak...35.to.44)
news$Crossbreak...45.to.54 <- as.character(news$Crossbreak...45.to.54)
news$Crossbreak...55.to.64 <- as.character(news$Crossbreak...55.to.64)
news$Crossbreak...65.to.74 <- as.character(news$Crossbreak...65.to.74)
news$Crossbreak...75.<-as.character(news$Crossbreak...75.)
news$Crossbreak...16.to.24[news$Crossbreak...16.to.24 == 0] <- NA
news$Crossbreak...16.to.24[is.na(news$Crossbreak...16.to.24)] <- news$Crossbreak...25.to.34[is.na(news$Crossbreak...16.to.24)]
news$Crossbreak...16.to.24[news$Crossbreak...16.to.24 == 0] <- NA
news$Crossbreak...16.to.24[is.na(news$Crossbreak...16.to.24)] <- news$Crossbreak...35.to.44[is.na(news$Crossbreak...16.to.24)]
news$Crossbreak...16.to.24[news$Crossbreak...16.to.24 == 0] <- NA
news$Crossbreak...16.to.24[is.na(news$Crossbreak...16.to.24)] <- news$Crossbreak...45.to.54[is.na(news$Crossbreak...16.to.24)]
news$Crossbreak...16.to.24[news$Crossbreak...16.to.24 == 0] <- NA
news$Crossbreak...16.to.24[is.na(news$Crossbreak...16.to.24)] <- news$Crossbreak...55.to.64[is.na(news$Crossbreak...16.to.24)]
news$Crossbreak...16.to.24[news$Crossbreak...16.to.24 == 0] <- NA
news$Crossbreak...16.to.24[is.na(news$Crossbreak...16.to.24)] <- news$Crossbreak...65.to.74[is.na(news$Crossbreak...16.to.24)]
news$Crossbreak...16.to.24[news$Crossbreak...16.to.24 == 0] <- NA
news$Crossbreak...16.to.24[is.na(news$Crossbreak...16.to.24)] <- news$Crossbreak...75.[is.na(news$Crossbreak...16.to.24)]
news$Crossbreak...16.to.24 <- as.character(news$Crossbreak...16.to.24)
news$Crossbreak...16.to.24[news$Crossbreak...16.to.24 == 0] <- NA
colnames(news)[colnames(news) == "Crossbreak...16.to.24"] <- "Age Group"
news[373:378] <- list(NULL)
colnames(news)[colnames(news) == "Crossbreak...Working"] <- "Work Status"
colnames(news)[colnames(news) == "X.A1..Gender"] <- "Gender"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Sun"] <- "Daily.News.TheSun"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Daily.Mail"] <- "Daily.News.TheDailyMail"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Daily.Express"] <- "Daily.News.TheDailyExpress"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Daily.Mirror"] <- "Daily.News.TheDailyMirror"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS.......The.Guardian"] <- "Daily.News.TheGuardian"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Times"] <- "Daily.News.TheTimes"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Daily.Telegraph"] <- "Daily.News.TheDTelegraph"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Daily.Record..SCOTLAND."] <- "Daily.News.TheDRecordSc"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Metro"] <- "Daily.News.TheMetro"
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Evening.Standard"] <- "Daily.News.EveStandard"
colnames(news)[colnames(news) == "X.D4a..Thinking.specifically.about.weekly.newspaper.s...which.of.the.following.do.you.use.for.news.nowadays....The.Sun.on.Sunday"] <- "Weekly.News.TheSunSunday"
colnames(news)[colnames(news) == "X.D4a..Thinking.specifically.about.weekly.newspaper.s...which.of.the.following.do.you.use.for.news.nowadays....The.Mail.on.Sunday"] <- "Weekly.News.TheMailSunday"
colnames(news)[colnames(news) == "X.D4a..Thinking.specifically.about.weekly.newspaper.s...which.of.the.following.do.you.use.for.news.nowadays....The.Daily.Star.on.Sunday"] <- "Weekly.News.TheDStarSunday"
colnames(news)[colnames(news) == "X.D4a..Thinking.specifically.about.weekly.newspaper.s...which.of.the.following.do.you.use.for.news.nowadays....The.Sunday.Express"] <- "Weekly.News.TheSundayExp"
colnames(news)[colnames(news) == "X.D4a..Thinking.specifically.about.weekly.newspaper.s...which.of.the.following.do.you.use.for.news.nowadays...The.Sunday.Mirror"] <- "Weekly.News.TheSundayMir"
colnames(news)[colnames(news) == "X.D4a..Thinking.specifically.about.weekly.newspaper.s...which.of.the.following.do.you.use.for.news.nowadays....The.Observer"] <- "Weekly.News.TheObserver"
news$Daily.News.TheSun <- as.character(news$Daily.News.TheSun)
news$Daily.News.TheSun[news$Daily.News.TheSun == "The Sun"] <- "Yes"
news$Daily.News.TheSun[news$Daily.News.TheSun == ""] <- "No"
news$Daily.News.TheSun[news$Daily.News.TheSun == " "] <- "No"
news$Daily.News.TheSun[news$Daily.News.TheSun == "0"] <- "No"
news$Daily.News.TheSun <- as.factor(news$Daily.News.TheSun)
colnames(news)[colnames(news) == "X.D3a..Thinking.specifically.about.daily.newspaper.s...which.of.the.following.do.you.use.for.news.NOWADAYS........The.Daily.Star"] <- "Daily.News.DStar"
news$Daily.News.TheGuardian <- as.character(news$Daily.News.TheGuardian)
news$Daily.News.TheGuardian[news$Daily.News.TheGuardian == "The Guardian"] <- "Yes"
news$Daily.News.TheGuardian[news$Daily.News.TheGuardian == ""] <- "No"
news$Daily.News.TheGuardian[news$Daily.News.TheGuardian == " "] <- "No"
news$Daily.News.TheGuardian[news$Daily.News.TheGuardian == "0"] <- "No"
news$Daily.News.TheGuardian <- as.factor(news$Daily.News.TheGuardian)
news$Daily.News.TheDailyMail <- as.character(news$Daily.News.TheDailyMail)
news$Daily.News.TheDailyMail[news$Daily.News.TheDailyMail == "The Daily Mail"] <- "Yes"
news$Daily.News.TheDailyMail[news$Daily.News.TheDailyMail  == ""] <- "No"
news$Daily.News.TheDailyMail[news$Daily.News.TheDailyMail  == " "] <- "No"
news$Daily.News.TheDailyMail[news$Daily.News.TheDailyMail  == "0"] <- "No"
news$Daily.News.TheDailyMail <- as.factor(news$Daily.News.TheDailyMail)
news$Daily.News.DStar <- as.character(news$Daily.News.DStar)
news$Daily.News.DStar[news$Daily.News.DStar == "The Daily Star"] <- "Yes"
news$Daily.News.DStar[news$Daily.News.DStar  == ""] <- "No"
news$Daily.News.DStar[news$Daily.News.DStar  == " "] <- "No"
news$Daily.News.DStar[news$Daily.News.DStar  == "0"] <- "No"
news$Daily.News.DStar <- as.factor(news$Daily.News.DStar)
news$Daily.News.TheMetro <- as.character(news$Daily.News.TheMetro)
news$Daily.News.TheMetro[news$Daily.News.TheMetro == "The Metro"] <- "Yes"
news$Daily.News.TheMetro[news$Daily.News.TheMetro  == ""] <- "No"
news$Daily.News.TheMetro[news$Daily.News.TheMetro  == " "] <- "No"
news$Daily.News.TheMetro[news$Daily.News.TheMetro  == "0"] <- "No"
news$Daily.News.TheMetro <- as.factor(news$Daily.News.TheMetro)
news$Daily.News.TheDailyExpress <- as.character(news$Daily.News.TheDailyExpress)
news$Daily.News.TheDailyExpress[news$Daily.News.TheDailyExpress == "The Daily Express"] <- "Yes"
news$Daily.News.TheDailyExpress[news$Daily.News.TheDailyExpress  == ""] <- "No"
news$Daily.News.TheDailyExpress[news$Daily.News.TheDailyExpress  == " "] <- "No"
news$Daily.News.TheDailyExpress[news$Daily.News.TheDailyExpress  == "0"] <- "No"
news$Daily.News.TheDailyExpress <- as.factor(news$Daily.News.TheDailyExpress)
news$Daily.News.TheDailyMirror <- as.character(news$Daily.News.TheDailyMirror)
news$Daily.News.TheDailyMirror[news$Daily.News.TheDailyMirror == "The Daily Mirror"] <- "Yes"
news$Daily.News.TheDailyMirror[news$Daily.News.TheDailyMirror  == ""] <- "No"
news$Daily.News.TheDailyMirror[news$Daily.News.TheDailyMirror  == " "] <- "No"
news$Daily.News.TheDailyMirror[news$Daily.News.TheDailyMirror  == "0"] <- "No"
news$Daily.News.TheDailyMirror <- as.factor(news$Daily.News.TheDailyMirror)
news$Daily.News.TheTimes <- as.character(news$Daily.News.TheTimes)
news$Daily.News.TheTimes[news$Daily.News.TheTimes == "The Times"] <- "Yes"
news$Daily.News.TheTimes[news$Daily.News.TheTimes  == ""] <- "No"
news$Daily.News.TheTimes[news$Daily.News.TheTimes  == " "] <- "No"
news$Daily.News.TheTimes[news$Daily.News.TheTimes  == "0"] <- "No"
news$Daily.News.TheTimes <- as.factor(news$Daily.News.TheTimes)
news$Daily.News.TheDTelegraph <- as.character(news$Daily.News.TheDTelegraph)
news$Daily.News.TheDTelegraph[news$Daily.News.TheDTelegraph == "The Daily Telegraph"] <- "Yes"
news$Daily.News.TheDTelegraph[news$Daily.News.TheDTelegraph  == ""] <- "No"
news$Daily.News.TheDTelegraph[news$Daily.News.TheDTelegraph  == " "] <- "No"
news$Daily.News.TheDTelegraph[news$Daily.News.TheDTelegraph  == "0"] <- "No"
news$Daily.News.TheDTelegraph <- as.factor(news$Daily.News.TheDTelegraph)
news$Daily.News.TheDRecordSc <- as.character(news$Daily.News.TheDRecordSc)
news$Daily.News.TheDRecordSc[news$Daily.News.TheDRecordSc == "The Daily Record (SCOTLAND)"] <- "Yes"
news$Daily.News.TheDRecordSc[news$Daily.News.TheDRecordSc  == ""] <- "No"
news$Daily.News.TheDRecordSc[news$Daily.News.TheDRecordSc  == " "] <- "No"
news$Daily.News.TheDRecordSc[news$Daily.News.TheDRecordSc  == "0"] <- "No"
news$Daily.News.TheDRecordSc <- as.factor(news$Daily.News.TheDRecordSc)
news$Daily.News.EveStandard <- as.character(news$Daily.News.EveStandard)
news$Daily.News.EveStandard[news$Daily.News.EveStandard == "The Evening Standard"] <- "Yes"
news$Daily.News.EveStandard[news$Daily.News.EveStandard  == ""] <- "No"
news$Daily.News.EveStandard[news$Daily.News.EveStandard  == " "] <- "No"
news$Daily.News.EveStandard[news$Daily.News.EveStandard  == "0"] <- "No"
news$Daily.News.EveStandard <- as.factor(news$Daily.News.EveStandard)
news$Weekly.News.TheSunSunday <- as.character(news$Weekly.News.TheSunSunday)
news$Weekly.News.TheSunSunday[news$Weekly.News.TheSunSunday == "The Sun on Sunday"] <- "Yes"
news$Weekly.News.TheSunSunday[news$Weekly.News.TheSunSunday  == ""] <- "No"
news$Weekly.News.TheSunSunday[news$Weekly.News.TheSunSunday  == " "] <- "No"
news$Weekly.News.TheSunSunday[news$Weekly.News.TheSunSunday  == "0"] <- "No"
news$Weekly.News.TheSunSunday <- as.factor(news$Weekly.News.TheSunSunday)
news$Weekly.News.TheMailSunday <- as.character(news$Weekly.News.TheMailSunday)
news$Weekly.News.TheMailSunday[news$Weekly.News.TheMailSunday == "The Mail on Sunday"] <- "Yes"
news$Weekly.News.TheMailSunday[news$Weekly.News.TheMailSunday  == ""] <- "No"
news$Weekly.News.TheMailSunday[news$Weekly.News.TheMailSunday  == " "] <- "No"
news$Weekly.News.TheMailSunday[news$Weekly.News.TheMailSunday  == "0"] <- "No"
news$Weekly.News.TheMailSunday <- as.factor(news$Weekly.News.TheMailSunday)
news$Weekly.News.TheDStarSunday <- as.character(news$Weekly.News.TheDStarSunday)
news$Weekly.News.TheDStarSunday[news$Weekly.News.TheDStarSunday == "The Daily Star on Sunday"] <- "Yes"
news$Weekly.News.TheDStarSunday[news$Weekly.News.TheDStarSunday  == ""] <- "No"
news$Weekly.News.TheDStarSunday[news$Weekly.News.TheDStarSunday  == " "] <- "No"
news$Weekly.News.TheDStarSunday[news$Weekly.News.TheDStarSunday  == "0"] <- "No"
news$Weekly.News.TheDStarSunday <- as.factor(news$Weekly.News.TheDStarSunday)
news$Weekly.News.TheSundayExp <- as.character(news$Weekly.News.TheSundayExp)
news$Weekly.News.TheSundayExp[news$Weekly.News.TheSundayExp == "The Sunday Express"] <- "Yes"
news$Weekly.News.TheSundayExp[news$Weekly.News.TheSundayExp  == ""] <- "No"
news$Weekly.News.TheSundayExp[news$Weekly.News.TheSundayExp  == " "] <- "No"
news$Weekly.News.TheSundayExp[news$Weekly.News.TheSundayExp  == "0"] <- "No"
news$Weekly.News.TheSundayExp <- as.factor(news$Weekly.News.TheSundayExp)
news$Weekly.News.TheSundayMir <- as.character(news$Weekly.News.TheSundayMir)
news$Weekly.News.TheSundayMir[news$Weekly.News.TheSundayMir == "The Sunday Mirror"] <- "Yes"
news$Weekly.News.TheSundayMir[news$Weekly.News.TheSundayMir  == ""] <- "No"
news$Weekly.News.TheSundayMir[news$Weekly.News.TheSundayMir  == " "] <- "No"
news$Weekly.News.TheSundayMir[news$Weekly.News.TheSundayMir  == "0"] <- "No"
news$Weekly.News.TheSundayMir <- as.factor(news$Weekly.News.TheSundayMir)
news$Weekly.News.TheObserver <- as.character(news$Weekly.News.TheObserver)
news$Weekly.News.TheObserver[news$Weekly.News.TheObserver == "The Observer"] <- "Yes"
news$Weekly.News.TheObserver[news$Weekly.News.TheObserver  == ""] <- "No"
news$Weekly.News.TheObserver[news$Weekly.News.TheObserver  == " "] <- "No"
news$Weekly.News.TheObserver[news$Weekly.News.TheObserver  == "0"] <- "No"
news$Weekly.News.TheObserver <- as.factor(news$Weekly.News.TheObserver)
news$Crossbreak...England <-as.character(news$Crossbreak...England)
news$Crossbreak...Scotland <-as.character(news$Crossbreak...Scotland)
news$Crossbreak...Wales <-as.character(news$Crossbreak...Wales)
news$Crossbreak...Northern.Ireland <-as.character(news$Crossbreak...Northern.Ireland)
news$Crossbreak...England[news$Crossbreak...England == 0] <- NA
news$Crossbreak...England[is.na(news$Crossbreak...England)] <- news$Crossbreak...Scotland[is.na(news$Crossbreak...England)]
news$Crossbreak...England[news$Crossbreak...England == 0] <- NA
news$Crossbreak...England[is.na(news$Crossbreak...England)] <- news$Crossbreak...Wales[is.na(news$Crossbreak...England)]
news$Crossbreak...England[news$Crossbreak...England == 0] <- NA
news$Crossbreak...England[is.na(news$Crossbreak...England)] <- news$Crossbreak...Northern.Ireland[is.na(news$Crossbreak...England)]
news$Crossbreak...England <-as.factor(news$Crossbreak...England)
news[375:377] <- list(NULL)
news[318:337] <- list(NULL)
news[91:311] <- list(NULL)
news[79:128] <- list(NULL) 
news[58:66] <- list(NULL)
news[37:49] <- list(NULL)
news[10:24] <-list(NULL)
colnames(news)[colnames(news) == "X.D4a..Thinking.specifically.about.weekly.newspaper.s...which.of.the.following.do.you.use.for.news.nowadays....The.Sunday.Times"] <- "Weekly.News.SundayTimes"
colnames(news)[colnames(news) == "X.D4a..Thinking.specifically.about.weekly.newspaper.s...which.of.the.following.do.you.use.for.news.nowadays....The.Sunday.Telegraph"] <- "Weekly.News.SundayTelegraph"
colnames(news)[colnames(news) == "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays...The.Sun.website.or.app"] <- "DigitalTheSun"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Daily.Mail.website.or.app"] <- "DigitalDailyMail"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Daily.Star.website.or.app"] <- "DigitalDailyStar"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Evening.Standard.website.or.app"] <- "DigitalEveStandard"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Metro.website.or.app"] <- "DigitalMetro"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Telegraph.website.or.app"] <- "DigitalTelegraph"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Times.Sunday.Times.website.or.app"] <- "DigitalTimes"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Guardian.Observer.website.or.app"] <- "DigitalGuardian"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Daily.Mirror.website.or.app"] <- "DigitalMirror"
colnames(news)[colnames(news) ==  "X.D8a..Thinking.specifically.about.other.internet.sources..including.apps...on.any.device..which.of.the.following.do.you.use.for.news.nowadays....The.Daily.Express.website.or.app"] <- "DigitalExpress"
news[36:37] <- list(NULL)
news[4:5] <- list(NULL)
news[14] <- NULL
news$Weekly.News.SundayTimes <- as.character(news$Weekly.News.SundayTimes)
news$Weekly.News.SundayTimes[news$Weekly.News.SundayTimes == "The Sunday Times"] <- "Yes"
news$Weekly.News.SundayTimes[news$Weekly.News.SundayTimes  == ""] <- "No"
news$Weekly.News.SundayTimes[news$Weekly.News.SundayTimes  == " "] <- "No"
news$Weekly.News.SundayTimes[news$Weekly.News.SundayTimes  == "0"] <- "No"
news$Weekly.News.SundayTimes <- as.factor(news$Weekly.News.SundayTimes)
news$Weekly.News.SundayTelegraph <- as.character(news$Weekly.News.SundayTelegraph)
news$Weekly.News.SundayTelegraph[news$Weekly.News.SundayTelegraph == "The Sunday Telegraph"] <- "Yes"
news$Weekly.News.SundayTelegraph[news$Weekly.News.SundayTelegraph  == ""] <- "No"
news$Weekly.News.SundayTelegraph[news$Weekly.News.SundayTelegraph  == " "] <- "No"
news$Weekly.News.SundayTelegraph[news$Weekly.News.SundayTelegraph  == "0"] <- "No"
news$Weekly.News.SundayTelegraph <- as.factor(news$Weekly.News.SundayTelegraph)
news$DigitalTheSun <- as.character(news$DigitalTheSun)
news$DigitalTheSun[news$DigitalTheSun == "The Sun website or app"] <- "Yes"
news$DigitalTheSun[news$DigitalTheSun  == ""] <- "No"
news$DigitalTheSun[news$DigitalTheSun  == " "] <- "No"
news$DigitalTheSun[news$DigitalTheSun  == "0"] <- "No"
news$DigitalTheSun <- as.factor(news$DigitalTheSun)
news$DigitalDailyMail <- as.character(news$DigitalDailyMail)
news$DigitalDailyMail[news$DigitalDailyMail == "The Daily Mail website or app"] <- "Yes"
news$DigitalDailyMail[news$DigitalDailyMail  == ""] <- "No"
news$DigitalDailyMail[news$DigitalDailyMail  == " "] <- "No"
news$DigitalDailyMail[news$DigitalDailyMail  == "0"] <- "No"
news$DigitalDailyMail <- as.factor(news$DigitalDailyMail)
news$DigitalDailyStar <- as.character(news$DigitalDailyStar)
news$DigitalDailyStar[news$DigitalDailyStar == "The Daily Star website or app"] <- "Yes"
news$DigitalDailyStar[news$DigitalDailyStar  == ""] <- "No"
news$DigitalDailyStar[news$DigitalDailyStar  == " "] <- "No"
news$DigitalDailyStar[news$DigitalDailyStar  == "0"] <- "No"
news$DigitalDailyStar <- as.factor(news$DigitalDailyStar)
news$DigitalExpress <- as.character(news$DigitalExpress)
news$DigitalExpress[news$DigitalExpress == "The Daily Express website or app"] <- "Yes"
news$DigitalExpress[news$DigitalExpress  == ""] <- "No"
news$DigitalExpress[news$DigitalExpress  == " "] <- "No"
news$DigitalExpress[news$DigitalExpress  == "0"] <- "No"
news$DigitalExpress <- as.factor(news$DigitalExpress)
news$DigitalMirror <- as.character(news$DigitalMirror)
news$DigitalMirror[news$DigitalMirror == "The Daily Mirror website or app"] <- "Yes"
news$DigitalMirror[news$DigitalMirror  == ""] <- "No"
news$DigitalMirror[news$DigitalMirror  == " "] <- "No"
news$DigitalMirror[news$DigitalMirror  == "0"] <- "No"
news$DigitalMirror <- as.factor(news$DigitalMirror)
news$DigitalGuardian <- as.character(news$DigitalGuardian)
news$DigitalGuardian[news$DigitalGuardian == "The Guardian/Observer website or app"] <- "Yes"
news$DigitalGuardian[news$DigitalGuardian  == ""] <- "No"
news$DigitalGuardian[news$DigitalGuardian  == " "] <- "No"
news$DigitalGuardian[news$DigitalGuardian  == "0"] <- "No"
news$DigitalGuardian <- as.factor(news$DigitalGuardian)
news$DigitalTimes <- as.character(news$DigitalTimes)
news$DigitalTimes[news$DigitalTimes == "The Times/Sunday Times website or app"] <- "Yes"
news$DigitalTimes[news$DigitalTimes  == ""] <- "No"
news$DigitalTimes[news$DigitalTimes  == " "] <- "No"
news$DigitalTimes[news$DigitalTimes  == "0"] <- "No"
news$DigitalTimes <- as.factor(news$DigitalTimes)
news$DigitalTelegraph <- as.character(news$DigitalTelegraph)
news$DigitalTelegraph[news$DigitalTelegraph == "The Telegraph website or app"] <- "Yes"
news$DigitalTelegraph[news$DigitalTelegraph  == ""] <- "No"
news$DigitalTelegraph[news$DigitalTelegraph  == " "] <- "No"
news$DigitalTelegraph[news$DigitalTelegraph  == "0"] <- "No"
news$DigitalTelegraph <- as.factor(news$DigitalTelegraph)
news$DigitalMetro <- as.character(news$DigitalMetro)
news$DigitalMetro[news$DigitalMetro == "The Metro website or app"] <- "Yes"
news$DigitalMetro[news$DigitalMetro  == ""] <- "No"
news$DigitalMetro[news$DigitalMetro  == " "] <- "No"
news$DigitalMetro[news$DigitalMetro  == "0"] <- "No"
news$DigitalMetro <- as.factor(news$DigitalMetro)
news$DigitalEveStandard <- as.character(news$DigitalEveStandard)
news$DigitalEveStandard[news$DigitalEveStandard == "The Evening Standard website or app"] <- "Yes"
news$DigitalEveStandard[news$DigitalEveStandard  == ""] <- "No"
news$DigitalEveStandard[news$DigitalEveStandard  == " "] <- "No"
news$DigitalEveStandard[news$DigitalEveStandard  == "0"] <- "No"
news$DigitalEveStandard <- as.factor(news$DigitalEveStandard)
news[4618, 41] <- "England" #we can do it because we know the BBC region of this guy and we know where this BBC region is 
news[4618, 39] <- "25 to 34" # we can do it because we know the exact age of this guy
colnames(news)[colnames(news) ==  "Age Group"] <- "AgeGroup"
news$AgeGroup <- as.factor(news$AgeGroup)
news[3] <- NULL
news[4] <-NULL 
colnames(news)[colnames(news) ==  "Serial.Number"] <- "ReaderID"
colnames(news)[colnames(news) ==  "Work Status"] <- "WorkStatus"
colnames(news)[colnames(news) ==  "Crossbreak...England"] <- "Region"
news[35] <- NULL
news[5] <-NULL
news$X.A7..Urbanity <- as.character(news$X.A7..Urbanity)
news$X.A7..Urbanity[news$X.A7..Urbanity == "REMOTE RURAL - Less than 10 miles from a large settlement"] <- "Remote Rural"
news$X.A7..Urbanity[news$X.A7..Urbanity == "RURAL - Less than 10 miles of a large settlement"] <- "Rural"
news$X.A7..Urbanity[news$X.A7..Urbanity == "Small town Less than 10 miles of a large settlement"] <- "Small Town"
news$X.A7..Urbanity[news$X.A7..Urbanity == "Small town more than 10 miles from a large settlement"] <- "Remote Small Town"
news$X.A7..Urbanity <- as.factor(news$X.A7..Urbanity)
news[34] <- NULL
colnames(news)[colnames(news) ==  "X.A7..Urbanity"] <- "Urbanity"
colnames(news)[colnames(news) ==  "X.A4..Social.grade"] <- "SocialGrade"
news$SocialGrade <- as.character(news$SocialGrade)
news$SocialGrade[news$SocialGrade == ""] <- "Unknown"
news$SocialGrade[news$SocialGrade == " "] <- "Unknown"
news$SocialGrade[news$SocialGrade == "(Refused)"] <- "Unknown"
news$SocialGrade <- as.factor(news$SocialGrade)
news$DigitalTheSunSunday = news$DigitalTheSun
news$DigitalMailonSunday = news$DigitalDailyMail
news$DigitalDailyStarSunday = news$DigitalDailyStar
news$DigitalObserver = news$DigitalGuardian
news$DigitalExpressSunday = news$DigitalExpress
news$DigitalMirrorSunday = news$DigitalMirror
news$DigitalTimesSunday = news$DigitalTimes
news$DigitalTelegraphSun = news$DigitalTelegraph
news[13] <- NULL
#now let's add the date
news$Date <- rep(dates3$date, len = 4618)
news$Date <- format(as.Date(news$Date, format="%d/%m/%Y"))
#install.packages("reshape")
library(reshape)
newsmelt <- melt(news, id=c("ReaderID", "Gender", "SocialGrade", "Urbanity", "AgeGroup", "WorkStatus", "Region", "Date"))
newsmelt$Brand = newsmelt$variable
newsmelt$Brand <- as.character(newsmelt$Brand)  
newsmelt$Brand[newsmelt$Brand == "Daily.News.TheSun"] <- "1"
newsmelt$Brand[newsmelt$Brand == "Weekly.News.TheSunSunday"] <- "1"
newsmelt$Brand[newsmelt$Brand == "DigitalTheSun"] <- "1"
newsmelt$Brand[newsmelt$Brand == "DigitalTheSunSunday"] <- "1"
newsmelt$Brand[newsmelt$Brand == "Daily.News.TheDailyMail"] <- "2"
newsmelt$Brand[newsmelt$Brand == "Weekly.News.TheMailSunday"] <- "2"
newsmelt$Brand[newsmelt$Brand == "DigitalDailyMail"] <- "2"
newsmelt$Brand[newsmelt$Brand == "DigitalMailonSunday"] <- "2"
newsmelt$Brand[newsmelt$Brand == "DigitalTelegraph"] <- "3"
newsmelt$Brand[newsmelt$Brand == "Daily.News.TheDTelegraph"] <- "3"
newsmelt$Brand[newsmelt$Brand == "Weekly.News.SundayTelegraph"] <- "3"
newsmelt$Brand[newsmelt$Brand == "DigitalTelegraphSun"] <- "3"
newsmelt$Brand[newsmelt$Brand == "Daily.News.TheGuardian"] <- "4"
newsmelt$Brand[newsmelt$Brand == "Weekly.News.TheObserver"] <- "4"
newsmelt$Brand[newsmelt$Brand == "DigitalObserver"] <- "4"
newsmelt$Brand[newsmelt$Brand == "DigitalGuardian"] <- "4"
newsmelt$Brand[newsmelt$Brand == "Daily.News.TheDailyMirror"] <- "5"
newsmelt$Brand[newsmelt$Brand == "Weekly.News.TheSundayMir"] <- "5"
newsmelt$Brand[newsmelt$Brand == "DigitalMirrorSunday"] <- "5"
newsmelt$Brand[newsmelt$Brand == "DigitalMirror"] <- "5"
newsmelt$Brand[newsmelt$Brand == "Daily.News.TheMetro"] <- "6"
newsmelt$Brand[newsmelt$Brand == "DigitalMetro"] <- "6"
newsmelt$Brand[newsmelt$Brand == "Daily.News.TheDailyExpress"] <- "7"
newsmelt$Brand[newsmelt$Brand == "Weekly.News.TheSundayExp"] <- "7"
newsmelt$Brand[newsmelt$Brand == "DigitalExpress"] <- "7"
newsmelt$Brand[newsmelt$Brand == "DigitalExpressSunday"] <- "7"
newsmelt$Brand[newsmelt$Brand == "Daily.News.EveStandard"] <- "8"
newsmelt$Brand[newsmelt$Brand == "DigitalEveStandard"] <- "8"
newsmelt$Brand[newsmelt$Brand == "DigitalTimes"] <- "9"
newsmelt$Brand[newsmelt$Brand == "DigitalTimesSunday"] <- "9"
newsmelt$Brand[newsmelt$Brand == "Daily.News.TheTimes"] <- "9"
newsmelt$Brand[newsmelt$Brand == "Weekly.News.SundayTimes"] <- "9"
newsmelt$Brand[newsmelt$Brand == "Daily.News.DStar"] <- "10"
newsmelt$Brand[newsmelt$Brand == "Weekly.News.TheDStarSunday"] <- "10"
newsmelt$Brand[newsmelt$Brand == "DigitalDailyStar"] <- "10"
newsmelt$Brand[newsmelt$Brand == "DigitalDailyStarSunday"] <- "10"
newsmelt$Brand <-as.factor(newsmelt$Brand)
newsmelt$Format = newsmelt$variable
newsmelt$Format <- as.character(newsmelt$Format)
newsmelt$Format[newsmelt$Format == "DigitalDailyMail"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalDailyStar"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalDailyStarSunday"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalEveStandard"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalObserver"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalTelegraph"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalExpress"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalExpressSunday"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalGuardian"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalMailonSunday"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalMetro"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalMirror"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalMirrorSunday"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalTelegraphSun"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalTheSun"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalTheSunSunday"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalTimes"] <- "Digital"
newsmelt$Format[newsmelt$Format == "DigitalTimesSunday"] <- "Digital"
newsmelt$Format[newsmelt$Format == "Daily.News.DStar"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.EveStandard"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.TheDailyExpress"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.TheDailyMail"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.TheDailyMirror"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.TheDTelegraph"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.TheGuardian"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.TheMetro"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.TheSun"] <- "Print"
newsmelt$Format[newsmelt$Format == "Daily.News.TheTimes"] <- "Print"
newsmelt$Format[newsmelt$Format == "Weekly.News.SundayTelegraph"] <- "Print"
newsmelt$Format[newsmelt$Format == "Weekly.News.SundayTimes"] <- "Print"
newsmelt$Format[newsmelt$Format == "Weekly.News.TheDStarSunday"] <- "Print"
newsmelt$Format[newsmelt$Format == "Weekly.News.TheMailSunday"] <- "Print"
newsmelt$Format[newsmelt$Format == "Weekly.News.TheObserver"] <- "Print"
newsmelt$Format[newsmelt$Format == "Weekly.News.TheSundayExp"] <- "Print"
newsmelt$Format[newsmelt$Format == "Weekly.News.TheSundayMir"] <- "Print"
newsmelt$Format[newsmelt$Format == "Weekly.News.TheSunSunday"] <- "Print"
newsmelt$Format <- as.factor(newsmelt$Format)
newsmelt$NewspaperID = newsmelt$variable
newsmelt$NewspaperID <-as.character(newsmelt$NewspaperID)
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.TheSun"] <- "1"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalTheSun"] <- "1"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Weekly.News.TheSunSunday"] <- "2"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalTheSunSunday"] <- "2"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.TheDailyMail"] <- "3"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalDailyMail"] <- "3"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Weekly.News.TheMailSunday"] <- "4" 
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalMailonSunday"] <- "4"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.TheDTelegraph"] <- "5"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalTelegraph"] <- "5"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Weekly.News.SundayTelegraph"] <- "6"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalTelegraphSun"] <- "6"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.TheGuardian"] <- "7"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalGuardian"] <- "7"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Weekly.News.TheObserver"] <- "8"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalObserver"] <- "8"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.TheDailyMirror"] <- "9"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalMirror"] <- "9"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Weekly.News.TheSundayMir"] <- "10"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalMirrorSunday"] <- "10"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.TheMetro"] <- "11"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalMetro"] <- "11"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.TheDailyExpress"] <- "12"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalExpress"] <- "12"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Weekly.News.TheSundayExp"] <- "13"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalExpressSunday"] <- "13"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.EveStandard"] <- "14"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalEveStandard"] <- "14"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.TheTimes"] <- "15"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalTimes"] <- "15"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Weekly.News.SundayTimes"] <- "16"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalTimesSunday"] <- "16"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Daily.News.DStar"] <- "17"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalDailyStar"] <- "17"
newsmelt$NewspaperID[newsmelt$NewspaperID == "Weekly.News.TheDStarSunday"] <- "18"
newsmelt$NewspaperID[newsmelt$NewspaperID == "DigitalDailyStarSunday"] <- "18"
newsmelt <- newsmelt[!is.na(newsmelt$AgeGroup), ]
newsmelt[9] <- NULL #deleting column variable as we are not going to use it anymore
newsmelt[2:7] <- list(NULL) #deleting info about readers as it will go to a separate table
colnames(newsmelt)[colnames(newsmelt) == "value"] <- "Yes/No"
newsmelt$`Yes/No` <- as.character(newsmelt$`Yes/No`)
newsmelt$`Yes/No`[newsmelt$`Yes/No` == "Yes"] <- "1"
newsmelt$`Yes/No`[newsmelt$`Yes/No` == "No"] <- "0"
newsmelt$`Yes/No` <- as.factor(newsmelt$`Yes/No`)
#newsmelt[2] <- NULL
#install.packages("dplyr")
library(dplyr)
newsmelt <- newsmelt %>% arrange(ReaderID)
newsmelt$Observation <- 1:nrow(newsmelt) 
facttable <- newsmelt[, c(7, 1, 4, 6, 5, 3, 2)]
facttable$NewspaperID <- as.factor(facttable$NewspaperID)
readers <- news[, c(1, 2, 3, 4, 33, 34, 35)]
readers$Gender <- as.character(readers$Gender)
readers$Gender[readers$Gender == " "] <- "Other"
readers$Gender[readers$Gender == ""] <- "Other"
readers$Gender <- as.factor(readers$Gender)
readers <- readers[!is.na(readers$AgeGroup), ]
write.csv(readers, file = "readersinfo.csv", row.names = FALSE)
write.csv(facttable, file = "observationstable.csv", row.names = FALSE)
write.csv(dates3, file="dates1.csv", row.names = FALSE)
