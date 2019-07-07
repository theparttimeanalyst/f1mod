#### packages #############################################################

library(tidyverse)

library(xgboost)

######################## fucntion ###############################################

prac1_scrape <- function(year, raceno, race) {
  
  Sys.sleep(3)
  
  cat(".")
  
  url <- glue("https://www.formula1.com/en/results.html/{year}/races/{raceno}/{race}/practice-1.html")
  
  dat2 <-   read_html(url) %>%
    html_nodes("body > div.site-wrapper > main > article > div > div.ResultArchiveContainer > div.resultsarchive-wrapper > div.resultsarchive-content.group > div.resultsarchive-col-right > table") %>%
    html_table() %>%
    flatten_df() 
  
  colnames(dat2)[1] <- "col1"
  colnames(dat2)[9] <- "col2"
  
  dat4 <- dat2 %>% separate(Time, c("min", "sec"), ":")
  
  dat4$min <- as.numeric(dat4$min)
  dat4$sec <- as.numeric(dat4$sec)
  
  dat5 <- dat4 %>% mutate(time = min * 60 + sec) %>%
    select(Pos, No, Car, time, Laps)
  
  
  first <- dat5$time[[1]]
  
  
  dat6 <- dat5 %>% mutate(delta = time- first) %>%
    mutate(gp = race) %>%
    mutate(race1 = raceno)
  
  colnames(dat6)[1] <- "prac1pos"
  colnames(dat6)[4] <- "pract1t"
  colnames(dat6)[5] <- "pract1l"
  colnames(dat6)[6] <- "pract1d"
  
  
  
  return(dat6)
  
}



prac2_scrape <- function(year, raceno, race) {
  
  Sys.sleep(3)
  
  cat(".")
  
  url <- glue("https://www.formula1.com/en/results.html/{year}/races/{raceno}/{race}/practice-2.html")
  
  dat2 <-   read_html(url) %>%
    html_nodes("body > div.site-wrapper > main > article > div > div.ResultArchiveContainer > div.resultsarchive-wrapper > div.resultsarchive-content.group > div.resultsarchive-col-right > table") %>%
    html_table() %>%
    flatten_df() 
  
  colnames(dat2)[1] <- "col1"
  colnames(dat2)[9] <- "col2"
  
  dat4 <- dat2 %>% separate(Time, c("min", "sec"), ":")
  
  dat4$min <- as.numeric(dat4$min)
  dat4$sec <- as.numeric(dat4$sec)
  
  dat5 <- dat4 %>% mutate(time = min * 60 + sec) %>%
    select(Pos, No, Car, time, Laps)
  
  
  first <- dat5$time[[1]]
  
  
  dat6 <- dat5 %>% mutate(delta = time- first) %>%
    mutate(gp = race) %>%
    mutate(race1 = raceno)
  
  colnames(dat6)[1] <- "prac2pos"
  colnames(dat6)[4] <- "pract2t"
  colnames(dat6)[5] <- "pract2l"
  colnames(dat6)[6] <- "pract2d"
  
  
  
  return(dat6)
  
}

prac3_scrape <- function(year, raceno, race) {
  
  Sys.sleep(3)
  
  cat(".")
  
  url <- glue("https://www.formula1.com/en/results.html/{year}/races/{raceno}/{race}/practice-3.html")
  
  dat2 <-   read_html(url) %>%
    html_nodes("body > div.site-wrapper > main > article > div > div.ResultArchiveContainer > div.resultsarchive-wrapper > div.resultsarchive-content.group > div.resultsarchive-col-right > table") %>%
    html_table() %>%
    flatten_df() 
  
  colnames(dat2)[1] <- "col1"
  colnames(dat2)[9] <- "col2"
  
  dat4 <- dat2 %>% separate(Time, c("min", "sec"), ":")
  
  dat4$min <- as.numeric(dat4$min)
  dat4$sec <- as.numeric(dat4$sec)
  
  dat5 <- dat4 %>% mutate(time = min * 60 + sec) %>%
    select(Pos, No, Car, time, Laps)
  
  
  first <- dat5$time[[1]]
  
  
  dat6 <- dat5 %>% mutate(delta = time- first) %>%
    mutate(gp = race) %>%
    mutate(race1 = raceno)
  
  colnames(dat6)[1] <- "prac2pos"
  colnames(dat6)[4] <- "pract2t"
  colnames(dat6)[5] <- "pract2l"
  colnames(dat6)[6] <- "pract2d"
  
  
  
  return(dat6)
  
}


qual_scrape <- function(year, raceno, race) {
  
  Sys.sleep(3)
  
  cat(".")
  
  url <- glue("https://www.formula1.com/en/results.html/{year}/races/{raceno}/{race}/qualifying.html")
  
  dat2 <-   read_html(url) %>%
    html_nodes("body > div.site-wrapper > main > article > div > div.ResultArchiveContainer > div.resultsarchive-wrapper > div.resultsarchive-content.group > div.resultsarchive-col-right > table") %>%
    html_table() %>%
    flatten_df() 
  
  colnames(dat2)[1] <- "col1"
  
  dat3 <- dat2 %>% select(Pos, No, Car, Q1, Q2, Q3) %>%
    gather("qual", "time", -c(Pos, No, Car)) %>%
    separate(time, c("min", "sec"), ":")
  
  
  dat3$min <- as.numeric(dat3$min)
  dat3$sec <- as.numeric(dat3$sec)
  
  
  dat5 <- dat3 %>% mutate(time = min * 60 + sec) %>%
    filter(!is.na(time)) %>%
    group_by(No) %>%
    filter(time == min(time)) %>%
    select(No, time) %>%
    mutate(race1 = raceno)
  
  
  colnames(dat5)[2] <- "qtime"
  
  return(dat5)
  
}


predprep <- function(x,y,t) {
  
  dat1 <- x %>% left_join(y, by= "No") %>%
    separate(Car.x, c("n1", "n2"), sep = " ") %>%
    mutate(Team = if_else(n1 == "Mercedes", "Mercedes", 
                          if_else(n1 == "Red", "Red Bull", 
                                  if_else(n1 == "Ferrari", "Ferrari", 
                                          if_else(n1 == "Haas", "Haas", 
                                                  if_else(n1 == "McLaren", "McLaren",
                                                          if_else(n1 == "Renault", "Renault", 
                                                                  if_else(n1 == "Scuderia", "Toro Rosso", 
                                                                          if_else(n1 == "Williams", "Williams",
                                                                                  if_else(n1 == "Racing", "Racing Point",
                                                                                          if_else(n1 == "Alfa", "Alfa Romeo", "NA"))))))))))) %>%
    select(prac1pos, No, prac2pos,pract1t, pract1l , pract1d, pract2t, pract2l , pract2d , Team) %>%
    mutate(Length = 4318) %>%
    filter(!is.na(pract1t)) %>% 
    filter(!is.na(pract2t)) 
  
  
  dat2 <- trackmat1[trackmat1[, t] == 1,]
  
  d3 <- dat2[1:20,]
  
  teamm <- dat1 %>% select(Team)
  
  teammatm <- model.matrix(~Team-1, teamm)
  
  f12 <- dat1 %>% select(-Team, -No)
  
  
  f1all <- cbind(f12, d3)
  f1all2 <- cbind(f1all, teammatm)
  
  f1all3 <- f1all2[1:20,]
  
  return(f1all3)
  
}

##### getitng data ###################################################

prac118 <- bind_rows(prac1_scrape(2018,979, "australia"), prac1_scrape(2018,980, "bahrain"), prac1_scrape(2018,981, "china"), 
                     prac1_scrape(2018,982, "azerbaijan"), prac1_scrape(2018,983, "spain"), prac1_scrape(2018,984, "monaco"),
                     prac1_scrape(2018,985, "canada"), prac1_scrape(2018,986, "france"), prac1_scrape(2018,987, "austria"),
                     prac1_scrape(2018,988, "great_britain"), prac1_scrape(2018,989, "germany"), prac1_scrape(2018,990, "hungary"),
                     prac1_scrape(2018,991, "belgium"), prac1_scrape(2018,992, "italy"), prac1_scrape(2018,993, "singapore"),
                     prac1_scrape(2018,994, "russia"), prac1_scrape(2018,995, "japan"), prac1_scrape(2018,996, "united-states"),
                     prac1_scrape(2018,997, "mexico"), prac1_scrape(2018,998, "brazil"), prac1_scrape(2018,999, "abu-dhabi"))




prac117 <- bind_rows(prac1_scrape(2017, 959, "australia"), prac1_scrape(2017, 960, "china"), prac1_scrape(2017, 961, "bahrain"), 
                     prac1_scrape(2017, 962, "russia"), prac1_scrape(2017, 963, "spain"), prac1_scrape(2017, 964, "monaco"),
                     prac1_scrape(2017, 965, "canada"), prac1_scrape(2017, 966, "azerbaijan"), prac1_scrape(2017,967, "austria"),
                     prac1_scrape(2017,968, "great_britain"), prac1_scrape(2017,969, "hungary"), prac1_scrape(2017,970, "belgium"),
                     prac1_scrape(2017,971, "italy"), prac1_scrape(2017,972, "singapore"), prac1_scrape(2017,973, "malaysia"),
                     prac1_scrape(2017,974, "japan"), prac1_scrape(2017,975, "united-states"), prac1_scrape(2017,976, "mexico"),
                     prac1_scrape(2017,977, "brazil"), prac1_scrape(2017,978, "abu-dhabi"))


prac116 <- bind_rows(prac1_scrape(2016, 938, "australia"), prac1_scrape(2016, 939, "bahrain"), prac1_scrape(2016, 940, "china"), 
                     prac1_scrape(2016, 941, "russia"), prac1_scrape(2016, 942, "spain"), prac1_scrape(2016, 943, "monaco"),
                     prac1_scrape(2016, 944, "canada"), prac1_scrape(2016, 958, "europe"), prac1_scrape(2017,967, "austria"),
                     prac1_scrape(2016,946, "great_britain"), prac1_scrape(2016,947, "hungary"), prac1_scrape(2016,948, "germany"),
                     prac1_scrape(2016,949, "belgium"), prac1_scrape(2016,950, "italy"), prac1_scrape(2016,951, "singapore"),
                     prac1_scrape(2016,952, "malaysia"), prac1_scrape(2016,953, "japan"), prac1_scrape(2016,954, "united-states"),
                     prac1_scrape(2016,955, "mexico"), prac1_scrape(2016,956, "brazil"), prac1_scrape(2016,957, "abu-dhabi"))

prac115 <- bind_rows(prac1_scrape(2015, 917, "australia"), prac1_scrape(2015, 918, "malaysia"), prac1_scrape(2015, 919, "china"), 
                     prac1_scrape(2015, 920, "bahrain"), prac1_scrape(2015, 921, "spain"), prac1_scrape(2015, 922, "monaco"),
                     prac1_scrape(2016, 944, "canada"), prac1_scrape(2016, 958, "europe"), prac1_scrape(2017,967, "austria"),
                     prac1_scrape(2016,946, "great_britain"), prac1_scrape(2016,947, "hungary"), prac1_scrape(2016,948, "germany"),
                     prac1_scrape(2016,949, "belgium"), prac1_scrape(2016,950, "italy"), prac1_scrape(2016,951, "singapore"),
                     prac1_scrape(2016,952, "malaysia"), prac1_scrape(2016,953, "japan"), prac1_scrape(2016,954, "united-states"),
                     prac1_scrape(2016,955, "mexico"), prac1_scrape(2016,956, "brazil"), prac1_scrape(2016,957, "abu-dhabi"))






prac218 <- bind_rows(prac2_scrape(2018,979, "australia"), prac2_scrape(2018,980, "bahrain"), prac2_scrape(2018,981, "china"), 
                     prac2_scrape(2018,982, "azerbaijan"), prac2_scrape(2018,983, "spain"), prac2_scrape(2018,984, "monaco"),
                     prac2_scrape(2018,985, "canada"), prac2_scrape(2018,986, "france"), prac2_scrape(2018,987, "austria"),
                     prac2_scrape(2018,988, "great_britain"), prac2_scrape(2018,989, "germany"), prac2_scrape(2018,990, "hungary"),
                     prac2_scrape(2018,991, "belgium"), prac2_scrape(2018,992, "italy"), prac2_scrape(2018,993, "singapore"),
                     prac2_scrape(2018,994, "russia"), prac2_scrape(2018,995, "japan"), prac2_scrape(2018,996, "united-states"),
                     prac2_scrape(2018,997, "mexico"), prac2_scrape(2018,998, "brazil"), prac2_scrape(2018,999, "abu-dhabi"))




prac217 <- bind_rows(prac2_scrape(2017, 959, "australia"), prac2_scrape(2017, 960, "china"), prac2_scrape(2017, 961, "bahrain"), 
                     prac2_scrape(2017, 962, "russia"), prac2_scrape(2017, 963, "spain"), prac2_scrape(2017, 964, "monaco"),
                     prac2_scrape(2017, 965, "canada"), prac2_scrape(2017, 966, "azerbaijan"), prac2_scrape(2017,967, "austria"),
                     prac2_scrape(2017,968, "great_britain"), prac2_scrape(2017,969, "hungary"), prac2_scrape(2017,970, "belgium"),
                     prac2_scrape(2017,971, "italy"), prac2_scrape(2017,972, "singapore"), prac2_scrape(2017,973, "malaysia"),
                     prac2_scrape(2017,974, "japan"), prac2_scrape(2017,975, "united-states"), prac2_scrape(2017,976, "mexico"),
                     prac2_scrape(2017,977, "brazil"), prac2_scrape(2017,978, "abu-dhabi"))


prac216 <- bind_rows(prac2_scrape(2016, 938, "australia"), prac2_scrape(2016, 939, "bahrain"), prac2_scrape(2016, 940, "china"), 
                     prac2_scrape(2016, 941, "russia"), prac2_scrape(2016, 942, "spain"), prac2_scrape(2016, 943, "monaco"),
                     prac2_scrape(2016, 944, "canada"), prac2_scrape(2016, 958, "europe"), prac2_scrape(2017,967, "austria"),
                     prac2_scrape(2016,946, "great_britain"), prac2_scrape(2016,947, "hungary"), prac2_scrape(2016,948, "germany"),
                     prac2_scrape(2016,949, "belgium"), prac2_scrape(2016,950, "italy"), prac2_scrape(2016,951, "singapore"),
                     prac2_scrape(2016,952, "malaysia"), prac2_scrape(2016,953, "japan"), prac2_scrape(2016,954, "united-states"),
                     prac2_scrape(2016,955, "mexico"), prac2_scrape(2016,956, "brazil"), prac2_scrape(2016,957, "abu-dhabi"))



qual118 <- bind_rows(qual_scrape(2018,979, "australia"), qual_scrape(2018,980, "bahrain"), qual_scrape(2018,981, "china"),
                     qual_scrape(2018,982, "azerbaijan"), qual_scrape(2018,983, "spain"), qual_scrape(2018,984, "monaco"),
                     qual_scrape(2018,985, "canada"), qual_scrape(2018,986, "france"), qual_scrape(2018,987, "austria"),
                     qual_scrape(2018,988, "great_britain"), qual_scrape(2018,989, "germany"), qual_scrape(2018,990, "hungary"),
                     qual_scrape(2018,991, "belgium"), qual_scrape(2018,992, "italy"), qual_scrape(2018,993, "singapore"),
                     qual_scrape(2018,994, "russia"), qual_scrape(2018,995, "japan"), qual_scrape(2018,996, "united-states"),
                     qual_scrape(2018,997, "mexico"), qual_scrape(2018,998, "brazil"), qual_scrape(2018,999, "abu-dhabi"))




qual117 <- bind_rows(qual_scrape(2017, 959, "australia"), qual_scrape(2017, 960, "china"), qual_scrape(2017, 961, "bahrain"), 
                     qual_scrape(2017, 962, "russia"), qual_scrape(2017, 963, "spain"), qual_scrape(2017, 964, "monaco"),
                     qual_scrape(2017, 965, "canada"), qual_scrape(2017, 966, "azerbaijan"), qual_scrape(2017,967, "austria"),
                     qual_scrape(2017,968, "great_britain"), qual_scrape(2017,969, "hungary"), qual_scrape(2017,970, "belgium"),
                     qual_scrape(2017,971, "italy"), qual_scrape(2017,972, "singapore"), qual_scrape(2017,973, "malaysia"),
                     qual_scrape(2017,974, "japan"), qual_scrape(2017,975, "united-states"), qual_scrape(2017,976, "mexico"),
                     qual_scrape(2017,977, "brazil"), qual_scrape(2017,978, "abu-dhabi"))


qual116 <- bind_rows(qual_scrape(2016, 938, "australia"), qual_scrape(2016, 939, "bahrain"), qual_scrape(2016, 940, "china"), 
                     qual_scrape(2016, 941, "russia"), qual_scrape(2016, 942, "spain"), qual_scrape(2016, 943, "monaco"),
                     qual_scrape(2016, 944, "canada"), qual_scrape(2016, 958, "europe"), qual_scrape(2017,967, "austria"),
                     qual_scrape(2016,946, "great_britain"), qual_scrape(2016,947, "hungary"), qual_scrape(2016,948, "germany"),
                     qual_scrape(2016,949, "belgium"), qual_scrape(2016,950, "italy"), qual_scrape(2016,951, "singapore"),
                     qual_scrape(2016,952, "malaysia"), qual_scrape(2016,953, "japan"), qual_scrape(2016,954, "united-states"),
                     qual_scrape(2016,955, "mexico"), qual_scrape(2016,956, "brazil"), qual_scrape(2016,957, "abu-dhabi"))





prac119 <-  bind_rows(prac1_scrape(2019,1000, "australia"), prac1_scrape(2019,1001, "bahrain"), prac1_scrape(2019,1002, "china"), 
                      prac1_scrape(2019,1003, "azerbaijan"), prac1_scrape(2019,1004, "spain"), prac1_scrape(2019,1005, "monaco"),
                      prac1_scrape(2019,1006, "canada"), prac1_scrape(2019,1007, "france"))

prac219 <- bind_rows(prac2_scrape(2019,1000, "australia"), prac2_scrape(2019,1001, "bahrain"), prac2_scrape(2019,1002, "china"), 
                     prac2_scrape(2019,1003, "azerbaijan"), prac2_scrape(2019,1004, "spain"), prac2_scrape(2019,1005, "monaco"),
                     prac2_scrape(2019,1006, "canada"), prac2_scrape(2019,1007, "france"))

qual119 <- bind_rows(qual_scrape(2019,1000, "australia"), qual_scrape(2019,1001, "bahrain"), qual_scrape(2019,1002, "china"),
                     qual_scrape(2019,1003, "azerbaijan"), qual_scrape(2019,1004, "spain"), qual_scrape(2019,1005, "monaco"),
                     qual_scrape(2019,1006, "canada"), qual_scrape(2019,1007, "france"))




#####################creat dataframe  prac1#######################################################################

trackcat <- read_csv("trackcat.csv")


all18 <- prac118 %>% left_join(qual118, by = c("race1", "No" ))%>% 
  left_join(trackcat)
all17 <- prac117 %>% left_join(qual117, by = c("race1", "No" )) %>%
  left_join(trackcat)
all16 <- prac116 %>% left_join(qual116, by = c("race1", "No" )) %>%
  left_join(trackcat)



f1q <- all18 %>% bind_rows(all17) %>%
  bind_rows(all16) %>%
  separate(Car, c("n1", "n2"), sep = " ") %>%
  mutate(Team = if_else(n1 == "Mercedes", "Mercedes", 
                        if_else(n1 == "Red", "Red Bull", 
                                if_else(n1 == "Ferrari", "Ferrari", 
                                        if_else(n1 == "Haas", "Haas", 
                                                if_else(n1 == "McLaren", "McLaren",
                                                        if_else(n1 == "Renault", "Renault", 
                                                                if_else(n1 == "Scuderia", "Toro Rosso", 
                                                                        if_else(n1 == "Williams", "Williams",
                                                                                if_else(n1 == "Force", "Racing Point",
                                                                                        if_else(n1 == "Sauber", "Alfa Romeo", "other"))))))))))) %>%
  select(prac1pos, No, Team, pract1t, pract1l , pract1d, Cat, qtime) %>%
  filter(!is.na(pract1t)) %>%
  filter(!is.na(qtime)) %>%
  filter(Team != "other")


######## create dataframe prac & 2 ##########################################################


trackcat <- read_csv("trackcat.csv")


all182 <- prac118 %>% left_join(qual118, by = c("race1", "No" ))%>%
  left_join(prac218, by = c("race1", "No" )) 

colnames(all182)[7] <- "gp"

all182 <- all182 %>%  left_join(trackcat, by = "gp")



all172 <- prac117 %>% left_join(qual117, by = c("race1", "No" )) %>%
  left_join(prac217, by = c("race1", "No" ))

colnames(all172)[7] <- "gp"

all172 <- all172 %>%  left_join(trackcat, by = "gp")

all162 <- prac116 %>% left_join(qual116, by = c("race1", "No" )) %>%
  left_join(prac216, by = c("race1", "No" )) 

colnames(all162)[7] <- "gp"

all162 <- all162 %>%  left_join(trackcat, by = "gp")



f1q2 <- all182 %>% bind_rows(all172) %>%
  bind_rows(all162) %>%
  separate(Car.x, c("n1", "n2"), sep = " ") %>%
  mutate(Team = if_else(n1 == "Mercedes", "Mercedes", 
                        if_else(n1 == "Red", "Red Bull", 
                                if_else(n1 == "Ferrari", "Ferrari", 
                                        if_else(n1 == "Haas", "Haas", 
                                                if_else(n1 == "McLaren", "McLaren",
                                                        if_else(n1 == "Renault", "Renault", 
                                                                if_else(n1 == "Scuderia", "Toro Rosso", 
                                                                        if_else(n1 == "Williams", "Williams",
                                                                                if_else(n1 == "Force", "Racing Point",
                                                                                        if_else(n1 == "Sauber", "Alfa Romeo", "other"))))))))))) %>%
  select(prac1pos, prac2pos, No, Team, pract1t, pract1l , pract1d, pract2t, pract2l , pract2d, Cat, qtime) %>%
  filter(!is.na(pract1t)) %>%
  filter(!is.na(pract2t)) %>%
  filter(!is.na(qtime)) %>%
  filter(Team != "other")


##### latest data model ####################################################################################


trackcat <- read_csv("trackcat.csv")


all182 <- prac118 %>% left_join(qual118, by = c("race1", "No" ))%>%
  left_join(prac218, by = c("race1", "No" )) 

colnames(all182)[7] <- "gp"

all182 <- all182 %>%  left_join(trackcat, by = "gp")



all172 <- prac117 %>% left_join(qual117, by = c("race1", "No" )) %>%
  left_join(prac217, by = c("race1", "No" ))

colnames(all172)[7] <- "gp"

all172 <- all172 %>%  left_join(trackcat, by = "gp")

all162 <- prac116 %>% left_join(qual116, by = c("race1", "No" )) %>%
  left_join(prac216, by = c("race1", "No" )) 

colnames(all162)[7] <- "gp"

all162 <- all162 %>%  left_join(trackcat, by = "gp")


all192 <- prac119 %>% left_join(qual119, by = c("race1", "No" )) %>%
  left_join(prac219, by = c("race1", "No" )) 

colnames(all192)[7] <- "gp"

all192 <- all192 %>%  left_join(trackcat, by = "gp")


f1qcur <- all182 %>% bind_rows(all172) %>%
  bind_rows(all162) %>%
  bind_rows(all192) %>%
  separate(Car.x, c("n1", "n2"), sep = " ") %>%
  mutate(Team = if_else(n1 == "Mercedes", "Mercedes", 
                        if_else(n1 == "Red", "Red Bull", 
                                if_else(n1 == "Ferrari", "Ferrari", 
                                        if_else(n1 == "Haas", "Haas", 
                                                if_else(n1 == "McLaren", "McLaren",
                                                        if_else(n1 == "Renault", "Renault", 
                                                                if_else(n1 == "Scuderia", "Toro Rosso", 
                                                                        if_else(n1 == "Williams", "Williams",
                                                                                if_else(n1 == "Force", "Racing Point",
                                                                                        if_else(n1 == "Sauber", "Alfa Romeo", "other"))))))))))) %>%
  select(prac1pos, prac2pos, No, Team, pract1t, pract1l , pract1d, pract2t, pract2l , pract2d, Cat, qtime, Length) %>%
  filter(!is.na(pract1t)) %>%
  filter(!is.na(pract2t)) %>%
  filter(!is.na(qtime)) %>%
  filter(Team != "other")

##### first practice model ############################################################################################

track <- f1q %>% mutate(cat2 = as.factor(Cat)) %>%
  select(cat2)

trackmat1 <- model.matrix(~cat2-1, track)

team <- f1q %>% select(Team)

teammat <- model.matrix(~Team-1, team)

f12 <- f1q %>% select(-Team, -No, -Cat)


f1all <- cbind(f12, trackmat1)
f1all2 <- cbind(f1all, teammat)



labels1 <- f1all2$qtime

features1 <- f1all2 %>% select(-qtime)


numberOfTrainingSamples <- round(length(labels1) * .9)

train_data1 <- features1[1:numberOfTrainingSamples, ]
train_labels1 <- labels1[1:numberOfTrainingSamples]

test_data1 <- features1[numberOfTrainingSamples:length(labels1),]
test_labels1 <- labels1[numberOfTrainingSamples:length(labels1)]

dtrain_f1m1 <- xgb.DMatrix(data = as.matrix(train_data1), label = train_labels1)
dtest_f1m1 <- xgb.DMatrix(data = as.matrix(test_data1), label = test_labels1)





f1mod1 <- xgboost(data = dtrain_f1m1, nrounds = 50, objective = "reg:linear")
f1mod2 <- xgboost(data = dtrain_f1m1, nrounds = 100, objective = "reg:linear")

########### orac 1 and 2mod #######################################################################

track <- f1q2 %>% mutate(cat2 = as.factor(Cat)) %>%
  select(cat2)

trackmat2 <- model.matrix(~cat2-1, track)

team <- f1q2 %>% select(Team)

teammat2 <- model.matrix(~Team-1, team)

f12 <- f1q2 %>% select(-Team, -No, -Cat)


f1all <- cbind(f12, trackmat2)
f1all2 <- cbind(f1all, teammat2)



labels2 <- f1all2$qtime

features2 <- f1all2 %>% select(-qtime)


numberOfTrainingSamples <- round(length(labels2) * .9)

train_data2 <- features2[1:numberOfTrainingSamples, ]
train_labels2 <- labels2[1:numberOfTrainingSamples]

test_data2 <- features2[numberOfTrainingSamples:length(labels2),]
test_labels2 <- labels2[numberOfTrainingSamples:length(labels2)]

dtrain_f1m2 <- xgb.DMatrix(data = as.matrix(train_data2), label = train_labels2)
dtest_f1m2 <- xgb.DMatrix(data = as.matrix(test_data2), label = test_labels2)



f1mod3 <- xgboost(data = dtrain_f1m2, nrounds = 50, objective = "reg:linear")
f1mod4 <- xgboost(data = dtrain_f1m2, nrounds = 100, objective = "reg:linear")


########## current data mod  ########################################

track <- f1qcur %>% mutate(cat2 = as.factor(Cat)) %>%
  select(cat2)

trackmat2 <- model.matrix(~cat2-1, track)

team <- f1qcur %>% select(Team)

teammat2 <- model.matrix(~Team-1, team)

f1qcur2 <- f1qcur %>% select(-Team, -No, -Cat)


f1qcur3 <- cbind(f1qcur2, trackmat2)
f1qcur4 <- cbind(f1qcur3, teammat2)



labels3 <- f1qcur4$qtime

features3 <- f1qcur4 %>% select(-qtime)


numberOfTrainingSamples <- round(length(labels3) * .9)

train_data3 <- features3[1:numberOfTrainingSamples, ]
train_labels3 <- labels3[1:numberOfTrainingSamples]

test_data3 <- features3[numberOfTrainingSamples:length(labels3),]
test_labels3 <- labels3[numberOfTrainingSamples:length(labels3)]

dtrain_f1m3 <- xgb.DMatrix(data = as.matrix(train_data3), label = train_labels3)
dtest_f1m3 <- xgb.DMatrix(data = as.matrix(test_data3), label = test_labels3)



f1mod5 <- xgboost(data = dtrain_f1m3, nrounds = 100, objective = "reg:linear")

