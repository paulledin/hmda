processHMDA_final <- function(databaseName="hmda", databaseHost="127.0.0.1", databaseUser="root", databasePassword="ledin", year=NULL)
{
    #library(RMySQL)
    library(plyr)
    library(sqldf)
    library(stringr)
    library(RMariaDB)
    
    #dbConn <- dbConnect(RMySQL::MySQL(), dbname=databaseName, host=databaseHost, username=databaseUser, password=databasePassword)
    dbConn <- dbConnect(RMariaDB::MariaDB(), dbname=databaseName, host=databaseHost, username=databaseUser, password=databasePassword)
    
    #hmda <- dbGetQuery(dbConn, paste("SELECT * FROM LAR_", year, " WHERE co_applicant_race_1 IN (6,7,8) ")) 
    
    total_rec_count <- dbGetQuery(dbConn, paste("SELECT COUNT(*) FROM LAR_", year, " ", sep="")) 
    names(total_rec_count)[1] <- "total_hmda_records"
    
    total_recs <- sum(total_rec_count$total_hmda_records)
    
    print(paste("Total HMDA Recs == ", format(total_recs, big.mark = ",")))
    
    return (total_rec_count)
    
    
    
    
    
    states <- dbGetQuery(dbConn, paste("SELECT DISTINCT(state_code) FROM LAR_", year, " ", sep=""))
    
    allHMDA <- NULL
    for(i in states$state_code)
    {
        hmda <- dbGetQuery(dbConn, paste("SELECT * FROM LAR_", year, " WHERE co_applicant_race_1 IN (6, 7 , 8) AND state_code='", i, "'",  sep=""))
        print (paste("Working on State Code: ", i))
        
        if(nrow(hmda) == 0)
        {
            next
        }
        
        ## Start with part I aka no co-App.
        no_coapp <- hmda[hmda$co_applicant_race_1==6 | hmda$co_applicant_race_1==7 | hmda$co_applicant_race_1==8, ]
        
        one_race <- no_coapp[no_coapp$applicant_race_2==" " & no_coapp$applicant_race_3==" " & no_coapp$applicant_race_4==" " & no_coapp$applicant_race_5==" ", ] 
        one_race$race_category <- one_race$applicant_race_1         
        
        # 1 = American Indian or Alaska Native
        # 2 = Asian
        # 3 = African American
        
        # 4 = Native Hawaiian or Other Pacific Islander
        # 5 = White
        # 6 = 2 or more minority races
        # 7 = Joint
        # 8 = NA
        
        multi_race <- no_coapp[no_coapp$applicant_race_2!=" " | no_coapp$applicant_race_3!=" " | no_coapp$applicant_race_4!=" " | no_coapp$applicant_race_5!=" ", ]
        
        multi_race$applicant_race_2 <- ifelse(multi_race$applicant_race_2==5, " ", multi_race$applicant_race_2)
        multi_race$applicant_race_3 <- ifelse(multi_race$applicant_race_3==5, " ", multi_race$applicant_race_3)
        multi_race$applicant_race_4 <- ifelse(multi_race$applicant_race_4==5, " ", multi_race$applicant_race_4)
        multi_race$applicant_race_5 <- ifelse(multi_race$applicant_race_5==5, " ", multi_race$applicant_race_5)
        
        multi_race$race_category <- ifelse(multi_race$applicant_race_2!=" " | multi_race$applicant_race_3!=" " | multi_race$applicant_race_4!=" "| multi_race$applicant_race_5!=" ", 6, multi_race$applicant_race_1)
        
        part1 <- rbind(one_race, multi_race)
        part1$race_category <- ifelse(part1$applicant_race_1==6 | part1$applicant_race_1==7, 8, part1$race_category)
        
        part1$applicant_ethnicity <- ifelse(part1$applicant_ethnicity==3 | part1$applicant_ethnicity==4, 3, part1$applicant_ethnicity)
        
        # 1 = Hispanic
        # 2 = Non-Hispanic
        # 3 = NA
        # 4 = Joint
        part1$ethnicity_category <- 3
        part1$ethnicity_category <- ifelse(part1$applicant_ethnicity==2 & part1$co_applicant_ethnicity==1, 4, part1$ethnicity_category)
        part1$ethnicity_category <- ifelse(part1$applicant_ethnicity==2 & part1$co_applicant_ethnicity==5, 2, part1$ethnicity_category)
        part1$ethnicity_category <- ifelse(part1$applicant_ethnicity==3 & part1$co_applicant_ethnicity==1, 4, part1$ethnicity_category)
        part1$ethnicity_category <- ifelse(part1$applicant_ethnicity==1 & part1$co_applicant_ethnicity==1, 1, part1$ethnicity_category)
        part1$ethnicity_category <- ifelse(part1$applicant_ethnicity==1 & part1$co_applicant_ethnicity==2, 4, part1$ethnicity_category)
        part1$ethnicity_category <- ifelse(part1$applicant_ethnicity==1 & part1$co_applicant_ethnicity==3, 4, part1$ethnicity_category)
        part1$ethnicity_category <- ifelse(part1$applicant_ethnicity==1 & part1$co_applicant_ethnicity==4, 4, part1$ethnicity_category)
        part1$ethnicity_category <- ifelse(part1$applicant_ethnicity==1 & part1$co_applicant_ethnicity==5, 1, part1$ethnicity_category)
        
        # 1 = Minority
        # 2 = Non-Minority
        # 3 = NA
        part1$minority_category <- 3
        part1$minority_category <- ifelse(part1$race_category==5 & part1$ethnicity_category==2, 2, part1$minority_category)
        part1$minority_category <- ifelse(part1$race_category==5 & part1$ethnicity_category==1, 1, part1$minority_category)
        part1$minority_category <- ifelse(part1$race_category!=5 & part1$ethnicity_category==2, 1, part1$minority_category)
        part1$minority_category <- ifelse(part1$race_category!=5 & part1$ethnicity_category==1, 1, part1$minority_category)
        part1$minority_category <- ifelse(part1$race_category==8 | part1$ethnicity_category==3, 3, part1$minority_category)
        
        gc()
        
        #allHMDA <- rbind(allHMDA, part1)
        
        part1$includes_white <- ifelse((part1$applicant_race_1==5 | part1$applicant_race_2==5 | part1$applicant_race_3==5 | part1$applicant_race_4==5 | part1$applicant_race_5==5) & part1$race_category==6, 1, 0)
        
        includes_white <- part1[part1$includes_white == 1, ]
        others <- part1[part1$includes_white == 0, ]
        
        includes_white_fixed <- NULL
        for(i in 1:nrow(includes_white))
        {
            thisRec <- includes_white[i, ]
            
            raceCount <- 0
            if(thisRec$applicant_race_1 != " ")
            {
                raceCount <- raceCount + 1
            }
            if(thisRec$applicant_race_2 != " ")
            {
                raceCount <- raceCount + 1
            }
            if(thisRec$applicant_race_3 != " ")
            {
                raceCount <- raceCount + 1
            }
            if(thisRec$applicant_race_4 != " ")
            {
                raceCount <- raceCount + 1
            }
            if(thisRec$applicant_race_5 != " ")
            {
                raceCount <- raceCount + 1
            }
            
            if(raceCount == 2)
            {
                if(thisRec$applicant_race_1 != " " & thisRec$applicant_race_1 != 5)
                {
                    thisRec$race_category <- thisRec$applicant_race_1
                }
                if(thisRec$applicant_race_2 != " " & thisRec$applicant_race_2 != 5)
                {
                    thisRec$race_category <- thisRec$applicant_race_2
                }
                if(thisRec$applicant_race_3 != " " & thisRec$applicant_race_3 != 5)
                {
                    thisRec$race_category <- thisRec$applicant_race_3
                }
                if(thisRec$applicant_race_4 != " " & thisRec$applicant_race_4 != 5)
                {
                    thisRec$race_category <- thisRec$applicant_race_4
                }
                if(thisRec$applicant_race_5 != " " & thisRec$applicant_race_5 != 5)
                {
                    thisRec$race_category <- thisRec$applicant_race_5
                }
            }
            
            includes_white_fixed <- rbind(includes_white_fixed, thisRec)
        }
        
        part1 <- rbind(others, includes_white_fixed)
        part1 <- subset(part1, select = -c(includes_white))
        part1$type <- "Single"
        
        without_co_app_sex <- part1[part1$co_applicant_sex!=1 & part1$co_applicant_sex!=2, ]
        without_co_app_sex$sex_category <- NA
        without_co_app_sex$sex_category <- without_co_app_sex$applicant_sex
        without_co_app_sex$sex_category <- ifelse(without_co_app_sex$applicant_sex==3, 4, without_co_app_sex$sex_category)
        
        with_co_app_sex <- part1[part1$co_applicant_sex==1 | part1$co_applicant_sex==2, ]
        with_co_app_sex$sex_category <- NA
        with_co_app_sex$sex_category <- ifelse(with_co_app_sex$applicant_sex == with_co_app_sex$co_applicant_sex, with_co_app_sex$applicant_sex, with_co_app_sex$sex_category)
        with_co_app_sex$sex_category <- ifelse(with_co_app_sex$applicant_sex != with_co_app_sex$co_applicant_sex, 3, with_co_app_sex$sex_category)
        
        part1 <- rbind(without_co_app_sex, with_co_app_sex)
        
        dbWriteTable(dbConn, paste("LAR_", year, "_tmp", sep=""), part1, append=TRUE)
    }
    

    
    #####  Thus concludes part I.
    print("Thus concludes part I.")
    
    #return (part1)
    
    print("Starting Part II")
    
    allHMDA <- NULL
    for(i in states$state_code)
    {
        coapp <- dbGetQuery(dbConn, paste("SELECT * FROM LAR_", year, " WHERE co_applicant_race_1 NOT IN (6, 7 , 8) AND state_code='", i, "'",  sep=""))
        print (paste("Working on State Code: ", i))
        
        if(nrow(coapp) == 0)
        {
            next
        }
        
        app_one_race <- coapp[coapp$applicant_race_2==" " & coapp$applicant_race_3==" " & coapp$applicant_race_4==" " & coapp$applicant_race_5==" ", ] 
        app_one_race$app_race_category <- app_one_race$applicant_race_1         
        
        # 1 = American Indian or Alaska Native
        # 2 = Asian
        # 3 = African American
        # 4 = Native Hawaiian or Other Pacific Islander
        # 5 = White
        # 6 = 2 or more minority races
        # 7 = Joint
        # 8 = NA
        
        app_multi_race <- coapp[coapp$applicant_race_2!=" " | coapp$applicant_race_3!=" " | coapp$applicant_race_4!=" " | coapp$applicant_race_5!=" ", ]
        app_multi_race$applicant_race_1 <- ifelse(app_multi_race$applicant_race_1==5, " ", app_multi_race$applicant_race_1)
        app_multi_race$applicant_race_2 <- ifelse(app_multi_race$applicant_race_2==5, " ", app_multi_race$applicant_race_2)
        app_multi_race$applicant_race_3 <- ifelse(app_multi_race$applicant_race_3==5, " ", app_multi_race$applicant_race_3)
        app_multi_race$applicant_race_4 <- ifelse(app_multi_race$applicant_race_4==5, " ", app_multi_race$applicant_race_4)
        app_multi_race$applicant_race_5 <- ifelse(app_multi_race$applicant_race_5==5, " ", app_multi_race$applicant_race_5)
        
        app_multi_race$app_race_category <- ifelse(app_multi_race$applicant_race_2!=" " | app_multi_race$applicant_race_3!=" " | app_multi_race$applicant_race_4!=" " | app_multi_race$applicant_race_5!=" ", 6, app_multi_race$applicant_race_1)
        
        app_multi_race$includes_white <- ifelse((app_multi_race$applicant_race_1==5 | app_multi_race$applicant_race_2==5 | app_multi_race$applicant_race_3==5 | app_multi_race$applicant_race_4==5 | app_multi_race$applicant_race_5==5) & app_multi_race$app_race_category==6, 1, 0)
        
        includes_white <- app_multi_race[app_multi_race$includes_white == 1, ]
        others <- app_multi_race[app_multi_race$includes_white == 0, ]
        
        includes_white_fixed <- NULL
        if(nrow(includes_white) > 0)
        {
            for(i in 1:nrow(includes_white))
            {
                thisRec <- includes_white[i, ]
                
                raceCount <- 0
                if(thisRec$applicant_race_1 != " ")
                {
                    raceCount <- raceCount + 1
                }
                if(thisRec$applicant_race_2 != " ")
                {
                    raceCount <- raceCount + 1
                }
                if(thisRec$applicant_race_3 != " ")
                {
                    raceCount <- raceCount + 1
                }
                if(thisRec$applicant_race_4 != " ")
                {
                    raceCount <- raceCount + 1
                }
                if(thisRec$applicant_race_5 != " ")
                {
                    raceCount <- raceCount + 1
                }
                
                if(raceCount == 2)
                {
                    if(thisRec$applicant_race_1 != " " & thisRec$applicant_race_1 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_1
                    }
                    if(thisRec$applicant_race_2 != " " & thisRec$applicant_race_2 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_2
                    }
                    if(thisRec$applicant_race_3 != " " & thisRec$applicant_race_3 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_3
                    }
                    if(thisRec$applicant_race_4 != " " & thisRec$applicant_race_4 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_4
                    }
                    if(thisRec$applicant_race_5 != " " & thisRec$applicant_race_5 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_5
                    }
                }
                
                includes_white_fixed <- rbind(includes_white_fixed, thisRec)
            }
        }
        
        app_multi_race <- rbind(others, includes_white_fixed)
        app_multi_race <- subset(app_multi_race, select = -c(includes_white))
        
        part2 <- rbind(app_one_race, app_multi_race)
        
        part2$app_race_category <- ifelse(part2$applicant_race_1==6 | part2$applicant_race_1==7, 8, part2$app_race_category)
        
        coapp_one_race <- part2[part2$co_applicant_race_2==" " & part2$co_applicant_race_3==" " & part2$co_applicant_race_4==" " & part2$co_applicant_race_5==" ", ]
        coapp_one_race$coapp_race_category <- coapp_one_race$co_applicant_race_1
        
        coapp_multi_race <- part2[part2$co_applicant_race_2!=" " | part2$co_applicant_race_3!=" " | part2$co_applicant_race_4!=" " | part2$co_applicant_race_5!=" ", ]
        coapp_multi_race$co_applicant_race_1 <- ifelse(coapp_multi_race$co_applicant_race_1==5, " ", coapp_multi_race$co_applicant_race_1)
        coapp_multi_race$co_applicant_race_2 <- ifelse(coapp_multi_race$co_applicant_race_2==5, " ", coapp_multi_race$co_applicant_race_2)
        coapp_multi_race$co_applicant_race_3 <- ifelse(coapp_multi_race$co_applicant_race_3==5, " ", coapp_multi_race$co_applicant_race_3)
        coapp_multi_race$co_applicant_race_4 <- ifelse(coapp_multi_race$co_applicant_race_4==5, " ", coapp_multi_race$co_applicant_race_4)
        coapp_multi_race$co_applicant_race_5 <- ifelse(coapp_multi_race$co_applicant_race_5==5, " ", coapp_multi_race$co_applicant_race_5)
        
        coapp_multi_race$coapp_race_category <- ifelse(coapp_multi_race$co_applicant_race_2!=" " | coapp_multi_race$co_applicant_race_3!=" " | coapp_multi_race$co_applicant_race_4!=" " | coapp_multi_race$co_applicant_race_5!=" ", 6, coapp_multi_race$co_applicant_race_1)
        
        coapp_multi_race$includes_white <- ifelse((coapp_multi_race$applicant_race_1==5 | coapp_multi_race$applicant_race_2==5 | coapp_multi_race$applicant_race_3==5 | coapp_multi_race$applicant_race_4==5 | coapp_multi_race$applicant_race_5==5) & coapp_multi_race$coapp_race_category==6, 1, 0)
        
        includes_white <- coapp_multi_race[coapp_multi_race$includes_white == 1, ]
        others <- coapp_multi_race[coapp_multi_race$includes_white == 0, ]
        
        includes_white_fixed <- NULL
        if(nrow(includes_white) > 0)
        {
            for(i in 1:nrow(includes_white))
            {
                thisRec <- includes_white[i, ]
                
                raceCount <- 0
                if(thisRec$applicant_race_1 != " ")
                {
                    raceCount <- raceCount + 1
                }
                if(thisRec$applicant_race_2 != " ")
                {
                    raceCount <- raceCount + 1
                }
                if(thisRec$applicant_race_3 != " ")
                {
                    raceCount <- raceCount + 1
                }
                if(thisRec$applicant_race_4 != " ")
                {
                    raceCount <- raceCount + 1
                }
                if(thisRec$applicant_race_5 != " ")
                {
                    raceCount <- raceCount + 1
                }
                
                if(raceCount == 2)
                {
                    if(thisRec$applicant_race_1 != " " & thisRec$applicant_race_1 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_1
                    }
                    if(thisRec$applicant_race_2 != " " & thisRec$applicant_race_2 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_2
                    }
                    if(thisRec$applicant_race_3 != " " & thisRec$applicant_race_3 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_3
                    }
                    if(thisRec$applicant_race_4 != " " & thisRec$applicant_race_4 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_4
                    }
                    if(thisRec$applicant_race_5 != " " & thisRec$applicant_race_5 != 5)
                    {
                        thisRec$race_category <- thisRec$applicant_race_5
                    }
                }
                
                includes_white_fixed <- rbind(includes_white_fixed, thisRec)
            }
        }
        
        coapp_multi_race <- rbind(others, includes_white_fixed)
        coapp_multi_race <- subset(coapp_multi_race, select = -c(includes_white))
        
        part2 <- rbind(coapp_one_race, coapp_multi_race)
        part2$race_category <- part2$app_race_category
        part2$race_category <- ifelse(part2$app_race_category!=5 & part2$coapp_race_category==5, 7, part2$race_category)
        part2$race_category <- ifelse(part2$app_race_category==5 & part2$coapp_race_category!=5, 7, part2$race_category)
        part2$race_category <- ifelse(part2$race_category==7 & part2$app_race_category==8, 8, part2$race_category)
        
        part2$applicant_ethnicity <- ifelse(part2$applicant_ethnicity==3 | part2$applicant_ethnicity==4, 3, part2$applicant_ethnicity)
        # 1 = Hispanic
        # 2 = Non-Hispanic
        # 3 = NA
        # 4 = Joint
        
        part2$ethnicity_category <- 3
        part2$ethnicity_category <- ifelse(part2$applicant_ethnicity==3 & part2$co_applicant_ethnicity==1, 4, part2$ethnicity_category)
        part2$ethnicity_category <- ifelse(part2$applicant_ethnicity==1 & part2$co_applicant_ethnicity==1, 1, part2$ethnicity_category)
        part2$ethnicity_category <- ifelse(part2$applicant_ethnicity==1 & part2$co_applicant_ethnicity==2, 4, part2$ethnicity_category)
        part2$ethnicity_category <- ifelse(part2$applicant_ethnicity==1 & part2$co_applicant_ethnicity==3, 4, part2$ethnicity_category)
        part2$ethnicity_category <- ifelse(part2$applicant_ethnicity==1 & part2$co_applicant_ethnicity==4, 4, part2$ethnicity_category)
        part2$ethnicity_category <- ifelse(part2$applicant_ethnicity==2 & part2$co_applicant_ethnicity==1, 4, part2$ethnicity_category)
        part2$ethnicity_category <- ifelse(part2$applicant_ethnicity==2 & part2$co_applicant_ethnicity==2, 2, part2$ethnicity_category)
        
        # 1 = Minority
        # 2 = Non-Minority
        # 3 = NA
        part2$minority_category <- 3
        part2$minority_category <- ifelse(part2$race_category==5 & part2$ethnicity_category==2, 2, part2$minority_category)
        part2$minority_category <- ifelse(part2$race_category==5 & part2$ethnicity_category==1, 1, part2$minority_category)
        part2$minority_category <- ifelse(part2$race_category!=5 & part2$ethnicity_category==2, 1, part2$minority_category)
        part2$minority_category <- ifelse(part2$race_category!=5 & part2$ethnicity_category==1, 1, part2$minority_category)
        part2$minority_category <- ifelse(part2$race_category==8 | part2$ethnicity_category==3, 3, part2$minority_category)
        
        part2 <- subset(part2, select = -c(app_race_category, coapp_race_category))
        
        part2$type <- "CoApp"
        
        gc()
        
        ### 1 = Male
        ### 2 = Female 
        ### 3 = Joint
        ### 4 = NA
        without_co_app_sex <- part2[part2$co_applicant_sex!=1 & part2$co_applicant_sex!=2, ]
        without_co_app_sex$sex_category <- NA
        without_co_app_sex$sex_category <- without_co_app_sex$applicant_sex
        without_co_app_sex$sex_category <- ifelse(without_co_app_sex$applicant_sex==3, 4, without_co_app_sex$sex_category)
        
        with_co_app_sex <- part2[part2$co_applicant_sex==1 | part2$co_applicant_sex==2, ]
        with_co_app_sex$sex_category <- NA
        with_co_app_sex$sex_category <- ifelse(with_co_app_sex$applicant_sex == with_co_app_sex$co_applicant_sex, with_co_app_sex$applicant_sex, with_co_app_sex$sex_category)
        with_co_app_sex$sex_category <- ifelse(with_co_app_sex$applicant_sex != with_co_app_sex$co_applicant_sex, 3, with_co_app_sex$sex_category)
        
        part2 <- rbind(without_co_app_sex, with_co_app_sex)
        
        dbWriteTable(dbConn, paste("LAR_", year, "_tmp", sep=""), part2, append=TRUE)
        #allHMDA <- rbind(allHMDA, part2)
    }    
    
    #part2 <- allHMDA
    print("Thus concludes part II")
    
    #allHMDA <- rbind(part1, part2)
    
    #allHMDA <- rbind(without_co_app_sex, with_co_app_sex)
    
    #dbWriteTable(dbConn, paste("LAR_", year, "_Proc", sep=""), allHMDA, overwrite=TRUE)
    
    query <- paste("ALTER TABLE LAR_", year, "_tmp"," ADD COLUMN old_agency_code CHAR(1) ", sep = "")
    dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp"," SET old_agency_code=agency_code ", sep = "")
    dbGetQuery(dbConn, query)
    
    
    if(year == '2017')
    {
        ## CFPB CUs in order => Navy, State Employees, Pentagon, Boeing, SchoolsFirst, Golden1
        query <- paste("UPDATE LAR_", year, "_tmp ", 
                       "SET old_agency_code=5 ", 
                       "WHERE agency_code=9 AND respondent_id IN ('0000617677-9', '0001189117-9', '0000546571-9', '0000972590-9', '0000937898-9', '0000959395-9') ", sep = "")
    }
    else if(year == '2016')
    {
        ## CFPB CUs in order => Navy, State Employees, Pentagon, Boeing, SchoolsFirst
        query <- paste("UPDATE LAR_", year, "_tmp ", 
                       "SET old_agency_code=5 ", 
                       "WHERE agency_code=9 AND respondent_id IN ('0000617677-9', '0001189117-9', '0000546571-9', '0000972590-9', '0000937898-9') ", sep = "")
    }
    else if(year == '2015')
    {
        ## CFPB CUs in order => Navy, State Employees, Pentagon, Boeing, SchoolsFirst
        query <- paste("UPDATE LAR_", year, "_tmp ", 
                       "SET old_agency_code=5 ", 
                       "WHERE agency_code=9 AND respondent_id IN ('0000617677-9', '0001189117-9', '0000546571-9', '0000972590-9', '0000937898-9') ", sep = "")
    }
    else if(year == '2014')
    {
        ## CFPB CUs in order => Navy, State Employees, Pentagon, Boeing
        query <- paste("UPDATE LAR_", year, "_tmp ", 
                       "SET old_agency_code=5 ", 
                       "WHERE agency_code=9 AND respondent_id IN ('0000617677-9', '0001189117-9', '0000546571-9', '0000972590-9') ", sep = "")
    }
    else if(year == '2013')
    {
        ## CFPB CUs in order => Navy, State Employees, Pentagon, Boeing
        query <- paste("UPDATE LAR_", year, "_tmp ", 
                       "SET old_agency_code=5 ", 
                       "WHERE agency_code=9 AND respondent_id IN ('0000617677-9', '0001189117-9', '0000546571-9', '0000972590-9') ", sep = "")
    }
    else if(year == '2012')
    {
        ## CFPB CUs in order => Navy, State Employees, Pentagon
        query <- paste("UPDATE LAR_", year, "_tmp ", 
                       "SET old_agency_code=5 ", 
                       "WHERE agency_code=9 AND respondent_id IN ('0000617677-9', '0001189117-9', '0000546571-9') ", sep = "")
    }
    else if(year == '2011')
    {
        ## CFPB CUs in order => Navy, State Employees, Pentagon
        query <- paste("UPDATE LAR_", year, "_tmp ", 
                       "SET old_agency_code=5 ", 
                       "WHERE agency_code=9 AND respondent_id IN ('0000617677-9', '0001189117-9', '0000546571-9') ", sep = "")
    }
    
    print(query)
    dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000617677' ", sep = "") # Navy
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0001189117' ", sep = "") # State ECU
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000546571' ", sep = "") # Pentagon
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000972590' ", sep = "") # BECU
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000937898' ", sep = "") # Schools First
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000061650' ", sep = "") # The Golden 1
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000011065' ", sep = "") # Security First 
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000067955' ", sep = "") # Alliant 
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000019976' ", sep = "") # First Technology
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000068465' ", sep = "") # Star One
    #dbGetQuery(dbConn, query)
    
    query <- paste("UPDATE LAR_", year, "_tmp "," SET old_agency_code=5 WHERE agency_code=9 AND RespID='0000024694' ", sep = "") # America First
    #dbGetQuery(dbConn, query)
    
    #return (allHMDA)
    
    query <- paste("CREATE INDEX respondent_id on LAR_", year, "_tmp ","(respondent_id) ", sep = "")
    dbGetQuery(dbConn, query)
    
    query <- paste("CREATE INDEX old_agency_code on LAR_", year, "_tmp ","(old_agency_code) ", sep = "")
    dbGetQuery(dbConn, query)
    
    query <- paste("CREATE INDEX StateCode on LAR_", year, "_tmp ","(state_code) ", sep = "")
    dbGetQuery(dbConn, query)
    
    query <- paste("CREATE INDEX race_category on LAR_", year, "_tmp ","(race_category) ", sep = "")
    dbGetQuery(dbConn, query)
    
    query <- paste("CREATE INDEX ethnicity_category on LAR_", year, "_tmp ","(ethnicity_category) ", sep = "")
    dbGetQuery(dbConn, query)
    
    query <- paste("CREATE INDEX minority_category on LAR_", year, "_tmp ","(minority_category) ", sep = "")
    dbGetQuery(dbConn, query)
    
    query <- paste("CREATE INDEX sex_category on LAR_", year, "_tmp ","(sex_category) ", sep = "")
    dbGetQuery(dbConn, query)
    
    dbDisconnect(dbConn)
    
    return (TRUE)
}