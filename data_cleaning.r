library(RSQLite)
library(data.table)
library(ggplot2)

#get preprocess function from other file (alternative names)
#should alternative names be even used?

source('code/data_preprocessing.r')
set.seed(1)
con = dbConnect(RSQLite::SQLite(), dbname="data/birth_records.sqlite3")

births = dbGetQuery(con, 'select * from births')

dbDisconnect(con)
rm(con)

#fix encoding issues, scandic letters look odd on Windows
for (i in 1:length(births)) {
    if (class(births[[i]]) == "character") {
        Encoding(births[[i]]) <- "UTF-8"
    }
}

births = data.table(births)
original.births = copy(births)

#save old year and set year so that we use year on default but if it's invalid (under zero), we use byear
births[, old_year := year]
births[year<=0 & byear>1649, year :=  byear]

#remove bad rows. years before 1656 have under 100 samples and years after 1907
#are in progress

births = births[year>1656&year<1907]

#we first split by \K and add columns for names and extra info
births[, c("real_names", "res1", "res2") := tstrsplit(name, "\\K", fixed=TRUE)]

#we then split the names to three first ones. we do this in different place than the last one
#because the residuals arent that important and we dont want to split them by " ".
#Some names have more than 3 words and they get truncated. more than 3 names is very rare.

births[, c("first_name", "second_name", "third_name") := tstrsplit(real_names, split=" +")]

#empty data as NA, remove rows with NA first name. make names lower case
births[births==""] <- NA
births=births[!is.na(first_name)]
births[, first_name := tolower(first_name)]
#setkey(births, "year")

#these are used as abbreviations in some rows, remove them so we can match in alternative
#names. pitäskö poistaa anna-kaisa jne. ? tai muuttaa annaksi?

#remove columns we dont need
births[, c("mom_last_name", "mom_first_name", "mom_patronymic", "dad_first_name", 
           "dad_last_name", "dad_profession", "dad_patronymic", "res1", "res2",
           "mom_age", "event_id", "id", "house", "mom_profession") := NULL]

#grepl tyhjää pikemminkin, paljon nopeempaa? remove 1-length names
births = births[nchar(first_name) > 1]
preprocess()
births[, processed_name := gsub(":|\\.|,|\\(|\\)|\\+|\\=", "", processed_name)]

births = births[births[,.I[.N>1], by=processed_name]$V1]
births[, non_processed_name := first_name]
births[, first_name := processed_name]
births[, processed_name := NULL]
births[, non_processed_name := gsub(":|\\.|,|\\(|\\)|\\+|\\=", "", non_processed_name)]

#only rows with a name that appears more than once are included
births = births[nchar(first_name) > 1]

name_counts = births[, .N, by=first_name]
name_counts = name_counts[order(-N)]
non_processed_name_counts = births[, .N, by=non_processed_name]

