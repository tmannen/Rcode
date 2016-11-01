#TODO: erista settiin men ja women ja matchaa jne.

preprocess = function() {

    #etunimim contains alternative spellings for names.
    men = readLines(file("data/etunimim.txt", encoding = "latin1"))
    splat = strsplit(men, split="@", fixed=TRUE)
    
    #first element has the right spelling, every other is an alternative spelling
    #normalize all names to the right one. add alt spellings as keys and the right spelling as value
    altnames = new.env() #dictionary
    
    for(i in 1:length(splat)) {
        for(j in splat[[i]]) {
            if(j != "") altnames[[tolower(j)]] = tolower(splat[[i]][1])
        }
    }
    
    women = readLines(file("data/etunimin.txt", encoding = "latin1"))
    splat = strsplit(women, split="@", fixed=TRUE)
    #gsub()?
    for(i in 1:length(splat)) {
        for(j in splat[[i]]) {
            if(j != "") altnames[[tolower(j)]] = tolower(splat[[i]][1])
        }
    }
    
    correct_spelling = function(x) {
        if(is.null(altnames[[x]])) x
        else altnames[[x]]
    }
    
    births[,processed_name := sapply(first_name, correct_spelling, USE.NAMES=FALSE)]
}

gendernames = function() {
    men = readLines(file("data/etunimim.txt", encoding = "latin1"))
    splatmen = strsplit(men, split="@", fixed=TRUE)
    women = readLines(file("data/etunimin.txt", encoding = "latin1"))
    splatwomen = strsplit(women, split="@", fixed=TRUE)
    
    mjaa = data.table(unlist(splatmen))
    mjaa[, gender := 1]
    mjaa = rbindlist(list(mjaa, data.table(unlist(splatwomen))), use.names=TRUE, fill=TRUE)
    mjaa[is.na(mjaa)] <- 0
    mjaa[, V1 := tolower(V1)]
    mjaa[, gender := as.integer(gender)]
    setnames(mjaa, "V1", "name")
    setkey(mjaa, "name")
    return(mjaa)
}

#käytän tässä sitä omaa listaa
altpreprocess = function() {
    gg = data.table(read.table("data/omanimilista.txt", header=TRUE, sep="@", stringsAsFactors=FALSE))
    setkey(gg, "ABBREVIATION")
    #TODO?
}