#data is in 'births', cleaned by the script data_cleaning.r
#enc2utf8("östen") #vain skandinimissä
#browser()?
#otin 200 koska sen jälkeen nimien suosiot ovat liian pieniä
#useat nimet alkavat vasta tietyllä ajalla, esim 1850, ongelma?

parishes = data.table(read.table("data/srk.lst.USE", sep="@", stringsAsFactors = FALSE, fill=TRUE, 
                                 col.names=c("id", "names", "laani/seurakunta", "maakunta", "extra"), 
                                 fileEncoding = "latin1", encoding="UTF-8", quote=NULL))

parishes[, c("finnish_name", "swedish_name") := tstrsplit(names, split="\\$")]
parishes[, c("useless", "laani", "seurakunta") := tstrsplit(laani.seurakunta, split="\\/")]
parishes[, c("useless", "laani.seurakunta", "seurakunta") := NULL]
parishes[, laani := as.integer(laani)]

#match parishes to areas
setkey(parishes, id)
births[,laani := parishes[.(parish_id)]$laani]

#TODO: tsekkaa duplikaatit ja removee ne manuaalisesti?
genders = gendernames()

#'names' contains each name, year, and area and their counts
names = births[, .N, by=c("first_name", "year", "laani")]
names[, name_all_areas_N := sum(N), by=c("year", "first_name")]
names[, laani_all_N := sum(N), by=c("year", "laani")]
names[, laani_unique := length(unique(first_name)), by=c("year", "laani")]
names[, all_N := sum(N), by=year]
names[, unique_N := length(unique(first_name)), by=year]
setnames(names, "N", "laani_name_N")

non_processed_names = births[, .N, by=c("non_processed_name", "year", "laani")]
non_processed_names[, name_all_areas_N := sum(N), by=c("year", "non_processed_name")]
non_processed_names[, laani_all_N := sum(N), by=c("year", "laani")]
non_processed_names[, laani_unique := length(unique(non_processed_name)), by=c("year", "laani")]
non_processed_names[, all_N := sum(N), by=year]
non_processed_names[, unique_N := length(unique(non_processed_name)), by=year]
setnames(non_processed_names, "N", "laani_name_N")

pautocorrsratio = sapply(name_counts[1:30]$first_name, function (x) {
    pacf(get_numbers(x)$ratios, plot=FALSE, lag.max = 250)[["acf"]]
    })

means = rowMeans(pautocorrsratio)
plot(1:length(means), means, type="h")

sine = sin(seq(-2*pi, 2*pi, by=0.05))
plot(seq(-2*pi, 2*pi, by=0.05), sine)
#pacf?