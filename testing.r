# first100 = names[1:100]$first_name
# autocorrspop = sapply(first100, function (x) {
#     acf(all_ns[first_name==enc2utf8(x)][order(year)]$popularity, plot=FALSE, lag.max = 250)[["acf"]]})
# 
# autocorrsratio = sapply(first100, function (x) {
#     acf(all_ns[first_name==enc2utf8(x)][order(year)]$ratio, plot=FALSE, lag.max = 250)[["acf"]]})
# 
# pautocorrspop = sapply(first100, function (x) {
#     pacf(all_ns[first_name==enc2utf8(x)][order(year)]$popularity, plot=FALSE, lag.max = 250)[["acf"]]})
# 
# pautocorrsratio = sapply(first100, function (x) {
#     pacf(all_ns[first_name==enc2utf8(x)][order(year)]$ratio, plot=FALSE, lag.max = 250)[["acf"]]})

#rowmeans = rowMeans(autocorrspop[,1:10])
#acf(all_ns[first_name==enc2utf8("maria")]$popularity)
#acf(all_ns[first_name==enc2utf8("anna")]$popularity)
#acf(all_ns[first_name==enc2utf8("johanna")]$popularity)
#qplot(1:250, rowmeans)

backargmax <- function(x, y, w=1, ...) {
    require(zoo)
    n <- length(y)
    y.smooth <- loess(y ~ x, ...)$fitted
    y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
    delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
    i.max <- which(delta <= 0) + w
    list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

#http://stats.stackexchange.com/questions/36309/how-do-i-find-peaks-in-a-dataset
argmax <- function(x, y, w=1, ...) {
    require(zoo)
    n <- length(y)
    y.smooth <- loess(y ~ x, ...)$fitted
    y.max <- rollapply(zoo(y.smooth), 2*w+1, max, fill="extend", align="center")
    y.mean <- rollapply(zoo(y.smooth), 2*w+1, mean, fill="extend", align="center")
    delta <- y.max - y.smooth
    i.max <- which(delta <= 0)
    peaksize = y.max[i.max] - y.mean[i.max]
    print(max(peaksize)/min(peaksize))
    list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

test <- function(x, y, w, span, name) {
    peaks <- argmax(x, y, w=w, span=span)
    plot(x, y, cex=0.75, col="Gray", xlab="Years", ylab="Suosioluku")
    lines(x, peaks$y.hat,  lwd=2) #$
    y.min <- min(y)
    #sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]),
    #col="Red", lty=2))
    #points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
}



get_numbers = function(name, laani_id=NULL) {
    years = sort(unique(births$year))
    gg = data.table(years)
    setnames(gg, old=c("years"), new=c("year"))
    
    if (!is.null(laani_id)) {
        #merge by year so every year is included, including where name is 0
        all_values = merge(gg, names[first_name==enc2utf8(name)&laani==laani_id], by=c("year"), all=TRUE)
        ratios = all_values$laani_name_N / all_values$laani_all_N
        popularities = ratios / (1/all_values$laani_unique)
        ratios[is.na(ratios)] <- 0
        popularities[is.na(popularities)] <- 0
        #NA to zero
        return (list(years=years, ratios=ratios, popularities=popularities))
    }
    
    all_values = merge(gg, names[first_name==enc2utf8(name)], by=("year"), all=TRUE)
    #we dont care which areas we get since the name_all_areas_N is the same in every area
    #(since it's not dependent on area)
    all_values = all_values[!duplicated(all_values$year)]
    ratios = all_values$name_all_areas_N / all_values$all_N
    popularities = ratios / (1/all_values$unique_N)
    ratios[is.na(ratios)] <- 0
    popularities[is.na(popularities)] <- 0
    
    return (list(years=years, ratios=ratios, popularities=popularities))
}

check_peaks = function(name, laani=NULL, w=10, span=0.15, method="ratios", plot=TRUE) {
    numbers = get_numbers(name, laani)
    if(plot) {
        if (method=="ratios") {test(numbers$years, numbers$ratios, w=w, span=span, name=name)}
        else {test(numbers$years, numbers$popularities, w=w, span=span, name=name)}
    }
    
    else {
        if(method=="ratios") {return (argmax(numbers$years, numbers$ratios, w=w, span=span))}
        else {return (argmax(numbers$years, numbers$popularities, w=w, span=span))}
    }
}

#get_numbers from names that havent been changed from the alternative names
get_non_processed_numbers = function(name, laani_id=NULL) {
    years = sort(unique(births$year))
    gg = data.table(years)
    setnames(gg, old=c("years"), new=c("year"))
    
    if (!is.null(laani_id)) {
        #merge by year so every year is included, including where name is 0
        all_values = merge(gg, non_processed_names[non_processed_name==enc2utf8(name)&laani==laani_id], by=c("year"), all=TRUE)
        ratios = all_values$laani_name_N / all_values$laani_all_N
        popularities = ratios / (1/all_values$laani_unique)
        ratios[is.na(ratios)] <- 0
        popularities[is.na(popularities)] <- 0
        #NA to zero
        return (list(years=years, ratios=ratios, popularities=popularities))
    }
    
    all_values = merge(gg, non_processed_names[non_processed_name==enc2utf8(name)], by=("year"), all=TRUE)
    #we dont care which areas we get since the name_all_areas_N is the same in every area
    #(since it's not dependent on area)
    all_values = all_values[!duplicated(all_values$year)]
    ratios = all_values$name_all_areas_N / all_values$all_N
    popularities = ratios / (1/all_values$unique_N)
    ratios[is.na(ratios)] <- 0
    popularities[is.na(popularities)] <- 0
    
    return (list(years=years, ratios=ratios, popularities=popularities))
}

check_non_processed_peaks = function(name, laani=NULL, w=1, span=0.2, method="ratios") {
    numbers = get_non_processed_numbers(name, laani)
    if (method=="ratios") {test(numbers$years, numbers$ratios, w=w, span=span)}
    else {test(numbers$years, numbers$popularities, w=w, span=span)}
}

find_many_peaks = function(amount=1:15, laani=NULL, w=1, span=0.2, method="ratios") {
    namies = name_counts[order(N, decreasing=TRUE)]$first_name[amount]
    numbers = lapply(namies, check_peaks, laani=laani, method=method, w=w, span=span)
}

autocorrspop = function() {
    sapply(name_counts[1:40]$first_name, function (x) {
    acf(get_numbers(x)$ratios, plot=FALSE, lag.max = 250)[["acf"]]
})
}

