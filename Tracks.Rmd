library(sp)
library(spacetime)
# Geolife trajectories,
# documentation: http://research.microsoft.com/apps/pubs/?id=152176 :
# data: http://research.microsoft.com/en-us/projects/geolife/
# or http://ftp.research.microsoft.com/downloads/b16d359d-d164-469e-9fd4-daa38f2b2e13/Geolife%20Trajectories%201.2.zip
setwd("/home/edzer/Downloads/Geolife Trajectories 1.3/Data/")
# TrackCollection:

Track = setClass("Track",
  contains = "STIDF", # locations, times and attribute data about the points
  representation(connections = "data.frame"), # attribute data BETWEEN points: speed, direction etc.
  validity = function(object) {
	stopifnot(nrow(object@connections) + 1 == nrow(object@data))
    return(TRUE)
  }
)

CreateTrack = function(obj, df = NULL) { # computes segment lenghts
	cc = coordinates(obj@sp)
	ll = is.projected(obj@sp)
	if (is.na(ll))
		ll = FALSE
	L = LineLength(cc, sum = FALSE, longlat = ll)
	if (is.null(df))
		df = data.frame(lengths = lengths)
	else {
		stopifnot(nrow(df) == length(L))
		df$lengths = L
	}
	df$speed = df$lengths / as.numeric(diff(index(obj@time)))
	Track(obj, connections = df)
}

Tracks = setClass("Tracks", # a collection of Track objects for single ID (car, person etc)
	representation(tracks = "list", tracksData = "data.frame"),
	validity = function(object) {
		stopifnot(all(sapply(object@tracks, function(x) is(x, "Track"))))
		stopifnot(nrow(object@tracksData) == length(object@tracks))
		stopifnot(length(object@tracks) > 0)
		return(TRUE)
	}
)

TracksCollection = setClass("TracksCollection", # collection of Tracks for several IDs 
	representation(tracksCollection = "list", tracksCollectionData = "data.frame"),
	validity = function(object) {
		stopifnot(all(sapply(object@tracksCollection, class) == "Tracks"))
		stopifnot(length(object@tracksCollection) == 
			nrow(object@tracksCollectionData))
		stopifnot(length(object@tracksCollection) > 0)
		names = names(object@tracksCollection)
		stopifnot(!(is.null(names) || any(is.na(names))))
		return(TRUE)
	}
)

IDS = c("079", "095", "111", "127", "143", "159", "175")

#sel = 1:2
#sel = TRUE
i = j = 1
#dirs = list.files("Data")[sel]
crs = CRS("+proj=longlat +datum=WGS84")
pb = txtProgressBar(style = 3, max = length(IDS))
elev = numeric(0)
lst0 = list()
for (d in IDS) {
	dir = paste(d, "Trajectory", sep = "/")
	#print(dir)
	lst = list()
	files = list.files(dir, pattern = "*plt", full.names = TRUE)
	i = 1
	for (f in files) {
		tab = read.csv(f, skip = 6, stringsAsFactors=FALSE)
		tab$time = as.POSIXct(paste(tab[,6],tab[,7]))
		tab[tab[,4] == -777, 4] = NA # altitude 
		tab = tab[,-c(3,5,6,7)]
		names(tab) = c("lat", "long", "elev", "time")
		if (all(tab$lat > -90 & tab$lat < 90 & tab$long < 360 
				& tab$long > -180)) {
			stidf = STIDF(SpatialPoints(tab[,2:1], crs), tab$time, tab)
			conn = tab[-1,"elev",drop=FALSE]
			lst[[i]] = CreateTrack(stidf, conn)
			i = i+1
		}
	}
	names(lst) = f
	n = sapply(lst, length)
	lst0[[j]] = Tracks(tracks = lst, tracksData = data.frame(length = n))
	setTxtProgressBar(pb, j)
	j = j+1
}
names(lst0) = IDS
df = data.frame(IDS=IDS)
TR = TracksCollection(tracksCollection = lst0, tracksCollectionData = df)
object.size(TR)

setAs("Track", "data.frame", function(from)
	as(as(from, "STIDF"), "data.frame")
)
setAs("Tracks", "data.frame", function(from)
	do.call(rbind, lapply(from@tracks, function(x) rbind(as(x, "data.frame"), NA)))
)
setAs("TracksCollection", "data.frame", function(from) {
		l = lapply(from@tracksCollection, function(x) rbind(as(x, "data.frame"), NA))
		n = sapply(l, nrow)
		ret = do.call(rbind, l)
		data.frame(ret, IDs = rep(names(from@tracksCollection), times = n))
	}
)
#as(TR@tracksCollection[[1]]@tracks[[1]], "data.frame")
#as(TR@tracksCollection[[1]], "data.frame")
df = as(TR, "data.frame")
# focus on Bejing:
setMethod("plot", "TracksCollection", function(x, ..., type = 'l', colorBy = "IDs") {
		df = as(x, "data.frame")
		if (colorBy == "IDs")
			col = as.numeric(as.factor(df$IDs))
		# print(col)
		plot(lat ~ long, df, asp = 1, type = type, col = col, ...)
	}
)
#plot(TR, xlim=c(116.3,116.5),ylim=c(39.8,40))
plot(TR)
library(lattice)
#xyplot(lat~long, df, type='l',xlim=c(116,116.5),ylim=c(39.5,40.5),groups=IDs,col=1:20)
#setMethod("stplot", "TracksCollection", function(obj, ..., mode = "byID") {
#		xyplot(lat~long, as(obj, "data.frame"), asp = "iso", type='l', 
#			groups = IDs, ...)
#	}
#)
#stplot(TR, xlim=c(116.3,116.5),ylim=c(39.8,40), col = 1:20)
