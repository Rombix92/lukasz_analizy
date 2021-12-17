#########ALL explanation
#http://rstudio-pubs-static.s3.amazonaws.com/288218_117e183e74964557a5da4fc5902fc671.html


#It is best to think of xts objects as normal R matrices, but with special powers. These powers let you manipulate your data as a function of time, as your data is now self-aware of when it exists in time. Before we can start to exploit these powers, it will be helpful to see how xts objects relate to their base-R relatives.

setwd("C:\\Users\\Public\\WORK\\Centrum wiedzy\\Programowanie\\R\\datacamp\\Time Series")

# Load xts
library(xts)
 # View the structure of ex_matrix
str(ex_matrix)
# Extract the 3rd observation of the 2nd column of ex_matrix
ex_matrix[3, 2]
# Extract the 3rd observation of the 2nd column of core 
core[3, 2]

#I can separate this from the xts object using coredata(). The index portion of the xts object is available using the index() function.
hayek<-rnorm(5)
ind<-seq(as.Date("1899-01-01"),length=5,by="days")
hayek<-xts(x=hayek,order.by=ind)

hayek_core<-coredata(hayek)
class(hayek_core)
hayek_index<-index(hayek)
class(hayek_index)


# Create dat by reading tmp_file
tmp_file<-"https://s3.amazonaws.com/assets.datacamp.com/production/course_1127/datasets/tmp_file.csv"
dat <-read.csv(tmp_file)
# Convert dat into xts
xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))
# Read tmp_file using read.zoo
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")
# Convert dat_zoo to xts
dat_xts <- as.xts(dat_zoo)

#There are two main use cases for exporting xts objects. First, you may require an object to persist across sessions for use in later analysis.
saveRDS(dat, "dat")

# Convert sunspots to xts using as.xts().
sunspots_xts <- as.xts(sunspots)
# Get the temporary file name
tmp <- tempfile()
# Write the xts object using zoo to tmp 
write.zoo(sunspots_xts, sep = ",", file = tmp)
# Read the tmp file. FUN = as.yearmon converts strings such as Jan 1749 into a proper time class
sun <- read.zoo(tmp, sep = ",", FUN = as.yearmon)
# Convert sun into xts. Save this as sun_xts
sun_xts <- as.xts(sun)

#ISO-8601
#Date ranges can be extracted from xts objects by simply specifying the period(s) you want using special character strings in your subset.

#A["20090825"]       ## Aug 25, 2009
#A["201203/201212"]  ## Mar to Dec 2012
#A["/201601"]        ## Up to and including Januar

# Select all of 2016 from x
x_2016 <- x["2016/201612"]
# Select January 1, 2016 to March 22, 2016
jan_march <- x["2016/2016-03-22"]
# Verify that jan_march contains 82 rows
82 == length(jan_march)

#On occasion, you may find yourself working with intraday data, which contains both dates and times. In this case it is sometimes necessary to view only a subset of time for each day over multiple days. Using xts, you can slice days easily by using special notation in the i = argument to the single bracket extraction (i.e. [i, j]

# Extract all data from irreg between 8AM and 10AM
morn_2010 <- irreg["T08:00/T10:00"]
# Extract the observations in morn_2010 for January 13th, 2010
morn_2010["2010-01-13"]

# Replace the values in x contained in the dates vector with NA
x[dates] <- NA
# Replace all values in x for dates starting June 9, 2016 with 0
x["2016-06-09/"] <- 0
# Verify that the value in x for June 11, 2016 is now indeed 0
x["2016-06-11"]


#Sometimes you need to locate data by relative time. Something that is easier said than put into code. This is equivalent to requesting the head or tail of a series, but instead of using an absolute offset, you describe a relative position in time. A simple example would be something like the last 3 weeks of a series, or the first day of current month.

#Without a time aware object, this gets quite complicated very quickly. Luckily, xts has the necessary prerequisites built in for you to use with very little learning required. Using the first() and last() functions it is actually quite easy!

#chicago temperatures
url<-"https://assets.datacamp.com/production/repositories/283/datasets/3914fa34c958b2b651d7586126fe8746ada0aaaf/Temps.csv"
temp<-read.csv(url)

# Create lastweek using the last 1 week of temps
lastweek <- last(temps, "1 week")
# Print the last 2 observations in lastweek
last(lastweek, n=2)
# Extract all but the first two days of lastweek
first(lastweek, "-2 days")

# Extract the first three days of the second week of temps
first(last(first(temps, "2 weeks"), "1 week"), "3 days")

# Add a to b, and fill all missing rows of b with 0
a + merge(b, index(a), fill = 0)

# Add a to b and fill NAs with the last observation
a + merge(b, index(a), fill = na.locf)

################JOINING
#merge() takes three key arguments which we will emphasize here. First is the ..., which lets you pass in an arbitrary number of objects to combine. The second argument is join, which specifies how to join the series - accepting arguments such as inner or left. This is similar to a relational database join, only here, the index is what we join on.
#when I use xlt object to merge, merging is done by index