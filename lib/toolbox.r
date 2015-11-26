# R functions for manipulating and analysing mQoL CSV data files.
#
# These functions can be used interactively in the R console or in R scripts.
# In addition, they are used by the shell commands, which in many cases are
# merely wrappers around these R functions.
#
# Daniel Weibel <daniel.weibel@unifr.ch> Feb. - Mar. 2015
#------------------------------------------------------------------------------#

ReadCSV <- function(file, colClasses=NA, nrows=-1) {
  # Read any CSV file to a data frame. Detects automatically if the file is an
  # OpenCellID file and optimises reading.
  #----------------------------------------------------------------------------#
  colnames <- names(read.csv(file, nrows=1))
  if (all(colnames %in% OcidNames())) colClasses <- OcidColClasses(colnames)
  read.csv(file, colClasses=colClasses, nrows=nrows, na.strings=c("NA", ""))
}

WriteCSV <- function(df, file="", col.names=TRUE, append=FALSE) {
  # Write a data frame to a CSV file, including header
  #----------------------------------------------------------------------------#
  # Enclose fields that contain commas in quotes
  for (n in names(df)) {
    v <- as.character(df[[n]])
    df[[n]] <- ifelse(grepl(".*,.*", v), paste0("\"", v, "\""), v)
  }
  # append=TRUE + col.names=TRUE produces a warning
  w <- getOption("warn"); options(warn=-1)
  # In write.csv 'append' and 'col.names' cannot be set, so use write.table
  write.table(df, file=file, sep=",", qmethod="double", quote=FALSE, na="",
              row.names=FALSE, col.names=col.names, append=append)
  options(warn=w)
}

Write <- function(x, file="") {
  # Write any object to a file
  #----------------------------------------------------------------------------#
  write(x, file=file)
}

AddCol <- function(df, after, ...) {
  # Add one or more columns to a data frame after column 'after'
  # Args: df:    data frame
  #       after: index [0,..ncol(df)], or column name
  #       ...:   named vectors to become the new columns
  # Returns: data frame with the added columns
  #----------------------------------------------------------------------------#
  new <- data.frame(...)
  ncol <- ncol(df)
  if (is.character(after)) after <- which(colnames(df) == after)
  if      (after == 0)    cbind(new, df)
  else if (after == ncol) cbind(df, new)
  else                    cbind(df[1:after], new, df[(after+1):ncol]) 
}

RemoveCol <- function(df, cols) {
  # Remove one or more columns from a data frame
  #----------------------------------------------------------------------------#
  df[,!names(df) %in% cols]
}

KeepCol <- function(df, cols) {
  # Remove all but the specified columns from a data frame
  #----------------------------------------------------------------------------#
  df[,cols]
}

HasCol <- function(df, cols) {
  # Return TRUE if data frame has all of the specified columns, FALSE otherwise
  #----------------------------------------------------------------------------#
  all(cols %in% names(df))
}

MergeCells <- function(df1, df2, cols=character()) {
  # Merge two data frames on the "mcc", "mnc", "lac", and "cid" fields. 'cols'
  # are the column names of the right data frame that are to be included in the
  # result. All rows of the left data frame are kept, but not of the right one.
  # Example: add location: MergeCells(cells, ocid, c("lat", "lon"))
  #----------------------------------------------------------------------------#
  gcid <- c("mcc", "mnc", "lac", "cid")
  # Helper column for reconstituting the row order after the merge
  df1$tmp.order <- 1:nrow(df1)
  # Merge (left outer join)
  df3 <- merge(df1, df2[c(gcid, cols)], by=gcid, all.x=TRUE, sort=FALSE)
  # Reorder rows, 'merge' changes the row order. From ?merge -> Value:
  #   "The rows are by default lexicographically sorted on the common columns,
  #    but for ‘sort = FALSE’ are in an unspecified order."
  df3 <- df3[order(df3$tmp.order),]
  df3$tmp.order <- NULL
  df1$tmp.order <- NULL
  # Reorder columns, 'merge' changes the colum order. From ?merge -> Value:
  #   "The columns are the common columns followed by the remaining columns in
  #    ‘x’ and then those in ‘y’."
  df3[c(names(df1), cols)]
}

MergeTs <- function(df1, df2, cols, tol=5000) {
  # Merge two data frames on their "ts" fields. A match doesn not have to be
  # exact, but may deviate by plus/minus 'tol'. If a "ts" field of the left data
  # frame has multiple matches in the right data frame, only the first match
  # is taken. 'cols' are the columns of the right df to include in the result.
  # Example: add RTT: MergeTs(cells, ping, "rtt")
  # --> Caveat: very unefficient!
  #----------------------------------------------------------------------------#
  df3 <- data.frame()
  for (ts1 in df1$ts) {
    df3 <- rbind(df3, df2[df2$ts >= ts1-tol & df2$ts <= ts1+tol, cols, drop=FALSE][1,,drop=FALSE])
  }
  data.frame(df1, df3)
}

CellsClean <- function(df) {
  # Remove rows containing invalid "mcc", "mnc", "lac", or "cid" data
  #----------------------------------------------------------------------------#
  bad <- with(df, cid < 0 | cid >= 2^28 | lac < 0 | lac >= 2^16 | 
                  mnc < 0 | mnc > 999 | mcc < 202 | mcc > 748)
  df[!bad,]
}

CellsSort <- function(df) {
  # Order rows hierarchically by "mcc", "mnc", "lac", "cid"
  #----------------------------------------------------------------------------#
  df[order(df$mcc, df$mnc, df$lac, df$cid),]
}

CellsDistinct <- function(df) {
  # From top to bottom, remove rows with a "mcc", "mnc", "lac", "cid" combi-
  # nation that has already occured before.
  #----------------------------------------------------------------------------#
  unique.rows <- rownames(unique(df[c("mcc", "mnc", "lac", "cid")]))
  df[unique.rows,]
}

CellsRecords <- function(df, mcc, mnc, lac, cid) {
  # Count the occurrences of the 'mcc', 'mnc', 'lac', 'cid' combinations in the
  # data frame 'df'.
  #----------------------------------------------------------------------------#
  f <- integer()
  for (i in 1:Length(mcc, mnc, lac, cid))
    f <- c(f, nrow(df[df$mcc == mcc[i] & df$mnc == mnc[i] & df$lac == lac[i] & df$cid == cid[i],]))
  f
}

AddDate <- function(df) {
  # Add column "date" with date converted from "ts"
  #----------------------------------------------------------------------------#
  AddCol(df, after="ts", date=TsToDate(df$ts))
}

AddTime <- function(df) {
  # Add column "time" with time converted from "ts"
  #----------------------------------------------------------------------------#
  AddCol(df, after="ts", time=TsToTime(df$ts))
}

AddWeekday <- function(df) {
  # Add column "day" with weekday converted from "ts"
  #----------------------------------------------------------------------------#
  AddCol(df, after="ts", day=TsToWeekday(df$ts))
}

AddInterval <- function(df) {
  # Add column "interval" with intervals between timestamps in seconds
  #----------------------------------------------------------------------------#
  AddCol(df, after="ts", interval=c(NA, TsInterInt(df)))
}

AddNetworkType <- function(df) {
  # Add column "nwType" with network type (string) corresopnding to "nwCode"
  #----------------------------------------------------------------------------#
  AddCol(df, after="nwCode", nwType=NetworkType(df$nwCode))
}

LocToKML <- function(df, file="", descr=NULL, snippet=NULL,
  icon=Icon("star"), icon.scale=0.5, name=NULL, show.label=FALSE,
  label.scale=0.5, doc.name=NULL, doc.descr=NULL, doc.snippet=NULL) {
  # Create a KML file from the coordinates in the "lat" and "lon" columns of the
  # passed data frame. Each row will be a point placemark in the KML file. The
  # KML code is either saved to file 'file' or printed to stdout if 'file'="".
  #----------------------------------------------------------------------------#
  if (!HasCol(df, c("lat", "lon")))
    stop("Data frame does not have 'lat' and 'lon' columns")
  if (any(is.na(df$lat)) | any(is.na(df$lon)))
    warning("'lat' and 'lon' columns contain NA values")

  n <- Length(df$lat, df$lon)
  if (length(descr)       == 1) descr       <- rep(descr,       times=n)
  if (length(snippet)     == 1) snippet     <- rep(snippet,     times=n)
  if (length(icon)        == 1) icon        <- rep(icon,        times=n)
  if (length(icon.scale)  == 1) icon.scale  <- rep(icon.scale,  times=n)
  if (length(name)        == 1) name        <- rep(name,        times=n)
  if (length(label.scale) == 1) label.scale <- rep(label.scale, times=n)
  if (length(show.label)  == 1) show.label  <- rep(show.label,  times=n)
  label.scale[!show.label] <- 0

  coordinates <- paste0(df$lon, ",", df$lat)

  x <- X('<?xml version="1.0" encoding="UTF-8"?>')
  x <- X(x, '<kml xmlns="http://www.opengis.net/kml/2.2">')
  x <- X(x, "<Document>", ind=1)
  if (!is.null(doc.name))  x <- X(x, "<name>", doc.name, "</name>", ind=2)
  if (!is.null(doc.descr)) x <- X(x, "<description>", doc.descr, "</description>", ind=2)
  x <- X(x, '<Snippet maxLines="1">', doc.snippet, "</Snippet>" , ind=2)
  for (i in 1:n) {
    x <- X(x, "<Placemark>", ind=2)
      x <- X(x, "<Style>", ind=3)
        x <- X(x, "<IconStyle>", ind=4)
          if (!is.null(icon))       x <- X(x, "<Icon><href>", icon[i], "</href></Icon>", ind=5)
          if (!is.null(icon.scale)) x <- X(x, "<scale>", icon.scale[i], "</scale>", ind=5)
        x <- X(x, "</IconStyle>", ind=4)
        x <- X(x, "<LabelStyle>", ind=4)
          if (!is.null(label.scale)) x <- X(x, "<scale>", label.scale[i], "</scale>", ind=5)
        x <- X(x, "</LabelStyle>", ind=4)
      x <- X(x, "</Style>", ind=3)
      if (!is.null(name))  x <- X(x, "<name>",        name[i], "</name>", ind=3)
      if (!is.null(descr)) x <- X(x, "<description>", descr[i], "</description>", ind=3)
      x <- X(x, '<Snippet maxLines="1">', snippet[i], "</Snippet>" , ind=3)
      x <- X(x, "<Point>", ind=3)
        x <- X(x, "<coordinates>", coordinates[i], "</coordinates>", ind=4)
      x <- X(x, "</Point>", ind=3)
    x <- X(x, "</Placemark>", ind=2)
  }
  x <- X(x, "</Document>", ind=1)
  x <- X(x, "</kml>")

  write(x, file=file)
}

HTMLTable <- function(..., width=NULL) {
  # Return HTML code of a table with one row and two columns for each argument.
  # Each argument is a named vector of size 1. The name will be the content of
  # the first and the value the content of the second column. If a vector has
  # value NULL, no table row is added.
  #----------------------------------------------------------------------------#
  data <- list(...)
  t <- "<![CDATA["
  t <- paste0(t, '<table border="none"')
  if (!is.null(width)) t <- paste0(t, ' width="', width, '"')
  t <- paste0(t, ">")
  for (i in 1:length(data))
    if (!is.null(data[[i]]))
      t <- paste0(t, "<tr><td><b>", names(data)[i], ":</b></td><td>", data[[i]], "</td></tr>")
  t <- paste0(t, "</table>")
  paste0(t, "]]>")
}

OcidSamples <- function(df, steps=c(1, 2, 3, 4, 5, 10, 50, 100)) {
  # Input: OpenCellID data frame. Return another data frame with number and
  # percent of cells that have equal or less than a certain number of samples.
  #----------------------------------------------------------------------------#
  steps <- steps
  res <- data.frame(samples=integer(), percent=double(), number=integer())
  total <- nrow(df)
  for (s in steps) {
    number <- nrow(df[df$samples <= s,])
    percent <- number * 100 / total
    res <- rbind(res, data.frame(samples=s, percent=percent, number=number))
  }
  res
}

OcidRemoveDuplicates <- function(df) {
  # Input: OpenCellID data frame. Remove rows with duplicated cells. From a set
  # of duplicates, keep only the one with the highest number of samples.
  #----------------------------------------------------------------------------#
  gcid <- c("mcc", "mnc", "lac", "cid")
  s <- df[gcid]
  # All rows for which another row with same "mcc", "net", "area", "cell" exists
  d <- df[duplicated(s, fromLast=FALSE) | duplicated(s, fromLast=TRUE),]
  # Sort these rows and keep only first one of a series of identical ones
  d <- d[order(d$mcc, d$mnc, d$lac, d$cid, d$samples),]
  remove <- rownames(d[duplicated(d[gcid], fromLast=TRUE),])
  l <- logical(nrow(df))         # Initialised with FALSE
  l[as.integer(remove)] <- TRUE  # Mark positions to remove with TRUE
  df[!l,]
}



# General utility functions
#------------------------------------------------------------------------------#

Table <- function(x) {
  # Table sorted from most to least frequent values
  #----------------------------------------------------------------------------#
  sort(table(x), decreasing=TRUE)
}

Levels <- function(x) {
  # Show distinct values of any vector
  #----------------------------------------------------------------------------#
  levels(as.factor(x))
}

Grep <- function(df, col, pat) {
  # Get rows of a data frame where column 'col' matches regex pattern 'pat'
  #----------------------------------------------------------------------------#
  df[grepl(pat, df$col),]
}

F <- function(n) {
  # Format number with thousand separator ","
  #----------------------------------------------------------------------------#
  formatC(round(n, 2), big.mark=",")
}

# Transform UNIX timestamp in milliseconds to weekday, date, and time
TsToWeekday <- function(ts) { strftime(TsToPOSIXct(ts), "%a") }
TsToDate    <- function(ts) { strftime(TsToPOSIXct(ts), "%Y-%m-%d") }
TsToTime    <- function(ts) { strftime(TsToPOSIXct(ts), "%H:%M:%S") }
TsToPOSIXct <- function(ts) { as.POSIXct(ts/1000, tz="CET", origin="1970-01-01") }



# Specific interest functions
#------------------------------------------------------------------------------#

# Number and percent of rows with a non-empty location ("lat", "lon")
LocYes        <- function(df) { nrow(LocYesGet(df)) }
LocYesGet     <- function(df) { df[!is.na(df$lat) & !is.na(df$lon),] }
LocYesPercent <- function(df) { 100 * LocYes(df) / nrow(df) }

# Number and percent of rows with an empty (NA) location ("lat", "lon")
LocNo         <- function(df) { nrow(LocNoGet(df)) }
LocNoGet      <- function(df) { df[is.na(df$lat) | is.na(df$lon),] }
LocNoPercent  <- function(df) { 100 * LocNo(df) / nrow(df) }

# Number and percent of rows wit a non-empty location ("lat", "lon"), but the
# location lies outside a bounding box around the corresponding country ("mcc").
LocOut        <- function(df) { nrow(LocOutGet(df)) }
LocOutPercent <- function(df) { 100 * LocOut(df) / nrow(df) }
LocOutGet     <- function(df) {
  l <- LocYesGet(df)
  with(l, l[(mcc == 228 & (lon < 5.8  | lon > 10.5  | lat < 45.8 | lat > 47.8))  |  # Switzerland
            (mcc == 208 & (lon < -4.8 | lon > 8.5   | lat < 42.2 | lat > 51.25)) |  # France
            (mcc == 222 & (lon < 6.6  | lon > 18.7  | lat < 36.5 | lat > 47.2))  |  # Italy
            (mcc == 232 & (lon < 9.5  | lon > 17.25 | lat < 46.3 | lat > 49.1))  |  # Austria
            (mcc == 295 & (lon < 9.45 | lon > 9.65  | lat < 47   | lat > 47.3))  |  # Liechtenstein
            (mcc == 262 & (lon < 5.7  | lon > 23.15 | lat < 47.2 | lat > 55.2))     # Germany
            ,])
}

# Intervals in seconds between the "ts" values of a data frame
TsInter      <- function(df) { diff(df$ts / 1000) }
TsInterInt   <- function(df) { round(TsInter(df)) }
TsInterTable <- function(df) { Table(TsInterInt(df)) }



# Helper functions
# Called by other functions, but not intneded to be called by the user
#------------------------------------------------------------------------------#

OcidNames <- function() {
  # Names of the columns of an OpenCellID CSV file
  #----------------------------------------------------------------------------#
  c("radio", "mcc", "mnc", "lac", "cid", "unit", "lon", "lat", "range",
    "samples", "changeable", "created", "updated", "averageSignal")
}

OcidColClasses <- function(colnames) {
  # Preferred R classes of the columns of an OpenCellID CSV file
  #----------------------------------------------------------------------------#
  m <- c(radio="factor", mcc="numeric", mnc="numeric", lac="numeric",
         cid="numeric", unit="numeric", lon="numeric", lat="numeric",
         range="numeric", samples="numeric", changeable="numeric",
         created="numeric", updated="numeric", averageSignal="numeric")
  colClasses <- character()
  for (n in colnames) colClasses <- c(colClasses, m[n])
  colClasses
}

NetworkType <- function(code) {
  # Return string for numerical code of mobile network type. These codes and
  # types are the ones defined in the TelephonyManager Android API class.
  #----------------------------------------------------------------------------#
  dict <- list("0"="UNKNOWN", "1"="GPRS",  "2"="EDGE", "3"="UMTS", "8"="HSDPA",
               "9"="HSUPA",  "10"="HSPA", "13"="LTE", "15"="HSPA+")
  sapply(code, function(c) { s <- dict[[as.character(c)]]; ifelse(is.null(s), "Code not found", s) })
}

Length <- function(..., omit.null=FALSE) {
  # Return the common length of the vectors passed as arguments. If not all
  # vectors have the same length, rais an error.
  #----------------------------------------------------------------------------#
  vectors <- list(...)
  if (omit.null) vectors <- vectors[!sapply(vectors, is.null)]
  length <- length(vectors[[1]])
  for (v in vectors)
    if (length(v) != length)
      stop("vectors have unequal lengths (", length, " and ", length(v), ")")
  length
}

X <- function(existing, ..., ind=0, nl=TRUE) {
  # Append one or more strings to an existing string (intended for building XML)
  #----------------------------------------------------------------------------#
  spaces <- character()
  if (ind > 0) for (i in 1:ind) spaces <- paste0(spaces, "  ")
  paste0(existing, spaces, ..., ifelse(nl, "\n", ""))
}

Icon <- function(name) {
  # Return URL of an icon file (for a point placemark in a KML file)
  #----------------------------------------------------------------------------#
  paste0("http://gearth.s3.amazonaws.com/", name, ".png")
}
