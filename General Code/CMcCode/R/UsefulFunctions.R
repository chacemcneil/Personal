### This contains handy functions that I use frequently. The included functions are:
# upd                 Modify width of console output and initialize lv = .Last.value
# ini                 Opens ~/.odbc.ini file for editting
# rsavvy              Creates a read.odbc function mimicking the savvy function, for use in Citrix
# ept                 (Evaluate Parsed Text) Evaluates an expression in character form
# revf                reverse the levels of a factor variable, useful for ggplot bar charts
# replace.na          function for replacing NAs in a vector more easily
# merge.list          Adds or modifies elements to a list based on entry names
# backup              Creates or modifies a .backup variable to contain a copy of passed objects
# revert              Restores all objects stored in .backup
# Table               Similar to table function, but adds columns and rows for totals
# vTable              Similar to Venn diagram, counts the number of unique elements contained in each set
# r2                  Calculate R^2
# adjr2               Calculate Adjusted R^2
# ext.chisq.test      Extends the categorical version of chisq.test to more than two dimensions
# matpaste            paste function that maintains matrix form
# dfSummary           Summarizes data.table objects in another table, one row per column
# is.color            Can the input be interpretted as a color
# hex                 Convert a color to hex format, uses col2rgb and rgb
# darken              Darken a color by a given factor
# lighten             Lighten a color by a given factor
# choose.colors       Choose colors from a palette
# locateIndex         Uses locator function to identify an observation (doesn't work with ggplot)
# snippets            Prints a list of available snippets
# roc.dt              Creates a table of sensitivities, specificities and corresponding thresholds
# ggroc               Prints a roc curve using ggplot
# ggroc2              Prints a roc curve using ggplot
# nps                 Calculate the Net Promoter Score (NPS)
# npsind              Convert scores to -1,0,1 indicators (for NPS)
# npsprop             Calculates the proportion of NPS indicators
# moe                 Calculates Margin of Error (for NPS)
# geom_violin2        function to work with ggplot to create asymetric violin plots

# library(abind)
# library(data.table)
# library(ggplot2)
# library(grid)
# library(gridExtra)
# library(pROC)
# library(proto)
# library(RODBC)

#' Update function
#'
#' This function initializes lv as .Last.value and sets the width of any printed output.
#' @param width Sets the width, in characters, of the output in the console window.
#' @examples
#' upd(200)
#' 3 + 4
#' lv
#' getwd()
#' lv
 
upd <- function(width=260) {
  if(exists("lv"))
  rm(lv, envir=.GlobalEnv)
  
  makeActiveBinding("lv", function(x) .Last.value, .GlobalEnv)
  
  options(width=width)
}

#' Open .odbc.ini file.
#'
#' This function opens the .odbc.ini file which shows the servesr/databases that are available.
#' @param print Boolean. Indicates whether the file should be displayed in the console. Default is FALSE.
#' @examples
#' ini()

ini <- function(print=F) {
  if(print)
    cat(paste(readLines("/home/cmcneil/.odbc.ini"), collapse="\n"))
  file.edit("/home/cmcneil/.odbc.ini")
}

#' Add "Savvy" Functions
#'
#' Adds functions that are in the savvy package, which is not accessible outside of Riley1/Riley2
#' @param
#' @examples
#' rsavvy()
#' read.odbc

rsavvy <- function() {
  read.odbc <<- function(db, dbTable = NULL, dbQuery = NULL, ...) {
    con <- odbcConnect(db)
    if(is.null(dbTable)) {
      tab <- sqlQuery(con, query = dbQuery, ...)
    } else {
      tab <- sqlQuery(con, paste("SELECT * FROM", dbTable), ...)
    }
    odbcClose(con)
    tab
  }
}

ssgray   <- rgb(166, 166, 166, max = 255)
ssblue   <- rgb(79, 129, 189, max = 255)
ssyellow <- rgb(254, 190, 1, max = 255)
ssgreen  <- rgb(155, 187, 89, max = 255)
ssred    <- rgb(192, 80, 77, max = 255)
sspurple <- rgb(128, 100, 162, max = 255)
ssteal   <- rgb(75, 172, 198, max = 255)
sscols <- c(ssblue, ssyellow, ssgreen, ssred, sspurple, ssteal, ssgray)

ss_theme <- theme_bw() + theme(panel.border = element_blank(), 
                               axis.line = element_line(ssgray),
                               panel.grid.major.y = element_line("dashed", size = .5, colour = ssgray),  
                               panel.grid.minor.y = element_line("dashed", size = .5, colour = ssgray),  
                               panel.grid.major.x = element_line(size = .5, colour = ssgray))

#' Evaluate Parsed Text
#'
#' Evaluates a string as written code
#' @param txt Text to be evaluated
#' @param env Environment in which to evaluate \code{txt}
#' @param drop If \code{txt} has length 1, whether to return value rather than list. Default is TRUE
#' @param useDT If TRUE, returns multiple results in a \code{data.table} object.
#' @examples
#' str <- "a > 2"
#' a <- 1:5
#' ept(str)
#' ept(str, drop = FALSE)
#' 
#' str <- c(first = "a > 2", second = "a %% 2 == 0")
#' a <- 1:5
#' ept(str)
#' ept(str, drop = FALSE)

ept <- function(txt, env=NULL, drop=T, useDT = T) {
  if(is.null(env))
    env <- parent.frame()
  dt <- lapply(txt, function(str) eval(parse(text=str), envir=env))
  if(useDT)
    dt <- as.data.table(dt)
  if(is.null(names(txt)))
    names(dt) <- txt
  else
    names(dt) <- names(txt)
  if(length(txt)==1 & drop == T)
    dt <- dt[[1]]
  dt
}

#' Reverse factor levels
#'
#' Creates a copy of a factor variable where the order of the levels is reversed. This is useful when 
#' using coord_flip() in ggplot2, puts factors in order top to bottom instead of bottom to top.
#' Does NOT reverse the order of the vector!
#' @param vec Factor vector
#' @examples
#' x <- factor(letters[1:5])
#' levels(x)
#' levels(revf(x))
#'
#' dt <- data.table(grp = x[sample(1:5, size = 200, replace = T)])
#' ggplot(dt, aes(grp)) + geom_barplot() + coord_flip()
#' ggplot(dt, aes(revf(grp))) + geom_barplot() + coord_flip()

revf <- function(vec) {
  vec <- factor(vec, levels = rev(levels(vec)))
  vec
}

#' Replace NA values
#'
#' Replaces NA values in the given input. 
#' @param x Vector with NAs to be replaced
#' @param repl Value used to replace NAs, or a function to calculate a new value. If \code{repl} is not
#' given, default replacements depend on the data type for \code{x}: 0 (numeric), "" (character) or FALSE (logical)
#' @param ... Additional parameters to be passed to \code{repl} if it is a function.
#' @examples
#' x <- c(10, 20, NA, 40, NA, 60, NA, NA)
#' replace.na(x)
#' replace.na(x, repl = -1)
#' replace.na(x, repl = function(x) seq_along(x))

replace.na <- function(x, repl = NULL, ...) {
  isfactor = (class(x) == "factor")
  if(is.null(repl))
    repl <- switch(class(x), logical = FALSE, numeric = 0, integer = 0L, character = "", factor = stop("If x is a factor, repl must be supplied"))
  if(isfactor) {
    xlevels <- levels(x)
    if(!repl %in% xlevels)
      xlevels <- c(xlevels, repl)
    x <- as.character(x)
  }
  if(class(repl) == "function")
    new <- ifelse(is.na(x), repl(x, ...), x)
  else
    new <- ifelse(is.na(x), repl, x)
  if(isfactor)
    new <- factor(new, levels = xlevels)
  new
}

#' Complete \code{data.table} combinations
#'
#' Adds rows to a \code{data.table} object for missing combinations of factor levels.
#' Useful when using \code{geom_bar} in \code{ggplot2}, so that bars are the same width.
#' @param dt \code{data.table} object to modify
#' @param ... \code{list} giving the names of columns to complete. Needs at least two character vectors.
#' @param replaceFUN If supplied, this gives values other than NA to remaining columns.
#' @examples
#' dt <- data.table(Gender = c("M", "F", "F"), Response = c("Y", "Y", "N"), Count = 1:3)
#' ggplot(dt, aes(Gender, Count, fill = Response)) + geom_bar(position = "dodge", stat = "identity")
#' 
#' dt2 <- complete.dt(dt, "Gender", "Response")
#' ggplot(dt2, aes(Gender, Count, fill = Response)) + geom_bar(position = "dodge", stat = "identity")

complete.dt <- function(dt, ..., replaceFUN = NULL) {
  lst <- list(...)
  if(length(lst) < 2)
    stop("Need at least two sets of columns in ...")
  tabs <- lapply(seq_along(lst), function(i) unique(dt[, lst[[i]], with = F])[, paste0("Ind", i) := 1:.N])
  inds <- lapply(seq_along(tabs), function(i) tabs[[i]][[paste0("Ind", i)]])
  names(inds) <- paste0("Ind", 1:length(inds))
  new <- data.table(expand.grid(inds))
  for (i in 1:length(lst)) {
    new <- merge(new, tabs[[i]], by = paste0("Ind", i), all.x = T)
  }
  new[, paste0("Ind", 1:length(inds)) := NULL]
  new <- merge(new, dt, by = unlist(lst), all.x = T)
  if(!is.null(replaceFUN)) {
    other_cols <- setdiff(colnames(dt), unlist(lst))
    new[, (other_cols) := lapply(.SD, replaceFUN), .SDcols = other_cols]
  }
  new
}

#' Count NA values
#'
#' Counts the number entries that are NA.
#' @param x Vector containing NA values.
#' @examples
#' x <- c(1, 2, NA, 4, NA, NA, 7, NA)
#' count.na(x)
#' 
#' sapply(airquality, count.na)

count.na <- function(x) {
  count <- sum(is.na(x))
  count
}

#' Merge lists
#'
#' Creates a new list with entries from multiple lists. Entries with common names are overwritten
#' according to a certain priority.
#' @param ... Lists to be merged. Elements of each list should be named to avoid confusion.
#' @param priority Designates which entry should be kept if lists have elements of the same name.
#' @examples
#' x <- list(a = 1:5, b = LETTERS[1:3])
#' y <- list(b = c("X", "Y", "Z"), c = TRUE)
#' merge.list(x, y)
#' merge.list(x, y, priority = "last")

merge.list <- function (..., priority = c("first", "last")) {
  priority <- match.arg(priority)
  lst <- list(...)
  if(priority == "last")
    lst <- rev(lst)
  if (length(lst) == 1)
    newlst <- lst[[1]]
  else
    newlst <- append(lst[[1]], do.call(merge.list, lst[-1]))
  newlst[!duplicated(names(newlst))]
}

#' Backup function
#'
#' Creates a backup of an object. Useful when tweaking code.
#' @param obj Object to save
#' @seealso \code{\link{revert}}
#' @examples
#' x <- sample(1:5, 50, replace = T)
#' backup(x)
#' x <- jitter(x)
#' plot(x, x)
#' revert(x)

backup <- function(obj) {
  if(!exists(".backup"))
    .backup <<- list()
  .backup[[as.character(substitute(obj))]] <<- obj
}

#' Revert function
#'
#' @describeIn backup Revert to saved version of object.

revert <- function(obj) {
  # Revert to previously backed up version of obj
  if(!exists(".backup"))
    stop("No backup has been created")
  nm <- as.character(substitute(obj))
  if(!nm %in% names(.backup))
    stop("Given object is not contained in backup")
  assign(nm, .backup[[nm]], env = parent.frame())
}


#' Total Table Function
#'
#' Similar to \code{table} but adds row and column totals.
#' @param ... Objects passed to \code{table}
#' @param along which dimensions to calculate totals for.
#' @param prop whether to show the results as proportions. Default is FALSE.
#' @examples
#' x <- c(1, 2, 2, 2, 2, 3, 3, 3)
#' y <- c(1, 1, 2, 2, 3, 3, 3, 4)
#' table(x, y)
#' Table(x, y)
#' Table(x, y, along = 2)
#' Table(x, y, along = 2, prop = T)

Table <- function(..., along = NULL, prop = FALSE) {
  require(abind)
  tab <- table(...)
  if(is.null(along))
    along <- 1:length(dim(tab))
  if(length(dim(tab)) == 1) {
    tab <- as.matrix(tab)
    colnames(tab) <- "Count"
  }
  for (i in along) {
    tab <- abind(tab, Total = apply(tab, MARGIN = setdiff(1:length(dim(tab)), i), sum), along = i)
  }
  if (prop) {
    if (is.null(along))
      tab <- prop.table(tab) * 2^(length(dim(tab)) - (length(dim(tab))==2 & dim(tab)[2]==1))
    else
      tab <- prop.table(tab) * 2^(length(along) - (length(along)==2 & dim(tab)[2]==1))
  }
  names <- names(list(...))
  if (is.null(names))
    names(dimnames(tab)) <- as.character(as.list(substitute(list(...)))[-1L])
  else
    names(dimnames(tab)) <- ifelse(names == "", as.character(as.list(substitute(list(...)))[-1L]), names)
  names(dimnames(tab)) <- NULL
  tab
}

#' Venn Table Function
#'
#' Similar to a Venn diagram. Gives the counts of unique entries that are shared between given inputs.
#' @param ... Vectors to compare
#' @param prop Whether to show proportions instead of counts. Default is FALSE.
#' @param sums Whether to show row and columns sums using \code{Table}. Default is FALSE.
#' @seealso \code{\link{Table}}
#' @examples
#' x <- c(2, 3, 3, 10, 12, 13)
#' y <- 1:6
#' vTable(x, y)
#' vTable(x, y, prop = TRUE)
#' vTable(x, y, sums = TRUE)

vTable <- function(..., prop = FALSE, sums = FALSE) {
  names <- make.unique(as.character(as.list(substitute(list(...)))[-1L]))
  lists <- lapply(list(...), function(x) if(class(x) == "factor") as.character(x) else x)
  tab <- data.table(Values = unique(unlist(lists)))[, (names) := lapply(lists, function(x) ifelse(Values %in% x, "In", "Out"))][, Values := NULL]
  if(sums)
    tab <- do.call(Table, tab)
  else
    tab <- do.call(table, tab)
  if (prop)
    tab <- prop.table(tab) * 2^((length(dim(tab)) - (length(dim(tab))==2 & dim(tab)[2]==1))*sums)
  tab
}

#' R Squared and Adjusted R Squared Functions
#'
#' Calculates the R^2 (or adjusted R^2) statistic of a model. Uses correlation of predicted values to actual values.
#' @param model A model as returned from \code{lm}
#' @examples
#' # From lm documentation:
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' 
#' r2(lm.D9)
#' adjr2(lm.D9)

r2 <- function(model) {
  r <- cor(predict(model), model$fitted + model$residuals)
  r^2
}

#' R Squared and Adjusted R Squared Functions
#'
#' @describeIn r2 Calculates the adjusted R^2 statistic of a model

adjr2 <- function(model) {
  r2 <- r2(model)
  n <- length(model$fitted)
  p <- model$rank - 1
  1- (1-r2)*(n-1)/(n-p-1)
}

#' Mean Squared Error and Root Mean Squared Error Functions
#'
#' Calculates the MSE and RMSE statistics of a model.
#' @param model A model as returned from \code{lm}
#' @examples
#' From \code{\link{lm}} documentation:
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' 
#' mse(lm.D9)
#' rmse(lm.D9)

mse <- function(model, newdata = NULL) {
  if(is.null(newdata)) {
    mse <- mean((model$residuals)^2)
  } else {
    mse <- mean((predict(model, newdata = newdata) - newdata[[as.character(model$formula[[2]])]])^2)
  }
  mse
}
rmse <- function(model, newdata = NULL) {
  sqrt(mse(model = model, newdata = newdata))
}

#' Extended Chi-Square Test
#'
#' Performss a categorical chi-squared test for tables of any size. Similar to \code{chisq.test}
#' @param arr An \code{array} or \code{matrix} object.
#' @seealso \code{\link{chisq.test}}
#' @examples
#' From \code{\link{lm}} documentation:
#' A <- cbind(1:4, (1:4)*2, (1:4)*3)
#' B <- cbind(1:4, (1:4)*2, c(3, 5, 2, 4))

ext.chisq.test <- function (arr) {
  if (length(dim(arr)) < 2)
    stop("arr must have at least two dimensions.")
  expected <- array(apply(arr, 1, sum)[slice.index(arr, 1)], dim = dim(arr))
  for (i in 2:length(dim(arr))) {
    expected <- expected * array(apply(arr, i, sum)[slice.index(arr, i)], dim = dim(arr))
  }
  expected <- expected / sum(arr)^(length(dim(arr)) - 1)
  chisq <- sum((arr - expected)^2/expected)
  df <- prod(dim(arr) - 1)
  p.value <- 1 - pchisq(chisq, df)
  cat(sprintf("Chi-squared statistic: %s\nDegrees of freedom: %s\np-value: %s\n", chisq, df, p.value))
  list(chisq = chisq, df = df, p.value = p.value)
}

# Can't remember what this was intended to be ...
bin <- function() {
  l
}

#' Matrix Paste Function
#'
#' Same as \code{paste} but retains form when applied to a matrix.
#' @param ..., sep, collapse Parameters passed to \code{paste}
#' @param dim Dimension to use if not implied by inputs.
#' @examples
#' mat <- matrix(0, nrow = 3, ncol = 3)
#' paste("Row: ", row(mat), ", ", "Col: ", col(mat), sep = "")
#' matpaste("Row: ", row(mat), ", ", "Col: ", col(mat), sep = "")

matpaste <- function(..., sep = " ", collapse = NULL, dim = NULL) {
  lst <- list(...)
  if(is.null(dim))
    dim <- dim(lst[[min(which(sapply(lst, is.matrix)))]])
  mat <- matrix(paste(..., sep = sep, collapse = collapse), nrow = dim[1], ncol = dim[2])
  mat
}

#' Data Table Summary
#'
#' Creates a table of summary statistics for \code{data.table} objects, each row coresponding to a column.
#' @param ... \code{data.table} objects to be summarized.
#' @param table.names Vector of names to be associated with tables. If not supplied, the object names are used.
#' @param track Logical. Whether to display information while summarizing tables. Default is TRUE.
#' @examples
#' dfSummary(data.table(iris))

dfSummary <- function(..., table.names=NULL, track=T) {
  tabs <- list(...)
  if(is.null(names(tabs))) {
    if(is.null(table.names))
      names(tabs) <- as.list(substitute(list(...)))[-1L]
    else
      names(tabs) <- table.names
  }
  summary <- do.call(rbind, c(lapply(seq_along(tabs), function(i) {
    if(track)
      cat(paste0("\rTable: ", names(tabs)[i], "       "))
    dat <- tabs[[i]]
    column <- data.table(TableName = names(tabs)[i],
                         ColName   = colnames(dat),
                         Class     = sapply(dat, function(x) paste(class(x), collapse=",")),
                         Mode      = sapply(dat, mode),
                         NumNA     = sapply(dat, function(col) sum(is.na(col))),
                         PctNA     = sapply(dat, function(col) mean(is.na(col))),
                         NumUnq    = sapply(dat, function(col) length(unique(col[!is.na(col)]))),
                         Length    = nrow(dat),
                         NumLevels = sapply(dat, function(x) length(levels(x))))
    column[Mode == "numeric" & !grepl("factor", Class),                   Min     := sapply(dat[, ColName, with = F], min, na.rm = T)]
    column[Mode == "numeric" & !grepl("(factor|Date)", Class),            Qrt1    := sapply(dat[, ColName, with = F], quantile, .25, na.rm = T)]
    column[Mode == "numeric" & !grepl("factor", Class),                   Median  := sapply(dat[, ColName, with = F], median, na.rm = T)]
    column[Mode == "numeric" & !grepl("factor", Class),                   Mean    := sapply(dat[, ColName, with = F], mean, na.rm = T)]
    column[Mode == "numeric" & !grepl("(factor|Date)", Class),            Qrt3    := sapply(dat[, ColName, with = F], quantile, .75, na.rm = T)]
    column[Mode == "numeric" & !grepl("factor", Class),                   Max     := sapply(dat[, ColName, with = F], max, na.rm = T)]
    column[Mode == "numeric" & !grepl("(factor|Date|POSIX)", Class),      Sum     := sapply(dat[, ColName, with = F], function(x) sum(as.numeric(x), na.rm = T))]
    column[Mode == "numeric" & !grepl("factor", Class),                   Nonzero := sapply(dat[, ColName, with = F], function(x) sum(x!=0, na.rm = T))]
    column[, Most        := sapply(dat, function(col) names(sort(table(col), decreasing = T))[1])]
    column[, MostCount   := sapply(dat, function(col) sort(table(col), decreasing = T)[1])]
    column[, MostUnique  := sapply(dat, function(col) diff(sort(table(col), decreasing = T)[1:2])!=0) | NumUnq == 1]
    column[, Least       := sapply(dat, function(col) names(sort(table(col)))[1])]
    column[, LeastCount  := sapply(dat, function(col) sort(table(col))[1])]
    column[, LeastUnique := sapply(dat, function(col) diff(sort(table(col))[1:2])!=0) | NumUnq == 1]
    return(column)
  }), fill = T))
  cat("\n")
  return(summary)
}

#' Browse Database
#' 
#' Creates a \code{list} object containing column names for each table in a database.
#' 

browse_db <- function(server, db) {
  tabs <- data.table(read.odbc(server, paste0(db, ".information_schema.tables")))
  lst <- lapply(tabs$TABLE_NAME, function(tab) colnames(read.odbc(server, dbQuery = paste0("select top 1 * from ", db, "..", tab))))
  names(lst) <- tabs$TABLE_NAME
  lst
}

#' Color Checking/Formatting Functions
#'
#' Checks whether character strings can be interpretted as colors. \code{is.color} is adapted from \code{\link{network:is.color}}
#' @param x Vector of \code{character} values to check.
#' @examples
#' cols <- c("red1", "red5", "#34AF0B", "#FFFFFF", "#FFFFFG", "#12345", "#123456", "#1234567", "#12345678")
#' x <- seq_along(cols)
#' plot(x, pch = 19, cex = 4, col = ifelse(is.color(cols), cols, "black"))
#' hex(cols)
#' data.table(String = cols, IsColor = is.color(cols), HexColor = hex(cols))

is.color <- function (x)
{
  xic <- rep(FALSE, length(x))
  #xc <- sapply(x, is.character)
  x <- as.character(x)
  xic <- (x %in% colors()) | ( (nchar(x) %in% c(7, 9)) & 
                               (substr(x, 1, 1) == "#") & 
                               sapply(strsplit(substr(x, 2, nchar(x)), ""), function(x) all(toupper(x) %in% c(0:9, LETTERS[1:6]))) )
  xic[is.na(x)] <- NA
  xic
}
hex <- function(col)  {
  col <- ifelse(is.color(col), col, NA)
  rgb <- col2rgb(col)/255
  hex <- rgb(red=rgb["red",], green=rgb["green",], blue=rgb["blue",], alpha=1)
  hex <- ifelse(is.na(col), NA, hex)
  hex
}

#' Color Modifying Functions
#'
#' Returns a color lighter/darker than the input
#' @param col Vector of colors to modify
#' @param factor Numerical value between 0 and 1 giving how much to modify. 0 is no modification.
#' @examples
#' x <- c("red", "green", "blue", "yellow")
#' plot(seq_along(x), rep(1, length(x)), pch = 19, cex = 4, col = darken(x), ylim = c(1, 4))
#' points(seq_along(x), rep(2, length(x)), pch = 19, cex = 4, col = x)
#' points(seq_along(x), rep(3, length(x)), pch = 19, cex = 4, col = lighten(x, factor = 2/3))
#' points(seq_along(x), rep(4, length(x)), pch = 19, cex = 4, col = lighten(x, factor = 11/12))

darken <- function(col, factor = 1/3) {
  rgb <- col2rgb(col)*(1-factor)/255
  newcol <- rgb(red = rgb["red",], green = rgb["green",], blue = rgb["blue",], alpha = 1)
  newcol
}

#' Color Modifying Functions
#'
#' @describeIn darken

lighten <- function(col, factor = 1/3) {
  rgb <- 1- (1-col2rgb(col)/255)*(1-factor)
  newcol <- rgb(red = rgb["red",], green = rgb["green",], blue = rgb["blue",], alpha = 1)
  newcol
}

#' Color Choosing Function
#'
#' Displays a palette of colors allowing a specified number of them to be selected.
#' @param locate Number of colors to select from the palette
#' @param plot.it Logical. If \code{FALSE}, no palette is plotted and \code{colors}() is printed.
#' @examples
#' x <- c(10, 40, 25, 50, 20, 15)
#' cols <- choose.colors(length(x))
#' barplot(x, col = cols)

choose.colors <- function (locate = 1, plot.it = T)
{
  if(!plot.it)
  {
    return(colors()) # so far, not different from colors()
  } # close on if
  else
  {
    ytop    <- rep(seq(1/26, 1, by=1/26), each=26)[1:657]
    ybottom <- rep(seq(0, 1-1/26, by=1/26), each=26)[1:657]
    xleft   <- rep(seq(0, 1-1/26, by=1/26), times=26)[1:657]
    xright  <- rep(seq(1/26, 1, by=1/26), times=26)[1:657]
    pall    <- round(col2rgb(colors())/256)
    pall    <- colSums(pall) ; pall2 <- character(0)
    pall2[pall>0]   <- "black"
    pall2[pall==0]  <- "white"
    
    par(mar=c(0,0,1,0))
    
    plot.new()
    title(main="Palette of colors()")
    rect(xleft, ybottom, xright, ytop, col=colors())
    text(x=xleft+((1/26)/2)
         , y=ytop-((1/26)/2)
         , labels = 1:657
         , cex=0.55
         , col=pall2)
    
  } # close on else
  if(locate==0) print("Palette of colors()")
  else
  {
    print(paste0("Select ", locate, " color", ifelse(locate == 1, "", "s"), " from the palette."))
	colmat    <- matrix(c(1:657, rep(NA, 26^2-657)), byrow = T, ncol = 26, nrow = 26)
    cols        <- NA
    i        <- NA
    for(i in 1:locate)
    {
      h    <- locator(1)
      if(any(h$x<0, h$y<0, h$x>1, h$y>1)) stop("locator out of bounds!")
      else
      {
        cc        <- floor(h$x/(1/26))+1
        rr        <- floor(h$y/(1/26))+1            
        cols[i]    <- colors()[colmat[rr, cc]]
      } # close on else
    } # close on i
    return(cols)
  } # close on else
} # close on else+function

#' Locate Plotted Point
#'
#' Uses \code{locator} to find the index of a plotted point. Useful for identifying outliers.
#' @param dat \code{data.frame} with plotting coordinates given in columns \code{x} and \code{y}. If none of the columns have those names, the first two columns are used.
#' @param gg Whether to use \code{ggplot}. NOT CURRENTLY WORKING.
#' @param print Whether to print the row(s) corresponding to selected points.
#' @param n How many points to select.
#' @param ... Parameters to be passed to \code{plot}.
#' @examples
#' locateIndex(trees, n = 3, cex = 2)

locateIndex <- function(dat, gg=F, print=T, n=1, ...) {
  require(ggplot2)
  if(sum(!c("x", "y") %in% colnames(dat))) {
    colnames(dat)[1:2] <- c("x", "y")
  }
  if(gg)
    ggplot() + geom_point(data=dat, aes(x, y))
  else
    plot(dat$x, dat$y, ...)
  
  if(gg) { # Not possible?
    gglocator(n)
  }
  else {
    print(paste0("Select ", n, " point", ifelse(n == 1, "", "s"), " from the plot."))
    coord <- data.frame(locator(n))
    ind <- NULL
    for (i in 1:nrow(coord)) {
      dist <- (dat[,1]-coord$x[i])^2 + (dat[,2]-coord$y[i])^2
      ind[i] <- which.min(dist)
    }
  }
  points(dat[ind,], col="red", pch=19)
  if(print)
    print(dat[ind,])
  return(ind)
}

# Not sure what this was intended for ...
plotprops <- function() {
  1
}

#' Snippets Function
#'
#' Print available snippets. Snippets are little keywords that can be used to write code. Example: in an R script, type ts<SHIFT-TAB>
#' These can be customized in the r.snippets file, accessible via the following function.
#' @param type File type of snippets to be returned. Default is "r", which gives snippets for .R files.
#' @param edit Whether to open the snippets file to edit. Default is \code{FALSE}.
#' @examples
#' snippets()

snippets <- function(type = c("r", "markdown", "css", "c_cpp", "html", "java", "javascript", "python", "sql", "stan"), edit = F) {
  type <- match.arg(type)
  filename <- paste0("~/.R/snippets/", type, ".snippets")
  if(file.exists(filename)) {
    snips <- readLines(filename)
    snips <- grep("^snippet .+$", snips, value = T)
    snips <- gsub("snippet ", "", snips)
    if(edit)
      file.edit(filename)
    return(snips)
  }
  else {
    cat(paste0("The file ", type, ".snippets has not been created.\n",
                "Open Global Options... under the Tools menu,\n",
                "  in the Code - Editing section select Edit Snippets...\n",
                "  then modify and save the file of ", type, " snippets.\n",
                "Use snippets() to get snippet names.\n",
                "Use parameter edit=TRUE to view/modify snippets."))
    return(invisible(NULL))
  }
}

#' ROC \code{data.table} Function
#'
#' Creates a \code{data.table} with sensitivity, specificity and threshold values associated with a \code{roc} object.
#' @param rc \code{roc} object to evaluate.
#' @param cost.fn, cost.fp Costs associated with false negatives and false positives. Both default to 1.
#' @param plot If \code{TRUE} (default), the ROC curve is plotted using \code{ggroc}.
#' @seealso \code{\link{ggroc}}
#' @examples
#' data(aSAH)
#' rc <- roc(aSAH$outcome, aSAH$s100b, levels=c("Good", "Poor"))
#' dt <- roc.dt(rc)
#' dt

roc.dt <- function (rc, cost.fn = 1, cost.fp = 1, plot = T) {
  dt <- data.table(Sens   = rc$sensitivities,
                   Spec   = rc$specificities,
                   Thresh = rc$thresholds,
                   AUC    = rc$auc)
  dt[, TP   := sapply(Thresh, function(th) sum(rc$cases    > th))]
  dt[, TN   := sapply(Thresh, function(th) sum(rc$controls < th))]
  dt[, FP   := sapply(Thresh, function(th) sum(rc$controls > th))]
  dt[, FN   := sapply(Thresh, function(th) sum(rc$cases    < th))]
  dt[, Cost := FP * cost.fp + FN * cost.fn]
  if (plot)
    print(ggroc(rc))
  setkeyv(dt, "Sens")
  dt
}

#' \code{ggplot2} ROC Curves
#'
#' Plots a ROC curve using \code{ggplot2}.
#' @param rc \code{roc} object to evaluate.
#' @param labels 
#' @param print.auc Where to print the area under the curve (AUC). By default it is put in the title.
#' @param mark.sens, mark.spec Sensitivites and specificities to highlight with plotted lines.
#' @seealso \code{\link{ggroc}}
#' @examples
#' data(aSAH)
#' rc <- roc(aSAH$outcome, aSAH$s100b, levels=c("Good", "Poor"))
#' ggroc(rc)
#' ggroc(rc, print.auc = "plot")

ggroc <- function(rc, labels = NULL, print.auc = c("title", "plot"), mark.sens = NULL, mark.spec = NULL) {
  print.auc = match.arg(print.auc)
  dat <- data.table(Sensitivity = rc$sensitivities,
                    Specificity = rc$specificities,
                    Threshold   = rc$thresholds )
  setkeyv(dat, "Sensitivity")
  
  numcontrols <- length(rc$controls)
  numcases    <- length(rc$cases)
  
  p <- ggplot()
  if(!is.null(mark.sens))
    p <- p + geom_vline(aes(xintercept = ifelse(is.null(mark.spec), NULL, 1-mark.spec)), linetype = 2, col = "slateblue")
  if(!is.null(mark.spec))
    p <- p + geom_hline(aes(yintercept = mark.sens), linetype=2, col="slateblue")
  p <- p + 
    geom_abline(aes(intercept = 0, slope = 1), col = "gray50") + 
    geom_line(data = dat, aes(1 - Specificity, Sensitivity)) + 
    labs(title = ifelse(print.auc == "title", paste0("AUC: ", round(rc$auc, 3)), ""), x = "Specificity", y = "Sensitivity", yy = "Cases") +
    theme(panel.grid.minor = element_blank(), 
          axis.text = element_text(size = 15), 
          plot.title = element_text(size = 20), 
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20))
  left <- ggplot_build(p)$panel$ranges[[1]]$x.range[1]
  bttm <- ggplot_build(p)$panel$ranges[[1]]$y.range[1]
  if(is.null(left)) {
    left <- ggplot_build(p)$layout$panel_ranges[[1]]$x.range[1]
    bttm <- ggplot_build(p)$layout$panel_ranges[[1]]$y.range[1]
  }
  p <- p + 
    geom_vline(xintercept = pretty(c(0, numcontrols))/numcontrols, col = "gray70", lty = 2) +
    scale_x_continuous(breaks = c(left, (0:5)/5, left, pretty(c(0, numcontrols))/numcontrols),
                       labels = c("", paste0((5:0)/5*100, "%"),
                                  "\nControls            ", paste0("\n", scales::comma(pretty(c(0, numcontrols)))) ),
                       limits = 0:1 ) +
    geom_hline(yintercept = pretty(c(0, numcases))/numcases, col = "gray70", lty = 2) +
    scale_y_continuous(breaks = c(bttm, (0:5)/5, bttm, pretty(c(0, numcases))/numcases),
                       labels = c("", paste0((0:5)/5*100, "%"),
                                  "\nCases       ", paste0(scales::comma(pretty(c(0, numcases))), "           ") ),
                       limits = 0:1 ) + 
    geom_hline(aes(yintercept = 0), size = 1) + 
    geom_vline(aes(xintercept = 0), size = 1)
  if (print.auc == "plot")
    p <- p + geom_text(aes(x = 0.55, y = 0.45, label = paste0("AUC: ", round(rc$auc, 3))), hjust = 0, size = 7)
    #p <- p + geom_text(aes(x = 1, y = 1, label = paste0("AUC: ", round(rc$auc, 3))), hjust = 1, vjust = -.5, size = 7)
  p
}

#' @describeIn ggroc

ggroc2 <- function(..., labels = NULL, print.auc = T, mark.sens = NULL, mark.spec = NULL) {
  rcs <- list(...)
  if(is.null(labels))
    labels = 1:length(rcs)
  names(rcs) <- labels
  dat <- do.call(rbind, lapply(seq_along(rcs), function(i) data.table(Sensitivity = rcs[[i]]$sensitivities,
                                                                      Specificity = rcs[[i]]$specificities,
                                                                      Threshold   = rcs[[i]]$thresholds,
                                                                      Name        = labels[i],
                                                                      AUC         = rcs[[i]]$auc )))
  setkeyv(dat, c("Name", "Sensitivity", "Specificity"))
  #numcontrols <- length(rc$controls)
  #numcases    <- length(rc$cases)
  
  p <- ggplot()
  if(!is.null(mark.sens))
    p <- p + geom_vline(aes(xintercept = ifelse(is.null(mark.spec), NULL, 1-mark.spec)), linetype = 2, col = "slateblue")
  if(!is.null(mark.spec))
    p <- p + geom_hline(aes(yintercept = mark.sens), linetype=2, col="slateblue")
  p <- p + 
    geom_abline(aes(intercept = 0, slope = 1), col = "gray50") + 
    geom_line(data = dat, aes(1 - Specificity, Sensitivity, col = paste(Name, "\nAUC:", round(AUC, 3)))) + 
    labs(x = "\nSpecificity", y = "Sensitivity\n", col = "") +
    theme(panel.grid.minor = element_blank(), 
          axis.text = element_text(size=15), 
          plot.title = element_text(size = 20), 
          axis.title.x = element_text(size = 20), 
          axis.title.y = element_text(size = 20),
          legend.text = element_text(size = 15))
  left <- ggplot_build(p)$panel$ranges[[1]]$x.range[1]
  bttm <- ggplot_build(p)$panel$ranges[[1]]$y.range[1]
  p <- p + 
    geom_hline(aes(yintercept = 0), size = 1) + 
    geom_vline(aes(xintercept = 0), size = 1)
  p
}

#' Kaplan-Meyer Plot for \code{ggplot2}
#' 
#' Creates KM curves for a survival model using \code{ggplot2} functions.
#' @param s Survival model as returned from \code{survfit} in the \code{survival} package.

ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = NULL, lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = F, xlab = 'Time',
                   ylab = 'Survival', main = ''){
  
  library(ggplot2)
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = ''){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)
    
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(col = col, lty = lty.est)
    
    pl <- if(CI == T | CI == 'def') {
      pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci)
    } else (pl)
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      if(is.null(cens.col))
        pl + geom_point(data = dat.cens, aes(y = surv, col = group), shape = cens.shape)
      else
        pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape, col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival', main = '') {
    n <- s$strata
    
    groups <- factor(unlist(strsplit(names
                                     (s$strata), '='))[seq(2, 2*strata, by = 2)])
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0, n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]),
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1))
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(aes(col = group))
    
    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}
    
    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
            to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low, lty = group), col = surv.col)}
    } else {pl}
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      if(is.null(cens.col))
        pl + geom_point(data = dat.cens, aes(y = surv, col = group), shape = cens.shape)
      else
        pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape, col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main)
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  pl
}

#' Round \code{data.table}
#'
#' Creates a new \code{data.table} object with numeric fields rounded
#' @param dt \code{data.frame} object to be rounded.
#' @param digits Number of places after the decimal.

roundNumeric <- function(dt, digits=1) {
  dt <- as.data.table(lapply(dt, function(x) {
    if(class(x) == "numeric") 
      round(x, digits) 
    else 
      x
  }))
}

#' Scale DayYear
#'
#' Adjusts the scale of an axis representing time to show days and years. Used with \code{ggplot2}.

scale_x_dayyear <- function(p) {
  values <- with(p$data, eval(p$mapping[["x"]]))
  days <- 365.25
  left <- ggplot_build(p)$panel$ranges[[1]]$x.range[1]
  p + scale_x_continuous(breaks = c(left, pretty(range(values, na.rm=T)),
                                    left, pretty(range(values/days, na.rm = T))*days ), 
                         labels = c("Days:", pretty(range(values, na.rm = T)),
                                    "\nYears:", paste0("\n", pretty(range(values/days, na.rm = T))) ) ) + 
    geom_vline(data = data.frame(x = pretty(values/days)*days), aes(xintercept = x), linetype = 2, col = "seagreen", size = 1) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(colour = rep(c("black", "seagreen"),
                                                  times = c(length(pretty(range(values, na.rm = T)))+1,
                                                            length(pretty(range(values/days, na.rm = T)))+1 )), size = 15)) +
    p$layers
}


#' Net Promoter Score (NPS) Functions
#'
#' Functions for calculating NPS from responses to the net promoter score survey question.
#' @describeIn nps Converts 0-10 scale to -1, 0, 1 indicators representing detractors, passives and promoters, respectively.
#' @param x Vector of responses, on an 11-point Likert scale (integers 0 to 10).
#' @param as.factor Whether to return indicators as a factor variable. Default is \code{FALSE}
#' @seealso \code{\link{ggroc}}
#' @examples
#' data(aSAH)
#' rc <- roc(aSAH$outcome, aSAH$s100b, levels=c("Good", "Poor"))
#' ggroc(rc)
#' ggroc(rc, print.auc = "plot")

nps <- function(x) {
  # Given individual raw scores, calculates group NPS score
  npsprop(x)[["1"]] - npsprop(x)[["-1"]]
}

#' @describeIn nps Returns the margin of error for the NPS. This is a half-width for a confidence interval.

npsind <- function(x, as.factor=F) {
  # Converts raw scores to -1,0,1 indicators
  x <- x[!is.na(x)]
  x <- ifelse(x > 6, ifelse(x > 8, 1, 0), -1)
  if(as.factor)
    x <- factor(x, levels=-1:1)
  x
}

#' @describeIn nps Returns the proportion of detractors, passives and pormoters

npsprop <- function(x) {
  # Given individual raw scores, calculates the proportion of detractors (-1), passives (0), and promotors (1)
  prop.table(table(npsind(x, as.factor=T)))[c("-1", "0", "1")]
}

#' @describeIn nps Calculates the Net Promoter Score.

moe <- function(x) {
  # Calculates the margin of error for the NPS score
  qnorm(.975) * sd(npsind(x)) / sqrt(sum(!is.na(x)))
}

# Adding to ggplot2

ggen <- environment(ggplot)

GeomViolin2 <- ggproto(GeomViolin)
for(i in names(GeomViolin)) {
  GeomViolin2[[i]] <- GeomViolin[[i]]
}
class(GeomViolin2) <- c("GeomViolin2", "Geom", "ggproto")
GeomViolin2$draw_group <- function (self, data, ..., draw_quantiles = NULL)
{
  data <- transform(data, xminv = x - ifelse(group %%2 == 1, 1, 0)*violinwidth * (x - xmin), 
                    xmaxv = x + ifelse(group %%2 == 0, 1, 0)*violinwidth * (xmax - x),
                    group = 1)
  newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                   plyr::arrange(transform(data, x = xmaxv), -y))
  newdata <- rbind(newdata, newdata[1, ])
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
    quantiles <- create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggen$ggname("geom_violin2", grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggen$ggname("geom_violin2", GeomPolygon$draw_panel(newdata, ...))
  }
}
environment(GeomViolin2) <- ggen

#' Modified Violin Plot
#'
#' Creates a violin plot with two different distributions on each side. Use similar to \code{geom_violin}
#' with a two-level factor variable in the \code{group} or \code{fill} arguments.
#' @examples
#' # geom_violin from ggplot2 package
#' ggplot(survey, aes(Fold, Wr.Hnd, fill = Sex)) + geom_violin()
#' # New geom_violin2 combined graphs
#' ggplot(survey, aes(Fold, Wr.Hnd, fill = Sex)) + geom_violin2()
geom_violin2 <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", 
                          ..., draw_quantiles = NULL, trim = TRUE, scale = "area", 
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  # This function has been manually added to be used with ggplot2.
  layer(data = data, mapping = mapping, stat = stat, geom = GeomViolin2, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, 
                      na.rm = na.rm, ...))
}
environment(geom_violin2) <- ggen


#' Check Predicted Probabilities
#' 
#' Compares the proportion of success to a predicted probability using \code{ggplot2} functions.
#' @param data \code{data.frame} as passed to \code{ggplot}()
#' @param mapping Aesthetic mapping using \code{aes}() as in \code{ggplot}
#' @param bins The number of quantiles to bin predicted probabilities into. Default is 10.
#' @param bound Whether to bound confidence intervals to the range [0, 1]. Default is \code{TRUE}
#' @param stat, position, na.rm, show.legend, inherit.aes, ... Parameters that get passed through to \code{geom_errorbar}.
#' @param pt... \code{list} object with parameters to pass through to \code{geom_point}.
#' @examples
#' esoph_mod <- glm(cbind(ncases, ncontrols) ~ ., data = esoph, family = "binomial")
#' esoph2 <- cbind(esoph, pred_prob = inv.logit(predict(esoph_mod)))
#' ggprop(esoph2, aes(pred_prob, cbind(ncases, ncontrols)))

ggprop <- function(data, mapping, stat = "identity", position = "identity", bins = 10, bound = T, ..., pt... = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  # currently only works with binary input, not two-column counts.
  x <- with(data, eval(mapping$x))
  y <- with(data, eval(mapping$y))
  data <- data.table(data)
  if(is.null(dim(y)) & length(unique(y)) > 2)
    stop("y parameter must be dichotomous are a two-column vector of counts for successes and failures.")
  if(!is.null(dim(y)) & ncol(y) != 2)
    stop("y parameter must be dichotomous are a two-column vector of counts for successes and failures.")
  
  if(!is.null(dim(y))) {
    data <- cbind(data, "_x" = x, setnames(data.table(y), c("_Pos", "_Neg")))
    data[, Index := 1:nrow(data)]
    data <- data[, list(x = `_x`, y = c(rep(1, `_Pos`), rep(0, `_Neg`))), by = setdiff(colnames(data), c("_x", "_Pos", "_Neg"))][, Index := NULL]
  } else {
    if(any(! y %in% 0:1))
      y <- as.numeric(factor(y)) - 1
    data <- data[, x := x][, y := y]
  }
  rm(x, y)
  
  data2 <- data[, {
    lst <- list(Predicted = quantile(x, (1:bins)/bins - .5/bins),
                Observed = tapply(y, quantcut(x, (0:bins)/bins), mean),
                n = tapply(y, quantcut(x, (0:bins)/bins), length) )
    lst
  }, by = eval(as.character(mapping[intersect(c("colour", "group"), names(mapping))])) ]
  data2[, ymin := pmax(Observed - 1.96 * sqrt(Observed*(1-Observed)/n), 0)]
  data2[, ymax := pmin(Observed + 1.96 * sqrt(Observed*(1-Observed)/n), 1)]
  # mapping$x <- substitue
  # mapping$ymin <- substitute(ymin)
  # mapping$ymax <- substitute(ymax)
  mapping <- aes(x = Predicted, y = Observed, ymin = ymin, ymax = ymax)
  p <- ggplot() + do.call(geom_point, c(list(data = data2, mapping = mapping), pt...)) + geom_errorbar(data = data2, mapping = mapping, stat = stat, position = position, ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
  p <- p + geom_line(data = data.table(x = pretty(data2$Predicted), y = pretty(data2$Predicted), ymin = -1, ymax = 1), mapping = aes(x, y), linetype = 2)
  p
}

#' Time Zone Converter
#' 
#' Converts time objects of class \code{POSIXct} from one time zone to another.
#' @param x Vector of times to be converted. If a \code{character} vector is supplied it is converted to \code{POSIXct}
#' @param tz New time zone
#' @param oldtz Old time zone. Defaults to "UTC"
#' @seealso \code{\link{POSIXct}}, \code{\link{OlsonNames}}
#' @example 
#' # Times of vernal equinox
#' x <- c("2004-03-20 06:49:00", "2005-03-20 12:33:00", "2006-03-20 18:26:00", "2007-03-21 00:07:00", "2008-03-20 05:48:00", 
#'        "2009-03-20 11:44:00", "2010-03-20 17:32:00", "2011-03-20 23:21:00", "2012-03-20 05:14:00", "2013-03-20 05:02:00") 
#' (y <- convert.tz(x, tz = "US/Mountain"))
#' data.table(UTC = x, US_Mountain = y)

convert.tz <- function(x, tz, oldtz = "UTC") {
  x <- as.POSIXct(x)
  tz(x) <- "UTC"
  x <- as.POSIXct(format(x, tz = tz), tz = tz)
  x
}


#   grid.align <- function (..., nrow=NULL, ncol=NULL, newpage=T, byrow=T, freecol=T, freerow=T) {
#     ps <- list(...)
#     n  <- length(ps)
#     if(is.null(nrow)) {
#       if(is.null(ncol)) {
#         nrow <- ceiling(sqrt(n))
#         ncol <- ceiling(n/nrow)
#       } else {
#         nrow <- ceiling(n/ncol)
#       }
#     } else {
#       if(is.null(ncol)) {
#         ncol <- ceiling(n/nrow)
#       } else {
#         if (n > ncol*nrow)
#           ps <- ps
#       }
#     }
#     if (newpage)
#       grid.newpage()
#     gs <- lapply(ps, ggplotGrob)
#     
#     # For each row of plots, standardize ylim
#     for (i in 1:nrow) {
#       if (byrow)
#         ind <- which(((1:n)-1) %/% nrow == i-1)
#       else
#         ind <- which(((1:n)-1) %% nrow == i-1)
#       lims <- do.call(cbind, lapply(ps[ind], function(pl) {
#         if (pl$scales$has_scale("y"))
#           lim <- pl$scales$get_scales("y")$limits
#         else {
#           colname <- as.character(pl$layers[[1]]$mapping$y)
#           if (length(colname)==0)
#             colname <- as.character(pl$mapping$y)
#           vals <- pl$data[[colname]]
#           lim <- range(vals[is.finite(vals)])
#         }
#         lim
#       }))
#       miny <- min(lims[1,])
#       maxy <- max(lims[2,])
#       ps[ind] <- lapply(ps[ind], function(pl) {pl <- pl + ylim(c(miny, maxy)); pl})
#     }
#     # For each column of plots, standardize xlim
#     for (i in 1:ncol) {
#       if (byrow)
#         ind <- which(((1:n)-1) %% ncol == i-1)
#       else
#         ind <- which(((1:n)-1) %/% ncol == i-1)
#       lims <- do.call(cbind, lapply(ps[ind], function(pl) {
#         if (pl$scales$has_scale("x"))
#           lim <- pl$scales$get_scales("x")$limits
#         else {
#           colname <- as.character(pl$layers[[1]]$mapping$x)
#           if (length(colname)==0)
#             colname <- as.character(pl$mapping$x)
#           vals <- pl$data[[colname]]
#           lim <- range(vals[is.finite(vals)])
#         }
#         lim
#       }))
#       minx <- min(lims[1,])
#       maxx <- max(lims[2,])
#       ps[ind] <- lapply(ps[ind], function(pl) {pl <- pl + xlim(c(minx, maxx)); pl})
#     }
#     # Standardize point size (?)
#     
#     # For each row of plots, standardize the plot height and vertical position
#     for (i in 1:nrow) {
#       if (byrow)
#         ind <- which(((1:n)-1) %/% nrow == i-1)
#       else
#         ind <- which(((1:n)-1) %% nrow == i-1)
#       maxheight <- do.call(grid::unit.pmax, lapply(ps[ind], function(pl) ggplotGrob(pl)$height[2:5]))
#       gs[ind] <- lapply(gs[ind], function(g) {g$height[2:5] <- maxheight; g})
#     }
#     # For each column of plots, standardize the plot width and horizontal powition
#     for (i in 1:ncol) {
#       if (byrow)
#         ind <- which(((1:n)-1) %% ncol == i-1)
#       else
#         ind <- which(((1:n)-1) %/% ncol == i-1)
#       maxwidth <- do.call(grid::unit.pmax, lapply(ps[ind], function(pl) ggplotGrob(pl)$width[2:5]))
#       gs[ind] <- lapply(gs[ind], function(g) {g$width[2:5] <- maxwidth; g})
#     }
#     
#     g <- do.call(arrangeGrob, c(gs, nrow=nrow, ncol=ncol))
#     grid.draw(g)
#     invisible(g)
#   }
 
 
# End script
 