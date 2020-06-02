### This contains handy functions that I use frequently. The included functions are:
# upd                 Initialize lv = .Last.value
# ini                 Opens ~/.odbc.ini file for editting
# rsavvy              Creates a read.odbc function mimicking the savvy function, for use in Citrix
# ept                 (Evaluate Parsed Text) Evaluates an expression in character form
# qu                  Allow for the creation of a character vector without using quotes
# revf                reverse the levels of a factor variable, useful for ggplot bar charts
# replace.na          function for replacing NAs in a vector more easily
# merge.list          Adds or modifies elements to a list based on entry names
# backup              Creates or modifies a .backup variable to contain a copy of passed objects
# revert              Restores all objects stored in .backup
# untable             Undoes what the function table does
# Table               Similar to table function, but adds columns and rows for totals
# vTable              Similar to Venn diagram, counts the number of unique elements contained in each set
# r2                  Calculate R^2
# adjr2               Calculate Adjusted R^2
# ext.chisq.test      Extends the categorical version of chisq.test to more than two dimensions
# matpaste            paste function that maintains matrix form
# dfSummary           Summarizes data.table objects in another table, one row per column
# browse_db           Create a list containing column names for all tables in a database
# snippets            Prints a list of available snippets
# nps                 Calculate the Net Promoter Score (NPS)
# npsind              Convert scores to -1,0,1 indicators (for NPS)
# npsprop             Calculates the proportion of NPS indicators
# moe                 Calculates Margin of Error (for NPS)
# roundNumeric        Rounds the numeric columns of a data.table
# convert.tz          Convert times between time zones
# hardcode            Generate code that creates the given object as is (currently only vectors).
# gg_us_hex           Creates a data.table object for plotting US states in a hex map
# setwd_curr          Sets the working directory to the location of the current file

## Use the following code to update package
# library(devtools)
# detach("package:CMcCode")
# install("c:/users/cmcnei12/documents/code/personal/general code/CMcCode")
# document("c:/users/cmcnei12/documents/code/personal/general code/CMcCode")




#' Update function
#'
#' This function initializes lv as .Last.value.
#' @export
#' @examples
#' upd()
#' 3 + 4
#' lv
#' getwd()
#' lv
 
upd <- function() {
  tmp <- .Last.value
  if(exists("lv"))
  rm(lv, envir=.GlobalEnv)
  makeActiveBinding("lv", function(x) .Last.value, .GlobalEnv)
  invisible(tmp)
}

#' Read Database
#' 
#' Function for reading data from databases.
#' @param db Name of database.
#' @param dbTable Name of table in database.
#' @param dbQuery Character vector of length one containing a query to run against the database.
#' @export
read.odbc <- function (db, dbTable = NULL, dbQuery = NULL, ...) 
{
  con <- RODBC::odbcConnect(db)
  if (is.null(dbTable)) {
    tab <- RODBC::sqlQuery(con, query = dbQuery, ...)
  }
  else {
    tab <- RODBC::sqlQuery(con, paste("SELECT * FROM", dbTable), 
                           ...)
  }
  RODBC::odbcClose(con)
  tab
}




#' Open .odbc.ini file.
#'
#' This function opens the .odbc.ini file which shows the servesr/databases that are available.
#' @param print Boolean. Indicates whether the file should be displayed in the console. Default is FALSE.
#' @export
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
#' @export
#' @examples
#' rsavvy()
#' read.odbc

rsavvy <- function() {
  read.odbc <<- function(db, dbTable = NULL, dbQuery = NULL, ...) {
    con <- RODBC::odbcConnect(db)
    if(is.null(dbTable)) {
      tab <- RODBC::sqlQuery(con, query = dbQuery, ...)
    } else {
      tab <- RODBC::sqlQuery(con, paste("SELECT * FROM", dbTable), ...)
    }
    RODBC::odbcClose(con)
    tab
  }
}


#' Yes/No Converter
#' 
#' Converts 0/1 to Yes/No
#' @param x Vector of 0s and 1s.
#' @export
#' @examples
#' x <- rbinom(100, 1, .4)
#' table(x)
#' table(yn(x))

yn <- function(x, type = c("YN", "PN", "TF", "yn", "pn", "tf"), as.factor = T) {
  type <- match.arg(type)
  if(length(setdiff(x, 0:1)) > 0)
    stop("Only values of 0 and 1 are allowed.")
  newvals <- switch(type, 
                    yn = c("Y", "N"), 
                    YN = c("Yes", "No"), 
                    pn = c("Pos", "Neg"), 
                    PN = c("Positive", "Negative"), 
                    tf = c("T", "F"), 
                    TF = c("True", "False"))
  x = newvals[2 - x]
  if(as.factor)
    x = factor(x, levels = newvals)
  x
}

#' Evaluate Parsed Text
#'
#' Evaluates a string as written code
#' @param txt Text to be evaluated
#' @param env Environment in which to evaluate \code{txt}
#' @param drop If \code{txt} has length 1, whether to return value rather than list. Default is TRUE
#' @param useDT If TRUE, returns multiple results in a \code{data.table} object.
#' @export
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

#' Create Character Vector
#' 
#' Creates a character vector from a vector of unquoted characters.
#' Ignores any value assigned to variables.
#' Does not work for character strings containing spaces.
#' @param vec Vector
#' @export
#' @examples
#' a <- 1
#' c(a, b)     # Gives error
#' # All the same
#' qu(c(a, b))
#' qu(c(a, "b"))
#' qu(c("a", "b"))

qu <- function(vec) {
  as.character(substitute(vec))[-1]
}

#' Dollar format
#' 
#' Uses code from \code{dollar} in \code{scales} package, but adds the option to not have even dollars rounded.
#' @param x Vector of numbers to format
#' @export
#' @examples
#' x <- c(1, 1.5, 150)
#' dollarfull(x)

dollarfull <- function(x) {
  newtxt <- scales::dollar(x)
  newtxt <- gsub("((?<!\\.)..)$", "\\1.00", newtxt, perl = T)
  newtxt
}

#' Reverse factor levels
#'
#' Creates a copy of a factor variable where the order of the levels is reversed. This is useful when 
#' using coord_flip() in ggplot2, puts factors in order top to bottom instead of bottom to top.
#' Does NOT reverse the order of the vector!
#' @param vec Factor vector
#' @export
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
#' @export
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
#' @export
#' @examples
#' dt <- data.table(Gender = c("M", "F", "F"), Response = c("Y", "Y", "N"), Count = 1:3)
#' ggplot(dt, aes(Gender, Count, fill = Response)) + geom_bar(position = "dodge", stat = "identity")
#' 
#' dt2 <- complete.dt(dt, "Gender", "Response")
#' ggplot(dt2, aes(Gender, Count, fill = Response)) + geom_bar(position = "dodge", stat = "identity")

complete.dt <- function(dt, ..., all.levels = T, replaceFUN = NULL) {
  lst <- list(...)
  if(length(lst) < 2)
    stop("Need at least two sets of columns in ...")
  tabs <- lapply(seq_along(lst), function(i) {
    if(length(lst[[i]]) == 1 & class(dt[[lst[[i]]]]) == "factor" & all.levels) {
      tmp <- data.table(levels = factor(levels(dt[[lst[[i]]]]), levels = levels(dt[[lst[[i]]]])))[, ind := 1:.N]
      setnames(tmp, c(lst[[i]], paste0("Ind", i)))
      tmp
    } else
      unique(dt[, lst[[i]], with = F])[, paste0("Ind", i) := 1:.N]
  })
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
#' @export
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
#' @export
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

#' Align lists
#'
#' Creates a table with similarly named items on the same line to compare elements of multiple lists
#' @param ... Lists to be merged. Elements of each list should be named to avoid confusion.
#' @export
#' @examples
#' x <- list(a = 2, b = 13, c = 8)
#' y <- list(b = 4, c = 8, d = 12)
#' align(x, y)
#' merge.list(x, y, priority = "last")

align <- function(...) {
  lsts <- list(...)
  if(is.null(names(lsts)))
    names(lsts) = rep("", length(lsts))
  nms <- ifelse(names(lsts) == "", paste0("Value", 1:length(lsts)), names(lsts))
  lsts <- lapply(seq_along(lsts), function(i) {
    setnames(data.table(names(lsts[[i]]), lsts[[i]]), c("Name", nms[i]))
  })
  res <- lsts[[1]]
  for (i in 2:length(lsts)) {
    res <- merge(res, lsts[[i]], by = "Name", all = T)
  }
  res
}

#' List Grep
#' 
#' Performs \code{grep} on each entry in a list
#' @param pattern, ... Parameters passed to \code{grep}.
#' @param x List of vectors to perform \code{grep} on.
#' @export
#' @examples
#' lst <- list(letters, LETTERS)
#' grep.list("a", lst)
#' grep.list("a", lst, value = T)
#' grep.list("a", lst, ignore.case = T)
#' grep.list("a", lst, value = T, ignore.case = T)
grep.list <- function(pattern, x, ...) {
  res <- lapply(lst, grep, pattern = pattern, ...)
  res <- res[sapply(res, length) > 0]
  res
}

#' Backup function
#'
#' Creates a backup of an object. Useful when tweaking code.
#' @param obj Object to save
#' @export
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
#' @export

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
#' @export
#' @examples
#' x <- c(1, 2, 2, 2, 2, 3, 3, 3)
#' y <- c(1, 1, 2, 2, 3, 3, 3, 4)
#' table(x, y)
#' Table(x, y)
#' Table(x, y, along = 2)
#' Table(x, y, along = 2, prop = T)

Table <- function(..., along = NULL, prop = FALSE) {
  tab <- table(...)
  if(is.null(along))
    along <- 1:length(dim(tab))
  if(length(dim(tab)) == 1) {
    tab <- as.matrix(tab)
    colnames(tab) <- "Count"
  }
  for (i in along) {
    tab <- abind::abind(tab, Total = apply(tab, MARGIN = setdiff(1:length(dim(tab)), i), sum), along = i)
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

#' Untable Function
#' 
#' Undoes what \code{table} does.
#' @param tab Table as returned from \code{table}
#' @export
#' @examples
#' data(Titanic)
#' dim(Titanic)
#' dt1 <- as.data.table(Titanic)
#' dt2 <- untable(Titanic)
untable <- function(tab) {
  newdt <- data.table(as.data.frame(tab))
  newdt <- newdt[, list(Ind = rep(1, Freq)), by = names(dimnames(tab))]
  newdt$Ind <- NULL
  newdt
}

#document("CMcCode")
#install("CMcCode")
#untable(Titanic)

#' Venn Table Function
#'
#' Similar to a Venn diagram. Gives the counts of unique entries that are shared between given inputs.
#' @param ... Vectors to compare
#' @param prop Whether to show proportions instead of counts. Default is FALSE.
#' @param sums Whether to show row and columns sums using \code{Table}. Default is FALSE.
#' @export
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
#' @export
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
#' @export

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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @param server Name/alias of server(s) containing the database
#' @param db Name of database
#' @export

browse_db <- function(server, db = NULL) {
  if(!exists("read.odbc"))
    rsavvy()
  if (is.null(db)) {
    server <- sort(server)
    lst <- lapply(server, function(sv) sort(data.table(read.odbc(sv, dbQuery = "select * from master.dbo.sysdatabases"))$name))
    names(lst) <- server
  } else {
    tabs <- sort(data.table(read.odbc(server, paste0(db, ".information_schema.tables")))$TABLE_NAME)
    lst <- lapply(tabs, function(tab) colnames(read.odbc(server, dbQuery = paste0("select top 1 * from ", db, "..", tab))))
    names(lst) <- tabs
  }
  lst
}

#' Snippets Function
#'
#' Print available snippets. Snippets are little keywords that can be used to write code. Example: in an R script, type ts<SHIFT-TAB>
#' These can be customized in the r.snippets file, accessible via the following function.
#' @param type File type of snippets to be returned. Default is "r", which gives snippets for .R files.
#' @param edit Whether to open the snippets file to edit. Default is \code{FALSE}.
#' @export
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

#' Round \code{data.table}
#'
#' Creates a new \code{data.table} object with numeric fields rounded
#' @param dt \code{data.frame} object to be rounded.
#' @param digits Number of places after the decimal.
#' @export

roundNumeric <- function(dt, digits=1) {
  dt <- as.data.table(lapply(dt, function(x) {
    if(class(x) == "numeric") 
      round(x, digits) 
    else 
      x
  }))
}

#' Net Promoter Score (NPS) Functions
#'
#' Functions for calculating NPS from responses to the net promoter score survey question:
#' "How likely are you to recommend [company/product] to a friend or colleague?"
#' @describeIn nps Converts 0-10 scale to -1, 0, 1 indicators representing detractors, passives and promoters, respectively.
#' @param x Vector of responses, on an 11-point Likert scale (integers 0 to 10).
#' @param as.factor Whether to return indicators as a factor variable. Default is \code{FALSE}
#' @export
#' @examples
#' responses <- c(10, 9, 10, 3, 7, 8, 5, 9, 8, 6)
#' npsind(responses)

nps <- function(x) {
  # Given individual raw scores, calculates group NPS score
  npsprop(x)[["1"]] - npsprop(x)[["-1"]]
}

#' @describeIn nps Returns the margin of error for the NPS. This is a half-width for a confidence interval.
#' @export

npsind <- function(x, as.factor=F) {
  # Converts raw scores to -1,0,1 indicators
  x <- x[!is.na(x)]
  x <- ifelse(x > 6, ifelse(x > 8, 1, 0), -1)
  if(as.factor)
    x <- factor(x, levels=-1:1)
  x
}

#' @describeIn nps Returns the proportion of detractors, passives and pormoters
#' @export

npsprop <- function(x) {
  # Given individual raw scores, calculates the proportion of detractors (-1), passives (0), and promotors (1)
  prop.table(table(npsind(x, as.factor=T)))[c("-1", "0", "1")]
}

#' @describeIn nps Calculates the Net Promoter Score.
#' @export

moe <- function(x) {
  # Calculates the margin of error for the NPS score
  qnorm(.975) * sd(npsind(x)) / sqrt(sum(!is.na(x)))
}

#' Time Zone Converter
#' 
#' Converts time objects of class \code{POSIXct} from one time zone to another.
#' @param x Vector of times to be converted. If a \code{character} vector is supplied it is converted to \code{POSIXct}
#' @param tz New time zone
#' @param oldtz Old time zone. Defaults to "UTC"
#' @export
#' @seealso \code{\link{POSIXct}}, \code{\link{OlsonNames}}
#' @examples
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

#' Hard Code Objects
#' 
#' Generates R code to re-create the given input as is. 
#' Generated code, when run, should create an identical object.
#' Supported object types include \code{numeric}, \code{character}, \code{logical}, \code{factor}, \code{matrix}, \code{data.frame} and \code{list}.
#' (other than \code{data.frame} objects which are converted to \code{data.table} objects).
#' @param x Vector to hard code.
#' @export
#' @examples
#' x <- rnorm(10, 5, 3)
#' hardcode(x)
#' 
#' lets <- letters
#' hardcode(y)
#' 
#' hardcode(mtcars)

hardcode <- function(x, assign = T, assign_oper = "<-", print = T) {
  if(length(x)==0)
    code <- paste0(class(x), ifelse(is.null(x), "", "()"))
  else {
    if(is.matrix(x)) {
      code <- paste("matrix(cbind(", paste(apply(mat, 2, hardcode, assign = F), collapse = ",\n"), "\n),\ndimnames = ", hardcode(dimnames(mat), assign = F), ")", sep = "")
    } else {
      if(is.numeric(x) || is.logical(x)) {
        code <- x
        if(length(x) > 1)
          code <- paste0("c(", paste0(code, collapse = ", "), ")")
      }
      if(is.character(x)) {
        code <- ifelse(is.na(x), NA, paste0('"', x, '"'))
        if(length(x) > 1)
          code <- paste0("c(", paste0(code, collapse = ", "), ")")
      }
    }
    if(is.factor(x)) {
      code <- paste0("factor(", hardcode(as.character(x), assign = F), ", levels = ", hardcode(levels(x), assign = F), ")")
    }
    if(is.data.frame(x)) {
      code <- paste0("data.table(", paste(names(x), "=", sapply(x, hardcode, assign = F), collapse = ",\n"), "\n)")
    } else {
      if (is.list(x)) {
        code <- paste0("list(", paste0(ifelse(names(x) == "", "", paste(names(x), "= ")), sapply(x, hardcode, assign = F), collapse = ",\n"), "\n)")
      }
    }
  }
  
  if(!exists("code")) stop(paste("Cannot create code for object of class", class(x)[1]))
  if(assign)
    code <- paste(substitute(x), assign_oper, code)
  if(print & identical(parent.frame(), globalenv()))
    cat(code)
  invisible(code)
}

#' US Hex Map
#' 
#' Generates a \code{data.table} object with center points and polygon information for graphing US states in a hex map.
#' @export
#' @param dc Logical, whether or not to include a row for Washinton D.C.
#' @examples
#' dt <- data.table(statepop)[, list(StateCd = abbr, pop_2015)]
#' gg_us_hex(dt, aes(fill = pop_2015))

gg_us_hex <- function(data, mapping, dc = T, ...) {
  ## Returns hexagon-shaped polygons to be used in geom_polygon
  require(data.table)
  hexgrid <- data.table(StateCd = state.abb,
                        x_ct = c(15, 1, 6, 12, 4, 7, 22, 19, 17, 16,
                                 1, 4, 12, 14, 10, 10, 13, 11, 23, 17,
                                 21, 15, 9, 13, 11, 5, 9, 5, 22, 20,
                                 7, 19, 18, 7, 16, 9, 3, 18, 23, 17,
                                 8, 14, 10, 8, 20, 16, 3, 15, 11, 6),
                        y_ct = c(3, 9, 4, 4, 4, 5, 6, 5, 1, 2,
                                 1, 6, 6, 6, 6, 4, 5, 3, 9, 5,
                                 7, 7, 7, 3, 5, 7, 5, 5, 8, 6,
                                 3, 7, 4, 7, 6, 3, 5, 6, 7, 3,
                                 6, 4, 2, 4, 8, 4, 7, 5, 7, 6))
  if(dc){
    hexgrid <- rbind(hexgrid, data.table(StateCd = "DC", x_ct = 22, y_ct = 4))
  }
  hexgrid <- hexgrid[, list(x = x_ct + c(0, -1, -1, 0, 1, 1, 0),
                            y = y_ct + c(-2, -1, 1, 2, 1, -1, -2)/3), by = list(StateCd, x_ct, y_ct)]

  data <- merge(data, hexgrid, by = "StateCd", all = T)
  ggplot(data, mapping) +
    geom_polygon(aes(x = x, y = y, group = StateCd)) +
    geom_text(aes(x_ct, y_ct, label = StateCd)) +
    labs(x = "", y = "") +
    theme(axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank()) +
    scale_fill_gradient(low = "wheat", high = "indianred", ...)
}

#' Set working directory to file location
#' 
#' Finds the location of the current code file and sets it as the working directory.
#' The function cannot be run from the console.
#' @export
setwd_curr <- function() {
  dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  if(dir == "")
    cat("setwd_curr() cannot be called from main console.\nCurrent working directory: ", getwd())
  else
    setwd(dir)
}

# End script
 