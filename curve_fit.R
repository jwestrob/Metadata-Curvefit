library('argparse')
options(warn=-1)
suppressPackageStartupMessages(library('dplyr'))
options(warn=0)
library('tidyr')
library('ggplot2')

#load argument parser
parser <- ArgumentParser(description="TESTING")

#Specify options
parser$add_argument("-i", "--infile", type="character", nargs=1,
    help="Infile with metadata, comma-separated csv format please.")
parser$add_argument("-o", "--outfile", type="character", nargs=1,
    help="Outfile containing linear fit parameters for metadata.")
parser$add_argument("-p", "--pdfout", type="character", nargs=1,
    help="PDF outfile name for spaghetti plot(s).")
parser$add_argument("-m", "--mode", type="character", nargs=1,
    help="Degree of polynomial curve to fit. Options are: linear, quadratic, cubic")
parser$add_argument("-c", "--columns", type="character", nargs="+", default="ALL",
    help="Names of columns to perform analysis on.")
parser$add_argument("-n", "--disp_numeric", action="store_true", default=FALSE,
    help="Print names of columns containing numeric data, then exit.")
parser$add_argument("-b", "--boxplot", action="store_true", default=FALSE,
    help="Display boxplot of selected columns.")
parser$add_argument("-s", "--summary", action="store_true", default=FALSE,
    help="Print summary of selected columns.")
parser$add_argument("-bin", "--binary", action="store_true", default=FALSE,
    help="Print names of columns containing binary data, then exit.")
parser$add_argument("-nb", "--nonbinary", action="store_true", default=FALSE,
    help="Print names of columns containing nonbinary numeric data, then exit.")
parser$add_argument("-hist", "--histogram", action="store_true", default=FALSE,
    help="Show histogram of IDs. (More functionality to be added later)")

########################################
#              READ DATA               #
########################################

#load arguments as variables (in array "args")
args <- parser$parse_args()

infile <- args$Infile
mode <- args$mode
columns <- args$columns
disp_numeric <- args$disp_numeric
boxplot <- args$boxplot
summary <- args$summary
binary <- args$binary
nonbinary <- args$nonbinary
histogram <- args$histogram
outfile <- args$outfile

if(mode == 'linear'){
  tolerance <- 2
}
if(mode == 'quadratic'){
  tolerance <- 3
}
if(mode == 'cubic'){
  tolerance <- 4
}

if(is.null(args$infile)){
  cat("ERROR: Specify input file name.\n")
  stop()
}

if(is.null(args$outfile)){
  cat("ERROR: Specify output file name.\n")
  stop()
}

if(is.null(args$mode)){
  cat("ERROR: Specify mode for curve fit.\n")
  stop()
}

#Read infile
metadata <- read.csv(args$infile)

########################################
#              FUNCTIONS               #
########################################

show_boxplot <- function(dataframe){
  X11()
  boxplot(dataframe)
  message("Press Return To Continue")
  invisible(readLines("stdin", n=1))
}

show_hist <- function(column, stepsize){
  X11()
  #Display histogram; maybe I'll add stepsize as a parameter later
  hist(column, breaks=seq(1, max(column), by=stepsize))
  message("Press Return To Continue")
  invisible(readLines("stdin", n=1))
}

ID_filter <- function(dataframe){
  #Keep only rows with IDs that occur number of times specified by tolerance flag (default 2; can't fit a curve to a point)
  new_md <- dataframe[dataframe$ID %in% names(table(dataframe$ID))[table(dataframe$ID) >= tolerance],]
}

########################################
#              MAIN SCRIPT             #
########################################

#####################
# VARIABLE DISPLAYS #
#####################

if(nonbinary){
  #Select all columns containing numeric values
  numeric <- metadata %>% select_if(function(col) all(is.numeric(col)))
  #Remove all columns containing only binary values
  nonbinary <- numeric %>% select_if(function(col) !all(na.omit(col) %in% 0:1))
  cat("\nNonbinary numeric columns: \n \n")
  print(names(nonbinary))
  if(args$boxplot){show_boxplot(nonbinary)}
}

if(disp_numeric){
  #Print names of all columns in metadata which contain numeric variables
  cat("\nNumeric columns: \n \n")
  print(names(metadata %>% select_if(function(col) all(is.numeric(col)))))
  if(args$boxplot){show_boxplot(metadata %>% select_if(function(col) all(is.numeric(col))))}
}

if(binary){
  #Selects all columns containing only binary data
  cat("\nBinary columns: \n \n")
  print(names(metadata %>% select_if(function(col) all(na.omit(col) %in% 0:1))))
  if(args$boxplot){show_boxplot(metadata %>% select_if(function(col) all(na.omit(col) %in% 0:1)))}
}

#Quit after displaying column names with particular attributes
if(disp_numeric || binary || nonbinary){stop()}

####################################################
#       Reduce dataframes to selected columns      #
####################################################

if(columns[[1]] == "ALL"){
  print("No columns specified. Proceeding with analysis on all continuous numerical columns.")
  #Select columns from metadata which contain numeric variables and no NAs
  reduced_meta_unfiltered <- metadata %>% select_if(function(col) all(is.numeric(col)))
  reduced_meta <- ID_filter(reduced_meta_unfiltered)
  if(boxplot){
    show_boxplot(reduced_meta)
  }
  if(summary){
    print(summary(reduced_meta))
  }
  if(histogram){
    show_hist(reduced_meta$ID)
  }

} else{
  #Select only specified columns and ID column
  reduced_meta_wnans_unfiltered <- metadata[c("ID",args$columns)]
  reduced_meta_wnans <- ID_filter(reduced_meta_wnans_unfiltered)
  #Remove rows with one column containing NA
  reduced_meta <- reduced_meta_wnans %>% drop_na()
  if(boxplot){
    show_boxplot(reduced_meta)
  }
  if(histogram){
    show_hist(reduced_meta$ID)
  }
  if(summary){
    print(summary(reduced_meta))
  }
}

####################################################
#        Separate out IDs for Curve Fitting        #
####################################################

#List of relevant IDs
ID_list <- unique(reduced_meta$ID)

############################################
#               function zoo               #
############################################

curve_fit <- function(df_column){
  if(mode == "linear"){
    if(any(is.na(df_column))){
      #Don't do any analysis if at least one row of column contains an NA field
      return(list(NA, NA))
    }
    range <- seq(1, length(df_column), by=1)
    #Perform linear least-squares regression
    curve <- lm(df_column ~ range)
    #Make list of coefficients (stored as list of lists)
    comp_coeff <- list(curve$coefficients[[1]], curve$coefficients[[2]])
    #Undo list comprehension
    coeff <- unlist(comp_coeff)
  }else{
    cat("Proper mode for curve_fit not selected. Seek answers from within.\n")
    stop()
  }
  return(coeff)
}

ID_processor <- function(ID, ID_DF, header){
  #Operates sequentially on IDs; sends one set of data belonging to a particular ID to the curve_fit function

  #Only operate on one ID per pass

  ID_red_DF <- ID_DF[ID_DF$ID == ID,]

  #Create reduced dataframe consisting of only ID column; add columns progressively with fit parameters
  new_df <- ID_red_DF$ID

  #Create dataframe without ID column to pass to curve fit
  sans_ID <- subset(ID_red_DF, select=-ID)

  #Declare empty dataframe
  new_row <- data.frame(matrix(ncol=(2*ncol(sans_ID) + 1), nrow=1))
  colnames(new_row) <- header

  #Generate row with curve fit parameter data
  ID_row_badheader <- lapply(sans_ID, FUN=curve_fit)
  new_row$ID <- ID
  new_row[1,2:length(new_row)] <- unlist(ID_row_badheader)
}

header_name_generator <- function(name, header){
  if(mode == "linear"){
    a_header = paste(name, "_a",sep="")
    b_header = paste(name, "_b",sep="")
    combined = c(a_header, b_header)
    return(c(header, combined))
  }
  if(mode == "quadratic"){
    cat("Functionality does not yet exist. Slow down.\n")
    stop()
  }
  if(mode == "cubic"){
    cat("Functionality does not yet exist. Slow down.\n")
    stop()
  }
}

param_df_constructor <- function(reduced_meta, ID_list){

  #Create list of column names not including ID
  non_id_colnames <- unique(names(subset(reduced_meta, select=-ID)))

  #Create labels for parameters
  ID <- c("ID")
  header <- c()
  header <- lapply(non_id_colnames, header_name_generator, header=header)

  #Sorry for mutating data
  header <- c(ID,header)
  header <- unlist(header)

  #Pass data and ID_list to downstream functions to create curve-fit dataframe
  #NOT READY YET
  df_without_header <- lapply(ID_list, FUN=ID_processor, ID_DF=reduced_meta, header=header)
  df_without_header <- do.call("rbind", df_without_header)
  param_df <- cbind(ID_list, df_without_header)
  colnames(param_df) <- header
  return(param_df)
}

spaghetti <- function(param_df){
  #Need date/time. Try making a scatterplot first, then worry about the lines.
  return
}

#Create DF with rows corresponding to IDs and columns containing curve parameters
param_df <- param_df_constructor(reduced_meta, ID_list)
cat("Writing csv to: ")
print(outfile)
write.csv(param_df, outfile)
cat("Constructing spaghetti plot.\n")
stop()
#NOT READY YET
spaghetti(param_df)



print("Execution complete; pending addition of .pdf construction subroutine.")