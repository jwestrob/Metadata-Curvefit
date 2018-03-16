if (!require("pacman")) install.packages("pacman")
pacman::p_load(argparse, dplyr, tidyr, ggplot2, gplots)


#load argument parser
parser <- ArgumentParser(description="Create a set of polynomial features out of numerical data in tabular format (csv please). Specify the degree of the polynomial you want (-m), or optionally the columns you want to operate on (-c).")

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
    help="OPTIONAL: Names of columns to perform analysis on.")
parser$add_argument("-n", "--disp_numeric", action="store_true", default=FALSE,
    help="OPTIONAL: Print names of columns containing numeric data, then exit.")
parser$add_argument("-b", "--boxplot", action="store_true", default=FALSE,
    help="OPTIONAL: Display boxplot of selected columns.")
parser$add_argument("-s", "--summary", action="store_true", default=FALSE,
    help="OPTIONAL: Print summary of selected columns.")
parser$add_argument("-bin", "--binary", action="store_true", default=FALSE,
    help="OPTIONAL: Print names of columns containing binary data, then exit.")
parser$add_argument("-nb", "--nonbinary", action="store_true", default=FALSE,
    help="OPTIONAL: Print names of columns containing nonbinary numeric data, then exit.")
parser$add_argument("-hist", "--histogram", action="store_true", default=FALSE,
    help="OPTIONAL: Show histogram of IDs. (More functionality to be added later)")
parser$add_argument("-gm", "--graph_mode", action="store_true", default=FALSE,
    help="OPTIONAL: Use ggplot2 R package to plot fit lines.")

########################################
#              READ DATA               #
########################################

#load arguments as variables (in array "args")
args <- parser$parse_args()

infile <- args$infile
mode <- args$mode
columns <- args$columns
disp_numeric <- args$disp_numeric
boxplot <- args$boxplot
summary <- args$summary
binary <- args$binary
nonbinary <- args$nonbinary
histogram <- args$histogram
outfile <- args$outfile
pdfout <- args$pdfout
graph_mode <- args$graph_mode

if(mode == 'linear'){
  tolerance <- 2
}
if(mode == 'quadratic'){
  tolerance <- 3
}
if(mode == 'cubic'){
  tolerance <- 4
}

if(is.null(infile)){
  cat("ERROR: Specify input file name.\n")
  stop()
}

if(is.null(outfile)){
  cat("ERROR: Specify output file name.\n")
  stop()
}

if(is.null(mode)){
  cat("ERROR: Specify mode for curve fit.\n")
  stop()
}

#Read infile
metadata <- read.csv(infile)

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

scatter <- function(x, y, df){
  X11()
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  print(cbbPalette[df$ID])
  plot(x, y, col=cbbPalette[df$ID])
  message("Press Return To Continue")
  invisible(readLines("stdin", n=1))
}

ID_filter <- function(dataframe){
  #Keep only rows with IDs that occur number of times specified by degree of desired polynomial fit
  #Ex.: If you have two points there's no reason to fit a parabola to them. Same with three points and a cubic.
  new_md <- dataframe[dataframe$ID %in% names(table(dataframe$ID))[table(dataframe$ID) >= tolerance],]
}

color_constructor <- function(ID_col, Palette){
  color_range <- vector("list", length=max(unique(ID_col)))
  #color_map <- function(x, Palette, list){list[[x]] <- Palette[[(x %% length(Palette)) + 1]]}
  for(i in 1:max(unique(ID_col))){
    if(i %in% unique(ID_col)){
      color_range[[i]] <- Palette[[(i %% length(Palette)) + 1]]
    }else{
      color_range[[i]] <- "#000000"
    }
  }
  #color_range <- lapply(unique(ID_col), FUN=color_map, Palette=Palette, list=color_range)
  names(color_range) <- seq(1, max(unique(ID_col)))
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
  if(boxplot){show_boxplot(nonbinary)}
}

if(disp_numeric){
  #Print names of all columns in metadata which contain numeric variables
  cat("\nNumeric columns: \n \n")
  print(names(metadata %>% select_if(function(col) all(is.numeric(col)))))
  if(boxplot){show_boxplot(metadata %>% select_if(function(col) all(is.numeric(col))))}
}

if(binary){
  #Selects all columns containing only binary data
  cat("\nBinary columns: \n \n")
  print(names(metadata %>% select_if(function(col) all(na.omit(col) %in% 0:1))))
  if(boxplot){show_boxplot(metadata %>% select_if(function(col) all(na.omit(col) %in% 0:1)))}
}

#Quit after displaying column names with particular attributes
if(disp_numeric || binary || nonbinary){stop()}

####################################################
#       Reduce dataframes to selected columns      #
####################################################

if(columns[[1]] == "ALL"){
  cat("No columns specified. Proceeding with analysis on all continuous numerical columns.")

  #Store dates separately
  ID_and_dates <- metadata[c("ID", "Visit_DT")]

  #Select columns from metadata which contain numeric variables and no NAs
  numeric_without_dates <- metadata %>% select_if(function(col) all(is.numeric(col)))

  #Stick the dates column between the ID column and everything else
  reduced_meta_unfiltered <- cbind(ID_and_dates, subset(numeric_without_dates, select=-ID))

  #Filter out rows with inadequate n_samples
  reduced_meta <- ID_filter(reduced_meta_unfiltered)

  #Remove redundant information; no need for visit IDs
  #What to do about this in the future?
  reduced_meta$Visit <- NULL
  reduced_meta$ID_visit <- NULL

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
  reduced_meta_wnans_unfiltered <- metadata[c("ID", "Visit_DT", columns)]
  #Remove rows corresponding to IDs with insufficient n_samples
  reduced_meta <- ID_filter(reduced_meta_wnans_unfiltered)

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

#Create date lookup table
date.lookup <- format(seq(as.Date("1980-01-01"), as.Date("2025-01-01"), by="1 day"))

############################################
#               function zoo               #
############################################

normalized_date_column_constructor <- function(param_df){
  normalizer<-function(ID, ID_DF){
    #Takes a series of dates and creates a list; day of first visit is day 0
    ID_red_DF <- ID_DF[ID_DF$ID == ID,]
    dates = ID_red_DF$int_date
    min_date = min(dates)
    norm.dates = dates - min_date
  }
  #Map normalizer function to dataframe to produce normalized date column
  norm.dates <- unlist(lapply(ID_list, FUN=normalizer, ID_DF=param_df))
  #Add normalized date column to dataframe
  param_df$norm.dates <- norm.dates
  return(param_df)
}

date_range_constructor <- function(date){
  #Convert date to Date class
  form_date <- as.Date(date, format="%m/%d/%y")
  #Return integer value from lookup table; must convert date class to character prior to lookup
  return(match(as.character(form_date), date.lookup))
}

curve_fit <- function(df_column, dates){
  if(mode == "linear"){
    if(any(is.na(df_column))){
      #If there aren't at least two elements, return NA
      if(length(df_column) - sum(is.na(df_column)) < 2){
        return(list(NA, NA))
      }else{
        #Save copy of df_column bc i'm a scrub and i'm mutating data
        df_column_orig <- df_column
        #Otherwise, eliminate the row(s) containing NA and perform regression.
        df_column <- df_column[!is.na(df_column)]
        dates <- dates[!is.na(df_column_orig)]
      }
    }


    #Perform linear least-squares regression
    curve <- lm(df_column ~ dates)

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
  ID_row_badheader <- lapply(sans_ID, FUN=curve_fit, dates=ID_red_DF$norm.dates)
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
  #Makes a new header for output dataframe with a and b (intercept and slope) fields for each variable
  header <- lapply(non_id_colnames, header_name_generator, header=header)

  #Sorry for mutating data
  header <- c(ID,header)
  header <- unlist(header)

  #Pass data and ID_list to downstream functions to create curve-fit dataframe
  df_without_header <- lapply(ID_list, FUN=ID_processor, ID_DF=reduced_meta, header=header)

  #Turn df_without_header from list into data.frame
  df_without_header <- do.call("rbind", df_without_header)

  #Attach list of unique IDs to df_without_header
  param_df <- data.frame(cbind(ID_list, df_without_header))

  #Add column names (generated by header_name_generator)
  colnames(param_df) <- header

  return(param_df)
}

hash_slinging_slasher <- function(ID_list, colorscheme){
  #Creates a hash; entering an ID gets a unique, ordered number for that ID
  ID_colors <- hash()
  for(i in 1:length(ID_list)){
    ID_colors[i] <- ID_list[[i]]
  }
  return(invert(ID_colors))
}

add_fitlines_g <- function(col_name, hash){
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    col_a_name = paste(col_name, 'a', sep='_')
    col_b_name = paste(col_name, 'b', sep='_')
    plot <- ggplot(reduced_meta_wnorm, aes_string(x='norm.dates', y=col_name, colour=factor(ID))) + geom_point() + ggtitle(col_name)
    for(i in 1:ncol(param_df)){
        #Add individual fit lines to the plot; iterate through colors in cbbPalette, rotating through available selections in sequence
        plot <- plot + geom_abline(slope=param_df[i,col_b_name], intercept=param_df[i,col_a_name], colour=cbbPalette[[(i %% length(cbbPalette)) + 1]])
        }
    return(plot)
}


#Convert dates to integers from lookup table for later conversion back to dates
reduced_meta$int_date <- unlist(lapply(reduced_meta$Visit_DT, FUN=date_range_constructor))

#Create column of dates normalized so that the first visit is day 0 (norm.dates)
reduced_meta_wnorm <- normalized_date_column_constructor(reduced_meta)

#DEBUG ONLY:
#write.csv(reduced_meta_wnorm, 'normalized_dates.csv')

#Create DF with rows corresponding to IDs and columns containing curve parameters
param_df <- param_df_constructor(reduced_meta_wnorm, ID_list)

names_list <- colnames(reduced_meta_wnorm)
names_list_without_id <- names_list[-c(1, 2)]

cat("Generating plots...")

if(graph_mode){
#Optional: Use ggplot2
plot_list <- lapply(names_list_without_id, FUN=add_fitlines_g)

cat("\nPlots successfully generated. Saving to PDF...")

pdf(pdfout)
invisible(lapply(plot_list, print))
dev.off()
}else{
  pdf(pdfout)
  par(mfrow=c(2,2))
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  Palette <- color_constructor(reduced_meta_wnorm$ID, cbbPalette)
  for(name in names_list_without_id){
    #print(name)
    #print(reduced_meta_wnorm[name])
    #print(reduced_meta_wnorm['norm.dates'])

    colname_a = paste(name, 'a', sep='_')
    colname_b = paste(name, 'b', sep='_')
    testplot <- sapply(reduced_meta_wnorm[name], as.numeric)
    if(FALSE){
      X11()
      plot(reduced_meta_wnorm$norm.dates, testplot, col=Palette[reduced_meta_wnorm$ID])
      message("Press Return To Continue")
      invisible(readLines("stdin", n=1))
      print(reduced_meta_wnorm[name])
      show_hist(reduced_meta_wnorm[name])
    }
    plot(reduced_meta_wnorm$norm.dates, testplot, col=Palette[reduced_meta_wnorm$ID], main=name, xlab="norm.dates", ylab=name)
    for(i in 1:ncol(param_df)){
      red_param_df = param_df[i, c(colname_a, colname_b)]
      if(!any(is.na(red_param_df))){
      abline(param_df[i, colname_a], param_df[i,colname_b], col=Palette[param_df$ID[i]])}
    }
  info1 <- sapply(param_df[colname_a], function(x) round(c(Mean=mean(x), SD=sd(x), N=gdata::nobs(x)),2))
  info2 <- sapply(param_df[colname_b], function(x) round(c(Mean=mean(x), SD=sd(x), N=gdata::nobs(x)),2))
  if(!all(is.na(param_df[colname_a]))){boxplot(param_df[colname_a], main=colname_a)}else{textplot(info1)}
  if(!all(is.na(param_df[colname_b]))){boxplot(param_df[colname_b], main=colname_b)}else{textplot(info2, valign="top")}
  boxplot(reduced_meta_wnorm[name], main=name)
  }
  dev.off()
}
cat("PDF saved to... ", pdfout)

cat("\nWriting csv to: ")
print(outfile)
write.csv(param_df, outfile)

cat("Execution complete; pending addition of quadratic and cubic subroutines.")
