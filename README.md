# Metadata-Curvefit

## This package is intended to assist in statistical analysis of medical metadata by creating polynomial fits to numeric columns in tabular data for downstream analysis.

## General usage is as follows:

> Rscript curve_fit.R -i [INPUT_FILE.csv] -m [MODE] -o [OUTPUT_FILE.csv] -p [PLOT_FILE.pdf]

## For a complete set of command-line options, type

>Rscript curve_fit.R -h

## The R package dependencies for this script (as of version 0.1.0) are:

>argparse,
>dplyr,
>tidyr,
>ggplot2
