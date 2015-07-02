#!/usr/bin/env Rscript

source("classifier.R")

args <- commandArgs(TRUE)
user <- as.character(args[1])
print( main_classifier(user) )
