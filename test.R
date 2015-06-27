#!/usr/bin/env Rscript

args <- commandArgs(TRUE)

user <- as.character(args[1])

source("classifier.R")

classify(method='inst', user=user)