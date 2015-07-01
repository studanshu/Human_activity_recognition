#!/usr/bin/env Rscript

args <- commandArgs(TRUE)

user <- as.character(args[1])

source("classifier.R")

if( user == 'all' ){
  classify(method='inst')
}else{
  classify(method='inst', user=user)
}