# Required Packages for EPA's RPGen
# AE, ORAU, 2020.

requiredpackages<-  c("data.table",
                      "stringr",
                      "plyr",
                      "dplyr",
                      "dtplyr",
                      "ggplot2",
                      "bit64",
                      "httk",
                      "msm",
                      "truncnorm",
                      "survey",
                      "downloader")

neededpackages<-setdiff(requiredpackages,rownames(installed.packages()))

install.packages(neededpackages)

library("data.table")
library("stringr")
library("plyr")
library("dplyr")
library("dtplyr")
library("ggplot2")
library("bit64")
library("httk")
library("msm")
library("truncnorm")
library("survey")
library("downloader")

rm(requiredpackages,neededpackages)
