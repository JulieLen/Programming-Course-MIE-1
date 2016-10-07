#### Session 5 - Homework ####

# Create a Package 

library("devtools")
library("roxygen2")
library("testthat")

setwd("~/Documents/1 - ScPo/16:18 - MIE/Programming course/Programming-Course-MIE-1/Session5_HW")
create('JuliesPackage')

dir.create(paste('JuliesPackage', "man", sep="/"))

devtools::use_testthat('JuliesPackage')

document('JuliesPackage')

context("Session5_HW_Function.R")

test_that((12,5, 1, 2, 1, 1),{
  expect_match(lindirect_utility(12,5, 1, 2, 1, 1))
})

build('JuliesPackage')

# I still have probs with the command line in the Terminal, so I guess this is why it does not work !