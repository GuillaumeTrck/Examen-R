setwd("./")

# Load the packages
library(DBI)
library(tidyverse)
library(magrittr)
library(dplyr)
library(shiny)
library(ggplot2)
library(readxl)
notes<-read_excel("note.xlsx")%>%as.data.frame()
