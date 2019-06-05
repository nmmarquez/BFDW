## load libraries
rm(list=ls())
library(data.table) ## for streamlining data processing (an alternative to base-R and tidyverse)
library(memisc) ## for viewing regression results
library(maps) ## for making maps
library(readstata13) ## for reading in ACS data
library(tools) ## to make some of the functions work
library(tidyverse) ## I like the tidyverse 
library(httr) # to pull data from wiki 
library(sf) # for making pretty maps
library(XML) # more webscraping tools 
library(leaflet) # making interactive maps

# load demography functions
source("https://courses.demog.berkeley.edu/_formaldemog2019/Labs/project_map_functions.R")
source("https://courses.demog.berkeley.edu/_formaldemog2019/Labs/project_data_prep_functions.R")

if(!file.exists("./data/dt.Rds")){
    my.df <- read.dta13("https://courses.demog.berkeley.edu/_formaldemog2019/Labs/usa_00180.dta")
    
    ## (can ignore duplicated factor warning)
    dt <- as.data.table(my.df) ## converts to data.table
    print(dt) ## view
    
    saveRDS(dt, "./data/dt.Rds")
}

## (can ignore duplicated factor warning)
dt <- readRDS("./data/dt.Rds")

## incase we want to look at gdp
gdppcDF <- paste0(
    "https://en.wikipedia.org/wiki/",
    "List_of_U.S._states_by_GDP_per_capita") %>%
    GET() %>%
    content("text") %>%
    {readHTMLTable(doc=., header=T)} %>%
    .[[1]] %>%
    mutate_if(is.factor, as.character) %>%
    gather("Year", "GDPPC", -Rank, -State) %>%
    mutate(Year=as.numeric(sub("\\n", "", Year))) %>%
    mutate(GDPPC=as.numeric(sub(",", "", GDPPC))) %>%
    mutate_if(is.factor, as.character)

# pull in urban pop 
urbpDF <- paste0(
    "https://en.wikipedia.org/wiki/",
    "Urbanization_in_the_United_States") %>%
    GET() %>%
    content("text") %>%
    {readHTMLTable(doc=., header=T, skip.rows = 1)} %>%
    .[[1]] %>%
    select(State=V1, pUrban=V2) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(pUrban=as.numeric(sub("%", "", pUrban))) %>%
    mutate(State=sub("[[:digit:]]+", "", State)) %>%
    mutate(State=sub("\\[", "", State)) %>%
    mutate(State=sub("\\]", "", State))

# cl
dt.clean <- clean.acs.data(indiv.dt = dt)

## transform indivdiual level data into aggregate data for gravity modeling
grav.dt1 <- create.gravity.data(my.dt = dt.clean) ##
nrow(grav.dt1) ## should be 50x50 = 2500 rows
## merge in distance measure and urban data infromation 
grav.dt <- as_tibble(add.distance.variable.to.gravity.data(grav.dt1)) %>%
    mutate(origin=stringr::str_to_title(origin)) %>%
    mutate(destination=stringr::str_to_title(destination)) %>%
    left_join(rename(urbpDF, origin=State, opUrban=pUrban)) %>%
    left_join(rename(urbpDF, destination=State, dpUrban=pUrban)) %>%
    mutate(uMi=Mi*opUrban, dMi=Mj*dpUrban)
