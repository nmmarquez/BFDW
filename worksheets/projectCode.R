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
library(stargazer)
library(circlize)

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
    mutate(State=sub("\\]", "", State)) %>%
    mutate(urbanQ=cut_number(
        pUrban, 4, labels=c("Very Low", "Low", "Hi", "Very Hi"))) 

# cl
dt.clean <- clean.acs.data(indiv.dt = dt)

## transform indivdiual level data into aggregate data for gravity modeling
grav.dt1 <- create.gravity.data(my.dt = dt.clean) ##
nrow(grav.dt1) ## should be 50x50 = 2500 rows
## merge in distance measure and urban data infromation 
grav.dt <- as_tibble(add.distance.variable.to.gravity.data(grav.dt1)) %>%
    mutate(origin=stringr::str_to_title(origin)) %>%
    mutate(destination=stringr::str_to_title(destination)) %>%
    left_join(rename(select(urbpDF, -urbanQ), origin=State, opUrban=pUrban)) %>%
    left_join(rename(select(urbpDF, -urbanQ), destination=State, dpUrban=pUrban)) %>%
    mutate(uMi=Mi*opUrban*.01, uMj=Mj*dpUrban*.01) %>%
    mutate(uR=uMi/uMj, uD=uMi-uMj)

write.csv(grav.dt, file="./data/gravdt.csv")

# Models to test 

modelList <- list(
    base = Fij ~ 1 + log(Mi) + log(Mj) + log(distance),
    urbanReplace = Fij ~ 1 + log(uMi) + log(uMj) + log(distance),
    urbanPCov = Fij ~ 1 + log(Mi) + log(Mj) + log(distance) + log(opUrban) + log(dpUrban),
    urbanRatioCov = Fij ~ 1 + log(Mi) + log(Mj) + log(distance) + log(uR)
)

m.pois <- lapply(modelList, function(f){
    glm(f, family=poisson(link = "log"), data = grav.dt)
    })

stargazer(m.pois)

# We are gonna make a midnight drunk attempt on making a deent looking chord diagram
grav.dt

grav.dt %>%
    left_join(rename(select(urbpDF, -pUrban), destination=State, dUrbanQ=urbanQ)) %>%
    left_join(rename(select(urbpDF, -pUrban), origin=State, oUrbanQ=urbanQ)) %>%
    group_by(dUrbanQ, oUrbanQ) %>%
    summarize(Fij=sum(Fij, na.rm=T)) %>%
    chordDiagram()

grav.dt %>%
    left_join(rename(select(urbpDF, -pUrban), destination=State, dUrbanQ=urbanQ)) %>%
    left_join(rename(select(urbpDF, -pUrban), origin=State, oUrbanQ=urbanQ)) %>%
    group_by(dUrbanQ, oUrbanQ) %>%
    summarize(Fij=sum(Fij, na.rm=T)) %>%
    mutate(HI=grepl("Hi", dUrbanQ) & grepl("Hi", oUrbanQ)) %>%
    group_by(HI) %>%
    summarize(Fij=sum(Fij)) %>%
    mutate(pF=Fij/sum(Fij))

# What is the expeted value here??? This isnt right
grav.dt %>%
    left_join(rename(select(urbpDF, -pUrban), destination=State, dUrbanQ=urbanQ)) %>%
    left_join(rename(select(urbpDF, -pUrban), origin=State, oUrbanQ=urbanQ)) %>%
    filter(origin == destination) %>%
    mutate(HI=grepl("Hi", dUrbanQ) & grepl("Hi", oUrbanQ)) %>%
    group_by(HI) %>%
    summarize(Mi=sum(Mi)) %>%
    mutate(pPop=Mi/sum(Mi))
