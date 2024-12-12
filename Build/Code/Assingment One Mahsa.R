#Assignment One
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ipumsr)
library(stargazer)
library(tidycensus)
library(sf)

#Getting data from IPUMS API
#set_ipums_api_key("59cba10d8a5da536fc06b59dc1f0ad8c2685400386e4e500129f338f", save = FALSE , overwrite = FALSE) #Sets your personal key and saves it

tst <- get_metadata_nhgis("time_series_tables") #This gives you a list of the time series tables that you can get

#Insert Answer to Question 2 Here####
  print(n=25, tst$time_series[[5]])
  
#data_names <- c("A00","A57","B57","B18","CL6","B69") #These are the tables we are using
  data_ext<- define_extract_nhgis(
    description = "ECON 691",
    time_series_tables =  list(
      tst_spec("A00", "state"),
      tst_spec("A57", "state"),
      tst_spec("B57", "state"),
      tst_spec("B18", "state"),
      tst_spec("CL6", "state"),
      tst_spec("BX3", "state")
    )
  )
  
  ts<-submit_extract(data_ext)
  wait_for_extract(ts)
  filepath <- download_extract(ts)
  dat <- read_nhgis(filepath)
  
  ufo <- read.csv("./Data/scrubbed.csv", stringsAsFactors = FALSE)
  st_abb <- read.csv("./Data/st_abb.csv")
    #dat <- read_nhgis("C:/Users/Z1974765/Desktop/R assignment/nhgis0007_ts_nominal_state.csv")

#Obtain Census Map Data
  
  #census_api_key("2ff10680b03f27649fa8f4f235684ea818f16de1", install = TRUE, overwrite = TRUE)
  
  state <- c("31", "20", "19", "29")
  
  cen.stat <- get_acs(geography = "state", 
                      survey = "acs5",
                      variables = "B01003_001E", 
                      year = 2020,
                      geometry = TRUE)


  cen.map <- cen.stat %>%
    select(GEOID, NAME, geometry)  %>%
    mutate(STATEFP = GEOID) %>%
    filter(STATEFP %in% state)

 #Basic Clan of data####

    dat2 <- dat %>%
    #mutate(STATEFP = as.character(STATEFP)) %>%
      select(STATEFP, ends_with(c("1970", "1980", "1990", "2000", "2010", "105", "2020", "205"))) %>%
      filter(!is.na(STATEFP)) %>%
    pivot_longer(!STATEFP, names_to = "series", values_to = "estimate") %>%
      mutate(series = str_replace(series, "105", "2010"),
             series = str_replace(series, "205", "2020"),
             year = substr(series, 6, nchar(series)),
             series = substr(series, 1, 5)) %>%
      distinct(STATEFP, series, year, .keep_all = TRUE) %>%
      filter(!is.na(estimate)) %>%
      pivot_wider(id_cols = c(STATEFP, year), names_from = series, values_from = estimate) %>%
      select(-B18AE)  %>%
      mutate( 
   und18 = rowSums(across(starts_with("B57AA"))) / across(starts_with("A00AA")),
             over65 = rowSums(across(starts_with("B57AP"))) / across(starts_with("A00AA")),
             white = across(starts_with("A57AA")) / across(starts_with("A00AA")), #White Population
             black = across(starts_with("A57AB")) / across(starts_with("A00AA")), #Black Population
             asian = across(starts_with("A57AC")) / across(starts_with("A00AA")), #Asian Population
             other = across(starts_with("A57AD")) / across(starts_with("A00AA")),, #Something other than the above including multi-race
             lessHS = (rowSums(across(starts_with("BX3AA"))) +
                         rowSums(across(starts_with("BX3AB"))) +
                         rowSums(across(starts_with("BX3AC")))) / across(starts_with("A00AA")),
             hscol =  (BX3AD + BX3AE + BX3AJ + BX3AK) / A00AA, #12th Grade and some college
             ungrd =  (BX3AF + BX3AL) / A00AA, #4 years of college or Bach Degree
             #advdg =  [Insert Code Here], #More than 4 years or advanced degree
             pov =  rowSums(across(starts_with("CL6AA"))) / across(starts_with("A00AA")), #Share of population under Poverty Line
             ln_pop = log(rowSums(across(starts_with("A00AA"))))
   )%>%  #Natural Log of Population
      
    select(STATEFP, year, und18:ln_pop) 
  
  
     ufo <- read.csv("C:/Users/Z1974765/Desktop/R assignment/scrubbed-ufo.csv", header = TRUE, as.is = TRUE, sep = ",")
     
     st_abb <- read.csv("C:/Users/Z1974765/Desktop/R assignment/st_abb.csv", stringsAsFactors = FALSE)
     
     
     ufo.us <- ufo %>%
      filter(country == "us") %>%
      mutate(
        state = toupper(state),
        date = as.Date(str_split_i(datetime," ", 1), "%m/%d/%Y"),
             year = year(date),
             decade = year - year %% 10
        ) %>%
      filter(decade > 1959) %>%
      count(state, decade) %>%
      mutate(
        Abbr = toupper(state),
        year = as.numeric(decade)
        ) %>%
      full_join(st_abb, by = "Abbr") %>%
      filter(!is.na(n)) %>%
      rename("GEOID" = "Code") %>%
      mutate(
        GEOID = str_pad(as.character(GEOID), width = 2, side = "left", pad="0"),
             ln_n = log(n)
        )

     
     # Join the data and map it
     dat2 <- dat2 %>%
       mutate(STATEFP = as.character(STATEFP),
     year = as.numeric(year)) 

     cen.map <- cen.map %>%
       mutate(STATEFP = as.character(STATEFP))
     
     core <- cen.map %>%
       left_join(dat2, by = "STATEFP") %>%
       filter(year >= 1970 & year <= 2010) %>%  
       select(-ends_with(".x"), -ends_with(".y"))
     dat2 <- dat2 %>%
       mutate(year = as.numeric(year))
     cen.map <- cen.map %>%
       select(GEOID, NAME, geometry, STATEFP)
     
     dat2 <- dat2 %>%
       mutate(
         STATEFP = as.character(STATEFP),
         year = as.numeric(year),
         decade = year - year %% 10 # Calculate decade
       )
     
     core <- cen.map %>%
       left_join(dat2, by = "STATEFP") %>%
       filter(year >= 1970 & year <= 2010) %>%
       select(-ends_with(".x"), -ends_with(".y"))
     
     core <- core %>%
       group_by(decade) %>%
       filter(n() == min(n()))
     
     nrow(core)        # Total rows in `core`
     length(core$geometry) # Number of geometries
     
     core <- core %>%
       group_by(decade) %>%
       filter(row_number() <= min(n()))
     
     core <- core %>%
       filter(!(STATEFP == "72" & decade < 2010))
     
     core <- core %>%
       filter(STATEFP != "72")
     
     core <- core %>%
       filter(!st_is_empty(geometry), !is.na(und18))
     
     core <- core %>%
       filter(!st_is_empty(geometry) & !is.na(und18))
     
#Non-Race Variable Graphic Visualization#####
     
    ggplot(core) +
      geom_sf(aes(fill =  und18$A00AA)) +
      scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                           name = "Under 18 (%)",
                           limits = c(0, 0.13)) +
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom") +
      labs(
        title = "FIgure One: Percentage of Population Under 18 Across the Decades",
       fill ="Under 18 (%)"
     ) +
       
      facet_wrap(~ decade) 
    
    ggsave("./Analysis/Output/Figure1.png", dpi = 600)

#Race Variable Graphic Visualization

    ggplot(core) +
      geom_sf(aes(fill = white$A57AA)) +
      scale_fill_gradient2(low = "white", high = "red", na.value = NA, 
                           name = "White Population (%)",
                           limits = c(0, 1)) +
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom") +
      labs(title = "Figure Two: Percentage of White Population Across the Decades") +
      facet_wrap(~ decade)
    
    ggsave("./Analysis/Output/Figure2.png", dpi = 600)

#Summary Statistics

var1 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "LN of Population","Decade","Number of Signthings", "LN of Sightings")

stargazer(as.data.frame(core), type = "html", out = "./Analysis/Output/Table1.html",
          title = "Table One - Summary Statistics",
          covariate.labels [Insert Code Here]

#Regression Analysis

mod1 <- lm([Insert Code Here]
           data=core)

mod2 <- lm([Insert Code Here]
           data=core)

mod3 <- lm([Insert Code Here]
           data=core)

var2 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "LN of Population","Decade","Number of Signthings", "LN of Sightings")


stargazer(mod1, mod2, mod3,
          omit = ".State.",
          type = "html",
          title = "Table Two - Regression Results",
          out = "./Analysis/Output/Table2.html",
          add.lines=list(c("State F.E." [Insert Code Here] )),
          dep.var.labels = "LN(Sightings)",
          covariate.labels=var2)

