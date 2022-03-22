
# import libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)

# Read Data

df = read.csv('LED2.csv')

# train predictive model

df.emptyMod <- lm(Life.expectancy ~ 1, data = df[,-c(1,2)])
df.fullMod <- lm(Life.expectancy ~ ., data = df[,-c(1,2)])
scope = list(lower = formula(df.emptyMod), upper = formula(df.fullMod))
df.model <- step(df.emptyMod, scope, direction = 'forward', k=2)
options(scipen = 999)
meanround <- function(x){
  round(mean(x),2)
  
}

# function that takes in BMI and schooling and outputs a life expectancy

expectLife <- function(country,BMI,school){
  
  # need to pull status from df
  first(df[df$Country == country,]$Status) -> developing
  
  df %>%
    dplyr::select(Country, HIV.AIDS, Diphtheria, under.five.deaths, GDP, Polio, Thinness, Hepatitis.B, Population) %>%
    dplyr::filter(Country == country) %>%
    dplyr::select(-Country) %>%
    summarise_all(mean) %>%
    cbind(., 'Status' = developing, 'BMI' = BMI, 'Schooling' = school) -> newwdata
  prediction <- predict(df.model, newwdata, interval = 'confidence')
  prediction[,1]
  
}
IntervalsBMI <- function(country, BMImin, BMIMax, minSchool, maxSchool){

      # need to pull status from df
    first(df[df$Country == country,]$Status) -> developing
    BMIRange <- seq(from= BMImin, to = BMIMax, length.out = 20)
    school <- mean(minSchool, maxSchool)
    modeled <- data.frame(matrix(ncol = 6, nrow = 0))
    x <- c('BMI', 'confFit', 'confLower', 'confUpper', 'predLower', 'predUpper')
    colnames(modeled) <- x
    for (i in BMIRange){
      
      df %>%
        dplyr::select(Country, HIV.AIDS, Diphtheria, under.five.deaths, GDP, Polio, Thinness, Hepatitis.B, Population) %>%
        dplyr::filter(Country == country) %>%
        dplyr::select(-Country) %>%
        summarise_all(mean) %>%
        cbind(., 'Status' = developing, 'BMI' = i, 'Schooling' = school) -> newwdata
      conf <- predict(df.model, newwdata, interval = 'confidence')
      pred <- predict(df.model, newwdata, interval = 'prediction')
      modeled %>%
        add_row(BMI = round(i,2),
                confFit = round(conf[1],2),
                confLower = round(conf[2],2),
                confUpper = round(conf[3],2),
                predLower = round(pred[2],2),
                predUpper = round(pred[3],2)) -> modeled
      
    }
    modeled %>%
      pivot_longer(., cols = confFit:predUpper, names_to = 'Type', values_to = 'Life_Expectancy')
      
    
  
}

IntervalsSchooling <- function(country, BMImin, BMIMax, minSchool, maxSchool){
  
  # need to pull status from df
  first(df[df$Country == country,]$Status) -> developing
  BMImean <- mean(BMImin, BMIMax)
  schoolRange <- seq(from = minSchool, to = maxSchool, length.out = 20)
  modeled <- data.frame(matrix(ncol = 6, nrow = 0))
  x <- c('Schooling', 'confFit', 'confLower', 'confUpper', 'predLower', 'predUpper')
  colnames(modeled) <- x
  for (i in schoolRange){
    
    df %>%
      dplyr::select(Country, HIV.AIDS, Diphtheria, under.five.deaths, GDP, Polio, Thinness, Hepatitis.B, Population) %>%
      dplyr::filter(Country == country) %>%
      dplyr::select(-Country) %>%
      summarise_all(mean) %>%
      cbind(., 'Status' = developing, 'BMI' = BMImean, 'Schooling' = i) -> newwdata
    conf <- predict(df.model, newwdata, interval = 'confidence')
    pred <- predict(df.model, newwdata, interval = 'prediction')
    modeled %>%
      add_row(Schooling = round(i,2),
              confFit = round(conf[1],2),
              confLower = round(conf[2],2),
              confUpper = round(conf[3],2),
              predLower = round(pred[2],2),
              predUpper = round(pred[3],2)) -> modeled
    
  }
  modeled %>%
    pivot_longer(., cols = confFit:predUpper, names_to = 'Type', values_to = 'Life_Expectancy')
}

DemoTable <- function(country){

  df %>%
    dplyr::filter(Country == country) %>%
    dplyr::select(5:18) %>%
    summarise_all(meanround) %>%
    mutate(Hepatitis.B = Hepatitis.B * 100,
           Polio = Polio * 100,
           Total.expenditure = Total.expenditure * 100,
           Diphtheria = Diphtheria * 100,
           Thinness = Thinness * 100) %>%
    rename('Life Expectancy (Years)' = Life.expectancy,
           'Average Drinks Per Week' = Alcohol,
           'Percentage Vax Hepatitis B'=  Hepatitis.B,
           'Measles Cases' = Measles,
           '5 year Mortality per 1000' = under.five.deaths,
           'Percentage Vax Polio' = Polio,
           'Percentage of GDP Spent on Healthcare' = Total.expenditure,
           'Percentage Vax DTP3' = Diphtheria,
           'Infant HIV Deaths per 1000' = HIV.AIDS,
           'GDP per Person' = GDP,
           'Average Education (Years)' = Schooling,
           'Percentage Below Ideal BMI' = Thinness) %>%
    pivot_longer(., everything(), names_to = 'Demographic', values_to = 'Value')
  
  

}









