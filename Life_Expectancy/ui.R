



dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarUserPanel('Data Input'),
    sidebarMenu(
      menuItem('Graphs', tabName = 'graphs', icon = icon('map')),
      menuItem('Data', tabName = 'data', icon = icon('database'))
    ),
    selectizeInput(inputId = 'place',
                   label = 'Country',
                   choices = unique(df$Country)),
    sliderInput(inputId = 'minBMI',
                label = 'Minimum BMI',
                step = 1,
                min = 1,
                max = 85,
                value = 15,
                round = FALSE,
                ticks = FALSE
                ),
    sliderInput(inputId = 'maxBMI',
                label = 'Maximum BMI',
                step = 1,
                min = 1,
                max = 85,
                value = 25,
                round = FALSE,
                ticks = FALSE
    ),    
    sliderInput(inputId = 'minSchool',
                label = 'Minimum Years of Schooling',
                step = 1,
                min = 1,
                max = 20,
                value = 6,
                round = FALSE,
                ticks = FALSE
    ),    
    sliderInput(inputId = 'maxSchool',
                label = 'Maximum Years of Schooling',
                step = 1,
                min = 1,
                max = 20,
                value = 12,
                round = FALSE,
                ticks = FALSE
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = 'graphs',
              fluidRow(
                column(12, plotOutput('BMIplot'))
              ),
              fluidRow(
                column(12, plotOutput('SchoolPlot'))
              )
              ),
      tabItem(tabName = 'data',
              fluidRow(
                column(12, dataTableOutput('fulltable'))
              ))
    )
    
  )
)
