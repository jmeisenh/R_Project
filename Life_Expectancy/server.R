



function(input, output){
  
  BMIData <- reactive({
    IntervalsBMI(input$place, input$minBMI, input$maxBMI, input$minSchool, input$maxSchool)})
  SchoolData <- reactive({
    IntervalsSchooling(input$place, input$minBMI, input$maxBMI, input$minSchool, input$maxSchool)})
  Demographics <- reactive({
    DemoTable(input$place)
  })
    

  output$BMIplot <- renderPlot(
    BMIData() %>%
      ggplot(., aes(x = BMI, y = Life_Expectancy, color = Type)) + geom_smooth()
    + ggtitle(paste0('Life Expectancy vs. BMI for Peoples Educated ',(input$minSchool + input$maxSchool)/2, ' Years')) +
      labs(y = 'Life Expectancy', color = 'Fit\n') + theme(plot.title = element_text(size = 20, face = 'bold')) + 
      scale_color_discrete(labels = c('Predicted', 'Lower Confidence Band', 'Upper Confidence Band', 'Lower Prediction Band', 'Upper Prediction Band'))
  )
  output$SchoolPlot <- renderPlot(
    SchoolData() %>%
      ggplot(., aes(x = Schooling, y = Life_Expectancy, color = Type)) + geom_smooth() +
      ggtitle(paste0('Life Expectancy vs. Years Educated for Peoples with a BMI of ', (input$minBMI + input$maxBMI)/2)) +
      labs(y = 'Life Expectancy', x = 'Years Educated', color = 'Fit\n') + theme(plot.title = element_text(size = 20, face = 'bold')) + 
      scale_color_discrete(labels = c('Predicted', 'Lower Confidence Band', 'Upper Confidence Band', 'Lower Prediction Band', 'Upper Prediction Band'))
  )
  output$fulltable <- renderDataTable(Demographics())
  
}
