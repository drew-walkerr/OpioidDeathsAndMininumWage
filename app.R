library(shiny)
library(tidyverse)
library(patchwork)
library(haven)

opioid_m2 <- read_sas("T:/bsheprojs/Family Economic Security Policies/Mortality Data/Analysis Files/opioid_mw2.sas7bdat")
ui <- fluidPage(selectInput(inputId = "state_input",
                            label = "Choose a State",
                            choices = opioid_m2$State),
                plotOutput(outputId = "minimum_wage_state_plot")
                )
server <- function(input, output) {
  filter_state <- reactive({
    a <- subset(opioid_m2, State == input$state_input)
    return(a)
  })
  output$minimum_wage_state_plot <- renderPlot({
    ggplot(data=filter_state(), aes(Mon,mwdiff2015))+
      geom_line()+
      ggtitle('State Minimum Wage Difference from Federal', subtitle = 'in 2015 Dollars over Time by State, 2000-2015')+
      theme_classic()+
    ggplot(data = filter_state(), aes(Mon,Frequency))+
      geom_line(aes(color = State))+
      ggtitle('Opioid-related deaths over Time by State, 2000-2015')+
      theme_classic()
    })
}

shinyApp(ui = ui, server = server)