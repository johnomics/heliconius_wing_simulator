library(shiny)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Heliconius wing simulator"),

  sidebarPanel(
      checkboxInput("B","Red forewing band"),
      checkboxInput("D","Dennis"),
      checkboxInput("R","Rays"),
      checkboxInput("Y","Yellow band"),
      checkboxInput("N","White forewing band")
  ),
  
  mainPanel(
      plotOutput("wing")
  )
))





