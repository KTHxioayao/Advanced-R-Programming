library(shiny)
data("member_basic_info")
bootstrapPage(

  selectInput(inputId = "party_name",
              label = "The name of the party:",
              choices = unique(member_basic_info$Party),
              selected = unique(member_basic_info$Party[1])),


  plotOutput(outputId = "main_plot", height = "300px")



)
