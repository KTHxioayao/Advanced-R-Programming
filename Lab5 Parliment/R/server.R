
#' Plot Member Age Distribution Histogram
#'
#' This function plots a histogram of the age distribution for members of a specified party.
#'
#' @param input A list containing parameters. \code{party_name} is a string representing the name of the party.
#' @param output A Shiny output object used to render the histogram.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming member_basic_info data is loaded
#' # input <- list(party_name = "Test_Party")
#' # output <- list(main_plot = renderPlot(function() NULL))
#' # draw_member_age_hist(input, output)
draw_member_age_hist <- function(input, output) {

  output$main_plot <- renderPlot({

    hist(member_basic_info[member_basic_info$Party == input$party_name, ]$Age,
         probability = FALSE,
         breaks = 10,
         xlab = "Age",
         ylab = 'Number',
         border = 'white',
         col = 'skyblue',
         main = "Member Age Hist")
  })
}
