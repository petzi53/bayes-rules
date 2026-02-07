library(shiny)
library(bayesrules)
library(ggplot2)

# Build the server
server2 <- function(input, output) {
    output$priorPdf <- renderPlot({
        # Calculate 95% credible interval
        lower <- qbeta(0.025, input$aPrior, input$bPrior)
        upper <- qbeta(0.975, input$aPrior, input$bPrior)

        plot_beta(alpha = input$aPrior, beta = input$bPrior) +
            ggplot2::lims(y = c(0, input$ymax)) +
            ggplot2::geom_vline(xintercept = lower, linetype = "dashed",
                                color = "darkgreen", linewidth = 1) +
            ggplot2::geom_vline(xintercept = upper, linetype = "dashed",
                                color = "darkgreen", linewidth = 1) +
            ggplot2::annotate("rect", xmin = lower, xmax = upper,
                              ymin = 0, ymax = Inf,
                              alpha = 0.1, fill = "darkgreen")
    })

    output$priorTable <- renderTable({
        summarize_beta(alpha = input$aPrior, beta = input$bPrior)
    }, digits = 4
    )

    output$credibleInterval <- renderText({
        lower <- qbeta(0.025, input$aPrior, input$bPrior)
        upper <- qbeta(0.975, input$aPrior, input$bPrior)

        paste0("<h4>95% Credible Interval (green dashed lines):</h4>",
               "<p><strong>[", round(lower, 4), ", ", round(upper, 4), "]</strong></p>",
               "<p>There is a 95% probability that π lies between ",
               round(lower, 4), " and ", round(upper, 4), ".</p>")
    })

    output$click_info <- renderPrint({
        if (!is.null(input$plot_click)) {
            paste0("Clicked x-position: ", round(input$plot_click$x, 3))
        } else {
            "Click on the plot to record x-position"
        }
    })
}

# Build the user interface
ui2 <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            h4("Tune the Beta(a,b) prior:"),
            sliderInput("aPrior", "alpha", min = 0, max = 100, value = 1, step = 0.5),
            sliderInput("bPrior", "beta", min = 0, max = 100, value = 1, step = 0.5),
            h4("Specify the limit of the y-axis:"),
            sliderInput("ymax", "", min = 1, max = 40, value = 10)
        ),
        mainPanel(
            h4("Plot of the prior pdf:"),
            plotOutput("priorPdf", click = "plot_click"),
            h4("Prior features:"),
            tableOutput("priorTable"),
            htmlOutput("credibleInterval"),
            verbatimTextOutput("click_info")
        )
    )
)

# Run the shiny app!
shinyApp(ui = ui2, server = server2)
