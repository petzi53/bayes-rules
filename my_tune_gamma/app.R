# library(shiny)
# library(tidyverse)
# library(ggplot2)

# Define UI
ui <- shiny::fluidPage(
    shiny::titlePanel("Gamma Prior Calculator"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::numericInput("mean", "Mean (μ)", value = 7, min = 0.01, step = 0.1),
            shiny::numericInput("mode", "Mode (λ)", value = 4, min = 0, step = 0.1),
            shiny::helpText("Ensure mean > mode for valid Gamma distribution."),
            shiny::actionButton("calculate", "Calculate Gamma Prior")
        ),
        shiny::mainPanel(
            shiny::h4("Prior Distribution"),
            shiny::plotOutput("gamma_plot"),
            shiny::h4("Summary"),
            shiny::verbatimTextOutput("summary")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    # Reactive calculation
    result <- shiny::eventReactive(input$calculate, {
        mean_val <- input$mean
        mode_val <- input$mode

        # Validate inputs
        if (mean_val <= 0) {
            return(list(error = "Mean must be positive."))
        }
        if (mode_val < 0) {
            return(list(error = "Mode must be non-negative."))
        }

        # Compute parameters
        diff <- mean_val - mode_val

        if (diff <= 0) {
            s <- 1.0
            r <- 1.0 / mean_val
            warning_msg <- paste0(
                "Mean ≤ mode. Cannot satisfy. Defaulting to exponential (s=1). ",
                "Mean: ", round(mean_val, 3), ", Mode: ", round(mode_val, 3)
            )
            return(list(
                s = s,
                r = r,
                warning = warning_msg,
                distribution = "Gamma(1, 1/mean)",
                mean = mean_val,
                mode = mode_val
            ))
        }

        s <- mean_val / diff
        r <- 1 / diff

        if (s <= 1) {
            s <- 1.0
            r <- 1.0 / mean_val
            warning_msg <- paste0(
                "Shape s = ", round(s, 3), " ≤ 1. Cannot achieve mode ", round(mode_val, 3), ". ",
                "Defaulting to exponential (s=1)."
            )
        } else {
            warning_msg <- if (diff < 0.1) {
                paste0("Mean - mode = ", round(diff, 3), " is very small. High sensitivity.")
            } else {
                NULL
            }
        }

        # Return result
        list(
            s = s,
            r = r,
            warning = warning_msg,
            distribution = paste0("Gamma(", round(s, 3), ", ", round(r, 3), ")"),
            mean = mean_val,
            mode = mode_val
        )
    })

    # Plot
    output$gamma_plot <- shiny::renderPlot({
        res <- result()
        if (is.null(res$error)) {
            s <- res$s
            r <- res$r
            x <- seq(0, 2 * res$mean, length.out = 1000)
            y <- dgamma(x, shape = s, rate = r)

            # Plot
            ggplot2::ggplot(data.frame(x = x, y = y), ggplot2::aes(x = x, y = y)) +
                ggplot2::geom_line(color = "darkblue", size = 1) +
                ggplot2::geom_vline(xintercept = res$mode, color = "red", linetype = "dashed") +
                ggplot2::geom_vline(xintercept = res$mean, color = "green", linetype = "dotted") +
                ggplot2::labs(title = "Gamma Distribution",
                     x = "x", y = "Density") +
                ggplot2::annotate("text", x = res$mode, y = max(y) * 0.8, label = "Mode", color = "red") +
                ggplot2::annotate("text", x = res$mean, y = max(y) * 0.7, label = "Mean", color = "green") +
                ggplot2::theme_minimal()
        } else {
            plot(1, 1, type = "n", xlab = "", ylab = "", main = "Error")
            text(1, 1, res$error, col = "red", cex = 1.2)
        }
    })

    # Summary
    output$summary <- shiny::renderText({
        res <- result()
        if (is.null(res$error)) {
            paste0(
                "Mean: ", round(res$mean, 3), "\n",
                "Mode: ", round(res$mode, 3), "\n",
                "Shape (s): ", round(res$s, 3), "\n",
                "Rate (r): ", round(res$r, 3), "\n",
                "Distribution: ", res$distribution, "\n",
                if (!is.null(res$warning)) paste0("Warning: ", res$warning)
            )
        } else {
            res$error
        }
    })
}

# Run the app
shinyApp(ui = ui, server = server)
