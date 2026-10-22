
library(shiny)
library(ggplot2)

# Consistent colour palette
col_type1  <- "#d95f02"
col_type2  <- "#7570b3"
col_power  <- "#1b9e77"

# --------------------------------------------------
# User interface
# --------------------------------------------------

ui <- fluidPage(

  titlePanel("Understanding Type I Error, Type II Error and Power"),

  sidebarLayout(

    sidebarPanel(

      h4("Hypothesis test"),

      tags$p(
        tags$b("H\u2080: \u03BC \u2264 25")
      ),
      tags$p(
        tags$b("H\u2081: \u03BC > 25")
      ),

      tags$p(
        "Assume independent observations from a normal population ",
        "with known population standard deviation."
      ),

      hr(),

      sliderInput(
        "alpha",
        "Significance level (\u03B1)",
        min = 0.01,
        max = 0.20,
        value = 0.05,
        step = 0.01
      ),

      sliderInput(
        "n",
        "Sample size (n)",
        min = 5,
        max = 200,
        value = 25,
        step = 5
      ),

      sliderInput(
        "sigma",
        "Population standard deviation (\u03C3)",
        min = 1,
        max = 20,
        value = 10,
        step = 1
      ),

      sliderInput(
        "mu",
        "True population mean (\u03BC)",
        min = 20,
        max = 40,
        value = 30,
        step = 0.5
      ),

      hr(),

      h4("Interpret the results"),

      uiOutput("decision"),

      actionButton(
        "reset",
        "Reset settings",
        icon = icon("rotate-left")
      )
    ),

    mainPanel(

      h3("1. Sampling distributions"),

      plotOutput(
        "distributionPlot",
        height = "480px"
      ),

      tags$p(
        tags$span(
          style = paste0(
            "color:", col_type1, "; font-weight:bold;"
          ),
          "\u25A0 Type I error (\u03B1)"
        ),
        "  ",
        tags$span(
          style = paste0(
            "color:", col_type2, "; font-weight:bold;"
          ),
          "\u25A0 Type II error (\u03B2)"
        ),
        "  ",
        tags$span(
          style = paste0(
            "color:", col_power, "; font-weight:bold;"
          ),
          "\u25A0 Power (1 \u2212 \u03B2)"
        )
      ),

      hr(),

      h3("2. Numerical results"),

      fluidRow(
        column(
          4,
          wellPanel(
            h4("Type I error"),
            h2(
              textOutput("alphaResult"),
              style = paste0(
                "color:", col_type1, ";"
              )
            ),
            p("Probability of rejecting H\u2080 at the boundary \u03BC = 25.")
          )
        ),

        column(
          4,
          wellPanel(
            h4("Type II error"),
            h2(
              textOutput("betaResult"),
              style = paste0(
                "color:", col_type2, ";"
              )
            ),
            p("Probability of failing to reject H\u2080 at the selected true mean.")
          )
        ),

        column(
          4,
          wellPanel(
            h4("Power"),
            h2(
              textOutput("powerResult"),
              style = paste0(
                "color:", col_power, ";"
              )
            ),
            p("Probability of correctly rejecting H\u2080 at the selected true mean.")
          )
        )
      ),

      p(
        strong("Critical value for the sample mean: "),
        textOutput("criticalResult", inline = TRUE)
      ),

      hr(),

      h3("3. Power curve"),

      p(
        "Explore how power changes as the true population mean changes. ",
        "The vertical line marks the selected true mean."
      ),

      plotOutput(
        "powerPlot",
        height = "400px"
      ),

      hr(),

      h3("4. Compare the effect of different factors"),

      selectInput(
        "factor",
        "Choose a factor to investigate",
        choices = c(
          "Sample size" = "n",
          "Population standard deviation" = "sigma",
          "Significance level" = "alpha",
          "True population mean" = "mu"
        )
      ),

      plotOutput(
        "factorPlot",
        height = "400px"
      ),

      uiOutput("factorExplanation")
    )
  )
)


# --------------------------------------------------
# Server
# --------------------------------------------------

server <- function(input, output, session) {

  # Reset controls
  observeEvent(input$reset, {

    updateSliderInput(
      session, "alpha", value = 0.05
    )

    updateSliderInput(
      session, "n", value = 25
    )

    updateSliderInput(
      session, "sigma", value = 10
    )

    updateSliderInput(
      session, "mu", value = 30
    )

  })


  # Standard error
  se <- reactive({
    input$sigma / sqrt(input$n)
  })


  # Upper-tail critical value
  critical_value <- reactive({
    25 + qnorm(1 - input$alpha) * se()
  })


  # Type II error and power at the selected true mean
  error_probabilities <- reactive({

    cv <- critical_value()

    beta <- pnorm(
      cv,
      mean = input$mu,
      sd = se()
    )

    power <- 1 - beta

    list(
      alpha = input$alpha,
      beta = beta,
      power = power
    )
  })


  # Numerical outputs
  output$alphaResult <- renderText({
    sprintf("%.4f (%.1f%%)",
            error_probabilities()$alpha,
            100 * error_probabilities()$alpha)
  })

  output$betaResult <- renderText({
    sprintf("%.4f (%.1f%%)",
            error_probabilities()$beta,
            100 * error_probabilities()$beta)
  })

  output$powerResult <- renderText({
    sprintf("%.4f (%.1f%%)",
            error_probabilities()$power,
            100 * error_probabilities()$power)
  })

  output$criticalResult <- renderText({
    sprintf("%.3f", critical_value())
  })


  # Decision and interpretation
  output$decision <- renderUI({

    p <- error_probabilities()

    tagList(
      p(
        strong("Critical value: "),
        round(critical_value(), 3)
      ),

      p(
        strong("Power: "),
        paste0(round(100 * p$power, 1), "%")
      ),

      if (input$mu <= 25) {
        p(
          style = "color:#b45309;",
          "The selected mean is in the null hypothesis. ",
          "Power is not the appropriate interpretation here; ",
          "the rejection probability is at most \u03B1."
        )
      } else {
        p(
          style = paste0(
            "color:", col_power, "; font-weight:bold;"
          ),
          if (p$power >= 0.8) {
            "Power is at least 80% for this alternative."
          } else {
            "Power is below 80% for this alternative."
          }
        )
      }
    )
  })


  # ------------------------------------------------
  # Sampling distributions
  # ------------------------------------------------

  output$distributionPlot <- renderPlot({

    s <- se()
    cv <- critical_value()

    # Include both means and the critical value
    x_min <- min(25 - 4 * s, input$mu - 4 * s)
    x_max <- max(25 + 4 * s, input$mu + 4 * s, cv + 0.5 * s)

    x <- seq(x_min, x_max, length.out = 1500)

    df <- data.frame(
      x = x,
      null = dnorm(x, mean = 25, sd = s),
      alternative = dnorm(x, mean = input$mu, sd = s)
    )

    # The alternative density is split into beta and power.
    df$beta <- ifelse(
      df$x <= cv, df$alternative, NA_real_
    )

    df$power <- ifelse(
      df$x >= cv, df$alternative, NA_real_
    )

    ggplot(df, aes(x = x)) +

      # Type I error: right tail under H0
      geom_area(
        data = subset(df, x >= cv),
        aes(y = null),
        fill = col_type1,
        alpha = 0.65
      ) +

      # Type II error: left side under H1
      geom_area(
        data = subset(df, x <= cv),
        aes(y = alternative),
        fill = col_type2,
        alpha = 0.65
      ) +

      # Power: right side under H1
      geom_area(
        data = subset(df, x >= cv),
        aes(y = alternative),
        fill = col_power,
        alpha = 0.65
      ) +

      # Distribution curves
      geom_line(
        aes(y = null, colour = "H0: mu = 25"),
        linewidth = 1.1
      ) +

      geom_line(
        aes(y = alternative, colour = "H1: selected mu"),
        linewidth = 1.1
      ) +

      geom_vline(
        xintercept = cv,
        linetype = "dashed",
        linewidth = 0.9
      ) +

      geom_vline(
        xintercept = 25,
        linetype = "dotted"
      ) +

      geom_vline(
        xintercept = input$mu,
        linetype = "dotted"
      ) +

      annotate(
        "text",
        x = cv + 0.3 * s,
        y = dnorm(cv + 0.3 * s, 25, s) * 1.5,
        label = "Type I error\n\u03B1",
        colour = col_type1,
        fontface = "bold",
        hjust = 0,
        size = 4
      ) +

      annotate(
        "text",
        x = cv - 0.8 * s,
        y = max(df$alternative) * 0.7,
        label = "Type II error\n\u03B2",
        colour = col_type2,
        fontface = "bold",
        size = 4
      ) +

      annotate(
        "text",
        x = cv + 0.7 * s,
        y = max(df$alternative) * 0.35,
        label = "Power\n1 - \u03B2",
        colour = col_power,
        fontface = "bold",
        size = 4
      ) +

      scale_colour_manual(
        name = "Sampling distribution",
        values = c(
          "H0: mu = 25" = "black",
          "H1: selected mu" = "#666666"
        )
      ) +

      labs(
        x = "Sample mean",
        y = "Density",
        title = "Sampling distributions under H0 and H1",
        subtitle = paste0(
          "n = ", input$n,
          " | \u03C3 = ", input$sigma,
          " | \u03B1 = ", input$alpha,
          " | Critical value = ", round(cv, 3)
        )
      ) +

      theme_minimal(base_size = 13) +

      theme(
        legend.position = "bottom"
      )
  })


  # ------------------------------------------------
  # Power curve
  # ------------------------------------------------

  output$powerPlot <- renderPlot({

    s <- se()
    cv <- critical_value()

    mu_values <- seq(20, 40, length.out = 400)

    power_values <- 1 - pnorm(
      cv,
      mean = mu_values,
      sd = s
    )

    df <- data.frame(
      mu = mu_values,
      power = power_values
    )

    ggplot(df, aes(x = mu, y = power)) +

      geom_line(
        colour = col_power,
        linewidth = 1.2
      ) +

      geom_hline(
        yintercept = 0.8,
        linetype = "dashed",
        colour = "grey45"
      ) +

      geom_vline(
        xintercept = input$mu,
        linetype = "dashed"
      ) +

      geom_point(
        data = data.frame(
          mu = input$mu,
          power = error_probabilities()$power
        ),
        aes(x = mu, y = power),
        colour = col_power,
        size = 4
      ) +

      annotate(
        "text",
        x = 39,
        y = 0.83,
        label = "80% power",
        hjust = 1,
        colour = "grey35"
      ) +

      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2)
      ) +

      labs(
        x = "True population mean (\u03BC)",
        y = "Power",
        title = "Power increases as the true mean moves above 25"
      ) +

      theme_minimal(base_size = 13)
  })


  # ------------------------------------------------
  # Compare effects of the four factors
  # ------------------------------------------------

  output$factorPlot <- renderPlot({

    a <- input$alpha
    n0 <- input$n
    sig <- input$sigma
    mu0 <- input$mu

    factor <- input$factor

    if (factor == "n") {

      values <- seq(5, 200, by = 5)
      labels <- "Sample size (n)"

      power <- sapply(values, function(nn) {

        ss <- sig / sqrt(nn)
        cv <- 25 + qnorm(1 - a) * ss

        1 - pnorm(cv, mean = mu0, sd = ss)
      })

    } else if (factor == "sigma") {

      values <- seq(1, 20, by = 0.5)
      labels <- "Population standard deviation (\u03C3)"

      power <- sapply(values, function(sigma0) {

        ss <- sigma0 / sqrt(n0)
        cv <- 25 + qnorm(1 - a) * ss

        1 - pnorm(cv, mean = mu0, sd = ss)
      })

    } else if (factor == "alpha") {

      values <- seq(0.01, 0.20, by = 0.01)
      labels <- "Significance level (\u03B1)"

      power <- sapply(values, function(alpha0) {

        ss <- sig / sqrt(n0)
        cv <- 25 + qnorm(1 - alpha0) * ss

        1 - pnorm(cv, mean = mu0, sd = ss)
      })

    } else {

      values <- seq(20, 40, by = 0.25)
      labels <- "True population mean (\u03BC)"

      ss <- sig / sqrt(n0)
      cv <- 25 + qnorm(1 - a) * ss

      power <- 1 - pnorm(
        cv,
        mean = values,
        sd = ss
      )
    }

    df <- data.frame(
      value = values,
      power = power
    )

    selected_value <- switch(
      factor,
      n = n0,
      sigma = sig,
      alpha = a,
      mu = mu0
    )

    ggplot(df, aes(x = value, y = power)) +

      geom_line(
        colour = col_power,
        linewidth = 1.2
      ) +

      geom_point(
        data = data.frame(
          value = selected_value,
          power = 1 - pnorm(
            critical_value(),
            mean = input$mu,
            sd = se()
          )
        ),
        colour = col_power,
        size = 3.5
      ) +

      geom_hline(
        yintercept = 0.8,
        linetype = "dashed",
        colour = "grey45"
      ) +

      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2)
      ) +

      labs(
        x = labels,
        y = "Power",
        title = paste("Effect of", labels, "on power")
      ) +

      theme_minimal(base_size = 13)
  })


  # Explanations for factor comparisons
  output$factorExplanation <- renderUI({

    factor <- input$factor

    message <- switch(
      factor,

      n = paste(
        "Increasing sample size reduces the standard error",
        "\u03C3/\u221An. The sampling distributions become narrower,",
        "so power increases when the true mean is above 25."
      ),

      sigma = paste(
        "Increasing population standard deviation increases the",
        "standard error. The distributions overlap more, so power",
        "decreases for the selected alternative mean above 25."
      ),

      alpha = paste(
        "Increasing the significance level moves the critical value",
        "to the left. The rejection region becomes larger and power",
        "increases, but the probability of a Type I error also increases."
      ),

      mu = paste(
        "As the true mean moves farther above 25, the alternative",
        "distribution separates from the null boundary distribution.",
        "Power increases and Type II error decreases."
      )
    )

    wellPanel(
      p(message)
    )
  })

}

shinyApp(ui = ui, server = server)
