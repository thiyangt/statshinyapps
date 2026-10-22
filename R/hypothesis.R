library(shiny)
library(ggplot2)

ui <- fluidPage(

  titlePanel("Understanding Type I Error, Type II Error and Power"),

  sidebarLayout(

    sidebarPanel(

      h4("Hypothesis"),
      HTML("<b>H<sub>0</sub>: μ ≤ 25</b>"),
      HTML("<br><b>H<sub>1</sub>: μ > 25</b>"),

      hr(),

      sliderInput(
        "alpha",
        "Significance level (α)",
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
        "Population standard deviation (σ)",
        min = 1,
        max = 20,
        value = 10,
        step = 1
      ),

      sliderInput(
        "mu",
        "True population mean (μ)",
        min = 25,
        max = 40,
        value = 30,
        step = 0.5
      ),

      hr(),

      h4("Results"),
      verbatimTextOutput("results")
    ),

    mainPanel(

      h3("Sampling distributions"),
      plotOutput("distributionPlot", height = "500px"),

      hr(),

      h3("Power as the true mean changes"),
      plotOutput("powerPlot", height = "400px")
    )
  )
)


server <- function(input, output) {

  # Critical value for X-bar
  critical_value <- reactive({

    25 + qnorm(1 - input$alpha) *
      input$sigma / sqrt(input$n)
  })


  # Type II error and power
  beta_power <- reactive({

    cval <- critical_value()

    beta <- pnorm(
      cval,
      mean = input$mu,
      sd = input$sigma / sqrt(input$n)
    )

    power <- 1 - beta

    list(
      beta = beta,
      power = power
    )
  })


  # Numerical results
  output$results <- renderText({

    bp <- beta_power()

    paste0(
      "Critical value = ", round(critical_value(), 3), "\n\n",

      "Type I error (α) = ",
      round(input$alpha, 3), "\n\n",

      "Type II error (β) = ",
      round(bp$beta, 3), "\n\n",

      "Power (1 − β) = ",
      round(bp$power, 3)
    )
  })


  # Distribution plot
  output$distributionPlot <- renderPlot({

    se <- input$sigma / sqrt(input$n)

    cval <- critical_value()

    # x-axis
    x <- seq(
      min(20, 25 - 4 * se),
      max(40, input$mu + 4 * se),
      length.out = 1000
    )

    # Null distribution at boundary μ = 25
    y0 <- dnorm(x, mean = 25, sd = se)

    # Alternative distribution
    y1 <- dnorm(x, mean = input$mu, sd = se)

    df <- data.frame(
      x = x,
      null = y0,
      alternative = y1
    )

    ggplot(df, aes(x = x)) +

      # Null distribution
      geom_line(
        aes(y = null),
        linewidth = 1
      ) +

      # Alternative distribution
      geom_line(
        aes(y = alternative),
        linewidth = 1
      ) +

      # Type I error region
      geom_area(
        data = subset(df, x >= cval),
        aes(y = null),
        alpha = 0.4
      ) +

      # Type II error region
      geom_area(
        data = subset(df, x < cval),
        aes(y = alternative),
        alpha = 0.4
      ) +

      # Critical value
      geom_vline(
        xintercept = cval,
        linetype = "dashed",
        linewidth = 1
      ) +

      # Null mean
      geom_vline(
        xintercept = 25,
        linetype = "dotted"
      ) +

      # Alternative mean
      geom_vline(
        xintercept = input$mu,
        linetype = "dotted"
      ) +

      annotate(
        "text",
        x = cval + se,
        y = max(y0) * 0.8,
        label = "Type I error\nα",
        hjust = 0
      ) +

      annotate(
        "text",
        x = (25 + input$mu) / 2,
        y = max(y1) * 0.5,
        label = "Type II error\nβ"
      ) +

      annotate(
        "text",
        x = 25,
        y = max(y0) * 1.05,
        label = "μ = 25",
        vjust = 0
      ) +

      annotate(
        "text",
        x = input$mu,
        y = max(y1) * 1.05,
        label = paste0("μ = ", input$mu),
        vjust = 0
      ) +

      labs(
        x = "Sample mean",
        y = "Density",
        title = "Null and Alternative Sampling Distributions",
        subtitle = paste0(
          "n = ", input$n,
          ", σ = ", input$sigma,
          ", α = ", input$alpha
        )
      ) +

      theme_minimal(base_size = 14)
  })


  # Power curve
  output$powerPlot <- renderPlot({

    se <- input$sigma / sqrt(input$n)

    cval <- 25 + qnorm(1 - input$alpha) * se

    mu_values <- seq(25, 40, length.out = 300)

    power <- 1 - pnorm(
      cval,
      mean = mu_values,
      sd = se
    )

    df <- data.frame(
      mu = mu_values,
      power = power
    )

    ggplot(df, aes(x = mu, y = power)) +

      geom_line(linewidth = 1.2) +

      geom_hline(
        yintercept = 0.8,
        linetype = "dashed"
      ) +

      geom_vline(
        xintercept = input$mu,
        linetype = "dashed"
      ) +

      geom_point(
        data = data.frame(
          mu = input$mu,
          power = beta_power()$power
        ),
        size = 4
      ) +

      labs(
        x = "True population mean (μ)",
        y = "Power",
        title = "Power increases as μ moves away from 25",
        subtitle = paste0(
          "n = ", input$n,
          ", σ = ", input$sigma,
          ", α = ", input$alpha
        )
      ) +

      scale_y_continuous(
        limits = c(0, 1)
      ) +

      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
