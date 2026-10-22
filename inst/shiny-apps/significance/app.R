
library(shiny)
library(ggplot2)

# Colour palette
col_stat <- "#1b9e77"
col_practical <- "#7570b3"
col_neutral <- "#d95f02"

ui <- fluidPage(

  titlePanel(
    "Statistical Significance vs Practical Significance"
  ),

  sidebarLayout(

    sidebarPanel(

      h4("Study settings"),

      p(
        "Imagine measuring the improvement in test scores ",
        "after introducing a new teaching programme."
      ),

      sliderInput(
        "effect",
        "Observed mean improvement",
        min = -5,
        max = 10,
        value = 1,
        step = 0.1
      ),

      sliderInput(
        "sd",
        "Sample standard deviation",
        min = 0.5,
        max = 10,
        value = 4,
        step = 0.5
      ),

      sliderInput(
        "n",
        "Sample size",
        min = 5,
        max = 2000,
        value = 30,
        step = 5
      ),

      sliderInput(
        "alpha",
        "Significance level (alpha)",
        min = 0.01,
        max = 0.10,
        value = 0.05,
        step = 0.01
      ),

      sliderInput(
        "threshold",
        "Minimum practically meaningful improvement",
        min = 0.1,
        max = 5,
        value = 1,
        step = 0.1
      ),

      helpText(
        "The practical threshold is expressed in the same ",
        "units as the measured improvement."
      ),

      actionButton("reset", "Reset settings")
    ),

    mainPanel(

      h3("1. Study results"),

      fluidRow(

        column(
          4,
          wellPanel(
            h4("Observed effect"),
            h2(textOutput("effectResult")),
            p("Estimated mean improvement")
          )
        ),

        column(
          4,
          wellPanel(
            h4("P-value"),
            h2(textOutput("pResult")),
            p("Evidence against the null hypothesis")
          )
        ),

        column(
          4,
          wellPanel(
            h4("95% confidence interval"),
            h4(textOutput("ciResult")),
            p("Uncertainty in the estimated effect")
          )
        )
      ),

      h3("2. Two different questions"),

      fluidRow(

        column(
          6,
          wellPanel(
            h4("Statistical significance"),
            uiOutput("statDecision")
          )
        ),

        column(
          6,
          wellPanel(
            h4("Practical significance"),
            uiOutput("practicalDecision")
          )
        )
      ),

      h3("3. Visualise the estimated effect"),

      p(
        "The purple band represents effects considered too small ",
        "to be practically meaningful. The vertical black line ",
        "represents zero effect."
      ),

      plotOutput("effectPlot", height = "300px"),

      h3("4. How do sample size and effect size matter?"),

      p(
        "Explore how the p-value changes with sample size ",
        "while holding the observed mean improvement and ",
        "standard deviation fixed."
      ),

      plotOutput("samplePlot", height = "350px"),

      uiOutput("interpretation"),

      hr(),

      h4("Key lesson"),

      tags$blockquote(
        "A small p-value does not necessarily mean that an ",
        "effect is large or important. Always consider the ",
        "size of the effect, its uncertainty, and whether ",
        "it matters in practice."
      )
    )
  )
)


server <- function(input, output, session) {

  observeEvent(input$reset, {

    updateSliderInput(session, "effect", value = 1)
    updateSliderInput(session, "sd", value = 4)
    updateSliderInput(session, "n", value = 30)
    updateSliderInput(session, "alpha", value = 0.05)
    updateSliderInput(session, "threshold", value = 1)

  })


  # Calculate the one-sample t-test against zero
  results <- reactive({

    n <- input$n
    effect <- input$effect
    s <- input$sd
    df <- n - 1

    se <- s / sqrt(n)

    t_stat <- effect / se

    p_value <- 2 * pt(
      -abs(t_stat),
      df = df
    )

    critical_t <- qt(
      1 - input$alpha / 2,
      df = df
    )

    ci_lower <- effect - critical_t * se
    ci_upper <- effect + critical_t * se

    statistically_significant <-
      p_value < input$alpha

    practically_meaningful <-
      abs(effect) >= input$threshold

    # Is the CI entirely outside the negligible-effect region?
    ci_practically_meaningful <-
      ci_lower > input$threshold ||
      ci_upper < -input$threshold

    list(
      n = n,
      df = df,
      effect = effect,
      sd = s,
      se = se,
      t = t_stat,
      p = p_value,
      lower = ci_lower,
      upper = ci_upper,
      significant = statistically_significant,
      practical = practically_meaningful,
      ci_practical = ci_practically_meaningful
    )

  })


  output$effectResult <- renderText({
    sprintf("%.2f", results()$effect)
  })

  output$pResult <- renderText({
    p <- results()$p

    if (p < 0.0001) {
      "< 0.0001"
    } else {
      sprintf("%.4f", p)
    }
  })

  output$ciResult <- renderText({
    r <- results()

    sprintf(
      "[%.2f, %.2f]",
      r$lower,
      r$upper
    )
  })


  # Statistical significance decision
  output$statDecision <- renderUI({

    r <- results()

    if (r$significant) {

      tagList(
        tags$h3(
          "Statistically significant",
          style = paste0(
            "color:", col_stat, ";"
          )
        ),
        p(
          paste0(
            "p-value = ",
            format.pval(r$p, digits = 4),
            " < alpha = ",
            input$alpha,
            "."
          )
        ),
        p(
          "Reject the null hypothesis that the ",
          "population mean improvement is zero."
        )
      )

    } else {

      tagList(
        tags$h3(
          "Not statistically significant",
          style = paste0(
            "color:", col_neutral, ";"
          )
        ),
        p(
          paste0(
            "p-value = ",
            format.pval(r$p, digits = 4),
            " >= alpha = ",
            input$alpha,
            "."
          )
        ),
        p(
          "There is insufficient evidence to reject ",
          "the null hypothesis at this significance level."
        )
      )

    }

  })


  # Practical significance decision
  output$practicalDecision <- renderUI({

    r <- results()

    if (r$practical) {

      tagList(
        tags$h3(
          "Meaningful effect size",
          style = paste0(
            "color:", col_practical, ";"
          )
        ),
        p(
          paste0(
            "|Observed effect| = ",
            round(abs(r$effect), 2),
            " >= ",
            input$threshold,
            "."
          )
        ),
        p(
          "The observed effect reaches the predefined ",
          "threshold for practical importance."
        )
      )

    } else {

      tagList(
        tags$h3(
          "Small effect size",
          style = paste0(
            "color:", col_neutral, ";"
          )
        ),
        p(
          paste0(
            "|Observed effect| = ",
            round(abs(r$effect), 2),
            " < ",
            input$threshold,
            "."
          )
        ),
        p(
          "The observed effect does not reach the ",
          "predefined threshold for practical importance."
        )
      )

    }

  })


  # Effect size and confidence interval plot
  output$effectPlot <- renderPlot({

    r <- results()
    threshold <- input$threshold

    xmin <- min(
      r$lower, -threshold, -1
    )

    xmax <- max(
      r$upper, threshold, 1
    )

    padding <- 0.12 * (xmax - xmin)

    xmin <- xmin - padding
    xmax <- xmax + padding

    df <- data.frame(
      effect = r$effect,
      lower = r$lower,
      upper = r$upper,
      y = 1
    )

    ggplot() +

      # Practically negligible region
      annotate(
        "rect",
        xmin = -threshold,
        xmax = threshold,
        ymin = 0.5,
        ymax = 1.5,
        fill = col_practical,
        alpha = 0.18
      ) +

      # Zero-effect reference
      geom_vline(
        xintercept = 0,
        colour = "black",
        linetype = "dashed"
      ) +

      # Practical importance boundaries
      geom_vline(
        xintercept = c(-threshold, threshold),
        colour = col_practical,
        linetype = "dotted",
        linewidth = 1
      ) +

      # Confidence interval
      geom_segment(
        data = df,
        aes(
          x = lower,
          xend = upper,
          y = y,
          yend = y
        ),
        linewidth = 2,
        colour = col_stat
      ) +

      # Point estimate
      geom_point(
        data = df,
        aes(x = effect, y = y),
        size = 5,
        colour = col_stat
      ) +

      annotate(
        "text",
        x = 0,
        y = 1.38,
        label = "Zero effect",
        size = 3.5
      ) +

      annotate(
        "text",
        x = 0,
        y = 0.66,
        label = "Practically negligible region",
        size = 3.5,
        colour = col_practical
      ) +

      scale_x_continuous(
        limits = c(xmin, xmax)
      ) +

      scale_y_continuous(
        breaks = NULL
      ) +

      labs(
        x = "Mean improvement and confidence interval",
        y = NULL,
        title = "Estimated effect with 95% confidence interval"
      ) +

      theme_minimal(base_size = 13) +

      theme(
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )

  })


  # P-value against sample size
  output$samplePlot <- renderPlot({

    n_values <- unique(
      round(
        exp(seq(
          log(5),
          log(2000),
          length.out = 250
        ))
      )
    )

    p_values <- sapply(n_values, function(n) {

      t_stat <- input$effect /
        (input$sd / sqrt(n))

      2 * pt(
        -abs(t_stat),
        df = n - 1
      )

    })

    df <- data.frame(
      n = n_values,
      p = p_values
    )

    ggplot(df, aes(x = n, y = p)) +

      geom_line(
        colour = col_stat,
        linewidth = 1.2
      ) +

      geom_hline(
        yintercept = input$alpha,
        colour = col_neutral,
        linetype = "dashed"
      ) +

      geom_point(
        data = data.frame(
          n = input$n,
          p = results()$p
        ),
        size = 4,
        colour = col_stat
      ) +

      scale_x_log10() +

      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.2)
      ) +

      labs(
        x = "Sample size (log scale)",
        y = "Two-sided p-value",
        title = "Effect of sample size on statistical significance",
        subtitle = paste0(
          "Observed effect = ", input$effect,
          "; standard deviation = ", input$sd,
          "; dashed line = alpha = ", input$alpha
        )
      ) +

      theme_minimal(base_size = 13)

  })


  # Dynamic interpretation
  output$interpretation <- renderUI({

    r <- results()

    if (r$significant && !r$practical) {

      message <- paste(
        "Statistically significant but not practically meaningful:",
        "the data provide evidence of a non-zero mean effect,",
        "but the estimated effect is smaller than the practical",
        "importance threshold."
      )

    } else if (!r$significant && r$practical) {

      message <- paste(
        "Practically meaningful estimated effect, but not",
        "statistically significant. The observed effect reaches",
        "the practical threshold, but the evidence is too",
        "uncertain to reject the zero-effect hypothesis."
      )

    } else if (r$significant && r$practical) {

      message <- paste(
        "Both statistically significant and practically meaningful:",
        "the estimated effect exceeds the practical threshold",
        "and there is evidence against a zero population effect."
      )

    } else {

      message <- paste(
        "Neither statistically significant nor practically meaningful",
        "according to the selected criteria. This does not prove",
        "that the true effect is zero."
      )

    }

    wellPanel(
      h4("What do these results mean?"),
      p(message),
      p(
        strong("Confidence interval check: "),
        if (r$lower > input$threshold ||
            r$upper < -input$threshold) {
          "The entire 95% confidence interval lies beyond the practical threshold."
        } else if (r$lower >= -input$threshold &&
                   r$upper <= input$threshold) {
          "The entire 95% confidence interval lies within the practically negligible region."
        } else {
          "The 95% confidence interval overlaps the practical threshold region; practical importance remains uncertain."
        }
      )
    )

  })

}

shinyApp(ui = ui, server = server)
