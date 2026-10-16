# app.R

library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("🎲 Tossing a Die: Empirical Probability"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Simulation"),
      
      sliderInput(
        inputId = "n",
        label = "Number of tosses:",
        min = 10,
        max = 10000,
        value = 100,
        step = 10
      ),
      
      actionButton(
        inputId = "toss",
        label = "Toss the Die",
        class = "btn-primary"
      ),
      
      actionButton(
        inputId = "reset",
        label = "Reset"
      ),
      
      hr(),
      
      h4("Choose an outcome"),
      
      selectInput(
        inputId = "face",
        label = "Track the probability of:",
        choices = 1:6,
        selected = 6
      ),
      
      hr(),
      
      h4("Theoretical probability"),
      
      div(
        style = "font-size: 24px;",
        "P(X = x) = 1/6 ≈ 0.1667"
      )
    ),
    
    mainPanel(
      
      h3("Empirical probabilities"),
      
      plotOutput(
        outputId = "probabilityPlot",
        height = "400px"
      ),
      
      hr(),
      
      h3("Convergence of empirical probability"),
      
      plotOutput(
        outputId = "convergencePlot",
        height = "400px"
      ),
      
      hr(),
      
      h3("Simulation results"),
      
      tableOutput("resultsTable")
    )
  )
)


server <- function(input, output, session) {
  
  # Store the simulated die rolls
  rolls <- reactiveVal(integer(0))
  
  
  # Store the history of empirical probabilities
  history <- reactiveVal(
    data.frame(
      toss = numeric(0),
      probability = numeric(0)
    )
  )
  
  
  # Toss the die
  observeEvent(input$toss, {
    
    new_rolls <- sample(
      1:6,
      size = input$n,
      replace = TRUE
    )
    
    rolls(new_rolls)
    
    # Calculate cumulative probability for selected face
    selected_face <- as.numeric(input$face)
    
    cumulative <- cumsum(new_rolls == selected_face)
    toss_number <- seq_along(new_rolls)
    
    new_history <- data.frame(
      toss = toss_number,
      probability = cumulative / toss_number
    )
    
    history(new_history)
  })
  
  
  # Reset
  observeEvent(input$reset, {
    
    rolls(integer(0))
    
    history(
      data.frame(
        toss = numeric(0),
        probability = numeric(0)
      )
    )
    
    updateSliderInput(
      session,
      "n",
      value = 100
    )
  })
  
  
  # Empirical probability plot
  output$probabilityPlot <- renderPlot({
    
    req(length(rolls()) > 0)
    
    x <- rolls()
    
    counts <- table(
      factor(x, levels = 1:6)
    )
    
    probabilities <- as.numeric(counts) / length(x)
    
    plot_data <- data.frame(
      face = factor(1:6),
      probability = probabilities
    )
    
    ggplot(
      plot_data,
      aes(x = face, y = probability)
    ) +
      geom_col(width = 0.65) +
      
      geom_hline(
        yintercept = 1 / 6,
        linetype = "dashed",
        linewidth = 1
      ) +
      
      geom_text(
        aes(
          label = sprintf("%.3f", probability)
        ),
        vjust = -0.5,
        size = 5
      ) +
      
      scale_y_continuous(
        limits = c(0, 0.3),
        breaks = seq(0, 0.3, 0.05)
      ) +
      
      labs(
        x = "Die face",
        y = "Empirical probability",
        title = paste(
          "Empirical probabilities after",
          length(x),
          "tosses"
        ),
        subtitle = "Dashed line = theoretical probability (1/6)"
      ) +
      
      theme_minimal(base_size = 15)
  })
  
  
  # Convergence plot
  output$convergencePlot <- renderPlot({
    
    h <- history()
    
    req(nrow(h) > 0)
    
    theoretical <- 1 / 6
    
    ggplot(
      h,
      aes(x = toss, y = probability)
    ) +
      
      geom_line(linewidth = 1) +
      
      geom_hline(
        yintercept = theoretical,
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        x = "Number of tosses",
        y = "Empirical probability",
        title = paste(
          "Convergence for outcome",
          input$face
        ),
        subtitle = "Dashed line = theoretical probability (1/6)"
      ) +
      
      scale_y_continuous(
        limits = c(0, 1)
      ) +
      
      theme_minimal(base_size = 15)
  })
  
  
  # Results table
  output$resultsTable <- renderTable({
    
    req(length(rolls()) > 0)
    
    x <- rolls()
    
    counts <- table(
      factor(x, levels = 1:6)
    )
    
    probabilities <- as.numeric(counts) / length(x)
    
    data.frame(
      `Die face` = 1:6,
      `Frequency` = as.numeric(counts),
      `Empirical probability` =
        round(probabilities, 4),
      `Theoretical probability` =
        round(rep(1 / 6, 6), 4),
      check.names = FALSE
    )
  })
}


shinyApp(ui = ui, server = server)