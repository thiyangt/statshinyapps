# app.R

library(shiny)
library(ggplot2)

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Central Limit Theorem Demonstration"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("1. Choose a population distribution"),
      
      selectInput(
        "distribution",
        "Distribution:",
        choices = c(
          "Exponential",
          "Beta",
          "Gamma",
          "Normal",
          "Uniform",
          "Poisson",
          "Binomial"
        ),
        selected = "Exponential"
      ),
      
      uiOutput("parameters"),
      
      hr(),
      
      h4("2. Define the simulation"),
      
      numericInput(
        "sample_size",
        "Sample size (n):",
        value = 30,
        min = 2,
        max = 10000,
        step = 1
      ),
      
      numericInput(
        "num_samples",
        "Number of samples:",
        value = 1000,
        min = 10,
        max = 100000,
        step = 100
      ),
      
      actionButton(
        "simulate",
        "Run simulation",
        class = "btn-primary"
      ),
      
      actionButton(
        "reset",
        "Reset"
      ),
      
      hr(),
      
      h4("What is being demonstrated?"),
      
      p(
        "The Central Limit Theorem states that, under suitable",
        "conditions, the distribution of sample means becomes",
        "approximately Normal as the sample size increases."
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Population",
          
          br(),
          
          plotOutput(
            "populationPlot",
            height = "450px"
          ),
          
          verbatimTextOutput(
            "populationSummary"
          )
        ),
        
        tabPanel(
          "Sample Means",
          
          br(),
          
          plotOutput(
            "sampleMeansPlot",
            height = "450px"
          ),
          
          verbatimTextOutput(
            "sampleMeansSummary"
          )
        ),
        
        tabPanel(
          "Compare",
          
          br(),
          
          plotOutput(
            "comparisonPlot",
            height = "500px"
          ),
          
          tableOutput(
            "comparisonTable"
          )
        )
      )
    )
  )
)


# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------

server <- function(input, output, session) {
  
  
  # -------------------------------------------------------
  # Distribution parameters
  # -------------------------------------------------------
  
  output$parameters <- renderUI({
    
    switch(
      input$distribution,
      
      "Exponential" = tagList(
        
        numericInput(
          "rate",
          "Rate (λ):",
          value = 1,
          min = 0.01,
          step = 0.1
        )
      ),
      
      "Beta" = tagList(
        
        numericInput(
          "shape1",
          "Shape 1 (α):",
          value = 2,
          min = 0.01,
          step = 0.1
        ),
        
        numericInput(
          "shape2",
          "Shape 2 (β):",
          value = 5,
          min = 0.01,
          step = 0.1
        )
      ),
      
      "Gamma" = tagList(
        
        numericInput(
          "gamma_shape",
          "Shape:",
          value = 2,
          min = 0.01,
          step = 0.1
        ),
        
        numericInput(
          "gamma_rate",
          "Rate:",
          value = 1,
          min = 0.01,
          step = 0.1
        )
      ),
      
      "Normal" = tagList(
        
        numericInput(
          "normal_mean",
          "Mean (μ):",
          value = 10,
          step = 1
        ),
        
        numericInput(
          "normal_sd",
          "Standard deviation (σ):",
          value = 3,
          min = 0.01,
          step = 0.1
        )
      ),
      
      "Uniform" = tagList(
        
        numericInput(
          "uniform_min",
          "Minimum:",
          value = 0,
          step = 1
        ),
        
        numericInput(
          "uniform_max",
          "Maximum:",
          value = 10,
          step = 1
        )
      ),
      
      "Poisson" = tagList(
        
        numericInput(
          "lambda",
          "Lambda (λ):",
          value = 5,
          min = 0.01,
          step = 0.1
        )
      ),
      
      "Binomial" = tagList(
        
        numericInput(
          "size",
          "Number of trials:",
          value = 10,
          min = 1,
          step = 1
        ),
        
        numericInput(
          "prob",
          "Probability (p):",
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.05
        )
      )
    )
  })
  
  
  # -------------------------------------------------------
  # Generate random observations
  # -------------------------------------------------------
  
  generate_data <- function(n) {
    
    switch(
      input$distribution,
      
      "Exponential" =
        rexp(
          n,
          rate = input$rate
        ),
      
      "Beta" =
        rbeta(
          n,
          shape1 = input$shape1,
          shape2 = input$shape2
        ),
      
      "Gamma" =
        rgamma(
          n,
          shape = input$gamma_shape,
          rate = input$gamma_rate
        ),
      
      "Normal" =
        rnorm(
          n,
          mean = input$normal_mean,
          sd = input$normal_sd
        ),
      
      "Uniform" =
        runif(
          n,
          min = input$uniform_min,
          max = input$uniform_max
        ),
      
      "Poisson" =
        rpois(
          n,
          lambda = input$lambda
        ),
      
      "Binomial" =
        rbinom(
          n,
          size = input$size,
          prob = input$prob
        )
    )
  }
  
  
  # -------------------------------------------------------
  # Theoretical population mean
  # -------------------------------------------------------
  
  population_mean <- reactive({
    
    switch(
      input$distribution,
      
      "Exponential" =
        1 / input$rate,
      
      "Beta" =
        input$shape1 /
        (input$shape1 + input$shape2),
      
      "Gamma" =
        input$gamma_shape /
        input$gamma_rate,
      
      "Normal" =
        input$normal_mean,
      
      "Uniform" =
        (input$uniform_min + input$uniform_max) / 2,
      
      "Poisson" =
        input$lambda,
      
      "Binomial" =
        input$size * input$prob
    )
  })
  
  
  # -------------------------------------------------------
  # Theoretical population SD
  # -------------------------------------------------------
  
  population_sd <- reactive({
    
    switch(
      input$distribution,
      
      "Exponential" =
        1 / input$rate,
      
      "Beta" = {
        
        a <- input$shape1
        b <- input$shape2
        
        sqrt(
          a * b /
            ((a + b)^2 * (a + b + 1))
        )
      },
      
      "Gamma" =
        sqrt(input$gamma_shape) /
        input$gamma_rate,
      
      "Normal" =
        input$normal_sd,
      
      "Uniform" =
        (input$uniform_max -
           input$uniform_min) /
        sqrt(12),
      
      "Poisson" =
        sqrt(input$lambda),
      
      "Binomial" =
        sqrt(
          input$size *
            input$prob *
            (1 - input$prob)
        )
    )
  })
  
  
  # -------------------------------------------------------
  # Simulation
  # -------------------------------------------------------
  
  simulation <- eventReactive(
    input$simulate,
    {
      
      n <- input$sample_size
      B <- input$num_samples
      
      # Generate all observations
      x <- generate_data(n * B)
      
      # Convert into B samples of size n
      matrix_x <- matrix(
        x,
        nrow = B,
        ncol = n
      )
      
      # Calculate sample means
      sample_means <- rowMeans(matrix_x)
      
      list(
        population = x,
        sample_means = sample_means
      )
    },
    
    ignoreNULL = TRUE
  )
  
  
  # -------------------------------------------------------
  # Population plot
  # -------------------------------------------------------
  
  output$populationPlot <- renderPlot({
    
    sim <- simulation()
    
    req(sim)
    
    x <- sim$population
    
    ggplot(
      data.frame(x = x),
      aes(x = x)
    ) +
      
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 50,
        alpha = 0.7
      ) +
      
      geom_density(
        linewidth = 1
      ) +
      
      geom_vline(
        xintercept = population_mean(),
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        title = paste(
          "Population:",
          input$distribution,
          "distribution"
        ),
        subtitle = paste(
          "Population mean =",
          round(population_mean(), 3),
          "| Population SD =",
          round(population_sd(), 3)
        ),
        x = "Value",
        y = "Density"
      ) +
      
      theme_minimal(
        base_size = 15
      )
  })
  
  
  # -------------------------------------------------------
  # Population summary
  # -------------------------------------------------------
  
  output$populationSummary <- renderPrint({
    
    sim <- simulation()
    
    req(sim)
    
    cat(
      "Distribution:",
      input$distribution,
      "\n"
    )
    
    cat(
      "Population mean:",
      round(population_mean(), 4),
      "\n"
    )
    
    cat(
      "Population SD:",
      round(population_sd(), 4),
      "\n"
    )
  })
  
  
  # -------------------------------------------------------
  # Sample means plot
  # -------------------------------------------------------
  
  output$sampleMeansPlot <- renderPlot({
    
    sim <- simulation()
    
    req(sim)
    
    means <- sim$sample_means
    
    theoretical_mean <- population_mean()
    
    theoretical_se <-
      population_sd() /
      sqrt(input$sample_size)
    
    ggplot(
      data.frame(mean = means),
      aes(x = mean)
    ) +
      
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 40,
        alpha = 0.7,
        col="white"
      ) +
      
      geom_density(
        linewidth = 1
      ) +
      
      geom_vline(
        xintercept = theoretical_mean,
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        title = "Distribution of Sample Means",
        subtitle = paste(
          "n =",
          input$sample_size,
          "| Number of samples =",
          input$num_samples,
          "| Theoretical SE =",
          round(theoretical_se, 4)
        ),
        x = "Sample mean",
        y = "Density"
      ) +
      
      theme_minimal(
        base_size = 15
      )
  })
  
  
  # -------------------------------------------------------
  # Sample mean summary
  # -------------------------------------------------------
  
  output$sampleMeansSummary <- renderPrint({
    
    sim <- simulation()
    
    req(sim)
    
    means <- sim$sample_means
    
    theoretical_se <-
      population_sd() /
      sqrt(input$sample_size)
    
    cat(
      "Sample size (n):",
      input$sample_size,
      "\n"
    )
    
    cat(
      "Number of samples:",
      input$num_samples,
      "\n\n"
    )
    
    cat(
      "Mean of sample means:",
      round(mean(means), 4),
      "\n"
    )
    
    cat(
      "SD of sample means:",
      round(sd(means), 4),
      "\n"
    )
    
    cat(
      "Theoretical population mean:",
      round(population_mean(), 4),
      "\n"
    )
    
    cat(
      "Theoretical standard error:",
      round(theoretical_se, 4),
      "\n"
    )
  })
  
  
  # -------------------------------------------------------
  # Comparison plot
  # -------------------------------------------------------
  
  output$comparisonPlot <- renderPlot({
    
    sim <- simulation()
    
    req(sim)
    
    means <- sim$sample_means
    
    mu <- population_mean()
    
    se <- population_sd() /
      sqrt(input$sample_size)
    
    # Create theoretical normal curve
    curve_data <- data.frame(
      x = seq(
        min(means),
        max(means),
        length.out = 500
      )
    )
    
    curve_data$density <-
      dnorm(
        curve_data$x,
        mean = mu,
        sd = se
      )
    
    ggplot(
      data.frame(mean = means),
      aes(x = mean)
    ) +
      
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 40,
        alpha = 0.6,
        col="white"
      ) +
      
      geom_density(
        linewidth = 1,
        col="#1b9e77"
      ) +
      
      geom_line(
        data = curve_data,
        aes(
          x = x,
          y = density,
          col="#d95f02"
        ),
        linewidth = 1.2
      ) +
      
      geom_vline(
        xintercept = mu,
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        title = "Central Limit Theorem",
        subtitle = paste(
          "Sampling distribution of the mean",
          "| n =",
          input$sample_size
        ),
        x = "Sample mean",
        y = "Density"
      ) +
      
      theme_minimal(
        base_size = 15
      )
  })
  
  
  # -------------------------------------------------------
  # Comparison table
  # -------------------------------------------------------
  
  output$comparisonTable <- renderTable({
    
    sim <- simulation()
    
    req(sim)
    
    means <- sim$sample_means
    
    data.frame(
      
      Quantity = c(
        "Population mean",
        "Mean of sample means",
        "Population SD",
        "SD of sample means",
        "Theoretical standard error"
      ),
      
      Value = round(
        c(
          population_mean(),
          mean(means),
          population_sd(),
          sd(means),
          population_sd() /
            sqrt(input$sample_size)
        ),
        4
      )
    )
  })
  
  
  # -------------------------------------------------------
  # Reset
  # -------------------------------------------------------
  
  observeEvent(input$reset, {
    
    updateSelectInput(
      session,
      "distribution",
      selected = "Exponential"
    )
    
    updateNumericInput(
      session,
      "sample_size",
      value = 30
    )
    
    updateNumericInput(
      session,
      "num_samples",
      value = 1000
    )
  })
}


# ---------------------------------------------------------
# Run application
# ---------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)

