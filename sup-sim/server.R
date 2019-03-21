library(shiny)
library(ggplot2)

# Create populations from 10 to 10000, using a log10 scale
pop <-  round(10 ^ seq(1, 4, by = 0.03))
base <- 5

# Functions for base 5 flooring and ceiling
floor_ <- function(num, base) { floor(num / base) * base }
ceil_ <- function(num, base) { ceiling(num / base) * base }

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$sim <- renderPlot({
    
    denum <- pop
    num <- (input$prev / 100) * denum
    prev <- num / denum
    
    
    # Classical rounding
    # Aléatoire
    # Controlled
    
    worst_lo = floor_(num, base) / ceil_(denum, base)
    worst_hi = ceil_(num, base) / floor_(denum, base)
    
    ggplot(data.frame(num, denum, prev)) +
      aes(x=denum) +
      geom_point(aes(y = prev), colour = 'red') +
      geom_point(aes(y = worst_lo), colour = 'blue') +
      geom_point(aes(y = worst_hi), colour = 'green') +
      scale_x_log10() +
      xlab("Population size (from 1 to 10000)") +
      ylab("Prevalence in the population")
  })
})
