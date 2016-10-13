
ui <- fluidPage(

  # Application title
  titlePanel("Bayesian Data Analysis"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("n",
                   "Data Sample Size:",
                   min = 1, max = 250, value = 10, step=10),
      numericInput("dataMean",
                   "Data Mean:",
                   min = 1, max = 10, value = 7, step=1),
      numericInput("dataVar",
                   "Data Variance:",
                   min = 1, max = 5, value = 3),
      numericInput("priorMean",
                   "Prior Mean:",
                   min = 1, max = 10, value = 2, step=1),
      numericInput("priorVar",
                   "Prior Variance:",
                   min = 1, max = 10, value = 1)
    ),


    mainPanel(
      # plotOutput("bayesPlot", width='50%'), width = 4
      plotlyOutput("bayesPlot", width = '50%')
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  library(tidyverse); library(gridExtra); library(plotly)
  simN = 500
  theta = seq(1, 10, length.out = simN)

  output$bayesPlot <- renderPlotly({
    obs  = rnorm(input$n, input$dataMean, sqrt(input$dataVar))
    prior = data.frame(Distribution='Prior', theta=theta,
                       density = dnorm(theta, input$priorMean, sqrt(input$priorVar)))
    like = data.frame(Distribution='Likelihood', theta=theta,
                      density = sapply(theta, function(parm) exp(sum(dnorm(obs, mean=parm, sd=input$dataVar, log = T)))))
    denom = sum(like$density*prior$density)
    post = data.frame(Distribution='Posterior', theta=theta,
                      density = like$density*prior$density)

    d = rbind(prior, like, post)

    g = ggplot(aes(x=theta, y=density, group=Distribution, color=Distribution, fill=Distribution), data=d) +
        geom_ribbon(aes(ymin=0, ymax=density), alpha=.5 ) +
        facet_wrap(~Distribution, scales = 'free_y', ncol = 1) +
        lims(x=c(1, 10)) +
        lazerhawk::theme_trueMinimal() +
        theme(axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position='none')

    ggplotly(g)

  })
}

# Run the application
shinyApp(ui = ui, server = server)

# for testing
# input = data.frame(n=600, dataMean=5, dataVar=1, priorMean=5, priorVar=1)
# simN = 1000
# data  = rnorm(10, input$dataMean, input$dataVar)
# theta = seq(2, 8, length.out = input$n)

