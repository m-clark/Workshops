
ui <- pageWithSidebar(
  # Application title
  # titlePanel("Bayesian Data Analysis"),
  headerPanel(
    # tags$style("h5 {color:aliceblue;}
    #               label {color:dodgerblue;"),
    HTML("<span style=\"color:#ff6eb4; font-family:'Magneto'; \">Bayesian Demo</span>

    <br> <div><h5 style='color:gray'>The following demo regards estimating a mean (ignoring estimating the variance) and how the prior distribution and likelihood combine to produce the posterior distribution. You can set the following parameters: <br><br><br>

    <ul>
    <li> Sample size: 0-250
    <li> Observed and Prior Means: 1-10
    <li> Observed and Prior Variances: 1-5
    </ul>


    The tested &theta; parameters are a sequence of 500 values from 1 to 10.
    </h5></div>")
    ),

  # Sidebar with a slider input for number of bins
  sidebarPanel(
      tags$style(".well {background-color:aliceblue;}
                  label {color:cornflowerblue; font-family:'Gill Sans MT'}"),
      numericInput("n",
                   "Data Sample Size:",
                   min = 0, max = 250, value = 10, step=10),
      numericInput("dataMean",
                   "Data Mean:",
                   min = 1, max = 10, value = 7, step=1),
      numericInput("dataVar",
                   "Data Variance:",
                   min = 1, max = 5, value = 3),
      numericInput("priorMean",
                   HTML("Prior Mean for &theta;:"),
                   min = 1, max = 10, value = 2, step=1),
      numericInput("priorVar",
                   HTML("Prior Variance for &theta;:"),
                   min = 1, max = 5, value = 1)
    ),


    mainPanel(
      plotly::plotlyOutput("bayesPlot", width = '75%'),
      htmlOutput('results', span="style=color:red"), br(),
      htmlOutput("caption"),
      tags$style("#results {color:cornflowerblue; font-variant:small-caps; font-family:'Consolas';}
                  #caption {color:gray;}")
    , width=6)
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  library(tidyverse); library(gridExtra); library(plotly)
  simN = 500
  theta = seq(1, 10, length.out = simN)

  obs  = reactive({ rnorm(input$n, input$dataMean, sqrt(input$dataVar))})
  prior = reactive({data.frame(Distribution='Prior', theta=theta,
                     density = dnorm(theta, input$priorMean, sqrt(input$priorVar)))
  })
  like = reactive({data.frame(Distribution='Likelihood', theta=theta,
                    density = sapply(theta, function(parm) exp(sum(dnorm(obs(), mean=parm, sd=input$dataVar, log = T)))))
  })
  denom = reactive({sum(like()$density*prior()$density)})
  post = reactive({data.frame(Distribution='Posterior', theta=theta,
                    density = like()$density*prior()$density/denom())
  })
  thetamean = reactive({sum(post()$density*theta)})
  plotdata = reactive({rbind(prior(), like(), post())})



  output$results = renderText({
    HTML(paste0("<span style='color:cornflowerblue'>Observed Mean = ", format(mean(obs()), digits=3, nmsall=2), '<br>',
                "Observed Var = ",  format(var(obs()), digits=3, nmsall=2), '<br>',
                "Posterior Mean = ",  format(thetamean(), digits=3, nmsall=2))
         )
  })


  output$bayesPlot = renderPlotly({

    g = ggplot(aes(x=theta, y=density, group=Distribution, color=Distribution, fill=Distribution), data=plotdata()) +
      geom_ribbon(aes(ymin=0, ymax=density), alpha=.5 ) +
      geom_point(aes(x=value, y=0), data=data.frame(Distribution=c('Prior', 'Likelihood', 'Posterior'),
                                                        value=c(input$priorMean, mean(obs()), thetamean())),
                 color=alpha('#ff5503', .25)) +
      facet_wrap(~Distribution, scales = 'free_y', ncol = 1) +
      xlab(HTML('&theta;')) +
      lims(x=c(1, 10)) +
      lazerhawk::theme_trueMinimal() +
      theme(axis.title.x=element_text(color=alpha('black',.6), vjust=.1, hjust=.5),
            axis.text.y=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            strip.text=element_text(color=alpha('black',.5), vjust=.01),
            legend.position='none')

    ggplotly(g, tooltip='none')

  })
  output$caption <- renderText({
    HTML('All three distributions regard the <i>parameter</i> to be estimated, i.e. &theta;, the mean.  The prior regards the initial distribution given for &theta;. This may be based on prior beliefs and/or research, or simply one known to work well within the modeling context.  The likelihood regards the data given a particular estimate for &theta;, and is the same that one is familiar with from standard maximum likelihood methods. Finally, the posterior is the final likelihood for the &theta; values.')
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# for testing
# input = data.frame(n=600, dataMean=5, dataVar=1, priorMean=5, priorVar=1)
# simN = 1000
# data  = rnorm(10, input$dataMean, input$dataVar)
# theta = seq(2, 8, length.out = input$n)

