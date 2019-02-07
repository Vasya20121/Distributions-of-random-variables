library(shiny)
library(ggplot2)
library(ggfortify)
ui <- fluidPage(
  titlePanel("Distributions  "),
  withMathJax(),
    sidebarLayout(
      sidebarPanel(
        uiOutput("normal"),
        uiOutput("binom"),
        uiOutput("beta"),
        uiOutput("hypergeom"),
        selectInput(inputId = "distributions",
          label = strong("Distributions"),
          choices = c("Normal","Binomial", "Beta",
                      "Hypergeometric"),
          selected = "Normal"),
        uiOutput("formula"),
        radioButtons(inputId = "type_graphic",
          label = strong("PDF/PMF or CDF"),
          choices = c("PDF/PMF", "CDF")
          ),
        uiOutput("examples")
        ),
    mainPanel(
      plotOutput("plot", click = "plot_click"),
      verbatimTextOutput("info"),
      tableOutput("results")
      )
    )
)
server <- function(input, output){
  alpha <- reactive({
    input$shape1
  })
  beta <- reactive({
    input$shape2
  })
  Ex_beta <- reactive({
    alpha()/(alpha()+beta())
  })
  Vx_beta <- reactive({
    alpha()*beta()/(((alpha() + beta())^2)*(alpha()+beta()+1))
  })
  Ex_binom <- reactive({
    input$size_bin*input$prob_bin
  })
  Vx_binom <- reactive({
    input$size_bin*input$prob_bin*(1-input$prob_bin)
  })
  n_hyp <- reactive({
    input$n_hyper - input$m_hyper
  })
  N <- reactive({
    input$n_hyper
  })
  m <- reactive({
    input$m_hyper
  })
  n <- reactive({
    input$k_hyper
  })
  Ex_hyp <- reactive({
    n()*m()/N()
  })
  Vx_hyp <- reactive({
    n()*m()*(N()-n())*(N()-m())/((N()^2)*(N()-1))
  })
  output$normal <- renderUI({
    if(input$distributions == "Normal"){
    list(
      div(style="display: inline-block;vertical-align:top; ",
          numericInput(inputId = "x_norm1",
                       label = "$$x_1$$",
                       value = 0)),
      div(style="display: inline-block;vertical-align:top; ",
          numericInput(inputId = "x_norm2",
                       label = "$$x_n$$",
                       value = 10)),
      numericInput(inputId = "mean_norm",
                  label = "$$\\mu$$",
                  value = 5),
      numericInput(inputId = "sd_norm",
                  label = "$$\\sigma$$",
                  value = 2)
  )
    }
    })

  output$binom <- renderUI({
      if(input$distributions == "Binomial"){
      list(
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            numericInput(inputId = "x_bin1",
                         label = "$$x_1$$",
                         value = 0)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            numericInput(inputId = "x_bin2",
                         label = "$$x_n$$",
                         value = 10)),
        numericInput(inputId = "size_bin",
                    label = "n",
                    value = 10),
        numericInput(inputId = "prob_bin",
                    label = "p",
                    value = 0.6)
      )
  }
  })
  output$beta <- renderUI({
    if(input$distributions == "Beta"){
      list(
        if(input$type_graphic == "PDF/PMF"){
        sliderInput(inputId = "x_beta",
                    label = "x",
                    value = c(0, 1), min = 0, max = 1)
          }
        else if(input$type_graphic == "CDF"){
          sliderInput(inputId = "q_beta",
                      label = "q",
                      value = c(0, 1), min = 0, max = 1)
        },
        numericInput(inputId = "shape1",
                     label = "$$\\alpha$$",
                     value = 7),
        numericInput(inputId = "shape2",
                     label = "$$\\beta$$",
                     value = 3)
      )
    }
  })
  output$hypergeom <- renderUI({
    if(input$distributions == "Hypergeometric"){
      list(
        if(input$type_graphic == "PDF/PMF"){
          list(div(style="display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "x_hyper1",
                           label = "$$x_1$$",
                           value = 0)),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "x_hyper2",
                           label = "$$x_n$$",
                           value = 5)))
        }
        else if(input$type_graphic == "CDF"){
          list(div(style="display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "q_hyper1",
                           label = "$$x_1$$",
                           value = 0)),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              numericInput(inputId = "q_hyper2",
                           label = "$$x_n$$",
                           value = 5)))
        },
        numericInput(inputId = "m_hyper",
                     label = "m",
                     value = 5),
        numericInput(inputId = "n_hyper",
                     label = "N",
                     value = 30),
        numericInput(inputId = "k_hyper",
                     label = "n",
                     value = 14)
      )
    }
  })
    
  output$formula <- renderUI({
      if(input$distributions == "Binomial"){
        if(input$type_graphic == "PDF/PMF"){
        return(withMathJax(helpText('$$
  f_x(x) =\\binom{N}{k} \\cdot p^kq^{N-k}
                             $$')))
        }
        else if(input$type_graphic == "CDF"){
          return(withMathJax(helpText('$$F_x(x) = I_{1-p}(n-x, x+1)$$
')))
        }
      }
      else if(input$distributions == "Beta"){
        if(input$type_graphic == "PDF/PMF"){
          return(withMathJax(helpText('$$
f_x(x) = \\frac{\\Gamma (\\alpha + \\beta)}{\\Gamma (\\alpha)\\Gamma(\\beta)}x^{\\alpha -1}(1-x)^{\\beta-1}
                                      $$')))
        }
        else if(input$type_graphic == "CDF"){
          return(withMathJax(helpText('$$F_x(x) = I_x(\\alpha,\\beta)$$
                                      ')))
        }
      }
      else if(input$distributions == "Hypergeometric"){
        if(input$type_graphic == "PDF/PMF"){
          return(withMathJax(helpText('$$
                                      f_x(x) =\\frac{\\binom{m}{x}\\binom{N-m}{n-x}}{\\binom{N}{n}}
                                      $$')))
        }
        else if(input$type_graphic == "CDF"){
          return(withMathJax(helpText('$$F_x(x) \\approx \\phi(\\frac{x-np}{\\sqrt{np(1-p)}})$$
                                      ')))
        }
      }
      else if(input$distributions == "Normal"){
        if(input$type_graphic == "PDF/PMF"){
          return(withMathJax(helpText('$$
                                      \\phi(x)= \\frac{1}{\\sigma\\sqrt{2\\pi }}exp\\left \\{ -\\frac{(x-\\mu )^2}{2\\sigma^2} \\right \\}
                                      $$')))
        }
        else if(input$type_graphic == "CDF"){
          return(withMathJax(helpText('$$\\Phi(x)=\\int_{-\\infty}^{x}\\phi(t)dt$$
                                      ')))
        }
      }
      else {
        return(helpText("Bernouli"))
        }
    })
  
  output$plot <- renderPlot({
    if(input$distributions == "Normal"){
      if (is.null(input$x_norm1)) {
        return(NULL)
      }
      if (is.null(input$x_norm2)) {
        return(NULL)
      }
      if (is.null(input$mean_norm)) {
        return(NULL)
      }
      if (is.null(input$sd_norm)) {
        return(NULL)
      }
      if(input$type_graphic == "PDF/PMF"){
        
        return(ggdistribution(dnorm, seq(input$x_norm1, input$x_norm2, 0.1),
                       mean = input$mean_norm, sd = input$sd_norm))
        }
      else {
        return(ggdistribution(pnorm, seq(input$x_norm1, input$x_norm2, 0.1),
                       mean = input$mean_norm, sd = input$sd_norm))
      }
    }
    else if(input$distributions == "Binomial"){
      if (is.null(input$x_bin1)) {
        return(NULL)
      }
      if (is.null(input$x_bin2)) {
        return(NULL)
      }
      if (is.null(input$size_bin)) {
        return(NULL)
      }
      if (is.null(input$prob_bin)) {
        return(NULL)
      }
      if(input$type_graphic == "PDF/PMF"){
        return(ggdistribution(dbinom, seq(input$x_bin1, input$x_bin2, 1),
                       size = input$size_bin, prob = input$prob_bin))
      }
      else {
        return(ggdistribution(pbinom, seq(input$x_bin1, input$x_bin2, 1),
                       size = input$size_bin, prob = input$prob_bin))
      }
    }
    else if(input$distributions == "Beta"){
      if (is.null(input$shape1)) {
        return(NULL)
      }
      if (is.null(input$shape2)) {
        return(NULL)
      }
      if(input$type_graphic == "PDF/PMF"){
        if (is.null(input$x_beta[1])) {
          return(NULL)
        }
        if (is.null(input$x_beta[2])) {
          return(NULL)
        }
        return(ggdistribution(dbeta, seq(input$x_beta[1], input$x_beta[2], 0.01),
                       shape1 = input$shape1, shape2 = input$shape2))
      }
      else {
        if (is.null(input$q_beta[1])) {
          return(NULL)
        }
        if (is.null(input$q_beta[2])) {
          return(NULL)
        }
        return(ggdistribution(pbeta, seq(input$q_beta[1], input$q_beta[2], 0.01),
                       shape1 = input$shape1, shape2 = input$shape2))
      }
    }
    else if(input$distributions == "Hypergeometric"){
      if (is.null(input$m_hyper)) {
        return(NULL)
      }
      if (is.null(n_hyp())) {
        return(NULL)
      }
      if (is.null(input$k_hyper)) {
        return(NULL)
      }
      if(input$type_graphic == "PDF/PMF"){
        if (is.null(input$x_hyper1)) {
          return(NULL)
        }
        if (is.null(input$x_hyper2)) {
          return(NULL)
        }
        return(ggdistribution(dhyper, seq(input$x_hyper1, input$x_hyper2, 0.1),
                       m = input$m_hyper, n = n_hyp(), k = input$k_hyper))
      }
      else {
        if (is.null(input$q_hyper1)) {
          return(NULL)
        }
        if (is.null(input$q_hyper2)) {
          return(NULL)
        }
        return(ggdistribution(phyper, seq(input$q_hyper1, input$q_hyper2, 0.1),
                       m = input$m_hyper, n = n_hyp(), k = input$k_hyper))
      }
    }
    })
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  output$results <- renderText({
    if(input$distributions == "Normal"){
      return(paste(withMathJax("$$E[X] = \\mu$$ =", input$mean_norm,
'$$V[X] = \\sigma^2$$', input$sd_norm^2,
'$$M_x(s) = exp\\left \\{ \\mu s + \\frac{\\sigma^2 s^2}{2}\\right \\}$$')))
    }
    else if(input$distributions == "Binomial"){
      return(paste(withMathJax("$$E[X] = np$$ =",Ex_binom(),
                               '$$V[X] = np(1-p)$$',Vx_binom(),
                               '$$M_x(s) = (1-p+pe^s)^n$$')))
    }
    else if(input$distributions == "Beta"){
      return(paste(withMathJax("$$E[X] = \\frac{\\alpha}{\\alpha + \\beta}$$", Ex_beta(),
                               '$$V[X] = \\frac{\\alpha \\beta}{(\\alpha + \\beta)^2(\\alpha + \\beta + 1)}$$',Vx_beta(),
                               '$$M_x(s) = 1 + \\sum_{k=1}^{\\infty} (\\prod_{r=0}^{k-1}\\frac{\\alpha + r}{\\alpha + \\beta + r})\\frac{s^k}{k!}$$')))
    }
    else if(input$distributions == "Hypergeometric"){
      return(paste(withMathJax("$$E[X] = \\frac{nm}{N}$$",Ex_hyp(),
                               '$$V[X] = \\frac{nm(N-n)(N-m)}{N^2(N-1)}$$',Vx_hyp())))
    }
  })
  output$examples <- renderUI({
    if(input$distributions == "Normal"){
      return(tagList(strong("Example of normal distribution"), br(), "For example, we are talking about a movie, which has an average score(mean) of 5 out of 10 points. We would like to know what the probability of each evaluation. In this case, we know the varriance of valuation=2."))
    }
    else if(input$distributions == "Binomial"){
      return(tagList(strong("Example of binomial distribution"), br(), " A coin is tossed 10 times. What is the probability of getting exactly 6 heads?"))
    }
    else if(input$distributions == "Beta"){
      return(tagList(strong("Example of beta distribution"), br(),
        "For example, we have 7 positive and 3 negative evaluations. So, we would like to know the rating of the film and its accuracy."  
      ))  
      }
    else if(input$distributions == "Hypergeometric"){
      return(tagList(strong("Example of hypergeometric distribution"), br(), " 1. A sample of 5 parts are drawn without replacement from a total population of 30 parts. Determine the probability of getting exactly 2 defective parts. The population is known to have 14 defective parts.
                     ",br(), "2. For example, I buy a lottery ticket and I know their number. What is the probability that I will win if I buy several lottery tickets"))
    }
  })
}
shinyApp(ui = ui, server = server)