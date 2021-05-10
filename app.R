## libraries ----
library(shiny)
library(shinydashboard)# Guides on how to use this are at: ttps://rstudio.github.io/shinydashboard/

## Functions ----
source("functions.R")

## Tab pages ----
source("tabs.R")

## UI ----
ui <- dashboardPage(

  dashboardHeader(title = "Local Search Algorithms"),

  dashboardSidebar(
    width = 350,
    sidebarMenu(

      menuItem("Introduction",    tabName = "intro_tab"),
      menuItem("Steepest Ascent", tabName = "accent_tab"),
      menuItem("Newton-Raphson",  tabName = "newtraph_tab"),

      box(
        title = "Variables",
        solidHeader = FALSE, width = NULL, background = "navy",

        actionButton("nextIter",  "Next step"),
        actionButton("finish",    "Finish"),
        actionButton("reset",     "Reset"),
        selectInput("func", "Function",
                    choices = list("Poisson"  = 1, "Binomial" = 2),
                    selected = 1),

        conditionalPanel("input.func == 1",{
          box(
            background = "navy",
            sliderInput("pois_start_val", "Starting value", 1.5, min = 1, max = 5),
            sliderInput("pois_mean",      "Mean",           2.5, min = 1, max = 5),
            sliderInput("pois_n",         "Observations",   10,  min = 1, max = 100)
          )
        }),
        conditionalPanel("input.func == 2",{
          box(
            background = "navy",
            sliderInput("binom_start_val", "Starting value", .5, min = 0, max = 1),
            sliderInput("binom_successes", "Successes",       3, min = 1, max = 100),
            sliderInput("binom_n",         "Observations",   10, min = 1, max = 100)
          )
        }),

        numericInput("criterion", "Convergence criterion", 1e-5, min = 1e-10, max = 1e-3)
      )
    )
  ),

  dashboardBody(tabItems(intro_tab, accent_tab, newtraph_tab))
)

## UI ----
server <- function(input, output, session) {

  unlockBinding("invalidateLater", as.environment("package:shiny"))
  assign("invalidateLater", invalidateLaterNew, "package:shiny")


  #### reactive values ####
  i       <- reactiveVal(1)
  history <- get_iter_history(input)


  output$accent   <- renderPlot({plot_accent(history$accent, i())})
  output$newtraph <- renderPlot({plot_newtraph(history$newtraph, i())})

  #### buttons handling ####
  observeEvent(input$reset, {
    i(1)
    history <<- get_iter_history(input)
    output$accent   <- renderPlot({plot_accent(history$accent, i())})
    output$newtraph <- renderPlot({plot_newtraph(history$newtraph, i())})
  })

  observeEvent(input$nextIter, {
    i(i() + 1)
    output$accent   <- renderPlot({plot_accent(history$accent, i())})
    output$newtraph <- renderPlot({plot_newtraph(history$newtraph, i())})
  })

  observeEvent(input$finish, {

    ii <- i()
    jj <- i()

    output$accent <- renderPlot({
      ii <<- ii + 1
      invalidateLaterNew(100, session,  ii < length(history$accent$theta))
      plot_accent(history$accent, ii)
    })

    output$newtraph <- renderPlot({
      jj <<- jj + 1
      invalidateLaterNew(100, session,  jj < length(history$accent$theta))
      plot_newtraph(history$newtraph, jj)
    })

  })

}

shinyApp(ui, server)
