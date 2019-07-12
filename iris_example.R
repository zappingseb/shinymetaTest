# Needed library calls
library_code <- quote({
      library(shiny)
      library(shinymeta)
    })

eval(library_code)

ui <- fluidPage(
    h1("Regression App"),
    sidebarLayout(
        sidebarPanel(
            actionButton(inputId = "show_r_code", label = "Show R Code")
        ),
        mainPanel(
            plotOutput("plot1"),
            verbatimTextOutput("text1")
        )
    )
)

server <- function(input, output) {

  # Create the dataset
  data <- metaReactive({
        iris
      })

  # Create a linear model
  model <- metaReactive2({
        validate(need(is.data.frame(data()), "Data Set could not be created"))

        metaExpr({
              lm(formula = Sepal.Length ~ Sepal.Width, data = !!data())
            })
      })

  # show model summary
  output$text1 <- metaRender(renderPrint, {
        !!model() %>% summary()
      })
  # Plot Regression vs fitted
  output$plot1 <- metaRender(renderPlot, {
        plot(!!model(), which = 1)
      })


  # Show Code to produce the Outputs inside a modal
  observeEvent(input$show_r_code, {
        showModal(modalDialog(
                title = "R Code",
                tags$pre(
                    id = "r_code",
                    expandChain(
                        library_code,
                        output$plot1(),
                        output$text1()
                    ) %>% formatCode() %>% paste(collapse = "\n")
                ),
                footer = tagList(
                    actionButton("copyRCode", "Copy to Clipboard", `data-clipboard-target` = "#r_code"),
                    modalButton("Dismiss")
                ),
                size = "l",
                easyClose = TRUE
            ))
      })
}

shinyApp(ui, server)
