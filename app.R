# Needed library calls
library_code <- quote({
  library(shiny)
  library(shinymeta)
  library(tibble)
  library(dplyr)
})

eval(library_code)

# Data construction (random data for a clinical study)
data_code <- quote({

  # Patient listing
  pat_data <- list(
    SUBJID = 1:100,
    STUDYID = c(rep(1, 20), rep(2, 50), rep(3, 30)),
    AGE = sample(20:88, 100, replace = T) %>% as.numeric(),
    SEX = sample(c("M", "F", "U"), 100, replace = T) %>% as.factor()
  ) %>% as_tibble()

  # Days where Overall Survival (OS), Event free survival (EFS) and Progression Free Survival (PFS) happened
  event_data <- list(
    SUBJID = rep(1:100, 3),
    STUDYID = rep(c(rep(1, 20), rep(2, 50), rep(3, 30)), 3),
    PARAMCD = c(rep("OS", 100), rep("EFS", 100), rep("PFS", 100)),
    AVAL = c(rexp(100, 1 / 100), rexp(100, 1 / 80), rexp(100, 1 / 60)) %>% as.numeric(),
    AVALU = rep("DAYS", 300) %>% as.factor()
  ) %>% as_tibble()
})

eval(data_code)

ui <- fluidPage(
  h1("Regression App"),
  sidebarLayout(
    sidebarPanel(
      # Input of Response can be chosen from Event Table
      tags$b("Response from Event Table"),
      selectInput(
        inputId = "filter_param",
        label = "Filter PARAM",
        choices = c("OS", "PFS", "EFS"),
        selected = "OS"
      ),
      tags$span("Selected Column:", tags$code("AVAL")),
      tags$br(),
      tags$hr(),
      # Input of Regressor can be chosen from
      tags$b("Regressor from Patient data"),
      selectInput(
        inputId = "select_regressor",
        label = "Select regressor columns",
        choices = c("AGE", "SEX"),
        selected = "AGE",
        multiple = TRUE
      ),
      actionButton(inputId = "show_r_code", label = "Show R Code")
    ),
    mainPanel(
      plotOutput("plot1"),
      verbatimTextOutput("text1")
    )
  )
)

server <- function(input, output) {

  # Create the dataset by merging the two tables per patient
  data_set_reactive <- metaReactive({
    event_data_filtered <- event_data %>% dplyr::filter(PARAMCD == !!input$filter_param)
    ads_selected <- pat_data %>% dplyr::select(dplyr::one_of(c(!!input$select_regressor, c("SUBJID", "STUDYID"))))
    merge(ads_selected, event_data_filtered, by = c("SUBJID", "STUDYID"))
  })

  # Create a formula out of the inputs
  formula_reactive <- metaReactive2({
    validate(need(is.character(input$select_regressor), "Cannot work without selected column"))

    metaExpr({
      !!stats::as.formula(
          paste(
              "AVAL",
              paste(
                  sapply(input$select_regressor, as.symbol),
                  collapse = " + "
              ),
              sep = " ~ "
          )
      )
    })
  })

  # Create a linear model
  model_reactive <- metaReactive2({
    validate(need(is.data.frame(data_set_reactive()), "Data Set could not be created"))
    validate(need(is.language(formula_reactive()), "Formula could not be created from column selections"))

    metaExpr({
      model_data <- !!data_set_reactive()
      lm(formula = !!formula_reactive(), data = model_data)
    })
  })

  # Plot Regression vs fitted
  output$plot1 <- metaRender(renderPlot, {
    plot(!!model_reactive(), which = 1)
  })

  # show model summary
  output$text1 <- metaRender(renderPrint, {
    !!model_reactive() %>% summary()
  })

  # Show Code to produce the Outputs inside a modal
  observeEvent(input$show_r_code, {
    showModal(modalDialog(
      title = "R Code",
      tags$pre(
        id = "r_code",
        expandChain(
          library_code,
          data_code,
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
