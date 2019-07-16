# Needed library calls
library(shiny)
library(shinymeta)
library(tibble)
library(dplyr)
library(rlang)

# Data construction (random data for a clinical study)

# Patient listing
pat_data <- list(
    SUBJID = 1:200,
    STUDYID = c(rep(1, 40), rep(2, 100), rep(3, 60)),
    AGE = sample(20:88, 200, replace = T) %>% as.numeric(),
    SEX = c(sample(c("M", "F"), 180, replace = T), rep("U", 20)) %>% as.factor()
) %>% as_tibble()

# Days where Overall Survival (OS), Event free survival (EFS) and Progression Free Survival (PFS) happened
event_data <- list(
    SUBJID = rep(1:200, 3),
    STUDYID = rep(c(rep(1, 40), rep(2, 100), rep(3, 60)), 3),
    PARAMCD = c(rep("OS", 200), rep("EFS", 200), rep("PFS", 200)),
    AVAL = c(rexp(200, 1 / 100), rexp(200, 1 / 80), rexp(200, 1 / 60)) %>% as.numeric(),
    AVALU = rep("DAYS", 600) %>% as.factor()
) %>% as_tibble()

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
            )
        ),
        mainPanel(
            plotOutput("plot1"),
            verbatimTextOutput("text1")
        )
    )
)

server <- function(input, output) {

  # Create a linear model
  model_reactive <- reactive({
        validate(need(is.character(input$select_regressor), "Cannot work without selected column"))

        regressors <- Reduce(function(x, y) call("+", x, y), rlang::syms(input$select_regressor))
        formula_value <- rlang::new_formula(rlang::sym("AVAL"), regressors)

        event_data_filtered <- event_data %>% dplyr::filter(PARAMCD == input$filter_param)
        ads_selected <- pat_data %>% dplyr::select(dplyr::one_of(c(input$select_regressor, c("SUBJID", "STUDYID"))))

        anl <- merge(ads_selected, event_data_filtered, by = c("SUBJID", "STUDYID"))

        lm(formula = formula_value, data = anl)
      })

  # Plot Regression vs fitted
  output$plot1 <- renderPlot({
        plot(model_reactive(), which = 1)
      })

  # show model summary
  output$text1 <- renderPrint({
    model_reactive() %>% summary()
  })

}

shinyApp(ui, server)
