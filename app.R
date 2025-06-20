library(shiny)

# UI
ui <- fluidPage(
  titlePanel("EASI Score Calculator"),
  tags$head(
    tags$style(HTML("\n      body {\n        font-family: Arial, sans-serif;\n      }\n      .container-fluid {\n        max-width: 600px;\n        margin: auto;\n        padding: 20px;\n      }\n      .shiny-input-container {\n        margin-bottom: 15px;\n      }\n      .btn-primary {\n        width: 100%;\n        font-size: 18px;\n        padding: 10px;\n      }\n      h3 {\n        text-align: center;\n      }\n    "))
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Enter Age"),
      numericInput("age", "Patient Age:", value = NULL, min = 0, max = 100),
      br(),
      h3("Enter Severity Scores (0-3)"),
      h4("Head/Neck"),
      numericInput("head_erythema", "Erythema:", value = 0, min = 0, max = 3),
      numericInput("head_edema", "Edema/Papulation:", value = 0, min = 0, max = 3),
      numericInput("head_excoriation", "Excoriation:", value = 0, min = 0, max = 3),
      numericInput("head_lichenification", "Lichenification:", value = 0, min = 0, max = 3),
      br(),
      h4("Upper Limbs"),
      numericInput("upper_erythema", "Erythema:", value = 0, min = 0, max = 3),
      numericInput("upper_edema", "Edema/Papulation:", value = 0, min = 0, max = 3),
      numericInput("upper_excoriation", "Excoriation:", value = 0, min = 0, max = 3),
      numericInput("upper_lichenification", "Lichenification:", value = 0, min = 0, max = 3),
      br(),
      h4("Trunk"),
      numericInput("trunk_erythema", "Erythema:", value = 0, min = 0, max = 3),
      numericInput("trunk_edema", "Edema/Papulation:", value = 0, min = 0, max = 3),
      numericInput("trunk_excoriation", "Excoriation:", value = 0, min = 0, max = 3),
      numericInput("trunk_lichenification", "Lichenification:", value = 0, min = 0, max = 3),
      br(),
      h4("Lower Limbs"),
      numericInput("lower_erythema", "Erythema:", value = 0, min = 0, max = 3),
      numericInput("lower_edema", "Edema/Papulation:", value = 0, min = 0, max = 3),
      numericInput("lower_excoriation", "Excoriation:", value = 0, min = 0, max = 3),
      numericInput("lower_lichenification", "Lichenification:", value = 0, min = 0, max = 3),
      br(),
      h3("Enter Affected Area (%)"),
      numericInput("head_area", "Head/Neck Affected %:", value = 0, min = 0, max = 100),
      numericInput("upper_area", "Upper Limbs Affected %:", value = 0, min = 0, max = 100),
      numericInput("trunk_area", "Trunk Affected %:", value = 0, min = 0, max = 100),
      numericInput("lower_area", "Lower Limbs Affected %:", value = 0, min = 0, max = 100),
      br(),
      actionButton("calculate", "Calculate EASI Score", class = "btn-primary")
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("easi_score")
    )
  )
)

# Server Logic
server <- function(input, output) {
  observeEvent(input$calculate, {
    head_multiplier <- ifelse(input$age <= 7, 0.2, 0.1)
    lower_multiplier <- ifelse(input$age <= 7, 0.3, 0.4)
    weights <- c(head_multiplier, 0.2, 0.3, lower_multiplier)
    
    severity_scores <- c(
      sum(input$head_erythema, input$head_edema, input$head_excoriation, input$head_lichenification),
      sum(input$upper_erythema, input$upper_edema, input$upper_excoriation, input$upper_lichenification),
      sum(input$trunk_erythema, input$trunk_edema, input$trunk_excoriation, input$trunk_lichenification),
      sum(input$lower_erythema, input$lower_edema, input$lower_excoriation, input$lower_lichenification)
    )
    
    # Convert percentage area to area score based on the reference table
    convert_area_score <- function(pct) {
      if (pct == 0) return(0)
      else if (pct <= 9) return(1)
      else if (pct <= 29) return(2)
      else if (pct <= 49) return(3)
      else if (pct <= 69) return(4)
      else if (pct <= 89) return(5)
      else return(6)
    }
    
    area_scores <- sapply(c(input$head_area, input$upper_area, input$trunk_area, input$lower_area), convert_area_score)
    
    easi <- sum(weights * area_scores * severity_scores)
    
    output$easi_score <- renderText({
      paste("EASI Score:", round(easi, 2))
    })
  })
}

# Run App
shinyApp(ui = ui, server = server)

