library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)

# ---------------------------------------------------
# CLEAN + EASY-TO-READ DECISION TREE DEFINITION
# ---------------------------------------------------
flow <- list(
  q1 = list(
    question = "Have you completed food handling training?",
    options = list("Yes" = "q2", "No" = "end_get_training")
  ),
  q2 = list(
    question = "Do you prepare food in a home kitchen?",
    options = list("Yes" = "q3", "No" = "end_lfm")
  ),
  q3 = list(
    question = "Do your products include alcohol, intoxicants, marijuana, raw milk, fish, or shellfish?",
    options = list("Yes" = "end_lfm", "No" = "q4")
  ),
  q4 = list(
    question = "Have you registered with the Arizona Cottage Food Program?",
    options = list("Yes" = "q5", "No" = "end_register_cfp")
  ),
  q5 = list(
    question = "Is your food Time/Temperature Controlled for Safety (TCS)?",
    options = list("Yes" = "q6", "No" = "end_sell_non_tcs")
  ),
  q6 = list(
    question = "Do you want to sell your TCS food in a retail space or through 3rd-party delivery?",
    options = list("Yes" = "end_lfm", "No" = "end_sell_tcs_direct")
  ),
  
  end_get_training = list(
    question = "Please complete food handler training or the Certified Food Protection Manager course.",
    options = list()
  ),
  
  end_lfm = list(
    question = "You may need to become a Limited Food Manufacturer.",
    options = list()
  ),
  
  end_register_cfp = list(
    question = "Register with the Arizona Cottage Food Program.",
    options = list()
  ),
  
  end_sell_non_tcs = list(
    question = "You are ready to sell non-TCS foods at public events, retail spaces, pop-ups, or online in Arizona.",
    options = list()
  ),
  
  end_sell_tcs_direct = list(
    question = "You may sell TCS foods in person, at events, or for business meeting delivery (but not via 3rd-party services).",
    options = list()
  )
)

# ---------------------------------------------------
# GENERATE ALL POSSIBLE PATHS
# ---------------------------------------------------
generate_paths <- function(flow, node_id = "q1", path = list()) {
  node <- flow[[node_id]]
  
  if (length(node$options) == 0) {
    path$Outcome <- node$question
    return(list(path))
  }
  
  results <- list()
  for (opt in names(node$options)) {
    next_id <- node$options[[opt]]
    new_path <- path
    new_path[[node$question]] <- opt
    results <- c(results, generate_paths(flow, next_id, new_path))
  }
  results
}

all_paths <- generate_paths(flow)

df_paths <- bind_rows(lapply(all_paths, as.data.frame)) %>%
  mutate(across(everything(), ~replace_na(as.character(.x), "")))

df_consolidated <- tribble(
  ~Outcome,                                                                 ~`Requires Food Handling Training`, ~`Food prepared in a home kitchen`, ~`Arizona Cottage Food Program Registration`, ~`Products Include Alcohol Intoxicants Marijuana Raw Milk Fish or Shellfish`, ~`Food is Time Temperature Controlled for Safety (TCS)`, ~`TCS Food will be sold a Retail Space or Through 3rd Party Delivery`,
  "Selling non-TCS foods at public events, retail spaces, pop-ups, or online in Arizona", "Yes", "Yes", "Yes", "No", "No", "",
  "You may sell TCS foods in person, at events, or for business meeting delivery (but not via 3rd-party services).", "Yes", "Yes", "Yes", "No", "Yes", "No",
  "You may need to become a Limited Food Manufacturer.", "Yes", "Yes / No", "Yes", "Yes / No", "Yes", "Yes"
)

# ---------------------------------------------------
# UI
# ---------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  # ------------------- CUSTOM COLORS + FONTS -------------------
  tags$head(
    tags$style(HTML("
      :root {
        --color-primary:    #2a4a3a;
        --color-secondary:  #06968c;
        --color-accent:     #8dc642;
        --color-background: #ffffff;
        --color-text:       #333333;
        --color-muted:      #666666;
        --color-light-bg:   #f7f7f7;
      }
      
      body {
        background-color: var(--color-background);
        color: var(--color-text);
        font-family: 'Helvetica Neue', Arial, sans-serif;
        line-height: 1.6;
        margin: 0;
      }
      
      .main-container {
        max-width: 800px;
        margin: 40px auto 60px auto;
        padding: 0 20px;
      }
      
      h1, h2, h3, h4 {
        color: var(--color-primary);
        margin-top: 1.5em;
        margin-bottom: 0.75em;
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }
      h1 { font-size: 2.2em; }
      h3 { font-size: 1.3em; }
      
      a, a:visited {
        color: var(--color-secondary);
        text-decoration: none;
      }
      a:hover {
        text-decoration: underline;
      }
      
      .btn-custom {
        background-color: var(--color-primary);
        color: white;
        border: none;
        padding: 8px 20px;
        border-radius: 6px;
        cursor: pointer;
        font-size: 1em;
      }
      .btn-custom:disabled {
        background-color: #cccccc;
        cursor: not-allowed;
      }
      
      .summary-box {
        background-color: var(--color-light-bg);
        border-left: 4px solid var(--color-primary);
        padding: 15px 20px;
        margin-top: 30px;
      }
      .summary-box strong {
        color: var(--color-primary);
      }
      
      .dataTables_wrapper {
        width: 100%;
        overflow-x: auto;
      }
    "))
  ),
  # --------------------------------------------------------------
  
  div(class = "main-container",
      
      h1("Food Business Decision Helper"),
      
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Interactive Guide",
                 uiOutput("questionUI"),
                 br(),
                 fluidRow(
                   column(6, actionButton("back", "← Back", class = "btn-custom")),
                   column(6, actionButton("reset", "Start Over", class = "btn-custom"))
                 ),
                 uiOutput("summaryUI"),
                 br(),
                 actionLink("toTable", "View Summary Table")
        ),
        
        tabPanel("Summary Table",
                 DTOutput("flowTable"))
      )
  )
)

# ---------------------------------------------------
# SERVER
# ---------------------------------------------------
server <- function(input, output, session) {
  
  history <- reactiveValues(stack = "q1")
  answers <- reactiveValues(vals = list())
  
  currentQ <- reactive({
    tail(history$stack, 1)
  })
  
  observeEvent(input$reset, {
    history$stack <- "q1"
    answers$vals <- list()
    updateRadioButtons(session, "answer", selected = character(0))
  })
  
  observeEvent(input$back, {
    if (length(history$stack) > 1) {
      last <- tail(history$stack, 1)
      history$stack <- head(history$stack, -1)
      answers$vals[[last]] <- NULL
      updateRadioButtons(session, "answer", selected = character(0))
    }
  })
  
  observeEvent(input$answer, {
    req(input$answer)
    
    curr <- currentQ()
    answers$vals[[curr]] <- input$answer
    
    next_id <- flow[[curr]]$options[[input$answer]]
    history$stack <- c(history$stack, next_id)
    
    updateRadioButtons(session, "answer", selected = character(0))
  })
  
  output$questionUI <- renderUI({
    qid <- currentQ()
    node <- flow[[qid]]
    
    if (length(node$options) == 0) {
      h3(node$question)
    } else {
      radioButtons("answer",
                   label = node$question,
                   choices = names(node$options),
                   selected = character(0))
    }
  })
  
  output$summaryUI <- renderUI({
    if (length(history$stack) <= 1) return(NULL)
    
    div(class = "summary-box",
        h4("Your Choices So Far:"),
        lapply(head(history$stack, -1), function(qid) {
          question <- flow[[qid]]$question
          ans <- answers$vals[[qid]]
          div(strong(question), paste(": ", ans))
        })
    )
  })
  output$flowTable <- renderDT({
    datatable(
      df_consolidated,
      rownames = FALSE,
      filter = "none",
      escape = FALSE,   # allow HTML
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "tip",
        autoWidth = TRUE
      )
    ) %>%
      formatStyle(
        columns = colnames(df_consolidated)[-1],  # all except Outcome
        valueColumns = colnames(df_consolidated)[-1],
        backgroundColor = styleEqual(
          c("Yes", "No", "Yes / No", ""), 
          c("#d4edda", "#f8d7da", "#fff3cd", NA)  # green / red / yellow / blank
        )
      )
  })
  
  observeEvent(input$toTable, {
    updateTabsetPanel(session, "tabs", selected = "Full Decision Tree")
  })
  
  observe({
    shinyjs::toggleState("back", condition = length(history$stack) > 1)
  })
}

shinyApp(ui, server)
