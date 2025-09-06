library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)

# Define your flow data
flow <- list(
  q1 = list(
    question = "Are you an individual selling homemade food?",
    options = list("Yes" = "q2", "No" = "end_refer")
  ),
  q2 = list(
    question = "Do you have a food handler’s license?",
    options = list("Yes" = "q3", "No" = "end_food_license")
  ),
  q3 = list(
    question = "Do you prepare food in a home kitchen?",
    options = list("Yes" = "q4", "No" = "end_lfm")
  ),
  q4 = list(
    question = "Do your products include alcohol, marijuana, raw milk, fish, and/or shellfish?",
    options = list("Yes" = "end_illegal_products", "No" = "q5")
  ),
  q5 = list(
    question = "Is your food Time/Temperature Controlled for Safety (TCS)?",
    options = list("Yes" = "q6", "No" = "end_sell_anywhere")
  ),
  q6 = list(
    question = "Do you need a HACCP plan?",
    options = list("Yes" = "q7", "No" = "q7")
  ),
  q7 = list(
    question = "Is it produced in a way that makes it shelf-stable (e.g., canning, drying)?",
    options = list("Yes" = "end_sell_anywhere", "No" = "end_sell_in_person")
  ),
  
  # End nodes
  end_refer = list(
    question = "Refer to the Pima County Health Department Consumer Health and Food Safety Website.",
    options = list()
  ),
  end_food_license = list(
    question = "Look into food handling licenses.",
    options = list()
  ),
  end_lfm = list(
    question = "Look into becoming a Limited Food Manufacturer.",
    options = list()
  ),
  end_illegal_products = list(
    question = "You cannot produce food with these ingredients without special licenses. Look into commercial kitchens.",
    options = list()
  ),
  end_sell_anywhere = list(
    question = "✅ You can now sell in person or through a retail space!",
    options = list()
  ),
  end_sell_in_person = list(
    question = "✅ You can now sell, but only in person!",
    options = list()
  )
)

# Recursive function to generate all paths with Outcome as character scalar
generate_paths <- function(flow, current_id = "q1", current_path = list()) {
  node <- flow[[current_id]]
  
  if (length(node$options) == 0) {
    current_path[["Outcome"]] <- as.character(node$question)
    return(list(current_path))
  } else {
    all_paths <- list()
    for (opt in names(node$options)) {
      next_id <- node$options[[opt]]
      path_copy <- current_path
      path_copy[[node$question]] <- opt
      all_paths <- c(all_paths, generate_paths(flow, next_id, path_copy))
    }
    return(all_paths)
  }
}

# Generate all paths
all_paths <- generate_paths(flow)

# Clean Outcome in all_paths to ensure it's a character scalar
all_paths_clean <- lapply(all_paths, function(path) {
  if (is.list(path[["Outcome"]])) {
    path[["Outcome"]] <- paste(unlist(path[["Outcome"]]), collapse = " ")
  } else {
    path[["Outcome"]] <- as.character(path[["Outcome"]])
  }
  path
})

# Convert to data frame
df_paths <- bind_rows(lapply(all_paths_clean, function(x) as.data.frame(as.list(x), stringsAsFactors = FALSE)))

# Replace NA with ""
df_paths[is.na(df_paths)] <- ""

# Ensure all columns are character vectors
df_paths[] <- lapply(df_paths, as.character)

# Consolidate paths by Outcome: one row per Outcome with merged answers
df_consolidated <- df_paths %>%
  group_by(Outcome) %>%
  summarise(across(everything(), ~ {
    vals <- unique(.x[.x != ""])
    if (length(vals) == 0) "" else paste(sort(vals), collapse = " / ")
  })) %>%
  ungroup()

colnames(df_consolidated) <- gsub("\\.+", " ", colnames(df_consolidated))

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f9f8f6;
        color: #2e3e28;
        margin: 0;
        padding: 0;
      }
      .container {
        background: #ffffff;
        border-radius: 20px;
        box-shadow: 0 6px 15px rgba(46, 62, 40, 0.1);
        padding: 30px 40px;
        max-width: 900px;
        margin: 50px auto 70px auto;
      }
      h1, h2 {
        color: #3c6644;
        font-weight: 700;
        margin-bottom: 20px;
      }
      h3 {
        color: #4a6b48;
        font-weight: 600;
        margin-bottom: 12px;
      }
      .summary-list {
        background: #eaf1e7;
        padding: 15px 20px;
        border-radius: 12px;
        margin-top: 30px;
        font-size: 0.95em;
        line-height: 1.4;
      }
      .summary-list strong {
        color: #3c6644;
      }
      .shiny-input-radiogroup {
        margin-top: 20px;
      }
      .shiny-input-radiogroup > label {
        display: block;
        margin-bottom: 12px;
        font-size: 1.15em;
        cursor: pointer;
        transition: color 0.3s ease;
      }
      .shiny-input-radiogroup > label:hover {
        color: #5f7f5a;
      }
      input[type='radio'] {
        transform: scale(1.2);
        margin-right: 12px;
        cursor: pointer;
      }
      .btn-green {
        background-color: #3c6644;
        border-color: #3c6644;
        color: white;
        border-radius: 12px;
        padding: 10px 24px;
        font-size: 1.1em;
        font-weight: 600;
        transition: background-color 0.25s ease, box-shadow 0.25s ease;
        box-shadow: 0 4px 8px rgba(60, 102, 68, 0.3);
        cursor: pointer;
      }
      .btn-green:hover {
        background-color: #2e4b33;
        border-color: #2e4b33;
        box-shadow: 0 6px 12px rgba(46, 75, 51, 0.5);
      }
      .btn-green:disabled, .btn-green.btn-disabled {
        background-color: #a3b597;
        border-color: #a3b597;
        box-shadow: none;
        cursor: not-allowed;
        opacity: 0.7;
      }
      .button-row {
        display: flex;
        justify-content: space-between;
        margin-top: 30px;
      }
      .link-button {
        margin-top: 25px;
        font-size: 1em;
        color: #3c6644;
        cursor: pointer;
        text-decoration: underline;
      }
      .link-button:hover {
        color: #2e4b33;
      }
      /* Smaller font and wide container for data table */
      table.dataTable {
        font-size: 0.85em !important;
      }
      div.dataTables_wrapper {
        width: 100%;
        overflow-x: auto !important;
      }
    "))
  ),
  
  titlePanel(h1("Food Business Decision Helper")),
  
  tabsetPanel(
    id = "tabs",
    
    tabPanel("Decision Helper",
             div(class = "container",
                 uiOutput("questionUI"),
                 div(class = "button-row",
                     actionButton("back", "← Back", class = "btn-green"),
                     actionButton("reset", "🔄 Start Over", class = "btn-green")
                 ),
                 div(
                   "View full decision tree as table",
                   id = "toTableLink",
                   class = "link-button"
                 ),
                 uiOutput("summaryUI")
             )
    ),
    
    tabPanel("Full Decision Tree",
             div(class = "container",
                 DTOutput("flowTable")
             )
    )
  )
)

server <- function(input, output, session) {
  history <- reactiveValues(stack = c("q1"))
  answers <- reactiveValues(vals = list())
  
  current_question <- reactive({
    tail(history$stack, 1)
  })
  
  observeEvent(input$reset, {
    history$stack <- c("q1")
    answers$vals <- list()
    updateRadioButtons(session, "answer", selected = character(0))
  })
  
  observeEvent(input$back, {
    if (length(history$stack) > 1) {
      last_q <- tail(history$stack, 1)
      history$stack <- head(history$stack, -1)
      answers$vals[[last_q]] <- NULL
      updateRadioButtons(session, "answer", selected = character(0))
    }
  })
  
  observeEvent(input$answer, {
    req(input$answer)
    curr <- current_question()
    answers$vals[[curr]] <- input$answer
    next_q <- flow[[curr]]$options[[input$answer]]
    history$stack <- c(history$stack, next_q)
    updateRadioButtons(session, "answer", selected = character(0))
  })
  
  output$questionUI <- renderUI({
    qid <- current_question()
    q <- flow[[qid]]
    
    if (length(q$options) == 0) {
      h3(q$question)
    } else {
      radioButtons("answer", q$question, choices = names(q$options))
    }
  })
  
  output$summaryUI <- renderUI({
    if (length(history$stack) <= 1) return(NULL)
    
    summary_items <- lapply(head(history$stack, -1), function(qid) {
      question_text <- flow[[qid]]$question
      answer_text <- answers$vals[[qid]]
      if (is.null(answer_text)) answer_text <- ""
      div(
        tags$strong(question_text),
        tags$span(paste0(": ", answer_text)),
        style = "margin-bottom: 8px;"
      )
    })
    
    div(class = "summary-list",
        h4("Your Choices So Far:"),
        summary_items
    )
  })
  
  output$flowTable <- renderDT({
    datatable(
      df_consolidated,
      options = list(pageLength = 10, lengthChange = TRUE, scrollX = TRUE),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  observe({
    shinyjs::toggleState("back", condition = length(history$stack) > 1)
  })
  
  observeEvent(input$toTableLink, {
    updateTabsetPanel(session, "tabs", selected = "Full Decision Tree")
  })
}

shinyApp(ui, server)
