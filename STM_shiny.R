library(shiny)
library(visNetwork)
library(dplyr)
library(stringr)

# ---- STM NODES ----
nodes <- data.frame(
  id = c(
    "Reference",
    "Modified overstorey with reference understorey",
    "Reference overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Sparse/absent overstorey with modified understorey",
    "Reference overstorey with highly modified understorey",
    "Modified overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Collapsed/novel overstorey and understorey"
  ),
  label = c(
    "Reference",
    "Modified overstorey\nwith reference understorey",
    "Reference overstorey\nwith modified understorey",
    "Modified overstorey\nwith modified understorey",
    "Sparse/absent overstorey\nwith modified understorey",
    "Reference overstorey\nwith highly modified understorey",
    "Modified overstorey\nwith highly modified understorey",
    "Sparse/absent overstorey\nwith highly modified understorey",
    "Collapsed/novel overstorey\nwith highly modified understorey",
    "Reference overstorey\nwith collapsed/transformer understorey",
    "Modified overstorey\nwith collapsed/transformer understorey",
    "Sparse/absent overstorey\nwith collapsed/transformer understorey",
    "Collapsed/novel overstorey\nand understorey"
  ),
  # 4 x 4 grid (column: [0, 1, 2, 3], row: [0, 1, 2, 3])
  x = c(
    0,  1,      # Row 1
    0,  1,  2,  # Row 2
    0,  1,  2,  3,  # Row 3
    0,  1,  2,  3   # Row 4
  ) * 620, # spacing between columns
  y = c(
    0,  0,      # Row 1 (top)
    1,  1,  1,  # Row 2
    2,  2,  2,  2,  # Row 3
    3,  3,  3,  3   # Row 4 (bottom)
  ) * 360, # spacing between rows
  color.background = c(
    "#63B3D2",     # Reference
    "#A3D6B0",     # Modified overstorey with reference understorey
    "#A3D6B0",     # Reference overstorey with modified understorey
    "#A3D6B0",     # Modified overstorey with modified understorey
    "#F9E79F",     # Sparse/absent overstorey with modified understorey
    "#A3D6B0",     # Reference overstorey with highly modified understorey
    "#F9E79F",     # Modified overstorey with highly modified understorey
    "#F9E79F",     # Sparse/absent overstorey with highly modified understorey
    "#F7B79C",     # Collapsed/novel overstorey with highly modified understorey
    "#F9E79F",     # Reference overstorey with collapsed/transformer understorey
    "#F9E79F",     # Modified overstorey with collapsed/transformer understorey
    "#F7B79C",     # Sparse/absent overstorey with collapsed/transformer understorey
    "#E57373"      # Collapsed/novel overstorey and understorey
  ),
  color.border = rep("black", 13),
  font.size = rep(35, 13),
  font.face = rep("arial", 13),
  font.align = rep("center", 13),
  #font.family = rep("arial", 13),
  shape = rep("box", 13),
  widthConstraint = rep(520, 13),
  heightConstraint = rep(140, 13),
  stringsAsFactors = FALSE
)

# ---- Degradation edges (only) ----
edges_degradation <- data.frame(
  from = c(
    "Reference",
    "Reference",
    "Reference",
    "Reference",
    "Reference",
    "Modified overstorey with reference understorey",
    "Modified overstorey with reference understorey",
    "Reference overstorey with modified understorey",
    "Reference overstorey with modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    #"Reference overstorey with highly modified understorey",
    "Modified overstorey with reference understorey"
    #"Reference overstorey with collapsed/transformer understorey"
  ),
  to = c(
    "Modified overstorey with reference understorey",
    "Reference overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Sparse/absent overstorey with modified understorey",
    "Collapsed/novel overstorey and understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with modified understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    "Collapsed/novel overstorey and understorey",
    "Collapsed/novel overstorey and understorey",
    #"Reference overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with modified understorey"
    #"Modified overstorey with collapsed/transformer understorey"
  ),
  id = paste0("deg_", 1:23),
  label = "",
  arrows = "to",
  color = "#000000", # base color: black
  width = 10, 
  font.size = 18,
  smooth = FALSE,
  stringsAsFactors = FALSE
)

# ---- Regeneration edges (only) ----
edges_regeneration <- data.frame(
  from = c(
    "Collapsed/novel overstorey and understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with modified understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Reference overstorey with highly modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with reference understorey",
    "Reference overstorey with modified understorey"
  ),
  to = c(
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with modified understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Modified overstorey with modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with modified understorey",
    "Reference overstorey with modified understorey",
    "Modified overstorey with reference understorey",
    "Reference",
    "Reference"
  ),
  id = paste0("regen_", 1:17),
  label = "",
  arrows = "to",
  color = "#000000",
  width = 10, 
  font.size = 18,
  smooth = FALSE,
  stringsAsFactors = FALSE
)

# ---- Transition Drivers ----
transitions_drivers <- data.frame(
  from = c(
    "Reference",
    "Reference",
    "Reference",
    "Reference",
    "Reference",
    "Modified overstorey with reference understorey",
    "Modified overstorey with reference understorey",
    "Reference overstorey with modified understorey",
    "Reference overstorey with modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    "Modified overstorey with reference understorey",
    "Collapsed/novel overstorey and understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with modified understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Reference overstorey with highly modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with reference understorey",
    "Reference overstorey with modified understorey"
  ),
  
  to = c(
    "Modified overstorey with reference understorey",
    "Reference overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Sparse/absent overstorey with modified understorey",
    "Collapsed/novel overstorey and understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with modified understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Collapsed/novel overstorey with highly modified understorey",
    "Collapsed/novel overstorey and understorey",
    "Collapsed/novel overstorey and understorey",
    "Sparse/absent overstorey with modified understorey",
    "Sparse/absent overstorey with collapsed/transformer understorey",
    "Modified overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with highly modified understorey",
    "Sparse/absent overstorey with modified understorey",
    "Modified overstorey with highly modified understorey",
    "Modified overstorey with modified understorey",
    "Modified overstorey with highly modified understorey",
    "Reference overstorey with collapsed/transformer understorey",
    "Modified overstorey with modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with highly modified understorey",
    "Reference overstorey with modified understorey",
    "Reference overstorey with modified understorey",
    "Modified overstorey with reference understorey",
    "Reference",
    "Reference"
  ),
  
  scenario = c(
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "degradation",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration",
    "regeneration"
  ),
  
  drivers = c(
    "Tree death by loopers; Increased fire frequency; Tree removal (pastoralism); Drought",
    "Over-grazing; Increased fire frequency; Increased fire severity; Decreased fire frequency; Loss of eco-engineers; Drought",
    "Clearing + grazing (native & exotic)",
    "Rolling and clearing",
    "Rolling and clearing; Cropping/sewing pasture",
    "Rolling and clearing; Over-grazing; Drought",
    "Over-grazing; Increased fire frequency; Increased fire severity; Decreased fire frequency; Loss of eco-engineers; Drought",
    "Weed encroachement; Over-grazing; Tree death by loopers; Tree removal (pastoralism); Drought",
    "Over-grazing; Weed encroachement; Increased fire frequency; Increased fire severity; Decreased fire frequency; Loss of eco-engineers; Drought",
    "Over-grazing; Nutrient deposition; Drought",
    "Tree removal (pastoralism); Over-grazing; Tree death by loopers; Drought",
    "Tree death by loopers; Tree removal (pastoralism); Decreased fire frequency; Drought",
    "Tree removal (pastoralism); Drought",
    "Over-grazing; Increased fire frequency; Increased fire severity; Decreased fire frequency; Loss of eco-engineers; Drought",
    "Over-grazing; Nutrient deposition",
    "Tree removal (pastoralism); Over-grazing; Drought",
    "Tree removal (pastoralism); Over-grazing; Drought",
    "Weed encroachement (herbaceous weeds); hydrological alteration",
    "Over-grazing; Nutrient deposition; Erosion; Salinity",
    "Invasion of woody weeds; Salinity; Drought",
    "Tree removal (pastoralism); Invasion of woody weeds; Drought",
    "Nutrient deposition; Erosion; Fertilisation; Salinity",
    "Tree death by loopers",
    "Woody weed removal (overstorey); Revegetation; Propagules and rainfall",
    "Revegetation; Propagules and rainfall",
    "Weed control; Herbivore control; Revegetation; Reinstatement of low frequency fire; Propagules and rainfall",
    "Woody weed removal (overstorey); Revegetation; Herbivore control",
    "Weed control; Herbivore control; Revegetation; Reinstatement of low frequency fire",
    "Revegetation; Herbivore control; Reinstatement of low fire frequency",
    "Propagules and rainfall; Herbivore control; Revegetation",
    "Weed control; Herbivore control; Revegetation; Reinstatement of low frequency fire",
    "Revegetation; Propagules and rainfall",
    "Weed control; Herbivore control",
    "Revegetation; Herbivore control; Reinstatement of low fire frequency",
    "Weed control; Herbivore control; Revegetation; Reinstatement of low frequency fire",
    "Weed control; Herbivore control; Revegetation; Reinstatement of low frequency fire",
    "Revegetation; Herbivore control; Reinstatement of low fire frequency",
    "Weed control; Herbivore control; Revegetation",
    "Revegetation; reintroduction of eco-engineers",
    "Weed control; Herbivore control; Revegetation; Reinstatement of low frequency fire; Reintroduction of ecoengineers"
  )
)

get_transition_edge_id <- function(from, to, scenario) {
  paste0("from_", from, "_to_", to, "_", scenario)
}
transition_driver_map <- transitions_drivers %>%
  mutate(edge_id = paste0("from_", from, "_to_", to, "_", scenario)) %>%
  select(edge_id, drivers)

# ---- UI ----
ui <- fluidPage(
  titlePanel("STM Transition Probability"),
  sidebarLayout(
    sidebarPanel(
      textInput("expert_name", "Expert Name", ""),
      textInput("expert_id", "Expert Email", ""),
      br(),
      h4("Major scenario selection"),
      actionButton("set_degradation", "Degradation"),
      actionButton("set_regeneration", "Regeneration"),
      br(), br(),
      uiOutput("scenario_display"),
      br(),
      uiOutput("selected_transition"),
      conditionalPanel(
        condition = "output.transitionSelected",
        uiOutput("driver_prob_sliders"),
        actionButton("save_driver_probs", "Save Probabilities for Drivers"),
        br(), br(),
        radioButtons("has_interactions", "Do drivers interact when the transition happens?", 
                     choices = c("No", "Yes"), selected = "No"),
        uiOutput("interaction_ui"),
        br(),
        # Yes/No toggle for comments
        radioButtons("add_comment", "Would you like to add comments about this transition?",
                     choices = c("No", "Yes"), selected = "No"),
        # Comment box that appears only when "Yes" is selected
        conditionalPanel(
          condition = "input.add_comment == 'Yes'",
          textAreaInput("transition_comment", 
                        "Enter your comments:",
                        rows = 3),
          actionButton("save_comment", "Save Comment")
        )
      ),
      br(),
      actionButton("submit", "Submit All Probabilities"),
      br(),
      downloadButton("downloadData", "Download Responses")
    ),
    mainPanel(
      visNetworkOutput("network", height = "800px")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  rv <- reactiveValues(
    edges_degradation = edges_degradation,
    edges_regeneration = edges_regeneration,
    selected_edge = NULL,
    edge_driver_prob_degradation = setNames(vector("list", nrow(edges_degradation)), edges_degradation$id),
    edge_driver_prob_regeneration = setNames(vector("list", nrow(edges_regeneration)), edges_regeneration$id),
    scenario = "degradation",
    edge_driver_interactions_degradation = list(),
    edge_driver_interactions_regeneration = list(), 
    transition_comments = list()
  )
  
  # Helper functions to format driver and interaction probabilities for display/export
  driver_prob_label <- function(prob_list) {
    if (is.null(prob_list) || length(prob_list) == 0) return("")
    paste(names(prob_list), sprintf("%.2f", as.numeric(prob_list)), sep=": ", collapse="; ")
  }
  
  interaction_prob_label <- function(interaction_list) {
    if (is.null(interaction_list) || length(interaction_list) == 0) return("")
    paste(names(interaction_list), sprintf("%.2f", as.numeric(unlist(interaction_list))), sep=": ", collapse="; ")
  }
  
  has_all_probs <- function(edge_id, scenario) {
    rv_name <- paste0("edge_driver_prob_", scenario)
    probs <- rv[[rv_name]][[edge_id]]
    if (is.null(probs)) return(FALSE)
    if (length(probs) == 0) return(FALSE)
    from <- if (scenario == "degradation") {
      edges_degradation %>% filter(id == edge_id) %>% pull(from)
    } else {
      edges_regeneration %>% filter(id == edge_id) %>% pull(from)
    }
    to <- if (scenario == "degradation") {
      edges_degradation %>% filter(id == edge_id) %>% pull(to)
    } else {
      edges_regeneration %>% filter(id == edge_id) %>% pull(to)
    }
    edge_key <- paste0("from_", from, "_to_", to, "_", scenario)
    expected_drivers <- transition_driver_map %>%
      filter(edge_id == edge_key) %>%
      pull(drivers) %>%
      str_split(";") %>%
      unlist() %>%
      str_trim()
    all(expected_drivers %in% names(probs))
  }
  
  observeEvent(input$set_degradation, {
    rv$scenario <- "degradation"
    rv$selected_edge <- NULL
  })
  
  observeEvent(input$set_regeneration, {
    rv$scenario <- "regeneration"
    rv$selected_edge <- NULL
  })
  
  output$scenario_display <- renderUI({
    strong(paste("Currently editing:", toupper(rv$scenario)))
  })
  
  output$transitionSelected <- reactive({
    !is.null(rv$selected_edge)
  })
  outputOptions(output, "transitionSelected", suspendWhenHidden = FALSE)
  
  output$selected_transition <- renderUI({
    req(input$edge_id)
    edge <- switch(
      rv$scenario,
      "degradation" = rv$edges_degradation,
      "regeneration" = rv$edges_regeneration
    ) %>% filter(id == input$edge_id)
    rv$selected_edge <- input$edge_id
    from <- edge$from
    to <- edge$to
    HTML(paste0("<strong>Selected Transition (", toupper(rv$scenario), "):</strong><br>", from, " → ", to))
  })
  
  output$driver_prob_sliders <- renderUI({
    req(rv$selected_edge)
    edge_df <- if (rv$scenario == "degradation") edges_degradation else edges_regeneration
    edge <- edge_df %>% filter(id == rv$selected_edge)
    from <- edge$from; to <- edge$to; scenario <- rv$scenario
    edge_id_lookup <- get_transition_edge_id(from, to, scenario)
    drivers_str <- transition_driver_map %>% filter(edge_id == edge_id_lookup) %>% pull(drivers)
    driver_choices <- if(length(drivers_str) > 0 && drivers_str != "") {
      str_split(drivers_str, ";")[[1]] %>% str_trim()
    } else { character(0) }
    prob_list <- switch(
      rv$scenario,
      "degradation" = rv$edge_driver_prob_degradation[[rv$selected_edge]],
      "regeneration" = rv$edge_driver_prob_regeneration[[rv$selected_edge]]
    )
    if (is.null(prob_list)) prob_list <- setNames(rep(0, length(driver_choices)), driver_choices)
    sliderUIs <- lapply(driver_choices, function(driver) {
      sliderInput(
        inputId = paste0("prob_driver_", make.names(driver)),
        label = driver,
        min = 0, max = 1, value = prob_list[driver], step = 0.01
      )
    })
    do.call(tagList, sliderUIs)
  })
  
  output$interaction_ui <- renderUI({
    req(input$has_interactions == "Yes", rv$selected_edge)
    
    edge_df <- if (rv$scenario == "degradation") edges_degradation else edges_regeneration
    edge <- edge_df %>% filter(id == rv$selected_edge)
    from <- edge$from; to <- edge$to; scenario <- rv$scenario
    edge_id_lookup <- get_transition_edge_id(from, to, scenario)
    drivers_str <- transition_driver_map %>% filter(edge_id == edge_id_lookup) %>% pull(drivers)
    driver_choices <- if(length(drivers_str) > 0 && drivers_str != "") {
      str_split(drivers_str, ";")[[1]] %>% str_trim()
    } else { character(0) }
    
    if (length(driver_choices) < 2) {
      return(h6("Not enough drivers to define interactions."))
    }
    
    if (is.null(rv$interaction_count)) rv$interaction_count <- 1
    
    interaction_uis <- lapply(seq_len(rv$interaction_count), function(i) {
      tagList(
        fluidRow(
          column(6, selectInput(paste0("interaction_driver1_", i), 
                                label = paste("Interaction", i, "- Driver 1"), 
                                choices = driver_choices, 
                                selected = driver_choices[1])),
          column(6, selectInput(paste0("interaction_driver2_", i), 
                                label = paste("Interaction", i, "- Driver 2"), 
                                choices = driver_choices, 
                                selected = driver_choices[2]))
        ),
        fluidRow(
          column(12, 
                 sliderInput(paste0("interaction_prob_", i), 
                             label = "Interaction probability", 
                             min = 0, max = 1, value = 0, step = 0.01,
                             width = "100%"))
        ),
        hr()
      )
    })
    
    saved_interactions <- switch(
      rv$scenario,
      "degradation" = rv$edge_driver_interactions_degradation[[rv$selected_edge]],
      "regeneration" = rv$edge_driver_interactions_regeneration[[rv$selected_edge]]
    )
    
    saved_interactions_ui <- NULL
    if (!is.null(saved_interactions) && length(saved_interactions) > 0) {
      saved_interactions_ui <- tagList(
        h5("Saved interactions for this transition:"),
        tags$ul(
          lapply(names(saved_interactions), function(inter) {
            tags$li(paste0(inter, ": ", sprintf("%.2f", saved_interactions[[inter]])))
          })
        )
      )
    }
    
    tagList(
      actionButton("add_interaction", "Add Another Interaction"),
      br(), br(),
      interaction_uis,
      br(),
      actionButton("save_all_interactions", "Save All Interaction Probabilities"),
      br(), br(),
      saved_interactions_ui
    )
  })
  
  observeEvent(input$save_driver_probs, {
    req(rv$selected_edge)
    
    edge_df <- if (rv$scenario == "degradation") edges_degradation else edges_regeneration
    edge <- edge_df %>% filter(id == rv$selected_edge)
    from <- edge$from; to <- edge$to; scenario <- rv$scenario
    edge_id_lookup <- get_transition_edge_id(from, to, scenario)
    drivers_str <- transition_driver_map %>% filter(edge_id == edge_id_lookup) %>% pull(drivers)
    driver_choices <- if(length(drivers_str) > 0 && drivers_str != "") {
      str_split(drivers_str, ";")[[1]] %>% str_trim()
    } else { character(0) }
    prob_list <- setNames(
      sapply(driver_choices, function(driver) input[[paste0("prob_driver_", make.names(driver))]]),
      driver_choices
    )
    rv_name <- paste0("edge_driver_prob_", rv$scenario)
    rv[[rv_name]][[rv$selected_edge]] <- prob_list
    showNotification("Driver probabilities saved.", type = "message")
  })
  
  # Observer for saving comments
  observeEvent(input$save_comment, {
    req(rv$selected_edge)
    rv$transition_comments[[rv$selected_edge]] <- input$transition_comment
    rv$add_comment_flags[[rv$selected_edge]] <- TRUE
    showNotification("Comment saved.", type = "message")
  })
  
  observeEvent(rv$selected_edge, {
    req(rv$selected_edge)
    
    # Reset the add_comment radio button
    if (!is.null(rv$add_comment_flags[[rv$selected_edge]])) {
      updateRadioButtons(session, "add_comment", selected = "Yes")
      updateTextAreaInput(session, "transition_comment", 
                          value = rv$transition_comments[[rv$selected_edge]])
    } else {
      updateRadioButtons(session, "add_comment", selected = "No")
      updateTextAreaInput(session, "transition_comment", value = "")
    }
  })
  
  observeEvent(input$add_interaction, {
    if (is.null(rv$interaction_count)) {
      rv$interaction_count <- 1
    }
    rv$interaction_count <- rv$interaction_count + 1
  })
  
  observeEvent(input$save_all_interactions, {
    req(rv$selected_edge)
    edge_id <- rv$selected_edge
    scenario <- rv$scenario
    interaction_list_name <- paste0("edge_driver_interactions_", scenario)
    if (is.null(rv[[interaction_list_name]][[edge_id]])) {
      rv[[interaction_list_name]][[edge_id]] <- list()
    }
    
    new_interactions <- list()
    
    for (i in seq_len(rv$interaction_count)) {
      d1 <- input[[paste0("interaction_driver1_", i)]]
      d2 <- input[[paste0("interaction_driver2_", i)]]
      prob <- input[[paste0("interaction_prob_", i)]]
      
      if (is.null(d1) || is.null(d2) || is.null(prob)) next
      
      if (d1 == d2) next
      
      inter_name <- paste(sort(c(d1, d2)), collapse = " + ")
      new_interactions[[inter_name]] <- prob
    }
    
    rv[[interaction_list_name]][[edge_id]] <- new_interactions
    
    showNotification("All interaction probabilities saved.", type = "message")
  })
  
  output$network <- renderVisNetwork({
    scenario <- rv$scenario
    edge_list_name <- paste0("edges_", scenario)
    active_edges <- rv[[edge_list_name]] %>%
      rowwise() %>%
      mutate(
        color = if (has_all_probs(id, scenario)) {
          if (scenario == "degradation") "#e41a1c" else "#56B4E9"
        } else {
          "#000000"
        },
        width = if (has_all_probs(id, scenario)) 4 else 2
      ) %>%
      ungroup()
    
    visNetwork(nodes, active_edges) %>%
      visNodes(
        shape = "box",
        font = list(size = 75, face = "bold", color = "black", multi = TRUE, vadjust = 0, align = "center", family = "arial"),
        borderWidth = 0.5,
        widthConstraint = list(minimum = 520, maximum = 520),
        heightConstraint = list(minimum = 140, maximum = 140)
      ) %>%
      visEdges(
        arrows = "to",
        smooth = FALSE,
        font = list(align = "middle", vadjust = -30, color = "black", size = 28, family = "arial")
      ) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE),
        selectedBy = "label",
        nodesIdSelection = TRUE
      ) %>%
      visPhysics(enabled = FALSE, stabilization = FALSE) %>%
      visLayout(randomSeed = 4321) %>%
      visEvents(
        selectEdge = "function(properties) {
        var selectedEdgeId = properties.edges[0];
        this.body.data.edges.forEach(function(edge) {
          if(edge.id === selectedEdgeId) {
            edge.color = {color: 'red'};
            edge.width = 4;
          } else {
            edge.color = {color: 'rgba(0,0,0,0.1)'};
            edge.width = 1;
          }
          this.body.data.edges.update(edge);
        }.bind(this));

        var selectedEdge = this.body.data.edges.get(selectedEdgeId);
        var from = selectedEdge.from;
        var to = selectedEdge.to;
        this.body.data.nodes.forEach(function(node) {
          if(node.id === from || node.id === to) {
            node.opacity = 1;
          } else {
            node.opacity = 0.2;
          }
          this.body.data.nodes.update(node);
        }.bind(this));

        Shiny.setInputValue('edge_id', selectedEdgeId);
      }",
        deselectEdge = "function(properties) {
        this.body.data.edges.forEach(function(edge) {
          edge.color = {color: 'black'};
          edge.width = 2;
          this.body.data.edges.update(edge);
        }.bind(this));

        this.body.data.nodes.forEach(function(node) {
          node.opacity = 1;
          this.body.data.nodes.update(node);
        }.bind(this));

        Shiny.setInputValue('edge_id', null);
      }"
      )
  })
  
  observe({
    input$network_selectedEdges
    isolate({
      new_selected <- input$network_selectedEdges
      
      if (length(new_selected) > 0) {
        if (!is.null(rv$selected_edge) && rv$selected_edge != new_selected) {
          visNetworkProxy("network") %>%
            visUpdateEdges(edges = data.frame(id = rv$selected_edge, color = "black", width = 2))
        }
        
        rv$selected_edge <- new_selected
        
        visNetworkProxy("network") %>%
          visUpdateEdges(edges = data.frame(id = new_selected, color = "red", width = 4))
        
      } else {
        if (!is.null(rv$selected_edge)) {
          visNetworkProxy("network") %>%
            visUpdateEdges(edges = data.frame(id = rv$selected_edge, color = "black", width = 2))
          rv$selected_edge <- NULL
        }
      }
    })
  })
  
  observeEvent(input$edge_id, {
    rv$interaction_count <- 1
    if (is.null(input$edge_id)) {
      rv$selected_edge <- NULL
      
      visNetworkProxy("network") %>%
        visUpdateEdges(edges = {
          edges <- switch(
            rv$scenario,
            "degradation" = rv$edges_degradation,
            "regeneration" = rv$edges_regeneration
          )
          edges %>%
            mutate(color = "#000000", width = 10) %>%
            select(id, color, width)
        }) %>%
        visUpdateNodes(nodes = {
          nodes %>%
            mutate(
              color.background = color.background,
              color.border = "black",
              font.color = "black"
            ) %>%
            select(id, color.background, color.border, font.color)
        })
    }
  })
  
  observeEvent(input$submit, {
    if (input$expert_id == "" || input$expert_name == "") {
      showNotification("Please enter your Expert Name and Email.", type = "error")
      return()
    }
    df_deg <- rv$edges_degradation %>%
      select(from, to, id) %>%
      rename(edge_id = id) %>%
      mutate(
        expert_name = input$expert_name,
        expert_email = input$expert_id,
        scenario = "degradation",
        driver_probabilities = sapply(edge_id, function(eid) driver_prob_label(rv$edge_driver_prob_degradation[[eid]])),
        interaction_probabilities = sapply(edge_id, function(eid) interaction_prob_label(rv$edge_driver_interactions_degradation[[eid]]))
      )
    df_reg <- rv$edges_regeneration %>%
      select(from, to, id) %>%
      rename(edge_id = id) %>%
      mutate(
        expert_name = input$expert_name,
        expert_email = input$expert_id,
        scenario = "regeneration",
        driver_probabilities = sapply(edge_id, function(eid) driver_prob_label(rv$edge_driver_prob_regeneration[[eid]])),
        interaction_probabilities = sapply(edge_id, function(eid) interaction_prob_label(rv$edge_driver_interactions_regeneration[[eid]]))
      )
    df <- bind_rows(df_deg, df_reg)
    filename <- paste0("responses_", input$expert_name, "_", input$expert_id, ".csv")
    write.csv(df, filename, row.names = FALSE)
    showNotification("All responses saved.", type = "message")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("STM_probabilities_", input$expert_name, "_", input$expert_id, ".csv")
    },
    content = function(file) {
      # Create degradation data without has_comment column
      df_deg <- rv$edges_degradation %>%
        select(from, to, id) %>%
        rename(edge_id = id) %>%
        mutate(
          expert_name = input$expert_name,
          expert_email = input$expert_id,
          scenario = "degradation",
          driver_probabilities = sapply(edge_id, function(eid) driver_prob_label(rv$edge_driver_prob_degradation[[eid]])),
          interaction_probabilities = sapply(edge_id, function(eid) interaction_prob_label(rv$edge_driver_interactions_degradation[[eid]])),
          transition_comments = sapply(edge_id, function(eid) {
            ifelse(is.null(rv$transition_comments[[eid]]), NA, rv$transition_comments[[eid]])
          })
        )
      
      # Create regeneration data without has_comment column
      df_reg <- rv$edges_regeneration %>%
        select(from, to, id) %>%
        rename(edge_id = id) %>%
        mutate(
          expert_name = input$expert_name,
          expert_email = input$expert_id,
          scenario = "regeneration",
          driver_probabilities = sapply(edge_id, function(eid) driver_prob_label(rv$edge_driver_prob_regeneration[[eid]])),
          interaction_probabilities = sapply(edge_id, function(eid) interaction_prob_label(rv$edge_driver_interactions_regeneration[[eid]])),
          transition_comments = sapply(edge_id, function(eid) {
            ifelse(is.null(rv$transition_comments[[eid]]), NA, rv$transition_comments[[eid]])
          })
        )
      
      # Combine and ensure comments column is last
      df <- bind_rows(df_deg, df_reg) %>%
        select(-any_of("has_comment")) %>%  # Explicitly remove if it exists
        relocate(transition_comments, .after = last_col())
      
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)


