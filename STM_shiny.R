library(shiny)
library(visNetwork)
library(dplyr)

# ---- Drivers dataframe ----
drivers_df <- data.frame(
  Broad.theme = c(
    rep("Clearing", 6),
    rep("Climate", 12),
    rep("Cultural management", 2),
    rep("Fire", 7),
    rep("Grazing management", 8),
    rep("Hydrology", 9),
    rep("Native fauna", 6),
    rep("Pests and diseases", 8),
    rep("Production", 11),
    rep("Restoration", 25),
    rep("Soil", 9),
    rep("Vegetation", 30),
    rep("Weeds", 7)
  ),
  Driver.pre.condition = c(
    # Clearing
    "Clearing (vegetation removal) - all strata",
    "Clearing (vegetation removal) - midstorey, shrubs or saplings",
    "Clearing (vegetation removal) - overstorey species (including saplings)",
    "Clearing (vegetation removal) - woody vegetation",
    "Selective thinning of the overstorey - large trees (e.g., removal of large, hollow bearing trees)",
    "Selective thinning of the overstorey - saplings (e.g., removal of saplings or non-hollow bearing trees)",
    # Climate
    "Average climatic conditions",
    "Climate - extreme weather - cyclone/storms",
    "Climate - extreme weather - flooding",
    "Climate - extreme weather - high/damaging winds",
    "Climate - hail damage",
    "Climate - temperature extremes (frost or extreme cold)",
    "Climate - temperature extremes (heatwave or extreme heat)",
    "Climate - unfavourable climatic conditions (drought)",
    "Climate change - increases or decreases in temperature and/or precipitation",
    "Climate-change induced changes to phenology including pollinators and flowering and reproductive output",
    "Favourable climatic conditions (mild summer, regular rainfall)",
    "Sea level rise due to climate change",
    # Cultural management
    "Lack or loss of cultural management, cultural severance to country",
    "Reinstating cultural management, custodianship and self-determination, including cultural use of plants and animals, and cultural fire",
    # Fire
    "Change in fire seasonality (non-reference fire seasonality) linked to changes in management and/or climate",
    "High intensity fire (catastrophic wildfire, bushfire) and/or increase in fire intensity",
    "Increase in fire frequency",
    "Low intensity fire and/or decrease in fire intensity",
    "Maintain reference fire regime",
    "Reduction in fire frequency",
    "Reinstate reference fire regime",
    # Grazing management
    "Exclude livestock (e.g., through fencing)",
    "Livestock grazing (set-stock, continuous, rotational)",
    "Maintain appropriate grazing pressure (native herbivores only)",
    "Manage and maintain appropriate total grazing pressure (includes native and introduced herbivores)",
    "Protection from browsing fauna (native or introduced)",
    "Reduce total grazing pressure (includes native and introduced herbivores)",
    "Sporadic high intensity livestock grazing (for biomass removal)",
    "Unregulated total grazing pressure (native and introduced herbivores)",
    # Hydrology
    "Altered groundwater table - lowered",
    "Altered groundwater table - raised",
    "Decrease in the natural movement, quantity, timing, or distribution of water within an ecosystem e.g. floodplain harvesting or water extraction, landscape dehydration",
    "Drainage of water and aquatic habitat (e.g., lakes, rivers, streams, swamps)",
    "Ground water contamination",
    "Increase in the natural movement, quantity, timing, or distribution of water within an ecosystem e.g. environmental flows in rivers, damming waterways",
    "Irrigation - temporary or permanent overflow or input of water onto normally dry land via sprinklers or flooding",
    "Landscape rehydration interventions including water ponding (bunding of waters to retain water)",
    "Salinisation of groundwater (saline groundwater)",
    # Native fauna
    "Native fauna - habitat maintenance - retention and protection of ground layer habitat and fauna refugia (logs, rocks)",
    "Native fauna - habitat degradation - removal of ground layer habitat and fauna refugia (e.g., logs, rocks)",
    "Native fauna - habitat restoration - addition of arboreal habitat (e.g., nest boxes, artificial hollows)",
    "Native fauna - habitat restoration - addition of ground layer habitat and fauna refugia (e.g., logs, rocks)",
    "Overabundance of native fauna",
    "Reintroduction of native fauna, including predators (e.g., via translocation)",
    # Pests and diseases
    "Control of feral herbivores and/or predators leading to a reduction in their abundance and related impacts",
    "Herbivory and/or browsing by vertebrate pests (e.g., goats, deer, horses, donkeys, camels, rabbits, buffalo, other hard-hooved ungulates)",
    "Herbivory by defoliating insects leading to tree stress or mortality",
    "Inadequate of monitoring or control of fauna (feral predators, pest animals, overabundant native herbivores)",
    "Manage pest or disease outbreaks e.g. through the application of pesticides, sanitation processes or introduction of biocontrol agents",
    "Mortality due to fungal diseases (e.g., Phytophthora, Myrtle rust)",
    "Overabundance of feral animals due to lack or no active management or control",
    "Viruses, bacteria or parasites impacting native fauna abundance",
    # Production
    "Cessation of cropping and cultivation",
    "Cessation of fertiliser addition",
    "Cessation of horticulture (cultivation of exotic plants species)",
    "Cessation of land-use (land abandonment)",
    "Cessation of selective logging",
    "Cropping - conversion from mixed-pastures and/or native pastures to monoculture",
    "Cropping and cultivation (including fertiliser addition, tilling) or sowing of pasture species",
    "Cultivation - planting of native tree plantations (e.g., native forestry), sandalwood plantations",
    "Horticulture - cultivation of exotic woody plant species (e.g., orchards, olive groves, non-native forestry)",
    "Logging (e.g., harvesting, forestry, firewood collection, biomass removal, clearfell, selective, salvage)",
    "Cease land-use (abandonment) and remove barriers to plant growth (e.g. pavement, buildings)",
    # Restoration
    "Direct native seeding of locally endemic species indigenous to historical climate and/or climate adapted species",
    "Fencing to exclude pest herbivores and predators",
    "Mowing or slashing of the herbaceous layer with the removal of biomass",
    "Mowing or slashing of the herbaceous layer: no removal of biomass",
    "Mulching",
    "Passive restoration and natural vegetation regrowth",
    "Planting seedlings",
    "Protect soil and ground layer from disturbance",
    "Restoration of soil nutrient balance (e.g., scalping, addition of sugar, sand, or other method)",
    "Restoration of soil structure and hydrological function (e.g., reduce erosion, increase resource retention and infiltration)",
    "Revegetation of all strata with reference species",
    "Revegetation of exotic (non-native) vegetation, e.g. lawns, gardens",
    "Revegetation of ground layer species",
    "Revegetation of halophytes (salt tolerant species)",
    "Revegetation of midstorey species",
    "Revegetation of overstorey species",
    "Ripping for direct seeding",
    "Scalping to remove weed seeds",
    "Sow cover crops",
    "Supplementary planting and addition of seed for ground layer species",
    "Supplementary planting of any species that haven't previously established",
    "Supplementary planting of late successional species",
    "Supplementary planting of midstorey species",
    "Supplementary planting of overstorey species",
    "Tube stock survival and growth",
    # Soil
    "Change in soil nutrient cycling (e.g. nitrogen release)",
    "Nutrification of soil (e.g., from stock camps, fertiliser addition)",
    "Reference soil nutrient levels",
    "Saline scalding",
    "Soil disturbance and degradation of structure (e.g., mechanical, trampling, ripping, excavation)",
    "Soil remediation e.g. addition of ameliorants including mycorrhizae, lime, gypsum",
    "Soil stabilisation techniques",
    "Top soil loss due to wind erosion",
    "Top soil overburden or removal (e.g. mining, top soil stripping for restoration)",
    # Vegetation (abbreviated)
    "Disruption of plant recruitment due to lack of native seed germination",
    "Exotic species propagules in landscape",
    "Growth and development of mature overstorey structure",
    "Identify cause of canopy collapse",
    "Inadequate or slow self-thinning of woody species",
    "Inappropriate removal of native seeds via seed collection",
    "Increased edge effects (light, exotic propagules)",
    "Lack of available native propagules",
    "Lack of recruitment of reference shrub species",
    "Lack of recruitment of reference tree species",
    "Native propagules available",
    "Reduction in edge effects",
    "Reduction in weed propagules",
    "Seed germination and establishment",
    "Shrub mortality",
    "Transformer weed propagules in landscape",
    "Vegetation fragmentation",
    "Vegetation establishment - dense non-reference shrubs",
    "Vegetation establishment - reference midstorey structure",
    "Vegetation establishment - tree seedlings into saplings",
    "Vegetation establishment - reference species - all strata",
    "Vegetation establishment of exotic non-transformer species",
    "Vegetation establishment of exotic transformer species",
    "Vegetation establishment of grazing sensitive species",
    "Vegetation establishment of overstorey species",
    "Vegetation establishment of reference understorey species",
    "Vegetation maturation - saplings to mature overstorey",
    "Vegetation mortality - canopy loss",
    "Vegetation mortality - mature overstorey death",
    "Vegetation mortality - tree/shrub tube stock death",
    # Weeds
    "Establishment of non-woody exotic plant species",
    "Generic weed management",
    "Low impact chemical weed control",
    "Strategic livestock grazing to manage weeds",
    "Weed monitoring, surveillance and control",
    "Woody weed removal (including vines)",
    "Weed management using fire"
  ),
  stringsAsFactors = FALSE
)


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
    "Reference overstorey with highly modified understorey",
    "Modified overstorey with reference understorey",
    "Reference overstorey with collapsed/transformer understorey"
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
    "Reference overstorey with collapsed/transformer understorey",
    "Sparse/absent overstorey with modified understorey",
    "Modified overstorey with collapsed/transformer understorey"
  ),
  id = paste0("deg_", 1:25),
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
    "Reference overstorey with modified overstorey",
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
      h4("Year selection"),
      actionButton("set_2025", "Edit 2025"),
      actionButton("set_2100", "Edit 2100"),
      br(),
      uiOutput("scenario_display"),
      br(),
      uiOutput("selected_transition"),
      conditionalPanel(
        condition = "output.transitionSelected",
        sliderInput("prob_input", "Probability", min = 0, max = 1, value = 0, step = 0.01),
        uiOutput("driver_select_ui"),
        actionButton("save_prob", "Save Probability")
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
    edges_degradation_2025 = edges_degradation,
    edges_degradation_2100 = edges_degradation,
    edges_regeneration_2025 = edges_regeneration,
    edges_regeneration_2100 = edges_regeneration,
    selected_edge = NULL,
    edge_drivers_degradation_2025 = setNames(vector("list", nrow(edges_degradation)), edges_degradation$id),
    edge_drivers_degradation_2100 = setNames(vector("list", nrow(edges_degradation)), edges_degradation$id),
    edge_drivers_regeneration_2025 = setNames(vector("list", nrow(edges_regeneration)), edges_regeneration$id),
    edge_drivers_regeneration_2100 = setNames(vector("list", nrow(edges_regeneration)), edges_regeneration$id),
    scenario = "degradation",
    year = "2025"
  )
  
  # Scenario switching
  observeEvent(input$set_degradation, {
    rv$scenario <- "degradation"
    rv$selected_edge <- NULL
    updateSliderInput(session, "prob_input", value = 0)
  })
  observeEvent(input$set_regeneration, {
    rv$scenario <- "regeneration"
    rv$selected_edge <- NULL
    updateSliderInput(session, "prob_input", value = 0)
  })
  observeEvent(input$set_2025, {
    rv$year <- "2025"
    rv$selected_edge <- NULL
    updateSliderInput(session, "prob_input", value = 0)
  })
  observeEvent(input$set_2100, {
    rv$year <- "2100"
    rv$selected_edge <- NULL
    updateSliderInput(session, "prob_input", value = 0)
  })
  
  output$scenario_display <- renderUI({
    strong(paste("Currently editing:", toupper(rv$scenario), " - Year", rv$year))
  })
  
  output$transitionSelected <- reactive({
    !is.null(rv$selected_edge)
  })
  outputOptions(output, "transitionSelected", suspendWhenHidden = FALSE)
  
  output$selected_transition <- renderUI({
    req(input$edge_id)
    edge <- switch(
      paste(rv$scenario, rv$year),
      "degradation 2025" = rv$edges_degradation_2025,
      "degradation 2100" = rv$edges_degradation_2100,
      "regeneration 2025" = rv$edges_regeneration_2025,
      "regeneration 2100" = rv$edges_regeneration_2100
    ) %>% filter(id == input$edge_id)
    rv$selected_edge <- input$edge_id
    from <- edge$from
    to <- edge$to
    HTML(paste0("<strong>Selected Transition (", toupper(rv$scenario), " - ", rv$year, "):</strong><br>", from, " → ", to))
  })
  
  output$driver_select_ui <- renderUI({
    req(rv$selected_edge)
    driver_choices <- split(drivers_df$Driver.pre.condition, drivers_df$Broad.theme)
    
    selected_drivers <- switch(
      paste(rv$scenario, rv$year),
      "degradation 2025" = rv$edge_drivers_degradation_2025[[rv$selected_edge]],
      "degradation 2100" = rv$edge_drivers_degradation_2100[[rv$selected_edge]],
      "regeneration 2025" = rv$edge_drivers_regeneration_2025[[rv$selected_edge]],
      "regeneration 2100" = rv$edge_drivers_regeneration_2100[[rv$selected_edge]]
    )
    selectizeInput(
      "drivers_selected",
      "Select one or more drivers/preconditions:",
      choices = driver_choices,
      selected = selected_drivers,
      multiple = TRUE,
      options = list(placeholder = 'Choose drivers...')
    )
  })
  
  observeEvent(input$save_prob, {
    req(rv$selected_edge)
    idx <- switch(
      paste(rv$scenario, rv$year),
      "degradation 2025" = which(rv$edges_degradation_2025$id == rv$selected_edge),
      "degradation 2100" = which(rv$edges_degradation_2100$id == rv$selected_edge),
      "regeneration 2025" = which(rv$edges_regeneration_2025$id == rv$selected_edge),
      "regeneration 2100" = which(rv$edges_regeneration_2100$id == rv$selected_edge)
    )
    if (rv$scenario == "degradation" && rv$year == "2025") {
      rv$edges_degradation_2025$label[idx] <- sprintf("%.2f", input$prob_input)
      #rv$edges_degradation_2025$value[idx] <- input$prob_input * 10
      rv$edge_drivers_degradation_2025[[rv$selected_edge]] <- input$drivers_selected
    } else if (rv$scenario == "degradation" && rv$year == "2100") {
      rv$edges_degradation_2100$label[idx] <- sprintf("%.2f", input$prob_input)
      #rv$edges_degradation_2100$value[idx] <- input$prob_input * 10
      rv$edge_drivers_degradation_2100[[rv$selected_edge]] <- input$drivers_selected
    } else if (rv$scenario == "regeneration" && rv$year == "2025") {
      rv$edges_regeneration_2025$label[idx] <- sprintf("%.2f", input$prob_input)
      #rv$edges_regeneration_2025$value[idx] <- input$prob_input * 10
      rv$edge_drivers_regeneration_2025[[rv$selected_edge]] <- input$drivers_selected
    } else if (rv$scenario == "regeneration" && rv$year == "2100") {
      rv$edges_regeneration_2100$label[idx] <- sprintf("%.2f", input$prob_input)
      #rv$edges_regeneration_2100$value[idx] <- input$prob_input * 10
      rv$edge_drivers_regeneration_2100[[rv$selected_edge]] <- input$drivers_selected
    }
    showNotification("Probability and drivers saved.", type = "message")
  })
  
  output$network <- renderVisNetwork({
    active_edges <- switch(
      paste(rv$scenario, rv$year),
      "degradation 2025" = rv$edges_degradation_2025,
      "degradation 2100" = rv$edges_degradation_2100,
      "regeneration 2025" = rv$edges_regeneration_2025,
      "regeneration 2100" = rv$edges_regeneration_2100
    )
    # Arrow color logic
    active_edges <- active_edges %>%
      mutate(
        color = ifelse(label != "" & label != "0.00",
                       ifelse(rv$scenario == "degradation", "#e41a1c", "#2ca02c"),
                       "#000000"),
        width = 6  # fixed width for all arrows
      )
    
    center_x <- mean(nodes$x)
    center_y <- mean(nodes$y)
    visNetwork(nodes, active_edges) %>%
      visNodes(
        shape = "box",
        font = list(size = 75, face = "bold", 
                    color = "black", multi = TRUE, 
                    vadjust=0, align="center", family="arial"),
        borderWidth = .5,
        widthConstraint = list(minimum = 520, maximum = 520),
        heightConstraint = list(minimum = 140, maximum = 140)
      ) %>%
      visEdges(
        arrows = "to",
        smooth = FALSE,
        font = list(
          align = "middle", 
          vadjust = -30, 
          color = "black", 
          size = 28, 
          family = "arial"
        )
      ) %>%
      visOptions(
        highlightNearest = list(
          enabled = TRUE,
          degree = 1,
          hover = FALSE
        ), 
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
        }"
      )
  })
  
  # Helper: get "Broad theme - Driver" string for each driver
  driver_theme_label <- function(driver_vec) {
    if (length(driver_vec) == 0 || all(is.na(driver_vec))) return("")
    sapply(driver_vec, function(drv) {
      idx <- which(drivers_df$Driver.pre.condition == drv)
      if (length(idx) > 0) {
        paste0(drivers_df$Broad.theme[idx[1]], " - ", drv)
      } else {
        drv
      }
    }) %>% paste(collapse = "; ")
  }
  
  observeEvent(input$submit, {
    if (input$expert_id == "" || input$expert_name == "") {
      showNotification("Please enter your Expert Name and Email.", type = "error")
      return()
    }
    # Collect for all scenarios/years
    df_deg_2025 <- rv$edges_degradation_2025 %>%
      select(from, to, label, id) %>%
      rename(probability = label, edge_id = id) %>%
      mutate(
        probability = as.numeric(probability),
        expert_name = input$expert_name,
        expert_email = input$expert_id,
        scenario = "degradation",
        year = "2025",
        drivers = sapply(edge_id, function(eid) driver_theme_label(rv$edge_drivers_degradation_2025[[eid]]))
      )
    df_deg_2100 <- rv$edges_degradation_2100 %>%
      select(from, to, label, id) %>%
      rename(probability = label, edge_id = id) %>%
      mutate(
        probability = as.numeric(probability),
        expert_name = input$expert_name,
        expert_email = input$expert_id,
        scenario = "degradation",
        year = "2100",
        drivers = sapply(edge_id, function(eid) driver_theme_label(rv$edge_drivers_degradation_2100[[eid]]))
      )
    df_reg_2025 <- rv$edges_regeneration_2025 %>%
      select(from, to, label, id) %>%
      rename(probability = label, edge_id = id) %>%
      mutate(
        probability = as.numeric(probability),
        expert_name = input$expert_name,
        expert_email = input$expert_id,
        scenario = "regeneration",
        year = "2025",
        drivers = sapply(edge_id, function(eid) driver_theme_label(rv$edge_drivers_regeneration_2025[[eid]]))
      )
    df_reg_2100 <- rv$edges_regeneration_2100 %>%
      select(from, to, label, id) %>%
      rename(probability = label, edge_id = id) %>%
      mutate(
        probability = as.numeric(probability),
        expert_name = input$expert_name,
        expert_email = input$expert_id,
        scenario = "regeneration",
        year = "2100",
        drivers = sapply(edge_id, function(eid) driver_theme_label(rv$edge_drivers_regeneration_2100[[eid]]))
      )
    df <- bind_rows(df_deg_2025, df_deg_2100, df_reg_2025, df_reg_2100)
    filename <- paste0("responses_", input$expert_name, "_", input$expert_id, ".csv")
    write.csv(df, filename, row.names = FALSE)
    showNotification("All responses saved.", type = "message")
  })
  
  observeEvent(input$edge_id, {
    req(input$edge_id)
    rv$selected_edge <- input$edge_id
    
    active_edges <- switch(
      paste(rv$scenario, rv$year),
      "degradation 2025" = rv$edges_degradation_2025,
      "degradation 2100" = rv$edges_degradation_2100,
      "regeneration 2025" = rv$edges_regeneration_2025,
      "regeneration 2100" = rv$edges_regeneration_2100
    )
    
    idx <- which(active_edges$id == input$edge_id)
    if (length(idx) == 1) {
      prob <- active_edges$label[idx]
      prob_val <- ifelse(prob != "" && !is.na(as.numeric(prob)), as.numeric(prob), 0)
      updateSliderInput(session, "prob_input", value = prob_val)
    } else {
      updateSliderInput(session, "prob_input", value = 0)
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("STM_probabilities_", input$expert_name, "_", input$expert_id, ".csv")
    },
    content = function(file) {
      df_deg_2025 <- rv$edges_degradation_2025 %>%
        select(from, to, label, id) %>%
        rename(probability = label, edge_id = id) %>%
        mutate(
          probability = as.numeric(probability),
          expert_name = input$expert_name,
          expert_email = input$expert_id,
          scenario = "degradation",
          year = "2025",
          drivers = sapply(edge_id, function(eid) driver_theme_label(rv$edge_drivers_degradation_2025[[eid]]))
        )
      df_deg_2100 <- rv$edges_degradation_2100 %>%
        select(from, to, label, id) %>%
        rename(probability = label, edge_id = id) %>%
        mutate(
          probability = as.numeric(probability),
          expert_name = input$expert_name,
          expert_email = input$expert_id,
          scenario = "degradation",
          year = "2100",
          drivers = sapply(edge_id, function(eid) driver_theme_label(rv$edge_drivers_degradation_2100[[eid]]))
        )
      df_reg_2025 <- rv$edges_regeneration_2025 %>%
        select(from, to, label, id) %>%
        rename(probability = label, edge_id = id) %>%
        mutate(
          probability = as.numeric(probability),
          expert_name = input$expert_name,
          expert_email = input$expert_id,
          scenario = "regeneration",
          year = "2025",
          drivers = sapply(edge_id, function(eid) driver_theme_label(rv$edge_drivers_regeneration_2025[[eid]]))
        )
      df_reg_2100 <- rv$edges_regeneration_2100 %>%
        select(from, to, label, id) %>%
        rename(probability = label, edge_id = id) %>%
        mutate(
          probability = as.numeric(probability),
          expert_name = input$expert_name,
          expert_email = input$expert_id,
          scenario = "regeneration",
          year = "2100",
          drivers = sapply(edge_id, function(eid) driver_theme_label(rv$edge_drivers_regeneration_2100[[eid]]))
        )
      df <- bind_rows(df_deg_2025, df_deg_2100, df_reg_2025, df_reg_2100)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
