##########################################
# OSRS BARROWS COLLECTION LOG CALCULATOR #
##########################################

library(shiny)
library(dplyr)
library(DT)
library(scales)
library(bslib)
library(plotly)

# DATA
######

brothers <- c(
  "Ahrim",
  "Dharok",
  "Guthan",
  "Karil",
  "Torag",
  "Verac")

barrows_data <- data.frame(
  brother = c(
    rep("Ahrim", 4),
    rep("Dharok", 4),
    rep("Guthan", 4),
    rep("Karil", 4),
    rep("Torag", 4),
    rep("Verac", 4)),
  
  set = c(
    rep("Ahrim's", 4),
    rep("Dharok's", 4),
    rep("Guthan's", 4),
    rep("Karil's", 4),
    rep("Torag's", 4),
    rep("Verac's", 4)),
  
  item = c(
    "Ahrim's hood",
    "Ahrim's robetop",
    "Ahrim's robeskirt",
    "Ahrim's staff",
    
    "Dharok's helm",
    "Dharok's platebody",
    "Dharok's platelegs",
    "Dharok's greataxe",
    
    "Guthan's helm",
    "Guthan's platebody",
    "Guthan's chainskirt",
    "Guthan's warspear",
    
    "Karil's coif",
    "Karil's leathertop",
    "Karil's leatherskirt",
    "Karil's crossbow",
    
    "Torag's helm",
    "Torag's platebody",
    "Torag's platelegs",
    "Torag's hammers",
    
    "Verac's helm",
    "Verac's brassard",
    "Verac's plateskirt",
    "Verac's flail"),
  stringsAsFactors = FALSE)

# BARROWS MECHANICS
###################

equipment_rolls <- function(n) {n + 1}

equipment_roll_probability <- function(n) {1 / (450 - 58 * n)}

available_equipment <- function(n) {4 * n}

expected_equipment <- function(n) {
  equipment_rolls(n) * equipment_roll_probability(n)}

specific_item_probability <- function(n) {
  M <- available_equipment(n)
  expected_equipment(n) / M}

target_item_probability <- function(target_brother, killed_brothers) {
  n <- length(killed_brothers)
  specific_item_probability(n)}

median_chests_for_item <- function(p) {ceiling(log(0.5) / log(1 - p))}

chance_item_after_chests <- function(p, chests) {1 - (1 - p) ^ chests}

completion_transition_matrix <- function(n_brothers, max_missing) {
  M <- available_equipment(n_brothers)
  rolls <- equipment_rolls(n_brothers)
  p <- equipment_roll_probability(n_brothers)
  T <- matrix(0, max_missing + 1, max_missing + 1)
  
  for (m in 0:max_missing) {
    for (k in 0:rolls) {
      j <- max(0, k - (M - m)):min(k, m)
      T[m + 1, m - j + 1] <-
        T[m + 1, m - j + 1] +
        dbinom(k, rolls, p) *
        dhyper(j, m, M - m, k)}}
  T}

collection_completion_probability <- function(
    missing_items,
    chests,
    killed_brothers) {
  n <- length(killed_brothers)
  
  if (missing_items <= 0) {return(1)}
  
  transition <- completion_transition_matrix(
    n_brothers = n,
    max_missing = missing_items)
  
  state <- numeric(missing_items + 1)
  
  state[missing_items + 1] <- 1
  
  for (i in seq_len(chests)) {
    
    state <- as.numeric(state %*% transition)}
  as.numeric(state[1])}

collection_completion_curve <- function(
    missing_items,
    killed_brothers,
    max_chests = 1000,
    step = 10) {
  if (missing_items <= 0) {
    return(data.frame(chests = 0, probability = 1))}
  
  n <- length(killed_brothers)
  
  chests_to_record <- seq(0, max_chests, by = step)
  transition <- completion_transition_matrix(
    n_brothers = n,
    max_missing = missing_items)
  
  state <- numeric(missing_items + 1)
  
  state[missing_items + 1] <- 1
  
  result <- numeric(length(chests_to_record))
  
  result[1] <- 0
  
  record_index <- 2
  
  for (chest in seq_len(max_chests)) {
    state <- as.numeric(state %*% transition)
    if (
      record_index <= length(chests_to_record) &&
      chest == chests_to_record[record_index]) {
      result[record_index] <- state[1]
      record_index <- record_index + 1}
    if (record_index > length(chests_to_record)) 
      {break}}
  
  data.frame(chests = chests_to_record, probability = result)}

completion_50_chests <- function(
    missing_items,
    killed_brothers,
    max_chests = 5000) {
  if (missing_items <= 0) {return(0L)}
  if (length(killed_brothers) == 0) {return(NA_integer_)}
  if (collection_completion_probability(
    missing_items = missing_items,
    chests = max_chests,
    killed_brothers = killed_brothers) < 0.5) {return(NA_integer_)}
  
  lo <- 1L
  hi <- max_chests
  while (lo < hi) {
    mid <- lo + (hi - lo) %/% 2L
    p <- collection_completion_probability(
      missing_items = missing_items,
      chests = mid,
      killed_brothers = killed_brothers)
    if (p >= 0.5) {hi <- mid} 
    else {lo <- mid + 1L}}
  lo}

# UI
####

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly"),
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #212529;
        }

        .main-title {
          margin-bottom: 25px;
        }

        .summary-card {
          background-color: #2b3035;
          border: 1px solid #495057;
          border-radius: 8px;
          padding: 18px;
          text-align: center;
          margin-bottom: 15px;
        }

        .summary-number {
          font-size: 2em;
          font-weight: bold;
          color: #f8f9fa;
        }

        .summary-label {
          color: #adb5bd;
          font-size: 0.9em;
        }

        .collection-progress {
          height: 28px;
          background-color: #343a40;
          border-radius: 6px;
          overflow: hidden;
          margin-bottom: 25px;
        }

        .collection-progress-bar {
          height: 100%;
          background-color: #28a745;
          text-align: center;
          color: white;
          font-weight: bold;
          line-height: 28px;
        }

        .collection-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(270px, 1fr));
          gap: 15px;
          margin-top: 15px;
        }

        .barrows-set {
          background-color: #2b3035;
          border: 1px solid #495057;
          border-radius: 8px;
          padding: 15px;
        }

        .set-title {
          font-size: 1.2em;
          font-weight: bold;
          margin-bottom: 5px;
          color: #f8f9fa;
        }

        .set-complete {
          color: #4cd964;
        }

        .set-progress {
          color: #adb5bd;
          font-size: 0.9em;
          margin-bottom: 12px;
        }

        .barrows-item {
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding: 8px;
          margin-bottom: 5px;
          border-radius: 5px;
          background-color: #343a40;
        }

        .barrows-item.collected {
          background-color: #183d27;
          border-left: 4px solid #28a745;
        }

        .barrows-item.missing {
          border-left: 4px solid #6c757d;
        }

        .item-name {
          color: #f8f9fa;
        }

        .item-count {
          color: #adb5bd;
          font-size: 0.85em;
        }

        .analysis-card {
          background-color: #2b3035;
          border: 1px solid #495057;
          border-radius: 8px;
          padding: 18px;
          margin-bottom: 15px;
        }

        .analysis-title {
          font-weight: bold;
          color: #f8f9fa;
          font-size: 1.1em;
        }

        .analysis-value {
          font-size: 1.7em;
          font-weight: bold;
          color: #4cd964;
        }

        .analysis-subtitle {
          color: #adb5bd;
          font-size: 0.85em;
        }

        .recommendation {
          background-color: #173b28;
          border: 1px solid #286b45;
          border-radius: 8px;
          padding: 18px;
          margin-bottom: 20px;
        }

        .warning-box {
          background-color: #3d3214;
          border: 1px solid #856404;
          color: #ffe69c;
          border-radius: 6px;
          padding: 12px;
          margin-top: 10px;
          margin-bottom: 15px;
        }

        .calculator-notes {
          margin-top: 30px;
          margin-bottom: 20px;
          color: #adb5bd;
          font-size: 0.9em;
        }

        .selectize-input,
        .selectize-control.single .selectize-input,
        .selectize-control.multi .selectize-input {
          background-color: #343a40 !important;
          color: #f8f9fa !important;
          border: 1px solid #6c757d !important;
        }

        .selectize-input .item {
          color: #f8f9fa !important;
        }

        .selectize-dropdown,
        .selectize-dropdown-content {
          background-color: #343a40 !important;
          color: #f8f9fa !important;
        }

        .selectize-dropdown .option {
          background-color: #343a40 !important;
          color: #f8f9fa !important;
        }

        .selectize-dropdown .option:hover,
        .selectize-dropdown .option.active {
          background-color: #495057 !important;
          color: white !important;
        }

        input.form-control,
        input[type='number'] {
          background-color: #343a40 !important;
          color: #f8f9fa !important;
          border: 1px solid #6c757d !important;
        }

        label,
        .form-label,
        .form-check-label {
          color: #e9ecef !important;
        }

        .sidebar,
        .well {
          background-color: #212529;
        }

        .collection-input-group {
          margin-bottom: 4px;
        }

        .collection-input-group .form-group {
          margin-bottom: 4px;
        }

        .collection-input-group label {
          font-size: 0.82em;
          margin-bottom: 2px;
        }

        .collection-input-group input {
          height: 30px;
          padding: 3px 7px;
          font-size: 0.85em;
        }

        .brother-section {
          margin-bottom: 8px;
          border: 1px solid #495057;
          border-radius: 7px;
          overflow: hidden;
          background-color: #2b3035;
        }

        .brother-header {
          background-color: #343a40;
          color: #f8f9fa;
          padding: 9px 12px;
          cursor: pointer;
          font-weight: bold;
          user-select: none;
          display: flex;
          align-items: center;
          justify-content: space-between;
        }

        .brother-header:hover {
          background-color: #495057;
        }

        .brother-header-left {
          display: flex;
          align-items: center;
        }

        .brother-body {
          padding: 10px;
          display: none;
        }

        .brother-status {
          color: #adb5bd;
          font-weight: normal;
          font-size: 0.85em;
        }

        .brother-complete {
          color: #4cd964;
        }

        .brother-toggle {
          display: inline-block;
          width: 18px;
          margin-right: 5px;
          color: #adb5bd;
          font-size: 0.9em;
          text-align: center;
        }")),
    
    tags$script(
      HTML("
        window.barrowsAccordionState =
          window.barrowsAccordionState || {};

        function restoreBarrowsAccordion() {

          $('.brother-section').each(function() {

            var section = $(this);

            var sectionId =
              section.attr('id');

            var header =
              section.find(
                '.brother-header'
              ).first();

            var body =
              section.find(
                '.brother-body'
              ).first();

            var arrow =
              header.find(
                '.brother-toggle'
              ).first();

            if (
              Object.prototype.hasOwnProperty.call(
                window.barrowsAccordionState,
                sectionId
              )
            ) {

              var isOpen =
                window.barrowsAccordionState[
                  sectionId
                ];


              if (isOpen) {

                body.show();

                arrow.text('▼');

                header.attr(
                  'aria-expanded',
                  'true'
                );

              } else {

                body.hide();

                arrow.text('▶');

                header.attr(
                  'aria-expanded',
                  'false'
                );
              }
            }
          });
        }

        $(document)
          .off(
            'click.barrowsAccordion',
            '.brother-header'
          );

        $(document).on(
          'click.barrowsAccordion',
          '.brother-header',
          function(e) {

            e.preventDefault();
            e.stopPropagation();

            var header = $(this);

            var section =
              header.closest(
                '.brother-section'
              );

            var sectionId =
              section.attr('id');

            var body =
              header.siblings(
                '.brother-body'
              ).first();

            var arrow =
              header.find(
                '.brother-toggle'
              ).first();

            var isOpen =
              body.is(':visible');


            if (isOpen) {

              body
                .stop(true, true)
                .slideUp(150);

              arrow.text('▶');

              header.attr(
                'aria-expanded',
                'false'
              );

              window.barrowsAccordionState[
                sectionId
              ] = false;

            } else {

              body
                .stop(true, true)
                .slideDown(150);

              arrow.text('▼');

              header.attr(
                'aria-expanded',
                'true'
              );

              window.barrowsAccordionState[
                sectionId
              ] = true;
            }
          }
        );

        $(document).on(
          'shiny:value',
          function(event) {

            if (
              event.target &&
              event.target.id ===
                'actual_drop_inputs'
            ) {

              setTimeout(
                function() {

                  restoreBarrowsAccordion();

                },
                0
              );
            }
          }
        );

        $(document).on(
          'shiny:connected',
          function() {

            setTimeout(
              function() {

                restoreBarrowsAccordion();

              },
              0
            );
          }
        );"))),
  
  titlePanel("OSRS Barrows Collection Log Calculator"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = paste0(
          "input.main_tabs == 'Collection Log' || ",
          "input.main_tabs == 'How Dry Am I?' || ",
          "input.main_tabs == 'Item Odds' || ",
          "input.main_tabs == 'Completion Odds'"),
        numericInput(
          inputId = "kills",
          label = "Barrows chests completed:",
          value = 0,
          min = 0,
          step = 1)),
      
      conditionalPanel(
        condition = paste0(
          "input.main_tabs == 'How Dry Am I?' || ",
          "input.main_tabs == 'Item Odds' || ",
          "input.main_tabs == 'Completion Odds'"),
        selectizeInput(
          inputId = "future_brothers",
          label = "Brothers you plan to kill:",
          choices = brothers,
          selected = brothers,
          multiple = TRUE,
          options = list(
            maxItems = 6,
            plugins = list("remove_button")))),
      
      conditionalPanel(
        condition = paste0(
          "input.main_tabs == 'Collection Log' || ",
          "input.main_tabs == 'How Dry Am I?' || ",
          "input.main_tabs == 'Completion Odds'"),
        h4("Your Collection"),
        p(style = "color:#adb5bd; font-size:0.85em;",
          "Click a brother to expand or collapse their equipment."),
        uiOutput("actual_drop_inputs")),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Item Odds'",
        selectizeInput(
          inputId = "graph_item",
          label = "Probability graph:",
          choices = barrows_data$item,
          selected = barrows_data$item[1],
          options = list(
            maxOptions = 24)))),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  tabPanel("Collection Log",
                           br(),
                           h3("Barrows Collection Progress"),
                           uiOutput("summary_cards"),
                           uiOutput("overall_progress"),
                           h3("Equipment Collection"),
                           uiOutput("collection_log")),
                  
                  tabPanel("How Dry Am I?",
                           br(),
                           uiOutput("dryness_summary"),
                           h3("Missing Equipment"),
                           DTOutput("missing_table")),
                  
                  tabPanel("Item Odds",
                           br(),
                           uiOutput("item_probability_summary"),
                           plotlyOutput(
                             "probability_plot",
                             height = "500px")),
                  
                  tabPanel(
                    "Completion Odds",
                    br(),
                    uiOutput("completion_summary"),
                    plotlyOutput(
                      "completion_plot",
                      height = "500px"))),
      
      div(class = "calculator-notes",
          p("RuneScape is a registered trademark of Jagex Ltd."),
          p("Barrows mechanics based on the ",
            a("Old School RuneScape Wiki",
              href = "https://oldschool.runescape.wiki/",
              target = "_blank"))))))

# SERVER
########

server <- function(
    input,
    output,
    session
) {
  future_brothers <- reactive({
    selected <- input$future_brothers
    # had to include defensive code around displaying 0s, becase the app would
    # break if the reactive elements weren't specified to 0 on the first load
    if (
      is.null(selected) ||
      length(selected) == 0
    ) {character(0)} 
    else {selected}})
  
  current_kc <- reactive({
    value <- input$kills
    if (
      is.null(value) ||
      is.na(value) ||
      value < 0
    ) {return(0)}
    floor(value)})
  
  output$actual_drop_inputs <- renderUI({
    tagList(
      lapply(
        brothers,
        function(brother) {
          indices <- which(
            barrows_data$brother == brother)
          
          input_ids <- paste0("actual_", indices)
          
          count_values <- sapply(
            input_ids,
            function(id) {
              value <- input[[id]]
              if (
                is.null(value) ||
                is.na(value) ||
                value < 0
              ) {return(0)}
              floor(value)})
          
          collected <- sum(count_values > 0)
          
          section_id <- paste0("brother_", tolower(brother))
          
          is_ahrim <- identical(brother, "Ahrim")
          # wife didn't realize the accordions were accordions when she first
          # looked at it, so I decided to have Ahrim expanded to show how the
          # app works. Not quite as clean looking, but I think is more user
          # friendly
          div(
            id = section_id,
            class = "brother-section",
            
            div(class = "brother-header",
                `aria-expanded` = if (
                  is_ahrim
                ) {"true"} 
                else {"false"},
                
                div(class = "brother-header-left",
                    span(class = "brother-toggle",
                         
                         if (is_ahrim) {"▼"} 
                         else {"▶"}),
                    span(brother)),
                
                span(
                  class = if (collected == 4) {
                    "brother-status brother-complete"} 
                  else {"brother-status"},
                  
                  paste0(collected, " / 4"))),
            
            div(class = "brother-body",
                style = if (is_ahrim) {"display:block;"} 
                else {"display:none;"},
                
                lapply(
                  seq_along(indices),
                  function(j) {
                    i <- indices[j]
                    current_value <- input[[paste0("actual_", i)]]
                    
                    if (is.null(current_value) ||
                        is.na(current_value)) {
                      current_value <- 0}
                    
                    div(class = "collection-input-group",
                        numericInput(
                          inputId = paste0("actual_", i),
                          label = barrows_data$item[i],
                          value = current_value,
                          min = 0,
                          step = 1,
                          width = "100%"))})))}))})
  
  actual_drops <- reactive({
    sapply(seq_len(nrow(barrows_data)),
           
           function(i) {
             value <- input[[paste0("actual_", i)]]
             
             if (
               is.null(value) ||
               is.na(value) ||
               value < 0
             ) {return(0)}
             floor(value)})})
  
  collection_stats <- reactive({
    
    actual <- actual_drops()
    
    unique_items <- sum(actual > 0)
    
    total_equipment <- nrow(barrows_data)
    
    duplicates <- sum(pmax(actual - 1, 0))
    
    complete_sets <- sum(
      sapply(unique(barrows_data$set),
             
             function(set_name) {
               
               indices <- which(
                 barrows_data$set == set_name)
               
               all(actual[indices] > 0)}))
    
    list(
      unique_items = unique_items,
      total_equipment = total_equipment,
      duplicates = duplicates,
      complete_sets = complete_sets,
      percentage =
        unique_items /
        total_equipment *
        100)})
  
  output$summary_cards <- renderUI({
    
    stats <- collection_stats()
    fluidRow(
      column(3,
             div(class = "summary-card",
                 div(class = "summary-number",
                     paste0(stats$unique_items, " / ", stats$total_equipment)),
                 div(class = "summary-label", "Equipment Uniques"))),
      
      column(3,
             div(class = "summary-card",
                 div(class = "summary-number",
                     paste0(round(stats$percentage, 1), "%")),
                 div(class = "summary-label", "Equipment Complete"))),
      
      column(3,
             div(class = "summary-card",
                 div(class = "summary-number",
                     paste0(stats$complete_sets, " / 6")),
                 div(class = "summary-label", "Complete Sets"))),
      
      column(3,
             div(class = "summary-card",
                 div(class = "summary-number",
                     format(stats$duplicates, big.mark = ",")),
                 div(class = "summary-label", "Duplicate Pieces"))))})
  
  output$overall_progress <- renderUI({
    
    stats <- collection_stats()
    div(class = "collection-progress",
        div(class = "collection-progress-bar",
            style = paste0("width:", min(stats$percentage, 100), "%;"),
            paste0(round(stats$percentage, 1), "%")))})
  
  
  output$collection_log <- renderUI({
    
    actual <- actual_drops()
    sets <- unique(barrows_data$set)
    div(class = "collection-grid",
        lapply(sets,
               
               function(set_name) {
                 indices <- which(
                   barrows_data$set == set_name)
                 counts <- actual[indices]
                 collected <- sum(counts > 0)
                 complete <- (collected == 4)
                 title_class <- if (complete) {
                   "set-title set-complete"
                 } else {"set-title"}
                 
                 div(class = "barrows-set",
                     div(class = title_class,
                         set_name,
                         if (complete) {" ✓"} 
                         else {""}),
                     
                     div(class = "set-progress",
                         paste0(collected," / 4 items collected")),
                     
                     lapply(seq_along(indices),
                            function(j) {
                              index <- indices[j]
                              count <- counts[j]
                              item_class <- if (count > 0) {
                                "barrows-item collected"
                              } else {"barrows-item missing"}
                              
                              div(class = item_class,
                                  span(
                                    class = "item-name",
                                    barrows_data$item[index]),
                                  span(
                                    class = "item-count",
                                    if (count > 0) {
                                      paste0("x", format(count, big.mark = ","))
                                    } else {"Missing"}))}))}))})
  
  output$missing_table <- renderDT({
    actual <- actual_drops()
    kc <- current_kc()
    selected_brothers <- future_brothers()
    n <- length(selected_brothers)
    p <- specific_item_probability(n)
    result <- barrows_data %>%
      mutate(
        `Your Drops` = actual,
        `Expected by Current KC` =kc * p,
        `Chance Received` = 1 - (1 - p) ^ kc,
        `Chance Still Missing` = (1 - p) ^ kc,
        `50% Item KC` =
          ifelse(p > 0, median_chests_for_item(p), NA)) %>%
      filter(`Your Drops` == 0) %>%
      mutate(
        `Expected by Current KC` =
          round(`Expected by Current KC`, 2),
        `Chance Received` =
          percent(`Chance Received`, accuracy = 0.001),
        `Chance Still Missing` =
          percent(`Chance Still Missing`, accuracy = 0.001)) %>%
      select(
        Set = set,
        Item = item,
        `Your Drops`,
        `Expected by Current KC`,
        `Chance Received`,
        `Chance Still Missing`,
        `50% Item KC`)
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        lengthChange = FALSE,
        scrollX = TRUE))})
  
  output$dryness_summary <- renderUI({
    actual <- actual_drops()
    kc <- current_kc()
    selected <- future_brothers()
    n <- length(selected)
    missing <- sum(actual == 0)
    if (missing == 0) {return(
      div(class = "recommendation",
          h4("🎉 Equipment Collection Complete"),
          p("You have collected all 24 Barrows equipment pieces.")))}
    p_item <- specific_item_probability(n)
    expected_per_item <- kc * p_item
    chance_missing <- (1 - p_item) ^ kc
    
    div(class = "analysis-card",
        h3("Your Collection Status"),
        fluidRow(
          column(4,
                 div(class = "analysis-value",
                     missing),
                 div(class = "analysis-subtitle",
                     "equipment pieces remaining")),
          
          column(4,
                 div(class = "analysis-value",
                     sprintf("%.2f", expected_per_item)),
                 div(class = "analysis-subtitle",
                     "expected copies per item")),
          
          column(4,
                 div(class = "analysis-value",
                     percent(chance_missing, accuracy = 0.001)),
                 div(class = "analysis-subtitle",
                     "zero-copy probability for a typical item"))),
        
        div(class = "warning-box",
            paste0(
              "Important: the probabilities above assume your ",
              "future strategy continues to kill ",
              n,
              " brothers per chest.")))})
  
  output$item_probability_summary <- renderUI({
    target <- input$graph_item
    row <- barrows_data %>%
      filter(item == target)
    
    req(nrow(row) == 1)
    selected <- future_brothers()
    p <- target_item_probability(row$brother, selected)
    kc <- current_kc()
    received <- chance_item_after_chests(p, kc)
    expected <- kc * p
    
    div(class = "analysis-card",
        h3(target),
        p(paste0("Eligible brother: ", row$brother)),
        fluidRow(
          column(4,
                 div(class = "analysis-value",
                     ifelse(p > 0,
                            paste0("1 / ", 
                                   format(round(1 / p, 1), big.mark = ",")),
                            "N/A")),
                 
                 div(class = "analysis-subtitle", "approx. per chest")),
          
          column(4,
                 div(class = "analysis-value",
                     percent(received, accuracy = 0.001)),
                 div(class = "analysis-subtitle",
                     "chance received by current KC")),
          
          column(4,
                 div(class = "analysis-value",
                     sprintf("%.2f", expected)),
                 div(class = "analysis-subtitle",
                     "expected copies by current KC"))))})
  
  output$probability_plot <- renderPlotly({
    target <- input$graph_item
    row <- barrows_data %>%
      filter(item == target)
    req(nrow(row) == 1)
    selected <- future_brothers()
    p <- target_item_probability(row$brother, selected)
    kc <- current_kc()
    if (p <= 0) {
      return(
        plot_ly() %>%
          layout(
            title = paste(
              "You cannot receive",
              target,
              "with the selected brothers.")))}
    
    max_kc <- max(500, kc * 1.25, ceiling(log(0.99) / log(1 - p)))
    max_kc <- min(max_kc, 50000)
    x <- seq(0, max_kc, length.out = 600)
    y <- (1 - (1 - p) ^ x)
    current_probability <- (1 - (1 - p) ^ kc)
    graph_data <- data.frame(
      chests = x,
      probability = y)
    plot_ly(
      data = graph_data,
      x = ~chests,
      y = ~probability,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#2C7FB8",
        width = 3),
      hovertemplate = paste0(
        "Chests: %{x:,.0f}",
        "<br>",
        "Chance: %{y:.2%}",
        "<extra></extra>")) %>%
      add_segments(
        x = kc,
        xend = kc,
        y = 0,
        yend = current_probability,
        line = list(
          color = "#D95F02",
          dash = "dash",
          width = 2),
        inherit = FALSE,
        hoverinfo = "skip") %>%
      add_markers(
        x = kc,
        y = current_probability,
        marker = list(
          color = "#D95F02",
          size = 10),
        inherit = FALSE,
        hovertemplate = paste0(
          "Your KC: ",
          format(kc, big.mark = ","),
          "<br>",
          "Chance: ",
          sprintf("%.2f%%", current_probability * 100),
          "<extra></extra>")) %>%
      layout(
        title = paste("Chance of Receiving", target),
        xaxis = list(
          title = "Total Barrows Chests",
          tickformat = ",d"),
        yaxis = list(
          title = "Chance of At Least One",
          tickformat = ".0%",
          range = c(0, 1)),
        plot_bgcolor = "#212529",
        paper_bgcolor = "#212529",
        font = list(color = "#F8F9FA"),
        showlegend = FALSE)})
  
  output$completion_summary <- renderUI({
    actual <- actual_drops()
    missing_indices <- which(actual == 0)
    missing <- length(missing_indices)
    selected <- future_brothers()
    if (missing == 0) {
      return(
        div(class = "recommendation",
            h3("🎉 Equipment Collection Complete"),
            p("All 24 Barrows equipment pieces have been collected.")))}
    
    missing_brothers <- unique(barrows_data$brother[missing_indices])
    unobtainable <- setdiff(missing_brothers, selected)
    if (length(unobtainable) > 0) {
      return(
        div(class = "warning-box",
            h4("Collection cannot currently be completed"),
            p(paste0("Your future strategy does not kill: ",
                     paste(unobtainable, collapse = ", "),
                     ".")),
            p("Items belonging to those brothers cannot be ",
              "received until those brothers are added to your ",
              "future strategy.")))}
    
    kc25 <- collection_completion_probability(missing, 25, selected)
    kc50 <- collection_completion_probability( missing, 50, selected)
    kc100 <- collection_completion_probability(missing, 100, selected)
    fifty <- completion_50_chests(missing,selected)
    
    div(class = "analysis-card",
        h3("Collection Completion Odds"),
        p(paste0("You currently have ",
                 missing,
                 " equipment pieces remaining.")),
        fluidRow(
          column(3,
                 div(class = "summary-card",
                     div(class = "summary-number",
                         percent(kc25, accuracy = 0.001)),
                     div(class = "summary-label", "Next 25 chests"))),
          
          column(3,
                 div(class = "summary-card",
                     div(class = "summary-number",
                         percent(kc50, accuracy = 0.001)),
                     div(class = "summary-label", "Next 50 chests"))),
          
          column(3,
                 div(class = "summary-card",
                     div(class = "summary-number",
                         percent(kc100, accuracy = 0.001)),
                     div(class = "summary-label", "Next 100 chests"))),
          
          column(3,
                 div(class = "summary-card",
                     div(class = "summary-number",
                         ifelse(
                           is.na(fifty),
                           "5,000+",
                           format(fifty, big.mark = ","))),
                     div(class = "summary-label", "50% completion KC")))))})
  
  output$completion_plot <- renderPlotly({
    actual <- actual_drops()
    missing <- sum(actual == 0)
    selected <- future_brothers()
    if (missing == 0) {return(NULL)}
    
    missing_indices <- which(actual == 0)
    missing_brothers <- unique(barrows_data$brother[missing_indices])
    unobtainable <- setdiff(missing_brothers, selected)
    if (length(unobtainable) > 0) {return(NULL)}
    
    curve <- collection_completion_curve(
      missing_items = missing,
      killed_brothers = selected,
      max_chests = 1000,
      step = 10)
    
    plot_ly(
      data = curve,
      x = ~chests,
      y = ~probability,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#28a745",
        width = 3),
      hovertemplate = paste0(
        "Additional chests: %{x:,.0f}",
        "<br>",
        "Chance of completion: %{y:.2%}",
        "<extra></extra>")) %>%
      layout(
        title = "Probability of Completing the Equipment Collection",
        xaxis = list(
          title = "Additional Barrows Chests",
          tickformat = ",d"),
        yaxis = list(
          title = "Chance of Completion",
          tickformat = ".0%",
          range = c(0, 1)),
        plot_bgcolor = "#212529",
        paper_bgcolor = "#212529",
        font = list(color = "#F8F9FA"),
        showlegend = FALSE)})}

# RUN IT
########

shinyApp(
  ui = ui,
  server = server)
