library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(shinyBS)

team_v1 <- readRDS("team_type_v1.RDS")
team_v2 <- readRDS("team_type_v2.RDS")
gender_v1 <- readRDS("gender_trend_v1.RDS")
journaldata <- readRDS("journal_info.RDS")
active_by_year <- readRDS("active_by_year.RDS")

if (is.null(journaldata) && !is.null(team_v1)) {
  journaldata <- team_v1 %>%
    group_by(journal) %>%
    summarise(count = sum(count, na.rm = TRUE),
              first_year_included = min(year, na.rm = TRUE),
              last_year_included = max(year, na.rm = TRUE),
              .groups = "drop")
}

base_for_ui <- if (!is.null(team_v1)) team_v1 else if (!is.null(team_v2)) team_v2 else tibble::tibble(year = integer(0), journal = character(0))

coerce_year <- function(df) {
  if (is.null(df)) return(NULL)
  if ("year" %in% names(df)) df <- df %>% mutate(year = suppressWarnings(as.integer(as.character(year))))
  df
}
team_v1 <- coerce_year(team_v1); team_v2 <- coerce_year(team_v2)
gender_v1 <- coerce_year(gender_v1)
journaldata <- coerce_year(journaldata); base_for_ui <- coerce_year(base_for_ui)
active_by_year <- coerce_year(active_by_year)

order_v1 <- c("Solo Woman", "Female Team", "Mixed Team", "Male Team", "Solo Man")
order_v2 <- c("Solo Woman", "Female Team", "Female-led Team", "Male-led Team", "Male Team", "Solo Man")

team_colors <- c(
  "Solo Woman" = "#d73027","Female Team" = "#fc8d59","Female-led Team" = "#fee090",
  "Mixed Team" = "#67a9cf","Male-led Team" = "#e6f5d0","Male Team" = "#a1d76a","Solo Man" = "#4d9221"
)
sex_colors <- c("Male" = "#a1d76a", "Female" = "#fc8d59")

ui <- fluidPage(
  div(
    style = "display: flex; align-items: center; gap: 10px; margin-bottom: 4px;",
    h2("Gender of Authors in Political Science Journals", style = "margin: 0;"),
    actionButton("help_btn", label = "How to use", icon = icon("question-circle"),
                 style = "font-size: 14px; color: #337ab7; cursor: pointer;")
  ),
  tags$p(
    class = "text-muted", style = "margin: 0 0 15px 0;",
    "Companion to “Mechanical Change or Systematic Disparity? Gendered Authorship in Political Science Publishing Across Five Decades” by Christina Gahn and Michael Imre, ",
    tags$em("Perspectives on Politics"), " (forthcoming)"
  ),
  sidebarLayout(
    sidebarPanel(
      div(
        id = "mode_label_container",
        style = "display: flex; align-items: center; margin-bottom: 5px; gap: 5px;",
        tags$b("Display mode:"),
        shinyBS::bsButton(
          inputId = "mode_info",
          label = "",
          icon = icon("info-circle"),
          style = "link",
          size = "extra-small"
        )
      ),
      radioButtons(
        inputId = "mode",
        label = NULL,
        choices = c(
          "Team type (5 groups)" = "mode1",
          "Team type (6 groups)" = "mode2",
          "Share of all authors" = "mode3"
        ),
        selected = "mode1"
      ),
      
      sliderInput(
        inputId = "year_range",
        label = "Select Year Range:",
        min = ifelse(nrow(base_for_ui)>0, min(base_for_ui$year, na.rm = TRUE), 1970),
        max = ifelse(nrow(base_for_ui)>0, max(base_for_ui$year, na.rm = TRUE), 2020),
        value = c(ifelse(nrow(base_for_ui)>0, min(base_for_ui$year, na.rm = TRUE), 1970),
                  ifelse(nrow(base_for_ui)>0, max(base_for_ui$year, na.rm = TRUE), 2020)),
        step = 1,
        sep = ""
      ),
      
      
      div(
        style = "display: flex; align-items: center; margin-bottom: 5px; gap: 5px;",
        checkboxInput("add_estimate", "Add Gender Estimate (Share Men)", value = FALSE),
        shinyBS::bsButton(
          inputId = "estimate_info",
          label = "",
          icon = icon("info-circle"),
          style = "link",
          size = "extra-small"
        )
      ),


      tags$b("Option 1: Pick journals directly:"),
      selectizeInput(
        inputId = "selected_journals",
        label = NULL,
        choices = c("All Journals", sort(unique(base_for_ui$journal))),
        selected = "All Journals",
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      div(
        style = "background-color: #d6e4f0; border-radius: 6px; padding: 10px 10px 10px 15px; margin-top: 6px; margin-left: -15px; margin-right: -15px;",
        tags$b("Option 2: Filter by criteria, then click \u2018All in Subset\u2019:"),
        selectizeInput(
          inputId = "selected_category",
          label = "Select Category/Categories:",
          choices = sort(unique(journaldata$category)),
          selected = NULL,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        
        div(
          style = "display: flex; align-items: center; margin-bottom: -15px; gap: 5px;",
          tags$label("Earliest Publication Year of Journal:"),
          shinyBS::bsButton(
            inputId = "year_filter_info",
            label = "",
            icon = icon("info-circle"),
            style = "link",
            size = "extra-small"
          )
        ),
        sliderInput(
          inputId = "first_year_filter",
          label = NULL,
          min = ifelse(!is.null(journaldata) && nrow(journaldata)>0, min(journaldata$first_year_included, na.rm = TRUE), ifelse(nrow(base_for_ui)>0,min(base_for_ui$year,na.rm=TRUE),1970)),
          max = ifelse(!is.null(journaldata) && nrow(journaldata)>0, max(journaldata$first_year_included, na.rm = TRUE), ifelse(nrow(base_for_ui)>0,max(base_for_ui$year,na.rm=TRUE),2020)),
          value = c(ifelse(!is.null(journaldata) && nrow(journaldata)>0, min(journaldata$first_year_included, na.rm = TRUE), ifelse(nrow(base_for_ui)>0,min(base_for_ui$year,na.rm=TRUE),1970)),
                    ifelse(!is.null(journaldata) && nrow(journaldata)>0, max(journaldata$first_year_included, na.rm = TRUE), ifelse(nrow(base_for_ui)>0,max(base_for_ui$year,na.rm=TRUE),2020))),
          step = 1,
          sep = ""
        ),
        
        div(
          style = "display: flex; align-items: center; margin-bottom: -15px; gap: 5px;",
          tags$label("Number of Times in Q1:"),
          shinyBS::bsButton(
            inputId = "count_filter_info",
            label = "",
            icon = icon("info-circle"),
            style = "link",
            size = "extra-small"
          )
        ),
        sliderInput(
          inputId = "count_filter",
          label = NULL,
          min = ifelse(!is.null(journaldata) && nrow(journaldata) > 0, min(journaldata$count, na.rm = TRUE), 0),
          max = ifelse(!is.null(journaldata) && nrow(journaldata) > 0, max(journaldata$count, na.rm = TRUE), 1),
          value = c(ifelse(!is.null(journaldata) && nrow(journaldata) > 0, min(journaldata$count, na.rm = TRUE), 0), ifelse(!is.null(journaldata) && nrow(journaldata) > 0, max(journaldata$count, na.rm = TRUE), 1)),
          step = 1
        ),
        
        div(
          style = "margin-top: 10px;",
          actionButton("select_subset", "All in Subset")
        )
      ),
      div(
        style = "margin-top: 10px;",
        actionButton("reset_filters", "Reset")
      ),
      
      
      
      br(), br(),
      textOutput("num_selected"),
      br(),
      uiOutput("data_availability_note")
    ),
    
    mainPanel(
      plotOutput("genderPlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  programmatic_journal_update <- reactiveVal(FALSE)
  selection_mode <- reactiveVal("all")
  subset_description <- reactiveVal("")
  
  help_modal <- modalDialog(
    title = "How to Use This App",
    tags$p("This dashboard accompanies the article ",
           tags$b("“Mechanical Change or Systematic Disparity? Gendered Authorship in Political Science Publishing Across Five Decades”"),
           " by Christina Gahn and Michael Imre, forthcoming in ", tags$em("Perspectives on Politics", .noWS = "after"),
           ", and visualises the gender composition of authors in political science journals over time."),
    tags$hr(),
    tags$h4("Display Options"),
    tags$p("Use the ", tags$b("Display mode"), " selector to switch between visualisations."),
    tags$p("Use the ", tags$b("Year Range"), " slider to focus on a specific time period."),
    tags$p("Add an estimate of the ", tags$b("Share of Men"), " in the discipline over time to the figure by checking the mark."),
    tags$hr(),
    tags$h4("Selecting Journals"),
    tags$p("There are two ways to select which journals to display:"),
    tags$h5("Option 1: Manual Selection"),
    tags$p("Use the ", tags$b("Select Journal(s)"), " dropdown to pick individual journals by name.
            You can select as many as you like. Choose ", tags$em("All Journals"), " to show everything."),
    tags$h5("Option 2: Filter by Parameters"),
    tags$p("Use the filters below the journal selector to narrow down journals by criteria:"),
    tags$ul(
      tags$li(tags$b("Select Category/Categories:"), "limit to one or more journal categories"),
      tags$li(tags$b("Earliest Publication Year:"), "only include journals that started publishing within a given range"),
      tags$li(tags$b("Number of Times in Q1:"), "only include journals that appeared in the Q1 JCR ranking a certain number of times")
    ),
    tags$p("These filters control which journals appear in the ", tags$em("Select Journal(s)"), " dropdown.
            Once you have set your filters, click ", tags$b("All in Subset"), " to select all matching journals."),
    tags$hr(),
    tags$p("Click ", tags$b("Reset"), " to clear all filters and/or drop all journals from the selection and return to showing all journals."),
    footer = modalButton("Got it"),
    size = "m",
    easyClose = TRUE
  )
  
  
  # Re-open help modal when button is clicked
  observeEvent(input$help_btn, {
    showModal(help_modal)
  })
  
  # Display Mode
  observe({
    shinyBS::addTooltip(
      session = session,
      id = "mode_info",
      title = "<b>Team type (5 groups):</b> default grouping used in the accompanying article.<br><br><b>Team type (6 groups):</b> splits mixed teams into female-led and male-led (see Appendix A4 of the accompanying article).<br><br><b>Share of all authors:</b> counts all authors in selected journals per year (see Appendix A5 of the accompanying article).",
      placement = "right",
      trigger = "click",
      options = list(html = TRUE)
    )
  })
  
  # Earliest Publication Year of Journal
  observe({
    shinyBS::addTooltip(
      session = session,
      id = "year_filter_info",
      title = "Only include journals that started publishing within these years. Journals that started publishing before 1975 are set to 1975, the start of data collection for this project.",
      placement = "right",
      trigger = "click"
    )
  })
  
  # Number of Times in Q1
  observe({
    shinyBS::addTooltip(
      session = session,
      id = "count_filter_info",
      title = "Only include journals that were listed in Q1 of the Political Science JCR ranking this many times since 1997. While the JCR has existed much longer, they only started using Quartiles in 1997",
      placement = "right",
      trigger = "click"
    )
  })
  
  # Add Gender Estimate
  observe({
    shinyBS::addTooltip(
      session = session,
      id = "estimate_info",
      title = "Add the percentage of men among all political scientists over time to the figure. This is not based on any hard data but is just an estimate based on our data (see the accompanying article for more details)",
      placement = "right",
      trigger = "click"
    )
  })
  
  # Reactive for filtering the active_by_year data by year range
  estimate_df <- reactive({
    if (is.null(active_by_year) || !("share_men" %in% names(active_by_year))) return(NULL)
    
    yrmin <- input$year_range[1]; yrmax <- input$year_range[2]
    
    active_by_year %>%
      filter("year" %in% names(.) & year >= yrmin, year <= yrmax) %>%
      select(year, share_men) %>%
      mutate(share_men = as.numeric(share_men))
  })

  filtered_journals <- reactive({
    if (is.null(journaldata)) return(sort(unique(base_for_ui$journal)))
    jd <- journaldata %>%
      filter(
        first_year_included >= input$first_year_filter[1],
        first_year_included <= input$first_year_filter[2],
        count >= input$count_filter[1],
        count <= input$count_filter[2]
      )
    
    sel_cats <- input$selected_category
    if (!is.null(sel_cats) && length(sel_cats) > 0) {
      jd <- jd %>% filter(category %in% sel_cats)
    }
    
    jd %>% arrange(journal) %>% pull(journal)
  })
  
  session$onFlushed(function() {
    allj <- if (!is.null(base_for_ui) && nrow(base_for_ui) > 0) sort(unique(base_for_ui$journal)) else character(0)
    updateSelectInput(session, "selected_journals", choices = c("All Journals", allj), selected = "All Journals")
  }, once = TRUE)
  
  observe({
    new_choices <- filtered_journals()
    if (length(new_choices) == 0) {
      programmatic_journal_update(TRUE)
      updateSelectInput(session, "selected_journals", choices = c("All Journals"), selected = "All Journals")
      return()
    }
    current_sel <- isolate(input$selected_journals)
    
    if (!is.null(current_sel) && "All Journals" %in% current_sel) {
      sel_to_set <- "All Journals"
    } else if (is.null(current_sel) || length(current_sel) == 0) {
      sel_to_set <- "All Journals"
    } else {
      sel_int <- intersect(current_sel, new_choices)
      sel_to_set <- if (length(sel_int) == 0) "All Journals" else sel_int
    }
    
    programmatic_journal_update(TRUE)
    updateSelectInput(session,
                      "selected_journals",
                      choices = c("All Journals", new_choices),
                      selected = sel_to_set)
  })
  
  # Reset subset mode when any filter changes
  observeEvent({
    list(input$selected_category, input$first_year_filter, input$count_filter)
  }, {
    if (selection_mode() == "subset") {
      selection_mode("manual")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$selected_journals, {
    if (is.null(input$selected_journals)) return()
    
    is_prog <- programmatic_journal_update()
    programmatic_journal_update(FALSE)
    
    if (!is_prog) {
      sel <- input$selected_journals
      if (length(sel) == 1 && sel == "All Journals") {
        selection_mode("all")
      } else {
        selection_mode("manual")
      }
    } else {
      # For programmatic changes, correct mode if result became "All Journals" unexpectedly
      sel <- input$selected_journals
      if (!is.null(sel) && length(sel) == 1 && sel == "All Journals" && selection_mode() == "manual") {
        selection_mode("all")
      }
    }
    
    if ("All Journals" %in% input$selected_journals && length(input$selected_journals) > 1) {
      programmatic_journal_update(TRUE)
      updateSelectInput(session, "selected_journals", selected = setdiff(input$selected_journals, "All Journals"))
    }
    if (length(input$selected_journals) == 0) {
      programmatic_journal_update(TRUE)
      updateSelectInput(session, "selected_journals", selected = "All Journals")
    }
  })
  
  observeEvent(input$select_subset, {
    subset_journals <- filtered_journals()
    if (length(subset_journals) > 0) {
      # Build description from current filter state
      sel_cats <- input$selected_category
      if (!is.null(sel_cats) && length(sel_cats) > 0) {
        if (length(sel_cats) == 1) {
          journal_noun <- paste0("all ", sel_cats, " journals")
        } else if (length(sel_cats) == 2) {
          journal_noun <- paste0("all ", sel_cats[1], " and ", sel_cats[2], " journals")
        } else {
          cat_text <- paste(sel_cats[-length(sel_cats)], collapse = ", ")
          journal_noun <- paste0("all ", cat_text, ", and ", sel_cats[length(sel_cats)], " journals")
        }
      } else {
        journal_noun <- "all journals"
      }
      
      conditions <- c()
      
      yr_min_default <- if (!is.null(journaldata) && nrow(journaldata) > 0) min(journaldata$first_year_included, na.rm = TRUE) else NA
      yr_max_default <- if (!is.null(journaldata) && nrow(journaldata) > 0) max(journaldata$first_year_included, na.rm = TRUE) else NA
      yr_min_cur <- input$first_year_filter[1]
      yr_max_cur <- input$first_year_filter[2]
      
      yr_changed_min <- !is.na(yr_min_default) && yr_min_cur != yr_min_default
      yr_changed_max <- !is.na(yr_max_default) && yr_max_cur != yr_max_default
      
      if (yr_changed_min && yr_changed_max) {
        conditions <- c(conditions, paste0("first published between ", yr_min_cur, " and ", yr_max_cur))
      } else if (yr_changed_min) {
        conditions <- c(conditions, paste0("first published no earlier than ", yr_min_cur))
      } else if (yr_changed_max) {
        conditions <- c(conditions, paste0("first published no later than ", yr_max_cur))
      }
      
      ct_min_default <- if (!is.null(journaldata) && nrow(journaldata) > 0) min(journaldata$count, na.rm = TRUE) else NA
      ct_max_default <- if (!is.null(journaldata) && nrow(journaldata) > 0) max(journaldata$count, na.rm = TRUE) else NA
      ct_min_cur <- input$count_filter[1]
      ct_max_cur <- input$count_filter[2]
      
      ct_changed_min <- !is.na(ct_min_default) && ct_min_cur != ct_min_default
      ct_changed_max <- !is.na(ct_max_default) && ct_max_cur != ct_max_default
      
      if (ct_changed_min && ct_changed_max) {
        conditions <- c(conditions, paste0("in Q1 between ", ct_min_cur, " and ", ct_max_cur, " times"))
      } else if (ct_changed_min) {
        conditions <- c(conditions, paste0("in Q1 at least ", ct_min_cur, " times"))
      } else if (ct_changed_max) {
        conditions <- c(conditions, paste0("in Q1 no more than ", ct_max_cur, " times"))
      }
      
      if (length(conditions) > 0) {
        cond_text <- paste(conditions, collapse = " and were ")
        desc <- paste0(journal_noun, " which were ", cond_text)
      } else {
        desc <- journal_noun
      }
      
      subset_description(desc)
      selection_mode("subset")
      programmatic_journal_update(TRUE)
      updateSelectInput(session, "selected_journals", selected = subset_journals)
    } else {
      showNotification("No journals match your filters.", type = "warning")
    }
  })
  
  observeEvent(input$reset_filters, {
    selection_mode("all")
    programmatic_journal_update(TRUE)
    updateSelectInput(session, "selected_category", selected = character(0))
    updateSelectInput(session, "selected_journals", selected = "All Journals")
    
    updateSliderInput(session, "year_range",
                      value = c(ifelse(nrow(base_for_ui)>0,min(base_for_ui$year,na.rm=TRUE),1970),
                                ifelse(nrow(base_for_ui)>0,max(base_for_ui$year,na.rm=TRUE),2020)))
    
    updateSliderInput(session, "first_year_filter",
                      value = c(ifelse(!is.null(journaldata) && nrow(journaldata)>0, min(journaldata$first_year_included, na.rm = TRUE), ifelse(nrow(base_for_ui)>0,min(base_for_ui$year,na.rm=TRUE),1970)),
                                ifelse(!is.null(journaldata) && nrow(journaldata)>0, max(journaldata$first_year_included, na.rm = TRUE), ifelse(nrow(base_for_ui)>0,max(base_for_ui$year,na.rm=TRUE),2020))))
    
    updateSliderInput(session, "count_filter",
                      value = c(ifelse(!is.null(journaldata) && nrow(journaldata) > 0, min(journaldata$count, na.rm = TRUE), 0), ifelse(!is.null(journaldata) && nrow(journaldata) > 0, max(journaldata$count, na.rm = TRUE), 1)))
    
    # Display Mode is intentionally NOT reset.
  })
  
  output$num_selected <- renderText({
    mode <- selection_mode()
    sel <- input$selected_journals
    
    if (mode == "subset") {
      n <- length(resolve_selected_journals())
      paste0("Currently showing: ", subset_description(), " (", n, " journals)")
    } else if (mode == "all" || (length(sel) == 1 && "All Journals" %in% sel)) {
      total <- if (!is.null(base_for_ui) && nrow(base_for_ui) > 0) length(unique(base_for_ui$journal)) else 0
      paste0("Currently showing: all available journals (", total, " journals)")
    } else {
      n <- length(sel)
      paste0("Currently showing: manual selection of journals (", n, " journals)")
    }
  })
  
  resolve_selected_journals <- reactive({
    s <- input$selected_journals
    all_j <- if (!is.null(base_for_ui) && nrow(base_for_ui)>0) sort(unique(base_for_ui$journal)) else character(0)
    if (is.null(s) || length(s) == 0) return(all_j)
    if ("All Journals" %in% s) return(all_j)
    intersect(s, all_j)
  })
  
  build_plot_df <- reactive({
    mode <- input$mode
    sel_journals <- resolve_selected_journals()
    yrmin <- input$year_range[1]; yrmax <- input$year_range[2]
    
    if (mode == "mode1") {
      if (is.null(team_v1)) return(NULL)
      df <- team_v1
      if ("journal" %in% names(df)) df <- df %>% filter(journal %in% sel_journals)
      if ("year" %in% names(df)) df <- df %>% filter(!is.na(year))
      if ("year" %in% names(df)) df <- df %>% filter(year >= yrmin, year <= yrmax)
      df %>% group_by(year, team_type) %>% summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
    } else if (mode == "mode2") {
      if (is.null(team_v2)) return(NULL)
      df <- team_v2
      if ("journal" %in% names(df)) df <- df %>% filter(journal %in% sel_journals)
      if ("year" %in% names(df)) df <- df %>% filter(!is.na(year))
      if ("year" %in% names(df)) df <- df %>% filter(year >= yrmin, year <= yrmax)
      df %>% group_by(year, team_type) %>% summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
    } else if (mode == "mode3") {
      if (is.null(gender_v1)) return(NULL)
      df <- gender_v1
      if ("journal" %in% names(df)) df <- df %>% filter(journal %in% sel_journals)
      if ("year" %in% names(df)) df <- df %>% filter(!is.na(year))
      if ("year" %in% names(df)) df <- df %>% filter(year >= yrmin, year <= yrmax)
      if (all(c("male_authors","female_authors") %in% names(df))) {
        # pool all authors in the selected journals (as in the paper) rather than averaging journal shares
        df %>% group_by(year) %>%
          summarise(share_of_male_authors = sum(male_authors) / sum(male_authors + female_authors),
                    share_of_female_authors = sum(female_authors) / sum(male_authors + female_authors),
                    .groups = "drop")
      } else df
    } else NULL
  })
  
  output$genderPlot <- renderPlot({
    df <- build_plot_df()
    if (is.null(df) || nrow(df) == 0) {
      plot.new(); text(0.5, 0.55, "No data available for selected filters", cex = 1.1); return()
    }
    mode <- input$mode
    
    x_limits <- c(input$year_range[1], input$year_range[2])
    n_yrs <- diff(x_limits) + 1
    interval <- if (n_yrs <= 10) 1 else 5
    if (n_yrs <= 1) {
      breaks <- x_limits[1]
      x_limits <- c(x_limits[1] - 0.5, x_limits[2] + 0.5)
    } else {
      breaks <- seq(x_limits[1], x_limits[2], by = interval)
    }
    
    if (mode == "mode1") {
      
      # slight hack so that the legend is displayed the way it is displayed
      BLANK_HACK <- " "
      order_v1_fixed <- c("Solo Woman", "Female Team", "Mixed Team", BLANK_HACK, "Male Team", "Solo Man")
      df <- bind_rows(df, tibble(year = max(df$year, na.rm=T), team_type = BLANK_HACK, count = 0))
      df$team_type <- factor(df$team_type, levels = order_v1_fixed)
      
      team_colors_fixed <- c(
        team_colors[order_v1],
        BLANK_HACK = "transparent"
      )
      
      color_vector_for_scale <- unname(team_colors_fixed[order_v1_fixed])
      
      p <- ggplot(df) +
        geom_area(aes(x = year, y = count, fill = team_type), position = "fill", alpha = 0.85) +
        scale_fill_manual(values = color_vector_for_scale, drop = FALSE) +
        scale_x_continuous(limits = x_limits, breaks = breaks) +
        scale_y_continuous(labels = percent_format()) +
        labs(title = "Proportion of Papers by Team Type",
             x = "Year", y = "Proportion of Papers", fill = "Team Type") +
        theme_minimal(base_size = 14) + theme(legend.position = "bottom") +
        guides(fill = guide_legend(
          nrow = 3,
          byrow = TRUE,
          override.aes = list(
            fill = color_vector_for_scale,
            alpha = c(rep(1, 3), 0, rep(1, 2)),
            size = c(rep(1, 3), 0.01, rep(1, 2))
          )
        ))
      
    } else if (mode == "mode2") {
      
      df$team_type <- factor(df$team_type, levels = order_v2)
      
      p <- ggplot(df) +
        geom_area(aes(x = year, y = count, fill = team_type), position = "fill", alpha = 0.85) +
        scale_fill_manual(values = team_colors) +
        scale_x_continuous(limits = x_limits, breaks = breaks) +
        scale_y_continuous(labels = percent_format()) +
        labs(title = "Proportion of Papers by Team Type",
             x = "Year", y = "Proportion of Papers", fill = "Team Type") +
        theme_minimal(base_size = 14) + theme(legend.position = "bottom") +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE))
      
    } else if (mode == "mode3") {
      # Long format without tidyr: in the browser version, tidyr pulls in ~15 MB of packages
      plotdf <- bind_rows(
        df %>% transmute(year, sex = "Male", share = share_of_male_authors),
        df %>% transmute(year, sex = "Female", share = share_of_female_authors)
      )
      plotdf$sex <- factor(plotdf$sex, levels = c("Female", "Male"))
      
      p <- ggplot(plotdf) +
        geom_area(aes(x = year, y = share, fill = sex), position = "stack", alpha = 0.85) +
        scale_fill_manual(values = sex_colors) +
        scale_x_continuous(limits = x_limits, breaks = breaks) +
        scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
        labs(title = "Share of Male vs Female Authors", x = "Year", y = "Share of Authors", fill = "") +
        theme_minimal(base_size = 14) + theme(legend.position = "bottom") +
        guides(fill = guide_legend(nrow = 2))

    } else {
      plot.new(); text(0.5, 0.55, "No data available for selected filters", cex = 1.1); return()
    }

    # Add Gender Estimate Line (Overlay)
    if (input$add_estimate) {
      estimate_data <- estimate_df()
      
      if (!is.null(estimate_data) && nrow(estimate_data) > 0) {
        p <- p +
          geom_line(data = estimate_data, aes(x = year, y = share_men, group = 1),
                    color = "black", linetype = "solid", linewidth = 1) +
          geom_point(data = estimate_data, aes(x = year, y = share_men, group = 1),
                     color = "black", size = 2)
      }
    }
    
    p
  })
}

shinyApp(ui = ui, server = server)