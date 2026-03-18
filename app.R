library(shiny)
library(shinydashboard)
library(personalized)
library(xgboost)
library(ggplot2)

# -----------------------------
# 1. Load saved models and data
# -----------------------------

model_bundle <- readRDS("personalised_model.RDS")

patient_template <- model_bundle$collin_varz$result$df

models <- list(
  model_bundle$subgrp.model1,
  model_bundle$subgrp.model2,
  model_bundle$subgrp.model3,
  model_bundle$subgrp.model4,
  model_bundle$subgrp.model5,
  model_bundle$subgrp.model6
)

validation_objects <- list(
  model_bundle$validation1,
  model_bundle$validation2,
  model_bundle$validation3,
  model_bundle$validation4,
  model_bundle$validation5,
  model_bundle$validation6
)

outcome_names <- c(
  "NDI @ 1 year",
  "NDI @ 2 years",
  "Arm pain @ 1 year",
  "Arm pain @ 2 years",
  "Neck pain @ 1 year",
  "Neck pain @ 2 years"
)

# -----------------------------------------
# 2. Describe each sidebar input one by one
# -----------------------------------------

friendly_labels <- c(
  ndi_w0 = "Baseline NDI (0-100)",
  vas_neck_w0 = "Baseline neck pain VAS (0-100)",
  vas_arm_w0 = "Baseline arm pain VAS (0-100)",
  hnp_number_c7t1 = "HNP at C7-T1",
  work = "Physical workload",
  hnp_number_c45 = "HNP at C4-C5",
  educ = "Education level",
  hnp_loc_central = "Central HNP location",
  married = "Married",
  opioid = "Opioid use",
  hnp_loc_form = "Foraminal HNP location",
  armpain_freq = "Arm pain frequency",
  sick_leave = "Sick leave",
  neckpain_freq = "Neck pain frequency",
  alcohol = "Alcohol use",
  daytime = "Employment status",
  hnp_number_c67 = "HNP at C6-C7",
  nsaid = "NSAID use",
  loss_of_strength = "Loss of strength",
  tingl_freq = "Tingling frequency",
  hnp_loc_ml = "Mediolateral HNP location",
  root_compress = "Root compression",
  hnp_size = "HNP size",
  smoking = "Smoking",
  children = "Children",
  myel_compress_lvl = "Myelopathy compression level"
)

input_info <- vector("list", length = ncol(patient_template))
names(input_info) <- names(patient_template)

for (var_name in names(patient_template)) {
  values <- patient_template[[var_name]]
  label <- friendly_labels[[var_name]]

  if (is.null(label)) {
    label <- gsub(" +", " ", trimws(gsub("_", " ", var_name)))
  }

  if (is.factor(values)) {
    most_common <- names(sort(table(values), decreasing = TRUE))[1]

    input_info[[var_name]] <- list(
      label = label,
      is_factor = TRUE,
      levels = levels(values),
      default = most_common
    )
  } else {
    input_info[[var_name]] <- list(
      label = label,
      is_factor = FALSE,
      min = floor(min(values, na.rm = TRUE)),
      max = ceiling(max(values, na.rm = TRUE)),
      default = round(stats::median(values, na.rm = TRUE), 1)
    )
  }
}

# --------------------------------------
# 3. Build validation plots before the UI
# --------------------------------------

validation_plots <- vector("list", length(validation_objects))

for (i in seq_along(validation_objects)) {
  plot_data <- plot(validation_objects[[i]], type = "interaction")$data

  plot_data$Recommended <- factor(
    plot_data$Recommended,
    levels = c(0, 1),
    labels = c("Conservative", "Surgery")
  )

  plot_data$Received <- factor(
    plot_data$Received,
    levels = c(0, 1),
    labels = c("Conservative", "Surgery")
  )

  validation_plots[[i]] <- ggplot(
    plot_data,
    aes(x = Recommended, y = Value, color = Received, group = Received)
  ) +
    geom_errorbar(
      aes(ymin = Value - 1.96 * SE, ymax = Value + 1.96 * SE),
      width = 0.08,
      linewidth = 0.6,
      alpha = 0.7
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.8) +
    scale_color_manual(values = c("#1f6f78", "#c96a2a")) +
    labs(
      x = "Recommended treatment",
      y = outcome_names[i],
      color = "Actual treatment received"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
}

# ------------------------
# 4. Build the dashboard UI
# ------------------------

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Casino Dashboard", titleWidth = 320),
  dashboardSidebar(
    width = 380,
    sidebarMenu(
      id = "tabs",
      menuItem("Recommendations", tabName = "recommendations", icon = icon("user-md")),
      menuItem("Validation", tabName = "validation", icon = icon("chart-line"))
    ),
    tags$div(
      class = "sidebar-scroll",
      tags$h4("Patient profile"),
      tags$p("Enter the baseline variables for one patient."),
      tags$p(
        class = "sidebar-note",
        "Defaults use the training-set median for numeric variables and the most common category for factor variables."
      ),
      tags$div(
        class = "input-group-block",
        tags$h5("Symptoms"),
        numericInput(
          "ndi_w0",
          input_info$ndi_w0$label,
          value = input_info$ndi_w0$default,
          min = input_info$ndi_w0$min,
          max = input_info$ndi_w0$max,
          step = 0.1
        ),
        numericInput(
          "vas_neck_w0",
          input_info$vas_neck_w0$label,
          value = input_info$vas_neck_w0$default,
          min = input_info$vas_neck_w0$min,
          max = input_info$vas_neck_w0$max,
          step = 0.1
        ),
        numericInput(
          "vas_arm_w0",
          input_info$vas_arm_w0$label,
          value = input_info$vas_arm_w0$default,
          min = input_info$vas_arm_w0$min,
          max = input_info$vas_arm_w0$max,
          step = 0.1
        ),
        selectInput(
          "armpain_freq",
          input_info$armpain_freq$label,
          choices = input_info$armpain_freq$levels,
          selected = input_info$armpain_freq$default
        ),
        selectInput(
          "neckpain_freq",
          input_info$neckpain_freq$label,
          choices = input_info$neckpain_freq$levels,
          selected = input_info$neckpain_freq$default
        ),
        selectInput(
          "loss_of_strength",
          input_info$loss_of_strength$label,
          choices = input_info$loss_of_strength$levels,
          selected = input_info$loss_of_strength$default
        ),
        selectInput(
          "tingl_freq",
          input_info$tingl_freq$label,
          choices = input_info$tingl_freq$levels,
          selected = input_info$tingl_freq$default
        )
      ),
      tags$div(
        class = "input-group-block",
        tags$h5("Imaging and Clinical"),
        selectInput(
          "hnp_number_c45",
          input_info$hnp_number_c45$label,
          choices = input_info$hnp_number_c45$levels,
          selected = input_info$hnp_number_c45$default
        ),
        selectInput(
          "hnp_number_c67",
          input_info$hnp_number_c67$label,
          choices = input_info$hnp_number_c67$levels,
          selected = input_info$hnp_number_c67$default
        ),
        selectInput(
          "hnp_number_c7t1",
          input_info$hnp_number_c7t1$label,
          choices = input_info$hnp_number_c7t1$levels,
          selected = input_info$hnp_number_c7t1$default
        ),
        selectInput(
          "hnp_loc_central",
          input_info$hnp_loc_central$label,
          choices = input_info$hnp_loc_central$levels,
          selected = input_info$hnp_loc_central$default
        ),
        selectInput(
          "hnp_loc_form",
          input_info$hnp_loc_form$label,
          choices = input_info$hnp_loc_form$levels,
          selected = input_info$hnp_loc_form$default
        ),
        selectInput(
          "hnp_loc_ml",
          input_info$hnp_loc_ml$label,
          choices = input_info$hnp_loc_ml$levels,
          selected = input_info$hnp_loc_ml$default
        ),
        selectInput(
          "root_compress",
          input_info$root_compress$label,
          choices = input_info$root_compress$levels,
          selected = input_info$root_compress$default
        ),
        selectInput(
          "hnp_size",
          input_info$hnp_size$label,
          choices = input_info$hnp_size$levels,
          selected = input_info$hnp_size$default
        ),
        selectInput(
          "myel_compress_lvl",
          input_info$myel_compress_lvl$label,
          choices = input_info$myel_compress_lvl$levels,
          selected = input_info$myel_compress_lvl$default
        )
      ),
      tags$div(
        class = "input-group-block",
        tags$h5("Background"),
        selectInput(
          "work",
          input_info$work$label,
          choices = input_info$work$levels,
          selected = input_info$work$default
        ),
        selectInput(
          "educ",
          input_info$educ$label,
          choices = input_info$educ$levels,
          selected = input_info$educ$default
        ),
        selectInput(
          "married",
          input_info$married$label,
          choices = input_info$married$levels,
          selected = input_info$married$default
        ),
        selectInput(
          "children",
          input_info$children$label,
          choices = input_info$children$levels,
          selected = input_info$children$default
        ),
        selectInput(
          "sick_leave",
          input_info$sick_leave$label,
          choices = input_info$sick_leave$levels,
          selected = input_info$sick_leave$default
        ),
        selectInput(
          "alcohol",
          input_info$alcohol$label,
          choices = input_info$alcohol$levels,
          selected = input_info$alcohol$default
        ),
        selectInput(
          "smoking",
          input_info$smoking$label,
          choices = input_info$smoking$levels,
          selected = input_info$smoking$default
        ),
        selectInput(
          "opioid",
          input_info$opioid$label,
          choices = input_info$opioid$levels,
          selected = input_info$opioid$default
        ),
        selectInput(
          "nsaid",
          input_info$nsaid$label,
          choices = input_info$nsaid$levels,
          selected = input_info$nsaid$default
        ),
        selectInput(
          "daytime",
          input_info$daytime$label,
          choices = input_info$daytime$levels,
          selected = input_info$daytime$default
        )
      ),
      div(
        style = "margin-top: 12px;",
        actionButton("reset_defaults", "Reset defaults")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        :root {
          --bg: #eef2f1;
          --card: rgba(255,255,255,0.86);
          --ink: #182126;
          --muted: #61717a;
          --line: rgba(24, 33, 38, 0.08);
          --conservative: #1f6f78;
          --conservative-soft: #dff0ee;
          --surgery: #c96a2a;
          --surgery-soft: #fbe6d6;
          --tie-soft: #fbf2cc;
          --accent: #23343b;
        }
        .content-wrapper, .right-side {
          background:
            radial-gradient(circle at top right, rgba(201,106,42,0.10), transparent 26%),
            radial-gradient(circle at top left, rgba(31,111,120,0.10), transparent 24%),
            var(--bg);
          color: var(--ink);
        }
        .main-header .logo {
          background: #182126;
          font-size: 18px;
          font-weight: 700;
          letter-spacing: 0.04em;
        }
        .main-header .navbar {
          background: #23343b;
        }
        .main-sidebar {
          background: #203039;
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li:hover > a {
          background: rgba(255,255,255,0.08);
          border-left-color: #d8c7a1;
        }
        .sidebar-scroll {
          padding: 16px;
        }
        .sidebar-scroll h4,
        .sidebar-scroll p,
        .sidebar-scroll label,
        .sidebar-scroll h5 {
          color: #e8edf0;
        }
        .sidebar-scroll h4 {
          margin-top: 6px;
          margin-bottom: 8px;
          font-weight: 700;
        }
        .sidebar-note {
          font-size: 12px;
          opacity: 0.8;
        }
        .sidebar .form-control {
          min-height: 42px;
          border: 0;
          border-radius: 12px;
          box-shadow: none;
        }
        .sidebar .btn-default {
          border: 0;
          border-radius: 12px;
          background: #d8c7a1;
          color: #1f2a30;
          font-weight: 700;
        }
        .input-group-block {
          display: block;
          margin-top: 18px;
          padding-top: 18px;
          border-top: 1px solid rgba(255,255,255,0.12);
        }
        .input-group-block h5 {
          margin-bottom: 12px;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
        }
        .hero-card,
        .summary-card,
        .result-card,
        .panel-card {
          border: 1px solid var(--line);
          border-radius: 22px;
          background: var(--card);
          box-shadow: 0 16px 40px rgba(24, 33, 38, 0.06);
        }
        .hero-card {
          margin-bottom: 20px;
          padding: 24px 26px;
          border: 0;
          background: linear-gradient(135deg, rgba(24,33,38,0.96), rgba(35,52,59,0.96));
          color: #f7f4ee;
        }
        .hero-card h2 {
          margin: 0 0 10px 0;
          font-size: 30px;
          font-weight: 700;
          line-height: 1.1;
        }
        .hero-eyebrow {
          margin-bottom: 8px;
          font-size: 11px;
          letter-spacing: 0.12em;
          text-transform: uppercase;
          opacity: 0.72;
        }
        .hero-copy {
          max-width: 760px;
          margin: 0;
          font-size: 15px;
          color: rgba(247,244,238,0.82);
        }
        .summary-card {
          min-height: 150px;
          margin-bottom: 18px;
          padding: 20px 22px;
        }
        .summary-conservative {
          background: linear-gradient(180deg, var(--conservative-soft), #ffffff);
        }
        .summary-surgery {
          background: linear-gradient(180deg, var(--surgery-soft), #ffffff);
        }
        .summary-tie {
          background: linear-gradient(180deg, var(--tie-soft), #ffffff);
        }
        .summary-label {
          margin-bottom: 16px;
          color: var(--muted);
          font-size: 12px;
          letter-spacing: 0.08em;
          text-transform: uppercase;
        }
        .summary-value {
          margin-bottom: 12px;
          color: var(--ink);
          font-size: 34px;
          font-weight: 700;
          line-height: 1;
        }
        .summary-meta {
          color: var(--muted);
          font-size: 13px;
        }
        .result-card {
          position: relative;
          min-height: 164px;
          margin-bottom: 18px;
          padding: 20px 22px;
          overflow: hidden;
        }
        .result-card:before {
          content: '';
          position: absolute;
          left: 0;
          top: 0;
          bottom: 0;
          width: 6px;
        }
        .result-conservative:before {
          background: var(--conservative);
        }
        .result-surgery:before {
          background: var(--surgery);
        }
        .result-title {
          margin-bottom: 14px;
          color: var(--muted);
          font-size: 11px;
          letter-spacing: 0.08em;
          text-transform: uppercase;
        }
        .result-row {
          display: flex;
          justify-content: space-between;
          align-items: flex-start;
          gap: 12px;
          margin-bottom: 10px;
        }
        .result-choice {
          margin: 0;
          font-size: 28px;
          font-weight: 700;
          line-height: 1.1;
        }
        .score-badge {
          white-space: nowrap;
          border-radius: 999px;
          padding: 7px 12px;
          background: #f4f1eb;
          color: var(--accent);
          font-size: 12px;
          font-weight: 700;
        }
        .result-note {
          margin: 0;
          color: var(--muted);
          font-size: 14px;
          line-height: 1.5;
        }
        .box {
          border-top: 0;
        }
        .panel-card.box {
          border: 1px solid var(--line);
          border-radius: 22px;
          background: var(--card);
          box-shadow: 0 16px 40px rgba(24, 33, 38, 0.06);
        }
        .panel-card .box-header {
          padding: 18px 20px 0 20px;
        }
        .panel-card .box-body {
          padding: 16px 20px 20px 20px;
        }
        .panel-card .box-title {
          color: var(--ink);
          font-weight: 700;
        }
        .nav-tabs-custom,
        .nav-tabs-custom > .tab-content {
          border: 1px solid var(--line);
          border-radius: 22px;
          box-shadow: 0 16px 40px rgba(24, 33, 38, 0.06);
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: var(--accent);
        }
        .tab-content {
          background: var(--card);
        }
        @media (max-width: 991px) {
          .hero-card h2 {
            font-size: 24px;
          }
          .result-row {
            flex-direction: column;
            align-items: flex-start;
          }
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "recommendations",
        fluidRow(
          column(
            width = 12,
            div(
              class = "hero-card",
              div(class = "hero-eyebrow", "Personalised treatment support"),
              h2("Six outcome-specific recommendations from one patient profile"),
              p(
                class = "hero-copy",
              )
            )
          )
        ),
        uiOutput("summary_cards"),
        uiOutput("result_cards"),
        fluidRow(
          box(
            class = "panel-card",
            width = 6,
            title = "Outcome-specific recommendation table",
            tableOutput("recommendation_table")
          ),
          box(
            class = "panel-card",
            width = 6,
            title = "Benefit scores across outcomes",
            plotOutput("score_plot", height = 320)
          )
        ),
        fluidRow(
          box(
            class = "panel-card",
            width = 12,
            title = "Interpretation note",
            p(
              "Each outcome is predicted separately. The overall majority is only a simple count and should not replace a prespecified clinical decision rule."
            )
          )
        )
      ),
      tabItem(
        tabName = "validation",
        fluidRow(
          tabBox(
            width = 12,
            title = "Validation interaction plots",
            id = "validation_tabs",
            tabPanel("NDI @ 1 year", plotOutput("validation_plot_1", height = 420)),
            tabPanel("NDI @ 2 years", plotOutput("validation_plot_2", height = 420)),
            tabPanel("Arm pain @ 1 year", plotOutput("validation_plot_3", height = 420)),
            tabPanel("Arm pain @ 2 years", plotOutput("validation_plot_4", height = 420)),
            tabPanel("Neck pain @ 1 year", plotOutput("validation_plot_5", height = 420)),
            tabPanel("Neck pain @ 2 years", plotOutput("validation_plot_6", height = 420))
          )
        )
      )
    )
  )
)

# -----------------------------
# 5. Run models and show output
# -----------------------------

server <- function(input, output, session) {
  observeEvent(input$reset_defaults, {
    for (var_name in names(input_info)) {
      if (input_info[[var_name]]$is_factor) {
        updateSelectInput(
          session = session,
          inputId = var_name,
          selected = input_info[[var_name]]$default
        )
      } else {
        updateNumericInput(
          session = session,
          inputId = var_name,
          value = input_info[[var_name]]$default
        )
      }
    }
  })

  patient_row <- reactive({
    patient <- patient_template[1, , drop = FALSE]

    for (var_name in names(patient_template)) {
      current_value <- input[[var_name]]

      if (input_info[[var_name]]$is_factor) {
        patient[[var_name]] <- factor(
          current_value,
          levels = input_info[[var_name]]$levels
        )
      } else {
        req(!is.null(current_value), is.finite(current_value))
        patient[[var_name]] <- as.numeric(current_value)
      }
    }

    patient
  })

  recommendation_table <- reactive({
    patient <- patient_row()
    patient_matrix <- model.matrix(~ 0 + ., data = patient)
    results <- data.frame(
      outcome = character(),
      recommendation = character(),
      benefit_score = numeric(),
      note = character(),
      stringsAsFactors = FALSE
    )

    for (i in seq_along(models)) {
      needed_columns <- models[[i]]$var.names
      model_matrix <- patient_matrix
      missing_columns <- setdiff(needed_columns, colnames(model_matrix))

      if (length(missing_columns) > 0) {
        model_matrix <- cbind(
          model_matrix,
          matrix(
            0,
            nrow = 1,
            ncol = length(missing_columns),
            dimnames = list(NULL, missing_columns)
          )
        )
      }

      model_matrix <- model_matrix[, needed_columns, drop = FALSE]

      trt_group <- as.numeric(predict(models[[i]], model_matrix, type = "trt.group"))
      benefit_score <- as.numeric(predict(models[[i]], model_matrix, type = "benefit.score"))
      recommendation <- if (trt_group == 1) "Surgery" else "Conservative"

      if (recommendation == "Surgery") {
        note <- "This model favors surgery for this outcome."
      } else {
        note <- "This model favors conservative care for this outcome."
      }

      results <- rbind(
        results,
        data.frame(
          outcome = outcome_names[i],
          recommendation = recommendation,
          benefit_score = benefit_score,
          note = note,
          stringsAsFactors = FALSE
        )
      )
    }

    results
  })

  output$summary_cards <- renderUI({
    results <- recommendation_table()
    conservative_n <- sum(results$recommendation == "Conservative")
    surgery_n <- sum(results$recommendation == "Surgery")

    if (conservative_n > surgery_n) {
      majority_label <- "Conservative"
      majority_class <- "summary-card summary-conservative"
    } else if (surgery_n > conservative_n) {
      majority_label <- "Surgery"
      majority_class <- "summary-card summary-surgery"
    } else {
      majority_label <- "Tie"
      majority_class <- "summary-card summary-tie"
    }

    fluidRow(
      column(
        width = 4,
        div(
          class = "summary-card summary-conservative",
          div(class = "summary-label", "Outcomes favoring conservative care"),
          div(class = "summary-value", conservative_n),
          div(class = "summary-meta", "Across all six models")
        )
      ),
      column(
        width = 4,
        div(
          class = "summary-card summary-surgery",
          div(class = "summary-label", "Outcomes favoring surgery"),
          div(class = "summary-value", surgery_n),
          div(class = "summary-meta", "Across all six models")
        )
      ),
      column(
        width = 4,
        div(
          class = majority_class,
          div(class = "summary-label", "Overall majority"),
          div(class = "summary-value", majority_label),
          div(class = "summary-meta", "Simple count across outcomes")
        )
      )
    )
  })

  output$result_cards <- renderUI({
    results <- recommendation_table()

    row1 <- results[1, , drop = FALSE]
    row2 <- results[2, , drop = FALSE]
    row3 <- results[3, , drop = FALSE]
    row4 <- results[4, , drop = FALSE]
    row5 <- results[5, , drop = FALSE]
    row6 <- results[6, , drop = FALSE]

    fluidPage(
      fluidRow(
        column(
          width = 4,
          div(
            class = paste(
              "result-card",
              if (row1$recommendation == "Surgery") "result-surgery" else "result-conservative"
            ),
            div(class = "result-title", row1$outcome),
            div(
              class = "result-row",
              div(class = "result-choice", row1$recommendation),
              div(class = "score-badge", sprintf("Score %.2f", row1$benefit_score))
            ),
            p(class = "result-note", row1$note)
          )
        ),
        column(
          width = 4,
          div(
            class = paste(
              "result-card",
              if (row2$recommendation == "Surgery") "result-surgery" else "result-conservative"
            ),
            div(class = "result-title", row2$outcome),
            div(
              class = "result-row",
              div(class = "result-choice", row2$recommendation),
              div(class = "score-badge", sprintf("Score %.2f", row2$benefit_score))
            ),
            p(class = "result-note", row2$note)
          )
        ),
        column(
          width = 4,
          div(
            class = paste(
              "result-card",
              if (row3$recommendation == "Surgery") "result-surgery" else "result-conservative"
            ),
            div(class = "result-title", row3$outcome),
            div(
              class = "result-row",
              div(class = "result-choice", row3$recommendation),
              div(class = "score-badge", sprintf("Score %.2f", row3$benefit_score))
            ),
            p(class = "result-note", row3$note)
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          div(
            class = paste(
              "result-card",
              if (row4$recommendation == "Surgery") "result-surgery" else "result-conservative"
            ),
            div(class = "result-title", row4$outcome),
            div(
              class = "result-row",
              div(class = "result-choice", row4$recommendation),
              div(class = "score-badge", sprintf("Score %.2f", row4$benefit_score))
            ),
            p(class = "result-note", row4$note)
          )
        ),
        column(
          width = 4,
          div(
            class = paste(
              "result-card",
              if (row5$recommendation == "Surgery") "result-surgery" else "result-conservative"
            ),
            div(class = "result-title", row5$outcome),
            div(
              class = "result-row",
              div(class = "result-choice", row5$recommendation),
              div(class = "score-badge", sprintf("Score %.2f", row5$benefit_score))
            ),
            p(class = "result-note", row5$note)
          )
        ),
        column(
          width = 4,
          div(
            class = paste(
              "result-card",
              if (row6$recommendation == "Surgery") "result-surgery" else "result-conservative"
            ),
            div(class = "result-title", row6$outcome),
            div(
              class = "result-row",
              div(class = "result-choice", row6$recommendation),
              div(class = "score-badge", sprintf("Score %.2f", row6$benefit_score))
            ),
            p(class = "result-note", row6$note)
          )
        )
      )
    )
  })

  output$recommendation_table <- renderTable({
    results <- recommendation_table()
    results$benefit_score <- sprintf("%.2f", results$benefit_score)
    results
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$score_plot <- renderPlot({
    results <- recommendation_table()
    results$outcome <- factor(results$outcome, levels = rev(outcome_names))

    ggplot(results, aes(x = benefit_score, y = outcome, fill = recommendation)) +
      geom_vline(xintercept = 0, color = "#6c7a89", linewidth = 0.6, linetype = "dashed") +
      geom_col(width = 0.65) +
      scale_fill_manual(values = c("Conservative" = "#1f6f78", "Surgery" = "#c96a2a")) +
      labs(
        x = "Benefit score",
        y = NULL,
        fill = NULL,
        caption = "Negative scores favor surgery; positive scores favor conservative."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "top"
      )
  }, res = 110)

  output$validation_plot_1 <- renderPlot({
    validation_plots[[1]]
  }, res = 110)

  output$validation_plot_2 <- renderPlot({
    validation_plots[[2]]
  }, res = 110)

  output$validation_plot_3 <- renderPlot({
    validation_plots[[3]]
  }, res = 110)

  output$validation_plot_4 <- renderPlot({
    validation_plots[[4]]
  }, res = 110)

  output$validation_plot_5 <- renderPlot({
    validation_plots[[5]]
  }, res = 110)

  output$validation_plot_6 <- renderPlot({
    validation_plots[[6]]
  }, res = 110)
}

shinyApp(ui, server)


