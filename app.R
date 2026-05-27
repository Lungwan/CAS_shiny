library(shiny)
library(shinydashboard)
library(personalized)
library(xgboost)
library(ggplot2)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

format_label <- function(x) {
  gsub(" +", " ", trimws(gsub("_", " ", x)))
}

resolve_model_file <- function() {
  env_path <- Sys.getenv("CASINO_MODEL_FILE", unset = "")
  candidates <- c(
    env_path,
    file.path(getwd(), "personalised_model_190426.RDS"),
    file.path(getwd(), "personalised_model_160426.RDS"),
    file.path(getwd(), "personalised_model.RDS"),
    "~/Documents/Casino/personalised_model_190426.RDS",
    "~/Documents/Casino/personalised_model_160426.RDS",
    "~/Documents/Casino/personalised_model.RDS"
  )

  existing <- candidates[nzchar(candidates) & file.exists(path.expand(candidates))]

  if (length(existing) == 0) {
    stop("No personalised model file was found.")
  }

  path.expand(existing[[1]])
}

normalize_model_bundle <- function(bundle) {
  if (!is.null(bundle$subgrp_list)) {
    model_list <- bundle$subgrp_list
  } else {
    model_names <- grep("^subgrp\\.model[0-9]+$", names(bundle), value = TRUE)
    model_list <- bundle[model_names]
  }

  if (!is.null(bundle$valid_list)) {
    validation_list <- lapply(bundle$valid_list, function(x) x$result %||% x)
  } else {
    validation_names <- grep("^validation[0-9]+$", names(bundle), value = TRUE)
    validation_list <- bundle[validation_names]
  }

  list(
    df = bundle$df,
    models = model_list,
    validations = validation_list
  )
}

friendly_labels <- c(
  hnp_number_c67_lower = "Lower C6-C7 HNP",
  age = "Age",
  gender = "Gender",
  bmi = "BMI",
  married = "Married",
  children = "Children",
  daytime = "Employment status",
  educ = "Education level",
  work = "Physical workload",
  sick_leave = "Sick leave",
  alcohol = "Alcohol use",
  smoking = "Smoking",
  ds14_w0 = "Baseline DS14 score",
  myel_compress = "Myelopathy compression",
  tingl_time = "Tingling duration",
  neckpain_freq = "Neck pain frequency",
  armpain_freq = "Arm pain frequency",
  tingl_freq = "Tingling frequency",
  loss_of_strength = "Loss of strength",
  nsaid = "NSAID use",
  hnp_loc_central = "Central HNP location",
  hnp_loc_ml = "Mediolateral HNP location",
  hnp_loc_form = "Foraminal HNP location",
  hnp_size = "HNP size",
  root_compress = "Root compression",
  vas_arm_w0 = "Baseline arm pain VAS (0-100)",
  vas_neck_w0 = "Baseline neck pain VAS (0-100)",
  w0_eq_vas = "Baseline EQ-VAS (0-100)",
  ndi_w0 = "Baseline NDI (0-100)",
  opioid = "Opioid use",
  tingling_deg = "Average tingling severity"
)

input_groups <- list(
  "Symptoms" = c(
    "ndi_w0",
    "vas_neck_w0",
    "vas_arm_w0",
    "w0_eq_vas",
    "neckpain_freq",
    "armpain_freq",
    "tingl_freq",
    "tingl_time",
    "tingling_deg",
    "loss_of_strength"
  ),
  "Imaging and Clinical" = c(
    "hnp_number_c67_lower",
    "hnp_loc_central",
    "hnp_loc_ml",
    "hnp_loc_form",
    "hnp_size",
    "root_compress",
    "myel_compress",
    "opioid",
    "nsaid"
  ),
  "Background" = c(
    "age",
    "gender",
    "bmi",
    "married",
    "children",
    "daytime",
    "educ",
    "work",
    "sick_leave",
    "alcohol",
    "smoking",
    "ds14_w0"
  )
)

outcome_specs <- data.frame(
  index = 1:8,
  id = c(
    "ndi_1y", "ndi_2y",
    "arm_1y", "arm_2y",
    "neck_1y", "neck_2y",
    "eqvas_1y", "eqvas_2y"
  ),
  outcome = c(
    "NDI", "NDI",
    "Arm pain", "Arm pain",
    "Neck pain", "Neck pain",
    "EQ-VAS", "EQ-VAS"
  ),
  label = c(
    "NDI @ 1 year", "NDI @ 2 years",
    "Arm pain @ 1 year", "Arm pain @ 2 years",
    "Neck pain @ 1 year", "Neck pain @ 2 years",
    "EQ-VAS @ 1 year", "EQ-VAS @ 2 years"
  ),
  horizon = c(
    "1 year", "2 years",
    "1 year", "2 years",
    "1 year", "2 years",
    "1 year", "2 years"
  ),
  threshold = c(24, 24, 25, 25, 35, 35, 69, 69),
  larger_better = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

threshold_text <- function(threshold, larger_better) {
  if (larger_better) {
    sprintf("Success cutoff >= %.0f", threshold)
  } else {
    sprintf("Success cutoff <= %.0f", threshold)
  }
}

threshold_met <- function(value, threshold, larger_better) {
  if (isTRUE(larger_better)) {
    value >= threshold
  } else {
    value <= threshold
  }
}

interaction_plot <- function(validation_obj, ylab_text) {
  temp <- plot(validation_obj, type = "interaction")$data

  temp$Recommended <- factor(
    temp$Recommended,
    levels = c(0, 1),
    labels = c("Conservative", "Surgery")
  )
  temp$Received <- factor(
    temp$Received,
    levels = c(0, 1),
    labels = c("Conservative", "Surgery")
  )

  ggplot(temp, aes(x = Recommended, y = Value, color = Received, group = Received)) +
    geom_errorbar(
      aes(
        ymin = Value - 1.96 * SE,
        ymax = Value + 1.96 * SE
      ),
      width = 0.08,
      linewidth = 0.6,
      alpha = 0.7
    ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.8) +
    scale_color_manual(values = c("#1f78b4", "#e67e22")) +
    labs(
      x = "Recommended treatment",
      y = ylab_text,
      color = "Actual treatment received"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
}

extract_estimated_outcomes <- function(validation_data, trt_group) {
  subgroup_rows <- validation_data[validation_data$Recommended == trt_group, , drop = FALSE]
  conservative_row <- subgroup_rows[subgroup_rows$Received == 0, , drop = FALSE]
  surgery_row <- subgroup_rows[subgroup_rows$Received == 1, , drop = FALSE]

  conservative_value <- conservative_row$Value[[1]]
  surgery_value <- surgery_row$Value[[1]]

  if (trt_group == 1) {
    recommended_value <- surgery_value
    alternative_value <- conservative_value
    alternative_treatment <- "Conservative"
  } else {
    recommended_value <- conservative_value
    alternative_value <- surgery_value
    alternative_treatment <- "Surgery"
  }

  list(
    conservative_value = conservative_value,
    surgery_value = surgery_value,
    recommended_value = recommended_value,
    alternative_value = alternative_value,
    alternative_treatment = alternative_treatment
  )
}

build_input_control <- function(var_name) {
  values <- input_data[[var_name]]
  label <- friendly_labels[[var_name]] %||% format_label(var_name)

  if (is.factor(values)) {
    return(
      selectInput(
        inputId = var_name,
        label = label,
        choices = levels(values),
        selected = default_values[[var_name]]
      )
    )
  }

  numericInput(
    inputId = var_name,
    label = label,
    value = default_values[[var_name]],
    min = floor(min(values, na.rm = TRUE)),
    max = ceiling(max(values, na.rm = TRUE)),
    step = 0.1
  )
}

build_patient_profile <- function(input_values) {
  patient <- input_data[1, , drop = FALSE]

  for (var_name in input_names) {
    if (is.factor(input_data[[var_name]])) {
      patient[[var_name]] <- factor(
        input_values[[var_name]],
        levels = levels(input_data[[var_name]])
      )
    } else {
      patient[[var_name]] <- as.numeric(input_values[[var_name]])
    }
  }

  patient
}

build_design_matrix <- function(patient_data, var_names) {
  x_new <- model.matrix(~ 0 + ., data = patient_data)
  missing_cols <- setdiff(var_names, colnames(x_new))

  if (length(missing_cols) > 0) {
    missing_matrix <- matrix(
      0,
      nrow = nrow(x_new),
      ncol = length(missing_cols),
      dimnames = list(NULL, missing_cols)
    )
    x_new <- cbind(x_new, missing_matrix)
  }

  x_new[, var_names, drop = FALSE]
}

build_input_schema <- function(data) {
  data.frame(
    variable = names(data),
    input_type = vapply(data, function(x) {
      if (is.factor(x)) "factor" else "numeric"
    }, character(1)),
    levels_or_range = vapply(data, function(x) {
      if (is.factor(x)) {
        paste(levels(x), collapse = " | ")
      } else {
        sprintf("%.1f to %.1f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
      }
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

validate_app_schema <- function(model_bundle, input_data, input_groups) {
  schema <- build_input_schema(input_data)
  grouped_inputs <- unique(unlist(input_groups, use.names = FALSE))

  missing_from_groups <- setdiff(schema$variable, grouped_inputs)
  extra_in_groups <- setdiff(grouped_inputs, schema$variable)

  if (length(missing_from_groups) > 0 || length(extra_in_groups) > 0) {
    stop(
      paste(
        "Input groups are out of sync with the model schema.",
        if (length(missing_from_groups) > 0) {
          paste("Missing from UI groups:", paste(missing_from_groups, collapse = ", "))
        },
        if (length(extra_in_groups) > 0) {
          paste("Extra in UI groups:", paste(extra_in_groups, collapse = ", "))
        }
      )
    )
  }

  template_patient <- input_data[1, , drop = FALSE]
  design_checks <- vapply(model_bundle$models, function(model_obj) {
    identical(
      colnames(build_design_matrix(template_patient, model_obj$var.names)),
      model_obj$var.names
    )
  }, logical(1))

  if (!all(design_checks)) {
    failing_models <- paste(which(!design_checks), collapse = ", ")
    stop(
      sprintf(
        "The app inputs do not rebuild the expected model matrix for model(s): %s",
        failing_models
      )
    )
  }

  schema
}

model_bundle <- normalize_model_bundle(readRDS(resolve_model_file()))
baseline_vars <- names(model_bundle$df)[!grepl("treatment|w52|12|26|38|104|6_", names(model_bundle$df))]
input_data <- model_bundle$df[, baseline_vars, drop = FALSE]
input_names <- names(input_data)

default_values <- setNames(
  lapply(input_names, function(var_name) {
    values <- input_data[[var_name]]
    if (is.factor(values)) {
      as.character(mode_value(values))
    } else {
      round(stats::median(values, na.rm = TRUE), 1)
    }
  }),
  input_names
)

input_groups <- lapply(input_groups, intersect, y = input_names)
assigned_inputs <- unique(unlist(input_groups, use.names = FALSE))
remaining_inputs <- setdiff(input_names, assigned_inputs)

if (length(remaining_inputs) > 0) {
  input_groups[["Other"]] <- remaining_inputs
}

input_schema <- validate_app_schema(model_bundle, input_data, input_groups)

validation_data_list <- lapply(model_bundle$validations, function(x) {
  plot(x, type = "interaction")$data
})

validation_plot_list <- lapply(seq_len(nrow(outcome_specs)), function(i) {
  interaction_plot(model_bundle$validations[[i]], outcome_specs$label[[i]])
})

validation_choices <- setNames(
  as.character(outcome_specs$index),
  outcome_specs$label
)

server <- function(input, output, session) {
  build_horizon_summary <- function(recs, horizon_label) {
    better_treatment <- vapply(seq_len(nrow(recs)), function(i) {
      row <- recs[i, , drop = FALSE]

      if (isTRUE(all.equal(row$conservative_value, row$surgery_value))) {
        return("Tie")
      }

      if (isTRUE(row$larger_better)) {
        if (row$conservative_value > row$surgery_value) "Conservative" else "Surgery"
      } else {
        if (row$conservative_value < row$surgery_value) "Conservative" else "Surgery"
      }
    }, character(1))

    surgery_count <- sum(better_treatment == "Surgery")
    conservative_count <- sum(better_treatment == "Conservative")
    tie_count <- sum(better_treatment == "Tie")
    is_tie <- surgery_count == conservative_count

    overall_recommendation <- if (is_tie) {
      "Conservative = Surgery"
    } else if (surgery_count > conservative_count) {
      "Surgery"
    } else {
      "Conservative"
    }

    card_class <- if (is_tie) {
      "overall-tie"
    } else if (overall_recommendation == "Surgery") {
      "overall-surgery"
    } else {
      "overall-conservative"
    }

    note_text <- if (is_tie) {
      sprintf(
        "%d of 4 outcomes favour Conservative; %d of 4 favour Surgery%s.",
        conservative_count,
        surgery_count,
        if (tie_count > 0) sprintf("; %d tied", tie_count) else ""
      )
    } else {
      sprintf(
        "%d of 4 outcomes favour %s; %d of 4 favour %s%s.",
        max(surgery_count, conservative_count),
        overall_recommendation,
        min(surgery_count, conservative_count),
        if (overall_recommendation == "Surgery") "Conservative" else "Surgery",
        if (tie_count > 0) sprintf("; %d tied", tie_count) else ""
      )
    }

    data.frame(
      horizon = horizon_label,
      recommendation = overall_recommendation,
      card_class = card_class,
      note_text = note_text,
      stringsAsFactors = FALSE
    )
  }

  observeEvent(input$reset_defaults, {
    for (var_name in input_names) {
      if (is.factor(input_data[[var_name]])) {
        updateSelectInput(
          session = session,
          inputId = var_name,
          selected = default_values[[var_name]]
        )
      } else {
        updateNumericInput(
          session = session,
          inputId = var_name,
          value = default_values[[var_name]]
        )
      }
    }
  })

  patient_profile <- reactive({
    numeric_inputs_ok <- all(vapply(
      input_names[!vapply(input_data, is.factor, logical(1))],
      function(var_name) {
        !is.null(input[[var_name]]) && is.finite(input[[var_name]])
      },
      logical(1)
    ))

    req(numeric_inputs_ok)

    input_values <- setNames(vector("list", length(input_names)), input_names)

    for (var_name in input_names) {
      input_values[[var_name]] <- input[[var_name]]
    }

    build_patient_profile(input_values)
  })

  outcome_recommendations <- reactive({
    patient_data <- patient_profile()

    outcome_rows <- lapply(seq_len(nrow(outcome_specs)), function(i) {
      spec <- outcome_specs[i, , drop = FALSE]
      model_obj <- model_bundle$models[[spec$index]]
      design_matrix <- build_design_matrix(patient_data, model_obj$var.names)

      trt_group <- as.numeric(predict(model_obj, design_matrix, type = "trt.group"))
      treatment <- if (trt_group == 1) "Surgery" else "Conservative"

      estimates <- extract_estimated_outcomes(validation_data_list[[spec$index]], trt_group)
      conservative_met <- threshold_met(
        value = estimates$conservative_value,
        threshold = spec$threshold,
        larger_better = spec$larger_better
      )
      surgery_met <- threshold_met(
        value = estimates$surgery_value,
        threshold = spec$threshold,
        larger_better = spec$larger_better
      )
      comparison_state <- if (conservative_met && surgery_met) {
        "neutral-positive"
      } else if (!conservative_met && !surgery_met) {
        "neutral-negative"
      } else if (conservative_met) {
        "prefer-conservative"
      } else {
        "prefer-surgery"
      }
      comparison_note <- switch(
        comparison_state,
        "neutral-positive" = "Both treatments reach the published success threshold.",
        "neutral-negative" = "Neither treatment reaches the published success threshold.",
        "prefer-conservative" = "Conservative reaches the published success threshold; Surgery does not.",
        "prefer-surgery" = "Surgery reaches the published success threshold; Conservative does not."
      )

      data.frame(
        id = spec$id,
        outcome = spec$outcome,
        label = spec$label,
        horizon = spec$horizon,
        recommendation = treatment,
        conservative_value = estimates$conservative_value,
        surgery_value = estimates$surgery_value,
        threshold = spec$threshold,
        larger_better = spec$larger_better,
        threshold_text = threshold_text(spec$threshold, spec$larger_better),
        conservative_met = conservative_met,
        surgery_met = surgery_met,
        comparison_state = comparison_state,
        comparison_note = comparison_note,
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, outcome_rows)
  })

  one_year_recommendations <- reactive({
    recs <- outcome_recommendations()
    recs[recs$horizon == "1 year", , drop = FALSE]
  })

  two_year_recommendations <- reactive({
    recs <- outcome_recommendations()
    recs[recs$horizon == "2 years", , drop = FALSE]
  })

  build_treatment_box_ui <- function(row, treatment) {
    value <- if (treatment == "Conservative") row$conservative_value else row$surgery_value
    met <- if (treatment == "Conservative") row$conservative_met else row$surgery_met
    status_class <- if (met) "threshold-pass" else "threshold-fail"
    status_face <- if (met) ":)" else ":("
    status_text <- if (met) "Meets threshold" else "Does not meet threshold"
    compare_class <- if (isTRUE(all.equal(row$conservative_value, row$surgery_value))) {
      "treatment-tie"
    } else {
      better_treatment <- if (row$larger_better) {
        if (row$conservative_value > row$surgery_value) "Conservative" else "Surgery"
      } else {
        if (row$conservative_value < row$surgery_value) "Conservative" else "Surgery"
      }

      if (identical(treatment, better_treatment)) {
        "treatment-better"
      } else {
        "treatment-other"
      }
    }

    div(
      class = paste("treatment-box", compare_class),
      div(
        class = "treatment-topline",
        div(class = "treatment-name", treatment),
        div(class = paste("treatment-status", status_class), paste(status_face, status_text))
      ),
      div(class = paste("treatment-value", compare_class), sprintf("%.1f", value)),
      div(class = "treatment-caption", sprintf("Predicted value with %s", treatment)),
      div(class = "treatment-threshold", row$threshold_text)
    )
  }

  build_outcome_compare_ui <- function(recs) {
    fluidRow(
      lapply(seq_len(nrow(recs)), function(i) {
        row <- recs[i, , drop = FALSE]
        state_class <- paste("compare-note", row$comparison_state)

        column(
          width = 12,
          div(
            class = "comparison-card",
            div(class = "comparison-header",
              div(class = "comparison-title", row$label),
              div(class = state_class, row$comparison_note)
            ),
            div(
              class = "comparison-grid",
              div(class = "comparison-col", build_treatment_box_ui(row, "Conservative")),
              div(class = "comparison-col", build_treatment_box_ui(row, "Surgery"))
            ),
            div(class = "comparison-footer", sprintf("Published success threshold: %s", row$threshold_text))
          )
        )
      })
    )
  }

  output$year1_comparison_cards <- renderUI({
    build_outcome_compare_ui(one_year_recommendations())
  })

  output$year2_comparison_cards <- renderUI({
    build_outcome_compare_ui(two_year_recommendations())
  })

  output$year1_overall <- renderUI({
    summary_row <- build_horizon_summary(one_year_recommendations(), "Year 1")
    div(
      class = paste("overall-card", summary_row$card_class),
      div(class = "overall-kicker", sprintf("%s overall recommendation", summary_row$horizon)),
      div(class = "overall-value", summary_row$recommendation),
      div(class = "overall-note", summary_row$note_text)
    )
  })

  output$year2_overall <- renderUI({
    summary_row <- build_horizon_summary(two_year_recommendations(), "Year 2")
    div(
      class = paste("overall-card", summary_row$card_class),
      div(class = "overall-kicker", sprintf("%s overall recommendation", summary_row$horizon)),
      div(class = "overall-value", summary_row$recommendation),
      div(class = "overall-note", summary_row$note_text)
    )
  })

  output$validation_plot_1 <- renderPlot({
    plot_idx <- as.integer(input$validation_choice %||% "1")
    validation_plot_list[[plot_idx]]
  }, res = 110)
}

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
      tags$p(
        "Enter baseline variables. The dashboard updates automatically using the latest subgroup models."
      ),
      tags$p(
        class = "sidebar-note",
        "Defaults are training-set medians or most common categories."
      ),
      lapply(names(input_groups), function(group_name) {
        vars <- input_groups[[group_name]]
        tags$div(
          class = "input-group",
          tags$h5(group_name),
          lapply(vars, build_input_control)
        )
      }),
      div(
        class = "reset-wrap",
        actionButton("reset_defaults", "Reset defaults")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        :root {
          --bg: #eef2f1;
          --ink: #182126;
          --muted: #61717a;
          --line: rgba(24, 33, 38, 0.08);
          --conservative: #1f6f78;
          --surgery: #c96a2a;
          --accent: #23343b;
          --success: #2e7d32;
          --alert: #8d6e63;
        }
        .content-wrapper, .right-side {
          background:
            radial-gradient(circle at top right, rgba(201,106,42,0.10), transparent 26%),
            radial-gradient(circle at top left, rgba(31,111,120,0.10), transparent 24%),
            var(--bg);
          color: var(--ink);
        }
        .main-header .logo {
          font-weight: 700;
          letter-spacing: 0.04em;
          background: #182126;
          font-size: 18px;
        }
        .main-header .navbar {
          background: #23343b;
        }
        .main-sidebar {
          background: #203039;
          position: fixed;
          top: 50px;
          bottom: 0;
          overflow-y: auto;
          overflow-x: hidden;
          scrollbar-gutter: stable;
        }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li:hover > a {
          background: rgba(255,255,255,0.08);
          border-left-color: #d8c7a1;
        }
        .sidebar-menu {
          margin-top: 0;
        }
        .sidebar-scroll {
          padding: 16px;
          padding-bottom: 34px;
        }
        .reset-wrap {
          margin-top: 40px;
          padding-top: 14px;
          border-top: 1px solid rgba(255,255,255,0.12);
        }
        .sidebar-scroll h4 {
          color: #fffaf1;
          font-weight: 700;
          margin-top: 6px;
          margin-bottom: 8px;
        }
        .sidebar-scroll p,
        .sidebar-scroll label,
        .sidebar-scroll h5 {
          color: #e8edf0;
        }
        .sidebar .form-control {
          border-radius: 12px;
          border: 0;
          min-height: 42px;
          box-shadow: none;
        }
        .sidebar .btn-default {
          border-radius: 12px;
          border: 0;
          background: #d8c7a1;
          color: #1f2a30;
          font-weight: 700;
          margin-top: 12px;
        }
        .input-group {
          border-top: 1px solid rgba(255,255,255,0.12);
          margin-top: 18px;
          padding-top: 18px;
          display: block;
        }
        .input-group h5 {
          font-weight: 700;
          margin-bottom: 12px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          font-size: 11px;
        }
        .sidebar-note {
          font-size: 12px;
          opacity: 0.8;
        }
        .hero-card,
        .comparison-card,
        .panel-card {
          border-radius: 22px;
          border: 1px solid var(--line);
          background: rgba(255,255,255,0.82);
          box-shadow: 0 16px 40px rgba(24, 33, 38, 0.06);
        }
        .hero-card {
          padding: 24px 26px;
          margin-bottom: 20px;
          background: linear-gradient(135deg, rgba(24,33,38,0.96), rgba(35,52,59,0.96));
          color: #f7f4ee;
          border: 0;
        }
        .hero-eyebrow {
          text-transform: uppercase;
          letter-spacing: 0.12em;
          font-size: 11px;
          opacity: 0.72;
          margin-bottom: 8px;
        }
        .hero-title {
          font-size: 30px;
          line-height: 1.1;
          font-weight: 700;
          margin: 0 0 10px 0;
        }
        .hero-copy {
          max-width: 760px;
          color: rgba(247,244,238,0.82);
          margin: 0;
          font-size: 15px;
        }
        .year-tabs {
          margin-bottom: 16px;
          background: transparent;
          box-shadow: none;
          border: 0;
        }
        .year-tabs .nav-tabs-custom {
          background: transparent;
          box-shadow: none;
        }
        .year-tabs .nav-tabs {
          border-bottom: 0;
        }
        .year-tabs .nav-tabs li a {
          border-radius: 14px 14px 0 0;
          font-weight: 700;
          color: var(--muted);
        }
        .year-tabs .nav-tabs li.active a {
          color: var(--ink);
          background: rgba(255,255,255,0.94);
          border-top-color: #d8c7a1;
        }
        .year-tabs .tab-content {
          background: transparent;
          padding: 0;
        }
        .overall-card {
          border-radius: 22px;
          padding: 20px 22px;
          margin-bottom: 18px;
          border: 1px solid var(--line);
          box-shadow: 0 16px 40px rgba(24, 33, 38, 0.06);
          background: rgba(255,255,255,0.90);
        }
        .overall-conservative {
          background: linear-gradient(180deg, rgba(31,111,120,0.14), rgba(255,255,255,0.96));
        }
        .overall-surgery {
          background: linear-gradient(180deg, rgba(201,106,42,0.14), rgba(255,255,255,0.96));
        }
        .overall-tie {
          background: linear-gradient(180deg, rgba(35,52,59,0.10), rgba(255,255,255,0.96));
        }
        .overall-kicker {
          font-size: 12px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: var(--muted);
          margin-bottom: 12px;
        }
        .overall-value {
          font-size: 34px;
          font-weight: 700;
          color: var(--ink);
          line-height: 1;
          margin-bottom: 10px;
        }
        .overall-note {
          color: var(--muted);
          font-size: 13px;
          line-height: 1.5;
        }
        .comparison-card {
          padding: 20px 22px;
          margin-bottom: 18px;
        }
        .comparison-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          flex-wrap: wrap;
          gap: 12px;
          margin-bottom: 16px;
        }
        .comparison-title {
          font-size: 20px;
          font-weight: 700;
          color: var(--ink);
        }
        .compare-note {
          border-radius: 999px;
          padding: 8px 12px;
          font-size: 12px;
          font-weight: 700;
          line-height: 1.4;
        }
        .compare-note.neutral-positive {
          background: rgba(35,52,59,0.08);
          color: var(--accent);
        }
        .compare-note.neutral-negative {
          background: rgba(35,52,59,0.08);
          color: var(--accent);
        }
        .compare-note.prefer-conservative,
        .compare-note.prefer-surgery {
          background: rgba(46,125,50,0.10);
          color: var(--success);
        }
        .comparison-grid {
          display: flex;
          gap: 16px;
          flex-wrap: wrap;
        }
        .comparison-col {
          flex: 1 1 320px;
        }
        .treatment-box {
          border-radius: 18px;
          padding: 18px;
          border: 1px solid var(--line);
          min-height: 188px;
          background: rgba(255,255,255,0.84);
          transition: background 0.2s ease, border-color 0.2s ease, box-shadow 0.2s ease;
        }
        .treatment-box.treatment-better {
          background: rgba(46,125,50,0.08);
          border-color: rgba(46,125,50,0.24);
          box-shadow: inset 0 0 0 1px rgba(46,125,50,0.06);
        }
        .treatment-box.treatment-other {
          background: rgba(255,255,255,0.84);
        }
        .treatment-box.treatment-tie {
          background: rgba(35,52,59,0.05);
          border-color: rgba(35,52,59,0.12);
        }
        .treatment-topline {
          display: flex;
          justify-content: space-between;
          align-items: center;
          gap: 12px;
          margin-bottom: 14px;
        }
        .treatment-name {
          font-size: 22px;
          font-weight: 700;
          color: var(--ink);
        }
        .treatment-status {
          border-radius: 999px;
          padding: 7px 11px;
          font-size: 12px;
          font-weight: 700;
          white-space: nowrap;
        }
        .treatment-status.threshold-pass {
          color: var(--success);
          background: rgba(46,125,50,0.12);
        }
        .treatment-status.threshold-fail {
          color: var(--alert);
          background: rgba(141,110,99,0.16);
        }
        .treatment-value {
          font-size: 34px;
          font-weight: 700;
          line-height: 1;
          margin-bottom: 10px;
          color: var(--ink);
        }
        .treatment-value.treatment-better {
          color: var(--success);
        }
        .treatment-value.treatment-tie {
          color: var(--accent);
        }
        .treatment-caption {
          color: var(--muted);
          font-size: 13px;
          margin-bottom: 12px;
        }
        .treatment-threshold,
        .comparison-footer {
          color: var(--muted);
          font-size: 13px;
          line-height: 1.5;
        }
        .comparison-footer {
          margin-top: 14px;
        }
        .outcome-note {
          margin-top: 14px;
          padding: 12px 14px;
          border-radius: 14px;
          background: rgba(255, 255, 255, 0.72);
          border: 1px solid rgba(31, 120, 180, 0.14);
          color: var(--muted);
          font-size: 14px;
          line-height: 1.55;
        }
        .outcome-note strong {
          color: var(--ink);
        }
        .box {
          border-top: 0;
        }
        .panel-card.box {
          background: rgba(255,255,255,0.84);
          border-radius: 22px;
          border: 1px solid var(--line);
          box-shadow: 0 16px 40px rgba(24, 33, 38, 0.06);
        }
        .panel-card .box-header {
          padding: 18px 20px 0 20px;
        }
        .panel-card .box-title {
          font-weight: 700;
          color: var(--ink);
        }
        .panel-card .box-body {
          padding: 16px 20px 20px 20px;
        }
        .table {
          background: transparent;
        }
        @media (max-width: 991px) {
          .main-sidebar {
            position: static;
            top: auto;
            bottom: auto;
            height: auto;
            overflow: visible;
          }
          .sidebar-scroll {
            overflow: visible;
          }
          .hero-title {
            font-size: 24px;
          }
          .comparison-header,
          .treatment-topline {
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
              h2(class = "hero-title", "Treatment Comparison With Estimated Outcomes"),
              p(
                class = "hero-copy",
                "Choose the Year 1 or Year 2 tab to compare Conservative care and Surgery side by side for NDI, arm pain, neck pain, and EQ-VAS. Each treatment is marked against the published success threshold for that outcome."
              ),
              div(
                class = "outcome-note",
                HTML(
                  "<strong>How to read these scores:</strong> For NDI, Arm pain, and Neck pain, lower values are better. For EQ-VAS, higher values are better."
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "year-tabs",
              tabBox(
                width = 12,
                id = "year_tabs",
                tabPanel(
                  "Year 1",
                  uiOutput("year1_overall"),
                  uiOutput("year1_comparison_cards")
                ),
                tabPanel(
                  "Year 2",
                  uiOutput("year2_overall"),
                  uiOutput("year2_comparison_cards")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "validation",
        fluidRow(
          box(
            class = "panel-card",
            width = 12,
            title = "Validation interaction plot",
            selectInput(
              inputId = "validation_choice",
              label = "Outcome",
              choices = validation_choices,
              selected = "1"
            ),
            plotOutput(
              outputId = "validation_plot_1",
              height = 420
            )
          )
        )
      )
    )
  )
)

shinyApp(ui, server)
