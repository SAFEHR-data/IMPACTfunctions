#' Calculate Consensus Procedure Start and End Times
#'
#' @description
#' Often with the `Phases` table, a number of timestamps can be out of logical order or documented at the wriong time.
#' This often comes down to errors opening/closing the anaesthetic chart or retrospective/lack of documentation.
#'
#' This function looks to establish if there is an error in documentation and establishing the most likely procedure start/end times.
#'
#' Outlier detection is done using the width between the series of timestamps for the start and end respectively.
#' If the width is less than `start/end_width_limit` then a coalese statement picks the first value in order that is not `NA`.
#' For the `start_time_columns` this searches from the last column and works backwards.
#' For the `end_time_column` this searches from the first column backwards.
#' If the temporal spread exceeds the respective `width_limit`, a median time is taken between all of the non-`NA` columns.
#' (Note that this does include the outlier itself)
#'
#' @param dataset Study cohort (optional). If provided, filters the `phases` dataset
#'   to include only matching `admission_csn` identifiers. This improves performance by only processing a subset of data.
#' @param phases Surgical phases table.
#'   Must include columns specified in `start_time_columns`, `end_time_columns`,
#'   and core identifiers (`mrn`, `admission_csn`, `anaesthesia_csn`). Defaults to `Phases`.
#' @param start_time_columns Character vector. Column names designating procedural
#'   initiation milestones. Default sequence: `"anaesthesia_start"`, `"anaesthesia_ready"`,
#'   `"patient_in_room"`, `"procedure_start"`. If width limit acceptable then will coalesce from the **LAST** column backwards.
#' @param start_width_limit Numeric. The maximum acceptable duration (in minutes)
#'   between the earliest and latest start times before triggering fallback median calculation. Default is 90.
#' @param end_time_columns Character vector. Column names designating procedural
#'   termination milestones. Default sequence: `"procedure_end"`, `"patient_out_of_room"`,
#'   `"anaesthesia_end"`, `"in_recovery"`.  If width limit acceptable then will coalesce from the **FIRST** column backwards
#' @param end_width_limit Numeric. The maximum acceptable duration (in minutes)
#'   between the earliest and latest end times before triggering fallback median calculation. Default is 60.
#' @param time_column_suffix Character. An optional suffix string appended to the
#'   resulting consensus time columns (e.g., `start_time_[suffix]`).
#' @param diagnostics Logical. If `TRUE` (default), intercepts processing to return a
#'   list of diagnostic plots evaluating timestamp distribution widths. Useful to tune the function for different cohorts.
#' @param verbose Logical. If `TRUE` (default), outputs execution alerts and processing
#'   heuristics to the console via `cli`.
#'
#' @return If `diagnostics = TRUE`, returns a `list` containing two diagnostic plots.
#'   If `diagnostics = FALSE`, returns a processed data frame containing `mrn`,
#'   `admission_csn`, `anaesthesia_csn`, along with the resolved consensus start and end timestamps.
#'
#' @import patchwork
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(lubridate)
#'
#' # Mocking a workflow dataset
#' mock_phases <- tibble(
#'   mrn = c("1", "2"),
#'   admission_csn = c(101, 102),
#'   anaesthesia_csn = c(201, 202),
#'   anaesthesia_start = ymd_hms(c("2026-06-15 08:00:00", "2026-06-15 09:00:00")),
#'   anaesthesia_ready = ymd_hms(c("2026-06-15 08:15:00", "2026-06-15 09:10:00")),
#'   patient_in_room   = ymd_hms(c("2026-06-15 08:20:00", "2026-06-15 09:15:00")),
#'   procedure_start   = ymd_hms(c("2026-06-15 08:30:00", "2026-06-15 11:30:00")),
#'   procedure_end     = ymd_hms(c("2026-06-15 10:00:00", "2026-06-15 13:00:00")),
#'   patient_out_of_room = ymd_hms(c("2026-06-15 10:10:00", "2026-06-15 13:15:00")),
#'   anaesthesia_end   = ymd_hms(c("2026-06-15 10:15:00", "2026-06-15 13:20:00")),
#'   in_recovery       = ymd_hms(c("2026-06-15 10:30:00", "2026-06-15 13:45:00"))
#' )
#'
#' # Execute calculation without diagnostics
#' df_times <- calculate_procedure_times(
#'   phases = mock_phases,
#'   diagnostics = FALSE,
#'   time_column_suffix = "derived"
#' )
#' print(df_times)
#' }
calculate_procedure_times <- function(dataset,
                                      phases = Phases,
                                      start_time_columns = c("anaesthesia_start","anaesthesia_ready", "patient_in_room", "procedure_start"),
                                      start_width_limit = 90,
                                      end_time_columns = c("procedure_end", "patient_out_of_room","anaesthesia_end", "in_recovery"),
                                      end_width_limit = 60,
                                      time_column_suffix,
                                      diagnostics = TRUE,
                                      verbose = TRUE){

  columns <- c("mrn", "admission_csn", "anaesthesia_csn")

  # Rename only Phases.anaesthesia_end_derived on ingest, so the name
  # `anaesthesia_end_IMPACT` is free for our pattern-validated output.
  # The raw documented Phases column stays as `anaesthesia_end`.

  if(!missing(dataset)){
    phases <- phases %>% filter(admission_csn %in% pull(dataset, admission_csn) |
                                  mrn %in% pull(dataset, mrn) |
                                  anaesthesia_csn %in% pull(dataset, anaesthesia_csn))
    if(verbose){cli::cli_alert_info("{.arg dataset} provided, proccesing {.arg phases} for only these patients")}
  }

  phases <- phases %>%
    dplyr::select(dplyr::all_of(c(columns, start_time_columns, end_time_columns))) %>%
    dplyr::collect()

  if(verbose){cli::cli_progress_step("Checking time clusters for outliers.")}
  phases <- phases %>%
    rowwise() %>%
    mutate(start_min = min(c_across(all_of(start_time_columns)), na.rm = TRUE),
           start_max = max(c_across(all_of(start_time_columns)), na.rm = TRUE),
           end_min = min(c_across(all_of(end_time_columns)), na.rm = TRUE),
           end_max = max(c_across(all_of(end_time_columns)), na.rm = TRUE)
           )%>%
    ungroup() %>%
    mutate(start_width = start_max - start_min,
           end_width = end_max - end_min)

  if(diagnostics){

    cli::cli_alert_success("Returning the diagnostic plots. See {.fn ?calculate_procedure_times} for more info on the interpretation.")
   result <- list(
     calculate_procedure_times_diagnostics_plot(phases, "start_width", width_limit = start_width_limit),
    calculate_procedure_times_diagnostics_plot(phases, "end_width", width_limit = end_width_limit)
    )

   return(result)
  }


  phases <- phases %>%
    mutate(start_width_acceptable = ifelse(start_width <= as.period(start_width_limit, unit = "mins"), TRUE, FALSE),
           end_width_acceptable = ifelse(end_width <= as.period(end_width_limit, unit = "mins"), TRUE, FALSE),)


  if(verbose){
    cli::cli_alert_info("For each procedure checking if any outlier times. \nIf no outliers then coalesceing down (skipping) {.val NA} values) the columns: {.val {rev(start_time_columns)}}. \n If outlier detected taking the median time of columns")
    cli::cli_progress_step("\nIf no outliers then coalesceing down the columns (skipping) {.val NA} values) {.val {end_time_columns}}. \n If outlier detected taking the median time of columns")
     }

  phases <- phases %>% rowwise() %>%
    mutate(
      start_time = case_when(
        start_width_acceptable  ~ reduce(pick(all_of(rev(start_time_columns))), coalesce,),
        !start_width_acceptable ~ median(c_across(all_of(start_time_columns)), na.rm = TRUE),
        .default                = NA_POSIXct_
      ),
      end_time = case_when(
          end_width_acceptable  ~ reduce(pick(all_of(end_time_columns)), coalesce, .dir = "backward"),
          !end_width_acceptable ~ median(c_across(all_of(end_time_columns)), na.rm = TRUE),
          .default                = NA_POSIXct_
        )
    ) %>% ungroup()


  phases <- phases %>% select(all_of(columns), start_time, end_time)

  if(verbose){cli::cli_progress_step("Returning dataset")}
  if(!missing(time_column_suffix)){
    phases <- phases %>%
      rename_with(~paste0(.,"_" , time_column_suffix), .cols = !all_of(columns))
  }

return(phases)

}





#' Procedure Times Diagnostic Plot
#'
#' Generate density and box plots with statistical overlays for a given time variable.
#'
#' @param phases Data frame containing procedure phase data.
#' @param column_string Character. Name of the column to analyze.
#' @param width_limit Numeric. Optional threshold line to display on the density plot.
#'
#' @return A patchwork plot combining density and box plots with quartile and SD annotations.
#'
#' @importFrom rlang .data
#' @export
calculate_procedure_times_diagnostics_plot <- function(phases = phases,
                                                       column_string,
                                                       width_limit
                                                       ){

  phases <- phases %>%
    dplyr::mutate(!!column_string := as.numeric(.data[[column_string]], units = "mins"))



    # 1. Calculate descriptive statistics for precise annotation
  col_vector <- phases %>% dplyr::pull(.data[[column_string]])
  max_dens   <- max(density(as.numeric(col_vector), na.rm = TRUE)$y)

  # 2. Compute Summary Metrics using Tidy Evaluation
  quants <- phases %>%
    dplyr::summarise(
      Q1         = quantile(.data[[column_string]], 0.25, na.rm = TRUE),
      Med        = median(.data[[column_string]], na.rm = TRUE),
      Q3         = quantile(.data[[column_string]], 0.75, na.rm = TRUE),
      mean_val   = mean(.data[[column_string]], na.rm = TRUE),
      .sd        = sd(.data[[column_string]], na.rm = TRUE),
      SD1_upper  = mean_val + (1 * .sd),
      SD2_upper  = mean_val + (2 * .sd),
      SD3_upper  = mean_val + (3 * .sd)
    )

  sub_text <- paste0(
    "Mean: ", signif(quants$mean_val, 3),
    " | Median: ", signif(quants$Med, 3),
    " | +1 SD: ", signif(quants$SD1_upper, 3),
    " | +2 SD: ", signif(quants$SD2_upper, 3),
    " | +3 SD: ", signif(quants$SD3_upper, 3)
  )
    # 2. Construct the Density Plot with Quartile Overlays
    p_density <- ggplot(phases, aes(x = .data[[column_string]])) +
      geom_density(fill = "steelblue", alpha = 0.3) +
      geom_vline(xintercept = c(quants$Q1, quants$Med, quants$Q3),
                 color = "firebrick", linetype = "dashed", linewidth = 0.6) +
      annotate("text",
               x = c(quants$Q1, quants$Med, quants$Q3),
               y = rep(0, 3),
               label = c(paste("Q1:", round(quants$Q1, 2)),
                         paste("Med:", round(quants$Med, 2)),
                         paste("Q3:", round(quants$Q3, 2))),
               vjust = -0.5, hjust = -0.1, angle = 90, color = "firebrick", size = 3.2) +
      # --- MEAN & SD ANNOTATIONS (Parametric) ---
      geom_vline(xintercept = c(quants$mean_val, quants$SD1_upper, quants$SD2_upper, quants$SD3_upper),
                 color = "darkblue", linetype = "dashed", linewidth = 0.5) +
      annotate("text",
               x = c(quants$mean_val, quants$SD1_upper, quants$SD2_upper, quants$SD3_upper),
               # Positioned slightly higher on the y-axis to prevent label collisions if values are close
               y = rep(0, 4),
               label = c(paste("Mean:", round(quants$mean_val, 2)),
                         paste("+1 SD:", round(quants$SD1_upper, 2)),
                         paste("+2 SD:", round(quants$SD2_upper, 2)),
                         paste("+3 SD:", round(quants$SD3_upper, 2))),
               vjust = -0.5, hjust = -0.1, angle = 90, color = "darkblue", size = 3.2)+
      labs(title = paste0("Distribution Analysis of column ",column_string) ,subtitle = sub_text, y = "Density", x = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
      xlim(c(0, quants$Med*5))

    # 3. Construct the Corresponding Box and Whisker Plot
    p_box <- ggplot(phases, aes(x = .data[[column_string]], y = "")) +
      geom_boxplot(fill = "grey90", outlier.color = "firebrick", outlier.alpha = 0.6, outlier.shape = 16) +
      labs( y = NULL, x = "Width of min to max time (mins)") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
      xlim(c(0, quants$Med*5))

    if(!missing(width_limit)){
      p_density <- p_density +
        geom_vline(xintercept = width_limit, color = "green", linetype = "dashed", linewidth = 0.5) +
        annotate("text",
                 x = width_limit,
                 # Positioned slightly higher on the y-axis to prevent label collisions if values are close
                 y = rep(0, 4),
                 label = paste0(column_string, "_limit set at ", width_limit),
                 vjust = -0.5, hjust = -0.1, angle = 90, color = "green", size = 3.2)

    }

    # 4. Vertically align and stack plots (3:1 ratio)
    plots <- list(p_density, p_box)
    final_plot <- patchwork::wrap_plots(plots, heights = c(3, 1), ncol=1, nrow(2))

  return(final_plot)

}
