calculate_procedure_times <- function(dataset = NA,
                                      phases = Phases,
                                      start_time_columns = c("anaesthesia_start","anaesthesia_ready", "patient_in_room", "procedure_start"),
                                      start_width_limit = 90,
                                      end_time_columns = c("procedure_end", "patient_out_of_room","anaesthesia_end", "in_recovery"),
                                      end_width_limit = 60,
                                      diagnostics = TRUE,
                                      verbose = TRUE){

  columns <- c("mrn", "admission_csn", "anaesthesia_csn")

  # Rename only Phases.anaesthesia_end_derived on ingest, so the name
  # `anaesthesia_end_IMPACT` is free for our pattern-validated output.
  # The raw documented Phases column stays as `anaesthesia_end`.

  if(!is.na(dataset)){
    phases <- phases %>% filter(admission_csn %in% pull(dataset, admission_csn))
    if(verbose){cli::cli_alert_info("{.arg dataset} provided, proccesing {.arg phases} for only these patients")}
  }

  phases <- phases %>%
    dplyr::select(dplyr::all_of(c(columns, start_time_columns, end_time_columns))) %>%
    dplyr::collect()

  phases <- phases %>%
    head(n = 1000) %>%
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

    cli::cli_alert_success("Returning the diagnostic plots. See {.fn ?calculate_procedure_times} for for  ")
   result <- list(calculate_procedure_times_diagnostics_plot(phases, "start_width"),
    calculate_procedure_times_diagnostics_plot(phases, "end_width"))

   return(result)
  }

  phases <- phases %>%
    mutate(start_width_acceptable = ifelse(start_width <= as.period(start_width_limit, unit = "mins"), TRUE, FALSE),
           end_width_acceptable = ifelse(end_width <= as.period(end_width_limit, unit = "mins"), TRUE, FALSE),)

  ## To do to edit and review this to add the end time then return the rable

  phases <- phases %>% rowwise() %>%
    mutate(
      start_time = case_when(
        start_width_acceptable  ~ reduce(pick(all_of(start_time_columns)), coalesce),
        !start_width_acceptable ~ median(c_across(all_of(start_time_columns)), na.rm = TRUE),
        .default                = NA_POSIXct_
      )
    ) %>% ungroup()



### To do - Develop out this piece with an intelligent aspect to remove the outlier and then recalculate the median.

  phases %>% filter(!start_width_acceptable) %>%
    mutate(
      across(
        all_of(start_time_columns),
        ~ if_else(
          abs(as.numeric(difftime(.x, start_time, units = "mins"))) > start_width_limit,
          as.POSIXct(NA), # Ensure the NA injected matches the <dttm> class
          .x
        )
      )
    ) %>% View()

}






calculate_procedure_times_diagnostics_plot <- function(phases = phases,
                                                       column_string
                                                       ){

  phases <- phases %>%
    dplyr::mutate(!!column_string := as.numeric(.data[[column_string]], units = "mins"))


    # Run diagnostics to help tune function
    library(patchwork) # Crucial for clean, aligned multi-plot layouts

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

    # 4. Vertically align and stack plots (3:1 ratio)
    plot <- p_density / p_box + plot_layout(heights = c(3, 1))

  return(plot)

}
