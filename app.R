# app.R
library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(writexl)

# ----------------------------
# Helpers
# ----------------------------

get_palette <- function(n) {
  pal <- c(
    "#1B9E77", # verde
    "#D95F02", # naranjo oscuro
    "#7570B3", # violeta
    "skyblue", # celeste
    "#E7298A", # magenta
    "#A6761D", # café
    "#1F78B4", # azul
    "#B15928"  # marrón oscuro
  )
  pal[seq_len(min(n, length(pal)))]
}

make_group_label <- function(df, group_vars) {
  if (length(group_vars) == 0) {
    return(df %>% mutate(group_label = "General"))
  }
  df %>%
    mutate(
      group_label = do.call(
        paste,
        c(across(all_of(group_vars), ~ as.character(.x)), sep = " | ")
      )
    )
}

calc_simon_rt <- function(df, subj = "subject", cond = "condition", rt = "rt_ms",
                          congruent_label = "congruent", incongruent_label = "incongruent") {
  df %>%
    filter(!is.na(.data[[subj]]), !is.na(.data[[cond]]), !is.na(.data[[rt]])) %>%
    filter(.data[[cond]] %in% c(congruent_label, incongruent_label)) %>%
    group_by(.data[[subj]], .data[[cond]]) %>%
    summarise(mean_rt = mean(.data[[rt]], na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data[[cond]], values_from = mean_rt) %>%
    rename(RT_congruent = !!congruent_label,
           RT_incongruent = !!incongruent_label) %>%
    mutate(Simon_RT = RT_incongruent - RT_congruent)
}

count_trials <- function(df, group_vars) {
  if (length(group_vars) == 0) {
    df %>%
      summarise(
        N_trials = n(),
        N_subjects = n_distinct(subject)
      )
  } else {
    df %>%
      group_by(subject, across(all_of(group_vars))) %>%
      summarise(
        N_trials = n(),
        .groups = "drop"
      )
  }
}

drop_non_trials <- function(df) {
  bad <- c("welcome", "goodbye")
  
  df %>%
    filter(
      !(tolower(as.character(condition)) %in% bad) &
        !(tolower(as.character(block)) %in% bad)
    )
}

read_csv_safely <- function(path) {
  # readr::read_csv es más robusto que base::read.csv
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

bind_rows_tolerant <- function(dfs) {
  # Une por filas conservando todas las columnas (rellena con NA si faltan)
  dplyr::bind_rows(dfs)
}

standardize_data <- function(df, mapping) {
  # mapping: list(subject_col, rt_col, correct_col, condition_col (optional), block_col (optional))
  # RT viene en segundos -> convertimos a ms
  stopifnot(!is.null(mapping$subject_col), !is.null(mapping$rt_col), !is.null(mapping$correct_col))
  
  out <- df %>%
    transmute(
      subject   = .data[[mapping$subject_col]],
      rt_ms     = as.numeric(.data[[mapping$rt_col]]) * 1000,
      correct   = as.integer(.data[[mapping$correct_col]]),
      condition = if (!is.null(mapping$condition_col) && mapping$condition_col != "(None)") .data[[mapping$condition_col]] else NA,
      block     = if (!is.null(mapping$block_col) && mapping$block_col != "(None)") .data[[mapping$block_col]] else NA,
      group2 = if (!is.null(mapping$group2_col) && mapping$group2_col != "(None)") .data[[mapping$group2_col]] else NA
    )
  
  # Sanidad mínima
  out <- out %>%
    mutate(
      subject = as.character(subject),
      correct = ifelse(correct %in% c(0, 1), correct, NA_integer_),
      rt_ms   = ifelse(is.finite(rt_ms), rt_ms, NA_real_)
    )
  
  out
}

summarize_by <- function(df, group_vars) {
  # group_vars: character vector de columnas a agrupar (puede ser vacío)
  if (length(group_vars) == 0) {
    df %>%
      summarise(
        N_subjects = n_distinct(subject),
        N_trials   = n(),
        mean_rt_ms = mean(rt_ms, na.rm = TRUE),
        sd_rt_ms   = sd(rt_ms, na.rm = TRUE),
        acc_pct    = mean(correct, na.rm = TRUE) * 100
      )
  } else {
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        N_subjects = n_distinct(subject),
        N_trials   = n(),
        mean_rt_ms = mean(rt_ms, na.rm = TRUE),
        sd_rt_ms   = sd(rt_ms, na.rm = TRUE),
        acc_pct    = mean(correct, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  }
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  titlePanel("Visualizador de Pilotajes (CSV) — RT/Accuracy"),
  
  tabsetPanel(
    tabPanel("1) Cargar",
             fluidRow(
               column(6,
                      fileInput("files", "Selecciona uno o varios CSV", multiple = TRUE, accept = c(".csv")),
                      checkboxInput("keep_all_cols", "Mantener todas las columnas (tolerante)", value = TRUE),
                      helpText("Tip: Puedes cargar varios CSV; se unirán por filas.")
               ),
               column(6,
                      verbatimTextOutput("load_status")
               )
             ),
             hr(),
             DTOutput("preview_raw")
    ),
    
    tabPanel("2) Mapear columnas",
             fluidRow(
               column(6,
                      uiOutput("preset_ui"),
                      uiOutput("map_ui"),
                      actionButton("confirm_mapping", "Confirmar mapeo", class = "btn-primary"),
                      br(), br(),
                      checkboxInput("save_mapping", "Guardar este mapeo como preset en esta sesión", value = TRUE),
                      textInput("mapping_name", "Nombre del preset (opcional)", value = "psychopy_default")
               ),
               column(6,
                      h4("Notas"),
                      tags$ul(
                        tags$li("RT se asume en segundos y se convertirá a milisegundos automáticamente."),
                        tags$li("Correcto se asume codificado como 1/0."),
                        tags$li("Condition y Block son opcionales: si no los mapeas, puedes agrupar igual por nada o solo por sujeto.")
                      ),
                      verbatimTextOutput("mapping_status")
               )
             )
    ),
    
    tabPanel("3) Visualizar y exportar",
             fluidRow(
               column(4,
                      h4("Filtros"),
                      sliderInput("rt_min", "RT mínimo (ms)", min = 0, max = 3000, value = 100, step = 10),
                      sliderInput("rt_max", "RT máximo (ms)", min = 0, max = 6000, value = 1500, step = 10),
                      checkboxInput("drop_incorrect", "Excluir ensayos incorrectos", value = TRUE),
                      hr(),
                      h4("Agrupar por"),
                      checkboxGroupInput(
                        "group_by",
                        label = NULL,
                        choices = c("condition" = "condition", "block" = "block", "group2" = "group2"),
                        selected = c("condition")
                      ),
                      hr(),
                      downloadButton("download_summary_xlsx", "Descargar resumen (XLSX)"),
                      downloadButton("download_raw_xlsx", "Descargar data RAW (XLSX)"),
                      downloadButton("download_clean", "Descargar data limpia (XLSX)")
               ),
               column(8,
                      h2("Tabla Resumen RTs & Acc"),
                      DTOutput("summary_table"),
                      
                      fluidRow(
                        column(6,
                               h2("Gráfico RTs (ms)"),
                               plotlyOutput("rt_box", height = "320px")
                        ),
                        column(6,
                               h2("Gráfico Accuracy (%)"),
                               plotlyOutput("acc_bar", height = "320px")
                        )
                      ),
                      
                      h2("Tabla Resumen RTs & Acc por Participante"),
                      DTOutput("by_subject_table"),
                      h2("Gráfico RTs por Participante"),
                      plotOutput("rt_by_subject_static", height = "420px"),
                      h2("Gráfico Accuracy por Participante"),
                      plotOutput("acc_by_subject_static", height = "420px"),
                      h2("Simon effect (Incongruent – Congruent)"),
                      h4("Tabla por participante"),
                      DTOutput("tbl_simon_rt_subject"),
                      br(),
                      h4("Resumen general"),
                      DTOutput("tbl_simon_rt_summary"),
                      br(),
                      h4("Gráfico Simon effect (RT)"),
                      plotlyOutput("plt_simon_rt", height = "420px")
               )
             )
    ),
    tabPanel(
      "4) Calidad de datos",
      h2("Calidad de datos y retención de ensayos"),
      
      h4("Ensayos originales (sin filtros)"),
      DTOutput("tbl_trials_raw"),
      
      br(),
      h4("Ensayos tras filtros"),
      DTOutput("tbl_trials_clean"),
      
      br(),
      h4("Resumen de retención (%)"),
      DTOutput("tbl_trials_retention")
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    raw = NULL,
    mapping = NULL,
    mapping_presets = list()
  )
  
  # ---- Load files
  observeEvent(input$files, {
    req(input$files)
    
    paths <- input$files$datapath
    dfs <- lapply(paths, read_csv_safely)
    
    raw <- if (isTRUE(input$keep_all_cols)) {
      bind_rows_tolerant(dfs)
    } else {
      # Solo columnas comunes
      common <- Reduce(intersect, lapply(dfs, names))
      bind_rows(lapply(dfs, \(x) dplyr::select(x, all_of(common))))
    }
    
    rv$raw <- raw
  })
  
  output$load_status <- renderPrint({
    if (is.null(rv$raw)) {
      cat("Sin datos cargados todavía.")
    } else {
      cat("✅ Datos cargados\n")
      cat("Filas:", nrow(rv$raw), "\n")
      cat("Columnas:", ncol(rv$raw), "\n")
      cat("Archivos:", if (!is.null(input$files)) nrow(input$files) else 0, "\n")
    }
  })
  
  output$preview_raw <- renderDT({
    req(rv$raw)
    datatable(head(rv$raw, 25), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # ---- Mapping UI (crea los selectInput primero)
  output$map_ui <- renderUI({
    req(rv$raw)
    cols <- names(rv$raw)
    
    tagList(
      selectInput("subject_col", "Columna de sujeto (obligatoria)", choices = cols),
      selectInput("rt_col", "Columna de RT en segundos (obligatoria)", choices = cols),
      selectInput("correct_col", "Columna de correcto 1/0 (obligatoria)", choices = cols),
      selectInput("condition_col", "Columna de condición (opcional)", choices = c("(None)", cols), selected = "(None)"),
      selectInput("block_col", "Columna de bloque (opcional)", choices = c("(None)", cols), selected = "(None)"),
      selectInput("group2_col", "Columna de grupo extra (opcional)", choices = c("(None)", cols), selected = "(None)")
    )
  })
  
  # ---- Selector de presets (depende de rv$mapping_presets)
  output$preset_ui <- renderUI({
    if (length(rv$mapping_presets) == 0) return(NULL)
    
    selectInput(
      "preset_select",
      "Cargar preset",
      choices = names(rv$mapping_presets),
      selected = NULL
    )
  })
  
  # ---- Aplicar preset (requiere que existan los selectInput)
  observeEvent(input$preset_select, {
    req(input$preset_select, rv$raw)  # rv$raw asegura que map_ui ya puede renderizar
    
    preset <- rv$mapping_presets[[input$preset_select]]
    req(preset)
    
    updateSelectInput(session, "subject_col",   selected = preset$subject_col)
    updateSelectInput(session, "rt_col",        selected = preset$rt_col)
    updateSelectInput(session, "correct_col",   selected = preset$correct_col)
    updateSelectInput(session, "condition_col", selected = preset$condition_col)
    updateSelectInput(session, "block_col",     selected = preset$block_col)
    updateSelectInput(session, "group2_col", selected = preset$group2_col)
    
    rv$mapping <- preset
  })
  
  # ---- Confirm mapping (guarda preset)
  observeEvent(input$confirm_mapping, {
    req(rv$raw, input$subject_col, input$rt_col, input$correct_col)
    
    rv$mapping <- list(
      subject_col = input$subject_col,
      rt_col = input$rt_col,
      correct_col = input$correct_col,
      condition_col = input$condition_col,
      block_col = input$block_col,
      group2_col = input$group2_col
    )
    
    if (isTRUE(input$save_mapping)) {
      nm <- input$mapping_name
      if (is.null(nm) || nm == "") nm <- "psychopy_default"
      rv$mapping_presets[[nm]] <- rv$mapping
    }
  })
  
  
  # ---- Standardized data
  standardized <- reactive({
    req(rv$raw, rv$mapping)
    standardize_data(rv$raw, rv$mapping)
  })
  
  standardized_trials <- reactive({
    req(standardized())
    drop_non_trials(standardized())
  })
  
  cleaned <- reactive({
    df <- standardized_trials()   # en vez de standardized()
    
    df <- df %>%
      filter(rt_ms >= input$rt_min, rt_ms <= input$rt_max)
    
    if (isTRUE(input$drop_incorrect)) {
      df <- df %>% filter(correct == 1)
    }
    
    df
  })
  
  # ---- Plots and tables
  output$rt_box <- renderPlotly({
    req(cleaned())
    df <- cleaned()
    
    group_vars <- input$group_by
    
    df <- make_group_label(df, group_vars)
    
    n_groups <- nlevels(factor(df$group_label))
    
    p <- ggplot(df, aes(x = group_label, y = rt_ms, fill = group_label)) +
      geom_boxplot(outlier.alpha = 0.5, alpha = 0.65) +
      stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white") +
      labs(x = "Grupo", y = "RT (ms)", title = "RT (ms)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1)
      ) +
      scale_fill_manual(values = get_palette(n_groups))
    
    ggplotly(p) %>% layout(hovermode = "closest")
  })
  
  output$summary_table <- renderDT({
    req(cleaned())
    df <- cleaned()
    
    sum_df <- summarize_by(df, input$group_by)
    
    datatable(sum_df, options = list(scrollX = TRUE, pageLength = 10)) %>%
      formatRound(columns = c("mean_rt_ms", "sd_rt_ms"), digits = 0) %>%
      formatRound(columns = c("acc_pct"), digits = 1)
  })
  
  # output$by_subject_table <- renderDT({
  #   req(cleaned())
  #   df <- cleaned()
  #   
  #   by_subj <- df %>%
  #     group_by(subject) %>%
  #   summarise(
  #     N_trials = n(),
  #     mean_rt_ms = mean(rt_ms, na.rm = TRUE),
  #     sd_rt_ms   = sd(rt_ms, na.rm = TRUE),
  #     acc_pct    = mean(correct, na.rm = TRUE) * 100,
  #     .groups = "drop"
  #   ) %>%
  #     arrange(mean_rt_ms)
  #   
  #   datatable(by_subj, options = list(scrollX = TRUE, pageLength = 10)) %>%
  #     formatRound(columns = c("mean_rt_ms", "sd_rt_ms"), digits = 0) %>%
  #     formatRound(columns = c("acc_pct"), digits = 1)
  # })
  
  output$by_subject_table <- renderDT({
    req(cleaned(), standardized())
    
    df_rt  <- cleaned()  # ya aplica rt_min/max y (opcional) drop_incorrect
    df_acc <- standardized() %>% filter(rt_ms >= input$rt_min, rt_ms <= input$rt_max)  # NO excluye incorrectos
    
    group_vars <- input$group_by
    
    # RT summary (puede ser con o sin incorrectos según tu checkbox)
    by_subj_rt <- df_rt %>%
      {
        if (length(group_vars) == 0) group_by(., subject)
        else group_by(., subject, across(all_of(group_vars)))
      } %>%
      summarise(
        N_trials = n(),
        mean_rt_ms = mean(rt_ms, na.rm = TRUE),
        sd_rt_ms   = sd(rt_ms, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Accuracy summary (siempre basado en todos los ensayos en rango RT)
    by_subj_acc <- df_acc %>%
      {
        if (length(group_vars) == 0) group_by(., subject)
        else group_by(., subject, across(all_of(group_vars)))
      } %>%
      summarise(
        acc_pct = mean(correct, na.rm = TRUE) * 100,
        .groups = "drop"
      )
    
    # Join RT + acc
    by_subj <- by_subj_rt %>%
      left_join(by_subj_acc, by = c("subject", group_vars)) %>%
      arrange(mean_rt_ms)
    
    datatable(by_subj, options = list(scrollX = TRUE, pageLength = 10)) %>%
      formatRound(columns = c("mean_rt_ms", "sd_rt_ms"), digits = 0) %>%
      formatRound(columns = c("acc_pct"), digits = 1)
  })
  
  
  # output$acc_bar <- renderPlotly({
  #   req(standardized())
  #   df <- standardized() %>% filter(rt_ms >= input$rt_min, rt_ms <= input$rt_max)
  #   
  #   group_vars <- input$group_by
  #   if (length(group_vars) == 0) {
  #     acc_df <- df %>% summarise(group = "General", acc_pct = mean(correct, na.rm = TRUE) * 100)
  #   } else if (length(group_vars) == 1) {
  #     g <- group_vars[[1]]
  #     acc_df <- df %>%
  #       group_by(.data[[g]]) %>%
  #       summarise(acc_pct = mean(correct, na.rm = TRUE) * 100, .groups = "drop") %>%
  #       rename(group = !!g)
  #   } else {
  #     acc_df <- df %>%
  #       mutate(group = paste(condition, block, sep = " | ")) %>%
  #       group_by(group) %>%
  #       summarise(acc_pct = mean(correct, na.rm = TRUE) * 100, .groups = "drop")
  #   }
  #   
  #   pal <- get_palette(nrow(acc_df))
  #   
  #   plot_ly(
  #     acc_df,
  #     x = ~group, y = ~acc_pct,
  #     type = "bar",
  #     color = ~group,
  #     colors = pal
  #   ) %>%
  #     layout(
  #       yaxis = list(title = "Accuracy (%)", rangemode = "tozero"),
  #       xaxis = list(title = ""),
  #       showlegend = FALSE,
  #       margin = list(b = 120)
  #     )
  # })
  
  output$acc_bar <- renderPlotly({
    req(standardized())
    df <- standardized() %>%
      filter(rt_ms >= input$rt_min, rt_ms <= input$rt_max)
    
    group_vars <- input$group_by
    df <- make_group_label(df, group_vars)
    
    acc_df <- df %>%
      group_by(group_label) %>%
      summarise(acc_pct = mean(correct, na.rm = TRUE) * 100, .groups = "drop") %>%
      mutate(group_label = factor(group_label, levels = group_label))  # respeta orden actual
    
    n_groups <- nlevels(acc_df$group_label)
    
    p <- ggplot(acc_df, aes(x = group_label, y = acc_pct, fill = group_label)) +
      geom_col(alpha = 0.85) +
      scale_fill_manual(values = get_palette(n_groups)) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(y = "Accuracy (%)", x = NULL) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    ggplotly(p) %>% layout(margin = list(b = 120))
  })
  
  output$rt_by_subject_static <- renderPlot({
    req(cleaned())
    df <- cleaned()
    
    group_vars <- input$group_by
    df <- make_group_label(df, group_vars)  # crea df$group_label
    
    n_groups <- nlevels(factor(df$group_label))
    
    ggplot(df, aes(x = group_label, y = rt_ms, fill = group_label)) +
      geom_boxplot(alpha = 0.65, outlier.alpha = 0.4) +
      stat_summary(fun = mean, geom = "point", size = 2.5, shape = 21, fill = "white") +
      facet_wrap(~ subject, ncol = 3) +
      scale_fill_manual(values = get_palette(n_groups)) +
      labs(x = NULL, y = "RT (ms)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8)
      )
  })
  
  output$acc_by_subject_static <- renderPlot({
    req(standardized())
    df <- standardized() %>%
      filter(rt_ms >= input$rt_min, rt_ms <= input$rt_max)
    
    group_vars <- input$group_by
    df <- make_group_label(df, group_vars)  # crea df$group_label
    
    acc_subj <- df %>%
      group_by(subject, group_label) %>%
      summarise(acc_pct = mean(correct, na.rm = TRUE) * 100, .groups = "drop")
    
    n_groups <- nlevels(factor(acc_subj$group_label))
    
    ggplot(acc_subj, aes(x = group_label, y = acc_pct, fill = group_label)) +
      geom_col(alpha = 0.85) +
      facet_wrap(~ subject, ncol = 3) +
      scale_fill_manual(values = get_palette(n_groups)) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(x = NULL, y = "Accuracy (%)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8)
      )
  })
  
  # ---- Simon effect
  simon_rt_by_subject <- reactive({
    req(cleaned())
    df <- cleaned()
    
    validate(
      need("condition" %in% names(df), "No existe columna 'condition'. Debes mapear congruence -> condition.")
    )
    
    out <- calc_simon_rt(df)
    
    # orden tipo tu imagen (por Simon_RT)
    out %>% arrange(Simon_RT)
  })
  
  simon_rt_summary <- reactive({
    s <- simon_rt_by_subject()
    tibble(
      N_participantes = sum(!is.na(s$Simon_RT)),
      Simon_RT_media  = mean(s$Simon_RT, na.rm = TRUE),
      Simon_RT_DE     = sd(s$Simon_RT, na.rm = TRUE)
    )
  })
  
  output$tbl_simon_rt_subject <- renderDT({
    s <- simon_rt_by_subject() %>%
      transmute(
        Participant = subject,
        RT_congruent = RT_congruent,
        RT_incongruent = RT_incongruent,
        Simon_RT = Simon_RT
      )
    
    datatable(s, options = list(scrollX = TRUE, pageLength = 15)) %>%
      formatRound(columns = c("RT_congruent", "RT_incongruent", "Simon_RT"), digits = 0)
  })
  
  output$tbl_simon_rt_summary <- renderDT({
    sm <- simon_rt_summary()
    datatable(sm, options = list(dom = "t", scrollX = TRUE)) %>%
      formatRound(columns = c("Simon_RT_media", "Simon_RT_DE"), digits = 0)
  })
  
  # output$plt_simon_rt <- renderPlotly({
  #   s <- simon_rt_by_subject()
  #   
  #   plot_df <- s %>%
  #     mutate(Participant = factor(subject, levels = subject))  # respeta orden ya ordenado
  #   
  #   pal <- "#D95F02"  # naranjo oscuro (como tu gráfico)
  #   
  #   plot_ly(
  #     plot_df,
  #     x = ~Simon_RT,
  #     y = ~Participant,
  #     type = "bar",
  #     orientation = "h",
  #     marker = list(color = pal)
  #   ) %>%
  #     layout(
  #       xaxis = list(title = "Incongruent – Congruent (ms)", zeroline = TRUE),
  #       yaxis = list(title = "Participante"),
  #       margin = list(l = 90)
  #     )
  # })
  
  output$plt_simon_rt <- renderPlotly({
    s <- simon_rt_by_subject()
    
    # Respetar el orden ya calculado (importante)
    plot_df <- s %>%
      mutate(
        Participant = factor(subject, levels = subject)
      )
    
    # p <- ggplot(plot_df, aes(x = Simon_RT, y = Participant)) +
    #   geom_col(
    #     fill = "#D95F02",   # naranjo oscuro
    #     alpha = 0.85,
    #     width = 0.7
    #   ) +
    #   geom_vline(
    #     xintercept = 0,
    #     linetype = "dashed",
    #     color = "grey40"
    #   ) +
    #   labs(
    #     x = "Incongruent – Congruent (ms)",
    #     y = "Participante",
    #     title = "Simon effect RT por participante"
    #   ) +
    #   theme_minimal() +
    #   theme(
    #     plot.title = element_text(size = 14, face = "bold"),
    #     axis.title = element_text(size = 12),
    #     axis.text  = element_text(size = 11)
    #   )
    
    p <- ggplot(plot_df, aes(x = Simon_RT, y = Participant, fill = Simon_RT > 0)) +
      geom_col(alpha = 0.85, width = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#D95F02", "FALSE" = "#1B9E77")) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
      geom_text(
        aes(label = round(Simon_RT, 0)),
        hjust = ifelse(plot_df$Simon_RT > 0, -0.1, 1.1),
        size = 3
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  # ---- Conteo de ensayos
  
  trials_raw <- reactive({
    req(standardized_trials())
    
    df <- standardized_trials()
    group_vars <- input$group_by
    
    count_trials(df, group_vars)
  })
  
  
  trials_clean <- reactive({
    req(cleaned())
    
    df <- cleaned()
    group_vars <- input$group_by
    
    count_trials(df, group_vars)
  })
  
  output$tbl_trials_raw <- renderDT({
    df <- trials_raw()
    
    datatable(
      df,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$tbl_trials_clean <- renderDT({
    df <- trials_clean()
    
    datatable(
      df,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$tbl_trials_retention <- renderDT({
    raw   <- trials_raw()
    clean <- trials_clean()
    
    group_vars <- input$group_by
    
    by_cols <- c("subject", group_vars)
    
    retention <- raw %>%
      rename(N_trials_raw = N_trials) %>%
      left_join(
        clean %>% rename(N_trials_clean = N_trials),
        by = by_cols
      ) %>%
      mutate(
        N_trials_clean = ifelse(is.na(N_trials_clean), 0, N_trials_clean),
        retention_pct = (N_trials_clean / N_trials_raw) * 100
      )
    
    datatable(
      retention,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = "retention_pct", digits = 1)
  })
  
  
  # ---- Downloads
  output$download_summary_xlsx <- downloadHandler(
    filename = function() paste0("summary_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- cleaned()
      sum_df <- summarize_by(df, input$group_by) %>%
        mutate(
          mean_rt_ms = round(mean_rt_ms, 0),
          sd_rt_ms   = round(sd_rt_ms, 0),
          acc_pct    = round(acc_pct, 1)
        )
      
      # además puedes incluir por sujeto en otra hoja:
      by_subj <- df %>%
        group_by(subject) %>%
        summarise(
          N_trials = n(),
          mean_rt_ms = round(mean(rt_ms, na.rm = TRUE), 0),
          sd_rt_ms   = round(sd(rt_ms, na.rm = TRUE), 0),
          acc_pct    = round(mean(correct, na.rm = TRUE) * 100, 1),
          .groups = "drop"
        )
      
      writexl::write_xlsx(
        list(
          "summary" = sum_df,
          "by_subject" = by_subj
        ),
        path = file
      )
    }
  )
  
  output$download_raw_xlsx <- downloadHandler(
    filename = function() paste0("raw_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(rv$raw)
      writexl::write_xlsx(list("raw" = rv$raw), path = file)
    }
  )
  
  
  output$download_clean <- downloadHandler(
    filename = function() paste0("clean_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- cleaned()
      writexl::write_xlsx(df, file)
    }
  )
}

shinyApp(ui, server)

