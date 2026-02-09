# app.R
# AGRUPA · Cordialidad–Competencia (pipeline propio + descripciones ES)
# Sin ggiraph/plotly: hipervínculos por (a) clic/hover sobre puntos, (b) icono en selector, (c) enlace en tabla.
# Cambios clave:
#  - Cálculo bajo demanda con botón "Calcular"
#  - Ejes X/Y seleccionables entre todas las columnas dirmean_*
#  - Guards para evitar error de "nombre de variable de longitud cero" cuando x_dim/y_dim están vacíos


suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(scales)
  library(readr)
  library(htmltools)
  
  library(udpipe)
  library(stopwords)
})

# ============================================================
# ==========  IMPORTANTE: cargar funciones del proyecto  ======
# ============================================================
source("./../R/Data.R", encoding = "UTF-8")
source("./../R/Dictionary.R", encoding = "UTF-8")

req_fun <- c("prepare_descriptors", "dict_coverage", "dict_dim_coverage_all", "dict_dim_dirmean_all")
missing_fun <- req_fun[!vapply(req_fun, exists, logical(1))]
if (length(missing_fun) > 0) {
  stop(
    "Faltan funciones del proyecto: ", paste(missing_fun, collapse = ", "),
    "\nAsegúrate de hacer source() de los scripts correspondientes antes de lanzar la app."
  )
}

# ============================================================
# ==========  MODELO UDPipe (ES)  =============================
# ============================================================
if (!exists("ud_model")) {
  model_path <- Sys.getenv("UDPIPE_ES_MODEL", unset = NA_character_)
  if (!is.na(model_path) && nzchar(model_path) && file.exists(model_path)) {
    ud_model <- udpipe::udpipe_load_model(model_path)
  } else {
    stop(
      "No encuentro 'ud_model'.\n",
      "Opciones:\n",
      "  1) Crea ud_model en tu sesión antes de ejecutar la app.\n",
      "  2) Define UDPIPE_ES_MODEL con la ruta al modelo y reinicia.\n"
    )
  }
}

# ---- Datos base (títulos ES; descripciones ES) ----
artworks_all <- tribble(
  ~id, ~title, ~text, ~img_url,
  
  "las_meninas", "Las meninas (Velázquez, 1656)",
  "En un amplio estudio de la corte, la joven infanta está en el centro mientras unas damas de honor atentas,
   una enana de la corte y un perro tranquilo la rodean. A la izquierda, un pintor se detiene ante un gran lienzo
   y mira hacia fuera. La luz que entra por la ventana derecha ilumina los rostros y el suelo pulcro, mientras un espejo
   y una puerta abierta dejan ver figuras al fondo. La escena transmite etiqueta disciplinada, ayuda discreta
   y cooperación serena y contenida.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Las_Meninas_01.jpg/250px-Las_Meninas_01.jpg",
  
  "rendicion_breda", "La rendición de Breda (Velázquez, 1634–35)",
  "En un campo de batalla, el comandante vencedor extiende la mano con un gesto cortés mientras el líder derrotado
   ofrece las llaves de la ciudad. Los soldados se alinean con largas lanzas y estandartes; los oficiales mantienen el orden
   y muestran contención y respeto en lugar de crueldad. Los rostros están calmados, el intercambio es digno,
   y las filas disciplinadas subrayan un mando capaz y controlado.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4e/Vel%C3%A1zquez_-_de_Breda_o_Las_Lanzas_%28Museo_del_Prado%2C_1634-35%29.jpg/330px-Vel%C3%A1zquez_-_de_Breda_o_Las_Lanzas_%28Museo_del_Prado%2C_1634-35%29.jpg",
  
  "familia_carlos_iv", "La familia de Carlos IV (Goya, 1800)",
  "Los miembros de la familia real posan con vestimenta formal, con el rey y la reina en el centro.
   Su postura es ceremonial; asistentes y niños se agrupan alrededor. El conjunto proyecta estatus, riqueza y autoridad,
   mientras las expresiones se mantienen reservadas y socialmente distantes, más oficiales que afectuosas.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/7/74/La_familia_de_Carlos_IV%2C_por_Francisco_de_Goya.jpg/500px-La_familia_de_Carlos_IV%2C_por_Francisco_de_Goya.jpg",
  
  "escuela_atenas", "La escuela de Atenas (Rafael, 1509–11)",
  "Filósofos y estudiantes conversan, enseñan y demuestran ideas en una gran sala arquitectónica.
   Las figuras centrales caminan juntas y gesticulan mientras explican; otras escuchan, escriben y observan con atención.
   La escena destaca maestría intelectual, aprendizaje y cooperación tranquila entre estudiosos respetados.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/3/31/La_scuola_di_Atene.jpg/330px-La_scuola_di_Atene.jpg",
  
  "juramento_horacios", "El juramento de los Horacios (David, 1784)",
  "Un padre alza tres espadas mientras sus hijos extienden los brazos en un juramento solemne de deber.
   Los hombres muestran disciplina, determinación y obediencia al honor cívico; cerca, las mujeres aparecen angustiadas y llorosas.
   El ambiente valora la firmeza y la virtud pública por encima de la ternura o el consuelo.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Le_Serment_des_Horaces_-_Jacques-Louis_David_-_Mus%C3%A9e_du_Louvre_Peintures_INV_3692_%3B_MR_1432.jpg/330px-Le_Serment_des_Horaces_-_Jacques-Louis_David_-_Mus%C3%A9e_du_Louvre_Peintures_INV_3692_%3B_MR_1432.jpg",
  
  "libertad_pueblo", "La libertad guiando al pueblo (Delacroix, 1830)",
  "Una figura alegórica alza la bandera y guía a los ciudadanos sobre una barricada. Hombres armados se agrupan a su alrededor,
   animándose con un movimiento valiente y urgente. La escena mezcla solidaridad y propósito compartido con acción decidida
   bajo peligro, mientras heridos y muertos marcan el coste de la lucha.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/0/02/La_Libert%C3%A9_guidant_le_peuple_-_Eug%C3%A8ne_Delacroix_-_Mus%C3%A9e_du_Louvre_Peintures_RF_129_-_apr%C3%A9s_restauration_2024.jpg/330px-La_Libert%C3%A9_guidant_le_peuple_-_Eug%C3%A8ne_Delacroix_-_Mus%C3%A9e_du_Louvre_Peintures_RF_129_-_apr%C3%A9s_restauration_2024.jpg",
  
  "ronda_noche", "La ronda de noche (Rembrandt, 1642)",
  "Un capitán dirige a su compañía mientras el teniente escucha y señala; un tambor marca el ritmo.
   La milicia se reúne, revisa armas y se prepara para marchar. El grupo muestra coordinación y servicio público,
   con gestos ágiles y eficientes más que calidez íntima.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3a/La_ronda_de_noche%2C_por_Rembrandt_van_Rijn.jpg/330px-La_ronda_de_noche%2C_por_Rembrandt_van_Rijn.jpg",
  
  "tres_de_mayo_1808", "El 3 de mayo de 1808 (Goya, 1814)",
  "Un pelotón de fusilamiento apunta a civiles desarmados. Un hombre de blanco levanta los brazos mientras otros suplican, lloran o caen.
   Los soldados actúan de forma mecánica y sin piedad, mientras las víctimas despiertan compasión y conmoción.
   El contraste subraya miedo, crueldad y una súplica de misericordia hacia personas indefensas.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fd/El_Tres_de_Mayo%2C_by_Francisco_de_Goya%2C_from_Prado_thin_black_margin.jpg/500px-El_Tres_de_Mayo%2C_by_Francisco_de_Goya%2C_from_Prado_thin_black_margin.jpg",
  
  "guernica", "Guernica (Picasso, 1937)",
  "Figuras destrozadas lloran y gritan; una madre sostiene a su hijo muerto; un combatiente caído yace con una espada rota.
   Un caballo y un toro se retuercen en agonía; una luz dura atraviesa la oscuridad como una alarma.
   Civiles sufren caos y terror sin protección, evocando duelo, horror y llamadas desesperadas de auxilio.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6f/Mural_del_Gernika.jpg/330px-Mural_del_Gernika.jpg",
  
  "bar_folies_bergeres", "Un bar en el Folies-Bergère (Manet, 1882)",
  "Una camarera mira al espectador tras botellas y copas. En el espejo, los clientes abarrotan la sala y conversan entre sí.
   La mujer parece cansada y reservada; el servicio es práctico y distante, más transaccional que cercano o afectuoso.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0d/Edouard_Manet%2C_A_Bar_at_the_Folies-Berg%C3%A8re.jpg/330px-Edouard_Manet%2C_A_Bar_at_the_Folies-Berg%C3%A8re.jpg",
  
  "almuerzo_remeros", "El almuerzo de los remeros (Renoir, 1881)",
  "Un grupo de amistades descansa en una terraza junto al río, hablando, riendo y acercándose unas a otras.
   Ropa informal, vino y gestos juguetones sugieren ocio y compañerismo. Se intercambian sonrisas y atención,
   creando una atmósfera cálida y sociable con una competencia modesta en juego.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8d/Pierre-Auguste_Renoir_-_Luncheon_of_the_Boating_Party_-_Google_Art_Project.jpg/330px-Pierre-Auguste_Renoir_-_Luncheon_of_the_Boating_Party_-_Google_Art_Project.jpg",
  
  "judith_holofernes", "Judith decapitando a Holofernes (Artemisia Gentileschi, 1612–13)",
  "En una habitación oscura, Judith y su criada sujetan al general y lo decapitan con decisión con una espada afilada.
   Actúan coordinadas con fuerza, valentía y determinación; el hombre se resiste y sangra.
   La acción muestra agencia y competencia más que ternura o misericordia.",
  "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4e/Artemisia_Gentileschi_-_Judith_Beheading_Holofernes_-_WGA8563.jpg/250px-Artemisia_Gentileschi_-_Judith_Beheading_Holofernes_-_WGA8563.jpg",
  
  "dos_fridas", "Las dos Fridas (Frida Kahlo, 1939)",
  "Dos mujeres se sientan juntas y se dan la mano; una vena compartida conecta dos corazones expuestos.
   Una viste a la europea, la otra con indumentaria tradicional. A pesar del dolor y la sangre, miran al frente con calma,
   apoyo mutuo, empatía y solidaridad, expresando cuidado y conexión más que agresión.",
  "https://historia-arte.com/_/eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpbSI6WyJcL2FydHdvcmtcL2ltYWdlRmlsZVwvNWQ3YjdjOGI3NzczOS5qcGVnIiwicmVzaXplLDIwMDAsMjAwMCJdfQ.QfgFpqlwgF9Mh9oBKGq_q1PNq4mjsOmHUqcDyzcWcRU.jpeg"
)

# ---- utilidades ----
clip11 <- function(x) pmin(pmax(x, -1), 1)

.pick_col <- function(df, candidates) {
  for (nm in candidates) if (nm %in% names(df)) return(nm)
  NULL
}
.to_prop01 <- function(x) {
  if (all(is.na(x))) return(x)
  mx <- suppressWarnings(max(x, na.rm = TRUE))
  if (is.finite(mx) && mx > 1) x / 100 else x
}

pretty_dim <- function(col) {
  nm <- sub("^dirmean_", "", col)
  nm <- dplyr::recode(
    nm,
    "Warmth" = "Cordialidad",
    "Competence" = "Competencia",
    .default = nm
  )
  nm
}

# ============================================================
# ==========  CÁLCULO: pipeline propio (devuelve TODO)  =======
# ============================================================
compute_scm_project_full <- function(art_tbl, include_ngrams = TRUE, max_ngrams = 300) {
  
  desc_df <- prepare_descriptors(
    art_tbl$text,
    input_type       = "vector",
    input_format     = "text",
    include_ngrams   = include_ngrams,
    lemmatize        = "both",
    udpipe_model     = ud_model,
    remove_stopwords = TRUE,
    stopwords_lang   = "es",
    max_ngrams       = max_ngrams,
    text_col         = "text_proc"
  )
  
  cov_global <- dict_coverage(desc_df, prefix = "descriptor_")
  cov_dims   <- dict_dim_coverage_all(desc_df, prefix = "descriptor_")
  dir_all    <- dict_dim_dirmean_all(cov_dims, prefix = "descriptor_")
  
  out <- bind_cols(art_tbl, cov_global, dir_all)
  
  cov_col <- .pick_col(out, c("cov_pct_global", "coverage", "cov_global"))
  if (is.null(cov_col)) {
    out$coverage_prop <- NA_real_
  } else {
    out$coverage_prop <- .to_prop01(out[[cov_col]])
  }
  
  total_col  <- .pick_col(out, c("n_descriptores_fila", "tokens_total"))
  match_col  <- .pick_col(out, c("n_en_diccionario_fila", "tokens_matched"))
  
  if (is.null(total_col)) {
    desc_cols <- grep("^descriptor_", names(desc_df), value = TRUE)
    out$tokens_total <- rowSums(!is.na(desc_df[, desc_cols, drop = FALSE]) & desc_df[, desc_cols, drop = FALSE] != "")
    total_col <- "tokens_total"
  }
  if (is.null(match_col)) {
    out$tokens_matched <- NA_integer_
    match_col <- "tokens_matched"
  }
  
  out$tokens_total   <- out[[total_col]]
  out$tokens_matched <- out[[match_col]]
  
  out
}

# ============================================================
# ==========  Selector: etiquetas con icono de enlace  =========
# ============================================================
choice_names <- Map(function(title, url) {
  htmltools::HTML(
    sprintf('%s <a href="%s" target="_blank" rel="noopener noreferrer" class="link-icon" title="Abrir imagen">🔗</a>',
            title, url)
  )
}, artworks_all$title, artworks_all$img_url)
choice_names <- unname(choice_names)
choice_values <- unname(artworks_all$id)

# ---- UI ----
ui <- fluidPage(
  titlePanel("AGRUPA · Prototipo Cordialidad–Competencia (ES · pipeline propio)"),
  tags$style(HTML("
    #scatter { cursor: crosshair; }
    .link-icon { margin-left: 6px; text-decoration: none; }
    .hover-bubble {
      position: absolute; z-index: 10;
      background: rgba(255,255,255,0.96); border: 1px solid #ccc; border-radius: 6px;
      padding: 4px 8px; font-size: 12px; box-shadow: 0 1px 4px rgba(0,0,0,0.15);
      white-space: nowrap;
    }
  ")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('open-url', function(url) { window.open(url, '_blank'); });
    $(document).on('click', '#sel_ids a.link-icon', function(e){ e.stopPropagation(); });
  ")),
  sidebarLayout(
    sidebarPanel(
      style = "font-size:90%;",
      width = 3,
      checkboxGroupInput(
        inputId = "sel_ids", label = "Selecciona obras:",
        choiceNames  = choice_names,
        choiceValues = choice_values,
        selected     = choice_values
      ),
      hr(),
      radioButtons(
        "features",
        "Representación léxica:",
        choices = c("Solo unigramas" = "uni",
                    "Unigramas + bi/tri-gramas" = "ng"),
        selected = "ng"
      ),
      numericInput("max_ngrams", "Máximo de n-gramas (si aplica):", value = 300, min = 0, step = 50),
      hr(),
      actionButton("calc", "Calcular", class = "btn-primary"),
      tags$div(style = "margin-top:8px; font-size: 90%; color: #555;",
               textOutput("calc_info")),
      hr(),
      selectInput("x_dim", "Eje X:", choices = c("(pulsa Calcular primero)" = ""), selected = ""),
      selectInput("y_dim", "Eje Y:", choices = c("(pulsa Calcular primero)" = ""), selected = ""),
      hr(),
      sliderInput("xmax", "Límite superior eje X (margen dcha.):",
                  min = 1.05, max = 1.50, value = 1.30, step = 0.01),
      sliderInput("jitter", "Jitter (desv. típica):",
                  min = 0.00, max = 0.05, value = 0.02, step = 0.005),
      numericInput("seed", "Semilla (reproducibilidad):", value = 42, step = 1, min = 1),
      hr(),
      p(em("Tip: selecciona obras, pulsa Calcular, y luego elige dimensiones para X e Y.")),
      downloadButton("dl_csv", "Descargar tabla (CSV)"),
      downloadButton("dl_png", "Descargar gráfico (PNG)")
    ),
    mainPanel(
      div(style = "position:relative;",
          plotOutput("scatter", height = "720px",
                     click = "scatter_click",
                     hover = hoverOpts(id = "scatter_hover", delay = 50, delayType = "throttle")),
          uiOutput("hover_link")
      ),
      br(),
      h4("Tabla de resultados"),
      uiOutput("tbl")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  artworks_sel <- reactive({
    req(input$sel_ids)
    artworks_all %>% filter(id %in% input$sel_ids)
  })
  
  # ---- cálculo bajo demanda ----
  computed_all <- eventReactive(input$calc, {
    req(nrow(artworks_sel()) > 0)
    include_ngrams <- identical(input$features, "ng")
    max_ngrams <- if (include_ngrams) input$max_ngrams else 0
    
    withProgress(message = "Calculando métricas del diccionario...", value = 0, {
      incProgress(0.2, detail = "Preprocesado y extracción de descriptores")
      res <- compute_scm_project_full(artworks_sel(), include_ngrams = include_ngrams, max_ngrams = max_ngrams)
      incProgress(0.8, detail = "Cálculo de coberturas y dirmeans")
      res
    })
  }, ignoreInit = TRUE)
  
  # info de cálculo
  output$calc_info <- renderText({
    if (is.null(computed_all())) {
      "Aún no se ha calculado en esta sesión."
    } else {
      sprintf(
        "Último cálculo: %d obras | %s",
        nrow(computed_all()),
        if (identical(input$features, "uni")) "unigramas" else "unigramas + n-gramas"
      )
    }
  })
  
  # ---- actualizar dimensiones disponibles tras cálculo ----
  observeEvent(computed_all(), {
    df <- computed_all()
    
    # SOLO dirmean_* (excluye n_dirmean_* por diseño)
    dims <- grep("^dirmean_", names(df), value = TRUE)
    
    if (length(dims) == 0) {
      updateSelectInput(session, "x_dim", choices = c("No hay dirmean_* en resultados" = ""), selected = "")
      updateSelectInput(session, "y_dim", choices = c("No hay dirmean_* en resultados" = ""), selected = "")
      return()
    }
    
    dims <- dims[order(tolower(dims))]
    
    labels <- sub("^dirmean_", "", dims)
    labels <- dplyr::recode(
      labels,
      "Warmth" = "Warmth (Cordialidad)",
      "Competence" = "Competence (Competencia)",
      .default = labels
    )
    
    choices <- stats::setNames(dims, labels)
    
    x_default <- if ("dirmean_Competence" %in% dims) "dirmean_Competence" else dims[1]
    y_default <- if ("dirmean_Warmth" %in% dims) "dirmean_Warmth" else dims[min(2, length(dims))]
    
    x_sel <- if (!is.null(input$x_dim) && nzchar(input$x_dim) && input$x_dim %in% dims) input$x_dim else x_default
    y_sel <- if (!is.null(input$y_dim) && nzchar(input$y_dim) && input$y_dim %in% dims) input$y_dim else y_default
    
    updateSelectInput(session, "x_dim", choices = choices, selected = x_sel)
    updateSelectInput(session, "y_dim", choices = choices, selected = y_sel)
  }, ignoreInit = TRUE)
  
  # ---- subconjunto a mostrar (intersección con selección actual) ----
  scm_table <- reactive({
    req(computed_all())
    computed_all() %>% filter(id %in% input$sel_ids)
  })
  
  plot_df <- reactive({
    df <- scm_table()
    req(nrow(df) > 0)
    
    # Guards críticos: evita x_dim/y_dim vacíos (placeholder) y evita columnas inexistentes
    req(nzchar(input$x_dim), nzchar(input$y_dim))
    req(input$x_dim %in% names(df), input$y_dim %in% names(df))
    
    xraw <- df[[input$x_dim]]
    yraw <- df[[input$y_dim]]
    
    set.seed(input$seed)
    df %>%
      mutate(
        x_val = clip11(xraw),
        y_val = clip11(yraw),
        label = title,
        x_j   = clip11(x_val + rnorm(n(), 0, input$jitter)),
        y_j   = clip11(y_val + rnorm(n(), 0, input$jitter))
      )
  })
  
  output$scatter <- renderPlot({
    req(computed_all())
    df <- plot_df()
    
    xlim_user <- c(-0.20, input$xmax)
    ylim_user <- c(-1.05, 1.05)
    
    xlab <- pretty_dim(input$x_dim)
    ylab <- pretty_dim(input$y_dim)
    
    ggplot(df, aes(x = x_j, y = y_j)) +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey55") +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey55") +
      geom_point(aes(size = tokens_matched), alpha = 0.85) +
      scale_size_continuous(name = "Tokens emparejados", range = c(3, 8)) +
      ggrepel::geom_text_repel(
        aes(label = label),
        size = 4.2, fontface = "bold", color = "black",
        min.segment.length = 0, box.padding = 0.25, max.overlaps = Inf,
        segment.color = "grey60"
      ) +
      coord_cartesian(xlim = xlim_user, ylim = ylim_user, expand = FALSE) +
      scale_x_continuous(breaks = c(-0.5, 0, 0.5, 1.0, 1.25), name = xlab) +
      scale_y_continuous(breaks = seq(-1, 1, by = 0.5), name = ylab) +
      labs(
        title = "Obras en el espacio bidimensional (dirmean)",
        subtitle = sprintf(
          "N = %d | Textos ES | Pipeline propio | %s",
          nrow(df),
          if (identical(input$features, "uni")) "unigramas" else "unigramas + bi/tri-gramas"
        )
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position  = "right",
        plot.title       = element_text(face = "bold")
      )
  })
  
  # Hover: <a> flotante
  output$hover_link <- renderUI({
    req(computed_all())
    h <- input$scatter_hover
    req(h)
    df <- plot_df()
    
    hit <- nearPoints(df, h, xvar = "x_j", yvar = "y_j",
                      maxpoints = 1, threshold = 18, addDist = FALSE)
    if (!nrow(hit)) return(NULL)
    
    left <- h$coords_css$x + 12
    top  <- h$coords_css$y + 12
    
    tags$a(
      href = hit$img_url[1], target = "_blank", rel = "noopener noreferrer",
      class = "hover-bubble",
      style = sprintf("left:%spx; top:%spx;", left, top),
      title = "Abrir imagen",
      paste0("🔗 ", hit$title[1])
    )
  })
  
  # Click: abrir URL de la obra más cercana
  observeEvent(input$scatter_click, {
    req(computed_all())
    df <- plot_df()
    hit <- nearPoints(df, input$scatter_click, xvar = "x_j", yvar = "y_j",
                      maxpoints = 1, threshold = 15, addDist = FALSE)
    if (nrow(hit) == 1 && isTRUE(nzchar(hit$img_url[1]))) {
      session$sendCustomMessage("open-url", hit$img_url[1])
    }
  })
  
  # Tabla HTML (valores de ejes seleccionados + coverage + tokens)
  output$tbl <- renderUI({
    req(computed_all())
    df <- scm_table()
    req(nrow(df) > 0)
    
    # Guards para evitar nombre de variable de longitud cero y columnas inexistentes
    if (!nzchar(input$x_dim) || !nzchar(input$y_dim)) {
      return(tags$div(class = "alert alert-info",
                      "Pulsa «Calcular» y elige dimensiones para los ejes X e Y."))
    }
    if (!(input$x_dim %in% names(df)) || !(input$y_dim %in% names(df))) {
      return(tags$div(class = "alert alert-warning",
                      "Las dimensiones seleccionadas no están disponibles en los resultados actuales. Recalcula."))
    }
    
    xlab <- pretty_dim(input$x_dim)
    ylab <- pretty_dim(input$y_dim)
    
    # Si el usuario elige la misma dimensión en X e Y, evita nombres duplicados en la tabla
    if (identical(xlab, ylab)) {
      xlab <- paste0(xlab, " (X)")
      ylab <- paste0(ylab, " (Y)")
    }
    
    out <- df %>%
      transmute(
        id,
        title = sprintf('<a href="%s" target="_blank" rel="noopener noreferrer">%s</a>', img_url, title),
        !!xlab := sprintf("%.3f", .data[[input$x_dim]]),
        !!ylab := sprintf("%.3f", .data[[input$y_dim]]),
        coverage = percent(coverage_prop),
        tokens_matched = tokens_matched,
        tokens_total   = tokens_total
      )
    
    header <- tags$tr(lapply(names(out), tags$th))
    rows <- apply(out, 1, function(r) {
      tags$tr(HTML(paste0(
        "<td>", r[[1]], "</td>",
        "<td>", r[[2]], "</td>",
        "<td>", r[[3]], "</td>",
        "<td>", r[[4]], "</td>",
        "<td>", r[[5]], "</td>",
        "<td>", r[[6]], "</td>",
        "<td>", r[[7]], "</td>"
      )))
    })
    
    tags$table(class = "table table-striped table-sm",
               tags$thead(header),
               tags$tbody(rows))
  })
  
  # Descargas
  output$dl_csv <- downloadHandler(
    filename = function() "scm_pipeline_propio.csv",
    content  = function(file) readr::write_csv(scm_table(), file)
  )
  
  output$dl_png <- downloadHandler(
    filename = function() "scm_plot.png",
    content  = function(file) {
      req(computed_all())
      df <- plot_df()
      xlim_user <- c(-0.20, input$xmax)
      ylim_user <- c(-1.05, 1.05)
      
      xlab <- pretty_dim(input$x_dim)
      ylab <- pretty_dim(input$y_dim)
      
      p <- ggplot(df, aes(x = x_j, y = y_j)) +
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey55") +
        geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey55") +
        geom_point(aes(size = tokens_matched), alpha = 0.85) +
        scale_size_continuous(name = "Tokens emparejados", range = c(3, 8)) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 4.2, fontface = "bold", color = "black",
          min.segment.length = 0, box.padding = 0.25, max.overlaps = Inf,
          segment.color = "grey60"
        ) +
        coord_cartesian(xlim = xlim_user, ylim = ylim_user, expand = FALSE) +
        scale_x_continuous(breaks = c(-0.5, 0, 0.5, 1.0, 1.25), name = xlab) +
        scale_y_continuous(breaks = seq(-1, 1, by = 0.5), name = ylab) +
        labs(
          title = "Obras en el espacio bidimensional (dirmean)",
          subtitle = sprintf("N = %d | Textos ES | Pipeline propio | %s",
                             nrow(df),
                             if (identical(input$features, "uni")) "unigramas" else "unigramas + bi/tri-gramas")
        ) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              legend.position  = "right",
              plot.title       = element_text(face = "bold"))
      
      ggsave(file, p, width = 11, height = 8, dpi = 300)
    }
  )
}

shinyApp(ui, server)
