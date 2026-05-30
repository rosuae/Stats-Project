# ======================================================================
# PROBLEMA 2: Aplicatie Shiny - Transformari de Variabile Aleatoare
# ======================================================================
# Autori: 
# Data:   Mai 2026
# ======================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(mvtnorm)     # rmvnorm() pt Normala Bidimensionala
library(plotly)
library(gganimate)
library(gifski)

# Returneaza un data.frame cu indicatorii statistici empirici (medie, var, sd, cuartile)
calculeaza_statistici <- function(valori) {
  if (length(valori) == 0) return(data.frame(Indicator = "Fără date", Valoare = NA))
  
  q <- quantile(valori, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  data.frame(
    Indicator = c("Medie empirică", "Dispersie empirică", "Deviație standard empirică", 
                  "Minim", "Cuartila 1 (Q1)", "Mediană (Q2)", "Cuartila 3 (Q3)", "Maxim"),
    Valoare = c(mean(valori, na.rm = TRUE), var(valori, na.rm = TRUE), sd(valori, na.rm = TRUE),
                min(valori, na.rm = TRUE), q[1], q[2], q[3], max(valori, na.rm = TRUE))
  )
}

# === INTERFATA UTILIZATOR ===
ui <- navbarPage(
  title = "Transformări de Variabile Aleatoare",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # TRANSFORMARI UNIDIMENSIONALE
  tabPanel(
    "Transformări Unidimensionale",
    sidebarLayout(
      sidebarPanel(
        h4("Configurare Simulare X"),
        # Alegerea repartitiei
        selectInput("dist1d", "Repartiția lui X:",
                    choices = c("Normală N(µ, σ²)" = "norm",
                                "Exponențială Exp(λ)" = "exp",
                                "Uniformă Unif(a, b)" = "unif",
                                "Gamma(α, θ)" = "gamma")),
        
        # Parametrii se schimba dinamic in functie de repartitia aleasa
        uiOutput("din_params1d"),
        
        numericInput("n1d", "Dimensiunea eșantionului (n):", value = 1000, min = 10, step = 100),
        hr(),
        h4("Configurare Transformare g(x)"),
        # Cele 5 transformari cerute
        selectInput("trans1d", "Alege transformarea g(x):",
                    choices = c("g(x) = x²" = "patrat",
                                "g(x) = |x|" = "abs",
                                "g(x) = log(x)" = "log",
                                "g(x) = eˣ" = "exp_f",
                                "g(x) = 1 / (1 + e⁻ˣ) [Sigmoid]" = "sigmoid")),
        
        br(),
        # Butonul previne actualizarea haotica la fiecare modificare de parametru
        actionButton("sim1d", "Simulează", class = "btn-primary w-100")
      ),
      
      mainPanel(
        tabsetPanel(
          # Afisarea histogramei lui X si a statisticilor empirice
          tabPanel("Variabila X", 
                   br(),
                   plotlyOutput("plotX"),          # Histograma interactiva a lui X
                   h5("Statistici Empirice X"),
                   tableOutput("statsX")),         # Tabelul cu media, dispersia, etc.
          
          # Afisarea histogramei lui Y = g(X) si a statisticilor
          tabPanel("Variabila Y = g(X)", 
                   br(),
                   uiOutput("warningLog"),
                   plotlyOutput("plotY"), 
                   h5("Statistici Empirice Y"),
                   tableOutput("statsY")),
          
          # Comparatie vizuala X vs Y + interpretare automata
          tabPanel("Comparație & Interpretare", 
                   br(),
                   plotlyOutput("plotComp1d"),
                   br(),
                   card(
                     card_header(h5("Interpretare Automată")),
                     card_body(textOutput("interpret1d"))
                   ))
        )
      )
    )
  ),
  
  # TRANSFORMARI BIDIMENSIONALE
  tabPanel(
    "Transformări Bidimensionale",
    sidebarLayout(
      sidebarPanel(
        h4("Mod Generare (X, Y)"),
        # Modul indep = X,Y independente; mvnorm = Normala Bidimensionala cu rho
        radioButtons("mode2d", "Selectează modul:",
                     choices = c("Variante independente" = "indep",
                                 "Normală bidimensională" = "mvnorm")),
        hr(),
        uiOutput("din_params2d"),
        hr(),
        # Cele 4 transformari bidimensionale
        selectInput("trans2d", "Alege transformarea h(X,Y):",
                    choices = c("h(X,Y) = X + Y" = "suma",
                                "h(X,Y) = X - Y" = "diferenta",
                                "h(X,Y) = X · Y" = "produs",
                                "h(X,Y) = √(X² + Y²)" = "radical")),

        numericInput("n2d", "Dimensiunea eșantionului (n):", value = 1000, min = 10, step = 100),
        br(),

        actionButton("sim2d", "Simulează", class = "btn-primary w-100")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Analiză Z = h(X,Y)", 
                   br(),
                   fluidRow(
                     column(6, plotlyOutput("scatter2d")),
                     column(6, plotlyOutput("histZ2d"))
                   ),
                   br(),
                   h5("Indicatori Numerici Complecși"),
                   tableOutput("stats2d")),
          
          tabPanel("Histograme Marginale (X și Y)", 
                   br(),
                   fluidRow(
                     column(6, plotOutput("histX2d")), # Histograma marginala X
                     column(6, plotOutput("histY2d")) # Histograma marginala Y
                   )),
          
          tabPanel("Studiu Corelație (ρ)", 
                   br(),
                   p("Vizualizarea efectului coeficientului de corelație în cazul unei repartiții Normale Bidimensionale:"),
                   plotlyOutput("corrStudy2d", height = "350px"),
                   br(),
                   # Buton optional pentru generarea unei animatii GIF care arata
                   # cum se schimba structura norului de puncte pe masura ce rho variaza
                   actionButton("genCorrAnim", "Generează animație ρ", class = "btn-secondary"),
                   br(), br(),
                   imageOutput("corrAnim2d")
                   )
        )
      )
    )
  )
)

# === LOGICA SERVER ===
server <- function(input, output, session) {
  
  # Parametri dinamici pt repartitia 1D aleasa
  output$din_params1d <- renderUI({
    switch(input$dist1d,
      "norm" = tagList(
        numericInput("mu", "Media (µ):", value = 0),
        numericInput("sigma", "Deviația standard (σ):", value = 1, min = 0.001, step = 0.1)
      ),
      "exp" = tagList(
        numericInput("lambda", "Rata (λ):", value = 1, min = 0.001, step = 0.1)
      ),
      "unif" = tagList(
        numericInput("unif_a", "Limita inferioară (a):", value = 0),
        numericInput("unif_b", "Limita superioară (b):", value = 1)
      ),
      "gamma" = tagList(
        numericInput("alpha", "Forma (α):", value = 2, min = 0.001),
        numericInput("theta", "Scala (θ):", value = 1, min = 0.001)
      )
    )
  })
  
  # Simulare 1D se activeaza doar la apasarea butonului
  data1d <- eventReactive(input$sim1d, {
    n <- input$n1d
    dist <- input$dist1d
    trans <- input$trans1d
    
    # Validari
    if (dist == "norm") {
      validate(need(input$sigma > 0, "Eroare: Deviația standard (σ) trebuie să fie strict pozitivă!"))
    } else if (dist == "exp") {
      validate(need(input$lambda > 0, "Eroare: Parametrul λ trebuie să fie strict pozitiv!"))
    } else if (dist == "unif") {
      validate(need(input$unif_a < input$unif_b, "Eroare: Limita 'a' trebuie să fie strict mai mică decât 'b'!"))
    } else if (dist == "gamma") {
      validate(need(input$alpha > 0 && input$theta > 0, "Eroare: Parametrii α și θ trebuie să fie strict pozitivi!"))
    }
    
    # Generare esantion X din repartitia aleasa
    x <- switch(dist,
      "norm"  = rnorm(n, mean = input$mu, sd = input$sigma),
      "exp"   = rexp(n, rate = input$lambda),
      "unif"  = runif(n, min = input$unif_a, max = input$unif_b),
      "gamma" = rgamma(n, shape = input$alpha, scale = input$theta)
    )
    
    # Tratare log(x) pt valori
    warn_msg <- NULL
    x_filtered <- x
    
    if (trans == "log") {
      # Numaram cate valori sunt <= 0
      ilegale <- sum(x <= 0)
      if (ilegale > 0) {
        warn_msg <- paste("Avertisment: Au fost identificate și eliminate", ilegale, 
                          "valori <= 0 incompatibile cu transformarea logaritmică.")
        x_filtered <- x[x > 0]
      }
    }
    
    # Aplicare transformare g(x)
    y <- switch(trans,
      "patrat"  = x_filtered^2,
      "abs"     = abs(x_filtered),
      "log"     = log(x_filtered),
      "exp_f"   = exp(x_filtered),
      "sigmoid" = 1 / (1 + exp(-x_filtered))
    )
    
    if (!is.null(warn_msg)) {
      showNotification(warn_msg, type = "warning")
    }
    showNotification("Simulare 1D completă", type = "message")

    list(x = x, x_filtered = x_filtered, y = y, warn_msg = warn_msg, dist = dist, trans = trans)
  })
  
  # Avertizare vizuala pt log pe valori negative
  output$warningLog <- renderUI({
    res <- data1d()
    if (!is.null(res$warn_msg)) {
      div(class = "alert alert-warning", res$warn_msg)
    }
  })
  
  # Histograma X cu densitate teoretica suprapusa
  output$plotX <- renderPlotly({
    res <- data1d()
    df <- data.frame(x = res$x)
    p <- ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#34495e", color = "white", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Histograma lui X și Densitatea Teoretică", x = "X", y = "Densitate")

    # Suprapunere densitate teoretica
    if (res$dist == "norm") {
      p <- p + stat_function(fun = dnorm, args = list(mean = input$mu, sd = input$sigma), color = "#e74c3c", linewidth = 1)
    } else if (res$dist == "exp") {
      p <- p + stat_function(fun = dexp, args = list(rate = input$lambda), color = "#e74c3c", linewidth = 1)
    } else if (res$dist == "unif") {
      p <- p + stat_function(fun = dunif, args = list(min = input$unif_a, max = input$unif_b), color = "#e74c3c", linewidth = 1)
    } else if (res$dist == "gamma") {
      p <- p + stat_function(fun = dgamma, args = list(shape = input$alpha, scale = input$theta), color = "#e74c3c", linewidth = 1)
    }
    ggplotly(p)
  })
  
  # Histograma Y = g(X)
  output$plotY <- renderPlotly({
    res <- data1d()
    df <- data.frame(y = res$y)
    p <- ggplot(df, aes(x = y)) +
      geom_histogram(bins = 30, fill = "#1abc9c", color = "white", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Histograma variabilei transformate Y = g(X)", x = "Y", y = "Frecvență")
    ggplotly(p)
  })
  
  # Comparatie vizuala X vs Y
  output$plotComp1d <- renderPlotly({
    res <- data1d()
    dfX <- data.frame(Valoare = res$x_filtered, Variabila = "X")
    dfY <- data.frame(Valoare = res$y, Variabila = "Y")
    df_tot <- rbind(dfX, dfY)

    p <- ggplot(df_tot, aes(x = Valoare, fill = Variabila)) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "white") +
      facet_wrap(~Variabila, scales = "free") +
      scale_fill_manual(values = c("X" = "#34495e", "Y" = "#1abc9c")) +
      theme_minimal() +
      labs(title = "Comparație Vizuală Directă (Histograme Facetate)", x = "Valori", y = "Frecvență")
    ggplotly(p)
  })
  
  output$statsX <- renderTable({ calculeaza_statistici(data1d()$x) }, digits = 4)
  output$statsY <- renderTable({ calculeaza_statistici(data1d()$y) }, digits = 4)
  
  # Interpretare automata a efectului transformarii
  output$interpret1d <- renderText({
    res <- data1d()
    trans <- res$trans
    
    txt <- switch(trans,
      "patrat"  = "Transformarea pătratică g(x)=x² a modificat simetria distribuției inițiale, producând valori strict pozitive și accentuând puternic valorile extreme din coada superioară.",
      "abs"     = "Transformarea modul g(x)=|x| a pliat valorile negative peste axa pozitivă, producând o distribuție strict pozitivă și eliminând asimetria indusă de semne.",
      "log"     = "Transformarea logaritmică g(x)=log(x) a comprimat valorile mari și a eliminat complet valorile non-pozitive, generând o distribuție puternic asimetrică spre stânga.",
      "exp_f"   = "Transformarea exponențială g(x)=eˣ a accentuat extrem de agresiv valorile mari, întinzând distribuția la ordin de mărime exponențial și producând asimetrie masivă la dreapta.",
      "sigmoid" = "Transformarea sigmoidă a comprimat întregul suport într-un interval strict mărginit (0, 1), transformând cozile lungi în concentrări aproape de asimptote."
    )
    txt
  })
  
  # Parametri dinamici pt modul 2D (indep sau Normal Bidimensional)
  output$din_params2d <- renderUI({
    if (input$mode2d == "indep") {
      tagList(
        p(strong("Configurare X independentă:")),
        selectInput("distX2d", "Repartiție X:", choices = c("Normală N(0,1)" = "norm", "Exponențială Exp(1)" = "exp")),
        p(strong("Configurare Y independentă:")),
        selectInput("distY2d", "Repartiție Y:", choices = c("Normală N(0,1)" = "norm", "Exponențială Exp(1)" = "exp"))
      )
    } else {
      tagList(
        numericInput("muX", "Media µX:", value = 0),
        numericInput("muY", "Media µY:", value = 0),
        numericInput("sigmaX", "Deviația standard σX:", value = 1, min = 0.001),
        numericInput("sigmaY", "Deviația standard σY:", value = 1, min = 0.001),
        sliderInput("rho", "Coeficient de corelație (ρ):", min = -0.99, max = 0.99, value = 0.5, step = 0.05)
      )
    }
  })
  
  # Simulare 2D: genereaza (X,Y) si aplica h(X,Y)
  data2d <- eventReactive(input$sim2d, {
    n <- input$n2d
    mode <- input$mode2d
    trans <- input$trans2d
    
    if (mode == "indep") {
      x <- if (input$distX2d == "norm") rnorm(n, 0, 1) else rexp(n, 1)
      y <- if (input$distY2d == "norm") rnorm(n, 0, 1) else rexp(n, 1)
    } else {
      validate(
        need(input$sigmaX > 0 && input$sigmaY > 0, "Deviațiile standard trebuie să fie strict pozitive!"),
        need(input$rho > -1 && input$rho < 1, "Coeficientul rho trebuie să fie în intervalul (-1, 1)!")
      )
      
      # Matrice de covarianta pt Normala Bidimensionala
      sigma_mat <- matrix(c(
        input$sigmaX^2, input$rho * input$sigmaX * input$sigmaY,
        input$rho * input$sigmaX * input$sigmaY, input$sigmaY^2
      ), nrow = 2)
      
      sim_points <- rmvnorm(n, mean = c(input$muX, input$muY), sigma = sigma_mat)
      x <- sim_points[, 1]
      y <- sim_points[, 2]
    }
    
    # Aplicare transformare h(X,Y)
    z <- switch(trans,
      "suma"      = x + y,
      "diferenta" = x - y,
      "produs"    = x * y,
      "radical"   = sqrt(x^2 + y^2)
    )
    
    list(x = x, y = y, z = z, mode = mode)
  })

  observeEvent(input$sim2d, {
    showNotification("Simulare 2D completă", type = "message")
  })
  
  # Scatterplot (X,Y) cu linie de regresie
  output$scatter2d <- renderPlotly({
    res <- data2d()
    df <- data.frame(X = res$x, Y = res$y)
    p <- ggplot(df, aes(x = X, y = Y)) +
      geom_point(color = "#3498db", alpha = 0.4) +
      geom_smooth(method = "lm", color = "#e74c3c", se = FALSE) +
      theme_minimal() +
      labs(title = "Scatterplot (X, Y)", x = "X", y = "Y")
    ggplotly(p)
  })
  
  # Histograma Z = h(X,Y)
  output$histZ2d <- renderPlotly({
    res <- data2d()
    df <- data.frame(Z = res$z)
    p <- ggplot(df, aes(x = Z)) +
      geom_histogram(bins = 30, fill = "#9b59b6", color = "white", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Histograma variabilei Z = h(X,Y)", x = "Z", y = "Frecvență")
    ggplotly(p)
  })
  
  # Histograme marginale X si Y
  output$histX2d <- renderPlot({
    df <- data.frame(X = data2d()$x)
    ggplot(df, aes(x = X)) + geom_histogram(bins = 30, fill = "#34495e", color = "white") + 
      theme_minimal() + labs(title = "Marginală X")
  })
  
  output$histY2d <- renderPlot({
    df <- data.frame(Y = data2d()$y)
    ggplot(df, aes(x = Y)) + geom_histogram(bins = 30, fill = "#1abc9c", color = "white") + 
      theme_minimal() + labs(title = "Marginală Y")
  })
  
  # Tabel: medie, dispersie pt X, Y, Z + covarianta si corelatie empirica
  output$stats2d <- renderTable({
    res <- data2d()
    cov_emp <- cov(res$x, res$y)
    cor_emp <- cor(res$x, res$y)
    
    mX <- mean(res$x); vX <- var(res$x)
    mY <- mean(res$y); vY <- var(res$y)
    mZ <- mean(res$z); vZ <- var(res$z)
    
    data.frame(
      Variabila = c("X", "X", "Y", "Y", "Z (Transformată)", "Z (Transformată)", "Relație (X, Y)", "Relație (X, Y)"),
      Metrică   = c("Medie Empirică", "Dispersie Empirică", "Medie Empirică", "Dispersie Empirică", "Medie Empirică", "Dispersie Empirică", "Covarianță Empirică", "Coeficient Corelație (r)"),
      Valoare    = c(mX, vX, mY, vY, mZ, vZ, cov_emp, cor_emp)
    )
  }, digits = 4)
  
  # Studiu comparativ
  output$corrStudy2d <- renderPlotly({
    n_study <- 400
    mu_x <- isnull_default(input$muX, 0)
    mu_y <- isnull_default(input$muY, 0)
    sX <- isnull_default(input$sigmaX, 1)
    sY <- isnull_default(input$sigmaY, 1)

    # Protectie: deviatia standard trebuie sa fie strict pozitiva
    if(sX <= 0) sX <- 1
    if(sY <= 0) sY <- 1

    gen_df <- function(rho_val, label) {
      mat <- matrix(c(sX^2, rho_val*sX*sY, rho_val*sX*sY, sY^2), 2)
      pts <- rmvnorm(n_study, mean = c(mu_x, mu_y), sigma = mat)
      data.frame(X = pts[,1], Y = pts[,2], Scenariu = label)
    }

    df_all <- rbind(
      gen_df(0, "ρ = 0 (Independente)"),
      gen_df(0.5, "ρ = 0.5 (Pozitivă)"),
      gen_df(-0.5, "ρ = -0.5 (Negativă)")
    )

    # panou per valoare rho
    p <- ggplot(df_all, aes(x = X, y = Y, color = Scenariu)) +
      geom_point(alpha = 0.5) +
      facet_wrap(~Scenariu) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Comparație Vizuală a Structurii de Dependență (Cazul Normal)")
    ggplotly(p)
  })

  # Animatie GIF variatia norului de puncte cand rho creste de la -0.5 la 0.5
  observeEvent(input$genCorrAnim, {
    showNotification("Generare animație ρ — poate dura câteva secunde...", type = "message")
    n_study <- 300
    mu_x <- isnull_default(input$muX, 0)
    mu_y <- isnull_default(input$muY, 0)
    sX <- isnull_default(input$sigmaX, 1)
    sY <- isnull_default(input$sigmaY, 1)
    if(sX <= 0) sX <- 1
    if(sY <= 0) sY <- 1

    rhos <- seq(-0.5, 0.5, by = 0.1)
    df_list <- lapply(rhos, function(rho_val) {
      mat <- matrix(c(sX^2, rho_val*sX*sY, rho_val*sX*sY, sY^2), 2)
      pts <- rmvnorm(n_study, mean = c(mu_x, mu_y), sigma = mat)
      data.frame(X = pts[,1], Y = pts[,2], rho = sprintf("%.2f", rho_val))
    })
    df_anim <- bind_rows(df_list)

    p_anim <- ggplot(df_anim, aes(x = X, y = Y)) +
      geom_point(alpha = 0.6, size = 1, color = "#2c3e50") +
      theme_minimal() +
      labs(title = 'ρ = {closest_state}') +
      transition_states(rho, transition_length = 2, state_length = 1) +
      ease_aes('linear')

    anim <- animate(p_anim, nframes = length(unique(df_anim$rho)) * 10, fps = 10, renderer = gifski_renderer())
    outfile <- tempfile(fileext = ".gif")
    anim_save(filename = outfile, animation = anim)

    output$corrAnim2d <- renderImage({
      list(src = outfile, contentType = 'image/gif', width = 700, height = 450)
    }, deleteFile = TRUE)

    showNotification("Animație generată", type = "message")
  })
}

# Returneaza val daca nu e NULL, altfel def (pt inputuri neinitializate)
isnull_default <- function(val, def) {
  if (is.null(val)) return(def)
  return(val)
}

# Pornire aplicatie
shinyApp(ui = ui, server = server)