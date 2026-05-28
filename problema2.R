
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(mvtnorm)

# Funcție ajutătoare pentru calcularea indicatorilor statistici empirici
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

# ------------------------------------------------------------------------------
# INTERFAȚA UTILIZATOR (UI)
# ------------------------------------------------------------------------------
ui <- navbarPage(
  title = "Transformări de Variabile Aleatoare",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # TAB 1: TRANSFORMĂRI UNIDIMENSIONALE
  tabPanel(
    "Transformări Unidimensionale",
    sidebarLayout(
      sidebarPanel(
        h4("Configurare Simulare X"),
        selectInput("dist1d", "Repartiția lui X:",
                    choices = c("Normală N(µ, σ²)" = "norm",
                                "Exponențială Exp(λ)" = "exp",
                                "Uniformă Unif(a, b)" = "unif",
                                "Gamma(α, θ)" = "gamma")),
        
        # Panou dinamic pentru parametrii repartiției alese
        uiOutput("din_params1d"),
        
        numericInput("n1d", "Dimensiunea eșantionului (n):", value = 1000, min = 10, step = 100),
        hr(),
        h4("Configurare Transformare g(x)"),
        selectInput("trans1d", "Alege transformarea g(x):",
                    choices = c("g(x) = x²" = "patrat",
                                "g(x) = |x|" = "abs",
                                "g(x) = log(x)" = "log",
                                "g(x) = eˣ" = "exp_f",
                                "g(x) = 1 / (1 + e⁻ˣ) [Sigmoid]" = "sigmoid")),
        
        br(),
        actionButton("sim1d", "Simulează", class = "btn-primary w-100")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Variabila X", 
                   br(),
                   plotOutput("plotX"), 
                   h5("Statistici Empirice X"),
                   tableOutput("statsX")),
          
          tabPanel("Variabila Y = g(X)", 
                   br(),
                   uiOutput("warningLog"),
                   plotOutput("plotY"), 
                   h5("Statistici Empirice Y"),
                   tableOutput("statsY")),
          
          tabPanel("Comparație & Interpretare", 
                   br(),
                   plotOutput("plotComp1d"),
                   br(),
                   card(
                     card_header(h5("Interpretare Automată")),
                     card_body(textOutput("interpret1d"))
                   ))
        )
      )
    )
  ),
  
  # TAB 2: TRANSFORMĂRI BIDIMENSIONALE
  tabPanel(
    "Transformări Bidimensionale",
    sidebarLayout(
      sidebarPanel(
        h4("Mod Generare (X, Y)"),
        radioButtons("mode2d", "Selectează modul:",
                     choices = c("Variante independente" = "indep",
                                 "Normală bidimensională" = "mvnorm")),
        hr(),
        uiOutput("din_params2d"),
        hr(),
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
                     column(6, plotOutput("scatter2d")),
                     column(6, plotOutput("histZ2d"))
                   ),
                   br(),
                   h5("Indicatori Numerici Complecși"),
                   tableOutput("stats2d")),
          
          tabPanel("Histograme Marginale (X și Y)", 
                   br(),
                   fluidRow(
                     column(6, plotOutput("histX2d")),
                     column(6, plotOutput("histY2d"))
                   )),
          
          tabPanel("Studiu Corelație (ρ)", 
                   br(),
                   p("Vizualizarea efectului coeficientului de corelație în cazul unei repartiții Normale Bidimensionale:"),
                   plotOutput("corrStudy2d", height = "350px"))
        )
      )
    )
  )
)
# logica server
server <- function(input, output, session) {
  
  # --- TAB 1: DINAMIC UI PARAMETRI ---
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
  
  # tab 1: reactive pentru simulare 1d
  data1d <- eventReactive(input$sim1d, {
    n <- input$n1d
    dist <- input$dist1d
    trans <- input$trans1d
    
    # Validări input primare
    if (dist == "norm") {
      validate(need(input$sigma > 0, "Eroare: Deviația standard (σ) trebuie să fie strict pozitivă!"))
    } else if (dist == "exp") {
      validate(need(input$lambda > 0, "Eroare: Parametrul λ trebuie să fie strict pozitiv!"))
    } else if (dist == "unif") {
      validate(need(input$unif_a < input$unif_b, "Eroare: Limita 'a' trebuie să fie strict mai mică decât 'b'!"))
    } else if (dist == "gamma") {
      validate(need(input$alpha > 0 && input$theta > 0, "Eroare: Parametrii α și θ trebuie să fie strict pozitivi!"))
    }
    
    # Generare X
    x <- switch(dist,
      "norm"  = rnorm(n, mean = input$mu, sd = input$sigma),
      "exp"   = rexp(n, rate = input$lambda),
      "unif"  = runif(n, min = input$unif_a, max = input$unif_b),
      "gamma" = rgamma(n, shape = input$alpha, scale = input$theta)
    )
    
    # Aplicare g(x) și tratare cazuri speciale (log)
    warn_msg <- NULL
    x_filtered <- x
    
    if (trans == "log") {
      ilegale <- sum(x <= 0)
      if (ilegale > 0) {
        warn_msg <- paste("Avertisment: Au fost identificate și eliminate", ilegale, 
                          "valori <= 0 incompatibile cu transformarea logaritmică.")
        x_filtered <- x[x > 0]
      }
    }
    
    y <- switch(trans,
      "patrat"  = x_filtered^2,
      "abs"     = abs(x_filtered),
      "log"     = log(x_filtered),
      "exp_f"   = exp(x_filtered),
      "sigmoid" = 1 / (1 + exp(-x_filtered))
    )
    
    list(x = x, x_filtered = x_filtered, y = y, warn_msg = warn_msg, dist = dist, trans = trans)
  })
  
  # tab 1 randare rezultate
  output$warningLog <- renderUI({
    res <- data1d()
    if (!is.null(res$warn_msg)) {
      div(class = "alert alert-warning", res$warn_msg)
    }
  })
  
  output$plotX <- renderPlot({
    res <- data1d()
    df <- data.frame(x = res$x)
    p <- ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#34495e", color = "white", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Histograma lui X și Densitatea Teoretică", x = "X", y = "Densitate")
    
    # Suprapunere densitate teoretică în funcție de repartiție
    if (res$dist == "norm") {
      p <- p + stat_function(fun = dnorm, args = list(mean = input$mu, sd = input$sigma), color = "#e74c3c", linewidth = 1)
    } else if (res$dist == "exp") {
      p <- p + stat_function(fun = dexp, args = list(rate = input$lambda), color = "#e74c3c", linewidth = 1)
    } else if (res$dist == "unif") {
      p <- p + stat_function(fun = dunif, args = list(min = input$unif_a, max = input$unif_b), color = "#e74c3c", linewidth = 1)
    } else if (res$dist == "gamma") {
      p <- p + stat_function(fun = dgamma, args = list(shape = input$alpha, scale = input$theta), color = "#e74c3c", linewidth = 1)
    }
    p
  })
  
  output$plotY <- renderPlot({
    res <- data1d()
    df <- data.frame(y = res$y)
    ggplot(df, aes(x = y)) +
      geom_histogram(bins = 30, fill = "#1abc9c", color = "white", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Histograma variabilei transformate Y = g(X)", x = "Y", y = "Frecvență")
  })
  
  output$plotComp1d <- renderPlot({
    res <- data1d()
    dfX <- data.frame(Valoare = res$x_filtered, Variabila = "X")
    dfY <- data.frame(Valoare = res$y, Variabila = "Y")
    df_tot <- rbind(dfX, dfY)
    
    ggplot(df_tot, aes(x = Valoare, fill = Variabila)) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.5, color = "white") +
      facet_wrap(~Variabila, scales = "free") +
      scale_fill_manual(values = c("X" = "#34495e", "Y" = "#1abc9c")) +
      theme_minimal() +
      labs(title = "Comparație Vizuală Directă (Histograme Facetate)", x = "Valori", y = "Frecvență")
  })
  
  output$statsX <- renderTable({ calculeaza_statistici(data1d()$x) }, digits = 4)
  output$statsY <- renderTable({ calculeaza_statistici(data1d()$y) }, digits = 4)
  
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
  
  # tab 2 dinamic UI pentru parametrii
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
  
  # tab 2 reactive pentru simulare 2d
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
      
      # Construire matrice de covarianță pentru Normala Bidimensională
      sigma_mat <- matrix(c(
        input$sigmaX^2, input$rho * input$sigmaX * input$sigmaY,
        input$rho * input$sigmaX * input$sigmaY, input$sigmaY^2
      ), nrow = 2)
      
      sim_points <- rmvnorm(n, mean = c(input$muX, input$muY), sigma = sigma_mat)
      x <- sim_points[, 1]
      y <- sim_points[, 2]
    }
    
    # Aplicare transformare bidimensională h(X,Y)
    z <- switch(trans,
      "suma"      = x + y,
      "diferenta" = x - y,
      "produs"    = x * y,
      "radical"   = sqrt(x^2 + y^2)
    )
    
    list(x = x, y = y, z = z, mode = mode)
  })
  
  #tab 2 randare rezultate

  output$scatter2d <- renderPlot({
    res <- data2d()
    df <- data.frame(X = res$x, Y = res$y)
    ggplot(df, aes(x = X, y = Y)) +
      geom_point(color = "#3498db", alpha = 0.4) +
      geom_smooth(method = "lm", color = "#e74c3c", se = FALSE) +
      theme_minimal() +
      labs(title = "Scatterplot (X, Y)", x = "X", y = "Y")
  })
  
  output$histZ2d <- renderPlot({
    res <- data2d()
    df <- data.frame(Z = res$z)
    ggplot(df, aes(x = Z)) +
      geom_histogram(bins = 30, fill = "#9b59b6", color = "white", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Histograma variabilei Z = h(X,Y)", x = "Z", y = "Frecvență")
  })
  
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
  
  output$stats2d <- renderTable({
    res <- data2d()
    cov_emp <- cov(res$x, res$y)
    cor_emp <- cor(res$x, res$y)
    
    # Statistici de bază pentru X, Y, Z
    mX <- mean(res$x); vX <- var(res$x)
    mY <- mean(res$y); vY <- var(res$y)
    mZ <- mean(res$z); vZ <- var(res$z)
    
    data.frame(
      Variabila = c("X", "Y", "Z (Transformată)", "Relație (X, Y)", "Relație (X, Y)"),
      Metrică = c("Medie Empirică", "Medie Empirică", "Medie Empirică", "Covarianță Empirică", "Coeficient Corelație (r)"),
      Valoare = c(mX, mY, mZ, cov_emp, cor_emp)
    )
  }, digits = 4)
  
  # Studiu dinamic comparativ pentru rho = 0, 0.5, -0.5
  output$corrStudy2d <- renderPlot({
    n_study <- 400
    mu_x <- isnull_default(input$muX, 0)
    mu_y <- isnull_default(input$muY, 0)
    sX <- isnull_default(input$sigmaX, 1)
    sY <- isnull_default(input$sigmaY, 1)
    
    if(sX <= 0) sX <- 1
    if(sY <= 0) sY <- 1
    
    gen_df <- function(rho_val, label) {
      mat <- matrix(c(sX^2, rho_val*sX*sY, rho_val*sX*sY, sY^2), 2)
      pts <- rmvnorm(n_study, mean = c(mu_x, mu_y), sigma = mat)
      data.frame(X = pts[,1], Y = pts[,2], Scenariu = label)
    }
    
    df1 <- gen_df(0, "ρ = 0 (Independente)")
    df2 <- gen_df(0.5, "ρ = 0.5 (Pozitivă)")
    df3 <- gen_df(-0.5, "ρ = -0.5 (Negativă)")
    
    df_all <- rbind(df1, df2, df3)
    
    ggplot(df_all, aes(x = X, y = Y, color = Scenariu)) +
      geom_point(alpha = 0.5) +
      facet_wrap(~Scenariu) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "Comparație Vizuală a Structurii de Dependență (Cazul Normal)")
  })
}

# Funcție utilitară internă pentru siguranța randării inițiale a plotului de studiu
isnull_default <- function(val, def) {
  if (is.null(val)) return(def)
  return(val)
}

# Rulare aplicație
shinyApp(ui = ui, server = server)