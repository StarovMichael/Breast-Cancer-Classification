library(shiny)

# Načítanie modelu, zoznamu atribútov priemeru a štandardnej odchylky pre každy atribut
setwd("/Users/meshokk/RStudio_apps")
model <- readRDS("svm_trained_model.rds")
features <- readRDS("model_features.rds")
scaling_info <- readRDS("scaling_info.rds")

# Štandartizacia vstupov
standardize_input <- function(input_vector, scaling_info) {
  as.numeric(mapply(function(x, info) {
    (x - info$mean) / info$sd
  }, input_vector, scaling_info, SIMPLIFY = TRUE))
}

#Ppredikcia pomocou vytvoreného modelu
svm_predict <- function(model, X) {
  pred <- X %*% model$w + model$b
  ifelse(pred >= 0, 1, -1)
}

# Zobrazenie popisných názvov pre atribúty
feature_labels <- list(
  texture_mean = "Texture (mean)",
  area_mean = "Area (mean)",
  smoothness_mean = "Smoothness (mean)",
  compactness_mean = "Compactness (mean)",
  concave.points_mean = "Concave Points (mean)",
  symmetry_mean = "Symmetry (mean)",
  fractal_dimension_mean = "Fractal Dimension (mean)",
  radius_se = "Radius (SE)",
  texture_se = "Texture (SE)",
  smoothness_se = "Smoothness (SE)",
  compactness_se = "Compactness (SE)",
  concavity_se = "Concavity (SE)",
  concave.points_se = "Concave Points (SE)",
  symmetry_se = "Symmetry (SE)",
  fractal_dimension_se = "Fractal Dimension (SE)",
  smoothness_worst = "Smoothness (Worst)",
  compactness_worst = "Compactness (Worst)",
  concavity_worst = "Concavity (Worst)",
  symmetry_worst = "Symmetry (Worst)",
  fractal_dimension_worst = "Fractal Dimension (Worst)"
)

# Rozsahy skutočných hodnôt pre každý atribút 
feature_ranges <- list(
  texture_mean = c(9.71, 39.28),
  area_mean = c(143.5, 2501.0),
  smoothness_mean = c(0.05263, 0.16340),
  compactness_mean = c(0.01938, 0.34540),
  concave.points_mean = c(0.00000, 0.20120),
  symmetry_mean = c(0.1060, 0.3040),
  fractal_dimension_mean = c(0.04996, 0.09744),
  radius_se = c(0.1115, 2.8730),
  texture_se = c(0.3602, 4.8850),
  smoothness_se = c(0.001713, 0.031130),
  compactness_se = c(0.002252, 0.135400),
  concavity_se = c(0.00000, 0.39600),
  concave.points_se = c(0.000000, 0.052790),
  symmetry_se = c(0.007882, 0.078950),
  fractal_dimension_se = c(0.0008948, 0.0298400),
  smoothness_worst = c(0.07117, 0.22260),
  compactness_worst = c(0.02729, 1.05800),
  concavity_worst = c(0.0000, 1.2520),
  symmetry_worst = c(0.1565, 0.6638),
  fractal_dimension_worst = c(0.05504, 0.20750)
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .input-grid {
        display: flex;
        flex-direction: column;
        gap: 15px;
        width: 100%;
        max-width: 1000px;
        margin: 0 auto;
      }

      .input-line {
        display: flex;
        justify-content: space-between;
        align-items: center;
        flex-wrap: wrap;
        gap: 10px;
      }

      .input-label {
        flex: 1 1 200px;
        font-weight: bold;
        white-space: normal;
      }

      .input-field {
        flex: 0 0 120px;
      }

      .input-field input[type='number']::-webkit-outer-spin-button,
      .input-field input[type='number']::-webkit-inner-spin-button {
        -webkit-appearance: none;
        margin: 0;
      }

      .input-field input[type='number'] {
        -moz-appearance: textfield;
        appearance: textfield;
        width: 100%;
      }

      .range-text {
        flex: 1 1 200px;
        font-size: 13px;
        color: #666;
        font-style: italic;
        white-space: nowrap;
      }

      .error-wrapper {
        margin-left: 200px;
      }

      .error-text {
        color: orange;
        font-size: 13px;
        font-style: italic;
        margin-top: -4px;
        margin-bottom: 8px;
      }

      .button-row {
        display: flex;
        justify-content: center;
        gap: 20px;
        margin: 30px 0;
        flex-wrap: wrap;
      }

      .center-page {
        display: flex;
        flex-direction: column;
        align-items: center;
      }
    ")),
    
    tags$script(HTML("
      document.addEventListener('wheel', function(e) {
        if (document.activeElement.type === 'number' &&
            document.activeElement === document.activeElement.ownerDocument.activeElement) {
          document.activeElement.blur();
        }
      });
    "))
  ),
  
  titlePanel("Cancer Prediction App using SVM"),
  
  div(class = "center-page",
      div(class = "input-grid",
          lapply(features, function(feat) {
            label <- feature_labels[[feat]]
            range <- feature_ranges[[feat]]
            tagList(
              div(class = "input-line",
                  div(class = "input-label", label),
                  div(class = "input-field",
                      numericInput(inputId = feat, label = NULL, value = 0)
                  ),
                  div(class = "range-text", paste0("[", range[1], " to ", range[2], "]"))
              ),
              div(class = "error-wrapper",
                  uiOutput(paste0("error_", feat))
              )
            )
          })
      ),
      
      div(class = "button-row",
          actionButton("predictBtn", "Predict",
                       icon = icon("magic"),
                       style = "background-color: #28a745; color: white; font-weight: bold;"),
          actionButton("resetBtn", "Reset",
                       icon = icon("undo"),
                       style = "background-color: #dc3545; color: white; font-weight: bold;")
      ),
      
      uiOutput("prediction")
  )
)
# SERVER časť aplikácie
server <- function(input, output, session) {
  
  # Validácia vstupov v reálnom čase
  lapply(features, function(feat) {
    observe({
      val <- input[[feat]]
      range <- feature_ranges[[feat]]
      msg <- NULL
      
      if (is.null(val) || is.na(val)) {
        msg <- "Input is empty or not a number."
      } else if (val < range[1] || val > range[2]) {
        msg <- paste0("Value out of range [", range[1], " to ", range[2], "]")
      }
      
      output[[paste0("error_", feat)]] <- renderUI({
        if (!is.null(msg)) div(class = "error-text", msg)
      })
    })
  })
  
  # Funkcia tlačidla "Predict"
  observeEvent(input$predictBtn, {
    valid <- all(sapply(features, function(feat) {
      val <- input[[feat]]
      range <- feature_ranges[[feat]]
      #print(paste("Checking:", feat, "value:", val, "range:", paste(range, collapse = " - ")))
      if (is.null(val) || is.null(range)) return(FALSE)
      if (is.na(val)) return(FALSE)
      if (!is.numeric(val)) return(FALSE)
      if (length(range) != 2) return(FALSE)
      
      val >= range[1] && val <= range[2]
    }))
    
    if (!valid) {
      output$prediction <- renderUI({
        HTML("<div style='color: orange; font-size: 18px; font-weight: bold;'> Prediction failed. Please fix the errors above.</div>")
      })
      return()
    }
    
    raw_input <- sapply(features, function(feat) input[[feat]])
    standardized_input <- standardize_input(raw_input, scaling_info)
    X_new <- matrix(standardized_input, nrow = 1)
    pred <- svm_predict(model, X_new)
    
    output$prediction <- renderUI({
      if (pred == 1) {
        HTML("<div style='color: #dc3545; font-size: 20px; font-weight: bold; padding-bottom: 30px;'>
               Prediction: <span>Malignant</span>
             </div>")
      } else {
        HTML("<div style='color: #28a745; font-size: 20px; font-weight: bold; padding-bottom: 30px;'>
               Prediction: <span>Benign</span>
             </div>")
      }
    })
  })
  
  # Funkcia tlačidla "Reset" 
  observeEvent(input$resetBtn, {
    for (feat in features) {
      updateNumericInput(session, inputId = feat, value = 0)
      output[[paste0("error_", feat)]] <- renderUI({ NULL })
    }
    output$prediction <- renderUI({ NULL })  # wipes out the result
  })
}

# Spustenie
shinyApp(ui = ui, server = server)