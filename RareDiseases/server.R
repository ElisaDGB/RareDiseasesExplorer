################################################################################
#                              Server
################################################################################



shinyServer(function(input, output, session) {
            
      
      source(file.path("Inicio.R"), local = TRUE)$value # Home
      source(file.path("Enfermedades.R"), local = TRUE)$value # Diseases
      source(file.path("Analisis.R"), local = TRUE)$value # Analysis
      
 
})





