################################################################################
#                        Analisis/Analysis tab
################################################################################



# Update diseases options

updateSelectizeInput(session, 'buscadorEnfermedad2',
                     choices = datosSinonimos$synonyms,
                     server = TRUE
)


# Search by disease

filtrar_enfermedades <- function(matriz, enfermedad, num) {
      
      # Get standard disease's name
      
      nombreenfermedad <- datosSinonimos[datosSinonimos$synonyms == enfermedad,2]
      
      # Obtain all symptoms of the selected disease
      
      sintomas_enfermedad <- matriz[nombreenfermedad, , drop = FALSE]
      
      # Submatrix with diseases sharing at least two symptoms with the selected disease
      
      columnas_sintomas <- which(sintomas_enfermedad > 0)
      submatriz <- matriz[, columnas_sintomas, drop = FALSE]
      filas_enfermedades <- apply(submatriz, 1, function(fila) any(fila > 2))
      submatriz_filtrada <- submatriz[filas_enfermedades, , drop = FALSE]
      
      # Sort the matrix according to the diseases that share the most symptoms with the selected disease
      
      orden <- rowSums(submatriz_filtrada != 0)
      orden <- order(as.numeric(orden), decreasing = TRUE)
      submatriz_ordenada <- submatriz_filtrada[orden, , drop = FALSE]
      submatriz_ordenada <- submatriz_ordenada[c(1:num),, drop = FALSE]
      
      # Replace symptom codes by their names
      
      colnames(submatriz_ordenada) <- datosGruposHPO[datosGruposHPO$Codigo %in% colnames(submatriz_ordenada),3]
      
      # Return matrix with X (=num) diseases (rows) and all the symptoms from the selected disease (columns)
      
      return(submatriz_ordenada)
}

enfermedadParecida <- eventReactive(input$searchDiseases2,{
      filtrar_enfermedades(datosSintomas, input$buscadorEnfermedad2, input$nEnfermedades)
})  


heatmapPlot_Parecidas <- eventReactive(input$searchDiseases2,{
      
      if (length(rownames(enfermedadParecida())) == 1){
            heatmaply(enfermedadParecida(), colors = colorRampPalette(c("white", "#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))(6), limits = c(0,5), dendrogram = "column", column_text_angle = 30 )
      }else if(length(colnames(enfermedadParecida())) == 1){
            heatmaply(enfermedadParecida(), colors = colorRampPalette(c("white", "#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))(6), limits = c(0,5), dendrogram = "row", column_text_angle = 30)
      }else if ( (length(rownames(enfermedadParecida())) > 1) && (length(colnames(enfermedadParecida())) > 1)) {
            heatmaply(enfermedadParecida(), colors = colorRampPalette(c("white", "#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000")), limits = c(0,5), column_text_angle = 30)
      }
}) 




# Search by characteristics

filtros <- function(ageOnset, inheritance, genes, groupsSymptoms, symptoms){
      
      symptomsFinal <- c()
      diseaseFinal <- c()
      
      # Apply filters - characteristics
      
      if(!("All" %in% ageOnset)){
            b <- datosEdadAparicion[,ageOnset, drop=FALSE]
            listAgeOnset <- names(which(apply(b,1,sum) == length(ageOnset)))
            diseaseFinal <- c(diseaseFinal, listAgeOnset)
      }
      
      if(!("All" %in% inheritance)){
            b <- datosHerencia[,inheritance, drop=FALSE]
            listInheritance <- names(which(apply(b,1,sum) == length(inheritance)))
            diseaseFinal <- c(diseaseFinal, listInheritance)
      }
      
      if(!("All" %in% genes)){
            listGenes <- rownames(datosGenes)[which(datosGenes$Gene %in% genes)]
            diseaseFinal <- c(diseaseFinal, listGenes)
      }

      # Apply filters - symptoms
            
      if(!("All" %in% groupsSymptoms)){
            b <- datosGruposHPO[which(datosGruposHPO$Grupo2 %in% groupsSymptoms),4]
            hpoCodes <- c(b, datosGruposHPO[which(datosGruposHPO$Grupo1 %in% groupsSymptoms),4])
            symptomsFinal <- c(symptomsFinal, hpoCodes)
      }
      
      if(!("All" %in% symptoms)){
            hpoCodes <- datosGruposHPO[which(datosGruposHPO$Sintoma %in% symptoms),4]
            symptomsFinal <- c(symptomsFinal, hpoCodes)
      }
      
      #########################
      
      # Symptom selection based on user-selected characteristics
      
      if(length(symptomsFinal) != 0){
            symptomsFinal <- unique(symptomsFinal)
      } else {
            symptomsFinal <- datosSintomas[diseaseFinal,,drop=FALSE]
            symptomsFinal <- names(which(apply(symptomsFinal,2,sum) != 0))
      }
      
      # Selection of diseases based on the symptoms selected by the user
      
      if(length(diseaseFinal) != 0){
            diseaseFinal <- unique(diseaseFinal)
      } else {
            diseaseFinal <- datosSintomas[,symptomsFinal,drop=FALSE]
            diseaseFinal <- names(which(apply((diseaseFinal != 0),1,sum)  > 0))
      }
      
      #########################
      
      # Build matrix
      
      dataFilt <- datosSintomas[diseaseFinal ,symptomsFinal, drop=FALSE]
      colnames(dataFilt) <- datosGruposHPO[datosGruposHPO$Codigo %in% colnames(dataFilt),3]
      
      
      # Return matrix with X diseases (rows) and the selected symptoms (columns)
      
      return(dataFilt)
      
}


datosHerenciaSum2 <- eventReactive(input$searchDiseases,{
                              filtros(input$edadAparicion, input$herencia, input$genes, input$GruposSintomas, input$sintomas)
                        })


heatmapPlot_Filtro <- eventReactive(input$searchDiseases, {
      data <- datosHerenciaSum2()
      n_rows <- nrow(datosHerenciaSum2())
      n_cols <- ncol(datosHerenciaSum2())
      # Avoid app errors due to long time clustering processing in huge matrices
      if ((n_rows + n_cols) > 1000) {
            showNotification(paste0("This selection of filters results in ", n_rows, " diseases and ", n_cols, " symptoms. Reduce the number of features by selecting specific symptoms."), type = "error")
            return(NULL)
      # If matrices are affordable calculate clustering
      } else {
            if (n_rows == 1) {
                  heatmaply(datosHerenciaSum2(), colors = colorRampPalette(c("white", "#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))(6), limits = c(0, 5), dendrogram = "column")
            } else if (n_cols == 1) {
                  heatmaply(datosHerenciaSum2(), colors = colorRampPalette(c("white", "#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))(6), limits = c(0, 5), dendrogram = "row")
            } else {
                  heatmaply(datosHerenciaSum2(), colors = colorRampPalette(c("white", "#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"))(6), limits = c(0, 5))
            }
      }
})


# Output

lastButton <- reactiveVal(NULL)

observeEvent(input$searchDiseases, {
      lastButton("searchDiseases")
})

observeEvent(input$searchDiseases2, {
      lastButton("searchDiseases2")
})

output$interactiveHeatmap <- renderPlotly({
      if (lastButton() == "searchDiseases2" && !is.null(input$searchDiseases2) && input$searchDiseases2 > 0) {
            heatmapPlot_Parecidas()
      } else if (lastButton() == "searchDiseases" && !is.null(input$searchDiseases) && input$searchDiseases > 0) {
            heatmapPlot_Filtro()
      }
})



