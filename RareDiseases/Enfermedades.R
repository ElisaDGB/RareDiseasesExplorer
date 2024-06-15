################################################################################
#                        Enfermedades/Diseases tab
################################################################################



# Update diseases options

updateSelectizeInput(session, 'buscadorEnfermedad',
                     choices = datosSinonimos$synonyms,
                     server = TRUE
)


#  Disease's name output

enfermedadSeleccionada <- reactive({
      datosSinonimos[datosSinonimos$synonyms == input$buscadorEnfermedad,2]
})


output$enfermedadSeleccionadaOut <- renderText({
      h1(enfermedadSeleccionada())
})


#  Disease's description text

output$enfermedadSeleccionadaTextoOut <- renderText({
      datosTexto[enfermedadSeleccionada(),1]
})


#  Symptoms


Dtreemap <- function(enfermedadSeleccionada, results2, dfDisSymp){
      
      enf <- dfDisSymp[enfermedadSeleccionada,,drop=FALSE]
      nombresNumEnf <- enf[enf != 0]
      nombresEnf <- colnames(enf)[enf != 0]
      
      resultsHP <- results2[results2$Codigo %in% nombresEnf,]
      rownames(resultsHP) <- resultsHP$Codigo
      enf <-data.frame(cbind(nombresEnf, nombresNumEnf))
      rownames(enf) <- enf$nombresEnf
      
      resultsHP2 <- merge(resultsHP, enf, by=0)
      resultsHP2 <- resultsHP2[,-(c(1,5,6))]
      resultsHP2$nombresNumEnf <- as.numeric(resultsHP2$nombresNumEnf)
      resultsHP2  %>%
      data_to_hierarchical(c(Grupo1,Grupo2,Sintoma), nombresNumEnf) %>%
      hchart(type = "treemap",
             allowTraversingTree = T,
             animation = F,
             levels = list(
                   list(level = 1, dataLabels = list(enabled = TRUE), borderColor = "black", borderWidth = 2),
                   list(level = 2, dataLabels = list(enabled = TRUE), borderColor = "white", borderWidth = 2),
                   list(level = 3, dataLabels = list(enabled = TRUE))
             )) %>%
      hc_plotOptions(
            series  = list(
                  borderWidth = 0.5,
                  dataLabels = list(
                        style = list(
                              fontSize = "12px",
                              textOutline = FALSE
                        )
                  )
            )
      ) %>%
      hc_tooltip(pointFormat = "Síntoma: {point.name}<br>
                             Valor: {point.value}")
}

output$treemapOut <- renderHighchart(Dtreemap(enfermedadSeleccionada(), datosGruposHPO, datosSintomas))


# Age Onset

enfermedadSeleccionada <- reactive({
      datosSinonimos[datosSinonimos$synonyms == input$buscadorEnfermedad,2]
})


output$enfermedadSeleccionadaOut <- renderText({
      enfermedadSeleccionada()
})


      # Antenatal
      
      edadAparicionAntenatal <- reactive({
            datosEdadAparicion[as.character(enfermedadSeleccionada()) , "Antenatal"]
            })
      
      v <- reactiveValues()
      
      v$edadAparicionAntenatalImg <- tags$img(src = 'antenatal.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
      
            req(edadAparicionAntenatal())
            edadAparicionAntenatalIf <- edadAparicionAntenatal()
      
            if (edadAparicionAntenatalIf == 1) {v$edadAparicionAntenatalImg <- tags$img(src = 'antenatal_azul.svg', align = "center", height="100vh")
            } else{v$edadAparicionAntenatalImg <- tags$img(src = 'antenatal.svg', align = "center", height="100vh") }
      
      })
      
      output$edadAparicionAntenatalImgOut <- renderUI(v$edadAparicionAntenatalImg)
      
      
      # Neonatal
      
      edadAparicionNeonatal <- reactive({
            datosEdadAparicion[as.character(enfermedadSeleccionada()) , "Neonatal"]
      })
      
      v$edadAparicionNeonatalImg <- tags$img(src = 'neonatal.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(edadAparicionNeonatal())
            edadAparicionNeonatalIf <- edadAparicionNeonatal()
            
            if (edadAparicionNeonatalIf == 1) {v$edadAparicionNeonatalImg <- tags$img(src = 'neonatal_azul.svg', align = "center", height="100vh")
            } else{v$edadAparicionNeonatalImg <- tags$img(src = 'neonatal.svg', align = "center", height="100vh") }
            
      })
      
      output$edadAparicionNeonatalImgOut <- renderUI(v$edadAparicionNeonatalImg)
      
      
      
      # Infancy
      
      edadAparicionInfancia <- reactive({
            datosEdadAparicion[as.character(enfermedadSeleccionada()) , "Infancy"]
      })
      
      v$edadAparicionInfanciaImg <- tags$img(src = 'infancia.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(edadAparicionInfancia())
            edadAparicionNeonatalIf <- edadAparicionInfancia()
            
            if (edadAparicionNeonatalIf == 1) {v$edadAparicionInfanciaImg <- tags$img(src = 'infancia_azul.svg', align = "center", height="100vh")
            } else{v$edadAparicionInfanciaImg <- tags$img(src = 'infancia.svg', align = "center", height="100vh") }
            
      })
      
      output$edadAparicionInfanciaImgOut <- renderUI(v$edadAparicionInfanciaImg)
      
      
      
      # Childhood
      
      edadAparicionNinez <- reactive({
            datosEdadAparicion[as.character(enfermedadSeleccionada()) , "Childhood"]
      })
      
      
      v$edadAparicionNinezImg <- tags$img(src = 'infancia.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(edadAparicionNinez())
            edadAparicionNinezIf <- edadAparicionNinez()
            
            if (edadAparicionNinezIf == 1) {v$edadAparicionNinezImg <- tags$img(src = 'ninez_azul.svg', align = "center", height="100vh")
            } else{v$edadAparicionNinezImg <- tags$img(src = 'ninez.svg', align = "center", height="100vh") }
            
      })
      
      output$edadAparicionNinezImgOut <- renderUI(v$edadAparicionNinezImg)
      
      
      # Adolescent
      
      edadAparicionAdolescente <- reactive({
            datosEdadAparicion[as.character(enfermedadSeleccionada()) , "Adolescent"]
      })
      
      v$edadAparicionAdolescenteImg <- tags$img(src = 'adolescente.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(edadAparicionAdolescente())
            edadAparicionAdolescenteIf <- edadAparicionAdolescente()
            
            if (edadAparicionAdolescenteIf == 1) {v$edadAparicionAdolescenteImg <- tags$img(src = 'adolescente_azul.svg', align = "center", height="100vh")
            } else{v$edadAparicionAdolescenteImg <- tags$img(src = 'adolescente.svg', align = "center", height="100vh") }
            
      })
      
      output$edadAparicionAdolescenciaImgOut <- renderUI(v$edadAparicionAdolescenteImg)
      
      
      # Adult
      
      edadAparicionAdulto <- reactive({
            datosEdadAparicion[as.character(enfermedadSeleccionada()) , "Adult"]
      })
      
      v$edadAparicionAdultoImg <- tags$img(src = 'adulto.svg', align = "center", height="100vh")
      
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(edadAparicionAdulto())
            edadAparicionAdultoeIf <- edadAparicionAdulto()
            
            if (edadAparicionAdultoeIf == 1) {v$edadAparicionAdultoImg <- tags$img(src = 'adulto_azul.svg', align = "center", height="100vh")
            } else{v$edadAparicionAdultoImg <- tags$img(src = 'adulto.svg', align = "center", height="100vh") }
            
      })
      
      output$edadAparicionAdultoImgOut <- renderUI(v$edadAparicionAdultoImg)
      
      
      # Elderly
      
      edadAparicionVejez <- reactive({
            datosEdadAparicion[as.character(enfermedadSeleccionada()) , "Elderly"]
      })
      
      v$edadAparicionVejezImg <- tags$img(src = 'vejez.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(edadAparicionVejez())
            edadAparicionVejezIf <- edadAparicionVejez()
            
            if (edadAparicionVejezIf == 1) {v$edadAparicionVejezImg <- tags$img(src = 'vejez_azul.svg', align = "center", height="100vh")
            } else{v$edadAparicionVejezImg <- tags$img(src = 'vejez.svg', align = "center", height="100vh") }
            
      })
      
      output$edadAparicionVejezImgOut <- renderUI(v$edadAparicionVejezImg)
      


#  Logo               

output$logoImg <- renderUI(tags$img(src = 'lazo.svg', align = "center", height="150vh"))



#  Related genes

output$enfermedadGenesRelacionados <- renderText({
      datosGenes[enfermedadSeleccionada(),1]
})





# Inheritance  

      
      # Autosomal dominant
      
      herenciaAutDom <- reactive({
            datosHerencia[as.character(enfermedadSeleccionada()) , "Autosomal dominant"]
      })
      
      v$herenciaAutDomImg <- tags$img(src = 'autosomicaDom.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(herenciaAutDom())
            herenciaAutDomIf <- herenciaAutDom()
            
            if (herenciaAutDomIf == 1) {v$herenciaAutDomImg <- tags$img(src = 'autosomicaDom_azul.svg', align = "center", height="100vh")
            } else{v$herenciaAutDomImg <- tags$img(src = 'autosomicaDom.svg', align = "center", height="100vh") }
            
      })
      
      output$herenciaAutDomImgOut <- renderUI(v$herenciaAutDomImg)
      
      
      # Autosomal recessive
      
      herenciaAutRec <- reactive({
            datosHerencia[as.character(enfermedadSeleccionada()) , "Autosomal recessive"]
      })
      
      v$herenciaAutRecImg <- tags$img(src = 'autosomicaRec.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(herenciaAutRec())
            herenciaAutRecIf <- herenciaAutRec()
            
            if (herenciaAutRecIf == 1) {v$herenciaAutRecImg <- tags$img(src = 'autosomicaRec_azul.svg', align = "center", height="100vh")
            } else{v$herenciaAutRecImg <- tags$img(src = 'autosomicaRec.svg', align = "center", height="100vh") }
            
      })
      
      output$herenciaAutRecImgOut <- renderUI(v$herenciaAutRecImg)
      
      
      # Multigenic
      
      herenciaMultigenica <- reactive({
            datosHerencia[as.character(enfermedadSeleccionada()) , "Multigenic multifactorial"]
      })
      
      v$herenciaMultigenicaImg <- tags$img(src = 'multigenica.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(herenciaMultigenica())
            herenciaMultigenicaIf <- herenciaMultigenica()
            
            if (herenciaMultigenicaIf == 1) {v$herenciaMultigenicaImg <- tags$img(src = 'multigenica_azul.svg', align = "center", height="100vh")
            } else{v$herenciaMultigenicaImg <- tags$img(src = 'multigenica.svg', align = "center", height="100vh") }
            
      })
      
      output$herenciaMultigenicaImgOut <- renderUI(v$herenciaMultigenicaImg)
      
      
      # X linked dominant
      
      herenciaXDom <- reactive({
            datosHerencia[as.character(enfermedadSeleccionada()) , "X linked dominant"]
      })
      
      v$herenciaXDomImg <- tags$img(src = 'XlinkedDom.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(herenciaXDom())
            herenciaXDomIf <- herenciaXDom()
            
            if (herenciaXDomIf == 1) {v$herenciaXDomImg <- tags$img(src = 'XlinkedDom_azul.svg', align = "center", height="100vh")
            } else{v$herenciaXDomImg <- tags$img(src = 'XlinkedDom.svg', align = "center", height="100vh") }
            
      })
      
      output$herenciaXDomImgOut <- renderUI(v$herenciaXDomImg)
      
      
      # X linked recessive
      
      herenciaXRec <- reactive({
            datosHerencia[as.character(enfermedadSeleccionada()) , "X linked recessive"]
      })
      
      v$herenciaXRecImg <- tags$img(src = 'XlinkedRec.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(herenciaXRec())
            herenciaXRecIf <- herenciaXRec()
            
            if (herenciaXRecIf == 1) {v$herenciaXRecImg <- tags$img(src = 'XlinkedRec_azul.svg', align = "center", height="100vh")
            } else{v$herenciaXRecImg <- tags$img(src = 'XlinkedRec.svg', align = "center", height="100vh") }
            
      })
      
      output$herenciaXRecImgOut <- renderUI(v$herenciaXRecImg)
      
      
      # Y linked 
      
      herenciaY <- reactive({
            datosHerencia[as.character(enfermedadSeleccionada()) , "Y linked"]
      })
      
      v$herenciaYImg <- tags$img(src = 'Ylinked.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(herenciaY())
            herenciaYImgIf <- herenciaY()
            
            if (herenciaYImgIf == 1) {v$herenciaYImg <- tags$img(src = 'Ylinked_azul.svg', align = "center", height="100vh")
            } else{v$herenciaYImg <- tags$img(src = 'Ylinked.svg', align = "center", height="100vh") }
            
      })
      
      output$herenciaYImgOut <- renderUI(v$herenciaYImg)
      
      
      # Mithocondrial
      
      herenciaMitocondrial <- reactive({
            datosHerencia[as.character(enfermedadSeleccionada()) , "Mitochondrial inheritance"]
      })
      
      v$herenciaMitocondrialImg <- tags$img(src = 'mitocondrial.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(herenciaMitocondrial())
            herenciaMitocondrialImgIf <- herenciaMitocondrial()
            
            if (herenciaMitocondrialImgIf == 1) {v$herenciaMitocondrialImg <- tags$img(src = 'mitocondrial_azul.svg', align = "center", height="100vh")
            } else{v$herenciaMitocondrialImg <- tags$img(src = 'mitocondrial.svg', align = "center", height="100vh") }
            
      })
      
      output$herenciaMitocondrialImgOut <- renderUI(v$herenciaMitocondrialImg)
      
      
      
      # Semidominant
      
      herenciaSemidominante <- reactive({
            datosHerencia[as.character(enfermedadSeleccionada()) , "Semi dominant"]
      })
      
      v$herenciaSemidominanteImg <- tags$img(src = 'semidominancia.svg', align = "center", height="100vh")
      
      observeEvent(input$buscadorEnfermedad, {
            
            req(herenciaSemidominante())
            herenciaSemidominanteImgIf <- herenciaSemidominante()
            
            if (herenciaSemidominanteImgIf == 1) {v$herenciaSemidominanteImg <- tags$img(src = 'semidominancia_azul.svg', align = "center", height="100vh")
            } else{v$herenciaSemidominanteImg <- tags$img(src = 'semidominancia.svg', align = "center", height="100vh") }
            
      })
      
      output$herenciaSemidominanteImgOut <- renderUI(v$herenciaSemidominanteImg)
      
      










