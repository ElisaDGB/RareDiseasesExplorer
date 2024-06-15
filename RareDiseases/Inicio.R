################################################################################
#                              Inicio/Home tab
################################################################################



#  Age Onset

output$hc_BarcharEdadAparicion<- renderHighchart(highchart() %>%
                                                       hc_add_series(datosEdadAparicionSum, type="bar", hcaes(x=X1,  y=X2), dataLabels = list(enabled = TRUE)) %>%
                                                       hc_xAxis(categories = unique(datosEdadAparicionSum$X1)) %>%
                                                       hc_legend(enabled = FALSE)
)


#  Inheritance 

output$hc_BarcharHerencia<- renderHighchart(highchart() %>%
                                                hc_add_series(datosHerenciaSum, type="bar", hcaes(x=X1,  y=X2), dataLabels = list(enabled = TRUE)) %>%
                                                hc_xAxis(categories = unique(datosHerenciaSum$X1)) %>%
                                                hc_legend(enabled = FALSE) 
)

#  Symptoms 


output$hc_PackedBubble <- renderHighchart(highchart( ) %>%
      hc_add_series(datosGruposBubble, type="packedbubble",  hcaes(name = Grupo2, value = size, group = Grupo1)) %>%
      hc_plotOptions(packedbubble = list(
            layoutAlgorithm = list(
                  splitSeries = TRUE, gravitationalConstant =  0.95),
            dataLabels = list(
                  enabled = TRUE,
                  format = "{point.parent.name}"))) %>%
      hc_legend(enabled = TRUE)%>% 
      hc_tooltip(
            useHTML = TRUE,
            pointFormat = "{point.name}"
      ))






