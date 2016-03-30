library(shiny)
library("shinyIncubator")
library("shinysky")
#tabPanelAbout <- source("about.R")$value
shinyUI(pageWithSidebar(
  
  headerPanel(
    HTML('
         <div id="lancis">
         Conflicto pesca-conservación 
         <a href="http://lancis.ecologia.unam.mx/web/" target="_blank">
         <img id="logo_lancisi" align="right" alt="Logo LANCIS" 
         src="http://web.ecologia.unam.mx/laboratorios/aescalante/Site/L%C3%ADnea_de_investigación_files/Screen%20Shot%202013-07-30%20at%2010.06.15%20AM.png" 
         HEIGHT="70" WIDTH="200" BORDER="0"/>
         </a>
         <p>Tortuga amarilla  </p>
         </div>'
    ),
    "Modelo logístico aplicado"),
  
  sidebarPanel(
    HTML("<h4>Dinámica poblacional de la tortuga amarilla 
         </h4>"),
    br(),
    wellPanel(
      HTML("<p align ='justify'>Esta aplicación ejemplifica la dinámica de la población de individuos 
           tortuga amarilla que mueren atrapados en las redes de enmalle de manera incidental.
           El modelo dinámico se deriva del modelo logístico: $$\\frac{dB}{dt}=rB_{t}\\left(1-\\frac{B_{t}}{K}\\right)$$￼
           ￼￼￼ (<a href='https://www.dropbox.com/s/4l123t7qqq3r2v1/POEMRPN_Tortugas_FINAL-1%20Ago%2031.pdf?dl=0' target='_blank'>Para saber más</a>)</p>")
      ),
    tags$head(
      tags$script(src = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", 
                  type = 'text/javascript')
    ),
    
    sliderInput("nsim", 
                "Número de simulaciones:", 
                min = 500, 
                max = 1000,
                step = 100,
                value = 500)
    ,
    #actionButton(input="buscar",label="Actualizar",styleclass="primary"),
    selectInput("p_o", 
                "Población inicial:", 
                choices=c("43000", "30000",
                          "25000", "20000",
                          "15000", "10000"),
                selected= "15000")
    ,
    conditionalPanel(condition="input.tsp =='graf_sim' || input.tsp == 'tab_prom' || input.tsp== 'tab_dat' || input.tsp=='tab_captu' || input.tsp== 'tab_fig14' || input.tsp== 'tab_fig15'",
                     sliderInput("c_o", 
                                 "Captura incidental:", 
                                 min = 1, 
                                 max = 4, 
                                 value = 2)
    )    
    
    ,
    conditionalPanel(condition="input.tsp == 'graf_sim' || input.tsp == 'tab_dat' || input.tsp == 'tab_prom'
                     || input.tsp == 'tab_fig14' || input.tsp =='tab_fig15'",
                     sliderInput("r_pob", 
                                 "Tasa de crecimiento:", 
                                 min = 0.005, 
                                 max = 0.02, 
                                 #step = 0.001,
                                 value =c(0.005,0.02)),
                     numericInput(inputId="r_mod",label="Moda de la tasa de crecimiento",
                                  value=0.01,min=0.005,
                                  max=0.02,step=0.008)
                     ,

                     uiOutput("embar_range_slider")
                     ,
                     numericInput(inputId="emb_mod",label="Moda embarcaciones:",
                                  value=117,min=40,
                                  max=330,step=40)
                     ,
                     #uiOutput("temp_range_slider")
                     #,
                     #numericInput(inputId="temp_mod",label="Moda temporadas:",
                     #            value=1,min=1,max=3,step=0.5)
                     #,
                     uiOutput("viajes_range_slider")
                     ,
                     numericInput(inputId="viajes_mod",label="Moda viajes:",
                                  value=18,min=15,max=25,step=1)
                     ,
                     sliderInput(inputId = "k_sim",
                                 label = "Capacidad de carga (K):",
                                 min = 30000,
                                 max = 45000,
                                 value = 43000)
    )
    ,
    
    conditionalPanel(condition="input.tsp == 'tab_captu'",
                     sliderInput(inputId='embar',
                                 label='Número de embarcaciones',
                                 min = 25,
                                 max = 140,
                                 value= 117),
                     sliderInput(inputId='viajes',
                                 label="Número de viajes por embarcación",
                                 min = 15,
                                 max = 45,
                                 value=25))
    
    ,
    
    #conditionalPanel(condition= "input.tsp == 'graf_sim'",
    #                 wellPanel(
    #                   helpText("Opciones de la gráfica"),
    #                   checkboxInput(inputId="leyenda",
    #                                 label="Mostrar leyenda"),
    #                   checkboxInput(inputId="med_sim",
    #                                 label="Mostrar linea de tendencia")
    #                 ))
    #,
    
    wellPanel(
      conditionalPanel(condition="input.tsp == 'graf_sim'", 
                       downloadButton(outputId="grafica",label="Descargar gráfica"))
      ,
      conditionalPanel(condition="input.tsp == 'tab_dat'", 
                       downloadButton(outputId="data_table",label="Descargar datos"))
      ,
      conditionalPanel(condition= "input.tsp == 'tab_prom'",
                       downloadButton(outputId='graf_promedio', label ="Descargar gráfica"))
      ,
      conditionalPanel(condition="input.tsp == 'tab_captu'",
                       downloadButton(outputId="grafica_cap",label="Descargar gráfica"))
      ,
      conditionalPanel(condition="input.tsp== 'tab_fig14'",
                       downloadButton(outputId="figura_14_des", label="Descargar gráfica"))
      ,
      conditionalPanel(condition="input.tsp== 'tab_fig15'",
                       downloadButton(outputId="figura_15_des", label="Descargar gráfica"))
      
    )    
    
    #wellPanel(
    #  HTML("Código implementado por: <br> <b> Luis Osorio </b> <br><b>Olvier López</b>")
    #  )
    
    ),
  
  mainPanel(
    tabsetPanel(id="tsp",
                tabPanel(value= "tab_prom",title="Resultados", plotOutput(outputId="graf_esta",height="500px"), HTML('<div align="center">
                                                                                                                     <h5>Figura 1. Cambio del tamaño de la población de juveniles de tortuga amarilla del 
                                                                                                                        Golfo de Ulloa respecto a la población inicial para un escenario de 100 años.</h5></div>'), 
                         h2("Estadísticas descriptivas de las simulaciones"),verbatimTextOutput(outputId = "estat")),
                tabPanel(value="tab_fig15",title="Riesgo", plotOutput(outputId="fig15"), HTML("<div align='center'>
                                                                                              <h5>Figura 2. Riesgo de disminución de la población de juveniles de tortuga amarilla del Golfo de Ulloa. 
                                                                                                  La linea roja corresponde a probabilidades de disminución del 25% de la población. La linea verde a 
                                                                                                  50% de disminución. La linea azul del 75%.</h5></div>")),
                tabPanel(value="tab_fig14",title="Capturas", plotOutput(outputId="figura_14",height="500px"), HTML("<div align='center'>
                                                                                                                     <h5>Figura 3. Cambio máximo (azul), promedio (rojo) y mínimo (verde) del número total individuos de 
                                                                                                                                   tortuga amarilla del Golfo de Ulloa capturados incidentalmente.</h3></div>")),
                #tabPanel(value="tab_captu", title="Coef. Capturabilidad",plotOutput(outputId="captu_inci",height="500px")),
                tabPanel(value="graf_sim",title="Simulaciones", plotOutput(outputId="grafica_sim",height="500px"), HTML('<div align="center">
                                                                                                                        <h5>Figura 4. Simulaciones de la dinámica poblacional de la tortuga amarilla. En la gráfica se muestran
                                                                                                                        los posibles escenarios crecimiento (o decremento) de la población de tortugas para los valores de los parámetros
                                                                                                                        seleccionados.</h5></div>')),
                tabPanel(value="tab_dat",title="Datos", dataTableOutput('tabla_dat'))
                #tabPanelAbout()
                )
    )
    ))

