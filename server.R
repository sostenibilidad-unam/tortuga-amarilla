#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Cargando paquetes necesarios 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library("triangle")
library("shiny")
library("ggplot2")
library("psych")
library("deSolve")
library("splines")
library("MASS")
library("reshape")
library("gridExtra")
library("parallel")
library("grid")
source("funciones.R")

shinyServer(function(input, output, sessionInfo) {
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Definiendo sliders para controlar el rango
  ## de variación de los parámetros del modelo
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  ## slider para el número de embarcaciones
  
  output$embar_range_slider <- renderUI({
    ymin <- 40
    ymax <- 330
    
    sliderInput(inputId = "emb_range",
                label = "Rango de embarcaciones por temporada:",
                min = ymin, max = ymax, value = c(ymin, ymax))
  })
  
  ## slider para el número de temporadas
  
  output$temp_range_slider <- renderUI({
    ymin <- 1
    ymax <- 3
    
    sliderInput(inputId = "temp_range",
                label = "Rango de temporadas:",
                min = ymin, max = ymax, value = c(ymin, ymax))
  })
  
  ## slider para el número de viajes
  
  output$viajes_range_slider <- renderUI({
    ymin <- 15
    ymax <- 25
    
    sliderInput(inputId = "viaje_range",
                label = "Rango de viajes por embarcación:",
                min = ymin, max = ymax, value = c(ymin, ymax))
  })
  
  
  
  simul_reactive <- reactive({
    
    
    ## Número de veces que se correrá la simulación
    valores <- 1:input$nsim
    ## Número de procesos para el cómputo en paralelo
    numWorkers <- parallel::detectCores() -2
    v <- rtriangle(1,a=min(input$viaje_range),b=max(input$viaje_range),c=input$viajes_mod)
    p_o <- as.numeric(input$p_o)
    k_sim <-  as.numeric(input$k_sim)
    r_pob <- as.numeric(input$r_pob)
    r_mod <- as.numeric(input$r_mod)
    emb_range <- input$emb_range
    #temp_range <- input$temp_range
    #temp_mod <- input$temp_mod
    emb_mod <- input$emb_mod
    viaje_range <- input$viaje_range
    viajes_mod <- input$viajes_mod
    
    ## Cómputo en paralelo de las ODEs (la salida es una lista)
    datos1 <- mclapply(valores,FUN=function(x) simula(p_o = p_o,k_sim =k_sim,
                                                     r_pob = r_pob,r_mod = r_mod,c_o = 1,
                                                     emb_range = emb_range,emb_mod = emb_mod,
                                                     viaje_range = viaje_range,viajes_mod = viajes_mod,
                                                     temp_mod = NULL, temp_range = NULL),
                      mc.cores = numWorkers)
    
    
    datos2 <- mclapply(valores,FUN=function(x) simula(p_o = p_o,k_sim =k_sim,
                                                      r_pob = r_pob,r_mod = r_mod,c_o = 2,
                                                      emb_range = emb_range,emb_mod = emb_mod,
                                                      viaje_range = viaje_range,viajes_mod = viajes_mod,
                                                      temp_mod = NULL, temp_range = NULL),
                      mc.cores = numWorkers)
    
    datos3 <- mclapply(valores,FUN=function(x) simula(p_o = p_o,k_sim =k_sim,
                                                      r_pob = r_pob,r_mod = r_mod,c_o = 3,
                                                      emb_range = emb_range,emb_mod = emb_mod,
                                                      viaje_range = viaje_range,viajes_mod = viajes_mod,
                                                      temp_mod = NULL, temp_range = NULL),
                      mc.cores = numWorkers)
    
    datos4 <- mclapply(valores,FUN=function(x) simula(p_o = p_o,k_sim =k_sim,
                                                      r_pob = r_pob,r_mod = r_mod,c_o = 4,
                                                      emb_range = emb_range,emb_mod = emb_mod,
                                                      viaje_range = viaje_range,viajes_mod = viajes_mod,
                                                      temp_mod = NULL, temp_range = NULL),
                      mc.cores = numWorkers)
    
    
    return(list(datos1=datos1,datos2=datos2,datos3=datos3,datos4=datos4))
  })
 
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Función para descargar tabla de datos de la
  ## dinámica poblacional de la tortuga de amarilla
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  do_Table <- reactive({
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Código para convertir la lista del objeto datos a una matriz
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    ## Función para extraer un elemento de la lista (devuleve un vector)
    if(input$c_o==1) datos <- simul_reactive()$datos1
    if(input$c_o==2) datos <- simul_reactive()$datos2
    if(input$c_o==3) datos <- simul_reactive()$datos3
    if(input$c_o==4) datos <- simul_reactive()$datos4
    
    rn <- function(n) return(datos[[n]][,2])
    ## Número de veces que se correrá la simulación
    valores <- 1:input$nsim
    ## Almacena los vectores en una matriz 
    B <- sapply(valores, rn )
    
    colnames(B) <- paste("Sim", seq(1,input$nsim),sep=" ")
    tiempo <- seq(1,100)
    C <- as.data.frame(cbind(tiempo,B))
    
    return(C)
  })
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Imprimiendo la tabla de datos
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  output$tabla_dat <- renderDataTable({
    datos <- do_Table()
    return(datos)
  },options = list(aLengthMenu = c(10, 25, 50, 100), iDisplayLength = 10))
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Método para descargar tabla de datos
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  output$data_table <- downloadHandler(
    filename = "datos.csv",
    content = function(file) {
      write.csv(do_Table(), file,row.names=FALSE)
    }
  )
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Función para descargar la gráfica de la simulación
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  
  doPlot_simu2 <- function(){
    
    datos <- do_Table()
    
    #datos <- lapply(1:length(datos), function(x) {
    #  d <- datos[[x]]
    #  sim <- rep(paste0("sim_",x),dim(d)[1])
    #  d <- data.frame(d,sim)
    #  })
    #datos <- do.call(rbind.data.frame,datos)
    datos <- melt(datos,id.vars="tiempo")
    names(datos) <- c("time","sim","population")

    ## Gráfico base
    p <- ggplot(datos, aes(x=time,y=population,colour=sim))
    p <- p + geom_line() #+  ylim(-150, max(media))
    p <- p + xlab("Tiempo") + ylab("Número de Individuos") 
    p <- p + theme_bw()
    p <- p + coord_cartesian(ylim = c(0, 50000)) 
    p <- p + scale_x_continuous(expand = c(0, 0), breaks=seq(0,100,length=5))
    p <- p + ggtitle("Simulaciones de la dinámica \npoblacional de la tortuga amarilla")
    p <- p +  scale_colour_manual(values=sample(1:input$nsim,replace = T))
    p <- p +  theme(panel.grid.major.x = element_line(size=0.5),
                    panel.grid.major.y = element_line(size=0.5),
                    legend.position= "none",
                    plot.title=element_text(lineheight=.8, face="bold"),
                    #plot.title = element_text(size = rel(1.2),face="bold"),
                    axis.title.y = element_text(size = rel(1.1), angle = 90,face="bold"),
                    axis.title.x = element_text(size = rel(1.1), face="bold"))
    print(p)
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Gráfica de la figura 10 
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  output$grafica_sim <- renderPlot({
    
    doPlot_simu2()
    
  })
  
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Método para descargar gráfica de las simulaciones (figura 10)
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  output$grafica <- downloadHandler(
    filename = 'grafica.pdf',
    
    content = function(file){
      pdf(file = file, width=8, height=6)
      doPlot_simu2()
      dev.off()
    }
  )
  

  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Función para descargar la gráfica de dinámica promedio
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  doPlot_prom <- function(){
    B <- do_Table()
    tiempo <- seq(1,100,length=100)
    media <- apply(B,MARGIN=1,FUN=mean)
    #media <- colMeans(B,na.rm=TRUE)
    std <- function(x) sd(x)/sqrt(length(x))
    se <- apply(B,MARGIN=1, FUN=std)
    c_sup <- media + 1.96*se
    c_in <- media - 1.96*se
    datos <- data.frame(media = media,tiempo=tiempo, error_st=se, c_sup,c_in)
    #print(datos)
    
    ## Gráfico base
    p <- ggplot(datos, aes(x=tiempo,y=media))
    ## Límites de las bandas de error 
    limits <- aes(ymax = media + error_st, ymin=media - error_st)
    p <- p + geom_line() #+  ylim(-150, max(media))
    p <- p + geom_ribbon(data=datos,aes(ymin=c_in,ymax=c_sup),alpha=0.5)
    p <- p + xlab("Tiempo") + ylab("Número de Individuos") 
    p <- p + theme(plot.title=element_text(lineheight=.8, face="bold")) + theme_bw()
    p <- p + coord_cartesian(ylim = c(0, 50000)) 
    p <- p + scale_x_continuous(expand = c(0, 0), breaks=seq(0,100,length=5))
    p <- p + theme(panel.grid.major.x = element_line(size=0.5),
                     panel.grid.major.y = element_line(size=0.5))
    p<- p +  theme(panel.grid.major.x = element_line(size=0.5),
                   panel.grid.major.y = element_line(size=0.5),
                   legend.position= "none",
                   plot.title=element_text(lineheight=.8, face="bold"),
                   #plot.title = element_text(size = rel(1.2),face="bold"),
                   axis.title.y = element_text(size = rel(1.1), angle = 90,face="bold"),
                   axis.title.x = element_text(size = rel(1.1), face="bold"))
    
    ## Avisar si el RIESGO DE DISMINU es INACEPTABLE: si la población en el tiempo t= 20 
    ## es menor que el 25% de la población inicial o si la captura incidental es mayor a 3
    
    pob_20_anios <- datos$media[20]
    p_ini <- as.numeric(input$p_o)
    pob_25_porciento <- p_ini*0.75
    # input$c_o > 2 ||
    if(input$c_o > 2 ||pob_20_anios < pob_25_porciento){
      p <- p +  ggtitle("Dinámica poblacional\n promedio de la tortuga amarilla (RIESGO INACEPTABLE)")
      p <- p +  theme(plot.title = element_text(colour = "red"))
      print(p)
    } else {
      p <- p +  ggtitle("Dinámica poblacional\n promedio de la tortuga amarilla")
      print(p)
    }
    
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Estadísticas de las corridas
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  output$graf_esta <- renderPlot({
    doPlot_prom()
  })
  output$estat <- renderPrint({
    B <- do_Table()
    media <- apply(B[,-1],MARGIN=2,FUN=mean)

    std <- function(x) sd(x)/sqrt(length(x))
    se <- apply(B[,-1], 2,FUN=std)
    
    cat("-----------------------------------------------------------------------------------\n")
    cat("               Estadísticos descriptivos para de las simulaciones                  \n")
    cat("-----------------------------------------------------------------------------------\n")
    print(describe(media)[c(2,3,4,5,7,8,9,10,11,12)])
    cat("-----------------------------------------------------------------------------------\n")
   

  })
  
  output$graf_promedio <- downloadHandler(
    filename = "graf_promed.pdf",
    content <- function(file){
      pdf(file = file, width=8, height=6)
      doPlot_prom()
      dev.off()
    }
    
  )
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Código para la figura 14
  ## función para hacer la gráfica
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  doPlot_14 <- function(){
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Función para calcular el número de individuos capturados
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    simula2 <- function(...){
     
      ## Tiempo años
      tiempo <- seq(1,100)
      ## Población inicial
      B_c <- as.numeric(input$p_o)
      ## Número de embarcaciones
      e <- rtriangle(1,a=min(input$emb_range),b=max(input$emb_range),c=input$emb_mod) 
      ## Capacidad d carga
      k <- input$k_sim
      ## Número de viajes por embarcación
      v <- rtriangle(1,a=min(input$viaje_range),b=max(input$viaje_range),c=input$viajes_mod)
      ## Tasa de crecimiento
      r <- rtriangle(1, a= min(input$r_pob),b= max(input$r_pob),c=input$r_mod)
      ## Capturas incidentales
      #cap <-sample(1:c1,1)
      cap <- c1
      ## Esfuezo de captura
      f <- e*v
      ##  Coeficiente de capturabilidad para c=1
      q_1 <- cap*e/(f*B_c)
      pob_in <- B_c
      ## Vector donde se guardan a los valores del número de capturas
      Ct <- numeric(length(tiempo))

      ## Función para extraer un elemento de la lista (devuleve un vector)
      if(input$c_o==1) datos <- simul_reactive()$datos1
      if(input$c_o==2) datos <- simul_reactive()$datos2
      if(input$c_o==3) datos <- simul_reactive()$datos3
      if(input$c_o==4) datos <- simul_reactive()$datos4
      
      Ct <- sapply(datos, function(x){

        for(i in 1:length(tiempo)){
          Ct[i] <- cap*f*q_1*x[,2][i]
          if(Ct[i]<0) Ct[i] <-0
        }
        
        return(Ct)
      })
     
      return(Ct)
    }
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Función paralelizada para cálcular el número de individuos 
    ## capturados después de nsimulaciones 
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    simul2 <- function(nsim, B_c, c1,tao=FALSE){
      c1 <<- c1
      ## Cómputo en paralelo de las ODEs (la salida es una lista)
      datos <- simula2()
      return(datos)
    }
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Función para remodelar los datos para poderos gráficar con ggplot
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    mmm <- function(Ct){
      media_Ct <- apply(Ct,1,FUN=mean)
      min_Ct<- apply(Ct,1,FUN=min)
      max_Ct <- apply(Ct,1,FUN=max)   
      tiempo <- seq(1,100)
      datos1 <- data.frame(tiempo =tiempo,Promedio=media_Ct, 
                           Mínimo=min_Ct ,Máximo=max_Ct)

      datos <- melt(datos1,id.vars="tiempo")

      return(datos)
    }
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Número de individuos capturados por cada nivel de captura incidental
    ## c1 = número de capturas captura; 
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    # 1 captura
    Ct_c1 <- simul2(nsim=input$nsim,B_c=input$p_o,c1=1) 
    datos_c1 <- mmm(Ct_c1)
    # 2 capturas
    Ct_c2 <- simul2(nsim=input$nsim,B_c=input$p_o,c1=2) 
    datos_c2 <- mmm(Ct_c2)
    # 3 capturas
    Ct_c3 <- simul2(nsim=input$nsim,B_c=input$p_o,c1=3) 
    datos_c3 <- mmm(Ct_c3)
    # 4 Capturas
    Ct_c4 <- simul2(nsim=input$nsim,B_c=input$p_o,c1=4) 
    datos_c4 <- mmm(Ct_c4)
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Gráficas de número de individuos capturados utilizando ggplot2
    ## c1 = 1 captura; c2 = 2 capturas; c3 = 3 capturas; c4 = 4 capturas 
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    c1 <- ggplot(datos_c1, aes(tiempo, value,colour = variable))
    c1 <- c1 + geom_line() 
    #print(datos_c1)
    c1 <- c1 + theme(
      #axis.ticks= element_blank())#,
      #axis.title.x = element_blank(), axis.title.y =  element_blank(),
      axis.ticks = element_blank()) + theme_bw()
    c1 <- c1 + ggtitle("Hasta 1 individuo")
    c1 <- c1 + xlab("") + ylab("") 
    c1 <- c1 + scale_x_continuous(breaks=seq(0,100,length=5),expand = c(0, 0))
    c1 <- c1 + theme(panel.grid.major.x = element_line(linetype="dotted", colour="grey50",size=0.5),
                     panel.grid.major.y = element_line(linetype="dotted", colour="grey50",size=0.5))

    c2 <- ggplot(datos_c2, aes(tiempo, value,colour = variable))
    c2 <- c2  + geom_line() 
    
    c2 <- c2 + theme(
      #axis.ticks= element_blank())#,
      #axis.title.x = element_blank(), axis.title.y =  element_blank(),
      axis.ticks = element_blank()) + theme_bw()
    c2 <- c2 + ggtitle("Hasta 2 individuos")
    c2 <- c2 + xlab("") + ylab("") 
    c2 <- c2 + scale_x_continuous(breaks=seq(0,100,length=5),expand = c(0, 0))
    c2 <- c2 + theme(panel.grid.major.x = element_line(linetype="dotted", colour="grey50",size=0.5),
                     panel.grid.major.y = element_line(linetype="dotted", colour="grey50",size=0.5))
    
    
    c3 <- ggplot(datos_c3, aes(tiempo, value,colour = variable))
    c3 <- c3 + geom_line() 
    
    c3 <- c3+ theme(
      #axis.ticks= element_blank())#,
      #axis.title.x = element_blank(), axis.title.y =  element_blank(),
      axis.ticks = element_blank()) + theme_bw()
    c3 <- c3 + ggtitle("Hasta 3 individuos")
    c3 <- c3 + xlab("") + ylab("") 
    c3 <- c3 + scale_x_continuous(breaks=seq(0,100,length=5),expand = c(0, 0))
    c3 <- c3 + theme(panel.grid.major.x = element_line(linetype="dotted", colour="grey50",size=0.5),
                     panel.grid.major.y = element_line(linetype="dotted", colour="grey50",size=0.5))
    
    c4 <- ggplot(datos_c4, aes(tiempo, value,colour = variable))
    c4 <- c4 + geom_line() 
    
    
    c4 <- c4 + theme(
      #axis.ticks= element_blank())#,
      #axis.title.x = element_blank(), axis.title.y =  element_blank(),
      axis.ticks = element_blank()) + theme_bw()
    c4 <- c4 + ggtitle("Hasta 4 individuos")
    c4 <- c4 + xlab("") + ylab("") 
    c4 <- c4 + scale_x_continuous(breaks=seq(0,100,length=5),expand = c(0, 0))
    c4 <- c4 + theme(panel.grid.major.x = element_line(linetype="dotted", colour="grey50",size=0.5),
                     panel.grid.major.y = element_line(linetype="dotted", colour="grey50",size=0.5))
    ## Gráficos de los datos en una sola hoja
    if(input$c_o == 1){
      
      c1 <- c1 + theme(legend.title = element_text(size=12, face="bold"),
                       legend.position=c(0.85,0.9),legend.text = element_text(size = 10, face = "bold"))
      print(grid.arrange(arrangeGrob(c1, nrow=1),
                         heights=c(15, 1), 
                         left=grid.text("Probabilidad de disminución", 
                                        gp=gpar(fontsize=20), 
                                        check=TRUE,rot=450) ,
                         sub=grid.text("Tiempo",
                                       gp=gpar(fontsize=20), check=TRUE)
      ))
    }
    else if(input$c_o == 2){
      print(grid.arrange(arrangeGrob(c2 + theme(legend.position= "none"),
                                     c1 + theme(legend.title = element_text(size=12, face="bold"),
                                                legend.position=c(0.85,.9), legend.text = element_text(size = 10, face = "bold")),
                                     nrow=1),
                         nrow=2, heights=c(15, 1), 
                         left=grid.text("Individuos capturados", 
                                        gp=gpar(fontsize=20), 
                                        check=TRUE,rot=450) ,
                         sub=grid.text("Tiempo",
                                       gp=gpar(fontsize=20), check=TRUE)
      )) 
    }
    else if(input$c_o == 3){
      print(grid.arrange(arrangeGrob(c3 + theme(legend.position= "none"),
                                     c2 + theme(legend.title = element_text(size=12, face="bold"),
                                                legend.position=c(0.85,0.75), legend.text = element_text(size = 10, face = "bold")),
                                     c1 + theme(legend.position="none"),
                                     nrow=2),
                         nrow=2, heights=c(15, 1), 
                         left=grid.text("Individuos capturados", 
                                        gp=gpar(fontsize=20), 
                                        check=TRUE,rot=450) ,
                         sub=grid.text("Tiempo",
                                       gp=gpar(fontsize=20), check=TRUE)
      )) 
    }
    else if(input$c_o == 4){
      print(grid.arrange(arrangeGrob(c4 + theme(legend.position= "none"),
                                     c3 + theme(legend.title = element_text(size=12, face="bold"),
                                                legend.position=c(0.85,0.75),legend.text = element_text(size = 10, face = "bold")),
                                     c2 + theme(legend.position="none"),
                                     c1 + theme(legend.position="none"),
                                     nrow=2),
                         nrow=2, heights=c(15, 1), 
                         left=grid.text("Individuos capturados", 
                                        gp=gpar(fontsize=20), 
                                        check=TRUE,rot=450) ,
                         sub=grid.text("Tiempo",
                                       gp=gpar(fontsize=20), check=TRUE)
      )) 
    }
    
  }
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Figura 14 en el panel
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$figura_14 <- renderPlot({
    e <- rtriangle(1,a=min(input$emb_range),b=max(input$emb_range),c=input$emb_mod) 
    ## Capacidad d carga
    k <- input$k_sim
    v <- rtriangle(1,a=min(input$viaje_range),b=max(input$viaje_range),c=input$viajes_mod)
    ## Tasa de crecimiento
    r <- rtriangle(1, a= min(input$r_pob),b= max(input$r_pob),c=input$r_mod)
    B_c <- input$p_o
    doPlot_14()
  })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Método para descargar la gráfica 14
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$figura_14_des <- downloadHandler(
    filename = 'n_totalCapturas.pdf',
    content = function(file){
      pdf(file = file, width=10, height=6)
      doPlot_14()
      dev.off()
    }
  )
  

  simul3 <- function(nsim, B_c, c1,tao=FALSE){
    
    valores <- 1:nsim
    tiempo <- seq(1,100)
    ## Función para extraer un elemento de la lista (devuleve un vector)
    if(c1==1) datos <- simul_reactive()$datos1
    if(c1==2) datos <- simul_reactive()$datos2
    if(c1==3) datos <- simul_reactive()$datos3
    if(c1==4) datos <- simul_reactive()$datos4
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Código para convertir la lista del objeto datos a una matriz
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    ## Función para extraer un elemento de la lista (devuleve un vector)
    
    rn <- function(n) return(datos[[n]][,2])
    ## Matriz de datos
    B <- sapply(valores, rn )
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Cálculo de porcentaje de disminución según el tamaño de la población inicial
    ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    dismi_75 <- B_c*0.25 ## Disminución del 75%
    dismi_50 <- B_c*0.50 ## Disminución del 50%
    dismi_25 <- B_c*0.75 ## Disminución del 25%
    
    mini_75 <- function(x) which(x <= dismi_75)
    mini_50 <- function(x) which(x <= dismi_50)
    mini_25 <- function(x) which(x <= dismi_25)
    div <- function(x) x/nsim
    
    ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Diminución del 75%
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    d_75 <- apply(B, MARGIN=1,mini_75)
    d_75 <- lapply(d_75, FUN=length)
    d_75 <- lapply(d_75,div)
    g_75 <- unlist(d_75)
    if(is.null(g_75)) g_75 <- rep(0,length(tiempo))
    
    ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Diminución del 50%
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    d_50 <- apply(B, MARGIN=1,mini_50)
    d_50 <- lapply(d_50, FUN=length)
    d_50 <- lapply(d_50,div)
    g_50 <- unlist(d_50)
    if(is.null(g_50)) g_50 <- rep(0,length(tiempo))
    
    ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Diminución del 25%
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    d_25 <- apply(B, MARGIN=1,mini_25)
    d_25 <- lapply(d_25, FUN=length)
    d_25 <- lapply(d_25,div)
    g_25 <- unlist(d_25)
    if(is.null(g_25)) g_25 <- rep(0,length(tiempo))
    datos <- data.frame(Tiempo = tiempo, "Pérdida 25%"=g_25,
                        "Pérdida 50%"=g_50, "Pérdida 75%"=g_75)
    
    datos <- melt(datos,id.vars="Tiempo")
    return(datos)
  }
  
  simul3_reactive <- reactive({
    
    datos_c1 <- simul3(nsim=input$nsim,B_c=as.numeric(input$p_o),c1=1)
    datos_c2 <- simul3(nsim=input$nsim,B_c=as.numeric(input$p_o),c1=2)
    datos_c3 <- simul3(nsim=input$nsim,B_c=as.numeric(input$p_o),c1=3)
    datos_c4 <- simul3(nsim=input$nsim,B_c=as.numeric(input$p_o),c1=4)
    
    return(list(datos_c1=datos_c1,datos_c2=datos_c2,
                datos_c3=datos_c3,datos_c4=datos_c4))
    
  })
  
  
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Código para hacer las gráficas de probabilidad de disminución
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  grafi_15 <- function(){
    ## Capacidad d carga
    k <- input$k_sim
    ## Tasa de crecimiento
    r <- rtriangle(1, a= min(input$r_pob),b= max(input$r_pob),c=input$r_mod)
    
    e <- rtriangle(1,a=min(input$emb_range),b=max(input$emb_range),c=input$emb_mod) 
    v <- rtriangle(1,a=min(input$viaje_range),b=max(input$viaje_range),c=input$viajes_mod)
    
    datos_c1 <- simul3_reactive()$datos_c1
    datos_c2 <- simul3_reactive()$datos_c2
    datos_c3 <- simul3_reactive()$datos_c3
    datos_c4 <- simul3_reactive()$datos_c4
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## Gráficas la probabilidad de disminución  utilizando ggplot2
    ## c1 = 1 captura; c2 = 2 capturas; c3 = 3 capturas; c4 = 4 capturas 
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    c1 <- ggplot(datos_c1, aes(Tiempo, value,colour = variable))
    c1 <- c1+ geom_line() + theme_bw() + coord_cartesian(ylim = c(0, 0.5))
    c1 <- c1 + ggtitle("Hasta 1 individuo")
    c1 <- c1 + xlab("") + ylab("") 
    c1 <- c1 + scale_x_continuous(breaks=seq(0,100,length=5),expand = c(0, 0))
    c1 <- c1 + theme(panel.grid.major.x = element_line(linetype="dotted", colour="grey50",size=0.5),
                     panel.grid.major.y = element_line(linetype="dotted", colour="grey50",size=0.5))
    
    c2 <- ggplot(datos_c2, aes(Tiempo, value,colour = variable))
    c2 <- c2 + geom_line() + theme_bw() + coord_cartesian(ylim = c(0, 0.5))
    c2 <- c2 + ggtitle("Hasta 2 individuos")
    c2 <- c2 + xlab("") + ylab("")  
    c2 <- c2 + scale_x_continuous(expand = c(0, 0), breaks=seq(0,100,length=5))
    c2 <- c2 + theme(panel.grid.major.x = element_line(linetype="dotted", colour="grey50",size=0.5),
                     panel.grid.major.y = element_line(linetype="dotted", colour="grey50",size=0.5))
    
    c3 <- ggplot(datos_c3, aes(Tiempo, value,colour = variable))
    c3 <- c3 + geom_line() + theme_bw() + coord_cartesian(ylim = c(0, 0.5))
    c3 <- c3 + ggtitle("Hasta 3 individuos")
    c3 <- c3 + xlab("") + ylab("")  
    c3 <- c3 + scale_x_continuous(expand = c(0, 0), breaks=seq(0,100,length=5))
    c3 <- c3 + theme(panel.grid.major.x = element_line(linetype="dotted", colour="grey50",size=0.5),
                     panel.grid.major.y = element_line(linetype="dotted", colour="grey50",size=0.5))
    
    c4 <- ggplot(datos_c4, aes(Tiempo, value,colour = variable))
    c4 <- c4 + geom_line() + theme_bw() + coord_cartesian(ylim = c(0, 0.5))
    c4 <- c4 + ggtitle("Hasta 4 individuos")
    c4 <- c4 + xlab("") + ylab("")  
    c4 <- c4 + scale_x_continuous(expand = c(0, 0), breaks=seq(0,100,length=5))
    c4 <- c4 + theme(panel.grid.major.x = element_line(linetype="dotted", colour="grey50",size=0.5),
                       panel.grid.major.y = element_line(linetype="dotted", colour="grey50",size=0.5))
    # Código para leyenda 
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}
    
    leyenda <- g_legend(c1)
    
    if(input$c_o == 1){
      
      c1 <- c1 + theme(legend.title = element_text(size=12, face="bold"),
                       legend.position=c(0.85,0.9),legend.text = element_text(size = 10, face = "bold"))
      print(grid.arrange(arrangeGrob(c1, nrow=1),
                         heights=c(15, 1), 
                         left=grid.text("Probabilidad de disminución", 
                                        gp=gpar(fontsize=20), 
                                        check=TRUE,rot=450) ,
                         sub=grid.text("Tiempo",
                                       gp=gpar(fontsize=20), check=TRUE)
      ))
    }
    else if(input$c_o == 2){
      print(grid.arrange(arrangeGrob(c2 + theme(legend.position= "none"),
                                     c1 + theme(legend.title = element_text(size=12, face="bold"),
                                                legend.position=c(0.85,0.75),legend.text = element_text(size = 10, face = "bold")),
                                     nrow=1),
                         nrow=2, heights=c(15, 1), 
                         left=grid.text("Probabilidad de disminución", 
                                        gp=gpar(fontsize=20), 
                                        check=TRUE,rot=450) ,
                         sub=grid.text("Tiempo",
                                       gp=gpar(fontsize=20), check=TRUE)
      )) 
    }
    else if(input$c_o == 3){
      print(grid.arrange(arrangeGrob(c3 + theme(legend.position= "none"),
                                     c2 + theme(legend.title = element_text(size=12, face="bold"),
                                                legend.position=c(0.85,0.75),legend.text = element_text(size = 10, face = "bold")),
                                     c1 + theme(legend.position="none"),
                                     nrow=2),
                         nrow=2, heights=c(15, 1), 
                         left=grid.text("Probabilidad de disminución", 
                                        gp=gpar(fontsize=20), 
                                        check=TRUE,rot=450) ,
                         sub=grid.text("Tiempo",
                                       gp=gpar(fontsize=20), check=TRUE)
      )) 
    }
    else if(input$c_o == 4){
      print(grid.arrange(arrangeGrob(c4 + theme(legend.position= "none"),
                                     c3 + theme(legend.title = element_text(size=12, face="bold"),
                                                legend.position=c(0.85,0.9),legend.text = element_text(size = 10, face = "bold")),
                                     c2 + theme(legend.position="none"),
                                     c1 + theme(legend.position="none"),
                                     nrow=2),
                         nrow=2, heights=c(15, 1), 
                         left=grid.text("Probabilidad de disminución", 
                                        gp=gpar(fontsize=20), 
                                        check=TRUE,rot=450) ,
                         sub=grid.text("Tiempo",
                                       gp=gpar(fontsize=20), check=TRUE)
      )) 
    }
  }
  
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Figura 15 en el panel principal
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$fig15 <- renderPlot({
    
    grafi_15()
  })
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## Método para descargar la figura 15
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$figura_15_des <- downloadHandler(
    filename = "riesgo_disminu.pdf",
    content = function(file){
      pdf(file = file, width=10, height=6)
      grafi_15()
      dev.off()
    }
  )
})

