# Funciones app tortugas
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Función para hacer las simulaciones de la dinámica
## poblacional de la tortuga amarilla
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


simula <- function(p_o,k_sim,r_pob,r_mod,c_o,emb_range,emb_mod,viaje_range,viajes_mod,temp_range,temp_mod){
  #### Modelo (propuesta de L. Bojorquez)
  db_m <- function(t,y,parms){
    #list(r*y*(1-y/k)- ((cap*y*e*v*temporadas)/(v*B_c)))
    list(r*y*(1-y/k)- ((cap*y*e*v)/(v*B_c)))
  }
  
  ## Tiempo años
  tiempo <- seq(1,100,length=100)
  
  ## Poblacion inicial
  B_c <- as.numeric(p_o)
  ## Capacidad d carga
  k <- k_sim
  ## Tasa de crecimiento
  r <- rtriangle(1, a= min(r_pob),b= max(r_pob),c=r_mod)
  ## Temporadas
  #temporadas <- rtriangle(1, a= min(temp_range),b= max(temp_range),c=temp_mod)
  ## Capturas incidentales
  cap <- c_o#*
  ## Embarcaciones
  e <- rtriangle(1,a=min(emb_range),b=max(emb_range),c=emb_mod)  
  ## Poblacion inicial
  pob_in <- B_c
  ## Numero de viajes
  v <- rtriangle(1,a=min(viaje_range),b=max(viaje_range),c=viajes_mod)
  
  ## Resolviendo numéricamente la ecuación diferencial
  out <- ode(y =pob_in, times= tiempo, func=db_m, parms=NULL,method="rk4")

  return(out)
}


