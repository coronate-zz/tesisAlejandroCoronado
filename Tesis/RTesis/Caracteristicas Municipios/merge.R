print('-----------Cargando Datos----------')
vivienda       <-readRDS('vivienda.rds')
urbanizacion   <-readRDS('urbanizacion.rds')
desarrollo     <-readRDS('desarrolloHumano.rds')
educacion      <-readRDS('educacion.rds')
poblacion      <-readRDS('poblacion.rds')
delitos        <-readRDS('delitos.rds')
energia        <-readRDS('energiaElectrica.rds')
data           <-readRDS('violencia.rds')

bdatos<-c(vivienda,
          desarrollo,
          educacion,
          poblacion,
          delitos,
          energia,
          data)

print('-------Generando Claves-----')
vivienda  ['clave']<-paste(vivienda$miunicipioID,   vivienda$anio)
desarrollo['clave']<-paste(desarrollo$miunicipioID, desarrollo$anio)
educacion ['clave']<-paste(educacion$miunicipioID,  educacion$anio)
poblacion ['clave']<-paste(poblacion$miunicipioID,  poblacion$anio)
delitos   ['clave']<-paste(delitos$miunicipioID,    delitos$anio)
energia   ['clave']<-paste(energia$miunicipioID, energia$anio)
urbanizacion['clave'] <-paste(urbanizacion$miunicipioID, urbanizacion$anio)

#No necesito todas las variables si no planeo usarlas, solo seleccionare las que em interesan
print('----Clasificando Informacion----')
energia<-cbind(energia$clave, energia$Usuarios.de.energ.a.el.ctrica.a., energia$Volumen.de.las.ventas.de.energ.a.el.ctrica..megawatts.hora. )
colnames(energia)<-c('clave', 'usuarios_energia_electrica', 'volumen_ventas_megawattshora')

poblacion<-cbind(poblacion$clave, poblacion$PoblaciÑn.total, poblacion$Promedio.de.hijos.nacidos.vivos.de.las.mujeres.de.12.y.màs.aÐos, poblacion$RelaciÑn.hombres.mujeres.d., poblacion$Hogares , poblacion$TamaÐo.promedio.de.los.hogares.c.  )
colnames(poblacion)<-c('clave', 'poblacion_total', 'promedio_hijos_nacidos_vivos', 'ratio_hombres_mujeres', 'numero_hogares', 'tamaño_promedio_hogares')

urbanizacion<-cbind(urbanizacion$clave, urbanizacion$Localidades.con.red.de.distribuci.n.de.agua.entubada.a., urbanizacion$Localidades.con.el.servicio.de.drenaje.y.alcantarillado.a., urbanizacion$Localidades.con.el.servicio.de.energ.a.el.ctrica.a.  )
colnames(urbanizacion)<-c('clave', 'localidades_agua_entubada', 'localidades_denaje', 'localidades_energia_electrica')

educacion<-cbind(educacion$clave, educacion$Poblaci.n.de.6.y.m.s.a.os.por.condici.n.para.leer.y.escribir..y.sexo, educacion$Grado.promedio.de.escolaridad.de.la.poblaci.n.de.15.y.m.s.a.os  )
colnames(educacion)<-c('clave', 'poblacion_leer_escribir_mayor6', 'promedio_escolaridad_mayor15')

delitos<-cbind(delitos$clave, delitos$Total.delitos, delitos$Homicidio, delitos$Robo  )
colnames(delitos)<-c('clave', 'total_delitos', 'homicidios', 'robos')

desarrollo<-cbind(desarrollo$clave, desarrollo$êndice.de.desarrollo.humano.con.servicios.d., desarrollo$êndice.de.desarrollo.humano.per.c.pita.e.  )
colnames(desarrollo)<-c('clave', 'desarrollo_humano_servicios', 'desarrollo_humano_percapita')

data<-data
#data<-merge(x =data, y = vivienda,       by = "clave", all.x = TRUE)
data<-merge(x =data, y = poblacion,      by = "clave", all.x = TRUE)
data<-merge(x =data, y = desarrollo,     by = "clave", all.x = TRUE)
data<-merge(x =data, y = educacion,      by = "clave", all.x = TRUE)
data<-merge(x =data, y = delitos,        by = "clave", all.x = TRUE)
data<-merge(x =data, y = energia,        by = "clave", all.x = TRUE)
data<-merge(x =data, y = urbanizacion,   by = "clave", all.x = TRUE)

saveRDS( data, 'dataMunicipios.rds')

#Cambios 10 Octubre
print('Nombres sin acentos')
data<-readRDS('dataMunicipios.rds')
listaMunicipio <- read.csv("listaMuinicipio.csv", head=TRUE)
data['clave']<-paste( data$municipioID, data$estadoID)
listaMunicipio['clave']<-paste( listaMunicipio$municipioID, listaMunicipio$estadoID)
listaMunicipio$municipioID<-NULL
listaMunicipio$estadoID<-NULL
newdata<-merge(x=data, y=listaMunicipio, by='clave', all.x = TRUE )
newdata$estado.x<-NULL
newdata$municipio.x<-NULL
newdata$clave<-NULL
estado<-newdata$estado.y
municipio<-newdata$municipio.y
newdata<-cbind(estado, municipio, newdata)
newdata$estado.y<-NULL
newdata$municipio.y<-NULL

saveRDS( newdata, 'dataMunicipios.rds')

print('infromacion por Estado')
newdata['estadoID']<-paste(newdata$estado, newdata$anio, sep='_')

estadosData<-aggregate(  newdata, by=list(newdata$estadoID), function(x){ sum(as.numeric(x), na.rm = TRUE)})
y<-strsplit( estadosData$Group.1 , '_')

for(i in 1:length(estadosData[,1]))
{
    if(i ==1)
    {
        estado<-y[[i]][1]
        anios<-y[[i]][2]
    }
    else
    {
        estado<-rbind(estado, y[[i]][1])
        anios<-rbind(anios, y[[i]][2])
    }

}

estadosData['estado']<-estado
estadosData['anio']<-anios
estadosData$Group.1<-NULL
rm(estado)
rm(anios)
rm(y)
estadosData$municipioID<-NULL
estadosData$municipio<-NULL
estadosData$estadoID<-NULL


saveRDS( estadosData, 'dataEstados.rds')

