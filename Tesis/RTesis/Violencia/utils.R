
print('-----Funciones---------')

print('Funcion para ordenar homicidios')
dataHomicidios<-function(data)
{
    cont<-0
    ncolumna<-1

    for(i in 1:length(data))
    {

        if(ncolumna==1)
        {
            newset<-data[i] #Cada data[i] es una columna de 152 datos
            ncolumna<-ncolumna+1
        }
        else if(ncolumna==2 )
        {
            newset<-cbind(newset, data[i])
            ncolumna<-ncolumna+1
        }
        else
        {
            newset<-cbind(newset, data[i])
            ncolumna<-1
            if(cont==0)
            {
                data2<-newset
                colnames(data2)<-c('anio', 'mes', 'dato')
                cont=cont+1
            }
            else
            {
                colnames(newset)<-c('anio', 'mes', 'dato')
                data2<-rbind(data2, newset)
            }
        }

    }

    return(data2)

}


print('Funcion para ordenar añadir municipios')

addMunicipios<-function(data, data_municipios)
{
    m<-length(data_municipios[,1])
    n<-length(data[,1])

    repeticiones<-(n/m)-1
    print(paste('repeticiones: ', repeticiones))
    newdata<-data_municipios
    for(i in 1:repeticiones)
    {
        newdata<-rbind(newdata, data_municipios)
    }
    newdata<-cbind(newdata, data)
    return(newdata)

}
print('Recuperar los componenetes de un keyID')
recuperarClave<-function(keyID) #Dado una keyID aplicamos split y regresamos los componentes
{
    data<-strsplit(keyID, ' ')
    ncomponentes<-length(data[[1]])
    for(j in 1:ncomponentes)
    {
        for(i in 1:length(data))
        {
            if(i==1)
            {
                newVec<-data[[i]][j]
            }
            else
            {
                newVec<-rbind(newVec, data[[i]][j])
            }
        }

        if(j==1)
        {
            newDF<-newVec
        }
        else
        {
            newDF<-cbind(newDF, newVec)
        }

    }
    return(newDF)

}

violenciaPromedioAnio<-function(listaMunicipios)
{

    listaMunicipios['violenciaPromedio_anio']<-NA
    anios<-unique(listaMunicipios$absMes)
    for(i in anios)
    {
        print(paste('MES ABS: ', i))
        media<-mean(subset(listaMunicipios, listaMunicipios$absMes==i)$sumaAEE, na.rm = TRUE) #Promedio por anio
        for(j in 1:length(listaMunicipios$sumaAEE))
        {
            #print(paste(i, j, sep = '     '))
            if(is.na(listaMunicipios$sumaAEE[j]))
            {
                listaMunicipios['violenciaPromedio_anio'][j,1]<-NA
            }
            else
            {
                if( listaMunicipios$anio[j]==i )
                {
                    if(listaMunicipios$sumaAEE[j]>media  )
                    {
                        listaMunicipios['violenciaPromedio_anio'][j,1]<-1
                        print(listaMunicipios['violenciaPromedio_anio'][j,1])
                    }
                    else
                    {
                        listaMunicipios['violenciaPromedio_anio'][j,1]<-0
                    }
                }
            }
        }
    }
    return(listaMunicipios)
}

violenciaPromedioAnioEstados<-function(listaMunicipios)
{
    listaMunicipios['violenciaPromedio_anio_estado']<-NA
    anios<-unique(listaMunicipios$absMes)
    estados<-unique(listaMunicipios$estadoID)
    for(anio in anios)
    {
        print(anio)
        for(edo in estados)
        {
          #print(paste('Estado: ', paste(edo, paste( '    MES ABS:', anio)) ))
          media<-mean(subset(listaMunicipios, (listaMunicipios$absMes==anio & listaMunicipios$estadoID==edo ))$sumaAEE, na.rm = TRUE) #Promedio por anio
        for(j in 1:length(listaMunicipios$sumaAEE))
        {

            if(is.na(listaMunicipios$sumaAEE[j]))
            {
                listaMunicipios['violenciaPromedio_anio_estado'][j,1]<-NA
            }
            else
            {

                if( listaMunicipios$anio[j]==anio && listaMunicipios$estadoID[j]==edo)
                {
                    print(media)
                    if(listaMunicipios$sumaAEE[j]>=media)
                    {
                        print(media)
                    listaMunicipios['violenciaPromedio_anio_estado'][j,1]<-1
                    }
                    else
                    {
                      listaMunicipios['violenciaPromedio_anio_estado'][j,1]<-0
                    }
                }
            }
        }
    }
    }
    return(listaMunicipios)
}





