data <- read.csv("03102016_urbanizacion.csv", head=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
listaMuncipio<-cbind(data$Clave, data$Nombre)
data2<-data
cont<-0
ncolumna<-1
#nvariables<-(length(data)-2)/2 #cuantos variables tengo por año 2006:{ cantidad de viviendas, viviendas con computadora...}
nvariables<-(length(data)-2)/6
nombrecolumnasFinal<-colnames(data)
for(i in 3:length(data))#Empezamos despues de ID, Municipio
{

    if(ncolumna==1)
    {
        newset<-data[i] #Cada data[i] es una columna de 152 datos
        ncolumna<-ncolumna+1
    }
    else if(ncolumna<nvariables )
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
            cont=2005
            newset['anio']<-cont
            newset<-cbind(listaMuncipio, newset)
            data2<-newset
            colnames(data2)<-c( 'anio', LETTERS[seq(1:nvariables)])

        }
        else
        {
            #cont=2010
            cont=cont+1
            newset['anio']<-cont
            newset<-cbind(listaMuncipio, newset)
            colnames(newset)<-c( 'anio', LETTERS[seq(1:nvariables)])
            data2<-rbind(data2, newset)

        }
    }

}

print('Colocando nombres')
colnames(data2) <-c('miunicipioID','municipio',nombrecolumnasFinal[3:(nvariables+2)], 'anio')

print('Eliminando ND')
for(i in colnames(data2))
{
    print(i)
    data2[i][data2[i]=='ND']<-NA
}


print('GUARDANDO')
saveRDS(data2, 'urbanizacion.rds')
energia<-readRDS('urbanizacion.rds')


"""
print('Repitiendo datos')
canios<-c(2005, 2006, 2007, 2008, 2009, 2010)
cont<-0
for(i in canios)
{
    print(paste('Anio :  ', i))
 if(i <2008)
 {
     newset<-subset(data2, data2$anio==2005)
     if(cont==0)
     {
         newset['anio']<-i
         finalData<-newset
         cont=cont+1
     }
     else
     {

         newset['anio']<-i
         finalData<-rbind(finalData, newset)
         cont=cont+1
     }

 }
 else
    {
        newset<-subset(data2, data2$anio==2010)
        if(cont==0)
        {
            newset['anio']<-i
            finalData<-newset
            cont=cont+1
        }
        else
        {
            newset['anio']<-i
            finalData<-rbind(finalData, newset)
            cont=cont+1
        }

    }

}
saveRDS(finalData, 'poblacion.rds')
poblacion<-readRDS('poblacion.rds')

"""


print('Repitiendo datos')
canios<-c(2005, 2006, 2007, 2008, 2009, 2010)
cont<-0
for(i in canios)
{
    print(paste('Anio :  ', i))

        newset<-subset(data2, data2$anio==i)
        if(cont==0)
        {
            newset['anio']<-i
            finalData<-newset
            cont=cont+1
        }
        else
        {

            newset['anio']<-i
            finalData<-rbind(finalData, newset)
            cont=cont+1
        }



}
saveRDS(finalData, 'delitos.rds')
delitos<-readRDS('delitos.rds')

