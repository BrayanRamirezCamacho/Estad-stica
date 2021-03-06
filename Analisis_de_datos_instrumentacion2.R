#######################################################################
# PAQUETES NECESARIOS

#Instalaci�n y carga de los paquetes

install.packages("spectral")
library("spectral")

install.packages("pracma")
library("pracma")

install.packages("zoo")
library("zoo")

#######################################################################

#Lectura del archivo csv para crear un Dataframe
datos = read.csv("Tarea_datos_csv.csv", header = TRUE)

#Gr�fica de la se�al "cruda"
plot(datos, ylab = "Amplitud", xlab = "Tiempo (ms)", type="l", main = "Se�al 'cruda'", col="blue" )
#Se observa que posee mucho ruido

#Se hace un vector de tiempo a partir de la columna del dataframe
tiempo = as.vector(unlist(datos[1]))
typeof(tiempo)

#Se crea un vector de la variable X(t) a partir del dataframe
X = as.vector(unlist(datos[2]))
typeof(X)

#Se verifica que la longitud de los dos vectores coincide
length(tiempo) == length(X)

#######################################################################
# TRANSFORMADA R�PIDA DE FOURIER (FFT)

#C�lculo de la FFT
X_fft <- spec.fft(X, tiempo, center=TRUE)
typeof(X_fft)

#Grafica del espectro de frecuencias (positivas y negativas)
plot(X_fft, ylab = "Amplitud", xlab = "Frecuencia (kHz)", main = "Espectro de frecuencias", type="l", col = "blue")
#Se aprecia que el espectro tiene picos en aprox. 0.08, 0.18, 0.29 y 0.4 
# xlim = c(0.0,0.5)

#Para cuando sale el error "figure margins too large"
#o para resetear la gr�fica que se est� mostrando
#dev.off()

#######################################################################
# B�SQUEDA DE LOS PICOS DE FRECUENCIA EN EL ESPECTRO

#Extracci�n de un vector con las frecuencias calculadas con FFT
Frecuencias = as.vector(unlist(X_fft[1]))
length(Frecuencias)

#Obtenci�n de un vector con las amplitudes correspondientes a las frecuencias
Amplitudes = abs(as.vector(unlist(X_fft[2])))
length(Amplitudes)

#Se toma solo la porci�n positiva del eje de frecuencia
Frecuencias_positivas = Frecuencias[Frecuencias>=0]
#con sus correspondientes amplitudes
Amplitudes_frec_positivas = Amplitudes[129:256]

#Gr�fica de la porci�n de frecuencias positivas del espectro
plot(Frecuencias_positivas, Amplitudes_frec_positivas, xlab="Frecuencia (kHz)", ylab="Amplitud", main="Espectro (frecuencias positivas)", type="l", col="blue" )

#Uso de la funci�n findpeaks() para hallar los picos de frecuencia del espectro
findpeaks(Amplitudes_frec_positivas, minpeakheight=0.2, npeaks=4, sortstr=F)
#Los �ndices correspondientes a los picos se muestran en consola, con su amplitud
#Estos valores son
pico1 = Frecuencias_positivas[18] #Amplitud = 0.4942993
pico2 = Frecuencias_positivas[44] #Amplitud = 0.4137915
pico3 = Frecuencias_positivas[74] #Amplitud = 0.4844639
pico4 = Frecuencias_positivas[103] #Amplitud = 0.3761851

#Entonces los componentes principales del espectro de frecuencias son
picos = c(pico1, pico2, pico3, pico4)
print(picos)
# picos = (0.06640625, 0.16796875, 0.28515625, 0.39843750 )

#######################################################################
# DENSIDAD DE POTENCIA ESPECTRAL (PSD)

#Tomar el cuadrado de la magnitud de los datos de amplitud para estimar
#la densidad de potencia espectral (PSD)
Potencias = abs( as.vector(unlist(X_fft[2])) )**2
length(Potencias)

#Grafica de la densidad de potencia espectral
plot( Frecuencias, Potencias, xlim = c(0, 0.5), ylab = "Potencia", xlab = "Frecuencia (kHz)", main = "PSD", type="l", col = "red")

#C�lculo del periodograma de Lomb-Scargle
l <- spec.lomb(x = tiempo, y = X, mode = "normal")

#Gr�fica de la densidad de potencia espectral (PSD) y de la probabilidad de falsa alarma
#Calculada utilizando el espectro Lomb-Scargle (y no el de Fourier)
plot(l, log = "", main = "Espectro", FAPlab = "False Alarm Probability", xlab = "Frecuencia", ylab = "PSD Normalizado", FAPcol = "red", legend.on = T)

#Recuperaci�n de la se�al temporal filtrada a partir del periodograma de Lomb
Y = filter.lomb(l = l, newx = tiempo, threshold = 6, filt = picos, phase = "lockin")
#El par�metro "filt" de la funci�n filter.lomb() recibe un vector de frecuencias 
#alrededor de las cuales centrar los filtros pasa-bandas; si filt=NULL, los filtros se
#centran en las frecuencias que corresponden a los picos estimados con la misma funci�n. 

#Gr�fica de la se�al filtrada
plot(Y, type = "l", ylab = "Amplitud", xlab = "Tiempo (ms)", main = "Se�al filtrada", col = "blue")

#######################################################################

