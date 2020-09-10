!Este programa calcula el Radio de Schwarzschild para una masa dada, es decir, el radio necesario para que un cuerpo se convierta en un agujero negro.

PROGRAM programa1

IMPLICIT NONE
CHARACTER (len=15)::Nombre
REAL, PARAMETER:: G=6.67384e-11, c=299792458
REAL::Masa,Radio

!Escribir en pantalla la solicitud del nombre:
PRINT*, "Hola, introduzca su nombre: "
!Leer el nombre desde el teclado
READ*, Nombre
!Escribir en pantalla la solicitud de la masa:
PRINT*, "Hola, ", Nombre, ",Por favor, introduzca su masa en kg:"
!Leer la masa desde el teclado
READ*, Masa
!Calcular el Radio de Schwarzschild
Radio=(G*Masa)/(c**2)
PRINT*, "Muy bien ", Nombre, ",si usted tuviera forma esférica y un radio de"
PRINT*, Radio
PRINT*, "entonces usted sería un agujero negro."

!Aquí termina we
END PROGRAM programa1





