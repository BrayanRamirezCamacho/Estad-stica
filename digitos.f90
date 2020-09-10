!El objetivo de esta subrutina es leer un numero N y devolver el numero de digitos de N

SUBROUTINE digitos(N,etiqueta)

!----------Declaración de variables-------------------------
IMPLICIT NONE
INTEGER::N		!El numero entero introducido por el usuario
INTEGER::d		!El numero de digitos de N
INTEGER::etiqueta

!--------------Ciclo iterativo------------------------------
etiqueta=0
DO
if ( abs(N)<=1 ) stop
etiqueta=etiqueta+1
N=N/10
END DO

END SUBROUTINE
