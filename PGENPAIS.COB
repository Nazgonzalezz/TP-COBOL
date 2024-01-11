      ******************************************************************
      * Author: Gonzalez Nazarena Araceli
      * Date:
      * Purpose: Programa que genera el archivo "NACIONALIDAD.dat" con
      *    los datos que se ingresan por teclado validados
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGENMATE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL ARCH-NAC         ASSIGN TO "NACIONALIDAD.dat"
                                        ORGANIZATION IS SEQUENTIAL
                                        FILE STATUS IS wss-fs-nac.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCH-NAC.
           01  REG-NAC.
                05 codigNacion           PIC X(3).
                05 descripNacion         PIC X(20).


       WORKING-STORAGE SECTION.
       77  flag                          PIC 9     VALUE 0.
       01  WSS-REG-NAC.
                05 wss-codigNacion       PIC X(3).
                05 wss-descripNacion     PIC X(20).

       01  wss-fs-nac                    PIC X(2).
           88 wss-fs-nac-OK                        VALUE '00'.
           88 wss-fs-nac-EOF                       VALUE '10'.


       PROCEDURE DIVISION.
      */////////////////////////////////////////////////////////////////

       MAIN-PROCEDURE.
           PERFORM 1000-ABRIR-ARCHIVO
           PERFORM 2000-INGRESAR-CODI-NAC-VALIDADO
           PERFORM 3000-INGRE-DESCRIP-Y-CARG-ARCH
           PERFORM 4000-CERRAR-ARCHIVO
       STOP RUN.

      */////////////////////////////////////////////////////////////////

       1000-ABRIR-ARCHIVO.
           OPEN OUTPUT  ARCH-NAC
           IF NOT wss-fs-nac-OK then
               display "error al abrir el archivo"
           END-IF.

      *-----------------------------------------------------------------

       2000-INGRESAR-CODI-NAC-VALIDADO.
           PERFORM UNTIL flag = 1
               DISPLAY "INGRESE EL CODIGO DE LA NACION (del 000 al 999)"
               DISPLAY "(para terminar de ingresar escriba FIN)"
               ACCEPT wss-codigNacion

               IF wss-codigNacion IS NUMERIC THEN
                   IF wss-codigNacion>=000 AND
                   wss-codigNacion <=999 THEN
                       MOVE 1 TO flag
                   ELSE
                       DISPLAY "eror ingreso un codigo invalido"
                   END-IF
               ELSE
                   IF  wss-codigNacion EQUAL "FIN" THEN
                       MOVE 1 TO flag
                   ELSE
                       DISPLAY "eror ingreso un codigo invalido"
                   END-IF
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------

       3000-INGRE-DESCRIP-Y-CARG-ARCH.

           PERFORM UNTIL wss-codigNacion = "FIN"
               MOVE 0 TO flag
               DISPLAY "INGRESE LA DESCRIPCION DE LA NACION"
               ACCEPT wss-descripNacion

               WRITE REG-NAC FROM WSS-REG-NAC
               IF NOT wss-fs-nac-OK then
                   display "error al  escribir el archivo"
               END-IF
               PERFORM 2000-INGRESAR-CODI-NAC-VALIDADO UNTIL FLAG = 1
           END-PERFORM.

      *-----------------------------------------------------------------

       4000-CERRAR-ARCHIVO.
           CLOSE ARCH-NAC
           IF NOT wss-fs-nac-OK then
               display "error al  cerrar el archivo"
           END-IF.

      *-----------------------------------------------------------------
       END PROGRAM PGENMATE.
