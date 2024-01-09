      ******************************************************************
      * Author: Gonzalez Nazarena Araceli
      * Date:
      * Purpose: Programa que genera el archivo "MATERIA.dat" con
      *    los datos que se ingresan por teclado validados
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGENMATE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL ARCH-MAT         ASSIGN TO "MATERIA.dat"
                                        ORGANIZATION IS SEQUENTIAL
                                        FILE STATUS IS wss-fs-mat.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCH-MAT.
           01  REG-MAT.
                05 codigMateria           PIC X(2).
                05 descripMateria         PIC X(20).


       WORKING-STORAGE SECTION.
       77  flag                          PIC 9     VALUE 0.
       01  WSS-REG-MAT.
                05 wss-codigMateria      PIC X(2).
                05 wss-descripMateria    PIC X(20).

       01  wss-fs-mat                    PIC X(2).
           88 wss-fs-mat-OK                        VALUE '00'.
           88 wss-fs-mat-EOF                       VALUE '10'.

       LINKAGE SECTION.

       PROCEDURE DIVISION.
      */////////////////////////////////////////////////////////////////

       MAIN-PROCEDURE.
           PERFORM 1000-ABRIR-ARCHIVO
           PERFORM 2000-INGRESAR-CODI-MAT-VALIDADO UNTIL FLAG = 1
           PERFORM 3000-INGRE-DESCRIP-Y-CARG-ARCH UNTIL
                       wss-codigMateria = "FF"
           PERFORM 4000-CERRAR-ARCHIVO
       STOP RUN.

      */////////////////////////////////////////////////////////////////

       1000-ABRIR-ARCHIVO.
           OPEN OUTPUT  ARCH-MAT.
           IF NOT wss-fs-mat-OK then
               display "error al abrir el archivo"
           END-IF.

      *-----------------------------------------------------------------

       2000-INGRESAR-CODI-MAT-VALIDADO.
           DISPLAY "INGRESE EL CODIGO DE LA MATERIA (del 00 al 99)"
           DISPLAY "(para terminar de ingresar escriba FF)"
           ACCEPT wss-codigMateria
           IF wss-codigMateria IS NUMERIC THEN
               IF wss-codigMateria >= 00 AND wss-codigMateria <=99 THEN
                   MOVE 1 TO flag
               ELSE
                   DISPLAY "eror ingreso un codigo invalido"
               END-IF
           ELSE
               IF  wss-codigMateria = "FF" THEN
                   MOVE 1 TO flag
               ELSE
                   DISPLAY "error ingreso un codigo invalido"
               END-IF
           END-IF.

      *-----------------------------------------------------------------

       3000-INGRE-DESCRIP-Y-CARG-ARCH.
           MOVE 0 TO flag
           DISPLAY "INGRESE LA DESCRIPCION DE LA MATERIA"
           ACCEPT wss-descripMateria

           WRITE REG-MAT FROM WSS-REG-MAT
           IF NOT wss-fs-mat-OK then
               display "error al  escribir el archivo"
           END-IF
           PERFORM 2000-INGRESAR-CODI-MAT-VALIDADO UNTIL FLAG = 1.

      *-----------------------------------------------------------------

       4000-CERRAR-ARCHIVO.
           CLOSE ARCH-MAT
           IF NOT wss-fs-mat-OK then
               display "error al  cerrar el archivo"
           END-IF.

      *-----------------------------------------------------------------
       END PROGRAM PGENMATE.
