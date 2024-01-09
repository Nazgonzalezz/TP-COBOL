      ******************************************************************
      * Author: Gonzalez Nazarena Araceli
      * Date:
      * Purpose: Rutina que brinda tres opciones:
      *     1) bajar datos del archivo a un vector de la rutina
      *     2) buscar un dato dentro de los datos bajados al vector
      *     3) mostrar los datos que contiene el archivo
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RLEEMATE.

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
                05 codigMateri           PIC XX.
                05 descripMateri         PIC X(20).


       WORKING-STORAGE SECTION.
       77  flag                          PIC X         VALUE "N".
       77  cont                          PIC 9         VALUE 1.
       01  WSS-REG-MAT.
                05 wss-codigMateri       PIC X(3).
                05 wss-descripMateri     PIC X(20).

       01  wss-fs-mat                    PIC X(2).
           88 wss-fs-mat-OK                            VALUE '00'.
           88 wss-fs-mat-EOF                           VALUE '10'.

       01  VECTOR-MATERIAS  OCCURS 10 TIMES.
           05 vec-codigMateri            PIC X(3).
           05 vec-descripMateri          PIC X(20).

       01  WS-IND                        PIC 9(2) COMP.
          01  SW-INDICADOR               PIC X         VALUE SPACES.
                 88 NO-ENCONTRADO                      VALUE "N".
                  88 ENCONTRADO                        VALUE "S".
       01  MENU.
           05 FILLER                     PIC X(8)      VALUE "CODIGO: ".
           05 codi-menu                  PIC X(3).
           05 FILLER                     PIC X(15)     VALUE
           " DESCRIPCION: ".
           05 descripMateri-menu         PIC X(20).


       LINKAGE SECTION.
       01  AREA-DE-COMMUNICATIONN.
           05 DATOS-ENTRADA.
               10 OPCION                 PIC 9.
               10 NUM-DAT-BUSCAR         PIC Xx.
           05 DATOS-SALIDA.
               10 DESCRIPCION            PIC X(20).
               10 COMO-TERMINO.
                   15 retorn-num         PIC 9(3).
                   15 retorn-txt         PIC X(30).


       PROCEDURE DIVISION USING AREA-DE-COMMUNICATIONN.
      */////////////////////////////////////////////////////////////////

       MAIN-PROCEDURE.
           EVALUATE OPCION
               WHEN 1
                   PERFORM 1000-CARGAR-VECTOR-DE-LA-RUTINA
               WHEN 2
                   PERFORM 2000-BUSCAR-DATO-EN-EL-VECTOR
               WHEN 3
                   PERFORM 3000-MOSTRAR-DATOS-DEL-ARCH
               WHEN OTHER
                   DISPLAY "error al seleccionar una opcion"
           END-EVALUATE

       GOBACK.

      */////////////////////////////////////////////////////////////////

       1000-CARGAR-VECTOR-DE-LA-RUTINA.
           PERFORM 1100-ABRIR-ARCHIVO
           PERFORM 1200-GUARDAR-DATOS-AL-VEC
           PERFORM 1300-CERRAR-ARCHIVO.

      *-----------------------------------------------------------------

       1100-ABRIR-ARCHIVO.
           OPEN INPUT  ARCH-MAT
           IF NOT wss-fs-mat-OK then
               display "error al abrir el archivo"
           END-IF.

      *-----------------------------------------------------------------

       1200-GUARDAR-DATOS-AL-VEC.
           READ ARCH-MAT  AT END  MOVE "S" to flag
           END-READ
           PERFORM UNTIL flag = "S"
               MOVE descripMateri TO vec-descripMateri(cont)
               MOVE codigMateri TO vec-codigMateri(cont)
               ADD 1 TO cont
               READ ARCH-MAT AT END MOVE "S" to flag
               END-READ
           END-PERFORM.

      *-----------------------------------------------------------------

       1300-CERRAR-ARCHIVO.
           CLOSE ARCH-MAT
           IF NOT wss-fs-mat-OK then
               display "error al  cerrar el archivo"
           END-IF.

      *-----------------------------------------------------------------

       2000-BUSCAR-DATO-EN-EL-VECTOR.
           SET NO-ENCONTRADO TO TRUE
           MOVE 1 TO cont
           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > 10
                             OR ENCONTRADO
               IF vec-codigMateri(WS-IND)  EQUAL NUM-DAT-BUSCAR
                   SET ENCONTRADO TO TRUE
                   MOVE vec-descripMateri(WS-IND) TO DESCRIPCION
                   MOVE 0 TO retorn-num
               END-IF
           END-PERFORM

           IF NO-ENCONTRADO
               MOVE "Apellido no encontrado" TO retorn-txt
               MOVE 10 TO retorn-num
           END-IF.

      *-----------------------------------------------------------------

       3000-MOSTRAR-DATOS-DEL-ARCH.
           PERFORM 1100-ABRIR-ARCHIVO
           PERFORM 3100-MOSTRAR-DATOS
           PERFORM 1300-CERRAR-ARCHIVO.

      *-----------------------------------------------------------------

       3100-MOSTRAR-DATOS.
           MOVE "N" TO flag
           READ ARCH-MAT AT END MOVE "S" to flag
           END-READ
           PERFORM UNTIL flag = "S"
               MOVE codigMateri TO vec-codigMateri(cont)
               MOVE descripMateri TO vec-descripMateri(cont)
               MOVE vec-codigMateri(cont) TO codi-menu
               MOVE  vec-descripMateri(cont) to descripMateri-menu
               DISPLAY MENU
               DISPLAY "---------------------------------------------"
               READ ARCH-MAT AT END MOVE "S" to flag
               END-READ
           END-PERFORM.

      *-----------------------------------------------------------------
       END PROGRAM RLEEMATE.
