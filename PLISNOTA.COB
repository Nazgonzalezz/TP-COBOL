      ******************************************************************
      * Author:Gonzalez Nazarena Araceli
      * Date:
      * Purpose:Programa que lee el archivo "NOTAS.DAT" y saca el
      *    promedio de las notas de cada materia de cada alumno
      *    utilizando cortes de control
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLISNOTA.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT OPTIONAL ARCH-NOT         ASSIGN TO "NOTAS.DAT"
                                        ORGANIZATION IS SEQUENTIAL
                                        FILE STATUS IS wss-fs-not.

       SELECT OPTIONAL ARCH-ALU         ASSIGN TO "ALUMNOS.DAT"
                                        ORGANIZATION IS SEQUENTIAL
                                        FILE STATUS IS wss-fs-alu.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCH-NOT.
           01  REG-NOT.
                05 NOT-NRO-ALUMNO     PIC 9(4).
                05 NOT-NRO-MATERIA    PIC 99.
                05 NOT-ANIO           PIC 9(4).
                05 NOT-MES            PIC 99.
                05 NOT-NOTA           PIC 99.
                05 ESPACIO            PIC XX.

       FD  ARCH-ALU.
           01  REG-ALU.
                05 ALU-NRO-ALUMNO     PIC X(4).
                05 ALU-NOMBRE         PIC X(23).
                05 ALU-NRO-PAIS       PIC X(3).
                05 espacio            PIC X.

       WORKING-STORAGE SECTION.
       01  wss-fs-alu                 PIC X(2).
           88 wss-fs-alu-OK                     VALUE '00'.
           88 wss-fs-alu-EOF                    VALUE '10'.

       01  wss-fs-not                 PIC X(2).
           88 wss-fs-not-OK                     VALUE '00'.
           88 wss-fs-not-EOF                    VALUE '10'.

       77  fin-alu                    PIC X.
       77  fin                        PIC X.
       77  wss-nroAlumActual          PIC 9(4).
       77  wss-nroMateriaActual       PIC 99.
       77  wss-acu                    PIC 99 COMP.
       77  wss-cont                   PIC 99 COMP.

       01  RUTINAS.
           05 Rfecha                  PIC X(8)  VALUE "RUTFECHH".
           05 Rmate                   PIC X(8)  VALUE "RLEEMATE".
           05 Rnaci                   PIC X(8)  VALUE "RLEENACI".

       01  TITULO-P1.
           05 FILLER                  PIC X(15) VALUE "*  ALUMNOS :".
           05 wss-nombreAlu           PIC X(23).
           05 FILLER                  PIC X(15) VALUE "NACIONALIDAD :".
           05 wss-nombreNaci          PIC X(20).

       01  TITULO-P2.
           05 FILLER                 PIC X(16) VALUE "*  MATERIAS     ".
           05 FILLER                 PIC X(55) VALUE
           "                      PROMEDIO                   *".

       01  TITULO-ANIO.
           05 FILLER                  PIC X(55) VALUE
           "                                             DESDE ".
           05 FECHAA.
               10 wss-mes             PIC 99.
               10 FILLER              PIC X VALUE "/".
               10 wss-anio            PIC 9999.

       01  TITULO-DATOS.
           05 FILLER                  PIC XXX VALUE "  ".
           05 wss-nombrMate           PIC X(20).
           05 FILLER                  PIC X(15) VALUE "               ".
           05 wss-prom                PIC 9.99.

      *VARIABLES QUE USO PARA LA RUTINA DE FECHA
       01  AREA-DE-COMMUNICATION-FECH.
           05 DATOS-ENTRADA-A-LA-RUTINA.
               10 OPCION-F            PIC 9.
               10 FECHA.
                   15 diaa            PIC 99.
                   15 mess            PIC 99.
                   15 anioo           PIC 9(4).
           05 DATOS-SALIDA-DE-LA-RUTINA.
               10 ANIO-VALIDADO       PIC 9(4).
               10 MES-VALIDADO        PIC 99.
               10 FECHA-FORMATOO1     PIC X(8).
               10 FECHA-FORMATOO2     PIC X(8).
               10 FECHA-FORMATOO3     PIC X(10).
               10 FECHA-FORMATOO4     PIC X(10).
               10 FECHA-FORMATOO5     PIC X(35).
               10 COMO-TERMINO.
                   15 retorn-num-f    PIC 999.
                   15 retorn-txt-f    PIC X(40).

      *VARIABLES PARA INVOCAR A LA RUTINA DE BUSCAR NACIONALIDADES
       01  AREA-DE-COMMUNICATION-NACI.
           05 DATOS-ENTRADA.
               10 OPCION-N            PIC 9.
               10 NAC-NUM-DAT-BUSCAR  PIC X(3).
           05 DATOS-SALIDA.
               10 DESCRIPCION-NAC     PIC X(20).
               10 COMO-TERMINO.
                   15 retorn-num-n    PIC 9(3).
                   15 retorn-txt-n    PIC X(30).

      *VARIABLES PARA INVOCAR A LA RUTINA DE BUSCAR MATERIA
       01  AREA-DE-COMMUNICATION-MAT.
           05 DATOS-ENTRADA.
               10 OPCION-M            PIC 9.
               10 MAT-NUM-DAT-BUSCAR  PIC XX.
           05 DATOS-SALIDA.
               10 DESCRIPCION-MAT     PIC X(20).
               10 COMO-TERMINO.
                   15 retorn-num-m    PIC 9(3).
                   15 retorn-txt-m    PIC X(30).

       PROCEDURE DIVISION.
      */////////////////////////////////////////////////////////////////

       MAIN-PROCEDURE.
           PERFORM 1000-ABRIR-ARCH
           PERFORM 2000-INGRESO-ANIO-Y-MES
           PERFORM 3000-CARGAR-VEC-DE-RUTINAS
           PERFORM 4000-CORTES-DE-CONTROL
           PERFORM 5000-CERRAR-ARCH
       STOP RUN.

      */////////////////////////////////////////////////////////////////

       1000-ABRIR-ARCH.
           OPEN INPUT  ARCH-NOT
           IF NOT wss-fs-not-OK then
               display "error al abrir el archivo"
           END-IF.

      *-----------------------------------------------------------------

       2000-INGRESO-ANIO-Y-MES.
           INITIALIZE AREA-DE-COMMUNICATION-FECH
           MOVE 3    TO OPCION-F
           CALL Rfecha USING AREA-DE-COMMUNICATION-FECH
           MOVE ANIO-VALIDADO TO wss-anio
           MOVE MES-VALIDADO TO wss-mes.

      *-----------------------------------------------------------------

       3000-CARGAR-VEC-DE-RUTINAS.
           INITIALIZE AREA-DE-COMMUNICATION-NACI
           MOVE 1    TO OPCION-N
           CALL Rnaci USING AREA-DE-COMMUNICATION-NACI

           INITIALIZE AREA-DE-COMMUNICATION-MAT
           MOVE 1    TO OPCION-M
           CALL Rmate USING AREA-DE-COMMUNICATION-MAT.

      *-----------------------------------------------------------------

       4000-CORTES-DE-CONTROL.

           MOVE 'N' TO fin
           READ ARCH-NOT AT END MOVE 'S' TO fin
           END-READ
           DISPLAY TITULO-ANIO

           PERFORM UNTIL FIN = 'S'
               MOVE NOT-NRO-ALUMNO TO wss-nroAlumActual
               PERFORM 4100-BUSCAR-NOMBR-ALU
               DISPLAY "***********************************************"
               "*******************"
               DISPLAY TITULO-P1
               DISPLAY TITULO-P2
               DISPLAY "**********************************************"
               "********************"

               PERFORM UNTIL NOT-NRO-ALUMNO not EQUAL wss-nroAlumActual
               OR FIN = 'S'
                   MOVE NOT-NRO-MATERIA TO wss-nroMateriaActual
                   MOVE 0 TO wss-acu
                   MOVE 0 TO wss-cont
                   INITIALIZE AREA-DE-COMMUNICATION-MAT
                   MOVE 2    TO OPCION-M
                   MOVE wss-nroMateriaActual to MAT-NUM-DAT-BUSCAR
                   CALL Rmate USING AREA-DE-COMMUNICATION-MAT
                   MOVE DESCRIPCION-MAT TO wss-nombrMate

                   PERFORM UNTIL NOT-NRO-ALUMNO not equal
                   wss-nroAlumActual OR FIN = 'S' OR
                   wss-nroMateriaActual NOT EQUAL NOT-NRO-MATERIA
                       IF NOT-ANIO EQUAL ANIO-VALIDADO THEN
                           IF NOT-MES >= MES-VALIDADO THEN
                               ADD 1 TO wss-cont
                               ADD NOT-NOTA TO wss-acu
                           END-IF
                      END-IF
                      READ ARCH-NOT AT END MOVE 'S' TO fin
                      END-READ
                   END-PERFORM

               MOVE DESCRIPCION-NAC TO wss-nombreNaci
               DIVIDE  wss-cont INTO wss-acu GIVING wss-prom
               DISPLAY TITULO-DATOS
               END-PERFORM

           END-PERFORM.

      *-----------------------------------------------------------------

       4100-BUSCAR-NOMBR-ALU.
           PERFORM 4110-ABRIR-ARCH
           PERFORM 4120-BUSCAR-DATO
           PERFORM 4130-CERRAR-ARCH.

      *-----------------------------------------------------------------

       4110-ABRIR-ARCH.
           OPEN INPUT  ARCH-ALU
           IF NOT wss-fs-alu-OK then
               display "error al abrir el archivo"
           END-IF.

      *-----------------------------------------------------------------

       4120-BUSCAR-DATO.
           MOVE 'N' TO fin-alu
           READ ARCH-ALU AT END MOVE 'S' TO fin-alu
           END-READ

           PERFORM UNTIL fin-alu EQUAL 'S'
               IF ALU-NRO-ALUMNO EQUAL NOT-NRO-ALUMNO
                   MOVE ALU-NOMBRE TO wss-nombreAlu
                   MOVE 'S' TO fin-alu
               ELSE
                   MOVE "codi no existe" to wss-nombreAlu
               END-IF

               INITIALIZE AREA-DE-COMMUNICATION-NACI
               MOVE 2    TO OPCION-N
               MOVE ALU-NRO-PAIS TO NAC-NUM-DAT-BUSCAR
               CALL Rnaci USING AREA-DE-COMMUNICATION-NACI

               IF retorn-num-n = 0 THEN
                   MOVE DESCRIPCION-NAC to wss-nombreNaci
               ELSE
                   MOVE "no encontrado" to wss-nombreNaci
               END-IF
               READ ARCH-ALU AT END MOVE 'S' TO fin-alu
               END-READ
           END-PERFORM.

      *-----------------------------------------------------------------

       4130-CERRAR-ARCH.
           CLOSE ARCH-ALU
           IF NOT wss-fs-alu-OK then
               display "error al  cerrar el archivo"
           END-IF.

      *-----------------------------------------------------------------

       5000-CERRAR-ARCH.
           CLOSE ARCH-NOT
           IF NOT wss-fs-not-OK then
               display "error al  cerrar el archivo"
           END-IF.

      *-----------------------------------------------------------------
           END PROGRAM PLISNOTA.
