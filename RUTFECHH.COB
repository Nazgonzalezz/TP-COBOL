      ******************************************************************
      * Author: Gonzalez Nazarena Araceli
      * Purpose:Rutina de fechas que brinda tres opciones: 
      *        1)pide el ingreso de una fecha por teclado y la formatea
      *        2)recibe una fecha por la LINKAGE SECTION y la formatea
      *        3) pide el ingreso de un anio y un mes por teclado
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RUTFECHH.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  flag            PIC 9.
       77  auxBarra        PIC X VALUE '/'.
       77  auxGuion        PIC X VALUE '-'.
       77  nombrMes        PIC X(10).
       77  maxDiaMes       PIC 9(2) VALUE 31.
       77  bisiesto        PIC 9 VALUE 0.
       77  aux             PIC 9(5).
       77  resto           PIC 9(5).

       01  FECHAA.
           05 dia          PIC 99.
           05 mes          PIC 99.
           05 anio         PIC 9(4).

       01  FECHA-FORMATO1.
           05 AA           PIC X(4).
           05 MM           PIC X(2).
           05 DD           PIC x(2).

       01  FECHA-FORMATO2.
           05 MM2          PIC X(2).
           05 DD2          PIC X(2).
           05 AA2          PIC X(4).

       01  FECHA-FORMATO3.
           05 DD3          PIC X(2).
           05 FILLER       PIC X VALUE '/'.
           05 MM3          PIC X(2).
           05 FILLER       PIC X VALUE '/'.
           05 AA3          PIC X(4).

       01  FECHA-FORMATO4.
           05 DD4          PIC X(2).
           05 FILLER       PIC X VALUE '-'.
           05 MM4          PIC X(2).
           05 FILLER       PIC X VALUE '-'.
           05 AA4          PIC X(4).

       01  FECHA-FORMATO5.
           05 DD5          PIC X(2).
           05 FILLER       PIC X(9) VALUE " del mes ".
           05 MM5          PIC X(10).
           05 FILLER       PIC X(10) VALUE " del anio ".
           05 AA5          PIC X(4).

       LINKAGE SECTION.
       01  AREA-DE-COMMUNICATION.
           05 DATOS-ENTRADA.
               10 OPCION          PIC X.
               10 FECHA.
                   15 diaa        PIC 99.
                   15 mess        PIC 99.
                   15 anioo       PIC 9(4).
           05 DATOS-SALIDA.
               10 ANIO-VALIDADO   PIC 9(4).
               10 MES-VALIDADO    PIC 99.
               10 FECHA-FORMATOO1 PIC X(8).
               10 FECHA-FORMATOO2 PIC X(8).
               10 FECHA-FORMATOO3 PIC X(10).
               10 FECHA-FORMATOO4 PIC X(10).
               10 FECHA-FORMATOO5 PIC X(35).
               10 COMO-TERMINO.
                   15 retorn-num  PIC 999.
                   15 retorn-txt  PIC X(40).

       PROCEDURE DIVISION USING AREA-DE-COMMUNICATION.
      */////////////////////////////////////////////////////////////////

       MAIN-PROCEDURE.
           EVALUATE OPCION
               WHEN 1
                   PERFORM 1000-INGRESO-DATO-VALIDO
                   PERFORM 2000-FORMATEO-FECHA
               WHEN 2
                   PERFORM 3000-VALIDO-AREA-COMMUNICATION
                   PERFORM 2000-FORMATEO-FECHA
                   PERFORM 4000-REVISAR-SI-HAY-ERROR
               WHEN 3
                   PERFORM 1100-INGRESO-ANIO
                   PERFORM 1200-INGRESO-MES
                   MOVE anio TO ANIO-VALIDADO
                   MOVE mes  TO MES-VALIDADO
               WHEN OTHER
                   MOVE 002 TO retorn-num
                   PERFORM 4000-REVISAR-SI-HAY-ERROR
           END-EVALUATE
       GOBACK.

      */////////////////////////////////////////////////////////////////

       1000-INGRESO-DATO-VALIDO.
           PERFORM 1100-INGRESO-ANIO
           PERFORM 1200-INGRESO-MES
           PERFORM 1300-INGRESO-DIA
           PERFORM 1400-QUE-MES-ES.

      *----------------------------------------------------------------

       1100-INGRESO-ANIO.
           MOVE 0 TO flag
           PERFORM UNTIL flag = 1
               DISPLAY "ingrese el anio"
               ACCEPT anio
               IF anio >= 1900 and anio <=2024 THEN
                   MOVE 1 TO flag
               ELSE
                   DISPLAY "error, lo ingreso mal"
               END-IF

               DIVIDE  anio INTO 4 GIVING aux
               MULTIPLY 4 by aux GIVING aux
               SUBTRACT anio from aux  GIVING resto
               if resto = 0 THEN
                   MOVE 1 TO bisiesto
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------

       1200-INGRESO-MES.
           MOVE 0 TO flag
           PERFORM UNTIL flag = 1
               DISPLAY "ingrese el mes"
               ACCEPT mes
               if mes >=1 AND mes <=12 THEN
                   MOVE 1 TO flag
                   DIVIDE  anio INTO 4 GIVING aux
                   MULTIPLY 4 by aux GIVING aux
                   SUBTRACT anio from aux  GIVING resto
                   if resto = 0 THEN
                       MOVE 1 TO bisiesto
                   END-IF
                   PERFORM 1400-QUE-MES-ES
               ELSE
                   DISPLAY "error, lo ingreso mal"
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------

       1300-INGRESO-DIA.
           MOVE 0 TO flag
           PERFORM UNTIL flag = 1
               DISPLAY "ingrese el dia"
               ACCEPT dia
               if dia >=1 AND dia <=maxDiaMes THEN
                   MOVE 1 TO flag
               ELSE
                   DISPLAY "error, lo ingreso mal"
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------

       3000-VALIDO-AREA-COMMUNICATION.
           MOVE 0 TO FLAG

           IF anioo >= 1900 and anioo <=2024 THEN
               ADD 1 TO flag
           ELSE
               ADD 100 TO retorn-num
           END-IF


           IF mess >=1 AND mess <=12 THEN
               ADD 1 TO flag
           ELSE
               ADD 10 TO retorn-num
           END-IF


           PERFORM 1400-QUE-MES-ES

           IF diaa >=1 AND diaa <=maxDiaMes THEN
               ADD 1 TO flag
           ELSE
               ADD 1 TO retorn-num
           END-IF


           IF flag = 3 THEN
               MOVE 1 TO flag
               MOVE mess  to mes
               MOVE anioo to anio
               MOVE diaa  to dia
      *        PERFORM 1400-QUE-MES-ES
           ELSE 
               move 0 to flag
           END-IF.

      *----------------------------------------------------------------

       2000-FORMATEO-FECHA.
           IF flag=1 THEN
               MOVE "bien" to COMO-TERMINO
               MOVE 0 TO retorn-num

               MOVE mes TO MM
               MOVE mes TO MM2
               MOVE mes TO MM3
               MOVE mes TO MM4
               MOVE nombrMes TO MM5

               MOVE anio TO AA
               MOVE anio TO AA2
               MOVE anio TO AA3
               MOVE anio TO AA4
               MOVE anio TO AA5

               MOVE dia TO DD
               MOVE dia TO DD2
               MOVE dia TO DD3
               MOVE dia TO DD4
               MOVE dia TO DD5

               MOVE FECHA-FORMATO1 TO FECHA-FORMATOO1
               MOVE FECHA-FORMATO2 TO FECHA-FORMATOO2
               MOVE FECHA-FORMATO3 TO FECHA-FORMATOO3
               MOVE FECHA-FORMATO4 TO FECHA-FORMATOO4
               MOVE FECHA-FORMATO5 TO FECHA-FORMATOO5
           END-IF.

      *-----------------------------------------------------------------

       1400-QUE-MES-ES.
           EVALUATE mes
               WHEN 1
                   MOVE "enero"      TO nombrMes
                   MOVE 31           TO maxDiaMes
               WHEN 2
                   MOVE "febrero"    TO nombrMes
                   IF bisiesto = 1 THEN
                       MOVE 29       TO maxDiaMes
                   ELSE
                       MOVE  28      TO maxDiaMes
                   END-IF
               WHEN 3
                   MOVE "marzo"      TO nombrMes
                   MOVE 31           TO maxDiaMes
               WHEN 4
                   MOVE "abril"      TO nombrMes
                   MOVE 30           TO maxDiaMes
               WHEN 5
                   MOVE "mayo"       TO nombrMes
                   MOVE 31           TO maxDiaMes
               WHEN 6
                   MOVE "junio"      TO nombrMes
                   MOVE 30           TO maxDiaMes
               WHEN 7
                   MOVE "julio"      TO nombrMes
                   MOVE 31           TO maxDiaMes
               WHEN 8
                   MOVE "agosto"     TO nombrMes
                   MOVE 31           TO maxDiaMes
               WHEN 9
                   MOVE "septiembre" TO nombrMes
                   MOVE 30           TO maxDiaMes
               WHEN 10
                   MOVE "octubre"    TO nombrMes
                   MOVE 31           TO maxDiaMes
               WHEN 11
                   MOVE "noviembre"  TO nombrMes
                   MOVE 30           TO maxDiaMes
               WHEN 12
                   MOVE "diciembre"  TO nombrMes
                   MOVE 31           TO maxDiaMes
               WHEN OTHER
                   DISPLAY "numero ingresado inexistente"
           END-EVALUATE.

      *-----------------------------------------------------------------

       4000-REVISAR-SI-HAY-ERROR.
           EVALUATE retorn-num
               WHEN 002
                   MOVE "la opcion ingresada es invalida"  TO retorn-txt
               WHEN 001
                   MOVE "error en el dia igresado"         TO retorn-txt
               WHEN 011
                   MOVE "error en el dia y mes  ingresado" TO retorn-txt
               WHEN 111
                   MOVE "error en el dia,mes y anio ingresado"
                                                           TO retorn-txt
               WHEN 101
                   MOVE "error en el dia y anio  igresado" TO retorn-txt
               WHEN 110
                   MOVE "error en el anio y mes  igresado" TO retorn-txt
               WHEN 100
                   MOVE "error en el anio  igresado"       TO retorn-txt
               WHEN 010
                   MOVE "error en el mes ingresado"        TO retorn-txt
               WHEN 002
                   MOVE "opcion invalida"                  TO retorn-txt
           END-EVALUATE.

      *-----------------------------------------------------------------
       END PROGRAM RUTFECHH.
