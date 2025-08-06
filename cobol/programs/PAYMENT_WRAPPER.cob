       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMENT-WRAPPER.
       AUTHOR. SUITECRM-REAL-ESTATE.
       DATE-WRITTEN. 2025-01-27.
      *****************************************************************
      * WRAPPER FOR PAYMENT PROGRAM TO WORK WITH GATEWAY
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT-DATA           PIC X(100).
       01  WS-OUTPUT-DATA          PIC X(150).
       01  WS-ENV-VAR              PIC X(100).
       
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           ACCEPT WS-INPUT-DATA FROM SYSIN
           
           IF WS-INPUT-DATA = SPACES
               ACCEPT WS-ENV-VAR FROM ENVIRONMENT "COBOL_DATA"
               MOVE WS-ENV-VAR TO WS-INPUT-DATA
           END-IF
           
           CALL 'PAYMENT' USING WS-INPUT-DATA WS-OUTPUT-DATA
           
           DISPLAY WS-OUTPUT-DATA
           
           STOP RUN.