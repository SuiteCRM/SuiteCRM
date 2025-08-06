       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINANCIAL-CALCULATOR.
       AUTHOR. SUITECRM-COBOL-INTEGRATION.
       DATE-WRITTEN. 2025-01-21.
      *----------------------------------------------------------------
      * Banking Financial Calculations for SuiteCRM Integration
      * Provides exact decimal precision for financial operations
      *----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CALC-INPUT ASSIGN TO "calc-input.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CALC-OUTPUT ASSIGN TO "calc-output.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CALC-INPUT.
       01  CALC-INPUT-RECORD.
           05  CALC-TYPE           PIC X(20).
           05  PRINCIPAL           PIC 9(10)V99.
           05  RATE                PIC 9(3)V9(6).
           05  TERM                PIC 9(4).
           05  COMPOUND-FREQ       PIC 9(3).
           05  LINE-ITEMS OCCURS 50 TIMES.
               10  ITEM-QTY        PIC 9(5)V99.
               10  ITEM-PRICE      PIC 9(7)V99.
               10  ITEM-DISCOUNT   PIC 9(3)V99.
               10  ITEM-TAX-RATE   PIC 9(2)V999.
       
       FD  CALC-OUTPUT.
       01  CALC-OUTPUT-RECORD     PIC X(200).
       
       WORKING-STORAGE SECTION.
       01  WS-CALCULATIONS.
           05  WS-RESULT           PIC 9(12)V99.
           05  WS-INTEREST         PIC 9(10)V99.
           05  WS-PAYMENT          PIC 9(8)V99.
           05  WS-TOTAL-INTEREST   PIC 9(10)V99.
           05  WS-BALANCE          PIC 9(10)V99.
           05  WS-SUBTOTAL         PIC 9(10)V99.
           05  WS-TAX-AMOUNT       PIC 9(8)V99.
           05  WS-DISCOUNT-AMOUNT  PIC 9(8)V99.
           05  WS-LINE-TOTAL       PIC 9(10)V99.
           05  WS-GRAND-TOTAL      PIC 9(12)V99.
       
       01  WS-TEMP-FIELDS.
           05  WS-MONTHLY-RATE     PIC 9(3)V9(9).
           05  WS-POWER-TERM       PIC 9(3)V9(9).
           05  WS-COMPOUND-FACTOR  PIC 9(5)V9(9).
           05  WS-ITEM-COUNT       PIC 9(3).
           05  WS-I                PIC 9(3).
           05  WS-J                PIC 9(4).
       
       01  WS-OUTPUT-FIELDS.
           05  DSP-RESULT          PIC $$$,$$$,$$$,$$9.99.
           05  DSP-PAYMENT         PIC $$$,$$$,$$9.99.
           05  DSP-INTEREST        PIC $$$,$$$,$$9.99.
           05  DSP-TOTAL           PIC $$$,$$$,$$$,$$9.99.
       
       01  WS-STATUS.
           05  WS-EOF              PIC X VALUE 'N'.
           05  WS-ERROR-FLAG       PIC X VALUE 'N'.
           05  WS-ERROR-MSG        PIC X(100).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZATION
           PERFORM PROCESS-CALCULATION
           PERFORM TERMINATION
           STOP RUN.
       
       INITIALIZATION.
           OPEN INPUT CALC-INPUT
           OPEN OUTPUT CALC-OUTPUT
           READ CALC-INPUT
               AT END MOVE 'Y' TO WS-EOF
           END-READ.
       
       PROCESS-CALCULATION.
           EVALUATE CALC-TYPE
               WHEN "SIMPLE-INTEREST"
                   PERFORM CALCULATE-SIMPLE-INTEREST
               WHEN "COMPOUND-INTEREST"
                   PERFORM CALCULATE-COMPOUND-INTEREST
               WHEN "LOAN-PAYMENT"
                   PERFORM CALCULATE-LOAN-PAYMENT
               WHEN "AMORTIZATION"
                   PERFORM CALCULATE-AMORTIZATION
               WHEN "INVOICE-TOTAL"
                   PERFORM CALCULATE-INVOICE-TOTAL
               WHEN "QUOTE-TOTAL"
                   PERFORM CALCULATE-QUOTE-TOTAL
               WHEN "CURRENCY-CONVERT"
                   PERFORM CURRENCY-CONVERSION
               WHEN OTHER
                   MOVE "Invalid calculation type" TO WS-ERROR-MSG
                   MOVE 'Y' TO WS-ERROR-FLAG
           END-EVALUATE
           
           IF WS-ERROR-FLAG = 'N'
               PERFORM WRITE-OUTPUT
           ELSE
               PERFORM WRITE-ERROR
           END-IF.
       
       CALCULATE-SIMPLE-INTEREST.
           COMPUTE WS-INTEREST = PRINCIPAL * RATE * TERM / 365
           COMPUTE WS-RESULT = PRINCIPAL + WS-INTEREST.
       
       CALCULATE-COMPOUND-INTEREST.
           IF COMPOUND-FREQ = 0
               MOVE 365 TO COMPOUND-FREQ
           END-IF
           COMPUTE WS-COMPOUND-FACTOR = 1 + (RATE / COMPOUND-FREQ)
           COMPUTE WS-POWER-TERM = COMPOUND-FREQ * TERM / 365
           
           MOVE PRINCIPAL TO WS-RESULT
           PERFORM VARYING WS-J FROM 1 BY 1 
                   UNTIL WS-J > WS-POWER-TERM
               COMPUTE WS-RESULT = WS-RESULT * WS-COMPOUND-FACTOR
           END-PERFORM
           
           COMPUTE WS-INTEREST = WS-RESULT - PRINCIPAL.
       
       CALCULATE-LOAN-PAYMENT.
           IF RATE = 0
               COMPUTE WS-PAYMENT = PRINCIPAL / TERM
           ELSE
               COMPUTE WS-MONTHLY-RATE = RATE / 12
               COMPUTE WS-COMPOUND-FACTOR = 1 + WS-MONTHLY-RATE
               MOVE 1 TO WS-POWER-TERM
               
               PERFORM VARYING WS-J FROM 1 BY 1 
                       UNTIL WS-J > TERM
                   COMPUTE WS-POWER-TERM = 
                           WS-POWER-TERM * WS-COMPOUND-FACTOR
               END-PERFORM
               
               COMPUTE WS-PAYMENT = PRINCIPAL * WS-MONTHLY-RATE * 
                   WS-POWER-TERM / (WS-POWER-TERM - 1)
           END-IF
           
           MOVE WS-PAYMENT TO WS-RESULT.
       
       CALCULATE-AMORTIZATION.
           PERFORM CALCULATE-LOAN-PAYMENT
           COMPUTE WS-TOTAL-INTEREST = (WS-PAYMENT * TERM) - PRINCIPAL
           MOVE WS-TOTAL-INTEREST TO WS-RESULT.
       
       CALCULATE-INVOICE-TOTAL.
           MOVE ZERO TO WS-SUBTOTAL
           MOVE ZERO TO WS-TAX-AMOUNT
           MOVE ZERO TO WS-DISCOUNT-AMOUNT
           MOVE ZERO TO WS-GRAND-TOTAL
           
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > 50 OR ITEM-QTY(WS-I) = 0
               COMPUTE WS-LINE-TOTAL = ITEM-QTY(WS-I) * ITEM-PRICE(WS-I)
               
               IF ITEM-DISCOUNT(WS-I) > 0
                   COMPUTE WS-DISCOUNT-AMOUNT = WS-DISCOUNT-AMOUNT +
                       (WS-LINE-TOTAL * ITEM-DISCOUNT(WS-I) / 100)
                   COMPUTE WS-LINE-TOTAL = WS-LINE-TOTAL -
                       (WS-LINE-TOTAL * ITEM-DISCOUNT(WS-I) / 100)
               END-IF
               
               COMPUTE WS-SUBTOTAL = WS-SUBTOTAL + WS-LINE-TOTAL
               
               IF ITEM-TAX-RATE(WS-I) > 0
                   COMPUTE WS-TAX-AMOUNT = WS-TAX-AMOUNT +
                       (WS-LINE-TOTAL * ITEM-TAX-RATE(WS-I) / 100)
               END-IF
           END-PERFORM
           
           COMPUTE WS-GRAND-TOTAL = WS-SUBTOTAL + WS-TAX-AMOUNT
           MOVE WS-GRAND-TOTAL TO WS-RESULT.
       
       CALCULATE-QUOTE-TOTAL.
           PERFORM CALCULATE-INVOICE-TOTAL.
       
       CURRENCY-CONVERSION.
           COMPUTE WS-RESULT = PRINCIPAL * RATE.
       
       WRITE-OUTPUT.
           MOVE WS-RESULT TO DSP-RESULT
           MOVE WS-PAYMENT TO DSP-PAYMENT
           MOVE WS-INTEREST TO DSP-INTEREST
           MOVE WS-GRAND-TOTAL TO DSP-TOTAL
           
           STRING "{"
               '"type":"' CALC-TYPE '",'
               '"result":' DSP-RESULT ','
               '"payment":' DSP-PAYMENT ','
               '"interest":' DSP-INTEREST ','
               '"total":' DSP-TOTAL ','
               '"status":"success"'
               "}"
               DELIMITED BY SIZE INTO CALC-OUTPUT-RECORD
           WRITE CALC-OUTPUT-RECORD.
       
       WRITE-ERROR.
           STRING "{"
               '"status":"error",'
               '"message":"' WS-ERROR-MSG '"'
               "}"
               DELIMITED BY SIZE INTO CALC-OUTPUT-RECORD
           WRITE CALC-OUTPUT-RECORD.
       
       TERMINATION.
           CLOSE CALC-INPUT
           CLOSE CALC-OUTPUT.
