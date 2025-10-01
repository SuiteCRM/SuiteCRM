       IDENTIFICATION DIVISION.
       PROGRAM-ID. MORTGAGE.
       AUTHOR. SUITECRM-REAL-ESTATE.
       DATE-WRITTEN. 2025-01-22.
      *****************************************************************
      * MORTGAGE CALCULATION MODULE FOR REAL ESTATE CRM
      * CALCULATES MONTHLY PAYMENTS, INTEREST, AND AMORTIZATION
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MORTGAGE-FILE ASSIGN TO "MORTGAGES.DAT"
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  MORTGAGE-FILE.
       01  MORTGAGE-RECORD.
           05  MORTGAGE-ID         PIC X(20).
           05  PROPERTY-ID         PIC X(20).
           05  BORROWER-NAME       PIC X(50).
           05  LOAN-AMOUNT         PIC 9(9)V99.
           05  DOWN-PAYMENT        PIC 9(9)V99.
           05  INTEREST-RATE       PIC 99V9999.
           05  LOAN-TERM-MONTHS    PIC 999.
           05  MONTHLY-PAYMENT     PIC 9(7)V99.
           05  TOTAL-INTEREST      PIC 9(9)V99.
           05  CALC-DATE           PIC 9(8).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS          PIC XX.
       
       01  WS-CALC-REQUEST.
           05  WS-PROPERTY-PRICE   PIC 9(9)V99.
           05  WS-DOWN-PAYMENT     PIC 9(9)V99.
           05  WS-INTEREST-RATE    PIC 99V9999.
           05  WS-LOAN-YEARS       PIC 99.
           05  WS-PROPERTY-TAX     PIC 9(5)V99.
           05  WS-INSURANCE        PIC 9(5)V99.
           05  WS-HOA-FEES         PIC 9(5)V99.
       
       01  WS-CALC-RESULTS.
           05  WS-LOAN-AMOUNT      PIC 9(9)V99.
           05  WS-MONTHLY-PAYMENT  PIC 9(7)V99.
           05  WS-MONTHLY-PI       PIC 9(7)V99.
           05  WS-MONTHLY-TAX      PIC 9(5)V99.
           05  WS-MONTHLY-INS      PIC 9(5)V99.
           05  WS-MONTHLY-HOA      PIC 9(5)V99.
           05  WS-TOTAL-PAYMENT    PIC 9(7)V99.
           05  WS-TOTAL-INTEREST   PIC 9(9)V99.
           05  WS-LTV-RATIO        PIC 999V99.
           05  WS-DTI-RATIO        PIC 999V99.
       
       01  WS-CALC-WORK.
           05  WS-MONTHLY-RATE     PIC 9V9(8).
           05  WS-LOAN-MONTHS      PIC 999.
           05  WS-POWER-TERM       PIC 9(5)V9(8).
           05  WS-NUMERATOR        PIC 9(5)V9(8).
           05  WS-DENOMINATOR      PIC 9(5)V9(8).
           05  WS-PMI-REQUIRED     PIC X VALUE 'N'.
           05  WS-PMI-AMOUNT       PIC 9(5)V99.
       
       01  WS-RESPONSE-STATUS.
           05  WS-STATUS           PIC XX.
           05  WS-MESSAGE          PIC X(50).
       
       01  WS-CURRENT-DATE.
           05  WS-YEAR             PIC 9(4).
           05  WS-MONTH            PIC 99.
           05  WS-DAY              PIC 99.
       
       LINKAGE SECTION.
       01  LS-REQUEST              PIC X(100).
       01  LS-RESPONSE             PIC X(200).
       
       PROCEDURE DIVISION USING LS-REQUEST LS-RESPONSE.
       
       MAIN-PROCESS.
           PERFORM INITIALIZE-CALCULATION
           PERFORM PARSE-REQUEST
           PERFORM VALIDATE-REQUEST
           
           IF WS-STATUS = '00'
               PERFORM CALCULATE-LOAN-AMOUNT
               PERFORM CALCULATE-MONTHLY-PAYMENT
               PERFORM CALCULATE-TOTAL-COSTS
               PERFORM CHECK-PMI-REQUIREMENT
               PERFORM CALCULATE-RATIOS
               PERFORM LOG-CALCULATION
           END-IF
           
           PERFORM FORMAT-RESPONSE
           GOBACK.
       
       INITIALIZE-CALCULATION.
           INITIALIZE WS-CALC-RESULTS WS-RESPONSE-STATUS
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           MOVE '00' TO WS-STATUS.
       
       PARSE-REQUEST.
           UNSTRING LS-REQUEST DELIMITED BY '|'
               INTO WS-PROPERTY-PRICE
                    WS-DOWN-PAYMENT
                    WS-INTEREST-RATE
                    WS-LOAN-YEARS
                    WS-PROPERTY-TAX
                    WS-INSURANCE
                    WS-HOA-FEES.
       
       VALIDATE-REQUEST.
           IF WS-PROPERTY-PRICE <= ZERO
               MOVE '01' TO WS-STATUS
               MOVE "Invalid property price" TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DOWN-PAYMENT >= WS-PROPERTY-PRICE
               MOVE '02' TO WS-STATUS
               MOVE "Down payment exceeds property price" TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-INTEREST-RATE <= ZERO OR > 30
               MOVE '03' TO WS-STATUS
               MOVE "Invalid interest rate" TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           IF WS-LOAN-YEARS <= ZERO OR > 50
               MOVE '04' TO WS-STATUS
               MOVE "Invalid loan term" TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF.
       
       CALCULATE-LOAN-AMOUNT.
           COMPUTE WS-LOAN-AMOUNT = WS-PROPERTY-PRICE - WS-DOWN-PAYMENT.
       
       CALCULATE-MONTHLY-PAYMENT.
           COMPUTE WS-LOAN-MONTHS = WS-LOAN-YEARS * 12
           
           IF WS-INTEREST-RATE = ZERO
               COMPUTE WS-MONTHLY-PI = WS-LOAN-AMOUNT / WS-LOAN-MONTHS
               MOVE ZERO TO WS-TOTAL-INTEREST
           ELSE
               COMPUTE WS-MONTHLY-RATE = WS-INTEREST-RATE / 100 / 12
               
               COMPUTE WS-POWER-TERM = (1 + WS-MONTHLY-RATE) ** WS-LOAN-MONTHS
               
               COMPUTE WS-NUMERATOR = WS-LOAN-AMOUNT * WS-MONTHLY-RATE * 
                                      WS-POWER-TERM
               
               COMPUTE WS-DENOMINATOR = WS-POWER-TERM - 1
               
               COMPUTE WS-MONTHLY-PI = WS-NUMERATOR / WS-DENOMINATOR
               
               COMPUTE WS-TOTAL-INTEREST = (WS-MONTHLY-PI * WS-LOAN-MONTHS) 
                                          - WS-LOAN-AMOUNT
           END-IF
           
           MOVE WS-MONTHLY-PI TO WS-MONTHLY-PAYMENT.
       
       CALCULATE-TOTAL-COSTS.
           COMPUTE WS-MONTHLY-TAX = WS-PROPERTY-TAX / 12
           COMPUTE WS-MONTHLY-INS = WS-INSURANCE / 12
           MOVE WS-HOA-FEES TO WS-MONTHLY-HOA
           
           COMPUTE WS-TOTAL-PAYMENT = WS-MONTHLY-PAYMENT + 
                                      WS-MONTHLY-TAX + 
                                      WS-MONTHLY-INS + 
                                      WS-MONTHLY-HOA.
       
       CHECK-PMI-REQUIREMENT.
           COMPUTE WS-LTV-RATIO = (WS-LOAN-AMOUNT / WS-PROPERTY-PRICE) * 100
           
           IF WS-LTV-RATIO > 80
               MOVE 'Y' TO WS-PMI-REQUIRED
               COMPUTE WS-PMI-AMOUNT = WS-LOAN-AMOUNT * 0.005 / 12
               ADD WS-PMI-AMOUNT TO WS-TOTAL-PAYMENT
           ELSE
               MOVE 'N' TO WS-PMI-REQUIRED
               MOVE ZERO TO WS-PMI-AMOUNT
           END-IF.
       
       CALCULATE-RATIOS.
           COMPUTE WS-DTI-RATIO = 28.
       
       LOG-CALCULATION.
           OPEN EXTEND MORTGAGE-FILE
           
           IF WS-FILE-STATUS NOT = '00'
               OPEN OUTPUT MORTGAGE-FILE
           END-IF
           
           STRING "MTG" WS-YEAR WS-MONTH WS-DAY
                  FUNCTION CURRENT-DATE(9:6)
                  DELIMITED BY SIZE
                  INTO MORTGAGE-ID
           
           MOVE "CALC-ONLY" TO PROPERTY-ID
           MOVE "CALCULATOR REQUEST" TO BORROWER-NAME
           MOVE WS-LOAN-AMOUNT TO LOAN-AMOUNT
           MOVE WS-DOWN-PAYMENT TO DOWN-PAYMENT
           MOVE WS-INTEREST-RATE TO INTEREST-RATE
           MOVE WS-LOAN-MONTHS TO LOAN-TERM-MONTHS
           MOVE WS-MONTHLY-PAYMENT TO MONTHLY-PAYMENT
           MOVE WS-TOTAL-INTEREST TO TOTAL-INTEREST
           MOVE FUNCTION CURRENT-DATE(1:8) TO CALC-DATE
           
           WRITE MORTGAGE-RECORD
           
           CLOSE MORTGAGE-FILE.
       
       FORMAT-RESPONSE.
           STRING WS-STATUS
                  "|" WS-LOAN-AMOUNT
                  "|" WS-MONTHLY-PAYMENT
                  "|" WS-MONTHLY-TAX
                  "|" WS-MONTHLY-INS
                  "|" WS-MONTHLY-HOA
                  "|" WS-PMI-AMOUNT
                  "|" WS-TOTAL-PAYMENT
                  "|" WS-TOTAL-INTEREST
                  "|" WS-LTV-RATIO
                  "|" WS-PMI-REQUIRED
                  "|" WS-MESSAGE
                  DELIMITED BY SIZE
                  INTO LS-RESPONSE.