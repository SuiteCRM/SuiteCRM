       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMENT.
       AUTHOR. SUITECRM-REAL-ESTATE.
       DATE-WRITTEN. 2025-01-22.
      *****************************************************************
      * PAYMENT PROCESSING MODULE FOR REAL ESTATE TRANSACTIONS
      * HANDLES VARIOUS PAYMENT TYPES INCLUDING DEPOSITS AND FEES
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYMENT-FILE ASSIGN TO "PAYMENTS.DAT"
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-FILE-STATUS.
                  
           SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNTS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS ACCOUNT-ID
                  FILE STATUS IS WS-ACCT-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  PAYMENT-FILE.
       01  PAYMENT-RECORD.
           05  PAYMENT-ID          PIC X(20).
           05  PAYMENT-DATE        PIC 9(8).
           05  PAYMENT-TIME        PIC 9(6).
           05  PAYMENT-TYPE        PIC X(20).
           05  PAYMENT-AMOUNT      PIC 9(9)V99.
           05  PAYMENT-METHOD      PIC X(10).
           05  ACCOUNT-NUMBER      PIC X(20).
           05  REFERENCE-NUMBER    PIC X(30).
           05  PAYMENT-STATUS      PIC XX.
           05  PROCESSING-FEE      PIC 9(5)V99.
           05  NET-AMOUNT          PIC 9(9)V99.
       
       FD  ACCOUNT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCOUNT-ID          PIC X(20).
           05  ACCOUNT-TYPE        PIC X(10).
           05  ACCOUNT-BALANCE     PIC S9(9)V99.
           05  ACCOUNT-STATUS      PIC X.
           05  LAST-ACTIVITY       PIC 9(8).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS          PIC XX.
       01  WS-ACCT-STATUS          PIC XX.
       
       01  WS-PAYMENT-DATA.
           05  WS-PAYMENT-TYPE     PIC X(20).
           05  WS-AMOUNT           PIC 9(9)V99.
           05  WS-METHOD           PIC X(10).
           05  WS-ACCOUNT          PIC X(20).
           05  WS-REFERENCE        PIC X(30).
       
       01  WS-RESPONSE.
           05  WS-STATUS           PIC XX.
           05  WS-PAYMENT-ID       PIC X(20).
           05  WS-MESSAGE          PIC X(50).
           05  WS-NET-AMOUNT       PIC 9(9)V99.
           05  WS-FEE              PIC 9(5)V99.
       
       01  WS-FEE-STRUCTURE.
           05  WS-CARD-FEE-PCT     PIC 99V99 VALUE 02.95.
           05  WS-ACH-FEE-FLAT     PIC 9V99 VALUE 0.50.
           05  WS-WIRE-FEE-FLAT    PIC 99V99 VALUE 25.00.
       
       01  WS-CURRENT-DATE.
           05  WS-YEAR             PIC 9(4).
           05  WS-MONTH            PIC 99.
           05  WS-DAY              PIC 99.
       
       01  WS-CURRENT-TIME.
           05  WS-HOUR             PIC 99.
           05  WS-MINUTE           PIC 99.
           05  WS-SECOND           PIC 99.
       
       LINKAGE SECTION.
       01  LS-REQUEST              PIC X(100).
       01  LS-RESPONSE             PIC X(150).
       
       PROCEDURE DIVISION USING LS-REQUEST LS-RESPONSE.
       
       MAIN-PROCESS.
           PERFORM INITIALIZE-PROCESSING
           PERFORM PARSE-REQUEST
           PERFORM VALIDATE-PAYMENT
           
           IF WS-STATUS = '00'
               PERFORM CALCULATE-FEES
               PERFORM PROCESS-PAYMENT
               PERFORM UPDATE-ACCOUNT
               PERFORM LOG-PAYMENT
           END-IF
           
           PERFORM FORMAT-RESPONSE
           GOBACK.
       
       INITIALIZE-PROCESSING.
           INITIALIZE WS-RESPONSE
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           ACCEPT WS-CURRENT-TIME FROM TIME
           MOVE '00' TO WS-STATUS.
       
       PARSE-REQUEST.
           UNSTRING LS-REQUEST DELIMITED BY '|'
               INTO WS-PAYMENT-TYPE
                    WS-AMOUNT
                    WS-METHOD
                    WS-ACCOUNT
                    WS-REFERENCE.
       
       VALIDATE-PAYMENT.
           IF WS-AMOUNT <= ZERO
               MOVE '01' TO WS-STATUS
               MOVE "Invalid payment amount" TO WS-MESSAGE
               EXIT PARAGRAPH
           END-IF
           
           EVALUATE WS-METHOD
               WHEN "CARD"
               WHEN "ACH"
               WHEN "WIRE"
               WHEN "CHECK"
                   CONTINUE
               WHEN OTHER
                   MOVE '02' TO WS-STATUS
                   MOVE "Invalid payment method" TO WS-MESSAGE
                   EXIT PARAGRAPH
           END-EVALUATE
           
           EVALUATE WS-PAYMENT-TYPE
               WHEN "EARNEST"
               WHEN "DOWNPAYMENT"
               WHEN "COMMISSION"
               WHEN "CLOSING"
               WHEN "INSPECTION"
               WHEN "APPRAISAL"
                   CONTINUE
               WHEN OTHER
                   MOVE '03' TO WS-STATUS
                   MOVE "Invalid payment type" TO WS-MESSAGE
           END-EVALUATE.
       
       CALCULATE-FEES.
           EVALUATE WS-METHOD
               WHEN "CARD"
                   COMPUTE WS-FEE = WS-AMOUNT * (WS-CARD-FEE-PCT / 100)
               WHEN "ACH"
                   MOVE WS-ACH-FEE-FLAT TO WS-FEE
               WHEN "WIRE"
                   MOVE WS-WIRE-FEE-FLAT TO WS-FEE
               WHEN OTHER
                   MOVE ZERO TO WS-FEE
           END-EVALUATE
           
           COMPUTE WS-NET-AMOUNT = WS-AMOUNT - WS-FEE.
       
       PROCESS-PAYMENT.
           PERFORM GENERATE-PAYMENT-ID
           
           EVALUATE WS-METHOD
               WHEN "CARD"
                   PERFORM PROCESS-CARD-PAYMENT
               WHEN "ACH"
                   PERFORM PROCESS-ACH-PAYMENT
               WHEN "WIRE"
                   PERFORM PROCESS-WIRE-PAYMENT
               WHEN "CHECK"
                   PERFORM PROCESS-CHECK-PAYMENT
           END-EVALUATE.
       
       PROCESS-CARD-PAYMENT.
           MOVE "Payment processed via credit card" TO WS-MESSAGE.
       
       PROCESS-ACH-PAYMENT.
           MOVE "ACH transfer initiated" TO WS-MESSAGE.
       
       PROCESS-WIRE-PAYMENT.
           MOVE "Wire transfer processed" TO WS-MESSAGE.
       
       PROCESS-CHECK-PAYMENT.
           MOVE "Check payment recorded" TO WS-MESSAGE.
       
       UPDATE-ACCOUNT.
           OPEN I-O ACCOUNT-FILE
           
           IF WS-ACCT-STATUS NOT = '00'
               OPEN OUTPUT ACCOUNT-FILE
               INITIALIZE ACCOUNT-RECORD
               MOVE WS-ACCOUNT TO ACCOUNT-ID
               MOVE "ACTIVE" TO ACCOUNT-TYPE
               MOVE ZERO TO ACCOUNT-BALANCE
               MOVE 'A' TO ACCOUNT-STATUS
               WRITE ACCOUNT-RECORD
               CLOSE ACCOUNT-FILE
               OPEN I-O ACCOUNT-FILE
           END-IF
           
           MOVE WS-ACCOUNT TO ACCOUNT-ID
           READ ACCOUNT-FILE
               INVALID KEY
                   INITIALIZE ACCOUNT-RECORD
                   MOVE WS-ACCOUNT TO ACCOUNT-ID
                   MOVE "NEW" TO ACCOUNT-TYPE
                   MOVE WS-NET-AMOUNT TO ACCOUNT-BALANCE
                   MOVE 'A' TO ACCOUNT-STATUS
                   WRITE ACCOUNT-RECORD
               NOT INVALID KEY
                   ADD WS-NET-AMOUNT TO ACCOUNT-BALANCE
                   STRING WS-YEAR WS-MONTH WS-DAY
                          DELIMITED BY SIZE
                          INTO LAST-ACTIVITY
                   REWRITE ACCOUNT-RECORD
           END-READ
           
           CLOSE ACCOUNT-FILE.
       
       LOG-PAYMENT.
           OPEN EXTEND PAYMENT-FILE
           
           IF WS-FILE-STATUS NOT = '00'
               OPEN OUTPUT PAYMENT-FILE
           END-IF
           
           MOVE WS-PAYMENT-ID TO PAYMENT-ID
           MOVE FUNCTION CURRENT-DATE(1:8) TO PAYMENT-DATE
           STRING WS-HOUR WS-MINUTE WS-SECOND
                  DELIMITED BY SIZE
                  INTO PAYMENT-TIME
           MOVE WS-PAYMENT-TYPE TO PAYMENT-TYPE
           MOVE WS-AMOUNT TO PAYMENT-AMOUNT
           MOVE WS-METHOD TO PAYMENT-METHOD
           MOVE WS-ACCOUNT TO ACCOUNT-NUMBER
           MOVE WS-REFERENCE TO REFERENCE-NUMBER
           MOVE WS-STATUS TO PAYMENT-STATUS
           MOVE WS-FEE TO PROCESSING-FEE
           MOVE WS-NET-AMOUNT TO NET-AMOUNT
           
           WRITE PAYMENT-RECORD
           
           CLOSE PAYMENT-FILE.
       
       GENERATE-PAYMENT-ID.
           STRING "PAY" WS-YEAR WS-MONTH WS-DAY
                  WS-HOUR WS-MINUTE WS-SECOND
                  DELIMITED BY SIZE
                  INTO WS-PAYMENT-ID.
       
       FORMAT-RESPONSE.
           STRING WS-STATUS
                  "|" WS-PAYMENT-ID
                  "|" WS-MESSAGE
                  "|" WS-NET-AMOUNT
                  "|" WS-FEE
                  DELIMITED BY SIZE
                  INTO LS-RESPONSE.