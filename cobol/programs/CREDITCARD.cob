       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDITCARD.
       AUTHOR. SUITECRM-REAL-ESTATE.
       DATE-WRITTEN. 2025-01-22.
      *****************************************************************
      * CREDIT CARD VALIDATION AND PROCESSING FOR REAL ESTATE CRMS
      * HANDLES EARNEST MONEY, COMMISSIONS, AND PROPERTY PAYMENTS
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CARD-FILE ASSIGN TO "CARDDATA.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS CARD-NUMBER
                  FILE STATUS IS WS-FILE-STATUS.
                  
           SELECT TRANS-FILE ASSIGN TO "TRANSACTIONS.DAT"
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-TRANS-STATUS.
                  
           SELECT PROPERTY-FILE ASSIGN TO "PROPERTIES.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS PROPERTY-ID
                  FILE STATUS IS WS-PROP-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CARD-FILE.
       01  CARD-RECORD.
           05  CARD-NUMBER         PIC X(19).
           05  CARD-HOLDER-NAME    PIC X(30).
           05  EXPIRY-DATE.
               10  EXP-MONTH       PIC 99.
               10  EXP-YEAR        PIC 99.
           05  CARD-TYPE           PIC X(10).
           05  CREDIT-LIMIT        PIC 9(8)V99.
           05  CURRENT-BALANCE     PIC 9(8)V99.
           05  CARD-STATUS         PIC X.
               88  ACTIVE-CARD     VALUE 'A'.
               88  BLOCKED-CARD    VALUE 'B'.
               88  EXPIRED-CARD    VALUE 'E'.
           05  LAST-TRANSACTION    PIC X(8).
           05  RISK-SCORE          PIC 999.
       
       FD  TRANS-FILE.
       01  TRANS-RECORD.
           05  TRANS-ID            PIC X(20).
           05  TRANS-DATE          PIC 9(8).
           05  TRANS-TIME          PIC 9(6).
           05  TRANS-CARD-NUM      PIC X(19).
           05  TRANS-AMOUNT        PIC 9(8)V99.
           05  TRANS-TYPE          PIC X(10).
           05  TRANS-PROPERTY-ID   PIC X(20).
           05  TRANS-STATUS        PIC XX.
           05  AUTH-CODE           PIC X(6).
           05  RESPONSE-MSG        PIC X(50).
       
       FD  PROPERTY-FILE.
       01  PROPERTY-RECORD.
           05  PROPERTY-ID         PIC X(20).
           05  PROPERTY-ADDRESS    PIC X(100).
           05  LISTING-PRICE       PIC 9(9)V99.
           05  EARNEST-AMOUNT      PIC 9(7)V99.
           05  COMMISSION-RATE     PIC 99V99.
           05  AGENT-ID            PIC X(20).
           05  PROPERTY-STATUS     PIC X(20).
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS          PIC XX.
       01  WS-TRANS-STATUS         PIC XX.
       01  WS-PROP-STATUS          PIC XX.
       
       01  WS-REQUEST-DATA.
           05  WS-CARD-NUMBER      PIC X(19).
           05  WS-EXPIRY.
               10  WS-EXP-MONTH    PIC 99.
               10  WS-EXP-YEAR     PIC 99.
           05  WS-CVV              PIC 999.
           05  WS-AMOUNT           PIC 9(8)V99.
           05  WS-TRANS-TYPE       PIC X(10).
           05  WS-PROPERTY-ID      PIC X(20).
           05  WS-TIMESTAMP        PIC 9(14).
       
       01  WS-RESPONSE-DATA.
           05  WS-STATUS-CODE      PIC XX.
               88  APPROVED        VALUE '00'.
               88  DECLINED        VALUE '01'.
               88  INSUFF-FUNDS    VALUE '02'.
               88  INVALID-CARD    VALUE '03'.
               88  CARD-EXPIRED    VALUE '04'.
               88  INVALID-CVV     VALUE '05'.
               88  FRAUD-ALERT     VALUE '06'.
               88  SYSTEM-ERROR    VALUE '07'.
           05  WS-TRANS-ID         PIC X(20).
           05  WS-AUTH-CODE        PIC X(6).
           05  WS-RESPONSE-MSG     PIC X(50).
           05  WS-AVAILABLE-BAL    PIC 9(8)V99.
           05  WS-CARD-TYPE        PIC X(10).
           05  WS-RISK-SCORE       PIC 999.
       
       01  WS-CURRENT-DATE.
           05  WS-YEAR             PIC 9(4).
           05  WS-MONTH            PIC 99.
           05  WS-DAY              PIC 99.
       
       01  WS-CURRENT-TIME.
           05  WS-HOUR             PIC 99.
           05  WS-MINUTE           PIC 99.
           05  WS-SECOND           PIC 99.
       
       01  WS-LUHN-CHECK.
           05  WS-DIGIT            PIC 9.
           05  WS-SUM              PIC 999.
           05  WS-DOUBLE           PIC 99.
           05  WS-INDEX            PIC 99.
           05  WS-VALID-LUHN       PIC X VALUE 'N'.
       
       01  WS-COMMISSION-CALC.
           05  WS-SALE-PRICE       PIC 9(9)V99.
           05  WS-COMM-RATE        PIC 99V99.
           05  WS-COMM-AMOUNT      PIC 9(7)V99.
       
       LINKAGE SECTION.
       01  LS-REQUEST              PIC X(80).
       01  LS-RESPONSE             PIC X(101).
       
       PROCEDURE DIVISION USING LS-REQUEST LS-RESPONSE.
       
       MAIN-PROCESS.
           PERFORM INITIALIZE-RESPONSE
           PERFORM PARSE-REQUEST
           
           EVALUATE WS-TRANS-TYPE
               WHEN "VALIDATE"
                   PERFORM VALIDATE-CARD
               WHEN "EARNEST"
                   PERFORM PROCESS-EARNEST-MONEY
               WHEN "COMMISSION"
                   PERFORM PROCESS-COMMISSION
               WHEN "PAYMENT"
                   PERFORM PROCESS-PAYMENT
               WHEN OTHER
                   MOVE '07' TO WS-STATUS-CODE
                   MOVE "Invalid transaction type" TO WS-RESPONSE-MSG
           END-EVALUATE
           
           PERFORM FORMAT-RESPONSE
           GOBACK.
       
       INITIALIZE-RESPONSE.
           INITIALIZE WS-RESPONSE-DATA
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           ACCEPT WS-CURRENT-TIME FROM TIME.
       
       PARSE-REQUEST.
           UNSTRING LS-REQUEST DELIMITED BY SPACES
               INTO WS-CARD-NUMBER
                    WS-EXPIRY
                    WS-CVV
                    WS-AMOUNT
                    WS-TRANS-TYPE
                    WS-PROPERTY-ID
                    WS-TIMESTAMP.
       
       VALIDATE-CARD.
           PERFORM CHECK-LUHN-ALGORITHM
           
           IF WS-VALID-LUHN = 'N'
               MOVE '03' TO WS-STATUS-CODE
               MOVE "Invalid card number - failed Luhn check" 
                   TO WS-RESPONSE-MSG
               EXIT PARAGRAPH
           END-IF
           
           PERFORM CHECK-CARD-TYPE
           
           OPEN INPUT CARD-FILE
           
           IF WS-FILE-STATUS NOT = '00'
               MOVE '07' TO WS-STATUS-CODE
               MOVE "System error - cannot access card file" 
                   TO WS-RESPONSE-MSG
               EXIT PARAGRAPH
           END-IF
           
           MOVE WS-CARD-NUMBER TO CARD-NUMBER
           
           READ CARD-FILE
               INVALID KEY
                   MOVE '03' TO WS-STATUS-CODE
                   MOVE "Card not found in system" TO WS-RESPONSE-MSG
               NOT INVALID KEY
                   PERFORM VALIDATE-CARD-DETAILS
           END-READ
           
           CLOSE CARD-FILE.
       
       CHECK-LUHN-ALGORITHM.
           MOVE ZERO TO WS-SUM
           MOVE 'Y' TO WS-VALID-LUHN
           
           PERFORM VARYING WS-INDEX FROM 16 BY -1 UNTIL WS-INDEX < 1
               MOVE WS-CARD-NUMBER(WS-INDEX:1) TO WS-DIGIT
               
               IF FUNCTION MOD(16 - WS-INDEX + 1, 2) = 0
                   MULTIPLY WS-DIGIT BY 2 GIVING WS-DOUBLE
                   IF WS-DOUBLE > 9
                       SUBTRACT 9 FROM WS-DOUBLE
                   END-IF
                   ADD WS-DOUBLE TO WS-SUM
               ELSE
                   ADD WS-DIGIT TO WS-SUM
               END-IF
           END-PERFORM
           
           IF FUNCTION MOD(WS-SUM, 10) NOT = 0
               MOVE 'N' TO WS-VALID-LUHN
           END-IF.
       
       CHECK-CARD-TYPE.
           EVALUATE TRUE
               WHEN WS-CARD-NUMBER(1:1) = '4'
                   MOVE 'VISA' TO WS-CARD-TYPE
               WHEN WS-CARD-NUMBER(1:2) >= '51' AND <= '55'
                   MOVE 'MASTERCARD' TO WS-CARD-TYPE
               WHEN WS-CARD-NUMBER(1:2) = '34' OR '37'
                   MOVE 'AMEX' TO WS-CARD-TYPE
               WHEN WS-CARD-NUMBER(1:4) = '6011'
                   MOVE 'DISCOVER' TO WS-CARD-TYPE
               WHEN OTHER
                   MOVE 'UNKNOWN' TO WS-CARD-TYPE
           END-EVALUATE.
       
       VALIDATE-CARD-DETAILS.
           IF EXPIRED-CARD
               MOVE '04' TO WS-STATUS-CODE
               MOVE "Card has expired" TO WS-RESPONSE-MSG
               EXIT PARAGRAPH
           END-IF
           
           IF BLOCKED-CARD
               MOVE '01' TO WS-STATUS-CODE
               MOVE "Card is blocked" TO WS-RESPONSE-MSG
               EXIT PARAGRAPH
           END-IF
           
           IF EXP-YEAR < WS-YEAR(3:2) OR
              (EXP-YEAR = WS-YEAR(3:2) AND EXP-MONTH < WS-MONTH)
               MOVE '04' TO WS-STATUS-CODE
               MOVE "Card has expired" TO WS-RESPONSE-MSG
               EXIT PARAGRAPH
           END-IF
           
           COMPUTE WS-AVAILABLE-BAL = CREDIT-LIMIT - CURRENT-BALANCE
           
           IF WS-AMOUNT > WS-AVAILABLE-BAL
               MOVE '02' TO WS-STATUS-CODE
               MOVE "Insufficient funds" TO WS-RESPONSE-MSG
               EXIT PARAGRAPH
           END-IF
           
           MOVE RISK-SCORE TO WS-RISK-SCORE
           
           IF RISK-SCORE > 800
               MOVE '06' TO WS-STATUS-CODE
               MOVE "Transaction flagged for fraud review" 
                   TO WS-RESPONSE-MSG
               EXIT PARAGRAPH
           END-IF
           
           MOVE '00' TO WS-STATUS-CODE
           MOVE "Card validated successfully" TO WS-RESPONSE-MSG
           PERFORM GENERATE-AUTH-CODE.
       
       PROCESS-EARNEST-MONEY.
           PERFORM VALIDATE-CARD
           
           IF NOT APPROVED
               EXIT PARAGRAPH
           END-IF
           
           OPEN INPUT PROPERTY-FILE
           
           MOVE WS-PROPERTY-ID TO PROPERTY-ID
           READ PROPERTY-FILE
               INVALID KEY
                   MOVE '07' TO WS-STATUS-CODE
                   MOVE "Property not found" TO WS-RESPONSE-MSG
                   CLOSE PROPERTY-FILE
                   EXIT PARAGRAPH
           END-READ
           
           IF WS-AMOUNT NOT = EARNEST-AMOUNT
               MOVE '01' TO WS-STATUS-CODE
               MOVE "Amount does not match earnest requirement" 
                   TO WS-RESPONSE-MSG
               CLOSE PROPERTY-FILE
               EXIT PARAGRAPH
           END-IF
           
           CLOSE PROPERTY-FILE
           
           PERFORM PROCESS-TRANSACTION
           
           IF APPROVED
               MOVE "Earnest money processed successfully" 
                   TO WS-RESPONSE-MSG
           END-IF.
       
       PROCESS-COMMISSION.
           PERFORM VALIDATE-CARD
           
           IF NOT APPROVED
               EXIT PARAGRAPH
           END-IF
           
           OPEN INPUT PROPERTY-FILE
           
           MOVE WS-PROPERTY-ID TO PROPERTY-ID
           READ PROPERTY-FILE
               INVALID KEY
                   MOVE '07' TO WS-STATUS-CODE
                   MOVE "Property not found" TO WS-RESPONSE-MSG
                   CLOSE PROPERTY-FILE
                   EXIT PARAGRAPH
           END-READ
           
           COMPUTE WS-COMM-AMOUNT = LISTING-PRICE * (COMMISSION-RATE / 100)
           
           IF WS-AMOUNT NOT = WS-COMM-AMOUNT
               MOVE '01' TO WS-STATUS-CODE
               STRING "Commission amount mismatch. Expected: " 
                      WS-COMM-AMOUNT
                      INTO WS-RESPONSE-MSG
               CLOSE PROPERTY-FILE
               EXIT PARAGRAPH
           END-IF
           
           CLOSE PROPERTY-FILE
           
           PERFORM PROCESS-TRANSACTION
           
           IF APPROVED
               MOVE "Commission payment processed" TO WS-RESPONSE-MSG
           END-IF.
       
       PROCESS-PAYMENT.
           PERFORM VALIDATE-CARD
           
           IF NOT APPROVED
               EXIT PARAGRAPH
           END-IF
           
           PERFORM PROCESS-TRANSACTION.
       
       PROCESS-TRANSACTION.
           OPEN I-O CARD-FILE
           
           MOVE WS-CARD-NUMBER TO CARD-NUMBER
           READ CARD-FILE
               INVALID KEY
                   MOVE '07' TO WS-STATUS-CODE
                   MOVE "Card record not found" TO WS-RESPONSE-MSG
                   CLOSE CARD-FILE
                   EXIT PARAGRAPH
           END-READ
           
           ADD WS-AMOUNT TO CURRENT-BALANCE
           
           STRING WS-YEAR WS-MONTH WS-DAY 
                  DELIMITED BY SIZE
                  INTO LAST-TRANSACTION
           
           REWRITE CARD-RECORD
           
           CLOSE CARD-FILE
           
           PERFORM WRITE-TRANSACTION-LOG.
       
       WRITE-TRANSACTION-LOG.
           OPEN EXTEND TRANS-FILE
           
           PERFORM GENERATE-TRANS-ID
           
           MOVE WS-TRANS-ID TO TRANS-ID
           MOVE FUNCTION CURRENT-DATE(1:8) TO TRANS-DATE
           MOVE WS-CURRENT-TIME TO TRANS-TIME
           MOVE WS-CARD-NUMBER TO TRANS-CARD-NUM
           MOVE WS-AMOUNT TO TRANS-AMOUNT
           MOVE WS-TRANS-TYPE TO TRANS-TYPE
           MOVE WS-PROPERTY-ID TO TRANS-PROPERTY-ID
           MOVE WS-STATUS-CODE TO TRANS-STATUS
           MOVE WS-AUTH-CODE TO AUTH-CODE
           MOVE WS-RESPONSE-MSG TO RESPONSE-MSG
           
           WRITE TRANS-RECORD
           
           CLOSE TRANS-FILE.
       
       GENERATE-AUTH-CODE.
           STRING WS-HOUR WS-MINUTE WS-SECOND
                  DELIMITED BY SIZE
                  INTO WS-AUTH-CODE.
       
       GENERATE-TRANS-ID.
           STRING "TRN" WS-YEAR WS-MONTH WS-DAY 
                  WS-HOUR WS-MINUTE WS-SECOND
                  DELIMITED BY SIZE
                  INTO WS-TRANS-ID.
       
       FORMAT-RESPONSE.
           STRING WS-STATUS-CODE
                  WS-TRANS-ID
                  WS-AUTH-CODE
                  WS-RESPONSE-MSG
                  WS-AVAILABLE-BAL
                  WS-CARD-TYPE
                  WS-RISK-SCORE
                  DELIMITED BY SIZE
                  INTO LS-RESPONSE.