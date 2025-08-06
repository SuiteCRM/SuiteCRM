       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINFRAME.
       AUTHOR. SUITECRM-REAL-ESTATE.
       DATE-WRITTEN. 2025-01-22.
      *****************************************************************
      * MAINFRAME SYNCHRONIZATION MODULE FOR LEGACY SYSTEMS
      * HANDLES DATA SYNC WITH ENTERPRISE REAL ESTATE SYSTEMS
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYNC-FILE ASSIGN TO "SYNC-LOG.DAT"
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-FILE-STATUS.
                  
           SELECT PROPERTY-MASTER ASSIGN TO "PROPERTY-MASTER.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS PM-PROPERTY-ID
                  FILE STATUS IS WS-PROP-STATUS.
                  
           SELECT AGENT-MASTER ASSIGN TO "AGENT-MASTER.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS AM-AGENT-ID
                  FILE STATUS IS WS-AGENT-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  SYNC-FILE.
       01  SYNC-RECORD.
           05  SYNC-ID             PIC X(20).
           05  SYNC-TYPE           PIC X(20).
           05  SYNC-TIMESTAMP      PIC 9(14).
           05  SYNC-STATUS         PIC XX.
           05  SYNC-RECORDS        PIC 9(6).
           05  SYNC-MESSAGE        PIC X(100).
       
       FD  PROPERTY-MASTER.
       01  PROPERTY-MASTER-REC.
           05  PM-PROPERTY-ID      PIC X(20).
           05  PM-MLS-NUMBER       PIC X(20).
           05  PM-ADDRESS          PIC X(100).
           05  PM-CITY             PIC X(30).
           05  PM-STATE            PIC XX.
           05  PM-ZIP              PIC X(10).
           05  PM-LIST-PRICE       PIC 9(9)V99.
           05  PM-BEDROOMS         PIC 99.
           05  PM-BATHROOMS        PIC 99V9.
           05  PM-SQUARE-FEET      PIC 9(6).
           05  PM-LOT-SIZE         PIC 9(6)V99.
           05  PM-YEAR-BUILT       PIC 9(4).
           05  PM-STATUS           PIC X(20).
           05  PM-LIST-DATE        PIC 9(8).
           05  PM-LAST-UPDATE      PIC 9(14).
       
       FD  AGENT-MASTER.
       01  AGENT-MASTER-REC.
           05  AM-AGENT-ID         PIC X(20).
           05  AM-LICENSE-NUMBER   PIC X(20).
           05  AM-FIRST-NAME       PIC X(30).
           05  AM-LAST-NAME        PIC X(30).
           05  AM-EMAIL            PIC X(50).
           05  AM-PHONE            PIC X(20).
           05  AM-OFFICE-ID        PIC X(20).
           05  AM-COMMISSION-RATE  PIC 99V99.
           05  AM-STATUS           PIC X.
           05  AM-JOIN-DATE        PIC 9(8).
           05  AM-YTD-SALES        PIC 9(9)V99.
           05  AM-TOTAL-SALES      PIC 9(9)V99.
       
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS          PIC XX.
       01  WS-PROP-STATUS          PIC XX.
       01  WS-AGENT-STATUS         PIC XX.
       
       01  WS-SYNC-REQUEST.
           05  WS-SYNC-TYPE        PIC X(20).
           05  WS-SYNC-DATA        PIC X(500).
       
       01  WS-SYNC-RESPONSE.
           05  WS-STATUS           PIC XX.
           05  WS-SYNC-ID          PIC X(20).
           05  WS-RECORDS-SYNCED   PIC 9(6).
           05  WS-MESSAGE          PIC X(100).
       
       01  WS-COUNTERS.
           05  WS-RECORDS-READ     PIC 9(6) VALUE ZERO.
           05  WS-RECORDS-UPDATED  PIC 9(6) VALUE ZERO.
           05  WS-RECORDS-ADDED    PIC 9(6) VALUE ZERO.
           05  WS-RECORDS-ERROR    PIC 9(6) VALUE ZERO.
       
       01  WS-CURRENT-TIMESTAMP.
           05  WS-YEAR             PIC 9(4).
           05  WS-MONTH            PIC 99.
           05  WS-DAY              PIC 99.
           05  WS-HOUR             PIC 99.
           05  WS-MINUTE           PIC 99.
           05  WS-SECOND           PIC 99.
       
       01  WS-JSON-PARSER.
           05  WS-JSON-TYPE        PIC X(20).
           05  WS-JSON-ID          PIC X(20).
           05  WS-JSON-DATA        PIC X(400).
       
       LINKAGE SECTION.
       01  LS-REQUEST              PIC X(520).
       01  LS-RESPONSE             PIC X(150).
       
       PROCEDURE DIVISION USING LS-REQUEST LS-RESPONSE.
       
       MAIN-PROCESS.
           PERFORM INITIALIZE-SYNC
           PERFORM PARSE-SYNC-REQUEST
           
           EVALUATE WS-SYNC-TYPE
               WHEN "PROPERTY-SYNC"
                   PERFORM SYNC-PROPERTIES
               WHEN "AGENT-SYNC"
                   PERFORM SYNC-AGENTS
               WHEN "FULL-SYNC"
                   PERFORM FULL-SYSTEM-SYNC
               WHEN "STATUS-CHECK"
                   PERFORM CHECK-SYNC-STATUS
               WHEN "VALIDATE-MLS"
                   PERFORM VALIDATE-MLS-DATA
               WHEN OTHER
                   MOVE '99' TO WS-STATUS
                   MOVE "Invalid sync type" TO WS-MESSAGE
           END-EVALUATE
           
           PERFORM LOG-SYNC-ACTIVITY
           PERFORM FORMAT-RESPONSE
           GOBACK.
       
       INITIALIZE-SYNC.
           INITIALIZE WS-SYNC-RESPONSE WS-COUNTERS
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP
           MOVE '00' TO WS-STATUS
           PERFORM GENERATE-SYNC-ID.
       
       PARSE-SYNC-REQUEST.
           MOVE LS-REQUEST(1:20) TO WS-SYNC-TYPE
           MOVE LS-REQUEST(21:500) TO WS-SYNC-DATA.
       
       SYNC-PROPERTIES.
           OPEN I-O PROPERTY-MASTER
           
           IF WS-PROP-STATUS NOT = '00'
               OPEN OUTPUT PROPERTY-MASTER
           END-IF
           
           PERFORM PARSE-PROPERTY-DATA
           
           MOVE WS-JSON-ID TO PM-PROPERTY-ID
           READ PROPERTY-MASTER
               INVALID KEY
                   PERFORM ADD-NEW-PROPERTY
               NOT INVALID KEY
                   PERFORM UPDATE-PROPERTY
           END-READ
           
           CLOSE PROPERTY-MASTER
           
           COMPUTE WS-RECORDS-SYNCED = WS-RECORDS-ADDED + WS-RECORDS-UPDATED
           MOVE "Property sync completed" TO WS-MESSAGE.
       
       PARSE-PROPERTY-DATA.
           UNSTRING WS-SYNC-DATA DELIMITED BY '|'
               INTO PM-PROPERTY-ID
                    PM-MLS-NUMBER
                    PM-ADDRESS
                    PM-CITY
                    PM-STATE
                    PM-ZIP
                    PM-LIST-PRICE
                    PM-BEDROOMS
                    PM-BATHROOMS
                    PM-SQUARE-FEET
                    PM-STATUS.
       
       ADD-NEW-PROPERTY.
           MOVE FUNCTION CURRENT-DATE(1:8) TO PM-LIST-DATE
           STRING WS-YEAR WS-MONTH WS-DAY
                  WS-HOUR WS-MINUTE WS-SECOND
                  DELIMITED BY SIZE
                  INTO PM-LAST-UPDATE
           
           WRITE PROPERTY-MASTER-REC
               INVALID KEY
                   ADD 1 TO WS-RECORDS-ERROR
               NOT INVALID KEY
                   ADD 1 TO WS-RECORDS-ADDED
           END-WRITE.
       
       UPDATE-PROPERTY.
           STRING WS-YEAR WS-MONTH WS-DAY
                  WS-HOUR WS-MINUTE WS-SECOND
                  DELIMITED BY SIZE
                  INTO PM-LAST-UPDATE
           
           REWRITE PROPERTY-MASTER-REC
               INVALID KEY
                   ADD 1 TO WS-RECORDS-ERROR
               NOT INVALID KEY
                   ADD 1 TO WS-RECORDS-UPDATED
           END-REWRITE.
       
       SYNC-AGENTS.
           OPEN I-O AGENT-MASTER
           
           IF WS-AGENT-STATUS NOT = '00'
               OPEN OUTPUT AGENT-MASTER
           END-IF
           
           PERFORM PARSE-AGENT-DATA
           
           MOVE WS-JSON-ID TO AM-AGENT-ID
           READ AGENT-MASTER
               INVALID KEY
                   PERFORM ADD-NEW-AGENT
               NOT INVALID KEY
                   PERFORM UPDATE-AGENT
           END-READ
           
           CLOSE AGENT-MASTER
           
           COMPUTE WS-RECORDS-SYNCED = WS-RECORDS-ADDED + WS-RECORDS-UPDATED
           MOVE "Agent sync completed" TO WS-MESSAGE.
       
       PARSE-AGENT-DATA.
           UNSTRING WS-SYNC-DATA DELIMITED BY '|'
               INTO AM-AGENT-ID
                    AM-LICENSE-NUMBER
                    AM-FIRST-NAME
                    AM-LAST-NAME
                    AM-EMAIL
                    AM-PHONE
                    AM-OFFICE-ID
                    AM-COMMISSION-RATE
                    AM-STATUS.
       
       ADD-NEW-AGENT.
           MOVE FUNCTION CURRENT-DATE(1:8) TO AM-JOIN-DATE
           MOVE ZERO TO AM-YTD-SALES
           MOVE ZERO TO AM-TOTAL-SALES
           
           WRITE AGENT-MASTER-REC
               INVALID KEY
                   ADD 1 TO WS-RECORDS-ERROR
               NOT INVALID KEY
                   ADD 1 TO WS-RECORDS-ADDED
           END-WRITE.
       
       UPDATE-AGENT.
           REWRITE AGENT-MASTER-REC
               INVALID KEY
                   ADD 1 TO WS-RECORDS-ERROR
               NOT INVALID KEY
                   ADD 1 TO WS-RECORDS-UPDATED
           END-REWRITE.
       
       FULL-SYSTEM-SYNC.
           PERFORM SYNC-PROPERTIES
           PERFORM SYNC-AGENTS
           MOVE "Full system sync completed" TO WS-MESSAGE.
       
       CHECK-SYNC-STATUS.
           OPEN INPUT SYNC-FILE
           
           IF WS-FILE-STATUS = '00'
               PERFORM UNTIL WS-FILE-STATUS NOT = '00'
                   READ SYNC-FILE
                       AT END
                           CONTINUE
                       NOT AT END
                           ADD 1 TO WS-RECORDS-READ
                   END-READ
               END-PERFORM
               CLOSE SYNC-FILE
               MOVE WS-RECORDS-READ TO WS-RECORDS-SYNCED
               MOVE "Sync status retrieved" TO WS-MESSAGE
           ELSE
               MOVE "No sync history available" TO WS-MESSAGE
           END-IF.
       
       VALIDATE-MLS-DATA.
           OPEN INPUT PROPERTY-MASTER
           
           IF WS-PROP-STATUS = '00'
               MOVE WS-JSON-ID TO PM-PROPERTY-ID
               READ PROPERTY-MASTER
                   INVALID KEY
                       MOVE '01' TO WS-STATUS
                       MOVE "MLS property not found" TO WS-MESSAGE
                   NOT INVALID KEY
                       MOVE '00' TO WS-STATUS
                       STRING "MLS " PM-MLS-NUMBER " validated"
                              DELIMITED BY SIZE
                              INTO WS-MESSAGE
               END-READ
               CLOSE PROPERTY-MASTER
           ELSE
               MOVE '99' TO WS-STATUS
               MOVE "Cannot access property database" TO WS-MESSAGE
           END-IF.
       
       LOG-SYNC-ACTIVITY.
           OPEN EXTEND SYNC-FILE
           
           IF WS-FILE-STATUS NOT = '00'
               OPEN OUTPUT SYNC-FILE
           END-IF
           
           MOVE WS-SYNC-ID TO SYNC-ID
           MOVE WS-SYNC-TYPE TO SYNC-TYPE
           STRING WS-YEAR WS-MONTH WS-DAY
                  WS-HOUR WS-MINUTE WS-SECOND
                  DELIMITED BY SIZE
                  INTO SYNC-TIMESTAMP
           MOVE WS-STATUS TO SYNC-STATUS
           MOVE WS-RECORDS-SYNCED TO SYNC-RECORDS
           MOVE WS-MESSAGE TO SYNC-MESSAGE
           
           WRITE SYNC-RECORD
           
           CLOSE SYNC-FILE.
       
       GENERATE-SYNC-ID.
           STRING "SYNC" WS-YEAR WS-MONTH WS-DAY
                  WS-HOUR WS-MINUTE WS-SECOND
                  DELIMITED BY SIZE
                  INTO WS-SYNC-ID.
       
       FORMAT-RESPONSE.
           STRING WS-STATUS
                  "|" WS-SYNC-ID
                  "|" WS-RECORDS-SYNCED
                  "|" WS-MESSAGE
                  DELIMITED BY SIZE
                  INTO LS-RESPONSE.