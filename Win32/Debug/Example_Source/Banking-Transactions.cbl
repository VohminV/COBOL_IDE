       IDENTIFICATION DIVISION.
       PROGRAM-ID. BankingTransactions.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  MaxTransactions      PIC 9(3) VALUE 100.
       77  NumTransactions      PIC 9(3).
       77  I                    PIC 9(3) VALUE 1.
       77  TotalDebit           PIC 9(7)V99 VALUE 0.
       77  TotalCredit          PIC 9(7)V99 VALUE 0.
       77  Balance              PIC S9(7)V99 VALUE 0.
       77  TypeInput            PIC X.
       77  AmountInput          PIC 9(5)V99.

       01  Transactions.
           05  TransType    OCCURS 100 TIMES PIC X.
           05  TransAmount  OCCURS 100 TIMES PIC 9(5)V99.

       PROCEDURE DIVISION.
       Main-Logic.
           DISPLAY "Enter number of transactions: " WITH NO ADVANCING
           ACCEPT NumTransactions

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NumTransactions
               DISPLAY "Enter type (D/C) for transaction " I ": " WITH NO ADVANCING
               ACCEPT TypeInput
               DISPLAY "Enter amount: " WITH NO ADVANCING
               ACCEPT AmountInput

               MOVE TypeInput TO TransType(I)
               MOVE AmountInput TO TransAmount(I)
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NumTransactions
               IF TransType(I) = 'D' OR TransType(I) = 'd'
                   ADD TransAmount(I) TO TotalDebit
               ELSE
                   ADD TransAmount(I) TO TotalCredit
               END-IF
           END-PERFORM

           COMPUTE Balance = TotalCredit - TotalDebit

           DISPLAY " "
           DISPLAY "Total Debit:  " TotalDebit
           DISPLAY "Total Credit: " TotalCredit
           DISPLAY "Final Balance:" Balance

           STOP RUN.
