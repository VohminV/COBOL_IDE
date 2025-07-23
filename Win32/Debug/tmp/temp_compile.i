#line 1 "temp_compile.cbl"
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BankTransactions.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.

 77 MaxTransactions PIC 9(3) VALUE 100.
 77 NumTransactions PIC 9(3).
 77 I PIC 9(3) VALUE 1.
 77 TotalDebit PIC 9(7)V99 VALUE 0.
 77 TotalCredit PIC 9(7)V99 VALUE 0.
 77 Balance PIC S9(7)V99 VALUE 0.
 77 TypeInput PIC X.
 77 AmountInput PIC 9(5)V99.
 77 Line PIC X(50).

 01 Transactions.
 05 TransType OCCURS 100 TIMES PIC X.
 05 TransAmount OCCURS 100 TIMES PIC 9(5)V99.

 PROCEDURE DIVISION.

 Main-Logic.
 DISPLAY "How many transactions (max 100)? " WITH NO ADVANCING
 ACCEPT NumTransactions

 PERFORM VARYING I FROM 1 BY 1 UNTIL I > NumTransactions
 MOVE SPACES TO Line
 STRING "Enter type (D/C) for transaction " 
 I DELIMITED BY SIZE
 ": " DELIMITED BY SIZE
 INTO Line
 DISPLAY Line WITH NO ADVANCING
 ACCEPT TypeInput

 DISPLAY "Enter amount: " WITH NO ADVANCING
 ACCEPT AmountInput

 MOVE TypeInput TO TransType(I)
 MOVE AmountInput TO TransAmount(I)

 EVALUATE TypeInput
 WHEN "D" 
 ADD AmountInput TO TotalDebit
 SUBTRACT AmountInput FROM Balance
 WHEN "C" 
 ADD AmountInput TO TotalCredit
 ADD AmountInput TO Balance
 WHEN OTHER
 DISPLAY "Invalid type, skipping..." 
 END-EVALUATE
 END-PERFORM

 DISPLAY " " 
 DISPLAY "Summary of Transactions" 
 DISPLAY "------------------------" 
 DISPLAY "Total Debit : " TotalDebit
 DISPLAY "Total Credit: " TotalCredit
 DISPLAY "Balance     : " Balance

 STOP RUN.
