       IDENTIFICATION DIVISION.
       PROGRAM-ID. TransactionSummary.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  I                     PIC 9(3).
       77  NumTransactions      PIC 9(3) VALUE 0.
       77  TypeInput            PIC X.
       77  AmountInput          PIC 9(5)V99.
       77  PromptLine           PIC X(60).
       77  FinalBalance         PIC S9(7)V99 VALUE 0.

       01  Transactions.
           05  TransType       OCCURS 100 TIMES PIC X.
           05  TransAmount     OCCURS 100 TIMES PIC 9(5)V99.

       PROCEDURE DIVISION.
       Main-Logic.

           DISPLAY "How many transactions (max 100)? " WITH NO ADVANCING
           ACCEPT NumTransactions

           IF NumTransactions < 1 OR NumTransactions > 100
               DISPLAY "Invalid number of transactions. Must be 1-100."
               STOP RUN
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NumTransactions

               MOVE SPACES TO PromptLine
               STRING "Enter type (D/C) for transaction "
                      I DELIMITED BY SIZE
                      ": " DELIMITED BY SIZE
                   INTO PromptLine
               DISPLAY PromptLine WITH NO ADVANCING
               ACCEPT TypeInput

               *> Преобразуем к верхнему регистру, если введена строчная буква
               IF TypeInput >= "a" AND TypeInput <= "z"
                   MOVE FUNCTION UPPER-CASE(TypeInput) TO TypeInput
               END-IF

               IF NOT (TypeInput = "D" OR TypeInput = "C")
                   DISPLAY "Invalid type, skipping..."
                   CONTINUE
               END-IF

               DISPLAY "Enter amount: " WITH NO ADVANCING
               ACCEPT AmountInput

               *> Простейшая проверка — нулевая сумма игнорируется
               IF AmountInput = 0
                   DISPLAY "Zero amount, skipping..."
                   CONTINUE
               END-IF

               MOVE TypeInput TO TransType(I)
               MOVE AmountInput TO TransAmount(I)

               *> Обновляем итоговый баланс
               IF TypeInput = "D"
                   ADD AmountInput TO FinalBalance
               ELSE
                   SUBTRACT AmountInput FROM FinalBalance
               END-IF

           END-PERFORM

           DISPLAY "-------------------------------"
           DISPLAY "Final Balance: " FinalBalance
           DISPLAY "-------------------------------"

           STOP RUN.
