       IDENTIFICATION DIVISION.
       PROGRAM-ID. Sales-Analysis.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Sales        PIC 9(5) OCCURS 7 TIMES.
       01  Days.
           05 DayName PIC X(9) OCCURS 7 TIMES.

       01  Total        PIC 9(7) VALUE 0.
       01  Average      PIC 9(7)V99 VALUE 0.
       01  MaxSale      PIC 9(5) VALUE 0.
       01  MaxIndex     PIC 9 VALUE 1.
       01  I            PIC 9 VALUE 1.

       PROCEDURE DIVISION.
       Main-Logic.
           MOVE "Monday   " TO DayName(1)
           MOVE "Tuesday  " TO DayName(2)
           MOVE "Wednesday" TO DayName(3)
           MOVE "Thursday " TO DayName(4)
           MOVE "Friday   " TO DayName(5)
           MOVE "Saturday " TO DayName(6)
           MOVE "Sunday   " TO DayName(7)

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 7
               DISPLAY "Enter sales for " DayName(I) ": "
               ACCEPT Sales(I)
               ADD Sales(I) TO Total
           END-PERFORM

           COMPUTE Average = Total / 7

           MOVE Sales(1) TO MaxSale
           MOVE 1 TO MaxIndex
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 7
               IF Sales(I) > MaxSale THEN
                   MOVE Sales(I) TO MaxSale
                   MOVE I TO MaxIndex
               END-IF
           END-PERFORM

           DISPLAY "Total sales: " Total
           DISPLAY "Average sales: " Average
           DISPLAY "Best day: " DayName(MaxIndex)

           STOP RUN.
