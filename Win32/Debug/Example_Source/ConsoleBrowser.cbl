IDENTIFICATION DIVISION.
PROGRAM-ID. ConsoleBrowser.

DATA DIVISION.
WORKING-STORAGE SECTION.

01 WS-CMD                PIC X(300).
01 WS-INPUT              PIC X(100).
01 WS-CHOICE             PIC 99.
01 WS-LOOP-FLAG          PIC X VALUE "Y".
01 LINK-COUNT            PIC 99 VALUE 5.
01 I                     PIC 99.

01 LINK-TEXT.
    05 LINK-TEXT-ENTRY OCCURS 5 TIMES.
        10 PIC X(50).

01 LINK-URL.
    05 LINK-URL-ENTRY OCCURS 5 TIMES.
        10 PIC X(100).

PROCEDURE DIVISION.
MAIN-LOGIC.

    *> Инициализация фейковых ссылок
    MOVE "Google"               TO LINK-TEXT-ENTRY(1)
    MOVE "https://www.google.com" TO LINK-URL-ENTRY(1)

    MOVE "Wikipedia"            TO LINK-TEXT-ENTRY(2)
    MOVE "https://www.wikipedia.org" TO LINK-URL-ENTRY(2)

    MOVE "Wiktionary"           TO LINK-TEXT-ENTRY(3)
    MOVE "https://www.wiktionary.org" TO LINK-URL-ENTRY(3)

    MOVE "Commons"              TO LINK-TEXT-ENTRY(4)
    MOVE "https://commons.wikimedia.org" TO LINK-URL-ENTRY(4)

    MOVE "MediaWiki"            TO LINK-TEXT-ENTRY(5)
    MOVE "https://www.mediawiki.org" TO LINK-URL-ENTRY(5)

    PERFORM UNTIL WS-LOOP-FLAG = "N"
        CALL 'SYSTEM' USING "clear" *> или "cls" на Windows
        DISPLAY "================ Console Browser ================"
        DISPLAY "[1] Home     [2] Refresh     [3] Exit"
        DISPLAY "--------------------------------------------------"

        DISPLAY "Available Links:"
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > LINK-COUNT
            DISPLAY " [" I "] " LINK-TEXT-ENTRY(I)
            DISPLAY "      > " LINK-URL-ENTRY(I)
        END-PERFORM

        DISPLAY "--------------------------------------------------"
        DISPLAY "Enter link number or [3] to Exit: "
        ACCEPT WS-CHOICE

        EVALUATE WS-CHOICE
            WHEN 1 THRU LINK-COUNT
                DISPLAY "Opening link: " LINK-URL-ENTRY(WS-CHOICE)
                STRING "start " LINK-URL-ENTRY(WS-CHOICE) DELIMITED BY SIZE INTO WS-CMD
                CALL "SYSTEM" USING WS-CMD
            WHEN 3
                MOVE "N" TO WS-LOOP-FLAG
            WHEN OTHER
                DISPLAY "Invalid input. Try again."
        END-EVALUATE

        DISPLAY "Press Enter to continue..."
        ACCEPT WS-INPUT
    END-PERFORM

    DISPLAY "Goodbye!".
    STOP RUN.
