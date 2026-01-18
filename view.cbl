IDENTIFICATION DIVISION.
       PROGRAM-ID. VIEW-ORDERS.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORDER-FILE ASSIGN TO "orders.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ORDER-FILE.
       01  ORDER-RECORD.
           05  R-NAME      PIC X(20).
           05  R-PHONE     PIC X(15).
           05  R-ITEM      PIC X(10).
           05  R-QTY       PIC 9(3).
           05  R-PRICE     PIC 9(5).

       WORKING-STORAGE SECTION.
       01  WS-EOF          PIC A(1) VALUE 'N'.

       PROCEDURE DIVISION.
           OPEN INPUT ORDER-FILE.
           
      *> 讀取直到檔案結束 (EOF)
           PERFORM UNTIL WS-EOF = 'Y'
               READ ORDER-FILE
                   AT END 
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
      *> 把每一筆資料包裝成 HTML 表格列印出來
                       DISPLAY "<tr>"
                       DISPLAY "<td>" FUNCTION TRIM(R-NAME) "</td>"
                       DISPLAY "<td>" FUNCTION TRIM(R-PHONE) "</td>"
                       DISPLAY "<td>" FUNCTION TRIM(R-ITEM) "</td>"
                       DISPLAY "<td>" R-QTY "</td>"
                       DISPLAY "<td>" R-PRICE "</td>"
                       DISPLAY "</tr>"
               END-READ
           END-PERFORM.

           CLOSE ORDER-FILE.
           STOP RUN.
           