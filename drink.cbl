IDENTIFICATION DIVISION.
      PROGRAM-ID. API-DRINK.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CMD-LINE        PIC X(100).
       
      *> 用來接收參數的變數
       01  D-ID            PIC X(1).
       01  D-NAME          PIC X(20).
       01  D-PHONE         PIC X(15).
       01  D-QTY-STR       PIC X(5).
       
      *> 運算用變數
       01  D-QTY           PIC 9(3).
       01  PRICE           PIC 9(3).
       01  TOTAL           PIC 9(5).
       01  ITEM-NAME       PIC X(10).

       PROCEDURE DIVISION.
      *> 1. 抓取指令參數
           ACCEPT CMD-LINE FROM COMMAND-LINE.
           
      *> 2. 切割參數 (ID, 姓名, 電話, 數量)
           UNSTRING CMD-LINE DELIMITED BY SPACE
               INTO D-ID, D-NAME, D-PHONE, D-QTY-STR.

      *> 3. 轉成數字
           COMPUTE D-QTY = FUNCTION NUMVAL(D-QTY-STR).

      *> 4. 判斷邏輯
           IF D-ID = "1"
               MOVE 30 TO PRICE
               MOVE "紅茶" TO ITEM-NAME
           ELSE
               MOVE 50 TO PRICE
               MOVE "奶茶" TO ITEM-NAME
           END-IF.
           
      *> 5. 計算總價
           COMPUTE TOTAL = PRICE * D-QTY.

      *> 6. 輸出結果 (Python 會抓這一段文字)
           DISPLAY "收據: " 
                   FUNCTION TRIM(D-NAME) " " 
                   FUNCTION TRIM(D-PHONE) " 買了 "
                   FUNCTION TRIM(ITEM-NAME) " x" D-QTY "杯，"
                   "共 " TOTAL " 元".
           
           STOP RUN.
