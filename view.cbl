******************************************************************
      * Author: 1959製茶所開發團隊
      * Date: 2026-01-24
      * Purpose: 讀取 Python 產生的訂單檔，進行財務稽核與報表統計
      ******************************************************************
       IDENTIFICATION DIVISION.*>標誌部 Week 1練習手冊
       PROGRAM-ID. VIEW-ORDERS.*>隨便取的 Week 3 講義 (二版) 第 38 頁
      *> [身分證] 程式名稱叫做 VIEW-ORDERS。

       ENVIRONMENT DIVISION.*>環境部 Week 1 練習手冊
       INPUT-OUTPUT SECTION.*>負責輸出輸入
       FILE-CONTROL.
      *>程式裡的變數名稱，對應到電腦裡的哪個檔案 Week 3 講義 (一版) 第 21 頁
      *> --- 檔案控制台：設定外部檔案連線 ---
      
           SELECT ORDER-FILE ASSIGN TO "orders.txt"
           *>讀取 "orders.txt" 檔案
           *>SELECT ORDER-FILE：幫檔案取一個 「程式內部的暱稱」 (叫做 ORDER-FILE)。
           *>以後在程式碼裡寫 READ ORDER-FILE，程式就知道要讀誰
           *>ASSIGN TO "orders.txt"：指定這個暱稱對應到 「硬碟上真正的檔名」
           ORGANIZATION IS LINE SEQUENTIAL.
           *>ORGANIZATION：定義檔案的 「組織結構」
      *> [設定] 讀取 "orders.txt"。
      *> [解釋] LINE SEQUENTIAL 代表它是普通的文字檔，一行一行讀 (像記事本一樣)。

           SELECT REPORT-FILE ASSIGN TO "report.txt"
           *>SELECT：選定一個內部代號
           *>REPORT-FILE：這是幫它取的 「內部代號」 (在程式裡用的名字)
           *>ASSIGN：指派/連結。
           *>"report.txt"：這是 「外部真實檔名」
           ORGANIZATION IS LINE SEQUENTIAL.*>一行一行產生
      *> [設定] 輸出 "report.txt"。這是給 Python 讀取的成績單。

       DATA DIVISION.*>Week 1 練習手冊
       FILE SECTION.*>詳細描述檔案長什麼樣子 Week 3 講義 (第一版) 第 21 頁
       *>2026/1/30/00:04 
      *> --- 檔案結構定義：畫格子 ---

       FD  ORDER-FILE.*>FD意思是 「檔案描述符」Week 3 講義(二版) 第 21 頁
       *>ORDER-FILE我取的名字把「檔案設定」跟「檔案結構」連起來
       01  ORDER-RECORD.*>當程式執行 READ ORDER-FILE 時，讀進來的那一行資料
      *>，就會暫時存放在 ORDER-RECORD 這個記憶體空間裡 Week 1 講義(二版) 第 19-20 頁
      *> [注意] 这里的長度 (PIC) 必須跟 Python 輸出的 Byte 數完全精準對齊！
           05  R-NAME      PIC X(20).
      *> [欄位] 姓名：預留 20 個文字空間。
           05  R-PHONE     PIC X(15).
      *> [欄位] 電話：預留 15 個文字空間。
           05  R-ITEM      PIC X(10).
      *> [欄位] 品項：預留 10 個文字空間。
           05  R-UNIT      PIC 9(3).
      *> [欄位] 單價：3 位數的純數字 (例如 050)。
           05  R-QTY       PIC 9(3).
      *> [欄位] 數量：3 位數的純數字 (例如 002)。
           05  R-PRICE     PIC 9(5).
      *> [欄位] 總價：5 位數的純數字 (例如 00100)。

       FD  REPORT-FILE.*>REPORT-FILE代號對應到真實的 report.txt
       01  REPORT-LINE     PIC X(50).*>用來定義報表中每一行要印什麼
      *> [設定] 報表檔每一行最多 50 個字。

       WORKING-STORAGE SECTION.*>程式自己運算用的 
       *>計算總金額的 TOTAL-PRICE、迴圈用的計數器 I、暫存計算結果的變數，通通都要定義在這裡
       *>它們只存在於程式執行期間，不會直接寫進檔案 Week 1 講義(二版) 第 14 頁
      *> --- 辦公桌：記憶體暫存區 ---

       01  WS-EOF          PIC A(1) VALUE 'N'.
      *> [開關] 用來判斷檔案讀完了沒 (N=還沒, Y=讀完了)。

      *> --- 統計用的變數 (計算機) ---
       01  TOTAL-REVENUE   PIC 9(8) VALUE 0.
      *> [變數] 總營收：最多存 8 位數，預設是 0。
       01  CALC-TOTAL      PIC 9(8) VALUE 0.
      *> [變數] 驗算暫存：用來算「單價 x 數量」是否正確。
       01  ERROR-COUNT     PIC 9(4) VALUE 0.
      *> [變數] 錯誤計數器：抓到幾筆帳務不符。

      *> --- 陣列 (Table)：用來統計排行榜 ---
       01  POPULAR-STATS.
           05  ITEM-ENTRY OCCURS 20 TIMES INDEXED BY I.
      *> [陣列] 準備 20 個格子來存飲料。
      *> [索引] 我們用代號 'I' 來代表第幾格。
               10 T-NAME   PIC X(10) VALUE SPACES.
      *> [陣列內容] 飲料名稱。
               10 T-QTY    PIC 9(4) VALUE 0.
      *> [陣列內容] 累積銷量。

      *> --- 排行榜計算用 ---
       01  MAX-QTY         PIC 9(4) VALUE 0.
      *> [變數] 目前找到的最大數量。
       01  MAX-NAME        PIC X(10) VALUE SPACES.
      *> [變數] 目前的人氣王名稱。

       PROCEDURE DIVISION.*>過程部
       *>2026/1/30/20:36
      *> --- 程式邏輯區：開始工作 ---

       MAIN-PROCEDURE.
       *>程式的 「主控制台」
      *> === 主流程指揮官 ===
           OPEN INPUT ORDER-FILE.
      *> [動作] 打開訂單檔 (讀取)。
           OPEN OUTPUT REPORT-FILE.
      *> [動作] 打開報表檔 (寫入)。

      *> [迴圈] 一直讀，直到檔案結束 (WS-EOF = 'Y')
           PERFORM UNTIL WS-EOF = 'Y'
               READ ORDER-FILE
                   AT END 
                       MOVE 'Y' TO WS-EOF
      *> [判斷] 讀到底了，切換開關，準備下班。
                   NOT AT END
                       PERFORM PROCESS-RECORD
      *> [判斷] 還沒讀完，去執行「處理單筆訂單」的任務。
               END-READ
           END-PERFORM.

      *> [結算] 讀完所有資料後，開始做總結
           PERFORM FIND-POPULAR-DRINK.
           *>用迴圈從第 1 格檢查到第 20 格，比對哪一種飲料賣出的杯數(T-QTY)最多
           PERFORM WRITE-REPORT.
           *>將文字一行一行寫入到硬碟上的 report.txt 檔案中，這樣 Python (app.py) 才讀得到結果

           CLOSE ORDER-FILE.*>剛剛讀取的 Python 訂單資料
           CLOSE REPORT-FILE.*>剛剛寫好的成績單
      *> [動作] 關閉檔案 (存檔)。
           STOP RUN.

       PROCESS-RECORD.
      *> === 任務：處理每一筆訂單 ===

      *> [任務A] 累加今日營收
           ADD R-PRICE TO TOTAL-REVENUE.
           *>R-PRICE (Python 算好的總價)
           *>TOTAL-REVENUE (今日總營收)

      *> [任務B] COBOL 稽核驗算 
           COMPUTE CALC-TOTAL = R-UNIT * R-QTY.
           IF CALC-TOTAL NOT = R-PRICE THEN
               ADD 1 TO ERROR-COUNT
               DISPLAY "⚠️ 發現帳務異常: " R-NAME
           *>CALC-TOTAL (COBOL 驗算的總價)
           *>R-UNIT (單價)
           *>R-QTY (數量)
           *>R-PRICE (Python 算好的總價)
           *>ERROR-COUNT (異常-計數器)    
      *> [警告] 如果算出來不一樣，計一次過，並在螢幕警告！
           END-IF.
           *>2026/1/30/21:05

      *> [任務C] 統計銷量 (陣列查找)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
               *> 情況 1: 找到同名的飲料 (舊面孔)
               IF T-NAME(I) = R-ITEM THEN
                   ADD R-QTY TO T-QTY(I)
           *>T-NAME(I) (統計表上的品名)
           *>R-ITEM (訂單品項)
           *>R-QTY (訂單數量)
           *>T-QTY(I) (統計表上的總杯數)        
      *> [動作] 加數量。
                   EXIT PERFORM
      *> [動作] 做完了，跳出迴圈。
               END-IF
               
               *> 情況 2: 遇到空格 (新飲料)
               IF T-NAME(I) = SPACES THEN
                   MOVE R-ITEM TO T-NAME(I)
      *> [動作] 登記名字。
                   MOVE R-QTY TO T-QTY(I)
      *> [動作] 登記數量。
                   EXIT PERFORM
               END-IF
           END-PERFORM.

      *> [任務D] 在螢幕上印出處理進度
           DISPLAY "訂單處理: " FUNCTION TRIM(R-ITEM) " $" R-PRICE.
       *>FUNCTION呼叫 「內建函數」後面接的 TRIM 是一個功能
       *>TRIM(R-ITEM)(去除空白)
       *>" $"為了美觀，純文字字串
       *>R-PRICE從 orders.txt 讀進來的 「訂單金額」
       FIND-POPULAR-DRINK.
      *> === 任務：找出人氣王 ===
      *> [邏輯] 從第1格掃描到第20格，誰大就選誰
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
               IF T-QTY(I) > MAX-QTY THEN
                   MOVE T-QTY(I) TO MAX-QTY
                   *>把新的最高票數記錄下來
                   MOVE T-NAME(I) TO MAX-NAME
                   *>把新的冠軍飲料名字記下來
               END-IF
           END-PERFORM.

       WRITE-REPORT.
      *> === 任務：寫入期末報告給 Python ===
           MOVE SPACES TO REPORT-LINE.
           *>清空 REPORT-LINE
           *>STRING把後面列出的好幾樣東西，「無縫」 地黏在一起
           *>INTO(放入/存入)
           *>WRITE把內容，真正「寫」進硬碟的檔案（report.txt）裡，並且自動換行 
           *>Week 3 講義 (第一版) 第 28 頁
           *>FUNCTION呼叫「內建工具」
      *> [寫入] 總營收 (REVENUE=...)
           STRING "REVENUE=" TOTAL-REVENUE INTO REPORT-LINE.
           WRITE REPORT-LINE.
           
      *> [寫入] 人氣王 (POPULAR=...)
           MOVE SPACES TO REPORT-LINE.
           STRING "POPULAR=" FUNCTION TRIM(MAX-NAME) INTO REPORT-LINE.
           WRITE REPORT-LINE.
           
      *> [寫入] 稽核狀態 (AUDIT=...)
           MOVE SPACES TO REPORT-LINE.
           IF ERROR-COUNT = 0 THEN
               STRING "AUDIT=PASS_OK" INTO REPORT-LINE
      *> [狀態] 沒錯誤 -> PASS_OK (綠燈)
           ELSE
               STRING "AUDIT=FAIL_ERROR" INTO REPORT-LINE
      *> [狀態] 有錯誤 -> FAIL_ERROR (紅燈)
           END-IF.
           WRITE REPORT-LINE.
           
           DISPLAY "==============================".
           DISPLAY " COBOL 結算完成，報告已生成。".
           DISPLAY "==============================".
           