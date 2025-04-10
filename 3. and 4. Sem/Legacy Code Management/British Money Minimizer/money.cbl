       IDENTIFICATION DIVISION.
       PROGRAM-ID. BritishMoneyMinimizer.
       AUTHOR. Stefan Simanek.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CURRENT-PURSE.
           05 CURRENT-CURRENCY OCCURS 8 TIMES.
               10 CURRENT-CURRENCY-AMOUNT PIC 9(3) VALUE 0.
       01 PAYMENT-PURSE.
           05 PAYMENT-CURRENCY OCCURS 8 TIMES.
               10 PAYMENT-CURRENCY-AMOUNT PIC 9(4) VALUE 0.
       01 CUR.
           05 CUR-CURRENCY OCCURS 8 TIMES.
               10 CURRENCY-VALUE PIC 9(4) VALUE 0.

       01 CURRENT-AMOUNT PIC 9(6) VALUE 0.
       01 PAYMENT-AMOUNT PIC 9(6) VALUE 0.
       01 TOTAL-AMOUNT-PAID PIC 9(6) VALUE 0.
       01 CHANGE-AMOUNT PIC 9(6) VALUE 0.
       01 ITEM-COUNTER PIC 9 VALUE 1.
       01 CURRENCY-COUNTER PIC 9 VALUE 1.

       01 ITEMS.
           05 ITEM OCCURS 5 TIMES.
               10 ITEM-POUNDS PIC 9(3) VALUE 0.
               10 ITEM-SHILLINGS PIC 9(3) VALUE 0.
               10 ITEM-PENCE PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       Main.
           
           PERFORM ACCEPT-PROMPTS.
           PERFORM INPUT-PROMPTS
           PERFORM SET-CURRENCY-VALUES.

           PERFORM CALCULATE-PAYMENT-AMOUNT.
           DISPLAY "Total amount needed: " PAYMENT-AMOUNT " pence.".

           PERFORM CALCULATE-CURRENT-AMOUNT.
           DISPLAY "Total amount in purse: " CURRENT-AMOUNT " pence.".

           IF CURRENT-AMOUNT < PAYMENT-AMOUNT
               DISPLAY "Insufficient funds to complete the purchase."
               STOP RUN
           ELSE
               DISPLAY "Sufficient funds available."
           END-IF.

           PERFORM CALCULATE-PAYMENT-CURRENCY.
           DISPLAY "Total amount paid: " TOTAL-AMOUNT-PAID " pence.".

           PERFORM CALCULATE-CHANGE.
           DISPLAY "Change due: " CHANGE-AMOUNT " pence.".

           PERFORM SHOW-USED-CURRENCY-PLUS-CHANGE.

           STOP RUN.

       ACCEPT-PROMPTS.
               PERFORM VARYING CURRENCY-COUNTER FROM 1 BY 1 UNTIL CURRENCY-COUNTER > 8
                   ACCEPT CURRENT-CURRENCY-AMOUNT(CURRENCY-COUNTER)
               END-PERFORM.

            SET CURRENCY-COUNTER TO 1.

            PERFORM VARYING ITEM-COUNTER FROM 1 BY 1 UNTIL ITEM-COUNTER > 5
               ACCEPT ITEM-POUNDS(ITEM-COUNTER)
               ACCEPT ITEM-SHILLINGS(ITEM-COUNTER)
               ACCEPT ITEM-PENCE(ITEM-COUNTER)
            END-PERFORM.

            SET CURRENCY-COUNTER TO 1.

       INPUT-PROMPTS.
            DISPLAY "Enter the number of each type of coin or bill you have:".
            DISPLAY "Count of 5 pound notes       :".
            DISPLAY "Count of 1 pound notes       :".
            DISPLAY "Count of 1 pound coins       :".
            DISPLAY "Count of 2 shilling coins    :".
            DISPLAY "Count of 1 shilling coins    :".
            DISPLAY "Count of sixpence coins      :".
            DISPLAY "Count of threepence coins    :".
            DISPLAY "Count of penny coins         :".
            DISPLAY "Enter item prices (5 items) in pounds, shillings, pence:".
            DISPLAY "Item 01: Pounds:".
            DISPLAY "Shillings:".
            DISPLAY "Pence:".
            DISPLAY "Item 02: Pounds:".
            DISPLAY "Shillings:".
            DISPLAY "Pence:".
            DISPLAY "Item 03: Pounds:".
            DISPLAY "Shillings:".
            DISPLAY "Pence:".
            DISPLAY "Item 04: Pounds:".
            DISPLAY "Shillings:".
            DISPLAY "Pence:".
            DISPLAY "Item 05: Pounds:".
            DISPLAY "Shillings:".
            DISPLAY "Pence:".

       CALCULATE-PAYMENT-AMOUNT.
            PERFORM VARYING ITEM-COUNTER FROM 1 BY 1 UNTIL ITEM-COUNTER > 5
               COMPUTE PAYMENT-AMOUNT = PAYMENT-AMOUNT 
                   + (ITEM-POUNDS(ITEM-COUNTER) * 240) 
                   + (ITEM-SHILLINGS(ITEM-COUNTER) * 12) 
                   + ITEM-PENCE(ITEM-COUNTER)
            END-PERFORM.

       SET-CURRENCY-VALUES.
           MOVE 1200 TO CURRENCY-VALUE(1).  
           MOVE 240 TO CURRENCY-VALUE(2).   
           MOVE 240 TO CURRENCY-VALUE(3).   
           MOVE 24 TO CURRENCY-VALUE(4).   
           MOVE 12 TO CURRENCY-VALUE(5).    
           MOVE 6 TO CURRENCY-VALUE(6).     
           MOVE 3 TO CURRENCY-VALUE(7).     
           MOVE 1 TO CURRENCY-VALUE(8).     

       CALCULATE-CURRENT-AMOUNT.
           MOVE 0 TO CURRENT-AMOUNT.
           PERFORM VARYING CURRENCY-COUNTER FROM 1 BY 1 UNTIL CURRENCY-COUNTER > 8
               COMPUTE CURRENT-AMOUNT = CURRENT-AMOUNT 
                   + (CURRENT-CURRENCY-AMOUNT(CURRENCY-COUNTER) * CURRENCY-VALUE(CURRENCY-COUNTER))
           END-PERFORM.

            SET CURRENCY-COUNTER TO 1.

       CALCULATE-PAYMENT-CURRENCY.
           MOVE 0 TO TOTAL-AMOUNT-PAID.
           PERFORM VARYING CURRENCY-COUNTER FROM 1 BY 1 UNTIL CURRENCY-COUNTER > 8
               PERFORM UNTIL PAYMENT-AMOUNT < CURRENCY-VALUE(CURRENCY-COUNTER) 
                   OR CURRENT-CURRENCY-AMOUNT(CURRENCY-COUNTER) = 0
                   COMPUTE CURRENT-CURRENCY-AMOUNT(CURRENCY-COUNTER) = 
                       CURRENT-CURRENCY-AMOUNT(CURRENCY-COUNTER) - 1
                   COMPUTE PAYMENT-CURRENCY-AMOUNT(CURRENCY-COUNTER) = 
                       PAYMENT-CURRENCY-AMOUNT(CURRENCY-COUNTER) + 1
                   COMPUTE PAYMENT-AMOUNT = PAYMENT-AMOUNT - CURRENCY-VALUE(CURRENCY-COUNTER)
                   COMPUTE TOTAL-AMOUNT-PAID = TOTAL-AMOUNT-PAID + CURRENCY-VALUE(CURRENCY-COUNTER)
               END-PERFORM
           END-PERFORM.

            SET CURRENCY-COUNTER TO 1.

       CALCULATE-CHANGE.
           IF PAYMENT-AMOUNT = 0
               MOVE 0 TO CHANGE-AMOUNT
           ELSE
               PERFORM VARYING CURRENCY-COUNTER FROM 1 BY 1 UNTIL CURRENCY-COUNTER > 8
                   COMPUTE CHANGE-AMOUNT = CHANGE-AMOUNT 
                       + (CURRENT-CURRENCY-AMOUNT(CURRENCY-COUNTER) * CURRENCY-VALUE(CURRENCY-COUNTER))
               END-PERFORM
           END-IF.
           SET CURRENCY-COUNTER TO 1.

       SHOW-USED-CURRENCY-PLUS-CHANGE.
            DISPLAY "Coins and bills used:".
            IF PAYMENT-CURRENCY-AMOUNT(1) > 0
            DISPLAY PAYMENT-CURRENCY-AMOUNT(1) " x 5 pound notes".
            IF PAYMENT-CURRENCY-AMOUNT(2) > 0
            DISPLAY PAYMENT-CURRENCY-AMOUNT(2) " x 1 pound notes".
            IF PAYMENT-CURRENCY-AMOUNT(3) > 0
            DISPLAY PAYMENT-CURRENCY-AMOUNT(3) " x 1 pound coins".
            IF PAYMENT-CURRENCY-AMOUNT(4) > 0
            DISPLAY PAYMENT-CURRENCY-AMOUNT(4) " x 2 shilling coins".
            IF PAYMENT-CURRENCY-AMOUNT(5) > 0
            DISPLAY PAYMENT-CURRENCY-AMOUNT(5) " x 1 shilling coins".
            IF PAYMENT-CURRENCY-AMOUNT(6) > 0
            DISPLAY PAYMENT-CURRENCY-AMOUNT(6) " x sixpence coins".
            IF PAYMENT-CURRENCY-AMOUNT(7) > 0
            DISPLAY PAYMENT-CURRENCY-AMOUNT(7) " x threepence coins".
            IF PAYMENT-CURRENCY-AMOUNT(8) > 0
            DISPLAY PAYMENT-CURRENCY-AMOUNT(8) " x penny coins".

            DISPLAY "Change coins and notes:".