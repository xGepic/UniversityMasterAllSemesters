       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEECRUD.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       01 WS-CHOICE           PIC 9.
       01 WS-ID               PIC 9(4).
       01 WS-NAME             PIC X(100).
       01 WS-POSITION         PIC X(100).
       01 WS-SALARY           PIC 9(7)V99.
       01 WS-CONNECTION-STATUS PIC X(10).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DB-USER             PIC X(20) VALUE "root".
       01 DB-PASS             PIC X(20) VALUE "password".
       01 DB-NAME             PIC X(20) VALUE "company_db".
       EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION.
       MAIN-LOGIC.

           PERFORM DB-CONNECT
           PERFORM UNTIL WS-CHOICE = 5
               DISPLAY "====== EMPLOYEE MANAGEMENT ======"
               DISPLAY "1. Insert Employee"
               DISPLAY "2. View Employees"
               DISPLAY "3. Update Salary"
               DISPLAY "4. Delete Employee"
               DISPLAY "5. Exit"
               DISPLAY "Select an option: " WITH NO ADVANCING
               ACCEPT WS-CHOICE

               EVALUATE WS-CHOICE
                   WHEN 1
                       PERFORM INSERT-EMPLOYEE
                   WHEN 2
                       PERFORM VIEW-EMPLOYEES
                   WHEN 3
                       PERFORM UPDATE-SALARY
                   WHEN 4
                       PERFORM DELETE-EMPLOYEE
                   WHEN OTHER
                       DISPLAY "Invalid option!"
               END-EVALUATE
           END-PERFORM

           PERFORM DB-DISCONNECT
           STOP RUN.

       DB-CONNECT.
           EXEC SQL
               CONNECT :DB-USER IDENTIFIED BY :DB-PASS USING :DB-NAME
           END-EXEC
           IF SQLCODE NOT = 0
               DISPLAY "DB CONNECTION FAILED: " SQLERRMC
               STOP RUN
           ELSE
               DISPLAY "Connected to database.".

       DB-DISCONNECT.
           EXEC SQL
               DISCONNECT
           END-EXEC
           DISPLAY "Disconnected from database.".

       INSERT-EMPLOYEE.
           DISPLAY "Enter Name: " WITH NO ADVANCING
           ACCEPT WS-NAME
           DISPLAY "Enter Position: " WITH NO ADVANCING
           ACCEPT WS-POSITION
           DISPLAY "Enter Salary: " WITH NO ADVANCING
           ACCEPT WS-SALARY

           EXEC SQL
               INSERT INTO EMPLOYEES (NAME, POSITION, SALARY)
               VALUES (:WS-NAME, :WS-POSITION, :WS-SALARY)
           END-EXEC

           IF SQLCODE = 0
               DISPLAY "Employee inserted successfully."
           ELSE
               DISPLAY "Error inserting employee: " SQLERRMC.

       VIEW-EMPLOYEES.
           EXEC SQL DECLARE EMP_CURSOR CURSOR FOR
               SELECT ID, NAME, POSITION, SALARY FROM EMPLOYEES
           END-EXEC

           EXEC SQL OPEN EMP_CURSOR END-EXEC

           DISPLAY "ID  | NAME              | POSITION         | SALARY"
           DISPLAY "--------------------------------------------------------"

           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL FETCH EMP_CURSOR INTO :WS-ID, :WS-NAME, :WS-POSITION, :WS-SALARY END-EXEC
               IF SQLCODE = 0
                   DISPLAY WS-ID " | " WS-NAME " | " WS-POSITION " | " WS-SALARY
           END-PERFORM

           EXEC SQL CLOSE EMP_CURSOR END-EXEC.

       UPDATE-SALARY.
           DISPLAY "Enter Employee ID: " WITH NO ADVANCING
           ACCEPT WS-ID
           DISPLAY "Enter New Salary: " WITH NO ADVANCING
           ACCEPT WS-SALARY

           EXEC SQL
               UPDATE EMPLOYEES
               SET SALARY = :WS-SALARY
               WHERE ID = :WS-ID
           END-EXEC

           IF SQLCODE = 0
               DISPLAY "Salary updated."
           ELSE
               DISPLAY "Error updating salary: " SQLERRMC.

       DELETE-EMPLOYEE.
           DISPLAY "Enter Employee ID to Delete: " WITH NO ADVANCING
           ACCEPT WS-ID

           EXEC SQL
               DELETE FROM EMPLOYEES WHERE ID = :WS-ID
           END-EXEC

           IF SQLCODE = 0
               DISPLAY "Employee deleted."
           ELSE
               DISPLAY "Error deleting employee: " SQLERRMC.
