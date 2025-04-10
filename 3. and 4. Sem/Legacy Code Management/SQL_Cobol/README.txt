# COBOL SQL CRUD - README

## Overview
This project demonstrates how to use COBOL to interact with a MySQL/MariaDB database via embedded SQL using GnuCOBOL.

## Requirements
- MySQL or MariaDB server
- GnuCOBOL (OpenCOBOL)
- `libmysqlclient-dev` or `mariadb-dev` for SQL preprocessor

## Database Setup

1. Start MySQL/MariaDB.
2. Run the setup script:

   mysql -u root -p < setup_employees.sql

3. Confirm `company_db` and `EMPLOYEES` table are created.

## Compiling the COBOL Program

```bash
cobc -x -free -I/usr/include/mysql -L/usr/lib/mysql employee_crud.cbl -lmysqlclient
