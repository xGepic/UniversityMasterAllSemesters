DROP TABLE IF EXISTS EMPLOYEES;

CREATE TABLE EMPLOYEES (
    ID SERIAL PRIMARY KEY,
    NAME VARCHAR(100) NOT NULL,
    POSITION VARCHAR(100) NOT NULL,
    SALARY NUMERIC(10,2) NOT NULL
);

INSERT INTO EMPLOYEES (NAME, POSITION, SALARY) VALUES
('Alice Johnson', 'Manager', 75000.00),
('Bob Smith', 'Developer', 65000.00),
('Carol White', 'Designer', 60000.00);
