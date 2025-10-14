-- This is a "bulletproof" bootstrap script.
-- It forcefully drops existing tables to ensure a clean slate.
DROP TABLE IF EXISTS tx_log;
DROP TABLE IF EXISTS accounts;
DROP TABLE IF EXISTS customers CASCADE; -- CASCADE ensures all dependent objects are removed

-- Create the tables with the correct schema
CREATE TABLE customers (
    customer_id INT PRIMARY KEY,
    name VARCHAR(100) NOT NULL
);

CREATE TABLE accounts (
    account_id INT PRIMARY KEY,
    customer_id INT NOT NULL REFERENCES customers(customer_id),
    balance DECIMAL(10, 2) NOT NULL CHECK (balance >= 0)
);

CREATE TABLE tx_log (
    log_id SERIAL PRIMARY KEY,
    account_id INT NOT NULL REFERENCES accounts(account_id),
    tx_type VARCHAR(10) NOT NULL,
    amount DECIMAL(10, 2) NOT NULL,
    tx_timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Insert the initial sample data with the correct column name 'name'
INSERT INTO customers (customer_id, name) VALUES
(101, 'user name1'),
(202, 'user name2'),
(303, 'user name3'),
(404, 'user name4');

INSERT INTO accounts (account_id, customer_id, balance) VALUES
(1001, 101, 1111.11),
(2002, 202, 2222.22),
(3003, 303, 3333.33),
(4004, 404, 4444.44);

-- Grant all necessary permissions to the student user
GRANT ALL ON customers, accounts, tx_log TO postgres;
GRANT USAGE, SELECT ON SEQUENCE tx_log_log_id_seq TO postgres;