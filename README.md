# Technical Implementation Guide: Open COBOL ESQL Migration

## 1. System Architecture Overview

### Current State (DB2)
```
COBOL Source → DB2 Precompiler → COBOL with SQL calls → COBOL Compiler → Executable → DB2 Database
```

### Target State (Ocesql + PostgreSQL)
```
COBOL Source → Ocesql Precompiler → COBOL with CALL statements → GnuCOBOL → Executable → PostgreSQL
```

## 2. Installation and Setup

### 2.1 Prerequisites Installation
```bash
# Ubuntu/Debian
sudo apt update
sudo apt install -y \
    postgresql-14 postgresql-client-14 postgresql-contrib-14 \
    libpq-dev postgresql-server-dev-14 \
    gnucobol-dev gnucobol-common \
    build-essential autotools-dev automake libtool \
    flex bison pkg-config

# RHEL/CentOS
sudo yum install -y \
    postgresql-server postgresql-devel \
    gnucobol gnucobol-devel \
    gcc gcc-c++ make autoconf automake libtool \
    flex bison pkgconfig
```

### 2.2 Ocesql Installation
```bash
# Download and build Ocesql
git clone https://github.com/opensourcecobol/Open-COBOL-ESQL.git
cd Open-COBOL-ESQL

# Configure and build
./autogen.sh
./configure --prefix=/usr/local
make
sudo make install

# Set environment variables
export COBCPY=/usr/local/share/ocesql/copy
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PATH=/usr/local/bin:$PATH
```

### 2.3 PostgreSQL Setup
```bash
# Initialize and start PostgreSQL
sudo postgresql-setup initdb
sudo systemctl enable postgresql
sudo systemctl start postgresql

# Create application database and user
sudo -u postgres psql << EOF
CREATE DATABASE appdb;
CREATE USER appuser WITH PASSWORD 'secure_password';
GRANT ALL PRIVILEGES ON DATABASE appdb TO appuser;
\q
EOF
```

## 3. Development Environment Setup

### 3.1 Build Environment Configuration
```bash
# Create project structure
mkdir -p /opt/cobol-project/{src,build,lib,copy}

# Environment script (save as setup_env.sh)
#!/bin/bash
export COBCPY=/usr/local/share/ocesql/copy:/opt/cobol-project/copy
export LD_LIBRARY_PATH=/usr/local/lib:/opt/cobol-project/lib:$LD_LIBRARY_PATH
export PATH=/usr/local/bin:$PATH
export PGHOST=localhost
export PGPORT=5432
export PGDATABASE=appdb
export PGUSER=appuser
```

### 3.2 Build Script Template
```bash
#!/bin/bash
# build_cobol.sh - COBOL compilation script

SOURCE_FILE=$1
OUTPUT_NAME=${2:-$(basename "$SOURCE_FILE" .cbl)}

if [ -z "$SOURCE_FILE" ]; then
    echo "Usage: $0 <source.cbl> [output_name]"
    exit 1
fi

# Source environment
source setup_env.sh

# Precompile with Ocesql
echo "Precompiling $SOURCE_FILE..."
ocesql "$SOURCE_FILE" "${SOURCE_FILE%.cbl}.cob"
if [ $? -ne 0 ]; then
    echo "Precompilation failed"
    exit 1
fi

# Compile with GnuCOBOL
echo "Compiling ${SOURCE_FILE%.cbl}.cob..."
cobc -x -locesql "${SOURCE_FILE%.cbl}.cob" -o "$OUTPUT_NAME"
if [ $? -ne 0 ]; then
    echo "Compilation failed"
    exit 1
fi

echo "Build successful: $OUTPUT_NAME"
```

## 4. Code Migration Guidelines

### 4.1 EXEC SQL Statement Mapping

| DB2 Syntax | Ocesql Equivalent | Notes |
|------------|-------------------|-------|
| `EXEC SQL INCLUDE SQLCA END-EXEC` | `EXEC SQL INCLUDE SQLCA END-EXEC` | ✅ Direct compatibility |
| `EXEC SQL INCLUDE member END-EXEC` | `EXEC SQL INCLUDE member END-EXEC` | ✅ Direct compatibility |
| `EXEC SQL CONNECT TO :db USER :user END-EXEC` | `EXEC SQL CONNECT :user IDENTIFIED BY :pass USING :db END-EXEC` | ⚠️ Syntax difference |
| `EXEC SQL DECLARE cursor CURSOR FOR SELECT ... END-EXEC` | `EXEC SQL DECLARE cursor CURSOR FOR SELECT ... END-EXEC` | ✅ Direct compatibility |

### 4.2 Data Type Mapping

| COBOL Type | DB2 SQL Type | PostgreSQL Type | Notes |
|------------|---------------|-----------------|-------|
| `PIC 9(n)` | INTEGER | INTEGER | ✅ Compatible |
| `PIC S9(n) COMP` | INTEGER | INTEGER | ✅ Compatible |
| `PIC X(n)` | CHAR(n) | CHAR(n) or VARCHAR(n) | ✅ Compatible |
| `PIC S9(n)V9(m)` | DECIMAL(n,m) | DECIMAL(n,m) | ✅ Compatible |
| `PIC 9(8) COMP` | DATE | DATE | ⚠️ Format conversion may be needed |

### 4.3 Common Migration Patterns

#### Connection Handling
```cobol
*> DB2 Style
EXEC SQL CONNECT TO :DBNAME USER :USERNAME END-EXEC.

*> Ocesql Style  
EXEC SQL CONNECT :USERNAME IDENTIFIED BY :PASSWORD USING :DBNAME END-EXEC.
```

#### Error Handling (Compatible)
```cobol
EXEC SQL SELECT COUNT(*) INTO :RECORD-COUNT FROM EMPLOYEES END-EXEC.
EVALUATE SQLCODE
    WHEN ZERO
        DISPLAY "Success: " RECORD-COUNT " records found"
    WHEN 100
        DISPLAY "No records found"
    WHEN OTHER
        DISPLAY "SQL Error: " SQLCODE
        DISPLAY "Error State: " SQLSTATE
        DISPLAY "Error Message: " SQLERRMC
END-EVALUATE.
```

## 5. Database Schema Migration

### 5.1 DDL Conversion Examples

#### DB2 to PostgreSQL Table Creation
```sql
-- DB2 Original
CREATE TABLE EMPLOYEES (
    EMP_ID INTEGER NOT NULL PRIMARY KEY,
    EMP_NAME CHAR(50) NOT NULL,
    SALARY DECIMAL(10,2),
    HIRE_DATE DATE,
    DEPT_ID INTEGER
);

-- PostgreSQL Equivalent
CREATE TABLE EMPLOYEES (
    EMP_ID INTEGER NOT NULL PRIMARY KEY,
    EMP_NAME CHAR(50) NOT NULL,
    SALARY DECIMAL(10,2),
    HIRE_DATE DATE,
    DEPT_ID INTEGER
);
```

### 5.2 Data Migration Script Template
```bash
#!/bin/bash
# migrate_data.sh - Data migration from DB2 to PostgreSQL

# Export from DB2
db2 "EXPORT TO employees.csv OF DEL SELECT * FROM EMPLOYEES"

# Import to PostgreSQL
psql -h localhost -U appuser -d appdb << EOF
\copy EMPLOYEES FROM 'employees.csv' WITH CSV HEADER;
EOF
```

## 6. Testing Strategy

### 6.1 Unit Testing Framework
```cobol
*> test_template.cbl - Unit test template
IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-EMPLOYEE-CRUD.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  TEST-RESULT         PIC X(10).
01  TEST-COUNT          PIC 9(3) VALUE ZERO.
01  PASS-COUNT          PIC 9(3) VALUE ZERO.

EXEC SQL BEGIN DECLARE SECTION END-EXEC.
01  DB-NAME             PIC X(20) VALUE "testdb".
01  USER-NAME           PIC X(20) VALUE "testuser".
01  PASSWORD            PIC X(20) VALUE "testpass".
01  EMP-ID              PIC 9(6).
01  EMP-NAME            PIC X(50).
EXEC SQL END DECLARE SECTION END-EXEC.

EXEC SQL INCLUDE SQLCA END-EXEC.

PROCEDURE DIVISION.
MAIN-TEST.
    PERFORM SETUP-TEST-DB
    PERFORM TEST-INSERT-EMPLOYEE
    PERFORM TEST-SELECT-EMPLOYEE
    PERFORM TEST-UPDATE-EMPLOYEE
    PERFORM TEST-DELETE-EMPLOYEE
    PERFORM CLEANUP-TEST-DB
    PERFORM DISPLAY-TEST-RESULTS
    STOP RUN.

TEST-INSERT-EMPLOYEE.
    ADD 1 TO TEST-COUNT
    MOVE 12345 TO EMP-ID
    MOVE "John Doe" TO EMP-NAME
    
    EXEC SQL
        INSERT INTO EMPLOYEES (EMP_ID, EMP_NAME)
        VALUES (:EMP-ID, :EMP-NAME)
    END-EXEC
    
    IF SQLCODE = ZERO
        ADD 1 TO PASS-COUNT
        DISPLAY "PASS: Insert employee test"
    ELSE
        DISPLAY "FAIL: Insert employee test - SQLCODE: " SQLCODE
    END-IF.
```

### 6.2 Performance Testing
```bash
#!/bin/bash
# performance_test.sh - Performance comparison script

echo "Running performance tests..."

# Test 1: Bulk insert performance
time ./test_bulk_insert 10000

# Test 2: Complex query performance  
time ./test_complex_queries

# Test 3: Cursor performance
time ./test_cursor_operations

echo "Performance test completed"
```

## 7. Deployment Procedures

### 7.1 Production Deployment Checklist
- [ ] PostgreSQL server configured and tuned
- [ ] Ocesql runtime library installed on all application servers
- [ ] Environment variables configured
- [ ] Database schema migrated and validated
- [ ] Application binaries compiled and tested
- [ ] Backup and recovery procedures tested
- [ ] Monitoring and alerting configured

### 7.2 Rollback Procedures
```bash
#!/bin/bash
# rollback.sh - Emergency rollback script

echo "Starting rollback to DB2..."

# Stop new applications
systemctl stop cobol-apps

# Restore DB2 applications
cp /backup/db2-apps/* /opt/applications/

# Start DB2 applications
systemctl start db2-apps

echo "Rollback completed"
```

## 8. Monitoring and Maintenance

### 8.1 Application Monitoring
```bash
# Monitor COBOL application logs
tail -f /var/log/cobol-apps/application.log | grep -E "(SQLCODE|ERROR|FAIL)"

# Monitor PostgreSQL performance
psql -c "SELECT * FROM pg_stat_activity WHERE state = 'active';"
```

### 8.2 Database Maintenance
```sql
-- Regular maintenance queries
SELECT schemaname, tablename, n_tup_ins, n_tup_upd, n_tup_del 
FROM pg_stat_user_tables 
ORDER BY n_tup_ins DESC;

-- Index usage analysis
SELECT schemaname, tablename, indexname, idx_scan, idx_tup_read, idx_tup_fetch
FROM pg_stat_user_indexes 
ORDER BY idx_scan DESC;
```

## 9. Troubleshooting Guide

### 9.1 Common Issues and Solutions

| Issue | Symptoms | Solution |
|-------|----------|----------|
| Precompilation fails | "syntax error" during ocesql | Check COBOL syntax, verify EXEC SQL statements |
| Runtime connection error | SQLCODE -01 | Verify PostgreSQL connection parameters |
| Performance degradation | Slow query execution | Analyze and optimize PostgreSQL queries |
| Memory issues | Application crashes | Check host variable declarations and sizes |

### 9.2 Debug Procedures
```bash
# Enable detailed logging
export OCESQL_DEBUG=1

# Run with verbose output
./your_program 2>&1 | tee debug.log

# Analyze PostgreSQL logs
tail -f /var/log/postgresql/postgresql-14-main.log
```

## 10. Support and Resources

### 10.1 Documentation Links
- **Ocesql Official**: https://github.com/opensourcecobol/Open-COBOL-ESQL
- **PostgreSQL Documentation**: https://www.postgresql.org/docs/
- **GnuCOBOL Guide**: https://gnucobol.sourceforge.io/

### 10.2 Community Support
- **Ocesql Issues**: GitHub Issues tracker
- **PostgreSQL Community**: postgresql.org forums
- **COBOL Community**: Various COBOL forums and mailing lists

### 10.3 Commercial Support Options
- **PostgreSQL Enterprise**: EDB, 2ndQuadrant, Crunchy Data
- **COBOL Support**: Micro Focus, IBM (for migration consulting)
- **System Integration**: Various consulting firms specializing in legacy modernization