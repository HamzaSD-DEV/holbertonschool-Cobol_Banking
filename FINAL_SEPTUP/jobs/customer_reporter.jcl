//CUSTRPT  JOB CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//* Task 3: Customer Reporter
//* Student: [YOUR NAME]
//* 
//* This job generates customer activity reports:
//* - Reads customer master file
//* - Generates formatted customer reports
//* - Produces summary statistics
//* - Creates management reports
//*
//STEP1    EXEC PGM=REPORTER
//CUSTMAST DD   DSN=CUSTOMERS.MASTER,DISP=SHR
//SYSOUT   DD   SYSOUT=*