exec_bash_contains("cd cobol_batch_processing && ./jcl_parser.sh jobs/batch_validator.jcl && cat output/sysout/BATCHVAL_STEP1.log",[
"VALID: TXN001,DEPOSIT   ,12345,1000.00,20240101",
"INVALID: TXN002,OTHER     ,12346,500.00,20240101",
"INVALID: TXN003,TRANSFERI ,12345,200.00,20240101",
"INVALID: TXN004,DEPOSITO  ,12347,750.00,20240101",
"INVALID: TXN005,INVALID   ,99999,999.99,20240101",
"BATCH-VALIDATOR: Total transactions: 00005",
"BATCH-VALIDATOR: Valid transactions: 00001",
"BATCH-VALIDATOR: Invalid transactions: 00004"
])
    