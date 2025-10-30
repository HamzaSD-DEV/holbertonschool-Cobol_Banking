exec_bash_ignore_errors("cd cobol_transaction_validation && make test > task-output") and
file_contains_regex("cobol_transaction_validation/task-output",[
"ID: 1 \| Account: 3003 \| Type: WITHDRAW \| Amount: 3333.33",
"ID: 2 \| Account: 4004 \| Type: WITHDRAW \| Amount: 3333.33"
])