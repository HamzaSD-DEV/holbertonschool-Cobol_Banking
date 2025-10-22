#!/bin/bash
#########################################################################
# CICS Simulator - Open Source Customer Information Control System
# Simulates IBM CICS transaction processing environment
# Based on the JCL Framework architecture
#########################################################################

# Global CICS environment variables
CICS_REGION="CICSPROD"
CICS_APPLID="CICS001"
CICS_PORT="${CICS_PORT:-3270}"
CICS_MAXUSERS="${CICS_MAXUSERS:-100}"
CICS_STARTUP_TIME=""

# Directory structure
CICS_BASE_DIR="${CICS_BASE_DIR:-./cics}"
CICS_PROGRAMS_DIR="$CICS_BASE_DIR/programs"
CICS_MAPS_DIR="$CICS_BASE_DIR/maps"
CICS_TABLES_DIR="$CICS_BASE_DIR/tables"
CICS_LOGS_DIR="$CICS_BASE_DIR/logs"
CICS_TEMP_DIR="$CICS_BASE_DIR/temp"
CICS_DATASETS_DIR="$CICS_BASE_DIR/datasets"

# CICS Tables
PCT_TABLE="$CICS_TABLES_DIR/pct.dat"      # Program Control Table
PPT_TABLE="$CICS_TABLES_DIR/ppt.dat"      # Processing Program Table
FCT_TABLE="$CICS_TABLES_DIR/fct.dat"      # File Control Table
TCT_TABLE="$CICS_TABLES_DIR/tct.dat"      # Terminal Control Table
SIT_TABLE="$CICS_TABLES_DIR/sit.dat"      # System Initialization Table

# Session management
ACTIVE_SESSIONS_DIR="$CICS_TEMP_DIR/sessions"
TRANSACTION_LOG="$CICS_LOGS_DIR/transaction.log"
SYSTEM_LOG="$CICS_LOGS_DIR/system.log"

# Initialize CICS environment
init_cics() {
    echo "CICS Simulator - Initializing Environment"
    echo "=============================================="
    
    # Create directory structure
    mkdir -p "$CICS_PROGRAMS_DIR" "$CICS_MAPS_DIR" "$CICS_TABLES_DIR" \
             "$CICS_LOGS_DIR" "$CICS_TEMP_DIR" "$CICS_DATASETS_DIR" \
             "$ACTIVE_SESSIONS_DIR"
    
    # Initialize system tables
    create_system_tables
    
    # Set startup time
    CICS_STARTUP_TIME=$(date '+%Y-%m-%d %H:%M:%S')
    
    echo "CICS Region: $CICS_REGION"
    echo "Application ID: $CICS_APPLID"
    echo "Port: $CICS_PORT"
    echo "Max Users: $CICS_MAXUSERS"
    echo "Startup Time: $CICS_STARTUP_TIME"
    echo ""
    echo "CICS Environment initialized successfully"
    
    # Log system startup
    log_system_event "CICS" "STARTUP" "CICS region $CICS_REGION started"
}

# Create CICS system tables
create_system_tables() {
    echo "Creating CICS System Tables..."
    
    # Program Control Table (PCT) - Maps transaction IDs to programs
    cat > "$PCT_TABLE" << 'EOF'
# Program Control Table (PCT)
# Format: TRANID|PROGRAM|PRIORITY|SECURITY|STATUS
CEMT|CEMT|1|PUBLIC|ENABLED
CESN|CESN|1|PUBLIC|ENABLED
CESF|CESF|1|PUBLIC|ENABLED
CEDA|CEDA|1|ADMIN|ENABLED
CEDF|CEDF|1|PUBLIC|ENABLED
BANK|BANKPROG|2|PUBLIC|ENABLED
ACCT|ACCTPROG|2|PUBLIC|ENABLED
CUST|CUSTPROG|2|PUBLIC|ENABLED
INQR|INQUIRY|2|PUBLIC|ENABLED
UPDT|UPDATE|2|PUBLIC|ENABLED
MENU|MAINMENU|1|PUBLIC|ENABLED
EOF

    # Processing Program Table (PPT) - Program definitions
    cat > "$PPT_TABLE" << 'EOF'
# Processing Program Table (PPT)
# Format: PROGRAM|LANGUAGE|STATUS|RESIDENT|USAGE
CEMT|COBOL|ENABLED|YES|SYSTEM
CESN|COBOL|ENABLED|YES|SYSTEM
CESF|COBOL|ENABLED|YES|SYSTEM
CEDA|COBOL|ENABLED|YES|SYSTEM
CEDF|COBOL|ENABLED|YES|SYSTEM
BANKPROG|COBOL|ENABLED|NO|APPLICATION
ACCTPROG|COBOL|ENABLED|NO|APPLICATION
CUSTPROG|COBOL|ENABLED|NO|APPLICATION
INQUIRY|COBOL|ENABLED|NO|APPLICATION
UPDATE|COBOL|ENABLED|NO|APPLICATION
MAINMENU|COBOL|ENABLED|YES|APPLICATION
EOF

    # File Control Table (FCT) - File/Dataset definitions
    cat > "$FCT_TABLE" << 'EOF'
# File Control Table (FCT)
# Format: DDNAME|DATASET|ACCESS|STATUS|RECORDSIZE
CUSTFILE|CUSTOMERS.MASTER|READ|OPEN|80
ACCTFILE|ACCOUNTS.MASTER|UPDATE|OPEN|120
TRANFILE|TRANSACTIONS.LOG|WRITE|OPEN|200
AUDITLOG|AUDIT.LOG|WRITE|OPEN|150
ERRORLOG|ERROR.LOG|WRITE|OPEN|100
EOF

    # Terminal Control Table (TCT) - Terminal definitions
    cat > "$TCT_TABLE" << 'EOF'
# Terminal Control Table (TCT)
# Format: TERMID|TYPE|STATUS|USERID|PRIORITY
T001|3270|ACTIVE||1
T002|3270|ACTIVE||1
T003|3270|ACTIVE||1
T004|3270|ACTIVE||1
T005|3270|ACTIVE||1
CONSOLE|CONSOLE|ACTIVE|ADMIN|0
EOF

    # System Initialization Table (SIT) - System parameters
    cat > "$SIT_TABLE" << 'EOF'
# System Initialization Table (SIT)
# Format: PARAMETER=VALUE
APPLID=$CICS_APPLID
MAXUSERS=$CICS_MAXUSERS
REGION=$CICS_REGION
STARTUP=AUTO
SECURITY=INTERNAL
LOGGING=YES
DEBUGGING=YES
MONITORING=YES
EOF

    echo "System tables created successfully"
}

# Start CICS region
start_cics() {
    echo "Starting CICS Region: $CICS_REGION"
    echo "======================================"
    
    # Check if already running
    if [[ -f "$CICS_TEMP_DIR/cics.pid" ]]; then
        local pid=$(cat "$CICS_TEMP_DIR/cics.pid")
        if kill -0 "$pid" 2>/dev/null; then
            echo "CICS region $CICS_REGION is already running (PID: $pid)"
            return 1
        else
            rm -f "$CICS_TEMP_DIR/cics.pid"
        fi
    fi
    
    # Initialize if not done
    if [[ ! -d "$CICS_BASE_DIR" ]]; then
        init_cics
    fi
    
    # Store process ID
    echo $$ > "$CICS_TEMP_DIR/cics.pid"
    
    # Log startup
    log_system_event "CICS" "STARTUP" "Region $CICS_REGION starting on port $CICS_PORT"
    
    echo "CICS Region $CICS_REGION started successfully"
    echo "Listening on port $CICS_PORT"
    echo "Maximum users: $CICS_MAXUSERS"
    echo "Base directory: $CICS_BASE_DIR"
    echo ""
    echo "Available commands:"
    echo "  ./cics_simulator.sh terminal <termid>  - Start terminal session"
    echo "  ./cics_simulator.sh status             - Show system status"
    echo "  ./cics_simulator.sh shutdown           - Shutdown CICS"
    echo ""
}

# Shutdown CICS region
shutdown_cics() {
    echo "Shutting down CICS Region: $CICS_REGION"
    echo "=========================================="
    
    # Check if running
    if [[ ! -f "$CICS_TEMP_DIR/cics.pid" ]]; then
        echo "CICS region $CICS_REGION is not running"
        return 1
    fi
    
    # Terminate active sessions
    if [[ -d "$ACTIVE_SESSIONS_DIR" ]]; then
        local session_count=$(ls -1 "$ACTIVE_SESSIONS_DIR"/*.session 2>/dev/null | wc -l)
        if [[ $session_count -gt 0 ]]; then
            echo "Terminating $session_count active sessions..."
            rm -f "$ACTIVE_SESSIONS_DIR"/*.session
        fi
    fi
    
    # Log shutdown
    log_system_event "CICS" "SHUTDOWN" "Region $CICS_REGION shutting down"
    
    # Remove PID file
    rm -f "$CICS_TEMP_DIR/cics.pid"
    
    echo "CICS Region $CICS_REGION shutdown complete"
}

# Show CICS status
show_status() {
    echo "CICS Region Status: $CICS_REGION"
    echo "===================================="
    
    if [[ -f "$CICS_TEMP_DIR/cics.pid" ]]; then
        local pid=$(cat "$CICS_TEMP_DIR/cics.pid")
        if kill -0 "$pid" 2>/dev/null; then
            echo "Status: ACTIVE (PID: $pid)"
        else
            echo "Status: INACTIVE (stale PID file)"
            rm -f "$CICS_TEMP_DIR/cics.pid"
        fi
    else
        echo "Status: INACTIVE"
    fi
    
    echo "Region: $CICS_REGION"
    echo "Application ID: $CICS_APPLID"
    echo "Port: $CICS_PORT"
    echo "Startup Time: $CICS_STARTUP_TIME"
    echo ""
    
    # Show active sessions
    local session_count=0
    if [[ -d "$ACTIVE_SESSIONS_DIR" ]]; then
        session_count=$(ls -1 "$ACTIVE_SESSIONS_DIR"/*.session 2>/dev/null | wc -l)
    fi
    echo "Active Sessions: $session_count"
    
    # Show recent transactions
    echo ""
    echo "Recent Transactions (last 10):"
    echo "------------------------------"
    if [[ -f "$TRANSACTION_LOG" ]]; then
        tail -10 "$TRANSACTION_LOG" | while read -r line; do
            echo "  $line"
        done
    else
        echo "  No transactions logged"
    fi
}

# Start terminal session
start_terminal() {
    local termid="${1:-T001}"
    
    echo "Starting CICS Terminal: $termid"
    echo "=================================="
    
    # Check if CICS is running
    if [[ ! -f "$CICS_TEMP_DIR/cics.pid" ]]; then
        echo "CICS region $CICS_REGION is not running"
        echo "   Run: ./cics_simulator.sh start"
        return 1
    fi
    
    # Check if terminal is defined
    if ! grep -q "^$termid|" "$TCT_TABLE"; then
        echo "Terminal $termid is not defined in TCT"
        return 1
    fi
    
    # Create session file
    local session_file="$ACTIVE_SESSIONS_DIR/${termid}.session"
    echo "TERMID=$termid" > "$session_file"
    echo "USERID=" >> "$session_file"
    echo "TRANID=" >> "$session_file"
    echo "STARTTIME=$(date '+%Y-%m-%d %H:%M:%S')" >> "$session_file"
    
    log_system_event "$termid" "SIGNON" "Terminal session started"
    
    # Start interactive session
    cics_terminal_session "$termid"
}

# CICS Terminal Session Handler
cics_terminal_session() {
    local termid="$1"
    local userid=""
    local current_tranid=""
    
    echo "Terminal $termid connected to CICS region $CICS_REGION"
    echo "Enter CICS transactions (type 'CESF' to sign off)"
    echo ""
    
    while true; do
        # Display prompt
        if [[ -n "$userid" ]]; then
            echo -n "$termid($userid) ==> "
        else
            echo -n "$termid ==> "
        fi
        
        # Read transaction input
        read -r input
        
        # Parse input
        local tranid=$(echo "$input" | awk '{print $1}' | tr '[:lower:]' '[:upper:]')
        local params=$(echo "$input" | cut -d' ' -f2- 2>/dev/null)
        
        # Handle special cases
        case "$tranid" in
            "")
                continue
                ;;
            "CESF")
                echo "Signing off terminal $termid"
                log_transaction "$termid" "$userid" "CESF" "SIGNOFF" "0"
                break
                ;;
            "CESN")
                echo "CICS Sign-on"
                echo -n "Enter userid: "
                read -r userid
                echo "User $userid signed on to terminal $termid"
                log_transaction "$termid" "$userid" "CESN" "SIGNON" "0"
                continue
                ;;
            "CLEAR")
                clear
                continue
                ;;
            *)
                # Execute transaction
                execute_transaction "$termid" "$userid" "$tranid" "$params"
                ;;
        esac
    done
    
    # Clean up session
    rm -f "$ACTIVE_SESSIONS_DIR/${termid}.session"
    log_system_event "$termid" "SIGNOFF" "Terminal session ended"
}

# Execute CICS transaction
execute_transaction() {
    local termid="$1"
    local userid="$2"
    local tranid="$3"
    local params="$4"
    
    echo "Executing transaction: $tranid"
    
    # Check if transaction is defined in PCT
    local pct_entry=$(grep "^$tranid|" "$PCT_TABLE" 2>/dev/null)
    if [[ -z "$pct_entry" ]]; then
        echo "ERROR: Transaction $tranid not found in PCT"
        log_transaction "$termid" "$userid" "$tranid" "NOTFOUND" "4"
        return 4
    fi
    
    # Parse PCT entry
    local program=$(echo "$pct_entry" | cut -d'|' -f2)
    local priority=$(echo "$pct_entry" | cut -d'|' -f3)
    local security=$(echo "$pct_entry" | cut -d'|' -f4)
    local status=$(echo "$pct_entry" | cut -d'|' -f5)
    
    # Check if transaction is enabled
    if [[ "$status" != "ENABLED" ]]; then
        echo "ERROR: Transaction $tranid is disabled"
        log_transaction "$termid" "$userid" "$tranid" "DISABLED" "8"
        return 8
    fi
    
    # Execute program
    execute_cics_program "$termid" "$userid" "$tranid" "$program" "$params"
}

# Execute CICS program
execute_cics_program() {
    local termid="$1"
    local userid="$2"
    local tranid="$3"
    local program="$4"
    local params="$5"
    
    echo "Program: $program"
    
    # Check if program exists in PPT
    local ppt_entry=$(grep "^$program|" "$PPT_TABLE" 2>/dev/null)
    if [[ -z "$ppt_entry" ]]; then
        echo "ERROR: Program $program not found in PPT"
        log_transaction "$termid" "$userid" "$tranid" "PGMNOTFOUND" "12"
        return 12
    fi
    
    # Simulate program execution based on transaction type
    case "$tranid" in
        "CEMT")
            execute_cemt "$termid" "$userid" "$params"
            ;;
        "CEDA")
            execute_ceda "$termid" "$userid" "$params"
            ;;
        "CEDF")
            execute_cedf "$termid" "$userid" "$params"
            ;;
        "BANK")
            execute_banking_app "$termid" "$userid" "$params"
            ;;
        "ACCT")
            execute_account_inquiry "$termid" "$userid" "$params"
            ;;
        "CUST")
            execute_customer_inquiry "$termid" "$userid" "$params"
            ;;
        "MENU")
            execute_main_menu "$termid" "$userid" "$params"
            ;;
        *)
            # Generic program execution
            execute_generic_program "$termid" "$userid" "$tranid" "$program" "$params"
            ;;
    esac
    
    local rc=$?
    log_transaction "$termid" "$userid" "$tranid" "COMPLETE" "$rc"
    return $rc
}

# CEMT - Master Terminal transaction
execute_cemt() {
    local termid="$1"
    local userid="$2"
    local params="$3"
    
    echo "CEMT - CICS Master Terminal"
    echo "=============================="
    
    if [[ -z "$params" ]]; then
        echo "CEMT Commands:"
        echo "  INQUIRE SYSTEM    - Show system status"
        echo "  INQUIRE TASK      - Show active tasks"
        echo "  INQUIRE TERMINAL  - Show terminal status"
        echo "  INQUIRE PROGRAM   - Show program status"
        echo "  INQUIRE FILE      - Show file status"
        return 0
    fi
    
    local cmd=$(echo "$params" | awk '{print $1}' | tr '[:lower:]' '[:upper:]')
    local object=$(echo "$params" | awk '{print $2}' | tr '[:lower:]' '[:upper:]')
    
    case "$cmd $object" in
        "INQUIRE SYSTEM")
            echo "System Status:"
            echo "  Region: $CICS_REGION"
            echo "  Applid: $CICS_APPLID"
            echo "  Status: ACTIVE"
            echo "  Startup: $CICS_STARTUP_TIME"
            ;;
        "INQUIRE TASK")
            echo "Active Tasks:"
            local task_count=$(ls -1 "$ACTIVE_SESSIONS_DIR"/*.session 2>/dev/null | wc -l)
            echo "  Total: $task_count"
            ;;
        "INQUIRE TERMINAL")
            echo "Terminal Status:"
            while IFS='|' read -r term_id term_type status user priority; do
                [[ "$term_id" =~ ^# ]] && continue
                echo "  $term_id: $status ($term_type)"
            done < "$TCT_TABLE"
            ;;
        *)
            echo "ERROR: Invalid CEMT command: $params"
            return 4
            ;;
    esac
    
    return 0
}

# CEDA - Resource Definition transaction
execute_ceda() {
    local termid="$1"
    local userid="$2"
    local params="$3"
    
    echo "CEDA - Resource Definition Online"
    echo "===================================="
    echo "CEDA functionality simulated"
    echo "Use this to define CICS resources online"
    return 0
}

# CEDF - Execution Diagnostic Facility
execute_cedf() {
    local termid="$1"
    local userid="$2"
    local params="$3"
    
    echo "CEDF - Execution Diagnostic Facility"
    echo "========================================"
    echo "CEDF debugging mode simulated"
    echo "Use this to debug CICS programs"
    return 0
}

# Banking Application
execute_banking_app() {
    local termid="$1"
    local userid="$2"
    local params="$3"
    
    echo "Banking Application"
    echo "====================="
    echo "Welcome to CICS Banking System"
    echo ""
    echo "Available Functions:"
    echo "  1. Account Inquiry"
    echo "  2. Account Update"
    echo "  3. Transaction History"
    echo "  4. Customer Information"
    echo ""
    echo "Enter selection (1-4): "
    read -r selection
    
    case "$selection" in
        "1")
            echo "Account Inquiry selected"
            echo "Enter account number: "
            read -r account
            echo "Account $account: Balance $12,345.67"
            ;;
        "2")
            echo "Account Update selected"
            echo "Account update functionality"
            ;;
        "3")
            echo "Transaction History selected"
            echo "Recent transactions for account"
            ;;
        "4")
            echo "Customer Information selected"
            echo "Customer details and profile"
            ;;
        *)
            echo "ERROR: Invalid selection"
            return 4
            ;;
    esac
    
    return 0
}

# Account Inquiry
execute_account_inquiry() {
    local termid="$1"
    local userid="$2"
    local params="$3"
    
    echo "Account Inquiry"
    echo "=================="
    
    if [[ -n "$params" ]]; then
        local account="$params"
    else
        echo -n "Enter account number: "
        read -r account
    fi
    
    # Simulate account lookup
    echo "Account Number: $account"
    echo "Customer Name: John Doe"
    echo "Account Type: Checking"
    echo "Current Balance: $15,432.10"
    echo "Available Balance: $15,432.10"
    echo "Last Transaction: $(date '+%Y-%m-%d')"
    
    return 0
}

# Customer Inquiry
execute_customer_inquiry() {
    local termid="$1"
    local userid="$2"
    local params="$3"
    
    echo "Customer Inquiry"
    echo "==================="
    
    if [[ -n "$params" ]]; then
        local customer="$params"
    else
        echo -n "Enter customer ID: "
        read -r customer
    fi
    
    # Simulate customer lookup
    echo "Customer ID: $customer"
    echo "Name: Jane Smith"
    echo "Address: 123 Main St, Anytown, ST 12345"
    echo "Phone: (555) 123-4567"
    echo "Email: jane.smith@email.com"
    echo "Accounts: 3 (Checking, Savings, Credit)"
    
    return 0
}

# Main Menu
execute_main_menu() {
    local termid="$1"
    local userid="$2"
    local params="$3"
    
    echo "CICS Main Menu"
    echo "================="
    echo ""
    echo "Available Applications:"
    echo "  BANK - Banking Application"
    echo "  ACCT - Account Inquiry"
    echo "  CUST - Customer Inquiry"
    echo "  CEMT - Master Terminal"
    echo "  CEDA - Resource Definition"
    echo "  CEDF - Execution Diagnostic"
    echo ""
    echo "System Commands:"
    echo "  CESN - Sign On"
    echo "  CESF - Sign Off"
    echo "  CLEAR - Clear Screen"
    echo ""
    
    return 0
}

# Generic program execution
execute_generic_program() {
    local termid="$1"
    local userid="$2"
    local tranid="$3"
    local program="$4"
    local params="$5"
    
    echo "Executing program: $program"
    echo "Transaction: $tranid"
    echo "Parameters: $params"
    echo "Program completed successfully"
    
    return 0
}

# Log system events
log_system_event() {
    local component="$1"
    local event="$2"
    local message="$3"
    
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "$timestamp [$component] $event: $message" >> "$SYSTEM_LOG"
}

# Log transactions
log_transaction() {
    local termid="$1"
    local userid="$2"
    local tranid="$3"
    local status="$4"
    local rc="$5"
    
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "$timestamp $termid $userid $tranid $status RC=$rc" >> "$TRANSACTION_LOG"
}

# Main function
main() {
    case "${1:-help}" in
        "init")
            init_cics
            ;;
        "start")
            start_cics
            ;;
        "stop"|"shutdown")
            shutdown_cics
            ;;
        "status")
            show_status
            ;;
        "terminal")
            start_terminal "$2"
            ;;
        "help"|*)
            echo "CICS Simulator - Open Source Customer Information Control System"
            echo "=================================================================="
            echo ""
            echo "Usage: $0 <command> [options]"
            echo ""
            echo "Commands:"
            echo "  init                    - Initialize CICS environment"
            echo "  start                   - Start CICS region"
            echo "  stop                    - Stop CICS region"
            echo "  status                  - Show CICS status"
            echo "  terminal [termid]       - Start terminal session (default: T001)"
            echo "  help                    - Show this help"
            echo ""
            echo "Examples:"
            echo "  $0 init                 - Initialize CICS"
            echo "  $0 start                - Start CICS region"
            echo "  $0 terminal T001        - Connect to terminal T001"
            echo "  $0 status               - Check system status"
            echo ""
            ;;
    esac
}

# Execute main function
main "$@"