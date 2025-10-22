<p>This open-source CICS framework brings enterprise mainframe transaction processing concepts to your local development environment. This framework runs on standard Linux/Unix systems and provides authentic CICS learning experiences.</p>

![](https://i.imgflip.com/2be1h8.jpg)

This guide covers everything you need to get started with our CICS simulator framework and begin learning enterprise transaction processing concepts immediately.

### Executive Summary

The **CICS Simulator** is the core innovation that makes this educational framework possible. It translates IBM Customer Information Control System (CICS) concepts into executable Linux/Unix environments, enabling you to learn enterprise transaction processing without mainframe access.

### Key Learning Benefits

- **Real Skills**: Learn actual CICS concepts that transfer directly to IBM environments
- **Hands-on Experience**: Work with realistic banking and transaction scenarios
- **Progressive Learning**: Build on existing programming and system knowledge
- **Industry Relevant**: Gain skills used in enterprise environments worldwide

### Architecture Overview

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Your Terminal │    │   CICS           │    │   Banking       │
│   Session       │◄──►│   Simulator      │◄──►│   Applications  │
│                 │    │  (Framework)     │    │   (COBOL)       │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                                │
                                ▼
                       ┌──────────────────┐
                       │   System Tables  │
                       │   PCT, PPT, FCT  │
                       │   TCT, SIT       │
                       └──────────────────┘
```

### System Requirements

**Operating System:**

- Linux (Ubuntu 18.04+, CentOS 7+, RHEL 7+)
- macOS (10.14+)
- Windows with WSL2 (Windows Subsystem for Linux)

**Software Dependencies:**

- Bash shell (version 4.0+)
- Basic Unix utilities (sed, awk, grep, cat)
- Text editor (vim, nano, or VS Code)

**COBOL Compiler (for real program execution):**

- GnuCOBOL (open-source COBOL compiler)

**Development Tools:**

- Git (for version control)
- VS Code with COBOL extensions

### Quick Installation Guide

```bash
# 1. Download or clone the framework
git clone https://github.com/your-repo/cics-framework.git
cd cics-framework

# 2. Make scripts executable
chmod +x *.sh
```

### Installation Verification

#### Test 1: Basic Framework Test

```bash
# Run the demo to verify everything works
./cics_demo.sh
```

**Expected Output:**

```
pen Source CICS Alternative - Comprehensive Demo
====================================================

Step 1: Initializing CICS Environment
-------------------------------------
CICS Simulator - Initializing Environment
==============================================
Creating CICS System Tables...
System tables created successfully
CICS Region: CICSPROD
Application ID: CICS001
Port: 3270
Max Users: 100
Startup Time: 2025-08-27 12:51:22

CICS Environment initialized successfully

Step 2: Starting CICS Region

.....

Terminal Session Example:
   T001 ==> CESN              # Sign on
   T001 ==> BANK              # Banking app
   T001 ==> ACCT 123456       # Account inquiry
   T001 ==> CEMT INQ SYSTEM   # System status
   T001 ==> CESF              # Sign off

CICS Demo completed successfully!

This demonstrates a fully functional open source alternative to IBM CICS
   with enterprise-grade transaction processing capabilities!

```

#### Test 2: Start CICS System

```bash
# Start the CICS simulator
./cics_simulator.sh
```

#### Test 3: File Structure Verification

```bash
# Check that all required files exist
ls -l cics/tables/
```

**Expected Structure:**

```
cics/tables/
├── fct.dat    # File Control Table
├── pct.dat    # Program Control Table
├── ppt.dat    # Processing Program Table
├── sit.dat    # System Initialization Table
└── tct.dat    # Terminal Control Table

```

#### Test 4: CICS Simulator Test

```bash
# Test direct CICS execution
./cics_simulator.sh init
```

**Expected Output:**

```
CICS Simulator - Initializing Environment
==============================================
Creating CICS System Tables...
System tables created successfully
CICS Region: CICSPROD
Application ID: CICS001
Port: 3270
Max Users: 100
Startup Time: 2025-08-27 12:54:26

CICS Environment initialized successfully
```

```bash
# Test direct CICS execution
./cics_simulator.sh start
```

**Expected Output:**

```
======================================
CICS Region CICSPROD started successfully
Listening on port 3270
Maximum users: 100
Base directory: ./cics

Available commands:
  ./cics_simulator.sh terminal <termid>  - Start terminal session
  ./cics_simulator.sh status             - Show system status
  ./cics_simulator.sh shutdown           - Shutdown CICS
```

```bash
# Test direct CICS execution
./cics_simulator.sh terminal
```

**Expected Output:**

```
Starting CICS Terminal: T001
==================================
Terminal T001 connected to CICS region CICSPROD
Enter CICS transactions (type 'CESF' to sign off)

T001 ==> 
```

#### Framework Components Overview

**`cics_simulator.sh`**: The main CICS processing engine

- Simulates IBM CICS environment
- Executes COBOL programs
- Manages terminals and sessions
- Handles transaction processing and return codes

**`cics/tables/` directory**: Contains CICS system tables

- `pct.dat`: Program Control Table - maps transactions to programs
- `ppt.dat`: Processing Program Table - program definitions
- `fct.dat`: File Control Table - file and dataset definitions
- `tct.dat`: Terminal Control Table - terminal configurations
- `sit.dat`: System Initialization Table - system parameters


### Next Steps

Once you have the framework running:

1. **Complete the demo:** `./cics_demo.sh`
2. **Start your first session:** `./cics_simulator.sh start`
3. **Explore transactions:** Learn the basic CICS commands
4. **Create custom programs:** Add your own COBOL programs

**Congratulations! You now have a complete open-source CICS learning environment. This setup provides authentic enterprise transaction processing experience!**

**Ready to start learning CICS?** Begin with `./cics_demo.sh` and follow this guide for comprehensive learning!
