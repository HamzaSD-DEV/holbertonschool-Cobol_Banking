# 🚀 Quick Start Guide

## Prerequisites
- Linux/Unix environment (Ubuntu, CentOS, macOS, WSL)
- Bash shell (version 4.0+)
- Basic command-line knowledge

## Installation

### 1. Clone the Repository
```bash
git clone <your-repo-url>
cd jcl-framework-student
```

### 2. Make Scripts Executable
```bash
chmod +x *.sh
```

### 3. Initialize the Framework
```bash
# Initialize all components
./demo.sh

# Setup datasets
./setup_datasets.sh 

# Verify setup
./validate_environment.sh
```

## Your First JCL Job

### Step 1: Examine the Example
```bash
# Look at the sample JCL job
cat jobs/hello_world.jcl

# Look at the COBOL program
cat programs/hello_world.cbl
```

### Step 2: Submit the Job
```bash
# Execute your first job
./jcl_parser.sh jobs/hello_world.jcl

```

### Step 3: Understand the Output
```bash

# IMPORTANT: View the detailed execution log
cat ./output/sysout/HELLO_STEP1_cobol.log
```

**What you'll see in the execution log:**
```
Hello World from JCL Framework!
================================
This is a simple COBOL program demonstration
Program executed successfully
```

**This shows:**
- ✅ Real COBOL compilation (using GnuCOBOL compiler)
- ✅ Actual program execution (not simulation)
- ✅ Enterprise logging patterns (SYSOUT capture)
- ✅ Step-by-step job tracking

## Understanding the Framework

### JCL Syntax Basics
```jcl
//JOBNAME  JOB CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEPNAME EXEC PGM=PROGRAM-NAME
//DDNAME   DD   DSN=DATASET.NAME,DISP=SHR
//SYSOUT   DD   SYSOUT=*
```

### Key Commands
```bash

# Dataset Management
./dataset_manager.sh list          # List all datasets

# Framework Operations
./demo.sh                          # Run demonstration
./jcl_parser.sh <jcl-file>        # Parse JCL directly
```


## Next Steps

### Getting Help

- Each error message includes helpful guidance
- Review exercise instructions carefully
- Run `./demo.sh` to see working examples

## File Locations

| Component | Location | Purpose |
|-----------|----------|---------|
| JCL Jobs | `jobs/` | Job control language files |
| COBOL Programs | `programs/` | COBOL source code |
| Datasets | `datasets/` | Data files and catalogs |
---

**You're ready to go!** Start with `./demo.sh` and then dive into the exercises. Happy learning! 🎓