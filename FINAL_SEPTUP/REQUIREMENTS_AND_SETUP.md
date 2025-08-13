# Open Source JCL Framework - Requirements & Setup Guide

![Terminal Setup Screen](https://via.placeholder.com/800x400/2d3748/4fd1d8?text=SETUP+%26+REQUIREMENTS)

<p>This open-source JCL framework brings enterprise mainframe concepts to your local development environment. Unlike expensive IBM mainframe training that can cost thousands of dollars, this framework runs on standard Linux/Unix systems and provides authentic JCL learning experiences at zero cost.</p>

This guide covers everything you need to get started with our JCL parser framework and begin learning enterprise batch processing concepts immediately.

## System Requirements

### Minimum Requirements

**Operating System:**
- Linux (Ubuntu 18.04+, CentOS 7+, RHEL 7+)
- macOS (10.14+)
- Windows with WSL2 (Windows Subsystem for Linux)

**Hardware:**
- 2GB RAM minimum (4GB recommended)
- 1GB free disk space
- Any modern processor (x86_64 or ARM64)

**Software Dependencies:**
- Bash shell (version 4.0+)
- Basic Unix utilities (sed, awk, grep, cat)
- Text editor (vim, nano, or VS Code)

### Optional but Recommended

**COBOL Compiler (for real program execution):**
- GnuCOBOL (open-source COBOL compiler)
- If not available, framework falls back to simulation mode

**Development Tools:**
- Git (for version control)
- VS Code with COBOL extensions
- Terminal multiplexer (tmux or screen)

## Quick Installation Guide

### Option 1: Direct Download and Setup

```bash
# 1. Download or clone the framework
git clone <your-repository-url>
cd jcl-framework

# 2. Make scripts executable
chmod +x *.sh

# 3. Test the installation
./demo.sh
```

### Option 2: Manual Setup

If you don't have git, you can set up manually:

```bash
# 1. Create project directory
mkdir jcl-framework
cd jcl-framework

# 2. Copy all framework files to this directory
# (download files manually from your source)

# 3. Set permissions
chmod +x jcl_parser.sh
chmod +x demo.sh
chmod +x dataset_manager.sh

# 4. Verify structure
ls -la
```

## Installation Verification

### Test 1: Basic Framework Test

```bash
# Run the demo to verify everything works
./demo.sh
```

**Expected Output:**
```
==========================================
JCL Simulation Framework Demo
==========================================

Demo 1: JCL Syntax Example
---------------------------
Here's a sample JCL job (hello_world.jcl):

//HELLO    JOB CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//STEP1    EXEC PGM=HELLO-COBOL
//SYSOUT   DD   SYSOUT=*
```

### Test 2: JCL Parser Test

```bash
# Test direct JCL execution
./jcl_parser.sh jobs/hello_world.jcl
```

**Expected Output:**
```
JCL Simulation Framework
========================
JCL Simulation Environment initialized
Parsing JCL file: jobs/hello_world.jcl
JOB: HELLO (Class: A, MsgClass: X)
STEP: STEP1 (Program: HELLO-COBOL)
Job HELLO completed with return code: 0
```

### Test 3: File Structure Verification

```bash
# Check that all required files exist
ls -la jobs/
ls -la programs/
ls -la datasets/
```

**Expected Structure:**
```
jobs/
├── hello_world.jcl

programs/
├── hello_world.cbl

datasets/
├── .gitkeep
├── master_catalog.dat
└── my_data_set.dat
```

## COBOL Compiler Setup (Optional)

### Installing GnuCOBOL on Ubuntu/Debian

```bash
# Update package list
sudo apt update

# Install GnuCOBOL
sudo apt install gnucobol

# Verify installation
cobc --version
```

### Installing GnuCOBOL on CentOS/RHEL

```bash
# Enable EPEL repository
sudo yum install epel-release

# Install GnuCOBOL
sudo yum install gnucobol

# Verify installation
cobc --version
```

### Installing GnuCOBOL on macOS

```bash
# Using Homebrew
brew install gnucobol

# Verify installation
cobc --version
```

### Testing COBOL Compilation

```bash
# Test COBOL compilation with framework
./jcl_parser.sh jobs/hello_world.jcl

# If compiler works, you'll see:
# "Compilation successful. Executing..."

# If no compiler, you'll see:
# "COBOL compiler not available. Simulating execution..."
```

**Note:** The framework works perfectly in simulation mode if you don't have a COBOL compiler installed!

## Framework Components Overview

### Core Components (Required)

**`jcl_parser.sh`** - The main JCL processing engine
- Parses IBM JCL syntax
- Executes COBOL programs
- Manages datasets and file operations
- Handles error conditions and return codes

**`jobs/` directory** - Contains JCL job files
- `hello_world.jcl` - Basic example job
- Add your own JCL files here

**`programs/` directory** - Contains COBOL programs
- `hello_world.cbl` - Basic COBOL program
- Add your own COBOL programs here

**`datasets/` directory** - Working data files
- Input and output datasets
- Framework creates temporary datasets here

### Optional Components

**`demo.sh`** - Interactive demonstration
- Shows framework capabilities
- Good for initial learning

**`dataset_manager.sh`** - Advanced dataset operations
- VSAM simulation
- GDG (Generation Data Group) support
- Dataset catalog management

**`scheduler.sh`** - Enterprise job scheduling
- Job queue management
- Dependency handling
- **Note:** Not needed for basic JCL learning

## Running Your First JCL Job

### Step 1: Understand the Example

Look at the provided example:

```bash
# View the JCL job
cat jobs/hello_world.jcl

# View the COBOL program
cat programs/hello_world.cbl
```

### Step 2: Execute the Job

```bash
# Run the JCL job
./jcl_parser.sh jobs/hello_world.jcl
```

### Step 3: Check the Results

```bash
# View execution logs
ls -la /tmp/jcl_sim/sysout/

# Read the job log
cat /tmp/jcl_sim/sysout/HELLO.log

# Read the step log
cat /tmp/jcl_sim/sysout/HELLO_STEP1.log
```

## Common Setup Issues and Solutions

### Issue 1: Permission Denied

**Problem:** `./jcl_parser.sh: Permission denied`

**Solution:**
```bash
chmod +x jcl_parser.sh
chmod +x demo.sh
chmod +x *.sh
```

### Issue 2: Command Not Found

**Problem:** `jcl_parser.sh: command not found`

**Solution:**
```bash
# Make sure you're in the correct directory
pwd
ls -la jcl_parser.sh

# Use full path if needed
./jcl_parser.sh jobs/hello_world.jcl
```

### Issue 3: Missing Files

**Problem:** `ERROR: JCL file 'jobs/hello_world.jcl' not found`

**Solution:**
```bash
# Check file structure
ls -la jobs/
ls -la programs/

# Verify you're in the framework directory
ls -la jcl_parser.sh
```

### Issue 4: COBOL Compiler Issues

**Problem:** Compilation errors or library issues

**Solution:**
```bash
# Force simulation mode (bypasses compiler)
export FORCE_SIMULATION=true
./jcl_parser.sh jobs/hello_world.jcl

# Or install/fix COBOL compiler
sudo apt install gnucobol  # Ubuntu/Debian
```

## Development Environment Setup

### Recommended VS Code Extensions

```bash
# Install VS Code extensions for better development
code --install-extension bitlang.cobol
code --install-extension ms-vscode.vscode-json
```

### Useful Aliases

Add to your `~/.bashrc` or `~/.zshrc`:

```bash
# JCL Framework aliases
alias jcl='./jcl_parser.sh'
alias jcldemo='./demo.sh'
alias jcllog='ls -la /tmp/jcl_sim/sysout/ && cat /tmp/jcl_sim/sysout/*.log'

# Reload shell
source ~/.bashrc
```

### Project Structure Best Practices

```
jcl-framework/
├── jcl_parser.sh          # Core parser (required)
├── demo.sh                # Demo script
├── jobs/                  # Your JCL jobs
│   ├── hello_world.jcl
│   └── my_custom_job.jcl
├── programs/              # Your COBOL programs
│   ├── hello_world.cbl
│   └── my_program.cbl
├── datasets/              # Input/output data
│   ├── input_data.dat
│   └── master_file.dat
└── docs/                  # Documentation
    ├── JCL_CONCEPTS_GUIDE.md
    └── REQUIREMENTS_AND_SETUP.md
```

## Performance and Resource Usage

### Disk Space Usage

- **Framework files:** ~1MB
- **Temporary files:** ~10MB per job execution
- **Logs:** ~1MB per job (automatically cleaned)

### Memory Usage

- **Framework:** ~10MB RAM
- **COBOL compilation:** ~50MB RAM per program
- **Job execution:** ~5MB RAM per step

### Cleanup Commands

```bash
# Clean temporary files
rm -rf /tmp/jcl_sim/

# Clean old logs (optional)
find /tmp/jcl_sim/sysout/ -name "*.log" -mtime +7 -delete
```

## Getting Help and Support

### Built-in Help

```bash
# Framework usage
./jcl_parser.sh

# Demo with examples
./demo.sh

# Dataset manager help
./dataset_manager.sh
```

### Troubleshooting Steps

1. **Check file permissions:** `ls -la *.sh`
2. **Verify file structure:** `ls -la jobs/ programs/`
3. **Test with demo:** `./demo.sh`
4. **Check logs:** `cat /tmp/jcl_sim/sysout/*.log`
5. **Force simulation:** `export FORCE_SIMULATION=true`

### Learning Resources

- **Start here:** Run `./demo.sh`
- **Read:** `JCL_CONCEPTS_GUIDE.md`
- **Practice:** Modify `jobs/hello_world.jcl`
- **Experiment:** Create new COBOL programs

## Next Steps

Once you have the framework running:

1. **Complete the demo:** `./demo.sh`
2. **Read the concepts guide:** `JCL_CONCEPTS_GUIDE.md`
3. **Run your first job:** `./jcl_parser.sh jobs/hello_world.jcl`
4. **Create custom jobs:** Add files to `jobs/` and `programs/`
5. **Explore enterprise concepts:** Dataset management, error handling

💡 **Congratulations! You now have a complete open-source JCL learning environment. This setup provides authentic enterprise batch processing experience without the cost and complexity of mainframe systems!**

---

**Ready to start learning JCL?** Begin with `./demo.sh` and follow the `JCL_CONCEPTS_GUIDE.md` for comprehensive learning!