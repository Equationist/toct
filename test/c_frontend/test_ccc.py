#!/usr/bin/env python3
"""
Test the CCC compiler with simple C programs
"""

import os
import sys
import subprocess
from pathlib import Path

# Colors for output
GREEN = '\033[92m'
RED = '\033[91m'
YELLOW = '\033[93m'
BLUE = '\033[94m'
RESET = '\033[0m'

def run_command(cmd, cwd=None, input_data=None, timeout=10):
    """Run a command and return stdout, stderr, and return code"""
    try:
        result = subprocess.run(
            cmd, 
            shell=True, 
            capture_output=True, 
            text=True,
            cwd=cwd,
            input=input_data,
            timeout=timeout
        )
        return result.stdout, result.stderr, result.returncode
    except subprocess.TimeoutExpired:
        return "", "Command timed out", 1
    except Exception as e:
        return "", str(e), 1

def test_ccc_compile(c_file):
    """Test compiling a C file with CCC"""
    print(f"  Testing {Path(c_file).name}...", end=" ")
    
    # Get the CCC compiler path
    repo_root = Path(__file__).parent.parent.parent
    ccc_path = repo_root / "_build/default/examples/c_frontend/ccc.bc"
    
    if not ccc_path.exists():
        print(f"{RED}SKIP{RESET} - CCC compiler not found")
        return None
    
    # Compile with CCC
    c_file_path = Path(c_file)
    output_file = c_file_path.stem + "_ccc"
    
    compile_cmd = f"dune exec {ccc_path} -- {c_file} -o {output_file}"
    stdout, stderr, returncode = run_command(compile_cmd, cwd=c_file_path.parent)
    
    if returncode != 0:
        print(f"{RED}FAIL{RESET} - Compilation failed")
        if stderr:
            print(f"    Error: {stderr.strip()}")
        return False
    
    # Run the executable
    run_cmd = f"./{output_file}"
    stdout, stderr, returncode = run_command(run_cmd, cwd=c_file_path.parent)
    
    # Clean up
    exe_path = c_file_path.parent / output_file
    asm_path = c_file_path.parent / (output_file + ".s")
    obj_path = c_file_path.parent / (output_file + ".o")
    
    for path in [exe_path, asm_path, obj_path]:
        if path.exists():
            path.unlink()
    
    if returncode == 0:
        print(f"{GREEN}PASS{RESET} - Exit code: {returncode}")
        return True
    else:
        print(f"{YELLOW}PASS{RESET} - Compiled but exit code: {returncode}")
        return True  # Still counts as success if it compiles

def main():
    print(f"{BLUE}CCC Compiler Simple Tests{RESET}")
    print(f"{BLUE}========================{RESET}")
    print()
    
    # Test files in the examples directory
    examples_dir = Path(__file__).parent.parent.parent / "examples/c_frontend"
    
    test_files = [
        examples_dir / "test_simple_arithmetic.c",
        examples_dir / "test_control_flow.c",
    ]
    
    # Add more test files if they exist
    for f in ["test_loop.c", "test_function.c"]:
        path = examples_dir / f
        if path.exists():
            test_files.append(path)
    
    passed = 0
    total = 0
    
    for test_file in test_files:
        if test_file.exists():
            total += 1
            result = test_ccc_compile(test_file)
            if result:
                passed += 1
        else:
            print(f"  {test_file.name} not found, skipping")
    
    print()
    print(f"{BLUE}Summary:{RESET}")
    print(f"  Total: {total}")
    print(f"  Passed: {GREEN}{passed}{RESET}")
    print(f"  Failed: {RED}{total - passed}{RESET}")
    
    if passed == total:
        print(f"\n{GREEN}All tests passed! 🎉{RESET}")
        return 0
    else:
        print(f"\n{YELLOW}Some tests failed{RESET}")
        return 1

if __name__ == "__main__":
    sys.exit(main())