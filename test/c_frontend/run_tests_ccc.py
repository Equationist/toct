#!/usr/bin/env python3
"""
C Frontend Test Runner - CCC Edition

This script runs all C frontend tests using our CCC compiler
and compares the outputs against GCC for validation.
"""

import os
import sys
import subprocess
import re
import glob
from pathlib import Path

# Colors for output
GREEN = '\033[92m'
RED = '\033[91m'
YELLOW = '\033[93m'
BLUE = '\033[94m'
CYAN = '\033[96m'
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

def compile_and_run_with_ccc(c_file):
    """Compile and run a C file using our CCC compiler"""
    c_file_path = Path(c_file)
    exe_name = c_file_path.stem + "_ccc"
    
    # Get the CCC compiler path (relative to repo root)
    repo_root = Path(__file__).parent.parent.parent
    ccc_path = repo_root / "_build/default/bin/ccc_enhanced.bc"
    
    if not ccc_path.exists():
        return None, f"CCC compiler not found at {ccc_path}. Run 'dune build bin/ccc_enhanced.bc' first."
    
    # Compile with CCC
    compile_cmd = f"{ccc_path} {c_file} -o {exe_name}"
    stdout, stderr, returncode = run_command(compile_cmd, cwd=c_file_path.parent)
    
    if returncode != 0:
        return None, f"CCC compilation failed: {stderr}"
    
    # Run the executable
    run_cmd = f"./{exe_name}"
    stdout, stderr, returncode = run_command(run_cmd, cwd=c_file_path.parent)
    
    # Clean up
    exe_path = c_file_path.parent / exe_name
    if exe_path.exists():
        exe_path.unlink()
    
    if returncode != 0:
        return None, f"Runtime error: {stderr}"
    
    return stdout, None

def compile_and_run_with_gcc(c_file):
    """Compile and run a C file using GCC for reference"""
    c_file_path = Path(c_file)
    exe_name = c_file_path.stem + "_gcc"
    
    # Compile with GCC
    compile_cmd = f"gcc -o {exe_name} {c_file} -Wall -Wextra"
    stdout, stderr, returncode = run_command(compile_cmd, cwd=c_file_path.parent)
    
    if returncode != 0:
        return None, f"GCC compilation failed: {stderr}"
    
    # Run
    run_cmd = f"./{exe_name}"
    stdout, stderr, returncode = run_command(run_cmd, cwd=c_file_path.parent)
    
    # Clean up
    exe_path = c_file_path.parent / exe_name
    if exe_path.exists():
        exe_path.unlink()
    
    if returncode != 0:
        return None, f"Runtime error: {stderr}"
    
    return stdout, None

def compare_output(actual, expected):
    """Compare actual output with expected, handling regex patterns"""
    actual_lines = actual.strip().split('\n')
    expected_lines = expected.strip().split('\n')
    
    if len(actual_lines) != len(expected_lines):
        return False, f"Line count mismatch: {len(actual_lines)} vs {len(expected_lines)}"
    
    for i, (actual_line, expected_line) in enumerate(zip(actual_lines, expected_lines)):
        # Handle regex patterns - only treat as regex if it contains specific regex constructs
        is_regex = (
            ('{' in expected_line and '}' in expected_line) or  # {3}, {1,3}
            ('\\' in expected_line and any(c in expected_line for c in 'dwsWDS+*?')) or  # \d, \w, etc.
            ('.*' in expected_line) or  # .* wildcard
            ('|' in expected_line and '(' in expected_line and ')' in expected_line) or  # (a|b)
            ('[' in expected_line and ']' in expected_line and any(c in expected_line for c in '-^'))  # [a-z], [^abc]
        )
        
        if is_regex:
            # This looks like a regex pattern
            try:
                if not re.search(expected_line, actual_line):
                    return False, f"Line {i+1} mismatch: '{actual_line}' doesn't match pattern '{expected_line}'"
            except re.error:
                # If regex is invalid, fall back to literal comparison
                if actual_line != expected_line:
                    return False, f"Line {i+1} mismatch: '{actual_line}' vs '{expected_line}'"
        elif '0x[0-9a-f]+' in expected_line:
            pattern = expected_line.replace('0x[0-9a-f]+', r'0x[0-9a-f]+')
            if not re.match(pattern, actual_line):
                return False, f"Line {i+1} mismatch: '{actual_line}' doesn't match pattern '{pattern}'"
        else:
            if actual_line != expected_line:
                return False, f"Line {i+1} mismatch: '{actual_line}' vs '{expected_line}'"
    
    return True, "Match"

def test_c_file_ccc_vs_expected(c_file):
    """Test a C file with CCC compiler against expected output"""
    print(f"  Testing {Path(c_file).name}...", end=" ")
    
    # Get expected output
    expected_file = c_file.replace('.c', '.expected')
    if not os.path.exists(expected_file):
        print(f"{RED}FAIL{RESET} - No expected output file")
        return False, None, None
    
    with open(expected_file, 'r') as f:
        expected_output = f.read()
    
    # Compile and run with CCC
    ccc_output, ccc_error = compile_and_run_with_ccc(c_file)
    
    if ccc_error:
        print(f"{RED}FAIL{RESET} - CCC: {ccc_error}")
        return False, None, None
    
    # Compare CCC output with expected
    match, message = compare_output(ccc_output, expected_output)
    
    if match:
        print(f"{GREEN}PASS{RESET}")
        return True, ccc_output, expected_output
    else:
        print(f"{RED}FAIL{RESET} - {message}")
        print(f"    Expected: {expected_output[:50]}...")
        print(f"    CCC got:  {ccc_output[:50]}...")
        return False, ccc_output, expected_output

def test_c_file_ccc_vs_gcc(c_file):
    """Test a C file comparing CCC output to GCC output"""
    print(f"  Testing {Path(c_file).name} (CCC vs GCC)...", end=" ")
    
    # Compile and run with both compilers
    ccc_output, ccc_error = compile_and_run_with_ccc(c_file)
    gcc_output, gcc_error = compile_and_run_with_gcc(c_file)
    
    # Handle compilation errors
    if ccc_error and gcc_error:
        print(f"{YELLOW}SKIP{RESET} - Both compilers failed")
        return None
    elif ccc_error:
        print(f"{RED}FAIL{RESET} - CCC failed, GCC succeeded: {ccc_error}")
        return False
    elif gcc_error:
        print(f"{YELLOW}SKIP{RESET} - GCC failed (may be a complex feature): {gcc_error}")
        return None
    
    # Compare outputs
    match, message = compare_output(ccc_output, gcc_output)
    
    if match:
        print(f"{GREEN}PASS{RESET}")
        return True
    else:
        print(f"{RED}FAIL{RESET} - {message}")
        print(f"    GCC got:  {gcc_output[:50]}...")
        print(f"    CCC got:  {ccc_output[:50]}...")
        return False

def run_test_directory(test_dir, mode="expected"):
    """Run all tests in a directory"""
    test_files = glob.glob(os.path.join(test_dir, "*.c"))
    
    if not test_files:
        print(f"  No test files found in {test_dir}")
        return 0, 0
    
    passed = 0
    total = 0
    
    for test_file in sorted(test_files):
        if mode == "expected":
            result, _, _ = test_c_file_ccc_vs_expected(test_file)
        else:  # mode == "gcc"
            result = test_c_file_ccc_vs_gcc(test_file)
        
        if result is not None:  # None means skip
            total += 1
            if result:
                passed += 1
    
    return passed, total

def main():
    """Main test runner"""
    import argparse
    
    parser = argparse.ArgumentParser(description='CCC Compiler Test Suite')
    parser.add_argument('--mode', choices=['expected', 'gcc'], default='expected',
                       help='Test mode: compare against expected files or GCC output')
    parser.add_argument('--category', type=str,
                       help='Run tests only for specific category (e.g., basic, functions)')
    args = parser.parse_args()
    
    print(f"{BLUE}CCC Compiler Test Suite{RESET}")
    if args.mode == 'expected':
        print(f"{BLUE}==== (Against Expected Output) ===={RESET}")
    else:
        print(f"{BLUE}==== (CCC vs GCC Comparison) ===={RESET}")
    print()
    
    # Check if CCC compiler is built
    repo_root = Path(__file__).parent.parent.parent
    ccc_path = repo_root / "_build/default/bin/ccc_enhanced.bc"
    
    if not ccc_path.exists():
        print(f"{RED}ERROR: CCC compiler not found at {ccc_path}{RESET}")
        print("Please run: dune build bin/ccc_enhanced.bc")
        return 1
    
    test_root = Path(__file__).parent
    
    # Test categories
    test_categories = [
        ("basic", "Basic C constructs"),
        ("functions", "Function definitions and calls"),
        ("pointers", "Pointer and array operations"),
        ("structs", "Struct and typedef declarations"),
    ]
    
    if args.category:
        test_categories = [(cat, desc) for cat, desc in test_categories if cat == args.category]
    
    total_passed = 0
    total_tests = 0
    
    # Run tests
    for category, description in test_categories:
        test_dir = test_root / category
        if test_dir.exists():
            print(f"{YELLOW}{description}:{RESET}")
            passed, total = run_test_directory(str(test_dir), args.mode)
            total_passed += passed
            total_tests += total
            print(f"  {passed}/{total} tests passed")
            print()
    
    # Summary
    print(f"{BLUE}Summary:{RESET}")
    print(f"  Total tests: {total_tests}")
    print(f"  Passed: {GREEN}{total_passed}{RESET}")
    print(f"  Failed: {RED}{total_tests - total_passed}{RESET}")
    
    if total_passed == total_tests:
        print(f"\n{GREEN}All tests passed! 🎉{RESET}")
        return 0
    else:
        percentage = (total_passed / total_tests * 100) if total_tests > 0 else 0
        print(f"\n{CYAN}Success rate: {percentage:.1f}%{RESET}")
        if percentage >= 75:
            print(f"{CYAN}Great progress! 🚀{RESET}")
        elif percentage >= 50:
            print(f"{YELLOW}Good start! Keep going! 💪{RESET}")
        else:
            print(f"{RED}Needs more work, but you're getting there! 🔧{RESET}")
        return 1

if __name__ == "__main__":
    sys.exit(main())