#!/usr/bin/env python3
"""
C Frontend Test Runner

This script runs all C frontend tests and verifies outputs.
Currently uses GCC/Clang to verify test correctness.
Will be adapted to use our C frontend once implemented.
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

def preprocess_only_c_file(c_file, compiler="gcc"):
    """Run preprocessor only on a C file, return preprocessed output"""
    c_file_path = Path(c_file)
    
    # Run preprocessor only (-E flag)
    preprocess_cmd = f"{compiler} -E {c_file} -o -"
    stdout, stderr, returncode = run_command(preprocess_cmd, cwd=c_file_path.parent)
    
    if returncode != 0:
        return None, f"Preprocessing failed: {stderr}"
    
    # Clean up the output - remove line directives and empty lines for cleaner comparison
    lines = stdout.split('\n')
    cleaned_lines = []
    
    for line in lines:
        # Skip line directives (# linenum "filename")
        if line.strip().startswith('#') and ('"' in line or line.strip().startswith('# ')):
            continue
        # Skip empty lines
        if line.strip():
            cleaned_lines.append(line)
    
    return '\n'.join(cleaned_lines), None

def compile_and_run_c_file(c_file, compiler="gcc"):
    """Compile and run a C file, return output"""
    c_file_path = Path(c_file)
    exe_name = c_file_path.stem
    
    # Compile
    compile_cmd = f"{compiler} -o {exe_name} {c_file} -Wall -Wextra"
    stdout, stderr, returncode = run_command(compile_cmd, cwd=c_file_path.parent)
    
    if returncode != 0:
        return None, f"Compilation failed: {stderr}"
    
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

def test_preprocessor_only(c_file):
    """Test preprocessor output only"""
    print(f"  Testing {Path(c_file).name} (preprocessor)...", end=" ")
    
    # Get expected preprocessor output
    expected_file = c_file.replace('.c', '.preprocessed')
    if not os.path.exists(expected_file):
        print(f"{YELLOW}SKIP{RESET} - No expected preprocessor output file")
        return None  # Neither pass nor fail, just skip
    
    with open(expected_file, 'r') as f:
        expected_output = f.read()
    
    # Run preprocessor only
    actual_output, error = preprocess_only_c_file(c_file)
    
    if error:
        print(f"{RED}FAIL{RESET} - {error}")
        return False
    
    # Compare outputs
    match, message = compare_output(actual_output, expected_output)
    
    if match:
        print(f"{GREEN}PASS{RESET}")
        return True
    else:
        print(f"{RED}FAIL{RESET} - {message}")
        print(f"    Expected: {expected_output[:100]}...")
        print(f"    Actual:   {actual_output[:100]}...")
        return False

def test_positive_case(c_file):
    """Test a positive case (should compile and run successfully)"""
    print(f"  Testing {Path(c_file).name}...", end=" ")
    
    # Get expected output
    expected_file = c_file.replace('.c', '.expected')
    if not os.path.exists(expected_file):
        print(f"{RED}FAIL{RESET} - No expected output file")
        return False
    
    with open(expected_file, 'r') as f:
        expected_output = f.read()
    
    # Compile and run
    actual_output, error = compile_and_run_c_file(c_file)
    
    if error:
        print(f"{RED}FAIL{RESET} - {error}")
        return False
    
    # Compare outputs
    match, message = compare_output(actual_output, expected_output)
    
    if match:
        print(f"{GREEN}PASS{RESET}")
        return True
    else:
        print(f"{RED}FAIL{RESET} - {message}")
        print(f"    Expected: {expected_output[:50]}...")
        print(f"    Actual:   {actual_output[:50]}...")
        return False

def test_negative_case(c_file):
    """Test a negative case (should fail compilation)"""
    print(f"  Testing {Path(c_file).name}...", end=" ")
    
    # Try to compile (should fail)
    c_file_path = Path(c_file)
    exe_name = c_file_path.stem
    
    compile_cmd = f"gcc -o {exe_name} {c_file} -Wall -Wextra -Werror"
    stdout, stderr, returncode = run_command(compile_cmd, cwd=c_file_path.parent)
    
    if returncode == 0:
        print(f"{RED}FAIL{RESET} - Expected compilation failure but succeeded")
        # Clean up if executable was created
        exe_path = c_file_path.parent / exe_name
        if exe_path.exists():
            exe_path.unlink()
        return False
    else:
        print(f"{GREEN}PASS{RESET} - Compilation failed as expected")
        return True

def run_test_directory(test_dir, test_type="positive", include_preprocessor=False):
    """Run all tests in a directory"""
    test_files = glob.glob(os.path.join(test_dir, "*.c"))
    
    if not test_files:
        print(f"  No test files found in {test_dir}")
        return 0, 0, 0, 0  # passed_exec, total_exec, passed_preproc, total_preproc
    
    passed_exec = 0
    passed_preproc = 0
    total_exec = len(test_files)
    total_preproc = 0
    
    for test_file in sorted(test_files):
        # Run execution test
        if test_type == "positive":
            if test_positive_case(test_file):
                passed_exec += 1
        else:  # negative
            if test_negative_case(test_file):
                passed_exec += 1
        
        # Run preprocessor test if requested and if it's a preprocessor-relevant test
        if include_preprocessor and ("preprocessor" in test_dir or test_type == "positive"):
            result = test_preprocessor_only(test_file)
            if result is not None:  # Skip doesn't count toward total
                total_preproc += 1
                if result:
                    passed_preproc += 1
    
    return passed_exec, total_exec, passed_preproc, total_preproc

def main():
    """Main test runner"""
    import argparse
    
    parser = argparse.ArgumentParser(description='C Frontend Test Suite')
    parser.add_argument('--preprocessor-only', action='store_true', 
                       help='Run preprocessor-only tests')
    parser.add_argument('--include-preprocessor', action='store_true',
                       help='Include preprocessor output tests alongside execution tests')
    args = parser.parse_args()
    
    print(f"{BLUE}C Frontend Test Suite{RESET}")
    if args.preprocessor_only:
        print(f"{BLUE}====== (Preprocessor Only) ======{RESET}")
    elif args.include_preprocessor:
        print(f"{BLUE}==== (With Preprocessor Tests) ===={RESET}")
    else:
        print(f"{BLUE}==================={RESET}")
    print()
    
    test_root = Path(__file__).parent
    
    # Test categories
    test_categories = [
        ("basic", "Basic C constructs"),
        ("functions", "Function definitions and calls"),
        ("pointers", "Pointer and array operations"),
        ("structs", "Struct and typedef declarations"),
        ("preprocessor/basic", "Basic preprocessor directives"),
        ("preprocessor/functions", "Function-like macros"),
        ("preprocessor/conditional", "Conditional compilation"),
        ("preprocessor/complex", "Complex macro features"),
    ]
    
    total_passed_exec = 0
    total_tests_exec = 0
    total_passed_preproc = 0
    total_tests_preproc = 0
    
    # Run positive tests
    for category, description in test_categories:
        test_dir = test_root / category
        if test_dir.exists():
            print(f"{YELLOW}{description}:{RESET}")
            
            if args.preprocessor_only:
                # Only run preprocessor tests
                passed_exec, total_exec, passed_preproc, total_preproc = run_test_directory(
                    str(test_dir), "positive", include_preprocessor=True)
                if total_preproc > 0:
                    total_passed_preproc += passed_preproc
                    total_tests_preproc += total_preproc
                    print(f"  {passed_preproc}/{total_preproc} preprocessor tests passed")
                else:
                    print(f"  No preprocessor tests found")
            else:
                # Run execution tests, optionally with preprocessor tests
                passed_exec, total_exec, passed_preproc, total_preproc = run_test_directory(
                    str(test_dir), "positive", include_preprocessor=args.include_preprocessor)
                
                total_passed_exec += passed_exec
                total_tests_exec += total_exec
                
                if args.include_preprocessor and total_preproc > 0:
                    total_passed_preproc += passed_preproc
                    total_tests_preproc += total_preproc
                    print(f"  Execution: {passed_exec}/{total_exec} tests passed")
                    print(f"  Preprocessor: {passed_preproc}/{total_preproc} tests passed")
                else:
                    print(f"  {passed_exec}/{total_exec} tests passed")
            
            print()
    
    # Run negative tests
    negative_dirs = [
        ("negative", "Negative tests (should fail compilation)"),
        ("preprocessor/negative", "Preprocessor negative tests"),
    ]
    
    if not args.preprocessor_only:
        for neg_dir, description in negative_dirs:
            test_dir = test_root / neg_dir
            if test_dir.exists():
                print(f"{YELLOW}{description}:{RESET}")
                passed_exec, total_exec, _, _ = run_test_directory(str(test_dir), "negative")
                total_passed_exec += passed_exec
                total_tests_exec += total_exec
                print(f"  {passed_exec}/{total_exec} tests passed")
                print()
    
    # Summary
    print(f"{BLUE}Summary:{RESET}")
    
    if args.preprocessor_only:
        print(f"  Preprocessor tests: {total_tests_preproc}")
        print(f"  Passed: {GREEN}{total_passed_preproc}{RESET}")
        print(f"  Failed: {RED}{total_tests_preproc - total_passed_preproc}{RESET}")
        
        if total_passed_preproc == total_tests_preproc:
            print(f"\n{GREEN}All preprocessor tests passed!{RESET}")
            return 0
        else:
            print(f"\n{RED}Some preprocessor tests failed.{RESET}")
            return 1
    else:
        total_passed = total_passed_exec
        total_tests = total_tests_exec
        
        if args.include_preprocessor and total_tests_preproc > 0:
            print(f"  Execution tests: {total_tests_exec}")
            print(f"  Execution passed: {GREEN}{total_passed_exec}{RESET}")
            print(f"  Execution failed: {RED}{total_tests_exec - total_passed_exec}{RESET}")
            print(f"  Preprocessor tests: {total_tests_preproc}")
            print(f"  Preprocessor passed: {GREEN}{total_passed_preproc}{RESET}")
            print(f"  Preprocessor failed: {RED}{total_tests_preproc - total_passed_preproc}{RESET}")
            
            total_passed += total_passed_preproc
            total_tests += total_tests_preproc
        else:
            print(f"  Total tests: {total_tests}")
            print(f"  Passed: {GREEN}{total_passed}{RESET}")
            print(f"  Failed: {RED}{total_tests - total_passed}{RESET}")
        
        if total_passed == total_tests:
            print(f"\n{GREEN}All tests passed!{RESET}")
            return 0
        else:
            print(f"\n{RED}Some tests failed.{RESET}")
            return 1

if __name__ == "__main__":
    sys.exit(main())