#!/usr/bin/env python3
"""
Demo script showing preprocessor testing capabilities
"""

import subprocess
import sys
from pathlib import Path

def run_command(cmd):
    """Run a command and return output"""
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        return result.stdout, result.stderr, result.returncode
    except Exception as e:
        return "", str(e), 1

def demo_preprocessor_testing():
    print("🔬 C Frontend Preprocessor Testing Demo")
    print("="*50)
    
    # Change to test directory
    test_dir = Path(__file__).parent
    
    print("\n1. Standard execution tests only:")
    print("-" * 30)
    cmd = f"cd {test_dir} && python3 run_tests.py | tail -10"
    stdout, stderr, code = run_command(cmd)
    print(stdout)
    
    print("\n2. Preprocessor-only tests:")
    print("-" * 30)
    cmd = f"cd {test_dir} && python3 run_tests.py --preprocessor-only"
    stdout, stderr, code = run_command(cmd)
    print(stdout)
    
    print("\n3. Example: Manual preprocessor output inspection:")
    print("-" * 30)
    print("Source file: preprocessor/basic/simple_defines_no_includes.c")
    
    source_file = test_dir / "preprocessor/basic/simple_defines_no_includes.c"
    if source_file.exists():
        with open(source_file) as f:
            print("\nOriginal source:")
            print(f.read())
        
        cmd = f"cd {source_file.parent} && gcc -E {source_file.name} -o - | grep -v '^#'"
        stdout, stderr, code = run_command(cmd)
        print("\nPreprocessor output:")
        print(stdout)
    
    print("\n4. Example: Macro expansion test:")
    print("-" * 30)
    
    macro_file = test_dir / "preprocessor/functions/macro_expansion_test.c"
    if macro_file.exists():
        with open(macro_file) as f:
            content = f.read()
            print("\nMacro definitions and usage:")
            for line in content.split('\n')[:6]:
                print(line)
        
        expected_file = macro_file.with_suffix('.preprocessed')
        if expected_file.exists():
            with open(expected_file) as f:
                print("\nExpected preprocessor output:")
                print(f.read())
    
    print("\n5. Testing specific preprocessor features:")
    print("-" * 30)
    
    # Test stringification
    print("Testing stringification:")
    cmd = 'echo \'#define STR(x) #x\nchar* s = STR(hello world);\' | gcc -E -'
    stdout, stderr, code = run_command(cmd)
    print("Input: #define STR(x) #x")
    print("       char* s = STR(hello world);")
    print(f"Output: {stdout.split()[-1] if stdout else 'Error'}")
    
    # Test token pasting
    print("\nTesting token pasting:")
    cmd = 'echo \'#define CONCAT(a,b) a##b\nint CONCAT(var,name) = 42;\' | gcc -E -'
    stdout, stderr, code = run_command(cmd)
    print("Input: #define CONCAT(a,b) a##b")
    print("       int CONCAT(var,name) = 42;")
    relevant_line = [line for line in stdout.split('\n') if 'varname' in line]
    print(f"Output: {relevant_line[0] if relevant_line else 'Error'}")
    
    print("\n✅ Preprocessor testing demo complete!")
    print("\nKey Benefits of Separate Preprocessor Testing:")
    print("• Validates macro expansion independently of compilation")
    print("• Tests conditional compilation logic")
    print("• Verifies stringification and token pasting")
    print("• Enables debugging of preprocessor issues")
    print("• Supports incremental C frontend development")

if __name__ == "__main__":
    demo_preprocessor_testing()