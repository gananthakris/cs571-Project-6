import subprocess
import shutil
import sys
import argparse
from pathlib import Path

REF_OUTPUT = Path('.').resolve()/'ref_out'
NEW_REF_OUTPUT = Path('.').resolve()/'new_ref_out'
COLLABORATORS = Path('.').resolve()/'collaborators'

def check_collaborators():
    default_collabs = """Collaborators:

External resources:

I certify that other than listed above, I have not collaborated with anyone on this assignment nor have I used any external resources to assist me in completing this assignment: <put your name here>
"""

    collabs = None
    with COLLABORATORS.open('r') as f:
        collabs = f.read()

    if (collabs == default_collabs):
        print("Error: collaborators not edited!")
    else:
        print("Collaborators ok")
    sys.stdout.flush()

def test_ast_compile():
    subprocess.run(['make', '-C', 'ast', 'clean'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    run_output = subprocess.run(['make', '-C', 'ast'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    if run_output.returncode != 0:
        print('Error: compiling AST failed!')
    else:
        print('Compiling ok')
    sys.stdout.flush()

def test_ast_run():
    run_output = subprocess.run('./ast/test_ast', stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')

    if run_output.returncode != 0:
        print('Error: running program failed!')
    else:
        print('Running program ok')
    sys.stdout.flush()

    desired_output = None
    REF_OUTPUT = Path('.').resolve()/'ast'/'ref_out'
    with REF_OUTPUT.open('r') as f:
        desired_output = f.read()

    if run_output.stdout == desired_output and run_output.stderr == "":
        print("Program output ok")
    else:
        print("Error: incorrect program output!")
    sys.stdout.flush()

def test_ast_compile():
    subprocess.run(['make', '-C', 'ast', 'clean'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    run_output = subprocess.run(['make', '-C', 'ast'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    if run_output.returncode != 0:
        print('Error: compiling AST failed!')
    else:
        print('Compiling ok')
    sys.stdout.flush()

def test_ast_run():
    run_output = subprocess.run('./ast/test_ast', stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')

    if run_output.returncode != 0:
        print('Error: running program failed!')
    else:
        print('Running program ok')
    sys.stdout.flush()

    desired_output = None
    REF_OUTPUT = Path('.').resolve()/'ast'/'ref_out'
    with REF_OUTPUT.open('r') as f:
        desired_output = f.read()

    if run_output.stdout == desired_output and run_output.stderr == "":
        print("Program output ok")
    else:
        print("Error: incorrect program output!")
    sys.stdout.flush()

def test_bank_compile():
    subprocess.run(['make', '-C', 'bank', 'clean'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    run_output = subprocess.run(['make', '-C', 'bank'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')
    if run_output.returncode != 0:
        print('Error: compiling Bank failed!')
    else:
        print('Compiling ok')
    sys.stdout.flush()

def test_bank_run():
    run_output = subprocess.run('./bank/test_bank', stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding='utf-8')

    if run_output.returncode != 0:
        print('Error: running program failed!')
    else:
        print('Running program ok')
    sys.stdout.flush()

    desired_output = None
    REF_OUTPUT = Path('.').resolve()/'bank'/'ref_out'
    with REF_OUTPUT.open('r') as f:
        desired_output = f.read()

    if run_output.stdout == desired_output and run_output.stderr == "":
        print("Program output ok")
    else:
        print("Error: incorrect program output!")
    sys.stdout.flush()

def test_bank_run2():
    subprocess.run(['rm', '-rf', 'cs-571-fa24-extra-tests'])
    subprocess.run(['git', 'clone', 'https://gitlab.com/eatkinson/cs-571-fa24-extra-tests.git'],stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    EXTRA_TESTS_PATH = Path('cs-571-fa24-extra-tests/project4').resolve()
    try:
        shutil.copy(str(EXTRA_TESTS_PATH/'???'), '???')
        shutil.copy(str(EXTRA_TESTS_PATH/'???'), str(NEW_REF_OUTPUT))
    except FileNotFoundError:
        # Ok, files are't there. Must not have released new tests yet.
        print("New tests not yet available")
        return

    compile_output = subprocess.run(['make', 'clean'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    compile_output = subprocess.run(['make', 'new_expr'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    run_output = subprocess.run('./new_expr', stderr=subprocess.PIPE, stdout=subprocess.PIPE, encoding='utf-8')
    
    desired_output = None
    with NEW_REF_OUTPUT.open('r') as f:
        desired_output = f.read()

    if run_output.stdout == desired_output and run_output.stderr == "":
        print("New program output ok")
    else:
        print("Error: incorrect program output on new inputs!")
    sys.stdout.flush()

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--test', '-t', default=None, help="Test to run (1 = compile, 2 = run, 3 = run new tests)")

    args = parser.parse_args()

    if args.test == None:
        check_collaborators()
        test_ast_compile()
        test_ast_run()
        test_bank_compile()
        test_bank_run()
    elif args.test == "1":
        check_collaborators()
        test_ast_compile()
        test_ast_run()
    elif args.test == "2":
        check_collaborators()
        test_bank_compile()
    elif args.test == "3":
        check_collaborators()
        test_bank_run()
    elif args.test == "4":
        check_collaborators()
        test_bank_run2()