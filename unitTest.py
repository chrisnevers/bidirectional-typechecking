################################################################################
# End to end tests for the example programs found in ./examples/               #
################################################################################

import os
from colorama import init
from termcolor import colored

init()

# Pretty print the pass/fail messages
passed = colored(" PASSED ", 'white', 'on_green')
failed = colored(" FAILED ", 'white', 'on_red')

print(colored('Running the example programs ...', 'blue'))

def test(filepath, input, expected):
  # Compile file
  cmd = "bidir examples/" + filepath + ".fs"
  # Depending whether the program receives stdin, pipe input to the program
  # cmd = run if input == None else "echo " + input + " | " + run
  # Run the executable and capture the output
  myCmd = os.popen(cmd).read().strip()
  # print (myCmd)
  # See if the actual and expected output match
  status = passed if myCmd == expected else failed
  # Report results to user
  print(status + " " + filepath)

print(colored("Basics", "yellow", attrs=['bold']))
test("2_int", None, "5 : Int")
test("3_paren", None, "5 : Int")
test("4_app", None, "5 : Int")
test("5_fun", None, "(λ a . (λ b . ((add a) b))) : ∃'α/7 → ∃'α/7 → ∃'α/7")
test("6_add", None, "13 : Int")
test("7_show", None, "5 : String")
test("8_bool", None, "True : Bool")
test("9_let", None, "(λ b . b) : ∃'α/14 → ∃'α/14")
test("10_eq", None, "True : Bool")
test("11_nested", None, "4 : Int")
test("12_nested_parens", None, "6 : Int")
test("13_if", None, "True : Bool")
test("14_rec", None, "3628800 : Int")
test("15_add_many", None, "21 : Int")
test("17_dollars", None, "10 : Int")
