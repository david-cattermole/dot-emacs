#!/usr/bin/env python
"""
Test file for mypy flymake backend.

This file contains intentional type errors to verify that the mypy
flymake integration is working correctly in Emacs.

To test:
1. Open this file in Emacs
2. Ensure python-mode-hook has davidc-python-flymake-mypy-setup enabled
3. Verify flymake-mode is active (check mode line)
4. Save the file to trigger mypy checking
5. Look for error indicators in the buffer
6. Use M-x flymake-show-buffer-diagnostics to see all errors
7. Use M-x flymake-next-error to navigate between errors

Expected errors:
- Line 24: Argument has incompatible type
- Line 27: Incompatible return value type
- Line 30: Unsupported operand types for +
- Line 36: Incompatible types in assignment
- Line 39: Item has incompatible type
- Line 45: Argument missing
"""


def greet(name: str) -> str:
    """Function that expects a string argument."""
    return "Hello, " + name

# Error: Argument 1 has incompatible type "int"; expected "str"
result = greet(123)

def add_numbers(a: int, b: int) -> int:
    """Function that should return an int."""
    # Error: Incompatible return value type (got "str", expected "int")
    return "not a number"

def calculate(x: int, y: int) -> int:
    # Error: Unsupported operand types for + ("int" and "str")
    return x + "5"


def process_data() -> None:
    """Function with various type errors."""
    # Error: Incompatible types in assignment (expression has type "str", variable has type "int")
    num: int = "not a number"

    # Error: List item 1 has incompatible type "str"; expected "int"
    numbers: list[int] = [1, 2, "three", 4]


def optional_param(name: str, age: int) -> str:
    """Function with required parameters."""
    return f"{name} is {age} years old"

# Error: Missing positional argument "age"
optional_param("Alice")


class Person:
    """Simple class for testing."""

    def __init__(self, name: str, age: int) -> None:
        self.name = name
        self.age = age

    def get_name(self) -> str:
        """Should return a string."""
        # Error: Incompatible return value type (got "int", expected "str")
        return self.age


def use_person() -> None:
    """Test class usage."""
    # Error: Argument "age" has incompatible type "str"; expected "int"
    person = Person("Bob", "thirty")

    # Error: "Person" has no attribute "address"
    print(person.address)


# Valid code (no errors)
def valid_function(x: int, y: int) -> int:
    """This function should not produce any type errors."""
    result = x + y
    return result

valid_result = valid_function(10, 20)
print(f"Valid result: {valid_result}")
