#!/usr/bin/env python3
"""
JSON linter with Comments (JSONC) support for Emacs flymake.

https://jsonc.org/

This script removes // comments from JSON and validates the result.
Designed to work as a flymake backend in Emacs.

Usage:
    json_lint.py filename.json
"""

import json
import sys
import os


def peek_char(text, pos):
    """Peek at the next character without advancing position."""
    assert isinstance(text, str)
    assert isinstance(pos, int)
    assert pos >= 0

    if pos + 1 < len(text):
        return text[pos + 1]
    return None


def handle_string(text, pos, result):
    """Handle a JSON string, preserving all content including // inside strings.

    Returns the position after the closing quote, or end of text if unterminated.
    """
    assert isinstance(text, str)
    assert isinstance(pos, int)
    assert isinstance(result, list)
    assert pos >= 0
    assert pos < len(text)
    assert text[pos] == '"'

    # Opening quote.
    result.append('"')
    pos += 1

    while pos < len(text):
        char = text[pos]
        result.append(char)

        if char == "\\":
            # Handle escaped characters.
            pos += 1
            if pos < len(text):
                result.append(text[pos])
                pos += 1
        elif char == '"':
            # End of string found.
            pos += 1
            break
        else:
            pos += 1

    # pos now points after the closing quote, or at end of text if
    # unterminated.
    assert pos >= 0
    assert pos <= len(text)
    return pos


def skip_line_comment(text, pos, result):
    """Skip a // comment to the end of the line.

    Returns the position after the newline, or end of text.
    """
    assert isinstance(text, str)
    assert isinstance(pos, int)
    assert isinstance(result, list)
    assert pos >= 0
    assert pos + 1 < len(text)
    assert text[pos : pos + 2] == "//"

    # Skip the // and everything until newline.
    while pos < len(text) and text[pos] != "\n":
        pos += 1

    # Preserve line structure for error reporting.
    if pos < len(text) and text[pos] == "\n":
        result.append("\n")
        pos += 1

    assert pos >= 0
    assert pos <= len(text)
    return pos


def remove_comments(text):
    """Remove // comments from JSON text while preserving strings."""
    assert isinstance(text, str)

    pos = 0
    length = len(text)
    result = []

    while pos < length:
        char = text[pos]

        if char == '"':
            # Preserve everything inside including //.
            pos = handle_string(text, pos, result)
        elif char == "/" and peek_char(text, pos) == "/":
            # Line comment.
            pos = skip_line_comment(text, pos, result)
        else:
            result.append(char)
            pos += 1

    cleaned = "".join(result)
    assert isinstance(cleaned, str)
    return cleaned


def lint_json(text, filename):
    """
    Lint JSON text and return flymake-compatible error messages.

    Args:
        text: JSON text to lint
        filename: Name of file being linted (for error messages)

    Returns:
        List of error messages in flymake format: "filename:line:col: error: message"
    """
    assert isinstance(text, str)
    assert isinstance(filename, str)

    errors = []
    if not text.strip():
        errors.append(f"{filename}:1:1: error: Empty JSON document")
        return errors

    try:
        cleaned_text = remove_comments(text)

        # Check if we have any content left after comment removal.
        if not cleaned_text.strip():
            errors.append(
                f"{filename}:1:1: error: No JSON content after removing comments"
            )
            return errors

        # Try to parse the cleaned JSON
        parsed = json.loads(cleaned_text)

        # Additional validation: ensure we got some actual data
        if parsed is None:
            errors.append(f"{filename}:1:1: error: JSON document contains only null")

    except json.JSONDecodeError as e:
        # Extract error details from JSONDecodeError.
        line = getattr(e, "lineno", 1)
        col = getattr(e, "colno", 1)
        msg = getattr(e, "msg", str(e))

        # Ensure line and column are positive integers.
        line = max(1, int(line)) if isinstance(line, (int, float)) else 1
        col = max(1, int(col)) if isinstance(col, (int, float)) else 1

        if not msg:
            msg = "Invalid JSON syntax"

        # Format error for flymake: filename:line:col: error: message
        error_msg = f"{filename}:{line}:{col}: error: {msg}"
        errors.append(error_msg)

    except (TypeError, ValueError) as e:
        error_msg = f"{filename}:1:1: error: JSON parsing failed: {str(e)}"
        errors.append(error_msg)

    except Exception as e:
        error_msg = f"{filename}:1:1: error: Unexpected error: {str(e)}"
        errors.append(error_msg)

    assert isinstance(errors, list)
    return errors


def main():
    if len(sys.argv) != 2:
        print("Usage: json_lint.py <filename>", file=sys.stderr)
        sys.exit(2)

    filename = sys.argv[1]
    assert isinstance(filename, str)

    if not os.path.exists(filename):
        print(f"{filename}:1:1: error: File does not exist", file=sys.stderr)
        sys.exit(1)

    if not os.path.isfile(filename):
        print(f"{filename}:1:1: error: Path is not a regular file", file=sys.stderr)
        sys.exit(1)

    if not os.access(filename, os.R_OK):
        print(f"{filename}:1:1: error: File is not readable", file=sys.stderr)
        sys.exit(1)

    # Read input from file
    try:
        with open(filename, "r", encoding="utf-8") as f:
            content = f.read()
    except UnicodeDecodeError as e:
        print(f"{filename}:1:1: error: File encoding error: {str(e)}", file=sys.stderr)
        sys.exit(1)
    except IOError as e:
        print(f"{filename}:1:1: error: Cannot read file: {str(e)}", file=sys.stderr)
        sys.exit(1)
    except OSError as e:
        print(
            f"{filename}:1:1: error: System error reading file: {str(e)}",
            file=sys.stderr,
        )
        sys.exit(1)

    assert isinstance(content, str)
    errors = lint_json(content, filename)

    # Output errors to stdout (flymake expects this).
    for error in errors:
        assert isinstance(error, str)
        print(error)

    if errors:
        sys.exit(1)  # Errors found
    else:
        sys.exit(0)  # Success


if __name__ == "__main__":
    main()
