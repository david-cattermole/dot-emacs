#!/usr/bin/env python3
"""
yaml_lint.py - YAML linter for Emacs flymake integration.

This script validates YAML files and outputs diagnostics in a format
compatible with Emacs flymake:
  filename:line:column: level: message

Usage:
  python3 yaml_lint.py <yaml-file>

Requires:
  - Python 3.x
  - PyYAML package (optional, for enhanced validation)
"""

import sys
import os
import re

# Try to import PyYAML, but don't fail if it's not available.
HAS_PYYAML = False
try:
    import yaml

    HAS_PYYAML = True
except ImportError:
    pass


class YAMLLintError:
    """Represents a single lint error/warning."""

    def __init__(self, line, column, level, message):
        assert isinstance(line, int)
        assert isinstance(column, int)
        assert isinstance(level, str)
        assert isinstance(message, str)

        self.line = max(1, line)
        self.column = max(1, column)
        self.level = level  # "error", "warning", or "note".
        self.message = message

    def __str__(self, filename):
        assert isinstance(filename, str)

        return f"{filename}:{self.line}:{self.column}: {self.level}: {self.message}"

    def __eq__(self, other):
        return (
            isinstance(other, YAMLLintError)
            and self.line == other.line
            and self.column == other.column
            and self.level == other.level
            and self.message == other.message
        )

    def __hash__(self):
        return hash((self.line, self.column, self.level, self.message))


def check_yaml_syntax_basic(content):
    """Basic YAML syntax checking without PyYAML."""
    assert isinstance(content, str)

    errors = []
    lines = content.split("\n")

    # Track context.
    in_multiline_string = None  # '|' or '>' or None.
    multiline_indent = 0
    bracket_stack = []  # Track [], {} nesting.
    last_indent = 0
    expecting_value = False

    for line_num, line in enumerate(lines, 1):
        stripped = line.strip()
        indent = len(line) - len(line.lstrip())

        # Skip empty lines.
        if not stripped:
            continue

        # Handle multiline strings.
        if in_multiline_string:
            if indent <= multiline_indent and stripped:
                in_multiline_string = None
            else:
                continue

        # Skip comments.
        if stripped.startswith("#"):
            continue

        # Check for multiline string indicators.
        if line.rstrip().endswith("|") or line.rstrip().endswith(">"):
            in_multiline_string = line.rstrip()[-1]
            multiline_indent = indent
            expecting_value = False
            continue

        # Basic structure checks.
        #
        # Check for tabs in indentation.
        if "\t" in line[:indent]:
            errors.append(
                YAMLLintError(
                    line_num,
                    line.index("\t") + 1,
                    "error",
                    "Tab character in indentation",
                )
            )

        # Check for inconsistent indentation.
        if indent > 0 and indent != last_indent:
            if indent % 2 != 0:
                errors.append(
                    YAMLLintError(
                        line_num,
                        1,
                        "warning",
                        "Inconsistent indentation (not a multiple of 2)",
                    )
                )

        # Handle flow style (brackets).
        for i, char in enumerate(stripped):
            if char in "[{":
                bracket_stack.append((char, line_num, i + 1))
            elif char in "]}":
                if not bracket_stack:
                    errors.append(
                        YAMLLintError(
                            line_num,
                            indent + i + 1,
                            "error",
                            f"Unexpected closing bracket '{char}'",
                        )
                    )
                else:
                    open_char, open_line, open_col = bracket_stack.pop()
                    expected = "]" if open_char == "[" else "}"
                    if char != expected:
                        errors.append(
                            YAMLLintError(
                                line_num,
                                indent + i + 1,
                                "error",
                                f"Mismatched bracket: expected '{expected}' but got '{char}'",
                            )
                        )

        # Check for bare documents (content without ---).
        if (
            line_num == 1
            and not stripped.startswith("---")
            and not stripped.startswith("#")
        ):
            # This is just a note, not an error.
            pass

        # Check basic key-value structure.
        if ":" in line and not any(
            line.strip().startswith(p) for p in ["- ", "---", "..."]
        ):
            # Simple key-value check.
            parts = line.split(":", 1)
            if len(parts) == 2:
                key_part = parts[0].strip()
                value_part = parts[1].strip()

                # Check for empty key.
                if not key_part:
                    errors.append(
                        YAMLLintError(line_num, indent + 1, "error", "Empty key")
                    )

                # Check if we were expecting a value from previous line.
                if expecting_value and indent <= last_indent:
                    errors.append(
                        YAMLLintError(
                            line_num - 1,
                            1,
                            "warning",
                            "Key without value on previous line",
                        )
                    )

                expecting_value = not value_part

        # Check for list items.
        if stripped.startswith("- "):
            if expecting_value:
                errors.append(
                    YAMLLintError(
                        line_num - 1, 1, "warning", "Key without value on previous line"
                    )
                )
                expecting_value = False

        last_indent = indent

    # Check for unclosed brackets.
    for open_char, open_line, open_col in bracket_stack:
        expected = "]" if open_char == "[" else "}"
        errors.append(
            YAMLLintError(
                open_line,
                open_col,
                "error",
                f"Unclosed bracket '{open_char}' (expected '{expected}')",
            )
        )

    # If PyYAML is not available, add a note.
    if not HAS_PYYAML and not errors:
        errors.append(
            YAMLLintError(
                1, 1, "note", "PyYAML not available - using basic syntax checking only"
            )
        )

    return errors


def check_yaml_syntax_pyyaml(content):
    """Check YAML syntax using PyYAML."""
    assert isinstance(content, str)

    errors = []

    try:
        # Try to load all documents.
        list(yaml.safe_load_all(content))
    except yaml.scanner.ScannerError as e:
        errors.append(
            YAMLLintError(
                e.problem_mark.line + 1 if e.problem_mark else 1,
                e.problem_mark.column + 1 if e.problem_mark else 1,
                "error",
                f"YAML scanner error: {e.problem or 'Unknown error'}",
            )
        )
    except yaml.parser.ParserError as e:
        errors.append(
            YAMLLintError(
                e.problem_mark.line + 1 if e.problem_mark else 1,
                e.problem_mark.column + 1 if e.problem_mark else 1,
                "error",
                f"YAML parser error: {e.problem or 'Unknown error'}",
            )
        )
    except yaml.constructor.ConstructorError as e:
        errors.append(
            YAMLLintError(
                e.problem_mark.line + 1 if e.problem_mark else 1,
                e.problem_mark.column + 1 if e.problem_mark else 1,
                "error",
                f"YAML constructor error: {e.problem or str(e)}",
            )
        )
    except yaml.YAMLError as e:
        line = 1
        column = 1
        if hasattr(e, "problem_mark") and e.problem_mark:
            line = e.problem_mark.line + 1
            column = e.problem_mark.column + 1
        errors.append(YAMLLintError(line, column, "error", f"YAML error: {str(e)}"))

    return errors


def check_yaml_syntax(content):
    """Check YAML syntax, using PyYAML if available."""
    assert isinstance(content, str)

    if HAS_PYYAML:
        return check_yaml_syntax_pyyaml(content)
    else:
        return check_yaml_syntax_basic(content)


def check_indentation(lines):
    """Check for indentation issues."""
    assert isinstance(lines, list)
    assert all(isinstance(line, str) for line in lines)

    errors = []
    for line_num, line in enumerate(lines, 1):
        # Skip empty lines and comments.
        if not line.strip() or line.lstrip().startswith("#"):
            continue

        # Check for tabs.
        if "\t" in line:
            col = line.index("\t") + 1
            errors.append(
                YAMLLintError(
                    line_num,
                    col,
                    "error",
                    "Tab character found (use spaces for indentation)",
                )
            )

        # Check for trailing whitespace.
        if line.rstrip() != line:
            errors.append(
                YAMLLintError(
                    line_num, len(line.rstrip()) + 1, "note", "Trailing whitespace"
                )
            )

        # Detect consistent indentation.
        stripped = line.lstrip()
        indent = len(line) - len(stripped)

        # Simple check: indentation should be multiple of 2.
        if indent > 0 and indent % 2 != 0:
            errors.append(
                YAMLLintError(
                    line_num, 1, "warning", "Odd indentation (use 2 or 4 spaces)"
                )
            )

    return errors


def check_line_issues(lines):
    """Check for various line-level issues."""
    assert isinstance(lines, list)
    assert all(isinstance(line, str) for line in lines)

    errors = []

    for line_num, line in enumerate(lines, 1):
        # Check line length.
        if len(line) > 120:
            errors.append(
                YAMLLintError(
                    line_num,
                    121,
                    "note",
                    f"Line too long ({len(line)} > 120 characters)",
                )
            )

        # Check for Windows line endings.
        if line.endswith("\r"):
            errors.append(
                YAMLLintError(
                    line_num,
                    len(line),
                    "warning",
                    "Windows line ending (\\r\\n) detected",
                )
            )

        # Check for non-printable characters.
        for i, char in enumerate(line):
            if ord(char) < 32 and char not in ("\n", "\t", "\r"):
                errors.append(
                    YAMLLintError(
                        line_num,
                        i + 1,
                        "warning",
                        f"Non-printable character (ASCII {ord(char)})",
                    )
                )

    return errors


def check_problematic_strings(lines):
    """Check for strings that might cause issues."""
    assert isinstance(lines, list)
    assert all(isinstance(line, str) for line in lines)

    errors = []

    # Patterns for boolean-like values.
    bool_pattern = re.compile(r"^(yes|no|true|false|on|off|y|n)$", re.IGNORECASE)
    # Pattern for number-like values.
    number_pattern = re.compile(r"^-?\d+(\.\d+)?([eE][+-]?\d+)?$")

    for line_num, line in enumerate(lines, 1):
        # Skip empty lines and comments.
        if not line.strip() or line.lstrip().startswith("#"):
            continue

        # Simple check for unquoted values after colon.
        match = re.match(r"^(\s*)(.*?):\s*(.+?)(\s*#.*)?$", line)
        if match:
            indent, key, value, comment = match.groups()
            value = value.strip()

            # Check if value is unquoted (doesn't start with quote).
            if value and not value.startswith(("'", '"', "[", "{", "|", ">")):
                # Check for boolean-like values.
                if bool_pattern.match(value):
                    col = len(indent) + len(key) + 2  # After ": ".
                    errors.append(
                        YAMLLintError(
                            line_num,
                            col,
                            "warning",
                            f"Unquoted '{value}' looks like a boolean",
                        )
                    )

                # Check for number-like values.
                elif number_pattern.match(value):
                    col = len(indent) + len(key) + 2
                    errors.append(
                        YAMLLintError(
                            line_num,
                            col,
                            "warning",
                            f"Unquoted '{value}' looks like a number",
                        )
                    )

                # Check for colons in unquoted strings.
                elif ":" in value:
                    colon_pos = value.index(":")
                    col = len(indent) + len(key) + 2 + colon_pos
                    errors.append(
                        YAMLLintError(
                            line_num,
                            col,
                            "warning",
                            "Colon in unquoted string (consider quoting)",
                        )
                    )

        # Check for key without value.
        if line.rstrip().endswith(":") and not line.lstrip().startswith("#"):
            errors.append(
                YAMLLintError(
                    line_num, len(line.rstrip()), "warning", "Key without value"
                )
            )

    return errors


def check_duplicate_keys(content):
    """Check for duplicate keys using regex pattern matching."""
    assert isinstance(content, str)

    errors = []
    lines = content.split("\n")

    # Track keys at each indentation level.
    key_stack = [{}]  # Stack of dicts, each dict tracks keys at that level.
    current_indent = 0

    for line_num, line in enumerate(lines, 1):
        # Skip empty lines and comments.
        if not line.strip() or line.lstrip().startswith("#"):
            continue

        # Match key-value pairs.
        match = re.match(r"^(\s*)([\w\-\.]+):\s*(.*)$", line)
        if match:
            indent_str, key, value = match.groups()
            indent = len(indent_str)

            # Adjust key stack based on indentation.
            if indent > current_indent:
                key_stack.append({})
            elif indent < current_indent:
                # Pop stacks until we reach the right level.
                while len(key_stack) > 1 and indent < current_indent:
                    key_stack.pop()
                    current_indent -= 2  # Assume 2-space indent.
                if not key_stack:
                    key_stack = [{}]

            current_indent = indent

            # Check for duplicate key at current level.
            current_level = key_stack[-1] if key_stack else {}
            if key in current_level:
                errors.append(
                    YAMLLintError(
                        line_num,
                        len(indent_str) + 1,
                        "warning",
                        f"Duplicate key '{key}' (first defined at line {current_level[key]})",
                    )
                )
            else:
                current_level[key] = line_num

    return errors


def check_anchors_and_aliases(lines):
    """Check for anchor and alias issues."""
    assert isinstance(lines, list)
    assert all(isinstance(line, str) for line in lines)

    errors = []
    anchors = {}
    aliases = set()

    # Pattern to find anchors (&name) and aliases (*name),
    anchor_pattern = re.compile(r"&(\w+)")
    alias_pattern = re.compile(r"\*(\w+)")

    for line_num, line in enumerate(lines, 1):
        # Skip comments.
        if line.lstrip().startswith("#"):
            continue

        # Find anchors.
        for match in anchor_pattern.finditer(line):
            anchor_name = match.group(1)
            col = match.start() + 1

            if anchor_name in anchors:
                errors.append(
                    YAMLLintError(
                        line_num,
                        col,
                        "error",
                        f"Duplicate anchor '{anchor_name}' (first defined at line {anchors[anchor_name][0]})",
                    )
                )
            else:
                anchors[anchor_name] = (line_num, col)

        # Find aliases.
        for match in alias_pattern.finditer(line):
            alias_name = match.group(1)
            aliases.add(alias_name)

    # Check for undefined aliases.
    for alias in aliases:
        if alias not in anchors:
            # Find where the alias is used to report error.
            for line_num, line in enumerate(lines, 1):
                match = re.search(rf"\*{alias}\b", line)
                if match:
                    errors.append(
                        YAMLLintError(
                            line_num,
                            match.start() + 1,
                            "error",
                            f"Undefined alias '{alias}'",
                        )
                    )

    # Check for unused anchors.
    for anchor, (line_num, col) in anchors.items():
        if anchor not in aliases:
            errors.append(
                YAMLLintError(line_num, col, "warning", f"Unused anchor '{anchor}'")
            )

    return errors


def check_document_structure(lines):
    """Check document markers and structure."""
    assert isinstance(lines, list)
    assert all(isinstance(line, str) for line in lines)

    errors = []
    doc_starts = []
    doc_ends = []
    has_content_before_marker = False

    for line_num, line in enumerate(lines, 1):
        stripped = line.strip()

        # Check for document markers.
        if stripped == "---":
            doc_starts.append(line_num)
        elif stripped == "...":
            doc_ends.append(line_num)
        elif stripped and not stripped.startswith("#") and not doc_starts:
            has_content_before_marker = True

    # Note about missing document start (only if there's content).
    if has_content_before_marker and not doc_starts:
        errors.append(
            YAMLLintError(1, 1, "note", "Missing document start marker (---)")
        )

    # Check for multiple documents.
    if len(doc_starts) > 1:
        errors.append(
            YAMLLintError(
                doc_starts[1],
                1,
                "note",
                "Multiple documents found (this may be intentional)",
            )
        )

    return errors


def lint_yaml_file(filename):
    """Lint a YAML file and return list of errors/warnings."""
    assert isinstance(filename, str)

    # Check if file exists.
    if not os.path.exists(filename):
        return [YAMLLintError(1, 1, "error", f"File not found: {filename}")]

    # Read file content.
    try:
        with open(filename, "r", encoding="utf-8") as f:
            content = f.read()
    except UnicodeDecodeError:
        return [YAMLLintError(1, 1, "error", "File is not valid UTF-8")]
    except Exception as e:
        return [YAMLLintError(1, 1, "error", f"Failed to read file: {str(e)}")]

    # Check for empty file.
    if not content.strip():
        return [YAMLLintError(1, 1, "warning", "Empty YAML file")]

    lines = content.split("\n")
    errors = []

    # Run all checks.
    errors.extend(check_yaml_syntax(content))
    errors.extend(check_indentation(lines))
    errors.extend(check_line_issues(lines))
    errors.extend(check_problematic_strings(lines))
    errors.extend(check_duplicate_keys(content))
    errors.extend(check_anchors_and_aliases(lines))
    errors.extend(check_document_structure(lines))

    # Remove duplicates.
    unique_errors = list(set(errors))

    # Sort by line and column.
    unique_errors.sort(key=lambda e: (e.line, e.column, e.level))

    return unique_errors


def main():
    """Main entry point."""
    if len(sys.argv) != 2:
        print("Usage: yaml_lint.py <yaml-file>", file=sys.stderr)
        sys.exit(1)

    filename = sys.argv[1]
    errors = lint_yaml_file(filename)

    # Output errors in flymake format.
    for error in errors:
        print(error.__str__(filename))

    # Exit with non-zero status if there were errors.
    sys.exit(1 if any(e.level == "error" for e in errors) else 0)


if __name__ == "__main__":
    main()
