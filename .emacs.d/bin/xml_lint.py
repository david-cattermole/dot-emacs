#!/usr/bin/env python3
"""
xml_lint.py - XML linter for Emacs flymake integration.

This script validates XML files and outputs diagnostics in a format
compatible with Emacs flymake:
  filename:line:column: level: message

Usage:
  python3 xml_lint.py <xml-file>

Requires:
  - Python 3.x (uses built-in xml.etree.ElementTree only)
"""

import sys
import os
import re
import xml.etree.ElementTree as ET
from xml.parsers.expat import ExpatError


class XMLLintError:
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
            isinstance(other, XMLLintError)
            and self.line == other.line
            and self.column == other.column
            and self.level == other.level
            and self.message == other.message
        )

    def __hash__(self):
        return hash((self.line, self.column, self.level, self.message))


def check_xml_encoding(content):
    """Check XML encoding declaration and detect potential encoding issues."""
    assert isinstance(content, str)

    errors = []
    lines = content.split("\n")

    if not lines:
        return errors

    first_line = lines[0]

    # Check for BOM (Byte Order Mark).
    if first_line.startswith("\ufeff"):
        errors.append(
            XMLLintError(
                1, 1, "warning", "BOM (Byte Order Mark) found at start of file"
            )
        )
        first_line = first_line[1:]  # Remove BOM for further processing.

    # Check XML declaration format.
    xml_decl_pattern = re.compile(
        r'^\s*<\?xml\s+version\s*=\s*["\']([^"\']*)["\'](?:\s+encoding\s*=\s*["\']([^"\']*)["\'])?(?:\s+standalone\s*=\s*["\']([^"\']*)["\'])?\s*\?>'
    )

    match = xml_decl_pattern.match(first_line)
    if first_line.strip().startswith("<?xml"):
        if not match:
            errors.append(XMLLintError(1, 1, "error", "Malformed XML declaration"))
        else:
            version, encoding, standalone = match.groups()

            # Check version.
            if version not in ["1.0", "1.1"]:
                errors.append(
                    XMLLintError(
                        1,
                        first_line.find(version) + 1,
                        "warning",
                        f"Unusual XML version '{version}' (expected '1.0' or '1.1')",
                    )
                )

            # Check encoding.
            if encoding:
                common_encodings = ["utf-8", "utf-16", "iso-8859-1", "windows-1252"]
                if encoding.lower() not in common_encodings:
                    col = first_line.find(encoding) + 1
                    errors.append(
                        XMLLintError(
                            1,
                            col,
                            "note",
                            f"Uncommon encoding '{encoding}' specified",
                        )
                    )

            # Check standalone.
            if standalone and standalone.lower() not in ["yes", "no"]:
                col = first_line.find(standalone) + 1
                errors.append(
                    XMLLintError(
                        1,
                        col,
                        "error",
                        f"Invalid standalone value '{standalone}' (must be 'yes' or 'no')",
                    )
                )

    # Check for non-ASCII characters in XML declaration line.
    if any(ord(char) > 127 for char in first_line):
        for i, char in enumerate(first_line):
            if ord(char) > 127:
                errors.append(
                    XMLLintError(
                        1,
                        i + 1,
                        "warning",
                        f"Non-ASCII character in XML declaration (U+{ord(char):04X})",
                    )
                )
                break  # Report only the first one.

    return errors


def check_xml_structure_basic(content):
    """Basic XML structure checking without full parsing."""
    assert isinstance(content, str)

    errors = []
    lines = content.split("\n")

    # Track some basic XML structure issues.
    tag_stack = []
    in_comment = False
    in_cdata = False

    for line_num, line in enumerate(lines, 1):
        i = 0
        while i < len(line):
            char = line[i]

            # Handle CDATA sections.
            if not in_comment and line[i : i + 9] == "<![CDATA[":
                in_cdata = True
                i += 9
                continue
            elif in_cdata and line[i : i + 3] == "]]>":
                in_cdata = False
                i += 3
                continue
            elif in_cdata:
                i += 1
                continue

            # Handle comments.
            if not in_cdata and line[i : i + 4] == "<!--":
                in_comment = True
                i += 4
                continue
            elif in_comment and line[i : i + 3] == "-->":
                in_comment = False
                i += 3
                continue
            elif in_comment:
                i += 1
                continue

            # Skip if we're inside comment or CDATA.
            if in_comment or in_cdata:
                i += 1
                continue

            # Look for tags.
            if char == "<":
                # Find the end of the tag.
                tag_end = line.find(">", i)
                if tag_end == -1:
                    # Tag continues on next line or is malformed.
                    i += 1
                    continue

                tag_content = line[i + 1 : tag_end]

                # Skip processing instructions and DOCTYPE.
                if tag_content.startswith("?") or tag_content.startswith("!"):
                    i = tag_end + 1
                    continue

                # Check for empty tag.
                if not tag_content.strip():
                    errors.append(XMLLintError(line_num, i + 1, "error", "Empty tag"))
                    i = tag_end + 1
                    continue

                # Self-closing tag.
                if tag_content.endswith("/"):
                    i = tag_end + 1
                    continue

                # Closing tag.
                if tag_content.startswith("/"):
                    tag_name = tag_content[1:].split()[0]
                    if not tag_stack:
                        errors.append(
                            XMLLintError(
                                line_num,
                                i + 1,
                                "error",
                                f"Unexpected closing tag '{tag_name}'",
                            )
                        )
                    else:
                        expected = tag_stack.pop()
                        if tag_name != expected:
                            errors.append(
                                XMLLintError(
                                    line_num,
                                    i + 1,
                                    "error",
                                    f"Mismatched closing tag: expected '{expected}' but got '{tag_name}'",
                                )
                            )
                else:
                    # Opening tag.
                    tag_name = tag_content.split()[0]
                    tag_stack.append(tag_name)

                i = tag_end + 1
            else:
                i += 1

    # Check for unclosed tags.
    for tag in tag_stack:
        errors.append(XMLLintError(len(lines), 1, "error", f"Unclosed tag '{tag}'"))

    return errors


def check_xml_syntax(content):
    """Check XML syntax using Python's built-in xml.etree.ElementTree."""
    assert isinstance(content, str)

    errors = []

    try:
        # Try to parse the XML.
        ET.fromstring(content)
    except ET.ParseError as e:
        # Extract line and column from the error message.
        line = 1
        column = 1

        error_msg = str(e)
        # Parse error messages like "not well-formed (invalid token): line 5, column 10"
        line_match = re.search(r"line (\d+)", error_msg)
        col_match = re.search(r"column (\d+)", error_msg)

        if line_match:
            line = int(line_match.group(1))
        if col_match:
            column = int(col_match.group(1))

        # Clean up the error message.
        clean_msg = re.sub(r":\s*line \d+,?\s*column \d+", "", error_msg)
        clean_msg = clean_msg.strip()

        errors.append(
            XMLLintError(line, column, "error", f"XML parse error: {clean_msg}")
        )

    except ExpatError as e:
        # Expat errors from xml.etree.ElementTree.
        line = getattr(e, "lineno", 1)
        column = getattr(e, "offset", 1)
        message = str(e)

        errors.append(
            XMLLintError(line, column, "error", f"XML syntax error: {message}")
        )

    except Exception as e:
        errors.append(
            XMLLintError(1, 1, "error", f"Unexpected XML parsing error: {str(e)}")
        )

    return errors


def check_xml_characters(content):
    """Check for problematic characters in XML content."""
    assert isinstance(content, str)

    errors = []
    lines = content.split("\n")

    for line_num, line in enumerate(lines, 1):
        for i, char in enumerate(line):
            code = ord(char)

            # Check for invalid XML characters.
            # Valid XML characters: #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
            if not (
                code == 0x09  # Tab
                or code == 0x0A  # LF
                or code == 0x0D  # CR
                or (0x20 <= code <= 0xD7FF)
                or (0xE000 <= code <= 0xFFFD)
                or (0x10000 <= code <= 0x10FFFF)
            ):
                errors.append(
                    XMLLintError(
                        line_num,
                        i + 1,
                        "error",
                        f"Invalid XML character (U+{code:04X})",
                    )
                )

            # Check for control characters (except tab, LF, CR).
            elif 0x00 <= code <= 0x1F and code not in [0x09, 0x0A, 0x0D]:
                errors.append(
                    XMLLintError(
                        line_num,
                        i + 1,
                        "warning",
                        f"Control character in XML (U+{code:04X})",
                    )
                )

    return errors


def check_xml_namespaces(content):
    """Check for basic namespace issues."""
    assert isinstance(content, str)

    errors = []
    lines = content.split("\n")

    # Look for namespace declarations and usage.
    xmlns_pattern = re.compile(r'xmlns(?::(\w+))?\s*=\s*["\']([^"\']*)["\']')
    namespace_usage_pattern = re.compile(r"</?(\w+):(\w+)")

    declared_prefixes = set()
    used_prefixes = set()

    for line_num, line in enumerate(lines, 1):
        # Find namespace declarations.
        for match in xmlns_pattern.finditer(line):
            prefix = match.group(1)  # None for default namespace.
            uri = match.group(2)

            if prefix:
                declared_prefixes.add(prefix)

            # Check for empty namespace URI (usually an error).
            if not uri.strip():
                col = match.start() + 1
                errors.append(
                    XMLLintError(line_num, col, "warning", "Empty namespace URI")
                )

        # Find namespace prefix usage.
        for match in namespace_usage_pattern.finditer(line):
            prefix = match.group(1)
            used_prefixes.add(prefix)

    # Check for used but undeclared prefixes.
    undeclared = used_prefixes - declared_prefixes
    for prefix in undeclared:
        # Find first usage to report location.
        for line_num, line in enumerate(lines, 1):
            if f"{prefix}:" in line:
                col = line.find(f"{prefix}:") + 1
                errors.append(
                    XMLLintError(
                        line_num,
                        col,
                        "error",
                        f"Undeclared namespace prefix '{prefix}'",
                    )
                )
                break

    # Check for declared but unused prefixes.
    unused = declared_prefixes - used_prefixes
    for prefix in unused:
        # Find declaration to report location.
        for line_num, line in enumerate(lines, 1):
            if f"xmlns:{prefix}" in line:
                col = line.find(f"xmlns:{prefix}") + 1
                errors.append(
                    XMLLintError(
                        line_num, col, "note", f"Unused namespace prefix '{prefix}'"
                    )
                )
                break

    return errors


def check_xml_formatting(lines):
    """Check XML formatting and style issues."""
    assert isinstance(lines, list)
    assert all(isinstance(line, str) for line in lines)

    errors = []

    for line_num, line in enumerate(lines, 1):
        # Check for tabs.
        if "\t" in line:
            col = line.index("\t") + 1
            errors.append(
                XMLLintError(
                    line_num,
                    col,
                    "note",
                    "Tab character found (consider using spaces for indentation)",
                )
            )

        # Check for trailing whitespace.
        if line.rstrip() != line:
            errors.append(
                XMLLintError(
                    line_num,
                    len(line.rstrip()) + 1,
                    "note",
                    "Trailing whitespace",
                )
            )

        # Check line length.
        if len(line) > 120:
            errors.append(
                XMLLintError(
                    line_num,
                    121,
                    "note",
                    f"Line too long ({len(line)} > 120 characters)",
                )
            )

        # Check for Windows line endings.
        if line.endswith("\r"):
            errors.append(
                XMLLintError(
                    line_num,
                    len(line),
                    "warning",
                    "Windows line ending (\\r\\n) detected",
                )
            )

    return errors


def lint_xml_file(filename):
    """Lint an XML file and return list of errors/warnings."""
    assert isinstance(filename, str)

    # Check if file exists.
    if not os.path.exists(filename):
        return [XMLLintError(1, 1, "error", f"File not found: {filename}")]

    # Read file content.
    try:
        with open(filename, "r", encoding="utf-8") as f:
            content = f.read()
    except UnicodeDecodeError:
        # Try with latin-1 encoding as fallback.
        try:
            with open(filename, "r", encoding="latin-1") as f:
                content = f.read()
            return [
                XMLLintError(1, 1, "warning", "File is not valid UTF-8, using latin-1")
            ]
        except Exception:
            return [XMLLintError(1, 1, "error", "File encoding cannot be determined")]
    except Exception as e:
        return [XMLLintError(1, 1, "error", f"Failed to read file: {str(e)}")]

    # Check for empty file.
    if not content.strip():
        return [XMLLintError(1, 1, "warning", "Empty XML file")]

    lines = content.split("\n")
    errors = []

    # Run all checks.
    errors.extend(check_xml_encoding(content))
    errors.extend(check_xml_syntax(content))
    errors.extend(check_xml_structure_basic(content))
    errors.extend(check_xml_characters(content))
    errors.extend(check_xml_namespaces(content))
    errors.extend(check_xml_formatting(lines))

    # Remove duplicates.
    unique_errors = list(set(errors))

    # Sort by line and column.
    unique_errors.sort(key=lambda e: (e.line, e.column, e.level))

    return unique_errors


def main():
    """Main entry point."""
    if len(sys.argv) != 2:
        print("Usage: xml_lint.py <xml-file>", file=sys.stderr)
        sys.exit(1)

    filename = sys.argv[1]
    errors = lint_xml_file(filename)

    # Output errors in flymake format.
    for error in errors:
        print(error.__str__(filename))

    # Exit with non-zero status if there were errors.
    sys.exit(1 if any(e.level == "error" for e in errors) else 0)


if __name__ == "__main__":
    main()
