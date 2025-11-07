# Test Markdown Emphasis Patterns

## Snake_case test (should NOT be italicized):
- some_variable_name
- another_test_case
- file_name_here

## Word boundary test (should NOT be italicized):
- file*name*here should not highlight "name"
- test_underscore_word should not highlight "underscore"

## Valid emphasis (should be highlighted):
- *italic text*
- **bold text**
- ***bold italic text***
- _italic with underscores_
- __bold with underscores__
- ___bold italic with underscores___

## Edge cases:
- Start of line: *italic*
- End of line: text *italic*
- Multiple: *first* and *second* italic
- Mixed: **bold** and *italic* together
- Complex: ***bold italic*** with **bold** and *italic*

## Code context (should not affect):
```
some_variable_name = "test"
file*glob*pattern
```

## Indented Code Block Tests:

Valid code blocks (should be highlighted as code):
    def hello():
        return "world"

    x = 42
    print(x)

NOT code blocks (should NOT be highlighted as code):
- List item 1
    - Nested list item (indented but still a list)
    - Another nested item

1. Ordered list item
    1. Nested ordered item (indented but still a list)

> Blockquote text
    > Continued blockquote (indented but still a quote)

Normal text:
    This is just indented text in a paragraph
    and should not be treated as code.

Mixed context:
- List with code:

        def in_list():
            return "code in list context"

> Quote with code:

        def in_quote():
            return "code in quote context"

## Setext Header Tests:

Valid setext headers (should be highlighted):
This is a Level 1 Header
=========================

This is a Level 2 Header
-------------------------

Another Level 1
===============

Another Level 2
---------------

NOT headers (should NOT be highlighted as headers):
=========================
-------------------------
================
--------
===
---

Horizontal rule examples:
---
***
___

Mixed content:
=========================
Some text here but this line above isn't a header
=========================

Normal paragraph.
This has dashes below but shouldn't be a header since
there are multiple lines above.
-----------

## Strikethrough Tests:

Valid strikethrough (should be highlighted):
- This is ~~strikethrough text~~ in a sentence
- ~~Entire phrase struck through~~
- Mix with other: **bold** and *italic* and ~~strikethrough~~
- ~~Strike~~ and **bold** and *italic*

NOT strikethrough (should NOT be highlighted):
- This has single ~ tilde
- This has ~~~ three tildes ~~~
- This has ~two different~ but not double tildes
- Code with tildes: `~~not strikethrough~~`

Edge cases:
- Start: ~~strikethrough at start~~
- End: text with ~~strikethrough at end~~
- Multiple: ~~first~~ and ~~second~~ strikethrough
- Complex: ***~~bold italic strikethrough~~***

## Task List Tests:

Valid task lists (should be highlighted):
- [ ] Unchecked task item
- [x] Checked task item
- [X] Checked task item (capital)
* [ ] Task with asterisk marker
* [x] Checked with asterisk
+ [ ] Task with plus marker
+ [X] Checked with plus marker

Mixed lists:
- [ ] Task item
- Regular list item (should highlight differently)
- [x] Another task item

Nested task lists:
- [ ] Parent task
  - [ ] Nested unchecked task
  - [x] Nested checked task
    - [ ] Deeply nested task

NOT task lists (should be regular list items):
- [not a checkbox]
- [ not a checkbox]
- [x not a checkbox]
- This is just text with [x] checkbox-like text

Task lists with other markdown:
- [ ] Task with **bold text**
- [x] Task with *italic text*
- [ ] Task with `inline code`
- [x] Task with ~~strikethrough~~

## Image Tests:

Valid images (should be highlighted):
![Alt text](https://example.com/image.png)
![Another image](image.jpg)
![Image with spaces in alt](path/to/image.gif)
![](no-alt-text.png)

Reference-style images:
![Alt text][image-ref]
![Another reference image][img2]
![Empty reference][]

Image references:
[image-ref]: https://example.com/ref-image.png
[img2]: local-image.jpg
[Empty reference]: default.png

Images vs Links comparison:
![This is an image](image.png)
[This is a link](link.html)

NOT images (should be regular links):
[Not an image](file.png)
[Another link](https://example.com)

Mixed content:
- ![Inline image](thumb.jpg) in a list
- Link: [Regular link](page.html)
> ![Image in blockquote](quote-image.png)

Complex examples:
[![Image link](logo.png)](https://example.com)
![Image with **bold** text in alt](image.png)
