---
author: Fang Lungang
created: 2026-09-04
updated: 2026-09-07 +1000
---

# Markdown Example file

Markdown is not a single language. The original Markdown (2004) left many cases undefined, so implementations diverged. The variants worth knowing are:
- CommonMark: the strict specification most modern tools implement
- GitHub Flavoured Markdown (GFM): CommonMark plus tables, task lists, strikethrough and autolinks

This file is used to test how Emacs Markdown modes render Markdown files. It is *not* an exhaustive tour of either spec, but a collection of the constructs that I encounter most often.

## Inlines

Some frequently used inlines are: `code span`, ``backtick(`) in code span``, *emphasis*, **strong emphasis**, and ~~strikethrough~~ (a GFM extension).

Use a backslash (`\`) to escape special characters, for example, \*not emphasis\*.

### Links and autolinks

An autolink is a URI or email address in angle brackets that renders as a clickable link; GFM additionally links bare URLs.

- A link: [CommonMark](https://commonmark.org)
- An autolink: <https://spec.commonmark.org/>
- A GFM extended autolink: https://github.com/github/cmark-gfm

### Entity and numeric character references

An entity reference (`&lambda;`) names a character; a numeric character reference (`&#9881;`) gives its Unicode code point. Both render the character itself: &lambda; and &#9881;.

## Lists

### Bullet list

- A bullet list item
  + A *nested* item with some `inlines`
- Another item

### Ordered list

1. An ordered list item
2. Another one

### Task list (checkboxes)

- [ ] A task to be done
- [x] A completed task

## Block quotes

> "Markdown is intended to be as easy-to-read and easy-to-write as is
> feasible." — John Gruber

## Fenced code block

```elisp
(use-package markdown-ts-mode
  :mode "\\.md\\'"
  :custom (markdown-ts-hide-markup t)
  :hook (markdown-ts-mode . visual-line-mode))
```

## Tables

|     Construct | CommonMark | GFM |
|--------------:|:----------:|:---:|
|        Tables |     ✗      |  ✓  |
|    Task lists |     ✗      |  ✓  |
| Strikethrough |     ✗      |  ✓  |

Column alignment is optional, controlled by colons in the delimiter row: `:---` left, `:---:` centre, `---:` right. A column without colons defaults to left alignment.

Setext heading
---

A setext heading is a heading written as a line of text followed immediately by a row of `=` (level 1) or `-` (level 2) characters.

The name comes from Setext ("structure enhanced text"), the predecessor format that Markdown borrowed this syntax from.

This format is useful where `#` cannot serve as a heading marker, for example, in a git commit message that later populates a PR description. Git strips lines beginning with `#` as comments, so ATX-style headings (`# Heading`) never make it into commit messages, while setext headings do.

## Thematic breaks

A standalone line of three or more `-`, `_` or `*` between blank lines is a
thematic break (horizontal rule).

---

## Math expressions

Install MacTeX (`brew install --cask mactex-no-gui`) to preview LaTeX fragments in the buffer.

### Schoolkid math: Can the water heater start?

A gas water heater needs a startup pressure of $P_{\text{s}} = 0.03 \text{MPa}$ and is fed from a rooftop tank. Find the minimum height difference between the water surface in the tank and the water outlet of the heater for the heater to be able to start.

1. Hydrostatic pressure, rearranged for $h$:

   $$P = \rho \cdot h \cdot g \quad \Rightarrow \quad h = \frac{P}{\rho \cdot g}$$

2. Substitute the values: startup pressure $P_{\text{s}} = 0.03 \text{MPa} = 30{,}000 \text{Pa} = 30{,}000 \text{kg}/(\text{m} \cdot \text{s}^2)$, the density of water $\rho = 1000 \text{kg/m}^3$:

   $$h = \frac{30{,}000 \text{kg}/(\text{m} \cdot \text{s}^2)}{1000 \text{kg/m}^3 \cdot g}$$

3. Substitute $g \approx 9.8 \text{m/s}^2$ and cancel the units: $\text{kg}$ and $\text{s}^2$ cancel; $\text{m}^2/\text{m}$ leaves $\text{m}$:

   $$h = \frac{30{,}000}{1000 \cdot 9.8} \text{m} \approx 3.1 \text{m}$$

---

<!--
Local Variables:
time-stamp-pattern: "8/updated:[ \t]+%Y-%02m-%02d %5z$"
End:
-->
