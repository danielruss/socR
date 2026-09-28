# Find sibling codes within a hierarchical coding system

Given a code from a hierarchical coding system (e.g. NOC or SOC),
returns all other codes that share the same immediate parent. If the
target code has no siblings at its own level (i.e. it is an "only
child"), the function falls back to returning first cousins – codes at
the same level that share the same grandparent instead.

## Usage

``` r
siblings(target_code, system)
```

## Arguments

- target_code:

  A character string giving the code to find siblings for. Must be a
  valid code within `system`.

- system:

  A `codingsystem` object (as validated by `is.codingsystem`) containing
  a `table` element with, at minimum, `code` and `parent` columns.

## Value

A character vector of sibling (or, failing that, first-cousin) codes.
Returns `character(0)` if `target_code` has no parent, or if it has no
parent and no grandparent from which cousins could be derived.

## Details

The search proceeds in two steps:

1.  **Siblings**: codes sharing `target_code`'s immediate parent
    (excluding `target_code` itself).

2.  **Cousins**: if no siblings are found, codes sharing a parent with
    `target_code`'s parent (i.e. sharing a grandparent), excluding the
    parent itself. Because these are children of the parent's own
    siblings, they are automatically at the same hierarchical level as
    `target_code`.

The function does not climb beyond the grandparent level; if no siblings
or cousins are found there, it returns `character(0)`.

## Examples

``` r
if (FALSE) { # \dontrun{
siblings("0013", noc2011_all)  # same-parent siblings
siblings("0311", noc2011_all)  # falls back to cousins, since 0311
                                # is an only child under its parent
} # }
```
