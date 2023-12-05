---
title: "Advent of Code in R"
description: "Let's see how far I can take this"
author: "Renata Hirota"
date: "2023-12-03"
categories: 
  - advent of code
  - R
draft: false
---

![Photo by <a href="https://unsplash.com/@markusspiske?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash">Markus Spiske</a> on <a href="https://unsplash.com/photos/closeup-photo-of-red-ball-ornament-on-surface-AF_4tBQjdtc?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash">Unsplash</a>
  ](xmas.webp){fig-alt="Red ball ornament" .preview-image}

It's _that_ time of the year again! 🎄

I learned about [Advent of Code](https://adventofcode.com/2023/about) a couple
of years ago, when [Caio](https://github.com/clente) challenged himself to
complete every puzzle and post about it.

> Advent of Code is an Advent calendar of small programming puzzles for a
variety of skill sets and skill levels that can be solved in any programming
language you like. People use them as interview prep, company training,
university coursework, practice problems, a speed contest, or to challenge each
other.

I've tried to solve some of them, but this time of the year is usually so hectic
that I don't think I ever got past day 6. Maybe this is the year I get to the
second week? (Nevermind that I'm already late.)

I still have a full week ahead of exams and papers, so I'll probably take it
slow, but I wanted to lay the groundwork. I'm starting by installing
[{aor}](https://github.com/clente/aor), which is a neat R package with some
useful functions to help you with Advent of Code, so you can focus on actually
solving the puzzles.

```
# install.packages("devtools")
devtools::install_github("clente/aor")
```

Once installed and with the right cookie configurations (all explained in
`aor`'s readme), I can simply run

```
> aor::day_start("2023-12-01", "aoc2023/")
✔ Fetched puzzle.
✔ Fetched input.
✔ Created directory aoc2023/01_trebuchet
✔ Wrote part 1 to aoc2023/01_trebuchet/puzzle.R                                                                                        
✔ Wrote input to aoc2023/01_trebuchet/input.txt                                                                                        
ℹ To fetch part 2, run `aor::day_continue("2023-12-01", "aoc2023/01_trebuchet/puzzle.R")`
```

...and I'll have a directory for that day's puzzle, with a template for the code
and the input text! It's important to note that you must be logged in to get the
puzzle input, as they are different across users.

Next step is solving the puzzles. I'm starting with day one.

## Day 1: Trebuchet?!

To sum things up, the first part of day 1 is:

- Read a file with text
- Identify the first and last numbers on each line (the "calibration") and sum
  them up

Every puzzle comes with a minimal example:

> For example:  
    1abc2  
    pqr3stu8vwx  
    a1b2c3d4e5f  
    treb7uchet  
In this example, the calibration values of these four lines are `12`,
`38`, `15`, and `77`. Adding these together produces *`142`*.


There are many ways this can be done, but I ended up using `dplyr` because I'm
more used to it. With a little bit of regex, it was easy enough to extract the
numbers that I needed to clear part 1.


::: {.callout-tip appearance="simple" collapse="true"}
### Answer for part 1, Day 1 -- click to see my solution

```
input <- "aoc2023/01_trebuchet/input.txt"
input |>
  readr::read_csv(col_names = "input", show_col_types = FALSE) |>
  dplyr::filter(input != "") |>
  dplyr::mutate(
    first = stringr::str_extract(input, "[0-9]"),
    last = stringr::str_extract(input, "[0-9](?!.*[0-9])"),
    calibration = as.numeric(paste0(first, last))
  ) |>
  dplyr::pull(calibration) |>
  sum()
  ```
:::

Part 2 was a little bit trickier. The puzzle says:

> Your calculation isn't quite right. It looks like some of the digits are
actually *spelled out with letters*: `one`, `two`, `three`, `four`,
`five`, `six`, `seven`, `eight`, and `nine` *also* count as valid
"digits".
Equipped with this new information, you now need to find the real first
and last digit on each line. For example:  
    two1nine  
    eightwothree  
    abcone2threexyz  
    xtwone3four  
    4nineeightseven2  
    zoneight234  
    7pqrstsixteen  
In this example, the calibration values are `29`, `83`, `13`, `24`,
`42`, `14`, and `76`. Adding these together produces *`281`*.

::: {.callout-tip appearance="simple" collapse="true"}
### First try for part 2, Day 1 -- click to see my failed attempt

My first idea was using regex to get all ocurrences of numbers _and_ spelled out
numbers. So my regex would look something like

```
rx <- ("[0-9]|one|two|three|four|five|six|seven|eight|nine")
```

Then, I could switch the spelled out numbers, paste the first and last ones and sum them up.

```
switch_numbers <- function(num) {
  if (stringr::str_detect(num, "[a-z]")) {
    result <- switch(
      num, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7,
      eight = 8, nine = 9
    )
  } else {
    result <- num
  }
  as.numeric(result)
}

input |>
  readr::read_csv(col_names = "input", show_col_types = FALSE) |>
  dplyr::filter(input != "") |>
  dplyr::mutate(
    numbers = stringr::str_extract_all(input, rx),
    first = purrr::map_vec(numbers, head, n = 1),
    last = purrr::map_vec(numbers, tail, n = 1),
    first = purrr::map_vec(first, switch_numbers),
    last = purrr::map_vec(last, switch_numbers),
    calibration = as.numeric(paste0(first, last))
  )|>
  dplyr::pull(calibration) |>
  sum()
```

*But*, turns out I was wrong. You can see the problem with `str_extract_all` in this case:

```
> stringr::str_extract_all("threeight", rx)
[[1]]
[1] "three"
```
What I actually wanted:
```
[[1]]
[1] "three"  [2] "eight"
```

The regex I was using does not take into account overlapping!

:::

This takes us to attempt #2, where I try to take this problem into account with `stringi`.

::: {.callout-tip appearance="simple" collapse="true"}
### Second try for part 2, Day 1 -- click to see my solution

```
rx <- paste0("(?=([0-9]|", paste(xfun::n2w(1:9), collapse = "|"), "))")

input |>
  readr::read_csv(col_names = "input", show_col_types = FALSE) |>
  dplyr::filter(input != "") |>
  dplyr::mutate(
    numbers = stringi::stri_match_all_regex(input, rx),
    numbers = purrr::map(numbers, ~magrittr::extract(.x, ,2)),
    first = purrr::map_vec(numbers, head, n = 1),
    last = purrr::map_vec(numbers, tail, n = 1),
    first = purrr::map_vec(first, switch_numbers),
    last = purrr::map_vec(last, switch_numbers),
    calibration = as.numeric(paste0(first, last))
  ) |>
  dplyr::pull(calibration) |>
  sum()
```

Using a lookahead in the regex (?=) and `stringi::stri_match_all_regex` did the
trick and got me to the right answer! 🥳

:::

