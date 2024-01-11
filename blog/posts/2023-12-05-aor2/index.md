---
title: "Advent of Code: Day 2"
description: "Counting red, blue and green balls with elves"
author: "Renata Hirota"
date: "2023-12-05"
categories: 
  - advent of code
  - R
draft: false
---

![Photo by <a
href="https://unsplash.com/@melpoole?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash">Mel
Poole</a> on <a
href="https://unsplash.com/photos/two-poinsettia-wreathes-izPKjgCxGfM?utm_content=creditCopyText&utm_medium=referral&utm_source=unsplash">Unsplash</a>](xmas2.webp){fig-alt="Red
ball ornament" .preview-image}

If you haven't read the [previous
post](https://hirota.dev/posts/2023-12-03-aor/), I suggest you go back and do
it. It's got a little more context on [Advent of
Code](https://adventofcode.com/2023/about) and it's a great way to see
[`{aor}`](https://github.com/clente/aor) in action!

I know said I had exams and papers to write and I wasn't expecting to write
again so soon, but I was told Day 2 was easier than Day 1, so here I am! :)

## Day 2: Cube Conundrum

Let's start by fetching the puzzle and input for Day 2:

```r
> aor::day_start("2023-12-02", "aoc2023/")
✔ Fetched puzzle.
✔ Fetched input.
✔ Created directory aoc2023/02_cube_conundrum
✔ Wrote part 1 to aoc2023/02_cube_conundrum/puzzle.R                                                                                        
✔ Wrote input to aoc2023/02_cube_conundrum/input.txt                                                                                        
ℹ To fetch part 2, run `aor::day_continue("2023-12-02", "aoc2023/02_cube_conundrum/puzzle.R")`
```

Again, there are many ways to solve the puzzle, but I ended up using `dplyr` and
`tidyr`. Thankfully Day 2 was easier than Day 1; all you had to do was count
red, green and blue balls and check some given conditions.

For the first part of the puzzle, you had to find all posible games given a
number of balls (so you had to filter out games that exceeded that).

> For example, the record of a few games might look like this:  
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green  
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue  
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red  
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red  
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green  
In game 1, three sets of cubes are revealed from the bag (and then put back
again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red
cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes. The
Elf would first like to know which games would have been possible if the bag
contained *only 12 red cubes, 13 green cubes, and 14 blue cubes*?

::: {.callout-tip appearance="simple" collapse="true"}
### Answer for part 1, Day 2 -- click to see my solution

```r
input <- "aoc2023/02_cube_conundrum/input.txt"

input |>
  readr::read_delim(col_names = c("game_id", "sets"), show_col_types = FALSE) |>
  dplyr::mutate(
    game_id = as.numeric(stringr::str_extract(game_id, "[0-9]+")),
    sets = stringr::str_split(sets, ";")
  ) |>
  tidyr::unnest(sets) |>
  dplyr::mutate(
    green = as.numeric(stringr::str_extract(sets, "[0-9]+(?= green)")),
    blue = as.numeric(stringr::str_extract(sets, "[0-9]+(?= blue)")),
    red = as.numeric(stringr::str_extract(sets, "[0-9]+(?= red)"))
  ) |>
  tidyr::replace_na(list(green = 0, blue = 0, red = 0)) |>
  dplyr::group_by(game_id) |>
  dplyr::filter(red > 12 | green > 13 | blue > 14) |>
  dplyr::distinct(game_id, .keep_all = TRUE) |>
  dplyr::pull(game_id) |>
  setdiff(1:100, y = _) |>
  sum()
```
:::

For part 2, you had to find the mininum number of balls of each color for each
game.

> As you continue your walk, the Elf poses a second question: in each game you
played, what is the *fewest number of cubes of each color* that could have been
in the bag to make the game possible? Again consider the example games from
earlier:   
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green  
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue  
    In game 1, the game could have been played with as few as 4 red, 2
    green, and 6 blue cubes. If any color had even one fewer cube, the
    game would have been impossible.  
    Game 2 could have been played with a minimum of 1 red, 3 green, and
    4 blue cubes.

::: {.callout-tip appearance="simple" collapse="true"}
### Answer for part 2, Day 2 -- click to see my solution

```r
input |>
  readr::read_delim(col_names = c("game_id", "sets"), show_col_types = FALSE) |>
  dplyr::mutate(
    game_id = as.numeric(stringr::str_extract(game_id, "[0-9]+")),
    sets = stringr::str_split(sets, ";")
  ) |>
  tidyr::unnest(sets) |>
  dplyr::mutate(
    green = as.numeric(stringr::str_extract(sets, "[0-9]+(?= green)")),
    blue = as.numeric(stringr::str_extract(sets, "[0-9]+(?= blue)")),
    red = as.numeric(stringr::str_extract(sets, "[0-9]+(?= red)"))
  ) |>
  tidyr::replace_na(list(green = 0, blue = 0, red = 0)) |>
  dplyr::group_by(game_id) |>
  dplyr::summarise(max_red = max(red), max_green = max(green), max_blue = max(blue)) |>
  dplyr::mutate(power = max_red * max_green * max_blue) |>
  dplyr::pull(power) |>
  sum()
```

:::

If you have any other ideas, feel free to tell me more on
[Mastodon](https://fosstodon.org/@rmhirota) or
[Bluesky](https://bsky.app/profile/hirota.dev).

Happy coding! <3