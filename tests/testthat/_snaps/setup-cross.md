# `setup_cross()` returns correct 2x5 indices for cross-val setup

    Code
      flatten(cv)
    Output
      $test_rows
      [1] 31 15 19 14  3 10 23
      
      $training_rows
       [1]  1  2  4  5  6  7  8  9 11 12 13 16 17 18 20 21 22 24 25 26 27 28 29 30 32
      
      $test_rows
      [1] 28 13  6 26 18 29 32
      
      $training_rows
       [1]  1  2  3  4  5  7  8  9 10 11 12 14 15 16 17 19 20 21 22 23 24 25 27 30 31
      
      $test_rows
      [1] 12  4 11 16  9 24
      
      $training_rows
       [1]  1  2  3  5  6  7  8 10 13 14 15 17 18 19 20 21 22 23 25 26 27 28 29 30 31
      [26] 32
      
      $test_rows
      [1] 22  5  7  1 27 20
      
      $training_rows
       [1]  2  3  4  6  8  9 10 11 12 13 14 15 16 17 18 19 21 23 24 25 26 28 29 30 31
      [26] 32
      
      $test_rows
      [1] 25 21 17  8  2 30
      
      $training_rows
       [1]  1  3  4  5  6  7  9 10 11 12 13 14 15 16 18 19 20 22 23 24 26 27 28 29 31
      [26] 32
      
      $test_rows
      [1]  9 32 10 23 27 21  7
      
      $training_rows
       [1]  1  2  3  4  5  6  8 11 12 13 14 15 16 17 18 19 20 22 24 25 26 28 29 30 31
      
      $test_rows
      [1] 26  6  2  5 11 15 22
      
      $training_rows
       [1]  1  3  4  7  8  9 10 12 13 14 16 17 18 19 20 21 23 24 25 27 28 29 30 31 32
      
      $test_rows
      [1] 31  1 17 19 29 13
      
      $training_rows
       [1]  2  3  4  5  6  7  8  9 10 11 12 14 15 16 18 20 21 22 23 24 25 26 27 28 30
      [26] 32
      
      $test_rows
      [1] 14 24 25 12 16 20
      
      $training_rows
       [1]  1  2  3  4  5  6  7  8  9 10 11 13 15 17 18 19 21 22 23 26 27 28 29 30 31
      [26] 32
      
      $test_rows
      [1]  8  3  4 18 28 30
      
      $training_rows
       [1]  1  2  5  6  7  9 10 11 12 13 14 15 16 17 19 20 21 22 23 24 25 26 27 29 31
      [26] 32
      

# `setup_cross_strat()` returns stratified setup based on 'vs' column

    Code
      flatten(cv_strat)
    Output
      [[1]]
      [1] "vs"
      
      [[2]]
      [1] 2
      
      [[3]]
      [1] 5
      
      $test_rows
      [1] 29  7 16 18 26
      
      $training_rows
       [1]  1  2  5 12 13 14 15 17 22 23 24 25 27 30 31  3  4  6  8  9 10 11 19 20 21
      [26] 28 32
      
      $test_rows
      [1] 27 25  5 22 19 32 21
      
      $training_rows
       [1]  1  2  7 12 13 14 15 16 17 23 24 29 30 31  3  4  6  8  9 10 11 18 20 26 28
      
      $test_rows
      [1]  2 14 24 15 11  9 20
      
      $training_rows
       [1]  1  5  7 12 13 16 17 22 23 25 27 29 30 31  3  4  6  8 10 18 19 21 26 28 32
      
      $test_rows
      [1] 13 17  1 12 10  6 28
      
      $training_rows
       [1]  2  5  7 14 15 16 22 23 24 25 27 29 30 31  3  4  8  9 11 18 19 20 21 26 32
      
      $test_rows
      [1] 23 30 31  3  4  8
      
      $training_rows
       [1]  1  2  5  7 12 13 14 15 16 17 22 24 25 27 29  6  9 10 11 18 19 20 21 26 28
      [26] 32
      
      $test_rows
      [1] 14  2 30 10  8
      
      $training_rows
       [1]  1  5  7 12 13 15 16 17 22 23 24 25 27 29 31  3  4  6  9 11 18 19 20 21 26
      [26] 28 32
      
      $test_rows
      [1] 16  5 15 24 26 28  4
      
      $training_rows
       [1]  1  2  7 12 13 14 17 22 23 25 27 29 30 31  3  6  8  9 10 11 18 19 20 21 32
      
      $test_rows
      [1] 17 25 23  7  3 20 32
      
      $training_rows
       [1]  1  2  5 12 13 14 15 16 22 24 27 29 30 31  4  6  8  9 10 11 18 19 21 26 28
      
      $test_rows
      [1] 13  1 12 31 18 11  6
      
      $training_rows
       [1]  2  5  7 14 15 16 17 22 23 24 25 27 29 30  3  4  8  9 10 19 20 21 26 28 32
      
      $test_rows
      [1] 29 27 22  9 19 21
      
      $training_rows
       [1]  1  2  5  7 12 13 14 15 16 17 23 24 25 30 31  3  4  6  8 10 11 18 20 26 28
      [26] 32
      

# `setup_cross_strat()` errors when 'strat_column' is too uniform

    Code
      setup_cross_strat.feature_select(x)
    Output
      # A tibble: 1 x 2
        foo       n
        <chr> <int>
      1 bar      32
    Condition
      Error:
      ! Not enough representative samples per class to stratify: 'foo'

---

    Code
      setup_cross_strat.feature_select(x)
    Output
      # A tibble: 2 x 2
        foo       n
        <chr> <int>
      1 bar      29
      2 baz       3
    Condition
      Error:
      ! Not enough representative samples per class to stratify: 'foo'

