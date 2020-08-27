Simple backtracking based Sudoku solver in Scala.

# Usage

Solve a sudoku by passing it on stdin: 

```cat puzzles/example.txt | sbt "run"```

The input should contains 81 digits, where 0 stands in for free fields. Example:
```
003|020|600
900|305|001
001|806|400
-----------
008|102|900
700|000|008
006|708|200
-----------
002|609|500
800|203|009
005|010|300
```
where all non digit input characters are ignored.

The output for the example is 
```
483|921|657
967|345|821
251|876|493
-----------
548|132|976
729|564|138
136|798|245
-----------
372|689|514
814|253|769
695|417|382
```

To run the unit tests:

```sbt "test"```