# frequencyDistribution

A command-line tool written in Haskell that calculates and displays a frequency distribution for a given set of numbers. It supports both file-based input and an interactive mode.

## Features

- **Calculates a comprehensive frequency distribution**, including:
  - Sample Size
  - Range
  - Class Width
  - Class Intervals
  - Frequencies
  - Midpoints
  - Cumulative Frequencies
  - Relative Frequencies
  - Class Boundaries

## Requirements

You will need to have Haskell and Cabal installed to build and run the project. You can find instructions for installation on the [Haskell website](https://www.haskell.org/downloads/).

To get started, clone the repository and navigate into the project directory:

```bash
git clone https://github.com/justromeon/frequencyDistribution.git
cd codingbat
```

## Usage

The program can be run in two modes:

### 1. Interactive Mode

Run the program without any arguments to enter the interactive mode. The program will prompt you to input the data set and the number of classes directly.

```sh
cabal run
```

### 2. File Mode

Provide the path to a data file and the desired number of classes as command-line arguments. The data file should contain numbers separated by spaces or commas.

```sh
cabal run frequencyDistribution -- <file_path> <number_of_classes>
```

**Example:**

```sh
cabal run frequencyDistribution -- sample.txt 5
```

This will produce an output similar to the following:

```
Sample size: 10

Range: 23

Class Width: 5

Class intervals:
[(2,6),(7,11),(12,16),(17,21),(22,26)]

Frequencies:
[2,2,2,2,2]

Midpoints:
[4.0,9.0,14.0,19.0,24.0]

Cumulative f:
[2,4,6,8,10]

Relative f:
[0.2,0.2,0.2,0.2,0.2]

Class boundaries:
[(1.5,6.5),(6.5,11.5),(11.5,16.5),(16.5,21.5),(21.5,26.5)]
```
