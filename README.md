# frequencyDistribution

This project computes frequency distributions and related statistics for a given dataset.

## Files

### Main.hs

This file contains the main executable code.

#### Functionality

- **Input Parsing**
  - `parseList :: String -> [Integer]`: Parses a comma-separated string of numbers into a sorted list of integers.
  - `parsetoInt :: String -> Integer`: Converts a string into an integer.

- **Main Function**
  - Executes the main logic of the program:
    - Prompts the user to input a dataset and number of classes.
    - Calculates statistics such as sample size, range, class intervals, frequencies, midpoints, cumulative frequencies, relative frequencies, and class boundaries.
    - Displays all computed statistics.

### MyLib.hs

This file contains helper functions used in Main.hs.

#### Functions

- **getCW :: Integer -> Integer -> Integer**: Calculates the class width given the range and number of classes.
- **getClasses :: Integer -> [Integer] -> [(Integer,Integer)]**: Generates class intervals based on the class width and dataset.
- **getMids :: [(Integer,Integer)] -> [Float]**: Computes midpoints for each class interval.
- **getF :: [Integer] -> [(Integer,Integer)] -> [Integer]**: Computes frequencies of data points within each class interval.
- **getRelativeF :: Int -> [Integer] -> [Float]**: Calculates relative frequencies based on total sample size and frequency.
- **getClassB :: [(Integer, Integer)] -> [(Double, Double)]**: Computes class boundaries for each class interval.

## Usage

### Running the Program

1. **Setup**
   - Ensure you have GHC installed.

2. **Build**
   - Navigate to the project directory and run:
     ```bash
     cabal build
     ```

3. **Execute**
   - After building, run the executable:
     ```bash
     cabal run frequencyDistribution
     ```
   - Follow the prompts to input your dataset and number of classes.

### Example

#### Input
<p>
Input:<br />
10, 20, 30, 40, 50, 60, 70, 80, 90, 100<br />
5

#### Output
Sample size:<br />
10<br />
Range:<br />
90<br />
Class intervals:<br />
[(10,19),(20,29),(30,39),(40,49),(50,59)]<br />
Frequency:<br />
[1,1,1,1,1]<br />
Midpoints:<br />
[14.5,24.5,34.5,44.5,54.5]<br />
Cumulative f:<br />
[1,2,3,4,5]<br />
Relative f:<br />
[0.1,0.1,0.1,0.1,0.1]<br />
Class boundaries:<br />
[(9.5,19.5),(19.5,29.5),(29.5,39.5),(39.5,49.5),(49.5,59.5)]
</p>
