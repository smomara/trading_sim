# Stock Market Simulator

A command-line stock market simulator built with Haskell and Rust.

## Overview

This project simulates stock trading over a year, using historical data for Apple Inc. (AAPL) stock. Users can buy and sell stocks, view their portfolio value, and progress through trading days.

## Future Plans

- Improve functionality with more complex trading options
- Implement various trading strategy simulations
- Expand to include multiple stocks and markets
- Enhance user interface and data visualization

## Setup

1. Clone the repository
2. Build the Rust library:
   ```
   cd rust/stock_data
   cargo build --release
   cd ../..
   ```
3. Build the Haskell project:
   ```
   cabal build
   ```

## Usage

Run the simulator:
```
cabal run
```

Controls:
- 'b': Buy stocks
- 's': Sell stocks
- 'n': Next day
- 'q': Quit simulation
- Esc: Cancel buy/sell

Note: Replace "YOUR_API_KEY_HERE" in the main function with your actual API key.
