# Stock Market Simulator

A terminal-based stock market simulator built with Haskell and Rust.

## Overview

This project simulates stock trading over a year, using historical data for any stock available through the Polygon API. Users can buy and sell stocks, view their portfolio value, and progress through trading days.

## Features

- Interactive Terminal User Interface (TUI)
- Real-time stock data fetching using Polygon API
- Ability to simulate trading for any stock symbol
- Portfolio management with buy/sell functionality
- Day-by-day progression through a year of trading

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

When you start the application:
1. You'll be prompted to enter your Polygon API key
2. Then, you'll be asked to enter a stock symbol (e.g., AAPL for Apple Inc.)

Controls:
- 'b': Buy stocks
- 's': Sell stocks
- 'n': Next day
- 'q': Quit simulation
- Enter: Confirm action
- Esc: Cancel buy/sell or return to main screen

## Future Plans

- Implement more detailed trading signals and analysis
- Add ability to include multiple stocks in the portfolio
- Develop visualization of portfolio performance over time
- Implement random walk simulation for intra-day price movements
- Include functionality to test and compare different trading strategies
- Enhance data visualization with more advanced graphs and charts
- Add more complex trading options (e.g., short selling, options trading)
- Implement historical performance tracking and analysis

## Note

You need a valid Polygon API key to use this simulator. If you don't have one, you can sign up at https://polygon.io/
