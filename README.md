# Crypto Ticker

## Overview

a TUI (Terminal User Interface) that displays 24 hour crypto tickers. 
Simply type a crypto symbol pair you are interest in and press either 
`F2` to add, or `F3` to remove its ticker from the table.

Ticker data is streamed via the <a href="https://github.com/binance/binance-spot-api-docs/blob/master/web-socket-streams.md" target="_blank">Binance socket streams</a> endpoint. Thus, 
supported symbol pairs directly correspond to the Binance WSS service. 
Please note that it may take several seconds for the stream to be added 
to the ticker table after pressing the `F2` button.

Press `F4` and `F5` to scroll the ticker table when you have added 
more rows than your terminal window can render.

![Crypto Ticker Demo](/doc/demo.gif)

## Application Details

This application is written in Haskell and makes use of the 
following concepts and libraries:

- **Clock**: For dealing with temporary data via the `clock` package.
- **Concurrency**: Multiple threads utilizing `TChan` and `BChan` primitives.
- **JSON**: Encoding and decoding JSON using the `aeson` package.
- **Lenses**: For accessing data in nested data structures.
- **TUI**: The terminal user interface is drawn with the `brick` library.
- **Web sockets**: Usage of the `Wuss` and `Network.WebSockets` modules.

