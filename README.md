# CodeCraft Server

Play Framework based server that makes it possible, if unpleasant, to create and interact with [CodeCraft](http://www.codecraftgame.org/) game instances over an HTTP API.

## Requirements

- JDK 8
- [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html)

## Usage

After installing the requirements and cloning the repository, run `sbt run` to start the server.
You can observe the next game to be started by opening [](localhost:9000/observe) in your browser.
To automatically zoom out to see the entire map and keep joining games after they finish, visit [](localhost:9000/observe?autorestart=true&autozoom=true).
In my experience performance is best in Chrome.
If you close the browser while a game instance is still running, that game instance will be terminated after a timeout period.
CodeCraft and/or this server suffer from memory leaks so the server eventually slows down and crashes.
See [](https://github.com/cswinter/DeepCodeCraft/blob/master/codecraft.py) for an example of how to use the HTTP API.
Good luck.

