package betterviews

object Index {
  def apply(): String = {
    """
  <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8" content="text/html" http-equiv="Content-Type">
          <title>CodeCraft</title>
          <link rel="stylesheet" media="screen" href="assets/stylesheets/main.css")>
        </head>
        <body>
          <a href="/observe">Observe</a>
          <form action="start-game" method="post">
            <input type="submit" name="start-game" value="Start Game" />
          </form>
          <form action="start-demo" method="post">
 |          <input type="submit" name="start-demo" value="Start Demo" />
 |        </form>
 |      </body>
      </html>
      """
  }
}
