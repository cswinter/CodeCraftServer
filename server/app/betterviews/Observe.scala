package betterviews

object Observe {
  def apply(autorestart: Boolean): String = {
    s"""
  <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="UTF-8" content="text/html" http-equiv="Content-Type">
          <title>CodeCraft</title>
          <link rel="stylesheet" media="screen" href="assets/stylesheets/main.css")>
        </head>
        <body>
          <table id="page">
            <tr>
              <td id="tdcontent">
                <canvas id="webgl-canvas" tabindex="1"></canvas>
                <div id="text-container" tabindex="-1"></div>
                <div id="text-test-container" tabindex="-1"></div>
              </td>
            </tr>
          </table>

          <script id="script" type="text/javascript" src="assets/client-fastopt.js" data-autorestart="$autorestart"></script>
        </body>
      </html>
      """
  }
}
