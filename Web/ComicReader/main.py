#vim:fileencoding=utf-8

from flask import Flask, render_template_string, send_from_directory
import os, sys

app = Flask(__name__)

@app.route("/img/<chapter_name>/<img_name>")
def img(chapter_name, img_name):
    return send_from_directory(os.path.join(sys.argv[1], chapter_name), img_name)

@app.route("/chapter/<chapter_name>")
def chapter(chapter_name):
    root = os.path.join(sys.argv[1], chapter_name)

    img_names = [img_name.decode('utf-8')
        for img_name in sorted(os.listdir(root)) if os.path.isfile(os.path.join(root, img_name))]

    template = """
    <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="http://ajax.aspnetcdn.com/ajax/bootstrap/3.3.6/css/bootstrap.min.css">
        <link rel="stylesheet" href="http://ajax.aspnetcdn.com/ajax/bootstrap/3.3.6/css/bootstrap-theme.min.css">
      </head>
      <body>
        <script src="http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.11.3.min.js"></script>
        <div class="container">
            <ul>
                {% for img_name in img_names %}
                    <li>
                        <img class="img-responsive" src="{{url_for('img', chapter_name=chapter_name, img_name=img_name)}}" />
                    </li>
                {% endfor %}
            </ul>
        </div>
          <script src="http://ajax.aspnetcdn.com/ajax/bootstrap/3.3.6/bootstrap.min.js"> </script>
      </body>
     </html>
    """
    return render_template_string(template, img_names=img_names, chapter_name=chapter_name)

@app.route("/")
def index():
    root = sys.argv[1]

    chapter_names = [chapter_name.decode('utf-8')
        for chapter_name in sorted(os.listdir(root)) if os.path.isdir(os.path.join(root, chapter_name))]

    template = """
    <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
      </head>
      <body>
        <ul>
            {% for chapter_name in chapter_names %}
                <li>
                    <a href="{{ url_for('chapter', chapter_name=chapter_name) }}">
                        <h3>{{ chapter_name }}</h3>
                    </a>
                </li>
            {% endfor %}
        </ul>
      </body>
     </html>
    """
    return render_template_string(template, chapter_names=chapter_names)

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=12345, threaded=True)
