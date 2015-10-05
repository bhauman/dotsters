# dots

A game implemented in ClojureScript using the Core.async library.

[See the blog post explaining the game](http://rigsomelight.com/2013/08/12/clojurescript-core-async-dots-game.html)

[Play the game](http://rigsomelight.com/dotsters)

# Building and Running Locally

Clone the repo:
```
git clone https://github.com/bhauman/dotsters
cd dotsters
```
Build the ClojureScript
```
lein cljsbuild once
```
Start a Python web server (on a mac or unix machine)
```
python -m SimpleHTTPServer 8888
```
Browse to the locally running instance at [http://localhost:8888/resources/public/dots.html](http://localhost:8888/resources/public/dots.html)

## License

Copyright © 2013

Distributed under the Eclipse Public License, the same as Clojure.
