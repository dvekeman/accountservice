#!/bin/bash

# cd static 
# elm-make src/Main.elm --output elm.js
# cd ..

stack build && keter-package && cp accountservice-osx.keter /opt/keter/incoming/
