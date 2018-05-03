#!/bin/bash

# cd static 
# echo `pwd`
# elm-make src/Main.elm --output elm.js
# cd ..

echo `pwd`
docker-stack-build && keter-package
scp accountservice-linux.keter admin@th-bagua.ddns.me:/opt/keter/incoming/
