#!/usr/bin/env bash

elm-live src/Main.elm \
  --before-build="./scripts/clearConsole.sh" \
  --pushstate \
  --open \
  --start-page=index.html \
  -- --debug --output=elm.js \
