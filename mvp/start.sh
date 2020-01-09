#!/bin/bash

rm -f *.log

osascript -e "tell application \"terminal\" to do script \"cd ${PWD}; racket gui.rkt\"" &
osascript -e "tell application \"terminal\" to do script \"cd ${PWD}; scheme mcp.scm\"" &
osascript -e "tell application \"terminal\" to do script \"cd ${PWD}; sleep 3; scheme scp.scm\"" &
