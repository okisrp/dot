#!/usr/bin/env sh

PID="$( pstree -p | grep -E 'dwm.*sh.*sleep' | sed -r 's/.*\(([0-9]*).*/\1/g' )"

kill "${PID}"
