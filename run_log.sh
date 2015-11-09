#! /bin/sh

### Runs the run file while logging the output messages
"./run.sh" 2>&1 | tee "./run_log_output.log"

