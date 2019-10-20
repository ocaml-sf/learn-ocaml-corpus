#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Functions.

positive_test() {
  local f="$1"
  if $LEARN grade --timeout=5 --dump-reports=${f%.ml} >${f%.ml}.log 2>&1 ; then
    echo " [OK]  $f is correctly accepted." ;
  else
    echo "[FAIL] $f is rejected!" ;
  fi
}
export -f positive_test

negative_test() {
  local f="$1"
  rm -f ${f%.ml}.report.txt ${f%.ml}.report.html ;
  if $LEARN grade --timeout=5 --grade-student=$f --dump-reports=${f%.ml} >${f%.ml}.log 2>&1 ; then
    echo "[FAIL] $f is incorrectly accepted!" ;
  else
    echo " [OK]  $f is correctly rejected." ;
  fi
}
export -f negative_test

# Make sure that the proposed solution is (quickly) accepted.
echo "Grading the proposed solution..."
time -p positive_test solution.ml

# Allow other correct solutions to be proposed.
if [ -d right ] ; then
  echo "Grading known correct solutions..."
  time -p (ls right/*.ml | parallel --no-notice positive_test) ;
fi

# Make sure that a number of known incorrect solutions are rejected.
echo "Grading known incorrect solutions..."
time -p (ls wrong/*.ml | parallel --no-notice negative_test)

# Use git status to display any differences with respect to the expected
# output and expected log (which should be checked in).
git status -- *.report.txt *.log wrong/*.report.txt wrong/*.log
