#!/bin/bash
# COBOL Compilation Script

echo "Compiling COBOL programs..."

# Compile the main programs as modules (not executables)
cobc -m -o CREDITCARD.so CREDITCARD.cob
cobc -m -o PAYMENT.so PAYMENT.cob
cobc -m -o MORTGAGE.so MORTGAGE.cob
cobc -m -o MAINFRAME.so MAINFRAME.cob

# Compile the wrappers as executables
cobc -x -o CREDITCARD CREDITCARD_WRAPPER.cob -L. -lCREDITCARD
cobc -x -o PAYMENT PAYMENT_WRAPPER.cob -L. -lPAYMENT

# Create symlinks for gateway compatibility
ln -sf CREDITCARD VALIDATE-CARD
ln -sf PAYMENT PROCESS-PAYMENT
ln -sf MORTGAGE CALCULATE-MORTGAGE
ln -sf MAINFRAME MAINFRAME-SYNC

echo "Compilation complete!"
ls -la *.so
ls -la CREDITCARD PAYMENT VALIDATE-CARD PROCESS-PAYMENT