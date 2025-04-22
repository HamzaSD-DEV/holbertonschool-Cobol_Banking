#!/bin/bash

if \
  grep -Pq '(?mi)^\s*01\s+CUSTOMER-ID\s+PIC\s+X\(9\)'   1-format_pic.cbl && \
  grep -Pq '(?mi)^\s*01\s+ACCOUNT-BALANCE\s+PIC\s+9\(5\)V99' 1-format_pic.cbl && \
  grep -Pq '(?mi)^\s*01\s+INTEREST-RATE\s+PIC\s+9V99'       1-format_pic.cbl && \
  grep -Pq '(?mi)^\s*DISPLAY\s+.*CUSTOMER-ID\b'   1-format_pic.cbl && \
  grep -Pq '(?mi)^\s*DISPLAY\s+.*ACCOUNT-BALANCE\b' 1-format_pic.cbl && \
  grep -Pq '(?mi)^\s*DISPLAY\s+.*INTEREST-RATE\b'   1-format_pic.cbl && \
  ! grep -Pq '(?mi)^\s*DISPLAY(?:(?!CUSTOMER-ID|ACCOUNT-BALANCE|INTEREST-RATE).)*$' 1-format_pic.cbl
then
  echo "✅ Task1 passed"
else
  echo "❌ Task1 failed"
fi
