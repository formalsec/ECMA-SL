#! /bin/bash

sed -i "s/: 'empty/: \"'empty\"/g" globalHeap.json
sed -i "s/: 'undefined/: \"'undefined\"/g" globalHeap.json
sed -i "s/: 'null/: \"'null\" /g" globalHeap.json
sed -i "s/: nan/: \"nan\"/g" globalHeap.json
sed -i "s/: -inf/: \"-inf\"/g" globalHeap.json
sed -i "s/: inf/: \"inf\"/g" globalHeap.json
sed -i "s/(/[/g" globalHeap.json
sed -i "s/)/]/g" globalHeap.json

sed -i "s/: 'empty/: \"'empty\"/g" globalHeap_strict.json
sed -i "s/: 'undefined/: \"'undefined\"/g" globalHeap_strict.json
sed -i "s/: 'null/: \"'null\"/g" globalHeap_strict.json
sed -i "s/: nan/: \"nan\"/g" globalHeap_strict.json
sed -i "s/: -inf/: \"-inf\"/g" globalHeap_strict.json
sed -i "s/: inf/: \"inf\"/g" globalHeap_strict.json
sed -i "s/(/[/g" globalHeap_strict.json
sed -i "s/)/]/g" globalHeap_strict.json
