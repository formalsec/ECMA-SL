#! /bin/bash

cp "../implementation/heap.json" .

sed -i "s/'empty\b/\"'empty\"/g" heap.json
sed -i "s/'undefined\b/\"'undefined\"/g" heap.json
sed -i "s/'null\b/\"'null\"/g" heap.json
sed -i "s/nan\b/\"NaN\"/g" heap.json
sed -i "s/-inf\b/\"-Infinity\"/g" heap.json
sed -i "s/inf\b/\"Infinity\"/g" heap.json
