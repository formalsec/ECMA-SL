#! /bin/bash

cp "../implementation/test/heap.json" .

sed -i "s/'empty\b/\"'empty\"/g" heap.json
sed -i "s/'undefined\b/\"'undefined\"/g" heap.json
