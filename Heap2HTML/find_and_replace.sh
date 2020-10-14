#! /bin/bash

cp "../implementation/heap.json" .

sed -i "s/'empty\b/\"'empty\"/g" heap.json
sed -i "s/'undefined\b/\"'undefined\"/g" heap.json
