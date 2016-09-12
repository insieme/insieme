#!/bin/bash

input="$(grep -R ACCESSOR *.h)"

raw_list="$(echo "$input" \
	| sed 's/ir.*.h:\s*//g' \
	| sed 's/IR_NODE_ACCESSOR(\([^,]*\),\([^,]*\))/Name: \1; Base: \2/g' \
	| sed 's/IR_NODE_ACCESSOR(\([^,]*\),\([^,]*\),\([^)]*\))/Name: \1; Base: \2; Params: \3/g' \
	| sed 's/IR_LIST_NODE_ACCESSOR(\([^,]*\),\([^,]*\),\([^,]*\),\([^)]*\))/Name: \1; Base: \2; ListName: \3; Params: \4/g' \
	| sed '/^#define /d' \
	)"

record_type="$(echo "$raw_list" \
	| sed -n 's/Name: NAME; \(.*\), ##.*/\1/p')"

raw_list="$(echo "$raw_list" \
	| sed 's/IR_RECORD_ACCESSOR(\([^,]*\),\([^,]*\))/Name: \1; '"$record_type"'; \2/g' \
	| sed 's/IR_RECORD_ACCESSOR(\([^,]*\))/Name: \1; '"$record_type"'/g' \
	| sed '/Name: NAME/d' \
	| sed 's/  / /g' \
	| sed 's/;/; /g' \
	)"

datalog="$(echo "$raw_list" \
	| sed 's/ //g' \
	| sed 's/Name:\([^;]*\);Base:\([^;]*\);ListName:\([^;]*\);Params:\(.*\)/.decl \1 ( id : node, é\4è )/g' \
	| sed 's/Name:\([^;]*\);Base:\([^;]*\);Params:\(.*\)/.decl \1 ( id : node, é\3è )/g' \
	| sed 's/Name:\([^;]*\);Base:\([^;]*\)/.decl \1 ( id : node )/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\),/UNDEF : \1, é/g' \
	| sed 's/é\([^,]*\);/UNDEF : \1, é/g' \
	| sed 's/é\([^,;]*\)è/UNDEF : \1/g' \
	)"

datalog_aligned="$(echo "$datalog" \
	| sed 's/(/é(/g' \
	| column -t -s 'é' \
	)"

echo "$raw_list" | sort
echo "--------------------"
echo "$datalog_aligned"
