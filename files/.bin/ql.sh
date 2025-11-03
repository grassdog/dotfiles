#!/bin/sh

# usage:
#	ql /tmp/file.jpg
#	cat /tmp/file.jpg | ql
#	cal -h | ql

if [ -z "$*" ]; then
	cat > /tmp/ql.stdin
	mime_type=$(file --brief --mime-type /tmp/ql.stdin)
	extension=$(grep "$mime_type" /etc/apache2/mime.types | awk '{ print $2 }')
	if [ -z $extension ]; then
		qlmanage -p /tmp/ql.stdin 2>/dev/null 1>/dev/null
		rm /tmp/ql.stdin
	else
		file=/tmp/ql.$extension
		mv /tmp/ql.stdin $file
		qlmanage -p $file 2>/dev/null 1>/dev/null
		rm $file
	fi
else
	qlmanage -p "$@" 2>/dev/null 1>/dev/null
fi