
if pgrep -x "redshift" > /dev/null; then
    pkill redshift
else
    redshift -l 40.7128:-74.0060 -t 6500:4500 &
fi
