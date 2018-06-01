#!/bin/bash
# val=$(awk '/MemT/ {t=$2} /^(MemF|Bu|Ca)/ {f+=$2} END {print (t-f)/t}'
# /proc/meminfo)

# o=`echo '$($(printf "%.2f" $val)*100)' | bc`
# echo $o%
full_text=" "
out=`free -m | grep -i mem`
arr=($out)
mem_val=${arr[2]};
full_text="$full_text"`echo $mem_val | awk '{printf "%0.2f\n",$1/1000}'`"G"

#echo "${arr[2]}" | awk '{printf "%0.2f",$1/1000}'


out=`free -m | grep -i swap`
arr=($out)
swap_val=${arr[2]}
if [ $swap_val -ne 0 ]; then
    full_text="$full_text ("`echo $swap_val | awk '{printf "%0.2f\n",$1/1000}'`"G)"
fi
#echo $swap_val
echo $full_text
echo $full_text
if [ $mem_val -ge 7000 ]; then
    echo "#FF0000"
elif [ $mem_val -ge 5500 ]; then
    echo "#FFF600"
elif [ $mem_val -ge 3500 ]; then
    echo "#FFAE00"
else
    echo "#FFFFFF"
fi

# Left, right middle mouse button interaction
case $BLOCK_BUTTON in
  1|2|3) gnome-system-monitor -r > /dev/null ;;
esac
