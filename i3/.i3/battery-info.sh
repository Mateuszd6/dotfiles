#!/bin/bash
# It doesn't work well nor seem to have a chance to do so.

color="#ffffff"
pluged=""
adapter=($`acpi --ac-adapter`);
if [ ${adapter[2]} == "on-line" ]; then
#    echo "OK";
    full_text=" "
elif [ ${adapter[2]} != "off-line" ]; then
    echo "Not able to recogize acpi output!!!";
fi

bat_info=($`acpi --battery`)
short_text=$full_text;

# for i in `seq 0 10`;
# do
#    echo "$i: ${bat_info[$i]}" 
#done
val=${bat_info[3]};
#val=${val:: -2}

#echo ${val[0]};

#if [[ $val -ge 80 ]]; then
#     if [ $pluged == "" ]; then
# 	color="#00FF00"
#     fi;
# elif [[ $val -lt 20 ]]; then
#     color="#FF2222"
# elif [[ $val -lt 60 ]]; then
#     color="#FFAE00"
# else
#     color="#FFF600"
# fi

full_text="$full_text $val"
short_text="$full_text"
#echo $full_text

echo -e "<span color='$color'>$full_text</span>\n"
