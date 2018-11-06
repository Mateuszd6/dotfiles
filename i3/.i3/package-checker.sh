# NOTE: There is a hook in pacman config folder that sends
#       singal to i3blocks to that after each update/installation/etc
#       this is updated.

full_text="";

full=`yay -Quk | wc -l`
repo=`pacman -Quk | wc -l`
aur=$((full - repo))

if [ $repo -ne 0 ]; then
    full_text="$full_text$repo(Repo)"
    if [ $aur -ne 0 ]; then
        full_text="$full_text+$aur(Aur)"
    fi
elif [ $aur -ne 0 ]; then
    full_text="$full_text$aur(AUR)"
fi;

echo "$full_text"
echo "$full_text"

if [ $repo -ne 0 ]; then
    echo "#A8FF00"
fi
