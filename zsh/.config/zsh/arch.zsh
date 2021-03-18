alias pacin='sudo pacman -S'
alias pacreps='pacman -Ss'

if (( $+commands[yay] )); then
	alias yain='yay -S '
	alias yareps='yay -Ss --sortby popularity '
fi
