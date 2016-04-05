# modify the prompt to contain git branch name if applicable
git_prompt_info() {
  current_branch=$(git current-branch 2> /dev/null)
  if [[ -n $current_branch ]]; then
    echo " %{$fg_bold[green]%}$current_branch%{$reset_color%}"
  fi
}
setopt promptsubst
PS1='${SSH_CONNECTION+"%{$fg_bold[green]%}%n@%m:"}%{$fg_bold[blue]%}%~%{$reset_color%}$(git_prompt_info) $ '
RPROMPT='`jobs | sed -E "s;\[([0-9])*\]  (\+|\-| )? (s|r)[a-z]* *(.*); [\1]\3 \2\4;" | tr "\n" "," | sed "s/.$//"` [`date +%H:%M:%S`]'
