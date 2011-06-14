limit coredumpsize 0
typeset -U path
path=(~/local/homebrew/bin ~/local/lib/gems/bin ~/local/bin /usr/bin /usr/sbin /opt/local/bin /opt/local/sbin $path )

if [[ -z 0 ]]; then #{{{ from http://www.gfd-dennou.org/member/uwabami/cc-env/Zsh.html
export ZDOTDIR=${HOME}/.zsh
source ${ZDOTDIR}/.zshenv
fi  #}}}
