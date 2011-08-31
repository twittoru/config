if [[ -z 0 ]]; then #{{{1 mickey24's .zshrc
# History "{{{2
HISTFILE=${HOME}/.zsh_history
SAVEHIST=10000
HISTSIZE=10000
setopt append_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_nodups
setopt share_history

# Directory "{{{2
DIRSTACKSIZE=8
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_minus
setopt pushd_silent
setopt pushd_to_home

# Completion "{{{2
LISTMAX=0
setopt complete_aliases
setopt complete_in_word
setopt extendedglob
unsetopt list_ambiguous
setopt list_packed
setopt list_types
setopt mark_dirs
setopt numeric_glob_sort
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:warnings' format 'No matches'

autoload -U compinit
compinit -u

# Prompt "{{{2
autoload -U colors; colors
setopt prompt_subst
unsetopt transient_rprompt

if [ $SSH_CONNECTION ] || [ $REMOTEHOST ]; then
  PROMPT='%{%(!.$bg[default].%(?.$bg[blue].$bg[red]))%}%n@%m:%(5~,%-2~/.../%2~,%~)%#%{$reset_color%} '
  RPROMPT='%{%(!.$bg[default].%(?.$bg[blue].$bg[red]))%}[`date +%Y/%m/%d` %T]%{$reset_color%}'
else
  PROMPT='%{%(!.$bg[default].%(?.$bg[green].$bg[yellow]))%}%n@%m:%(5~,%-2~/.../%2~,%~)%#%{$reset_color%} '
  RPROMPT='%{%(!.$bg[default].%(?.$bg[green].$bg[yellow]))%}[`date +%Y/%m/%d` %T]%{$reset_color%}'
fi

# Misc "{{{2
umask 022
limit coredumpsize 0
stty erase '^h'
stty kill '^g'
stty stop 'undef'

bindkey -e

setopt bad_pattern
unsetopt beep
setopt c_bases
setopt check_jobs
unsetopt clobber
unsetopt flow_control
setopt ignore_eof
setopt long_list_jobs
setopt print_eightbit

autoload -U tetris; zle -N tetris

# History search "{{{2
autoload -U  up-line-or-beginning-search
zle      -N  up-line-or-beginning-search
bindkey '^P' up-line-or-beginning-search

autoload -U  down-line-or-beginning-search
zle      -N  down-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search

# Abbreviation "{{{2
typeset -A myAbbrev
myAbbrev=(
"L" "| less"
"G" "| grep"
"H" "| head"
"T" "| tail"
"W" "| wc"
"A" "| awk"
"S" "| sed"
"Y" "yes |"
"...." "../.."
)
function my-expand-abbrev() {
  emulate -L zsh
  setopt extendedglob
  typeset MATCH
  LBUFFER="${LBUFFER%%(#m)[^[:blank:]]#}${myAbbrev[${MATCH}]:-${MATCH}}${KEYS}"
}
zle -N my-expand-abbrev
bindkey " " my-expand-abbrev

# For GNU screen "{{{2
if [ "$TERM" = "screen" ]; then
  chpwd () { echo -n "_`dirs`\\" }
  preexec() {
    emulate -L zsh
    local -a cmd; cmd=(${(z)2})
    case $cmd[1] in
      fg)
      if (( $#cmd == 1 )); then
        cmd=(builtin jobs -l %+)
      else
        cmd=(builtin jobs -l $cmd[2])
      fi
      ;;
      %*)
      cmd=(builtin jobs -l $cmd[1])
      ;;
      cd)
      if (( $#cmd == 2)); then
        cmd[1]=$cmd[2]
      fi
      ;&
      *)
      echo -n "k$cmd[1]:t\\"
      return
      ;;
    esac

    local -A jt; jt=(${(kv)jobtexts})

    $cmd >>(read num rest
    cmd=(${(z)${(e):-¥$jt$num}})
    echo -n "k$cmd[1]:t\\") 2>/dev/null
  }
  chpwd
fi

if [ "$TERM" = "screen" ]; then
  precmd(){
    screen -X title $(basename $(print -P "%~"))
  }
fi

# Aliases "{{{2
setopt aliases
alias l='gls -F --color=auto'
alias ls='gls -F --color=auto'
alias ll='gls -lF --color=auto'
alias la='gls -aF --color=auto'
alias lla='gls -laF --color=auto'
alias x='exit'
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
alias dirs='dirs -v'
alias pd='popd'
alias ud='cd ../'
alias s='screen'
alias v='vim'
alias r='R'
alias emacs='open -a /Applications/Emacs.app'
fi 
if [[ -z 0 ]]; then #{{{1 from http://www.gfd-dennou.org/member/uwabami/cc-env/Zsh.html
#!/usr/bin/env zsh
# -*- mode: sh; coding: utf-8-unix; indent-tabs-mode: nil -*-
#  zshrc
#
# Copyright(C) 2010 Youhei SASAKI All rights reserved.
# $Id: $
#
# Author: Youhei SASAKI <uwabami@gfd-dennou.org>
# Keywords:
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  Code:

## umask
umask 002
## keybind  -> emacs like
bindkey -e

### history ###
## change history file for root/sudo
HISTFILE=${HOME}/.zsh/${USER}-zhistory
# メモリ内の履歴の数
HISTSIZE=100000
# 保存される履歴の数
SAVEHIST=100000
# 既にヒストリにあるコマンドは古い方を削除
setopt hist_ignore_all_dups
# コマンドラインの余分なスペースを削除
setopt hist_reduce_blanks
#ヒストリの共有 for GNU Screen
setopt share_history
# コマンドの開始時刻と経過時間を登録
setopt extended_history
# historyコマンドは登録しない
setopt hist_no_store
# コマンド行先頭が空白の時登録しない(直後ならば呼べる)
setopt hist_ignore_space
# 履歴の一覧を出す
function history-all { history -E 1}

### 補完 ###
if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    function () { # precompile
        local A
        A=~/.zsh/modules/auto-fu/auto-fu.zsh
        [[ -e "${A:r}.zwc" ]] && [[ "$A" -ot "${A:r}.zwc" ]] ||
        zsh -c "source $A; auto-fu-zcompile $A ${A:h}" > /dev/null 2>&1
    }
    source ~/.zsh/modules/auto-fu/auto-fu; auto-fu-install
    function zle-line-init () { auto-fu-init }
    zle -N zle-line-init
    zstyle ':auto-fu:highlight' input bold
    zstyle ':auto-fu:highlight' completion fg=white,bold
    zstyle ':auto-fu:var' postdisplay ''
# auto-fu cancel
    function afu+cancel () {
        afu-clearing-maybe
        ((afu_in_p == 1)) && { afu_in_p=0; BUFFER="$buffer_cur"; }
    }
    function bindkey-advice-before () {
        local key="$1"
        local advice="$2"
        local widget="$3"
        [[ -z "$widget" ]] && {
            local -a bind
            bind=(`bindkey -M main "$key"`)
            widget=$bind[2]
        }
        local fun="$advice"
        if [[ "$widget" != "undefined-key" ]] ; then
            local code=${"$(<=(cat <<"EOT"
              function $advice-$widget () {
                zle $advice
                zle $widget
              }
              fun="$advice-$widget"
EOT
        ))"}
            eval "${${${code//\$widget/$widget}//\$key/$key}//\$advice/$advice}"
        fi
        zle -N "$fun"
        bindkey -M afu "$key" "$fun"
    }
    bindkey-advice-before "^G" afu+cancel
    bindkey-advice-before "^[" afu+cancel
    bindkey-advice-before "^J" afu+cancel afu+a
fi
setopt auto_list auto_param_slash list_packed rec_exact print_eight_bit
unsetopt list_beep
zstyle ':completion:*' menu select=1
zstyle ':completion:*' format '%d%f'
zstyle ':completion:*' group-name
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' keep-prefix
zstyle ':completion:*' completer _oldlist _complete _history _ignored _approximate _match  _list 
#zstyle ':completion:*' completer _oldlist _complete _history 
# see LS_COLORS & LSCOLORS
zstyle ':completion:*' list-colors \
    'di=01;34' 'ln=01;36' 'pi=40;33' 'so=01;35' 'ex=01;32' \
    'bd=40;33;01' 'cd=40;33;01' 'su=37;41' 'sg=30;43' 'tw=30;42' 'ow=34;42'
autoload -U compinit
compinit -u

# コマンド入力エラーでBEEPを鳴らさない
setopt no_beep
# コアファイルを吐かないようにする
is_cygwin || limit coredumpsize    0
# nohup
setopt nohup
## cd の履歴
setopt autopushd
# ディレクトリスタックに重複する物は古い方を削除
setopt pushd_ignore_dups
# cdhist みたいなー
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'
# get the name of the chroot I'm on, need /etc/debian_chroot inside chroot
chroot_info() {
    chroot=$(cat /etc/debian_chroot 2>/dev/null) || return
    echo "${chroot}|"
}
# get the name of the VCS branch I'm on
if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    autoload -Uz vcs_info
    zstyle ':vcs_info:*' formats '%s:%b|'
    zstyle ':vcs_info:*' actionformats '%s:%b%a|'
    vcs_prompt_info(){
        LANG=C vcs_info
        [[ -n "$vcs_info_msg_0_" ]] && echo "$vcs_info_msg_0_"
    }
else
    vcs_prompt_info() {
        ref=$(git-symbolic-ref HEAD 2> /dev/null) || return
        echo "git:${ref#refs/heads/}|"
    }
fi
## prompt の設定
autoload -U colors
colors
# 被る時は右プロンプトを消す
unsetopt promptcr
# 環境変数をプロンプトに展開する
setopt prompt_subst
# 右プロンプトを設定
local dirs='%B%{$fg[white]%}%(5~,%-2~/.../%1~,%~)%b%f'
local chroot='%B%{$fg[green]%}$(chroot_info)%f'
local vcs='%B%{$fg[red]%}$(vcs_prompt_info)%f'
# 右プロンプトを設定 -- chroot|vcsinfo|pwd という表示
RPROMPT=$chroot$vcs$dirs
# 左プロンプトの表示 -- [username@hostname:jobs:%> という表示
PROMPT=$'%U%(!.%B%{%{$fg[magenta]%}%}.%{%{$fg[white]%}%})[%n@%m:%j:%(!.#.%%)%u%B>%b %f'
# screen の title をディレクトリ名or実行中のコマンド名にする
typeset -ga precmd_functions
typeset -ga preexec_functions
[[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]] && source ~/.zsh/screen-title.zsh

### alias ###
# ls の色設定
export LSCOLORS=exgxfxdxcxdxdxbxadacec
export LS_COLORS='di=01;34:ln=01;36:pi=40;33:so=01;35:ex=01;32:bd=40;33;01:cd=40;33;01:su=37;41:sg=30;43:tw=30;42:ow=34;42'
if is_darwin ; then
    alias ls='ls -FG'
    alias la='ls -haFG'
    alias ll='ls -hlFG'
    alias lla='ls -hlaFG'
    alias lsd='ls -ld *(-/DN)'
elif is_solaris ; then
    alias ls='/opt/sfw/bin/ls -F --color=auto'
    alias la='/opt/sfw/bin/ls -haF --color=auto'
    alias ll='/opt/sfw/bin/ls -hlF --color=auto'
    alias lla='/opt/sfw/bin/ls -hlaF --color=auto'
    alias lsd='/opt/sfw/bin/ls -ld *(-/DN)'
elif is_cygwin ; then
    alias ls='ls -F --color=auto --show-control-chars'
    alias la='ls -haF --color=auto --show-control-chars'
    alias ll='ls -hlF --color=auto --show-control-chars'
    alias lla='ls -hlaF --color=auto --show-control-chars'
    alias lsd='ls -ld *(-/DN)'
else
    alias ls='ls -F --color=auto'
    alias la='ls -haF --color=auto'
    alias ll='ls -hlF --color=auto'
    alias lla='ls -hlaF --color=auto'
    alias lsd='ls -ld *(-/DN)'
fi
alias sl='ls'
alias man='LANG=C man'
alias rm='nocorrect rm -i'
alias mv='nocorrect mv -i'
alias mkdir='nocorrect mkdir'
alias mv='nocorrect mv'
alias clean='rm -rf *~; rm -rf *.bak ; rm -rf a.out'
alias cleanall='rm -rf .*~ ; rm -rf .*.bak; rm -rf .saves-*'
alias logtail="tailf /var/log/syslog"
[[ ! -f `which vim` ]] && alias vim=vi
[ -f ${HOME}/Library/site_ruby/irb/ggraph.rb ] && \
  alias ggraph='irb -r irb/ggraph'
is_darwin && [ -d /Applications/MacPorts/Emacs.app ] && \
    alias emacs=/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
[[ ${HOST} == smaux ]] && [ -d ${HOME}/Library/emacs23.2 ] && \
    alias emacs=${HOME}/Library/emacs23.2/bin/emacs23
if [ -f /usr/local/bin/screen  -o -f ${HOME}/bin/screen ]; then
    alias screen="TERM=xterm-256color screen"
    alias en="TERM=screen-256color-bce emacs -nw"
else
    alias en="emacs -nw"
fi
alias xscreen="screen -x || screen"
alias xxx="rm -f ~/.xsession-errors; startx -- -dpi 100 -nolisten tcp 1> ${HOME}/.xlog 2>&1"
alias halt="rm -rf ~/.serverauth.* ; sudo halt"
alias reboot="sudo reboot"
alias s2ram="sudo pm-suspend"
