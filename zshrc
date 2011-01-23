# 文字コードの設定
export LANG=ja_JP.UTF-8
export __CF_USER_TEXT_ENCODING="0x1F5:0x08000100:14"
export EDITOR="vi"
export RLWRAP_HOME=~/.rlwrap
export GEM_HOME=~/local/lib/gems 
export PERL_CPANM_OPT="--local-lib=~/local/"
export PERL5LIB="$HOME/local/lib/perl5:$PERL5LIB"
export TERMINFO=/usr/share/terminfo
export PKG_CONFIG_PATH=/Library/Frameworks/Mono.framework/Versions/Current/lib/pkgconfig
bindkey -e

# エイリアスの設定
alias ls='gls --color=auto'
alias la='ls -a'
alias quit='exit'
alias vim='/Applications/MacVim.app/Contents/MacOS/Vim "$@"'
alias vi='/Applications/MacVim.app/Contents/MacOS/Vim "$@"'
#alias open='gnome-open'
alias less='/Applications/MacVim.app/Contents/Resources/vim/runtime/macros/less.sh'
alias ll='ls -alF'
alias crontab='crontab -i'

bindkey '^[[3~' backward-delete-char
bindkey '^@' backward-delete-char

for f in ~/.zsh/command/* ; do 
    source $f
done

fpath=(~/.zsh/function ${fpath})

# ヒストリの設定
HISTFILE=~/.zhistfile
HISTSIZE=10000
SAVEHIST=10000

# zargsを使う
autoload zargs

# zedを使う
autoload zed

# 履歴ファイルに時刻を記録
setopt extended_history

# 拡張グロブを使う
setopt extended_glob

# 補完するかの質問は画面を超える時にのみに行う｡
LISTMAX=0

# 補完の利用設定
autoload -Uz compinit; compinit

## 補完時に大小文字を区別しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu sele

# sudo でも補完の対象
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /opt/local/sbin /opt/local/bin ~/local/bin
zstyle ':completion:*:rlwrap:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin /opt/local/sbin /opt/local/bin ~/local/bin

# 補完 http://www.ayu.ics.keio.ac.jp/~mukai/translate/write_zsh_functions.html
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

# cdのタイミングで自動的にpushd
setopt auto_pushd 
function chpwd() { ls }

# 複数の zsh を同時に使う時など history ファイルに上書きせず追加
setopt append_history

# 補完候補が複数ある時に、一覧表示
setopt auto_list

# 補完キー（Tab, Ctrl+I) を連打するだけで順に補完候補を自動で補完
setopt auto_menu

# カッコの対応などを自動的に補完
setopt auto_param_keys

# ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash

# ビープ音を鳴らさないようにする
setopt NO_beep

# 直前と同じコマンドラインはヒストリに追加しない
setopt hist_ignore_dups

# 重複したヒストリは追加しない
setopt hist_ignore_all_dups

# ヒストリを呼び出してから実行する間に一旦編集できる状態になる
setopt hist_verify

# auto_list の補完候補一覧で、ls -F のようにファイルの種別をマーク表示しない
setopt NO_list_types

# コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt magic_equal_subst

# ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs

# 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt print_eight_bit

# シェルのプロセスごとに履歴を共有
setopt share_history

# Ctrl+wで､直前の/までを削除する｡
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# ディレクトリを水色にする｡
export LS_COLORS='di=01;36'

# ファイルリスト補完でもlsと同様に色をつける｡
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}



## ^で､cd ..
#function cdup() {
#echo
#cd ..
#zle reset-prompt
#}
#zle -N cdup
#bindkey '\^' cdup

# ディレクトリ名だけで､ディレクトリの移動をする｡
setopt auto_cd

# screenっぽいステータスライン
#if [ $TERM = "xterm-256color" ] ; then
#	preexec(){
#		echo -ne "\ek${1%% *}\e\\"
#	}
#fi

# http://www.zsh.org/mla/users/2002/msg00108.html

#
# Set vi mode status bar
#

#
# Reads until the given character has been entered.
#
readuntil () {
	typeset a
	while [ "$a" != "$1" ]
	do
		read -E -k 1 a
	done
}

#
# If the $SHOWMODE variable is set, displays the vi mode, specified by
# the $VIMODE variable, under the current command line.
# 
# Arguments:
#
#   1 (optional): Beyond normal calculations, the number of additional
#   lines to move down before printing the mode.  Defaults to zero.
#
showmode() {
	typeset movedown
	typeset row

	# Do not echo back
	stty -echo > /dev/null 2>&1

	# Get number of lines down to print mode
	movedown=$(($(echo "$RBUFFER" | wc -l) + ${1:-0}))

	# Get current row position
	echo -n "\e[6n"
	row="${${$(readuntil R)#*\[}%;*}"

	# Are we at the bottom of the terminal?
	if [ $((row+movedown)) -gt "$LINES" ]
	then
		# Scroll terminal up one line
		echo -n "\e[1S"

		# Move cursor up one line
		echo -n "\e[1A"
	fi

	# Save cursor position
	echo -n "\e[s"

	# Move cursor to start of line $movedown lines down
	echo -n "\e[$movedown;E"

	# Change font attributes
	echo -n "\e[1m"

	# Has a mode been set?
	if [ -n "$VIMODE" ]
	then
		# Print mode line
		echo -n "-- $VIMODE -- "
	else
		# Clear mode line
		echo -n "\e[0K"
	fi

	# Restore font
	echo -n "\e[0m"

	# Restore cursor position
	echo -n "\e[u"

	# Restore echo back
	stty echo > /dev/null 2>&1
}

clearmode() {
	VIMODE= showmode
}

#
# Temporary function to extend built-in widgets to display mode.
#
#   1: The name of the widget.
#
#   2: The mode string.
#
#   3 (optional): Beyond normal calculations, the number of additional
#   lines to move down before printing the mode.  Defaults to zero.
#
makemodal () {
	# Create new function
	eval "$1() { zle .'$1'; ${2:+VIMODE='$2'}; showmode $3 }"

	# Create new widget
	zle -N "$1"
}

# Extend widgets
makemodal vi-add-eol           INSERT
makemodal vi-add-next          INSERT
makemodal vi-change            INSERT
makemodal vi-change-eol        INSERT
makemodal vi-change-whole-line INSERT
makemodal vi-insert            INSERT
makemodal vi-insert-bol        INSERT
makemodal vi-open-line-above   INSERT
makemodal vi-substitute        INSERT
makemodal vi-open-line-below   INSERT 1
makemodal vi-replace           REPLACE
makemodal vi-cmd-mode          NORMAL

unfunction makemodal
setopt prompt_subst
PROMPT='${USER}@${HOST} %(5~,%-2~/.../%2~,%~)%  ${RESET}
$ ${RESET}' 

setopt nolistbeep       # beep off
setopt auto_cd          # directory名だけで移動可
setopt auto_pushd       # 移動したdirectoryを記録
setopt pushd_ignore_dups    # 同じdirectory名をpushdしない
setopt correct        # 入力したコマンド名が間違っている場合に修正
setopt list_packed    # 補完候補を詰めて表示

clear
