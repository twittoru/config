if [[ -z 0 ]]; then #{{{ from http://www.gfd-dennou.org/member/uwabami/cc-env/Zsh.html
#!/usr/bin/env zsh
# -*- mode: sh; coding: utf-8-unix; indent-tabs-mode: nil -*-
#  zshenv
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
### OS judge function
## Mac OS X
function is_darwin(){
  [[ $OSTYPE == darwin* ]] && return 0
  return 1
}
## Solaris
function is_solaris(){
  [[ $OSTYPE == solaris* ]] && return 0
  return 1
}
## Cygwin
function is_cygwin(){
  [[ $OSTYPE == cygwin* ]] && return 0
  return 1
}

### BASIC
# LANG
export LANG=ja_JP.UTF-8
# no local mailcheck
export MAILCHECK=0
# Debian specific
export DEBFULLNAME="Youhei SASAKI"
export DEBEMAIL="uwabami@gfd-dennou.org"
export KPKG_MAINTAINER=${DEBFULLNAME}
export KPKG_EMAIL=${DEBEMAIL}
export COLUMNS=${COLUMNS:-80}

### PATH settings
export PATH=/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:${PATH}

# for solaris (is needed?)
is_solaris && [ -d /opt/sfw ] && export PATH=/opt/sfw/bin:${PATH}
# for macports
is_darwin && \
    if [ -d /opt/mports ] ; then
    export PATH=/opt/local/bin:/opt/local/sbin:${PATH}
    export MANPATH=/opt/local/man:${MANPATH}
fi
## shareware
# for PGI compiler
[ -d /opt/pgi/linux86-64 ] && source ${ZDOTDIR}/load_PGI.zsh
# for Fujitsu Compiler
[ -d /opt/FJSVplang -o -d /usr/FFC ] && source ${ZDOTDIR}/load_Fujitsu.zsh
# for Intel Compiler
[ -d /opt/intel/Compiler/11.1/059 ] && source ${ZDOTDIR}/load_Intel.zsh
# for cuda
[ -d /usr/local/cuda ] && source ${ZDOTDIR}/load_CUDA.zsh

## LL settings
# for ruby
[ -d /opt/ruby-enterprise-1.8.7/bin ] && \
    export PATH=/opt/ruby-enterprise-1.8.7/bin:$PATH
[ -d ${HOME}/lib/site_ruby ] && \
    export RUBYLIB=${HOME}/lib/site_ruby:${RUBY_LIB}
[ -d ${HOME}/Library/site_ruby ] && \
    export RUBYLIB=${HOME}/Library/site_ruby:${RUBY_LIB}
[ -d /var/lib/gems/1.8 ] && \
    export PATH=/var/lib/gems/1.8/bin:${PATH}
[ -d /var/lib/gems/1.9.1 ] && \
    export PATH=/var/lib/gems/1.9.1/bin:${PATH}
export RI="--format ansi --width 70"
# for perl(CPAN)
if [ -d ${HOME}/Library/CPAN ]; then
    export PATH=${HOME}/Library/CPAN/bin:${PATH}
    export PERL5LIB=${HOME}/Library/CPAN/lib/perl5:${PERL5LIB}
    export PERL5LIB=${HOME}/Library/CPAN/lib/perl5/site_perl:${PERL5LIB}
    export MANPATH=${HOME}/Library/CPAN/man:${MANPATH}
fi
# for python
[ -d ${HOME}/Library/python ] && \
    export PYTHONPATH=${HOME}/Library/python:${PYTHONPATH}


## VCS
export CVS_RSH=ssh
if [ -d ${HOME}/Library/git ]; then
    export PATH=${HOME}/Library/git/bin:${PATH}
    export LD_LIBRARY_PATH=${HOME}/Library/git/lib:${LD_LIBRARY_PATH}
    export MANPATH=${HOME}/Library/git/share/man:${MANPATH}
fi


## misc.
# lv
export LV="-c -T8192"
# VTE terminal
export VTE_CJK_WIDTH=1
# for Chrome, Adobe Air
export GNOME_DESKTOP_SESSION=Default
# for OOffice, no using embedded bitmap fonts
export SAL_EMBEDDED_BITMAP_PRIORITY=0
# gtk locale (is needed?)
export G_FILENAME_ENCODING="@locale"
export G_BROKEN_FILENAMES="@locale"

# proxy
case ${HOST} in
    daphne)
        export http_proxy="http://localhost:20080"
        export https_proxy="http://localhost:20080"
        export ftp_proxy="http://localhost:20080"
        export soap_use_proxy="on"
        ;;
    camellia|yew|wisteria)
        export http_proxy="http://proxy.kuins.net:8080"
        export https_proxy="http://proxy.kuins.net:8080"
        export ftp_proxy="http://proxy.kuins.net:8080"
        export soap_use_proxy="on"
        ;;
    vmguest)
        export http_proxy="http://10.0.2.2:20080"
        export https_proxy="http://10.0.2.2:20080"
        export ftp_proxy="http://10.0.2.2:20080"
        export soap_use_proxy="on"
        ;;

    *)
        ;;
esac

## high priority path settings
[ -d ${HOME}/bin ] && export PATH=${HOME}/bin:${PATH}
[ -d ${HOME}/local/bin ] && export PATH=${HOME}/local/bin:${PATH}

# duplicate cleaning
typeset -U path cdpath fpatch manpath

# see ${ZDOTDIR}/.zshrc
fi  #}}}
if [[ -z 0 ]]; then #{{{ from mickey24's .zshenv
# .zshenv
export PATH=$HOME/local/bin:/opt/local/bin:/usr/local/bin:/usr/X11R6/bin:/usr/bin:/bin:/opt:local/sbin:/usr/local/sbin:/usr/sbin:/sbin
export MANPATH=$HOME/local/man:/opt/local/man:/usr/local/man:/usr/share/man:/usr/X11R6/man

export LANG=ja_JP.UTF-8
export LC_ALL="$LANG"
export EDITOR=vim
export LS_COLORS='no=0:fi=0:di=32:ln=36:ex=35'
export C_INCLUDE_PATH=/opt/local/include:$C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH=/opt/local/include:$CPLUS_INCLUDE_PATH

# for CPAN
export PATH=$HOME/local/perl/current/bin:$PATH
export MANPATH=$HOME/local/perl/current/man:$MANPATH
export PERL5LIB=$HOME/local/perl/current/lib/perl5:/System/Library/Perl/Extras/5.8.8:$PERL5LIB
fi  #}}}
