#!/bin/bash

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

brew tap caskroom/cask
brew tap homebrew/services

brew install \
     git \
     leiningen \
     mercurial \
     wget \
     cmake \
     gcc \
     mc \
     curl \
     markdown \
     zsh \
     ghc \
     postgresql \
     python3 \
     pandoc \
     mactex

brew cask install \
     1password \
     iterm2 \
     skype \
     google-chrome \
     chromium \
     slack \
     franz \
     pgadmin4 \
     emacs \
     java \
     docker \
     firefox \
     kdiff3 \
     virtualbox \
     sketch \
     torbrowser
