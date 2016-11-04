#!/bin/bash

brew tap caskroom/cask

brew install \
     git \
     wget \
     curl \
     pandoc \
     ghc \
     mactex

brew cask install \
     iterm2 \
     sketch \
     skype \
     google-chrome \
     chromium \
     slack \
     pgadmin4 \
     emacs \
     docker \
     firefox \
     kdiff3 \
     virtualbox \
     torbrowser
