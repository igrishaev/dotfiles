
brew-install:
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

brew-install-cli:
	brew install \
		git \
		leiningen \
		wget \
		mc \
		markdown \
		zsh \
		gettext \
		gpg \
		pinentry-mac \
		ctags

brew-install-app:
	brew cask install \
		1password \
		iterm2 \
		google-chrome \
		firefox
		slack \
		emacs \
		java \
		docker \
		kdiff3 \
		telegram \
		symboliclinker \
		vlc \
		obs \
		microsoft-excel \
		microsoft-word \
		sublime-text
