# My dotfiles

This directory contains the dotfiles for my system

## Requirements

Ensure you have the following installed on your system

### Git

```bash
# debian based system
sudo apt install git

# arch based system
sudo pacman -S git
```

### Stow

```bash
# debian based system
sudo apt install stow

# arch based system
sudo pacman -S stow
```

## Installation

First, check out the dotfiles repo in your $HOME directory using git

```bash
$ git clone git@github.com:vkardaras/dotfiles.git
$ cd dotfiles
```

then use GNU stow to create symlinks

```bash
# install symlink of a specific package
$ stow tmux

# install symlinks of all packages
$ stow */
```
