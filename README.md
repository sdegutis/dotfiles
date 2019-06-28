# sbdegutis's dotfiles

## Principles

- Installs dotfiles into $HOME via symlinks
- Simple bash script to install/uninstall
- Emacs installation installs itself when Emacs runs
  - Requires Emacs 26 probably
- Vim setup only exists for rare CLI situations
- Bash setup is similarly minimal
  - Assumes you have git and bash-completion installed

## Install

``` shell
$ cd dotfiles
$ ./run install
```

## Uninstall

``` shell
$ cd dotfiles
$ ./run clean
```

## License

MIT
