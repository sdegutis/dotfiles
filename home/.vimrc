" gui
set guifont=Menlo:h16
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar

" resize splits when window resizes
autocmd VimResized * wincmd =

" general
let mapleader = ","
syntax on
set number
set laststatus=2

" allow me to backspace
set backspace=2

" never word wrap
set nowrap

" tab stuff
set tabstop=2
set shiftwidth=2
set softtabstop=2
set autoindent
set expandtab
set smarttab

" search stuff
set incsearch
set hlsearch
set ignorecase
set smartcase
set gdefault

" move backup files out of the way
set directory=~/tmp,/var/tmp,/tmp

" get rid of search-highlight with ,d
nmap <silent> <Leader>d :noh<cr>

" ,q = strip trailing whitespace
nnoremap <leader>q :%s/\s\+$//e<cr>
