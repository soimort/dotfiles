set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'mattn/zencoding-vim'
Bundle 'wincent/Command-T'
Bundle 'L9'
Bundle 'FuzzyFinder'

filetype plugin indent on

set encoding=utf-8
set fileencodings=utf-8,gbk

set autoindent
set smartindent

set expandtab
set tabstop=4
set shiftwidth=4

set incsearch
set hlsearch
set ignorecase

set laststatus=2
set number

syntax on
