set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'mattn/emmet-vim'
Plugin 'wincent/Command-T'
Plugin 'L9'
Plugin 'FuzzyFinder'
call vundle#end()

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
