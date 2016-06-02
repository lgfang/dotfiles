" Created:  Fang lungang ??
" Modified: Fang Lungang 09/01/2015 13:40>

" To load this file, add "source path/to/lgfang.vimrc" to .vimrc
" General

set nocompatible " get out of horrible vi-compatible mode
set ignorecase

filetype on " detect the type of file
set history=1000 " How many lines of history to remember
set cf " enable error files and error jumping
set clipboard+=unnamed " turns out I do like is sharing windows clipboard
set ffs=unix,dos,mac " support all three, in this order
filetype plugin on " load filetype plugins
set viminfo+=! " make sure it can save viminfo
set isk+=_,$,@,%,#,- " none of these should be word dividers, so make them not be
runtime ftplugin/man.vim " enable :Man (\K)

" Theme/Colors

set background=dark " we are using a dark background
syntax on
" colorscheme molokai " or tango2

" Files/Backups

" set backup " make backup file
" set backupdir=$VIM\vimfiles\backup " where to put backup file
" set directory=$VIM\vimfiles\temp " directory is the directory for temp file
" set makeef=error.err " When using make, where should it dump the file

" Vim UI

"set mouse=a " use mouse everywhere. Doesn't work well with tmux
set wildmenu " turn on wild menu
set wildmode=list:longest,full " list all completions for wild menu
set lsp=0 " space it out a little more (easier to read)
set ruler " Always show current positions along the bottom
"set number " turn on line numbers
set lz " do not redraw while running macros (much faster) (LazyRedraw)
set hid " you can change buffer without saving
set backspace=2 " make backspace work normal
set whichwrap+=<,>,h,l  " backspace and cursor keys wrap to
set shortmess=atI " shortens messages to avoid 'press a key' prompt
set report=0 " tell us when anything is changed via :...
set noerrorbells " don't make noise
" make the splitters between windows be blank
set fillchars=vert:\ ,stl:\ ,stlnc:\

" Visual Cues

set showmatch " show matching brackets
set mat=1 " how many tenths of a second to blink matching brackets for
set hlsearch " highlight searched-for phrases
set incsearch " BUT do highlight as you type you search phrase
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<,eol:$ " what to show when I hit :set list
set so=10 " Keep 10 lines (top/bottom) for scope
set novisualbell " don't blink
set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [POS=%04l,%04v][%p%%]
set laststatus=2 " always show the status line

" Text Formatting/Layout

set fo=tcrqn " See Help (complex)
set ai " autoindent
set si " smartindent
set cindent " do c-style indenting
set tabstop=4 " tab spacing (settings below are just to unify it)
set softtabstop=4 " unify
set shiftwidth=4 " unify
set expandtab " tab to space. use ctrl-v<Tab> to insert tab
"set smarttab " use tabs at the start of a line, spaces elsewhere
set nowrap " do not wrap lines

" Minibuf

let g:miniBufExplTabWrap = 1 " make tabs show complete (no broken on two lines)
let g:miniBufExplModSelTarget = 1

" Matchit

let b:match_ignorecase = 1

" Autocommands

autocmd BufEnter * :syntax sync fromstart " ensure every file does syntax highlighting (full)
au BufNewFile,BufRead *.x :set ft=c " all my .x files are .h files

" Commands Ref
" :split split window, C-ww switch window, :close close current window
" C-^ switch buffer, :bd close buffer(buffer delete)
" :ls list buffers, :bn switch to buffer n, :sbn split and bn
" In editing mode, C-n/C-p auto-completion
" vim -d file1 file2 [file3]  diff files
" see man page: K or \K or :Man
" go to file at point: gf
" format code: select text in visual mode, then "="
" go to line upon file open: vim +linum filename
" make paste from terminal emulator work: "set paste"; when done "set nopaste".

" Thanks for .vimrc template from http://www.vi-improved.org/vimrc.php
