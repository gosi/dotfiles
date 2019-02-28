" vim-plug
call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'wellle/targets.vim'
Plug 'altercation/vim-colors-solarized'
call plug#end()

" filetype support and colours
filetype plugin indent on
syntax on
set background=dark
let base16colorspace=256  " Access colors present in 256 colorspace
colorscheme solarized

" various settings
let mapleader = "\<Space>"
let @/ = ""
set autoindent
set backspace=indent,eol,start
set clipboard=unnamedplus
set complete+=d
set diffopt+=vertical
set expandtab
set foldlevelstart=999
set foldmethod=indent
set grepprg=LC_ALL=C\ grep\ -nrsH
set hidden
set hlsearch
set ignorecase
set incsearch
set lazyredraw
set mouse=a
set noswapfile
set number
set relativenumber
set path=.,**
set ruler
set shiftround
set shiftwidth=4
set showcmd
set smartcase
set softtabstop=4
set splitright
set tabstop=8
set tags=./tags;,tags;
set textwidth=0
set ttimeout ttimeoutlen=100
set ttyfast
set wildignorecase
set wildmenu
set wildmode=full

" remove trailing whitespace on write
autocmd BufWritePre * %s/\s\+$//e

" use :h<Space> to open vertical help splits
cabbrev h vert h

" copy to the system clipboard
map <C-c> "+y

" Y should yank to EOL
map Y y$

" keep selection when indenting text
vnoremap < <gv
vnoremap > >gv

" play a macro recorded to register q
nnoremap Q @q

" quick blank line from normal mode
nnoremap <Enter> o<Esc>

" double tap Escape to remove search highlighting
nnoremap <Esc><Esc> :silent! nohls<CR>

" center screen on search result
nnoremap n nzz
nnoremap N Nzz

" move by screen line instead of file line
nnoremap j gj
nnoremap k gk

" tab for next matching item
nnoremap <tab> %
vnoremap <tab> %

" delete without saving to register
nnoremap <silent> <Leader>d "_d
vnoremap <silent> <Leader>d "_d

" juggling with files
nnoremap <Leader>e :e <C-r>=expand('%:p:h').'/'<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>L :Lines<CR>
nnoremap <Leader>F :find *
nnoremap <Leader>t :tabfind *
nnoremap <Leader>v :vert sfind *
nnoremap <Leader>w :w<CR>:echo "Written"<CR>
nnoremap <Leader>W :w !sudo tee % >/dev/null

" juggling with buffers
nnoremap <Leader>b  :buffer *
nnoremap <Leader>ls :ls<CR>:b<Space>
nnoremap <C-p>      :bprevious<CR>
nnoremap <C-n>      :bnext<CR>
nnoremap <BS>       :buffer#<CR>

" juggling with tabs
nnoremap tn :tabnew<CR>
nnoremap te :tabedit **/*
nnoremap tf :tabfind *

" juggling with definitions
nnoremap <Leader>D :dlist /
nnoremap [D [D     :djump<Space><Space><Space><C-r><C-w><S-Left><Left>
nnoremap ]D ]D     :djump<Space><Space><Space><C-r><C-w><S-Left><Left>

" juggling with matches
nnoremap <Leader>i :ilist /
nnoremap [I [I:ijump<Space><Space><Space><C-r><C-w><S-Left><Left><Left>
nnoremap ]I ]I:ijump<Space><Space><Space><C-r><C-w><S-Left><Left><Left>

" juggling with changes
nnoremap <Leader>, *``cgn
nnoremap <Leader>. #``cgN

" juggling with quickfix entries
nnoremap <End>  :cnext<CR>
nnoremap <Home> :cprevious<CR>

" super quick search and replace
nnoremap <Space>s :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>% :%s/\<<C-r>=expand("<cword>")<CR>\>/

" emacs-like editing in insert mode
inoremap <C-a> <Home>
inoremap <C-b> <Left>
inoremap <C-e> <End>
inoremap <C-f> <Right>
inoremap <C-j> <Down>
inoremap <C-k> <Up>

" incase Esc is not mapped to caps-lock on system
inoremap jk <Esc>

" smooth grepping
command! -nargs=+ -complete=file_in_path -bar Grep silent! grep! <q-args> | redraw!

" automagically set paste mode when pasting text
function! WrapForTmux(s)
  if !exists('$TMUX')
    return a:s
  endif

  let tmux_start = "\<Esc>Ptmux;"
  let tmux_end = "\<Esc>\\"

  return tmux_start . substitute(a:s, "\<Esc>", "\<Esc>\<Esc>", 'g') . tmux_end
endfunction

let &t_SI .= WrapForTmux("\<Esc>[?2004h")
let &t_EI .= WrapForTmux("\<Esc>[?2004l")

function! XTermPasteBegin()
  set pastetoggle=<Esc>[201~
  set paste
  return ""
endfunction

inoremap <special> <expr> <Esc>[200~ XTermPasteBegin()

" C man pages with K
runtime! ftplugin/man.vim
