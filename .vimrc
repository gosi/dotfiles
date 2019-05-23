" vim-plug
"call plug#begin('~/.vim/plugged')
"Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
"Plug 'junegunn/fzf.vim'
"Plug 'tpope/vim-fugitive'
"Plug 'tpope/vim-surround'
"Plug 'jiangmiao/auto-pairs'
"Plug 'w0rp/ale'
"call plug#end()

" filetype support and colours
filetype plugin indent on
syntax on
set background=dark
set t_Co=256
colorscheme default

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
set formatoptions-=cro
set grepprg=LC_ALL=C\ grep\ -nrsH
set hidden
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set lazyredraw
set mouse=a
set noswapfile
set number
set path=.,**
set ruler
set shiftround
set shiftwidth=4
set showcmd
set smartcase
set softtabstop=4
set splitright
set statusline=\ %F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ (%l,%c%V)\ %P
set tabstop=8
set tags=./tags;,tags;
set textwidth=0
set ttimeout ttimeoutlen=100
set ttyfast
set wildignorecase
set wildmenu
set wildmode=full

" source vimrc on save.
autocmd! bufwritepost $MYVIMRC source $MYVIMRC

" remove trailing whitespace on write
autocmd BufWritePre * %s/\s\+$//e

" use :h<Space> to open vertical help splits
cabbrev h vert h

" keep selection when indenting text
vnoremap < <gv
vnoremap > >gv

" play a macro recorded to register q
nnoremap Q @q

" quick blank line from normal mode
nnoremap <Enter> o<Esc>

" double tap Escape to remove search highlighting
nnoremap <Esc><Esc> :silent! nohls<CR>

" make Y consistent with C, S, D, etc.
nnoremap Y y$

" center screen on search result
nnoremap n nzz
nnoremap N Nzz

" files
nnoremap <Leader>e :e <C-r>=expand('%:p:h').'/'<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>L :Lines<CR>
nnoremap <Leader>F :find *

" buffers
nnoremap <Leader>ls :ls<CR>:b<Space>
nnoremap <C-p>      :bprevious<CR>
nnoremap <C-n>      :bnext<CR>
nnoremap <BS>       :buffer#<CR>

" quick search and replace
nnoremap <Space>s :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>% :%s/\<<C-r>=expand("<cword>")<CR>\>/

" closing parens/brackets etc
inoremap " ""<left>
inoremap ' ''<left>
inoremap ( ()<left>
inoremap [ []<left>
inoremap { {}<left>
inoremap {<CR> {<CR>}<ESC>O
inoremap {;<CR> {<CR>};<ESC>O

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
