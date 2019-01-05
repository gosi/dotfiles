" filetype support and colours
filetype plugin indent on
syntax on
set background=dark
colorscheme default

" vim-plug
call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'tommcdo/vim-lion'
call plug#end()

" various settings
let mapleader = "\<Space>"
let @/ = ""
set autoindent
set backspace=indent,eol,start
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
set path=.,**
set ruler
set shiftround
set shiftwidth=4
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

" plugin settings
nnoremap <Leader>gs :Gstatus<CR><c-w>L
let b:lion_squeeze_spaces = 1

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

" useful black hole delete
nnoremap <silent> <Leader>d "_d
vnoremap <silent> <Leader>d "_d

" juggling with files
nnoremap <Leader>e :e<Space>
nnoremap <Leader>E :e <C-r>=expand('%:p:h').'/'<CR>
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
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>te :tabedit **/*
nnoremap <Leader>tf :tabfind *

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

" smooth grepping
command! -nargs=+ -complete=file_in_path -bar Grep silent! grep! <q-args> | redraw!

" smooth listing
cnoremap <expr> <CR> <SID>CCR()
function! s:CCR()
	command! -bar Z silent set more|delcommand Z
	if getcmdtype() == ":"
		let cmdline = getcmdline()
		    if cmdline =~ '\v\C^(dli|il)' | return "\<CR>:" . cmdline[0] . "jump   " . split(cmdline, " ")[1] . "\<S-Left>\<Left>\<Left>"
		elseif cmdline =~ '\v\C^(cli|lli)' | return "\<CR>:silent " . repeat(cmdline[0], 2) . "\<Space>"
		elseif cmdline =~ '\C^changes' | set nomore | return "\<CR>:Z|norm! g;\<S-Left>"
		elseif cmdline =~ '\C^ju' | set nomore | return "\<CR>:Z|norm! \<C-o>\<S-Left>"
		elseif cmdline =~ '\v\C(#|nu|num|numb|numbe|number)$' | return "\<CR>:"
		elseif cmdline =~ '\C^ol' | set nomore | return "\<CR>:Z|e #<"
		elseif cmdline =~ '\v\C^(ls|files|buffers)' | return "\<CR>:b"
		elseif cmdline =~ '\C^marks' | return "\<CR>:norm! `"
		elseif cmdline =~ '\C^undol' | return "\<CR>:u "
		else | return "\<CR>" | endif
	else | return "\<CR>" | endif
endfunction

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

" open cppman in a vertical tmux split by pressing K on a function
command! -nargs=+ Cppman silent! call system("tmux split-window -h cppman " . expand(<q-args>))
autocmd FileType cpp nnoremap <silent><buffer> K <Esc>:Cppman <cword><CR>

" C man pages with K
runtime! ftplugin/man.vim
