call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'mattn/emmet-vim'
Plug 'jiangmiao/auto-pairs'
Plug 'dracula/vim'
call plug#end()

filetype plugin indent on
syntax on
colorscheme dracula

let mapleader = "\<Space>"
let maplocalleader = "\\"
let @/ = ""
set autoindent
set backspace=indent,eol,start
set clipboard=unnamedplus
set complete+=d
set completeopt=menuone,preview
set expandtab
set grepprg=LC_ALL=C\ grep\ -nrsH
set hidden
set history=1000
set hlsearch
set ignorecase
set incsearch
set mouse=a
set noswapfile
set number
set path=.,**
set ruler
set shiftwidth=4
set smartcase
set smartindent
set tabstop=4
set tags=./tags;,tags;
set textwidth=90
set wildcharm=<TAB>
set wildignorecase
set wildmenu
set wildmode=list:longest,full

" remove trailing whitespace on write
autocmd BufWritePre * %s/\s\+$//e

" clear hlsearch
nnoremap <silent> <CR> :nohlsearch<CR><C-L>

" make Y consistent with C, S, D, etc.
nnoremap Y y$

" move by display lines
nnoremap j gj
nnoremap k gk

" center screen on search result
nnoremap n nzz
nnoremap N Nzz

" list navigation
nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz

" files
nnoremap <Leader>e :e <C-r>=expand('%:p:h').'/'<CR>
nnoremap <Leader>f :find *
cmap w!! w !sudo tee % >/dev/null
cmap s!! mksession! $HOME/.vim/session.vim<CR>

" buffers and splits
nnoremap <Leader>k :ls<CR>:b<Space>

" search and replace
nnoremap <Space>s :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>% :%s/\<<C-r>=expand("<cword>")<CR>\>/

" toggle paste mode when pasting from clipboard
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

" remove a file
command! -complete=file -nargs=1 Remove :echo 'Remove: '.'<f-args>'.' '.(delete(<f-args>) == 0 ? 'SUCCEEDED' : 'FAILED')

" rename current file
command! -bar -nargs=1 -bang -complete=file Rename :
  \ let s:file = expand('%:p') |
  \ setlocal modified |
  \ keepalt saveas<bang> <args> |
  \ if s:file !=# expand('%:p') |
  \   call delete(s:file) |
  \ endif |
  \ unlet s:file

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

