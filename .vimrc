" vim-plug
call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
"Plug 'w0rp/ale'
Plug 'justinmk/vim-dirvish'
Plug 'jpalardy/vim-slime'
Plug 'altercation/vim-colors-solarized'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
call plug#end()

" filetype support and colours
filetype plugin indent on
syntax on
set background=dark
colorscheme zellner

" hit `%` on `if` to jump to `else`
runtime macros/matchit.vim

" various settings
let mapleader = "\<Space>"
let @/ = ""
set autoindent
set backspace=indent,eol,start
set clipboard=unnamedplus
set complete+=d
set completeopt=menuone,preview
set diffopt+=vertical
set expandtab
set foldlevelstart=999
set foldmethod=indent
set formatoptions-=cro
set grepprg=LC_ALL=C\ grep\ -nrsH
set hidden
set history=1000
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
set tabstop=8
set tags=./tags;,tags;
set textwidth=90
set title
set titleold=Terminal
set ttimeout ttimeoutlen=100
set ttyfast
set wildcharm=<TAB>
set wildignorecase
set wildmenu

set wildmode=list:longest,full
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

" double tap Escape to remove search highlighting
nnoremap <Esc><Esc> :silent! nohls<CR>

" make Y consistent with C, S, D, etc.
nnoremap Y y$

" move by display lines
nnoremap j gj
nnoremap k gk

" center screen on search result
nnoremap n nzz
nnoremap N Nzz

" files
nnoremap <Leader>e :e <C-r>=expand('%:p:h').'/'<CR>
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>L :Lines<CR>
nnoremap <Leader>F :find *
cmap w!! w !sudo tee % >/dev/null

" buffers and splits
nnoremap <Leader>ls :ls<CR>:b<Space>
nnoremap <C-p>          :bprevious<CR>
nnoremap <C-n>          :bnext<CR>
nnoremap <Leader><TAB> <C-^>
nnoremap <TAB> <C-w>w
nnoremap <S-TAB> <C-w>p

" cd to file directory in current buffer
nnoremap <Leader>cd :lcd %:h<CR>
nnoremap <Leader>md :!mkdir -p %:p:h<CR>

" search and replace
nnoremap <Space><Space> :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>% :%s/\<<C-r>=expand("<cword>")<CR>\>/

" reselect previously yanked text
nnoremap gb `[v`]

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

" slime settings for tmux
let g:slime_default_config={'socket_name': 'default', 'target_pane': '{right-of}'}
let g:slime_paste_file=tempname()
let g:slime_target='tmux'

" better completion menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap        ,,      <C-n><C-r>=pumvisible() ? "\<lt>Down>\<lt>C-p>\<lt>Down>\<lt>C-p>" : ""<CR>
inoremap        ,:      <C-x><C-f><C-r>=pumvisible() ? "\<lt>Down>\<lt>C-p>\<lt>Down>\<lt>C-p>" : ""<CR>
inoremap        ,=      <C-x><C-l><C-r>=pumvisible() ? "\<lt>Down>\<lt>C-p>\<lt>Down>\<lt>C-p>" : ""<CR>

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
