call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'mattn/emmet-vim'
"Plug 'jiangmiao/auto-pairs'
"Plug 'ludovicchabant/vim-gutentags'
Plug 'Lokaltog/vim-distinguished'
Plug 'nanotech/jellybeans.vim'
call plug#end()

filetype plugin indent on
syntax on
"colorscheme jellybeans
set background=dark
highlight Pmenu ctermbg=gray guibg=gray
highlight StatusLine ctermbg=23 ctermfg=white guibg=lime guifg=white
autocmd VimEnter,BufWinEnter * syn match parens /[(){}]/ | hi parens ctermfg=red

set nocompatible
set backspace=indent,eol,start
set clipboard=unnamedplus
set history=50
set path=.,**
set ruler
set showcmd
set showmatch
set showmode
set lazyredraw
set hlsearch
set incsearch
set ignorecase
set nobackup
set nojoinspaces
set wildmenu
set wildmode=longest,list
set tabstop=8
set tags=./tags;,tags;
set expandtab
set shiftwidth=4
set softtabstop=4
set scrolloff=5
set grepprg=LC_ALL=C\ grep\ -nrsH
set mouse=a
set autoindent
set smartindent
set ttimeout ttimeoutlen=100
set laststatus=2
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ (%l,%c%V)\ %P
" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction

" list navigation
nnoremap <left>  :cprev<cr>zvzz
nnoremap <right> :cnext<cr>zvzz
nnoremap <up>    :lprev<cr>zvzz
nnoremap <down>  :lnext<cr>zvzz

" files and tags
nnoremap <Leader>e :e <C-r>=expand('%:p:h').'/'<CR>
nnoremap <Leader>f :find *
cmap w!! w !sudo tee % >/dev/null
cmap s!! mksession! $HOME/.vim/session.vim<CR>

" buffers and splits
nnoremap <Leader>b :ls<CR>:b<Space>
nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>

" search and replace
nnoremap <Space>s :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>% :%s/\<<C-r>=expand("<cword>")<CR>\>/

vnoremap . :norm.<CR>
nnoremap Y y$

nmap <silent> <F2> :execute "vimgrep /" . expand("<cword>") . "/j **" <Bar> cw<CR>
nmap <silent> <F3> :!git diff %<CR>
nmap <silent> <F4> :Vexplore!<CR>
nmap <silent> <F5> :silent !ctags -R<CR>

nmap <C-P> [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>
nmap <silent> <C-N> :silent noh<CR>

" for when I want completion
inoremap (( ()<Left>
inoremap )) (<CR>)<C-c>O
inoremap {{ {}<Left>
inoremap }} {<CR>}<C-c>O
inoremap [[ []<Left>
inoremap ]] [<CR>]<C-c>O
inoremap "" ""<Left>
inoremap '' ''<Left>
inoremap `` ``<Left>

autocmd BufNewFile,BufRead *.ts set syntax=javascript

function! <SID>StripWhitespace() abort
    let pos = getcurpos()
    %s/\s\+$//e
    %s#\($\n\s*\)\+\%$##e
    call setpos('.', pos)
endfunction
augroup strip_trailing_whitespace
    autocmd!
    autocmd BufWritePre * call <SID>StripWhitespace()
augroup END

com!  -nargs=* -bar -bang -complete=dir  Lexplore  call netrw#Lexplore(<q-args>, <bang>0)

fun! Lexplore(dir, right)
  if exists("t:netrw_lexbufnr")
  " close down netrw explorer window
  let lexwinnr = bufwinnr(t:netrw_lexbufnr)
  if lexwinnr != -1
    let curwin = winnr()
    exe lexwinnr."wincmd w"
    close
    exe curwin."wincmd w"
  endif
  unlet t:netrw_lexbufnr

  else
    " open netrw explorer window in the dir of current file
    " (even on remote files)
    let path = substitute(exists("b:netrw_curdir")? b:netrw_curdir : expand("%:p"), '^\(.*[/\\]\)[^/\\]*$','\1','e')
    exe (a:right? "botright" : "topleft")." vertical ".((g:netrw_winsize > 0)? (g:netrw_winsize*winwidth(0))/100 : -g:netrw_winsize) . " new"
    if a:dir != ""
      exe "Explore ".a:dir
    else
      exe "Explore ".path
    endif
    setlocal winfixwidth
    let t:netrw_lexbufnr = bufnr("%")
  endif
endfun
map <silent> <C-E> :Lexplore<CR>
" absolute width of netrw window
let g:netrw_winsize = -40

" do not display info on the top of window
let g:netrw_banner = 0

" tree-view
let g:netrw_liststyle = 3

" sort is affecting only: directories on the top, files below
let g:netrw_sort_sequence = '[\/]$,*'

" use the previous window to open file
let g:netrw_browse_split = 4
