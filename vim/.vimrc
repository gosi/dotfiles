call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'mattn/emmet-vim'
call plug#end()

filetype plugin indent on
syntax on
highlight Comment ctermfg=green

runtime! macros/matchit.vim

set autoindent
set backspace=indent,eol,start
set clipboard=unnamedplus
set expandtab
set grepprg=LC_ALL=C\ grep\ -nrsH
set hidden
set hlsearch
set ignorecase
set incsearch
set mouse=a
set nobackup
set number
set path=.,**
set ruler
set shiftwidth=4
set softtabstop=4
set tabstop=8
set timeout timeoutlen=3000 ttimeoutlen=100
set wildmenu
set wildmode=list:longest,full

cmap w!! w !sudo tee % >/dev/null

nnoremap <Space>s :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>% :%s/\<<C-r>=expand("<cword>")<CR>\>/

vnoremap . :norm.<CR>
nnoremap riw ciw<C-r>0<Esc>:<C-u>let@/=@1<CR>:noh<CR>

inoremap <C-u> <C-g>u<C-u>
nnoremap <C-p> :ls<CR>:b<Space>
nnoremap <C-n> [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>
nnoremap <silent> <C-l> :silent noh<CR>

map <Leader>a ggVG"+y

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
