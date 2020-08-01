call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
call plug#end()

filetype plugin indent on
syntax on
set background=dark
colorscheme peachpuff
highlight Comment fg=green

" Allow tabs in Makefiles.
autocmd FileType make,automake set noexpandtab shiftwidth=8 softtabstop=8
" Trailing whitespace and tabs are forbidden, so highlight them.
highlight ForbiddenWhitespace ctermbg=red guibg=red
match ForbiddenWhitespace /\s\+$\|\t/
" Do not highlight spaces at the end of line while typing on that line.
autocmd InsertEnter * match ForbiddenWhitespace /\t\|\s\+\%#\@<!$/
" For trimming whitespace.
command! TrimWhitespace call TrimWhitespace()

set autoindent
set backspace=indent,eol,start
set cindent
set cinoptions=(0
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
" Grep recursively for word under cursor.
nmap <Leader>g :tabnew\|read !grep -Hnr '<C-R><C-W>'<CR>

nnoremap <Space>s :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>% :%s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap riw ciw<C-r>0<Esc>:<C-u>let@/=@1<CR>:noh<CR>
nnoremap <C-p> :ls<CR>:b<Space>
nnoremap <C-n> [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>
nnoremap <silent> <ESC><ESC> :silent noh<CR>
vnoremap . :norm.<CR>

vnoremap <F8> "+y
nnoremap <F8> "+y
nnoremap <silent> <F9> :set paste<CR>"+p :set nopaste<CR>

function! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfunction
