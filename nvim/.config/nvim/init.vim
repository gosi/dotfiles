call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'airblade/vim-gitgutter'
Plug 'jiangmiao/auto-pairs'
Plug 'luochen1990/rainbow'
Plug 'mattn/emmet-vim'
Plug 'w0rp/ale'
Plug 'jpalardy/vim-slime'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'easymotion/vim-easymotion'
Plug 'scrooloose/nerdtree'
Plug 'morhetz/gruvbox'
Plug 'leafgarland/typescript-vim'
Plug 'neoclide/vim-jsx-improve'
Plug 'othree/html5.vim'
call plug#end()

" slime settings for tmux
let g:slime_default_config={'socket_name': 'default', 'target_pane': '{right-of}'}
let g:slime_paste_file=tempname()
let g:slime_target='tmux'

filetype plugin indent on
syntax on
set background=dark
colorscheme gruvbox
set termguicolors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" hit `%` on `if` to jump to `else`
runtime macros/matchit.vim

" Basic settings
let mapleader = "\<Space>"
let maplocalleader = "\\"
let @/ = ""
set autoindent
set backspace=indent,eol,start
set clipboard=unnamedplus
set complete+=d
set completeopt=menuone,preview
set diffopt+=vertical
set expandtab
set foldmethod=indent
set foldlevelstart=0
set formatoptions-=cro
set grepprg=LC_ALL=C\ grep\ -nrsH
set hidden
set history=1000
set hlsearch
set inccommand=split
set ignorecase
set incsearch
set lazyredraw
set mouse=a
set nofoldenable
set noswapfile
set number
set path=.,**
set ruler
set shiftround
set shiftwidth=4
set showcmd
set smartcase
set smartindent
set softtabstop=4
set splitright
set tabstop=8
set tags=./tags;,tags;
set textwidth=90
set timeout timeoutlen=3000 ttimeoutlen=100
set ttyfast
set wildcharm=<TAB>
set wildignorecase
set wildmenu
set wildmode=list:longest,full

" statusline
set statusline=%<\ %n:%f\ %m%r%y%=%-35.(line:\ %l\ of\ %L,\ col:\ %c%V\ (%P)%)

" indicate where lines are wrapping
set linebreak
set showbreak=››\

" source vimrc on save.
autocmd! bufwritepost $MYVIMRC source $MYVIMRC

" autoquit if only nerdtree is open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" use :h<Space> to open vertical help splits
cabbrev h vert h

" keep selection when indenting text
vnoremap < <gv
vnoremap > >gv

" in insert mode, convert current word to uppercase
inoremap <C-u> <Esc>mzgUiw`za

" incase caps lock is not bound to Escape
inoremap jk <Esc>

" play a macro recorded to register 'q'
nnoremap Q @q

" find variable usage
nnoremap <C-P> [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<CR><C-L>
endif

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
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>L :Lines<CR>
nnoremap <Leader>F :find *
nnoremap <Leader>w :w<CR>
cmap w!! w !sudo tee % >/dev/null
cmap s!! mksession! $HOME/.vim/session.vim<CR>

" buffers and splits
nnoremap <Leader>b :ls<CR>:b<Space>
nnoremap H :bprevious<CR>
nnoremap L :bnext<CR>
nnoremap <Leader><TAB> <C-^>
nnoremap s <C-w>
nnoremap S <C-w><C-w>
nnoremap <C-b> :NERDTreeToggle<CR>

" tabs
nnoremap <Leader>( :tabnext<CR>
nnoremap <Leader>) :tabprevious<CR>

" delete without yanking
nnoremap <Leader>d "_d
vnoremap <Leader>d "_d

" move lines up and down
nnoremap <C-j> :m .+1<CR>==
nnoremap <C-k> :m .-2<CR>==
inoremap <C-j> <Esc>:m .+1<CR>==gi
inoremap <C-k> <Esc>:m .-2<CR>==gi
vnoremap <C-j> :m '>+1<CR>gv=gv
vnoremap <C-k> :m '<-2<CR>gv=gv

" copy file path to system clipboard
nnoremap <C-g> :let @+ = expand("%:p")<CR>

" replace currently selected text with default register without yanking it
vnoremap <Leader>p "_dP

" cd to file directory in current buffer
nnoremap <Leader>cd :lcd %:h<CR>
nnoremap <Leader>md :!mkdir -p %:p:h<CR>

" search and replace
nnoremap <Space>s :'{,'}s/\<<C-r>=expand("<cword>")<CR>\>/
nnoremap <Space>% :%s/\<<C-r>=expand("<cword>")<CR>\>/

" reselect previously yanked text
nnoremap gV `[v`]

" search highlighted text
vnoremap // y/\V<C-R>=escape(@",'/\')<CR><CR>

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

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
if has('patch8.1.1068')
  " Use `complete_info` if your (Neo)Vim version supports it.
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
else
endif

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

" remove whitespace on write
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

" vim-fugitive {{{
nnoremap <Leader>gs :<C-u>Gstatus<CR>
nnoremap <Leader>gc :<C-u>Gcommit -v<CR>
nnoremap <Leader>gl :<C-u>QuickRun sh -src 'git log --graph --oneline'<CR>
nnoremap <Leader>ga :<C-u>Gwrite <Bar> GitGutter<CR>
nnoremap <Leader>gd :<C-u>Gdiff<CR>
nnoremap <Leader>gb :<C-u>Gblame<CR>
