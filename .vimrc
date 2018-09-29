set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
"Core
Plugin 'flazz/vim-colorschemes'
Plugin 'sheerun/vim-polyglot'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-fugitive'
Plugin 'jiangmiao/auto-pairs'
Plugin 'sirver/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'airblade/vim-gitgutter'
Plugin 'vim-syntastic/syntastic'
Plugin 'tpope/vim-commentary'
Plugin 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer --racer-completer --tern-completer' }

call vundle#end()
filetype plugin indent on

"show unwanted whitespace, purge them with F8
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

filetype indent on
syntax on
set background=dark
set t_Co=256
colorscheme jellybeans

set encoding=utf8
set lazyredraw
set ttyfast
set hidden
set nobackup
set nowritebackup
set noswapfile
set autoread
set autoindent
set smartindent
set showmatch
set incsearch
set mouse=a
set number
set hlsearch

set foldenable
set foldlevelstart=9
set foldnestmax=9
set foldmethod=indent

set tabpagemax=50
set undolevels=10000

set notimeout
set ttimeout
set timeoutlen=100

set backspace=indent,eol,start
set statusline=
set expandtab smarttab shiftround nojoinspaces
set ruler
set textwidth=120
set tw=100
set ignorecase
set showmatch
set smartcase
set tabstop=4
set shiftwidth=4
set softtabstop=4
set pastetoggle=<F2>

"Keymappings
let mapleader="\<Space>"
nnoremap <Leader>/ :nohlsearch<Bar>:echo<CR>
nnoremap <Leader>s :%s/\<<C-r><C-w>\>//g<Left><Left>
nnoremap <Leader>b :ls<CR>:b<Space>
nnoremap <silent><Leader>j :set paste<CR>m`o<Esc>``:set nopaste<CR>
nnoremap <silent><Leader>k :set paste<CR>m`O<Esc>``:set nopaste<CR>
nnoremap <Leader>1 1gt
nnoremap <Leader>2 2gt
nnoremap <Leader>3 3gt
nnoremap <Leader>4 4gt
nnoremap <Leader>5 5gt
nnoremap <Leader>6 6gt
nnoremap <Leader>7 7gt
nnoremap <Leader>8 8gt
nnoremap <Leader>9 9gt

nmap <Leader>n :NERDTreeToggle<CR>
nmap <Leader>t :TagbarToggle<CR>
nmap <Leader>f :FZF <CR>
nmap <silent><Leader>ev :e $MYVIMRC<CR>
map <Leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>
map <Leader>x :YcmCompleter FixIt<CR>

map <S-TAB> :bprev<CR>
map tn :tabnew <CR>
map <F4> :e %:p:s,.h$,.X123X,:s,.cpp$,.h,:s,.X123X$,.cpp,<CR>
nnoremap <silent> <F8> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>
noremap ; :

imap <C-b> <Left>
imap <C-f> <Right>
imap <C-j> <Down>
imap <C-k> <Up>
imap <C-e> <End>
imap <C-a> <Home>

nnoremap <tab> %
vnoremap <tab> %

vnoremap < <gv
vnoremap > >gv

nnoremap j gj
nnoremap k gk

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_cpp_compiler = 'g++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'
let g:syntastic_python_python_exec = '/usr/local/bin/python3.6'
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"YouCompleteMe
autocmd CompleteDone * pclose
let g:ycm_autoclose_preview_window_after_completion=1
let g:ycm_global_ycm_extra_conf = '~/workspace/dotfiles/.ycm_extra_conf.py'
function! g:UltiSnips_Complete()
  call UltiSnips#ExpandSnippet()
  if g:ulti_expand_res == 0
    if pumvisible()
      return "\<C-n>"
    else
      call UltiSnips#JumpForwards()
      if g:ulti_jump_forwards_res == 0
        return "\<TAB>"
      endif
    endif
  endif
  return ""
endfunction

function! g:UltiSnips_Reverse()
  call UltiSnips#JumpBackwards()
  if g:ulti_jump_backwards_res == 0
    return "\<C-P>"
  endif

  return ""
endfunction

if !exists("g:UltiSnipsJumpForwardTrigger")
  let g:UltiSnipsJumpForwardTrigger = "<tab>"
endif

if !exists("g:UltiSnipsJumpBackwardTrigger")
  let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
endif

au InsertEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger     . " <C-R>=g:UltiSnips_Complete()<cr>"
au InsertEnter * exec "inoremap <silent> " .     g:UltiSnipsJumpBackwardTrigger . " <C-R>=g:UltiSnips_Reverse()<cr>"
