if has('nvim')
    set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor
    set inccommand=nosplit
end

" colours and syntax
set nocompatible
filetype plugin indent on
syntax enable
set termguicolors
set t_Co=256
colorscheme wallaby
set background=dark

" user-defined prefix
let g:mapleader=","
let g:maplocalleader=","

" swap ; & :
noremap : ;
noremap ; :
noremap @; @:
noremap @: @;

" encoding
set encoding=utf-8
set termencoding=utf-8
set fileencoding=utf-8

" file behaviour
set nobackup
set noswapfile

" indentation
set autoindent
set tabstop=4 shiftwidth=4 softtabstop=4
set shiftround
set expandtab
set smarttab
set breakindent

" text wrapping
set wrap
set winwidth=30
set wrapscan
set showmatch
set textwidth=0

" status line
set ruler
set laststatus=2
set rulerformat=%45(%12f%=\ %m%{'['.(&fenc!=''?&fenc:&enc).']'}\ %l-%v\ %p%%\ [%02B]%)
set statusline=%f:\ %{substitute(getcwd(),'.*/','','')}\ %m%=%{(&fenc!=''?&fenc:&enc).':'.strpart(&ff,0,1)}\ %l-%v\ %p%%\ %02B

" scroll margin
set scrolloff=5

" case sensitive with capital letters
set ignorecase
set smartcase

" searching
set hlsearch
set incsearch

" backspace for insert mode
set backspace=indent,eol,start

" don't comment on new line
set formatoptions-=cro

" hide files when switching files
set hidden

" enable mouse
set mouse=a

" use system clipboard
set clipboard=unnamed

" folding
set foldenable
set foldmethod=marker

" read file when saved
set autoread

" cross lines with h & l
set whichwrap +=h
set whichwrap +=l

" command history size
set history=100

" command line window size
set cmdwinheight=3

" list header
set formatlistpat&
let &formatlistpat .= '\|^\s*[*+-]\s*'

" extend completion menu
set wildmenu
set wildmode=full

" sudo write
cmap W w !sudo tee > /dev/null %

" make <C-c> the same as <Esc>
inoremap <C-c> <Esc>

" keep selection when indenting text
vnoremap < <gv
vnoremap > >gv

" play a macro recorded to register q
nnoremap Q @q

" insert space from normal mode
nnoremap <C-Space> i<Space><Esc><Right>

" new line from normal mode
nnoremap <Enter> o<Esc>

" make Y consistent with C, S, D, etc.
nnoremap Y y$

" center screen on search result
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zvzz
nnoremap # *zvzz

" move by screen line instead of file line
nnoremap j gj
nnoremap k gk

" tab for next matching item
nnoremap <tab> %
vnoremap <tab> %

" blank line movement
nnoremap <silent><C-j> :<C-u>keepjumps normal! }<CR>
nnoremap <silent><C-k> :<C-u>keepjumps normal! {<CR>
vnoremap <C-j> }
vnoremap <C-k> {

" move screen
nnoremap gh H
nnoremap gl L
nnoremap gm M
vnoremap gh H
vnoremap gl L
vnoremap gm M

" nice line movements
nnoremap H g^
nnoremap L g$
vnoremap H g^
vnoremap L g$
nnoremap gH ^
nnoremap gL $
vnoremap gH ^
vnoremap gL $

" Emacs-like bindings for cmd and insert mode.
inoremap <C-e> <END>
cnoremap <C-e> <END>
inoremap <C-a> <HOME>
cnoremap <C-a> <HOME>
"inoremap <silent><expr><C-b> pumvisible() ? "\<C-y>\<Left>" : "\<Left>"
"inoremap <silent><expr><C-f> pumvisible() ? "\<C-y>\<Right>" : "\<Right>"
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
inoremap <C-d> <Del>
cnoremap <expr><C-d> len(getcmdline()) == getcmdpos()-1 ? "\<C-d>" : "\<Del>"
inoremap <silent><expr><C-k> "\<C-g>u".(col('.') == col('$') ? '<C-o>gJ' : '<C-o>D')
cnoremap <C-k> <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>

" change buffer
nnoremap <silent><C-n>   :<C-u>bnext<CR>
nnoremap <silent><C-p>   :<C-u>bprevious<CR>
nnoremap <BS>            :buffer#<CR>

" <C-w> -> s
nmap s <C-w>

" delete all other windows
nnoremap <C-w>O <C-w>o

" search text by cursor (new window)
nnoremap <C-w>*  <C-w>s*
nnoremap <C-w>#  <C-w>s#

" remove any delay using <Esc>
augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
augroup END

" delete current buffer
function! s:delete_current_buf()
    let bufnr = bufnr('%')
    bnext
    if bufnr == bufnr('%') | enew | endif
    silent! bdelete! #
endfunction
nnoremap <C-w>d :<C-u>call <SID>delete_current_buf()<CR>

" don't use register for x
nnoremap x "_x

" save mark function to gm
noremap gm m

" insert a character without insert mode
nnoremap <silent><expr>m 'i'.nr2char(getchar())."\<Esc>"

" Clear highlight.
function! s:hier_clear()
  if exists(':HierClear')
    HierClear
  endif
endfunction
nnoremap <silent> <Esc><Esc> :<C-u>nohlsearch<CR>:<C-u>call <SID>hier_clear()<CR>


" change window size with arrow keys
nnoremap <silent><Down>  <C-w>-
nnoremap <silent><Up>    <C-w>+
nnoremap <silent><Left>  <C-w><
nnoremap <silent><Right> <C-w>>

" tabs
nnoremap ge :<C-u>tabedit<Space>
nnoremap gn :<C-u>tabnew<CR>
nnoremap <silent>gx :<C-u>tabclose<CR>

" get highlight group under cursor
command! -nargs=0 GetHighlightingGroup
            \ echo 'hi<' . synIDattr(synID(line('.'),col('.'),1),'name') . '>trans<'
            \ . synIDattr(synID(line('.'),col('.'),0),'name') . '>lo<'
            \ . synIDattr(synIDtrans(synID(line('.'),col('.'),1)),'name') . '>'

" welcome message
augroup InitialMessage
    autocmd!
    autocmd VimEnter * echo "(U^w^) enjoy (n)vimming!"
augroup END

" source vimrc on save.
autocmd! bufwritepost $MYVIMRC source $MYVIMRC

" remove trailing whitespace on write
autocmd BufWritePre * %s/\s\+$//e

" determine how to open help by looking at the vertical and horizontal width
command! -nargs=* -complete=help Help call <SID>smart_help(<q-args>)
set keywordprg=:Help
nnoremap <silent><Leader>h :<C-u>Help<Space><C-l>
function! s:smart_help(args)
    try
        if winwidth(0) > winheight(0) * 2
            " vertical split
            execute 'vertical topleft help ' . a:args
        else
            execute 'aboveleft help ' . a:args
        endif
    catch /^Vim\%((\a\+)\)\=:E149/
        echohl ErrorMsg
        echomsg "E149: Sorry, no help for " . a:args
        echohl None
    endtry
    if &buftype ==# 'help'
        " if you can't secure the width, open with tab
        if winwidth(0) < 80
            execute 'quit'
            execute 'tab help ' . a:args
        endif
        silent! AdjustWindowWidth --direction=shrink
    endif
endfunction

function! s:start_term(args) abort
    if exists('s:term_win')
        let winnr = win_id2win(s:term_win)
        if winnr > 0
            execute winnr . 'wincmd w'
            let job_id = &channel
            if job_id != 0 && a:args !=# ''
                call jobsend(job_id, [a:args])
            endif
            startinsert
            return
        endif
    endif

    if exists('s:term_bufnr') && bufexists(s:term_bufnr)
        execute 'buffer' s:term_bufnr
        if a:args !=# ''
            call jobsend(getbufvar(s:term_bufnr, '&channel'), [a:args])
        endif
        startinsert
        return
    endif

    hi link NormalFloat Normal

    execute 'terminal' a:args
    nnoremap <buffer>q :<C-u>quit<CR>
    startinsert
    let s:term_bufnr = bufnr('%')
endfunction
nnoremap <Leader>t :<C-u>call <SID>start_term('')<CR>
command! -nargs=* -complete=shellcmd T call <SID>start_term(<q-args>)
" leave from terminal mode
tnoremap <Esc><Esc> <C-\><C-n>

" auto set paste mode when pasting text
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

" =============================================================================
" # PLUGINS
" =============================================================================
call plug#begin()
" GUI enhancements
Plug 'itchyny/lightline.vim'
Plug 'w0rp/ale'
Plug 'machakann/vim-highlightedyank'
Plug 'andymass/vim-matchup'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
Plug 'rhysd/wallaby.vim'

" Git
Plug 'airblade/vim-gitgutter'

" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Semantic language support
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

" Completion plugins
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-tmux'
Plug 'ncm2/ncm2-path'

" Syntactic language support
Plug 'godlygeek/tabular'

call plug#end()

" airline
let g:airline_theme = 'wombat'
let g:airline_left_sep = '»'
let g:airline_right_sep = '«'
let g:airline_extensions = []

" Completion
autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
" tab to select
" and don't hijack my enter key
inoremap <expr><Tab> (pumvisible()?(empty(v:completed_item)?"\<C-n>":"\<C-y>"):"\<Tab>")
inoremap <expr><CR> (pumvisible()?(empty(v:completed_item)?"\<CR>\<CR>":"\<C-y>"):"\<CR>")
