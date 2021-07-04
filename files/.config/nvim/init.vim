let mapleader = "\<Space>"
let maplocalleader = ","


" Plug plugin manager
call plug#begin(stdpath('data') . '/plugged')

" Telescope
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" Treesitter
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/playground'

" LSP
Plug 'neovim/nvim-lspconfig'

" Theme
Plug 'dracula/vim'

call plug#end()

""""""""""
" General
""""""""""

set nocompatible            " disable compatibility to old-time vi


set encoding=utf-8          " Set default encoding to UTF-8
scriptencoding utf-8
set hidden                  " Hide buffers, don't nag about them

set tabstop=2               " number of columns occupied by a tab character
set softtabstop=2           " see multiple spaces as tabstops so <BS> does the right thing
set expandtab               " converts tabs to white space
set shiftwidth=2            " width for autoindents
set autoindent              " indent a new line the same amount as the line just typed
set shiftround              " round indent to multiples of shiftwidth

" auto-wrap comments, auto insert comment header, allow formatting of comments with "gq"
" long lines are not broken in insert mode, don't break a line after a one letter word
" remove comment leader when joining lines
set formatoptions=crql1j

set showmatch               " show matching brackets.
set mat=3             " Blink matching brackets for 3 tenths of a second


set hlsearch                " Don't highlight matches
set incsearch               " incremental searching
set ignorecase              " searches are case insensitive...
set smartcase               " ... unless they contain at least one capital letter

set autoread                " Reload files that have not been modified
set undodir=~/.cache/nvim/undo/
set backup
set backupdir=~/.cache/nvim/backups
set noswapfile
set writebackup

" Display tabs and whitepace
set list
set listchars=tab:▸\ ,trail:·,extends:»,precedes:«

" backspace through everything in insert mode
set backspace=indent,eol,start  
set whichwrap+=<,>,h,l,[,]    " Allow left, right, bs, del to cross lines

set number                    " add line numbers
set scrolloff=3               " Always show at least three lines below cursor
set cursorline
set laststatus=2              " always show the status bar
set showmode
set shortmess=atIOT           " Abbrev. of messages (avoids 'hit enter')
set showcmd

set nofoldenable
set foldlevelstart=99

filetype plugin indent on     " allows auto-indenting depending on file type
syntax on                     " syntax highlighting

" Tab completion settings
set wildmode=list:longest     " Wildcard matches show a list, matching the longest first
set wildignore+=*.swp         " Ignore vim backups
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=.DS_Store                        " OSX
set wildignore+=*COMMIT_EDITMSG

set wildignore+=*.obj,*.rbc,*.class,*.gem        " Disable output and VCS files
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz " Disable archive files

" Ignore bundler and sass cache
set wildignore+=**/vendor/gems/*,**/vendor/bundle/*,**/vendor/cache/*,**/.bundle/*,.sass-cache/*,doc/**
set wildignore+=*/tmp/*
set wildignore+=*/spec/vcr/*
set wildignore+=*/coverage/*
set wildignore+=*.otf,*.woff,*.orig

set wildignore+=node_modules

set wildignore+=*.luac        " Lua byte code
set wildignore+=*.pyc         " Python byte code
set wildignore+=*.orig        " Merge resolution files
set wildignore+=*.6           " Ignore Go compiled files


colorscheme dracula

" This is required to force 24-bit color since I use a modern terminal.
set termguicolors

set splitright
set splitbelow

"""""""""""
" Commands
"""""""""""

" Edit hot files
command! Myrc :normal :edit ~/.config/nvim/init.vim<cr>

" Easier current directory in command mode
cnoremap <expr> %%  getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'

" A standalone function to set the working directory to the project’s root, or
" to the parent directory of the current file if a root can’t be found:
function! s:setcwd()
  let cph = expand('%:p:h', 1)
  if match(cph, '\v^<.+>://') >= 0 | retu | en
  for mkr in ['.git/', '.hg/', '.svn/', '.bzr/', '_darcs/', '.vimprojects']
    let wd = call('find'.(mkr =~ '/$' ? 'dir' : 'file'), [mkr, cph.';'])
    if wd != '' | let &acd = 0 | brea | en
  endfo
  exe 'lc!' fnameescape(wd == '' ? cph : substitute(wd, mkr.'$', '.', ''))
endfunction
command! SetProjectDir :call s:setcwd()


"""""""""""
" Movement 
"""""""""""

" Move across display lines, not physical lines
noremap j gj
noremap gj j
noremap k gk
noremap gk k
noremap <down> gj
noremap <up> gk

" Move around splits with <c-hjkl>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

nnoremap <s-down> <c-w>j
nnoremap <s-up> <c-w>k
nnoremap <s-left> <c-w>h
nnoremap <s-right> <c-w>l

" Faster start and end of line
nnoremap L $
nnoremap H ^

""""""""""
" Editing
""""""""""

" Shift-tab in insert mode goes backward
inoremap <s-tab> <c-d>

" Indent or outdent and maintain selection in visual mode
vnoremap >> >gv
vnoremap << <gv

" Yank to the end of the line
noremap Y y$

" Move past the end of lines in visual block edit
set virtualedit=block

""""""""""""""""""
" Leader keybinds
""""""""""""""""""

" Yank to system clipboard
map <leader>y "*y
map <leader>p "*p

" Switch to last buffer
nnoremap <leader><tab> <c-^>

" Format the entire file
nnoremap <leader>ef m'ggVG=``zzg

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Kill the current buffer
nnoremap <leader>bd   :bd<cr>

" Toggle search highlighting
noremap <silent><leader>/ :set hlsearch! hlsearch?<CR>

" Change to directory of current file
nmap <leader>cd :cd %:h<CR>
nmap <leader>lcd :lcd %:h<CR>

nmap <leader>g <Plug>NetrwRefresh

if has("nvim")
  lua require("vim-misc")
endif
