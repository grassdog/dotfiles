let mapleader = "\<Space>"
let maplocalleader = ","
let g:italic=1


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

" Better file manager via netrw
Plug 'tpope/vim-vinegar'

" More pairs and surround operations
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'

" Search currently highlighted term
Plug 'nelstrom/vim-visual-star-search'

" Handle line numbers when opening files
Plug 'bogado/file-line'

" More robust repeat
Plug 'tpope/vim-repeat'

" Theme
Plug 'dracula/vim'

" Whitespace cleanup
Plug 'axelf4/vim-strip-trailing-whitespace'

" Comment line
Plug 'tpope/vim-commentary'

" Tmux and testing
Plug 'preservim/vimux'
Plug 'vim-test/vim-test'

" Format other file types manually using neoformat
Plug 'sbdchd/neoformat'

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
set mat=3                   " Blink matching brackets for 3 tenths of a second


set nohlsearch              " Don't highlight matches
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

""""""""""""""""""
" Files to ignore
""""""""""""""""""

set wildignore+=*.swp                                   " Ignore vim backups
set wildignore+=.hg,.git,.svn                           " Version control
set wildignore+=*.aux,*.out,*.toc                       " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg          " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest        " compiled object files
set wildignore+=*.spl                                   " compiled spelling word lists
set wildignore+=*.sw?                                   " Vim swap files
set wildignore+=.DS_Store                               " OSX
set wildignore+=*COMMIT_EDITMSG

set wildignore+=*.obj,*.rbc,*.class,*.gem               " Disable output and VCS files
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz " Disable archive files

" Ignore bundler and sass cache
set wildignore+=**/vendor/gems/*,**/vendor/bundle/*,**/vendor/cache/*,**/.bundle/*,.sass-cache/*
set wildignore+=*/tmp/*
set wildignore+=*/spec/vcr/*
set wildignore+=*/coverage/*
set wildignore+=*.otf,*.woff,*.orig

set wildignore+=node_modules

set wildignore+=*.luac                                  " Lua byte code
set wildignore+=*.pyc                                   " Python byte code
set wildignore+=*.orig                                  " Merge resolution files
set wildignore+=*.6                                     " Ignore Go compiled files


colorscheme dracula

" This is required to force 24-bit color since I use a modern terminal.
set termguicolors

set splitright
set splitbelow

""""""""
" Netrw
""""""""

" No Netrw menu
let g:netrw_menu      = 0
let g:netrw_list_hide = '.DS_Store$'
let g:netrw_liststyle =0

" Preview in a vertical split
let g:netrw_preview   = 1
let g:netrw_winsize   = 30
" Use the same window for all netrw windows
" let g:netrw_chgwin=2

" Remove netrw history files
let g:netrw_dirhistmax = 0
" Don't change current dir when browsing to different dirs
let g:netrw_keepdir=1

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

" Tmux Test helpers
let test#strategy = "vimux"
map <Leader>tf :w <bar> TestFile<CR>
map <Leader>tl :w <bar> TestNearest<CR>
map <Leader>ta :wall <bar> TestSuite<CR>

map <Leader>. :wall <bar> call VimuxRunLastCommand()<CR>


function! BufferAdd()
  let buffer = input("Buffer to add => ")
  if buffer == ""
    return 0
  else
    exec "badd " . buffer
    bnext
  endif
endfun

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

" Stop page movement on shift arrow
vnoremap <S-Down> <Down>
vnoremap <S-Up> <Up>

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

" Don't need ex-mode. Just rerun a macro thanks
nnoremap Q @@

augroup GRASS
  " No whitespace strip in git commit messages
  au FileType gitcommit let b:strip_trailing_whitespace_enabled=0
augroup END


let g:neoformat_enabled_ruby = ['prettier-plugin-ruby', 'rufo', 'rubybeautify', 'rubocop']


augroup GRASS
  autocmd!

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  autocmd BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
        \| exe "normal! g`\"" | endif

  " Write all buffers once I lose focus
  autocmd FocusLost * :silent! wall
augroup END


" Use Neoformat to automatically format the following filetypes
augroup GRASS_FORMAT
  autocmd BufWritePre *.html undojoin | Neoformat
  autocmd BufWritePre *.js undojoin | Neoformat
  autocmd BufWritePre *.ts undojoin | Neoformat
  autocmd BufWritePre *.rb undojoin | Neoformat
augroup END


""""""""""""""""""
" Leader keybinds
""""""""""""""""""

" Yank to system clipboard
map <leader>y "*y
map <leader>p "*p

" Switch to last buffer
nnoremap <leader><tab> <c-^>

" Format the entire file
nnoremap <leader>ei m'ggVG=``zzg
nnoremap <leader>ef <cmd>Neoformat<cr>

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Managing buffers
nnoremap <silent> <leader>bd :bd!<CR>
nnoremap <silent> <leader>ba :call BufferAdd()<CR>

" Toggle search highlighting
noremap <silent><leader>/ :set hlsearch! hlsearch?<CR>

" Change to directory of current file
nmap <leader>cd :cd %:h<CR>
nmap <leader>lcd :lcd %:h<CR>

nmap <leader>g <Plug>NetrwRefresh


" Lua config
lua <<EOF

--[[
-- Notes:
--
-- When updating TreeSitter, you'll want to update the parsers using
-- :TSUpdate manually. Or, you can call :TSInstall to install new parsers.
-- Run :checkhealth nvim_treesitter to see what parsers are setup.
--]]

---------------
-- LSP Clients
---------------
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { "gopls" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup { on_attach = on_attach }
end

--------------
-- Treesitter
--------------
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
  },
}

-------------
-- Telescope
-------------
require("telescope").setup {
  defaults = {
    file_ignore_patterns = { "vendor/*", "node_modules/*" },
  }
}
EOF

