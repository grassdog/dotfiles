let mapleader = "\<Space>"
let maplocalleader = ","
let g:italic=1


" Plug plugin manager
call plug#begin(stdpath('data') . '/plugged')

" Telescope
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" Better Syntax parsing
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" Tree file plugin
Plug 'preservim/nerdtree'

" Better file manager via netrw
Plug 'tpope/vim-vinegar'

" Easy search and replace
Plug 'dkprice/vim-easygrep'

" Some git interaction
Plug 'airblade/vim-gitgutter'

" More pairs and surround operations
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'

" Search currently highlighted term
Plug 'bronson/vim-visual-star-search'

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

" Show keybindings (with icons)
Plug 'folke/which-key.nvim'
Plug 'echasnovski/mini.icons'

" Extra text object
Plug 'bkad/CamelCaseMotion'

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

" Use persistent undo
set undofile
set undolevels=1000         " Maximum number of changes that can be undone
set undoreload=10000        " Maximum number lines to save for undo on a buffer reload
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
set signcolumn=number         " Combine sign and number columns

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

" Place CamelCase motion behind `,`
" Forward or back or end cc word: ,w or ,b or or ,e or ,ge
" In or around cc word: i,w or a,w,
let g:camelcasemotion_key = ','

if !has("gui_running")
  " vim hardcodes background color erase even if the terminfo file does
  " not contain bce (not to mention that libvte based terminals
  " incorrectly contain bce in their terminfo files). This causes
  " incorrect background rendering when using a color theme with a
  " background color.
  "
  " see: https://github.com/kovidgoyal/kitty/issues/108
  let &t_ut=''
endif

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

" Automatically write all files when replacing
let g:EasyGrepReplaceWindowMode=2

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


let g:neoformat_enabled_ruby = ['prettier', 'rufo', 'rubybeautify', 'rubocop']


augroup GRASS
  autocmd!

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  autocmd BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
        \| exe "normal! g`\"" | endif

  " Write all buffers once I lose focus
  autocmd FocusLost * :silent! wall
augroup END


" Uncomment the following to use Neoformat to automatically format the following filetypes
"
" This is disabled as a default for now because performance when calling prettier it pretty
" rubbish.

" augroup GRASS_FORMAT
"   autocmd BufWritePre *.html undojoin | Neoformat
"   autocmd BufWritePre *.js undojoin | Neoformat
"   autocmd BufWritePre *.ts undojoin | Neoformat
"   autocmd BufWritePre *.rb undojoin | Neoformat
" augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col
        return "\<tab>"
    endif

    let char = getline('.')[col - 1]
    if char =~ '\k'
        " There's an identifier before the cursor, so complete the identifier.
        return "\<c-p>"
    else
        return "\<tab>"
    endif
endfunction
inoremap <expr> <tab> InsertTabWrapper()
inoremap <s-tab> <c-n>

" Shift-tab in insert mode goes backward
" inoremap <s-tab> <c-d>

""""""""""""""""""
" NERDTree config
""""""""""""""""""
let NERDTreeShowHidden = 1
let NERDTreeAutoDeleteBuffer=1
let NERDTreeMinimalUI = 1
let NERDTreeIgnore = ['.DS_Store$', '.git$']

augroup grass_nerdtree
  autocmd!

  " Exit Vim if NERDTree is the only window remaining in the only tab.
  autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
augroup END

" Lua config
lua <<EOF


--------------------
--- Yank higlighting
--------------------
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = "*",
})

---------------
--- Treesitter
---------------
require'nvim-treesitter.configs'.setup({
  -- install language parser
  -- :TSInstallInfo Command to view supported languages
  ensure_installed = {
    "ruby",
    "bash",
    "html",
    "css",
    "vim",
    "vimdoc",
    "lua",
    "luadoc",
    "markdown",
    "javascript",
    "typescript",
    "tsx"
  },

  -- Enable code highlighting
  highlight = {
    enable = true,
  },

   -- Enable incremental selection
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<CR>',
      node_incremental = '<CR>',
      node_decremental = '<BS>',
      scope_incremental = '<TAB>',
    }
  },

  -- Enable based on Treesitter Code formatting for (=).
  indent = {
    enable = true,
  },
})

-- Turn on Folding for Treesitter
vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
-- Don't collapse by default
vim.wo.foldlevel = 99


-------------
-- Telescope
-------------
require("telescope").setup({
  defaults = {
    file_ignore_patterns = { "vendor/*", "node_modules/*", ".git/COMMIT_EDITMSG" },
  }
})

-------------
-- Which key
-------------

-- Initialise icons. Icons rely on a NerdFont (https://www.nerdfonts.com) being used in terminal
require('mini.icons').setup()
require("which-key").setup {}

local wk = require("which-key")
wk.add({
  { "<leader>b", group = "Buffer" },
  { "<leader>ba", ":call BufferAdd()<cr>", desc = "Create buffer" },
  { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Find buffer" },
  { "<leader>bg", ":bd!<cr>", desc = "Delete buffer" },
  { "<leader>f", group = "File" },
  { "<leader>fT", ":NERDTreeFind<cr>", desc = "Find file in file tree" },
  { "<leader>fc", ":cd %:h<cr>", desc = "Change to directory of current file" },
  { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File" },
  { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Grep across files" },
  { "<leader>fl", ":lcd %:h<cr>", desc = "Change local window to directory of current file" },
  { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Find recent File" },
  { "<leader>ft", ":NERDTreeToggle<cr>", desc = "Toggle file tree" },
  { "<leader>g", group = "Git" },
  { "<leader>gP", "<Plug>(GitGutterPreviewHunk)", desc = "Preview hunk" },
  { "<leader>gg", ":GitGutterToggle<cr>", desc = "Toggle Git Gutter" },
  { "<leader>gn", "<Plug>(GitGutterNextHunk)", desc = "Next hunk" },
  { "<leader>gp", "<Plug>(GitGutterPrevHunk)", desc = "Previous hunk" },
  { "<leader>gs", "<Plug>(GitGutterStageHunk)", desc = "Stage hunk" },
  { "<leader>gu", "<Plug>(GitGutterUndoHunk)", desc = "Undo hunk" },
  { "<leader>h", group = "Help" },
  { "<leader>hh", "<cmd>Telescope help_tags<cr>", desc = "Search Help tags" },
  { "<leader>m", group = "Format" },
  { "<leader>mf", "<cmd>Neoformat<cr>", desc = "Format file" },
  { "<leader>mi", "m'ggVG=``zzg", desc = "Reindent file" },
  { "<leader>s", group = "Search" },
  { "<leader>t", group = "Test" },
  { "<leader>ta", ":wall <bar> TestSuite<cr>", desc = "Test all" },
  { "<leader>tf", ":w <bar> TestFile<cr>", desc = "Test file" },
  { "<leader>tl", ":w <bar> TestNearest<cr>", desc = "Test nearest to current line" },
  { "<leader>v", group = "Grep and Replace" },
  { "<leader>w", group = "Window" },
  { "<leader>wo", ":only<cr>", desc = "Show only the current window" },
  { "<leader>ww", "<c-w><c-p>", desc = "Switch to previous window" },
  { "<leader>.", ":wall <bar> call VimuxRunLastCommand()<cr>", desc = "Rerun last command" },
  { "<leader>/", ":set hlsearch! hlsearch?<cr>", desc = "Toggle search highlighting" },
  { "<leader><tab>", "<c-^>", desc = "Switch to the last buffer" },
  -- Cleaning up the docs for these but `<leader>sw` is the better command to use
  { "<leader>*", ":execute 'noautocmd vimgrep /\\V' . substitute(escape(expand(\"<cword>\"), '\\'), '\\n', '\\n', 'g') . '/ **'<CR>", desc = "Recursive search work under cursor" },
  { "<leader>*", ":<C-u>call VisualStarSearchSet('/')<CR>:execute 'noautocmd vimgrep /' . @/ . '/ **'<CR>", desc = "Search for visual selection", mode = "v" },
  {
    -- Normal and visual mode
    mode = { "n", "v" },
    { "<leader>p", '"+p', desc = "Paste from system clipboard" },
    { "<leader>y", '"+y', desc = "Yank into system clipboard" },
    { "<leader>sw", "<cmd>Telescope grep_string<cr>", desc = "Search word under cursor" },
  },
})
EOF

