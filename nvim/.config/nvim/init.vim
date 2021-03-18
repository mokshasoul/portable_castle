" PLUGIN MANAGEMENT
" Install vim plug via: 
" curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
"     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.local/share/nvim/plugged')

Plug 'davidhalter/jedi-vim'
Plug 'tpope/vim-jdaddy'
Plug 'mrk21/yaml-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'
Plug 'sbdchd/neoformat'
Plug 'mboughaba/i3config.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tmhedberg/SimpylFold'
Plug 'mcchrish/nnn.vim'
Plug 'elzr/vim-json'
Plug 'aouelete/sway-vim-syntax'
Plug 'raghur/vim-ghost', {'do': ':GhostInstall'}
Plug 'junegunn/fzf.vim', {'do': { -> fzf#install() }}
Plug 'neomake/neomake'
Plug 'iCyMind/NeoSolarized'
Plug 'machakann/vim-highlightedyank'
Plug 'lervag/vimtex'


call plug#end()
" autocomplete settings (deoplete)
" options with :h deoplete-options
" let g:deoplete#enable_at_startup = 1

set iskeyword+=-                      	" treat dash separated words as a word text object"
set formatoptions-=cro                  " Stop newline continution of comments

syntax enable                           " Enables syntax highlighing
filetype plugin indent on
set colorcolumn=+1
set diffopt+=vertical
set list
set hidden                              " Required to keep multiple buffers open multiple buffers
set nowrap                              " Display long lines as just one line
set number
set ruler              			            " Show the cursor position all the time
set nobackup                            " This is recommended by coc
set nowritebackup                       " This is recommended by coc
set autochdir
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab smarttab autoindent
set encoding=utf-8
set title
set completeopt=longest,menuone,preview
set wildmode=longest:full,full
set cursorline
let mapleader = ","

colorscheme NeoSolarized
"set background=light
set termguicolors
" NeoFormat configurations
" Enable alignment
let g:neoformat_basic_format_align =1

" Enable tab to spaces conversion
let g:neoformat_basic_format_retab = 1

" Enable trimmming of trailing whitespace
let g:neoformat_basic_format_trim = 1

" disable autocompletion, cause we use deoplete for completion
let g:jedi#completions_enabled = 0

" open the go-to function in split, not another buffer
let g:jedi#use_splits_not_buffers = "right"

let g:neomake_python_enabled_makers = ['pylint']

" NNN integration
" Disable default mappings
let g:nnn#set_default_mappings = 0

"
" MAPS
"" Then set your own
nnoremap <silent> <leader>nn :NnnPicker<CR>
nnoremap <silent> <leader>w :w<CR>

set pastetoggle=<F6>
map <C-p> :FZF<CR>

set wildignore+=.git,.hg,.svn
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.rbc,*.class
set wildignore+=*.ai,*.bmp,*.gif,*.ico,*.jpg,*.jpeg,*.png,*.psd,*.webp
set wildignore+=*.avi,*.divx,*.mp4,*.webm,*.mov,*.m2ts,*.mkv,*.vob,*.mpg,*.mpeg
set wildignore+=*.mp3,*.oga,*.ogg,*.wav,*.flac
set wildignore+=*.eot,*.otf,*.ttf,*.woff
set wildignore+=*.doc,*.pdf,*.cbr,*.cbz
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz,*.kgb
set wildignore+=*.swp,.lock,.DS_Store,._*
" Or override
" Start nnn in the current file's directory
nnoremap <leader>n :NnnPicker '%:p:h'<CR>
"
"" YAML Settings
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

"
"" LaTex Setup
let g:tex_flavor = 'latex'
let g:tex_conceal = ''
let g:vimtex_fold_manual = 1
let g:vimtex_latexmk_continuous = 1
let g:vimtex_compiler_porgname = 'nvr'
let g:vimtex_view_method = 'zathura'

" Turn spellcheck on for markdown files
augroup auto_spellcheck
    autocmd BufNewFile,BufRead *.md setlocal spell
augroup END

""" Filetype-Specific Configurations

" HTML, XML, Jinja
autocmd FileType html setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType css setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType xml setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType htmldjango setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType htmldjango inoremap {{ {{  }}<left><left><left>
autocmd FileType htmldjango inoremap {% {%  %}<left><left><left>
autocmd FileType htmldjango inoremap {# {#  #}<left><left><left>

" Markdown and Journal
autocmd FileType markdown setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType journal setlocal shiftwidth=2 tabstop=2 softtabstop=2

" i3 config files
aug i3config_ft_detection
    au!
    au BufNewFile,BufRead ~/.config/i3/config set filetype=i3config
    au BufNewFile,BufRead ~/.config/sway/config set filetype=i3config
    au BufNewFile,BufRead ~/.config/sway/sway.d/* set filetype=i3config
aug end
