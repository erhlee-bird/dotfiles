" vimrc: erhlee.bird@gmail.com
    " Intro {"{{{
        " Authored by: erhlee.bird@gmail.com

        set nocompatible
    " }"}}}

    " Vundle Configuration {"{{{
        filetype off
        set rtp+=~/.vim/bundle/Vundle.vim
        call vundle#begin()

        " Vundle Plugins {
            Plugin 'VundleVim/Vundle.vim'
            Plugin 'scrooloose/syntastic'
            Plugin 'junegunn/fzf'
            Plugin 'junegunn/fzf.vim'
            Plugin 'tpope/vim-commentary'
        " }

        call vundle#end()
        filetype plugin indent on
    " }"}}}

    " General {"{{{
        syntax enable                     " Enable syntax highlighting
        scriptencoding utf-8

        set shortmess+=filmnrxoOtT        " Abbreviates messages and avoids
                                          " 'hit enter'

        set viewoptions=folds,options,    " Compatibility
            \ cursor,unix,slash

        set winminheight=0                " Allow splits to be 0 lines high

        if has('mouse')
            set mouse=a                   " Enable Mouse Usage
            set mousehide                 " Hide the Mouse Cursor while typing

            map <ScrollWheelUp> <C-Y>
            map <ScrollWheelDown> <C-E>
        endif

        if has('clipboard')
            if has('unnamedplus')
                set clipboard=unnamedplus " + register for copy-paste
            else
                set clipboard=unnamed     " * register for copy-paste
            endif
        endif

        set backup                        " Persistent backup

        if has('persistent_undo')
            set undofile                  " Allow for persistent undo
            set undolevels=150            " How many undo changes to store
            set undoreload=1000
        endif

        set virtualedit=onemore           " Entering insert mode after last

        set autochdir                     " Automatically change to current
                                          " working directory

        set number                        " Line numbering

        set hlsearch                      " Highlight search results
        set incsearch                     " Show search result as you type
        set ignorecase                    " Case insensitive search
        set smartcase                     " Case sensitive search when capital

        set showmatch                     " Show matching brackets/parens
        set matchtime=3                   " Matches blink

        set cursorline                    " Highlight the current line
        highlight clear SignColumn

        set colorcolumn=+2                " Highlight the column at textwidth

        set wildmode=list:longest,full    " Match the list up to the longest
                                          " common and the match fully
        set wildmenu                      " Expanded command-line completion

        if !&scrolloff
            set scrolloff=1               " Lines to keep on the screen
        endif
        if !&sidescrolloff
            set sidescrolloff=5           " Characters to keep on the screen
        endif

        set whichwrap=b,s,h,l             " Movement to next/previous line
                                          " with backspace, space, h, and l

        set foldenable                    " Fold code
        set foldcolumn=1                  " Show fold status in the gutter
        set foldmethod=marker             " Choose how vim folds

        set display+=lastline             " Always show the last line

        set autoread                      " Auto read when a file changes

        set timeoutlen=200                " Reduce ESC delay

        set hidden                        " Hides buffers

        set ttyfast                       " Faster terminal connections
    " }"}}}

    " Ruler and Status Line"{{{
        set showmode                      " Show mode in last line
        set showcmd                       " Show current command in last line

        if has('cmdline_info')
            set ruler
            set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%)
        endif

        if has('statusline')
            set laststatus=2                            " Always show status

            set statusline=%f                           " filename
            set statusline+=\ [%{&ff}/%Y]               " filetype
            set statusline+=\ [%.20{getcwd()}]          " current directory
            set statusline+=\ %m%r                      " options
            set statusline+=%=                          " right aligned items
            set statusline+=[%{&fo}]
            set statusline+=\ %-14.(%l\|%c%V\[%p%%\]%)  " row/col[percentage]
        endif

        set title                         " Allow vim to set the window title
    " }"}}}

    " Formatting {"{{{
        set formatoptions-=t              " Don't wrap based on textwidth
        set formatoptions-=c              " Don't wrap comments automatically
        set formatoptions-=r              " No comment when using <CR>
        set formatoptions-=o              " Comment continues with 'o'
        set formatoptions+=n              " Recognize numbered lists
        "set formatoptions+=l              " Long lines not broken in insert

        set backspace=indent,eol,start    " Better backspace options

        set linespace=0                   " No extra space between lines
        set textwidth=79                  " Maximum width of text
        set nojoinspaces                  " Prevents inserting two spaces after
                                          " punctuation on a join
        set nowrap                        " Disallow visual word wrapping
        "set linebreak                     " Only break lines at breakat char
        "set nolist                        " List disables linebreak
        set splitright                    " New vsplit windows go to the right
        set splitbelow                    " New split windows go to the bottom

        " Default Tab {
            set autoindent                " Indent same level as previous line
            set smarttab                  " Smarter indentation levels
            set shiftwidth=4              " Use indents of 4 spaces
            set expandtab                 " Tabs are spaces, not tabs
            set tabstop=4                 " An indentation every four columns
            set softtabstop=4             " Let backspace delete indent
        " }
    " }"}}}

    " Keybindings {"{{{
        " Reassign mapleader
        nnoremap <Space> <Nop>
        let mapleader=" "

        noremap <Leader><Space> :Buffers!<CR>
        noremap <Leader>f :GFiles!<CR>
        noremap <Leader>w :w<CR>
        noremap <Leader>x :q<CR>
        noremap <Leader>/ :Rg!<CR>
        noremap <Leader>; gcc

        " Easier split navigation
        noremap <Leader>v :vsp<CR>
        noremap <Leader>V :sp<CR>
        noremap <Leader>h <C-w>h
        noremap <Leader>j <C-w>j
        noremap <Leader>k <C-w>k
        noremap <Leader>l <C-w>l
        noremap <Leader>H <C-w>H
        noremap <Leader>J <C-w>J
        noremap <Leader>K <C-w>K
        noremap <Leader>L <C-w>L

        " Switch to last buffer
        noremap <Left> :bp<CR>
        noremap <Right> :bn<CR>

        " Clear search highlights
        noremap <silent><Leader>? :noh<CR>

        " Save with root permissions
        cabbrev w!! %!sudo tee > /dev/null %

        " Improve navigation on wrapped lines
        nnoremap j gj
        nnoremap k gk

        " Make Y work as other capitals do
        map Y y$

        " Reselect visual blocks after indent/outdent
        " vnoremap <S-Tab> <gv
        " vnoremap <Tab> >gv

        " Space toggles folds
        " nnoremap <Space> za
        " vnoremap <Space> za

        " Swap lines up and down
        noremap <C-K> :m-2<CR>
        noremap <C-J> :m+<CR>
        inoremap <C-K> <ESC>:m-2<CR>
        inoremap <C-J> <ESC>:m+<CR>
        vnoremap <C-K> :m-2<CR>gv
        vnoremap <C-J> :m'>+<CR>gv

        " Toggle between relative and absolute numbering
        " nnoremap <Leader>l :set relativenumber!<CR>

        " Keep search centered
        nnoremap <silent> n nzz
        nnoremap <silent> N Nzz
        nnoremap <silent> * *zz
        nnoremap <silent> # #zz
        nnoremap <silent> g* g*zz
        nnoremap <silent> g# g#zz

        " Increment and decrement numbers
        nnoremap + <C-a>
        nnoremap - <C-x>

        " Fix Vim's regex
        nnoremap / /\v
        vnoremap / /\v
        nnoremap ? /\v
        vnoremap ? /\v
    " }"}}}

    " Vundle Plugin Configuration {"{{{
        source ~/.vim/vimrc.bundles       " Handle bundle specific config
    " }"}}}

    " Automatic Commands {"{{{
        if has('autocmd')
            " Auto-reload the vim configuration
            augroup vimrcreload
                au!
                au BufWritePost .vimrc source ~/.vimrc
                au BufWritePost vimrc source vimrc
                au BufWritePost vimrc.bundles source ~/.vim/vimrc.bundles
            augroup END

            " Remove trailing whitespace characters
            augroup trailingwhitespace
                au!
                au BufWritePre * :%s/\s\+$//e
            augroup END

            " Only show highlights in active split
            augroup activebufferhighlights
                au!
                au WinLeave * set nocursorline colorcolumn= nonu nornu
                au WinEnter * set cursorline colorcolumn=+2 nu
            augroup END

            " Less files get typed to css
            augroup lesscss
                au!
                au BufRead,BufNewFile *.less setfiletype css
            augroup END

            " Json files get typed to json
            augroup json
                au!
                au BufRead,BufNewFile *.json setfiletype json
            augroup END

            " Run pdflatex when saving a tex file
            augroup latexsave
                au!
                au BufWritePost *.tex silent
                    \ !pdflatex -halt-on-error <afile> >/dev/null
            augroup END
        endif
    " }"}}}

    " Helper Functions {"{{{
        function! InitializeDirectories()
            let common_dir = $HOME . '/.vim/'
            let dir_list = { 'backup': 'backupdir',
                           \ 'views': 'viewdir',
                           \ 'swap': 'directory' }

            if has('persistent_undo')
                let dir_list['undo'] = 'undodir'
            endif

            for [dir, setting] in items(dir_list)
                let dir = common_dir . dir . '/'
                " If we can, create our directories
                if exists("*mkdir")
                    if !isdirectory(dir)
                        call mkdir(dir, 'p')
                    endif
                endif
                if !isdirectory(dir)
                    echo "Warning: Unable to create directory: " . dir
                    echo "Try: mkdir -p " . dir
                else
                    let dir = substitute(dir, " ", "\\\\ ", "g")
                    exec "set " . setting . "=" . dir
                endif
            endfor
        endfunction

        call InitializeDirectories()
    " }"}}}
" }
