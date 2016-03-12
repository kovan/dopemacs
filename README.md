Deprecated in favor of https://github.com/syl20bnr/spacemacs. Check it out!

![Image](../../blob/master/dopemacs.png?raw=true)
![Image](../../blob/master/dopemacs2.png?raw=true)

dopemacs
========

Emacs configuration that aims to add lots of enabled-by-default,
non-intrusive useful features while keeping traditional Emacs
keybindings and workflow.

Basically it consists just of a bunch of modules from ELPA, MELPA and
Marmalade that are downloaded the first time Emacs is started, and a
bunch of customize-variable statements that provide a better (IMHO)
defaults. So it very easy to customize. It contains as little elisp as
possible on its own and it is an aim of the project to keep it that
way.

**Included packages**

<pre>

  ace-jump-mode        A quick cursor location minor mode for emacs
  ack-and-a-half       Yet another front-end for ack
  ag                   A front-end for ag ('the silver searcher'), the C ack replacement.
  anzu                 Show number of matches in mode-line while searching
  apache-mode          major mode for editing Apache configuration files
  auto-complete        Auto Completion for GNU Emacs
  back-button          Visual navigation through mark rings
  browse-kill-ring     interactively insert items from kill-ring
  buffer-move          Swap buffers without typing C-x b on each window
  caml                 No description available.
  clojure-mode         Major mode for Clojure code
  cmake-mode           major-mode for editing CMake sources
  coffee-mode          Major mode to edit CoffeeScript files in Emacs
  company              Modular text completion framework
  concurrent           Concurrent utility functions for emacs lisp
  csharp-mode          C# mode derived mode
  csv-nav              navigate and edit CSV files
  ctable               Table component for Emacs Lisp
  cycbuf               Cycle buffers, inspired by swbuff.el, swbuff-x.el, and bs.el
  d-mode               D Programming Language mode for (X)Emacs
  dart-mode            Major mode for editing Dart files
  dash                 A modern list library for Emacs
  debian-changelo...   major mode for Debian changelog files.
  deferred             Simple asynchronous functions for emacs lisp
  diff-hl              Highlight uncommitted changes
  dired+               Extensions to Dired.
  dirtree              Directory tree views
  dos                  Major mode for editing Dos scripts
  ecb                  a code browser for Emacs
  editorconfig         EditorConfig Emacs extension
  el-get               Manage the external elisp bits and pieces you depend upon
  elpy                 Emacs Python Development Environment
  epc                  A RPC stack for the Emacs Lisp
  epl                  Emacs Package Library
  erlang               Erlang major mode
  exec-path-from-...   Get environment variables such as $PATH from the shell
  expand-region        Increase selected region by semantic units.
  f                    Modern API for working with files and directories
  feature-mode         Major mode for editing Gherkin (i.e. Cucumber) user stories
  fic-ext-mode         Show FIXME/TODO/BUG(...) in special face only in comments and strings
  find-file-in-pr...   Find files in a project quickly.
  flx                  fuzzy matching with good sorting
  flx-ido              flx integration for ido
  flycheck             On-the-fly syntax checking (Flymake done right)
  free-keys            Show free keybindings for modkeys or prefixes
  fuzzy                Fuzzy Matching
  git-commit-mode      Major mode for editing git commit messages
  git-rebase-mode      Major mode for editing git rebase files
  gitconfig-mode       Major mode for editing .gitconfig files
  go-mode              Major mode for the Go programming language
  google               Emacs interface to the Google API
  google-this          A set of functions and bindings to google under point.
  grizzl               Fuzzy Search Library & Completing Read
  groovy-mode          Groovy mode derived mode
  guess-offset         Automatically determine c-basic-offset
  guide-key            Guide the following key bindings automatically and dynamically
  guide-key-tip        Show guide-key.el hints using pos-tip.el
  haml-mode            Major mode for editing Haml files
  haskell-mode         A Haskell editing mode
  helm                 Helm is an Emacs incremental and narrowing framework
  helm-ack             Ack command with helm interface
  helm-ag              the silver search with helm interface
  helm-chrome          Helm interface for Chrome bookmarks
  helm-descbinds       Yet Another `describe-bindings' with `helm'.
  helm-flycheck        Show flycheck errors with helm
  helm-git-files       helm for git files
  helm-git-grep        helm for git grep, an incremental git-grep(1)
  helm-google          Emacs Helm Interface for quick Google searches
  helm-gtags           GNU GLOBAL helm interface
  helm-projectile      Helm integration for Projectile
  helm-pydoc           pydoc with helm interface
  helm-swoop           Efficiently hopping squeezed lines powered by helm interface
  helm-themes          Color theme selection with helm interface
  hide-lines           Commands for hiding lines based on a regexp
  highlight-inden...   Minor modes for highlighting indentation
  highlight-symbol     automatic and manual symbol highlighting
  howdoi               Instant coding answers via Emacs.
  htmlize              Convert buffer text and decorations to HTML.
  ido-ubiquitous       Use ido (nearly) everywhere.
  ido-vertical-mode    Makes ido-mode display vertically.
  idomenu              imenu tag selection a la ido
  iedit                Edit multiple regions in the same way simultaneously.
  iflipb               interactively flip between recently visited buffers
  jedi                 Python auto-completion for Emacs
  jinja2-mode          A major mode for jinja2
  jquery-doc           jQuery api documentation interface for emacs
  js2-mode             Improved JavaScript editing mode
  json-mode            Major mode for editing JSON files
  json-reformat        Reformatting tool for JSON
  less-css-mode        Major mode for editing LESS CSS files (lesscss.org)
  list-utils           List-manipulation utility functions
  magit                control Git from Emacs
  manage-minor-mode    Manage your minor-modes easily
  mark-tools           Some simple tools to access the mark-ring in Emacs
  markdown-mode        Emacs Major mode for Markdown-formatted text files
  matlab-mode          No description available.
  minimap              Minimap sidebar for Emacs
  move-text            Move current line or region with M-up or M-down.
  multi-term           Managing multiple terminal buffers in Emacs.
  multiple-cursors     Multiple cursors for Emacs.
  nav-flash            Briefly highlight the current line
  nlinum               Show line numbers in the margin
  nose                 Easy Python test running in Emacs
  nyan-mode            Nyan Cat shows position in current buffer in mode-line.
  pcache               persistent caching for Emacs
  persistent-soft      Persistent storage, returning nil on failure
  php-mode             Major mode for editing PHP code
  pkg-info             Information about packages
  pkgbuild-mode        Interface to the ArchLinux package manager
  popup                Visual Popup User Interface
  popwin               Popup Window Manager.
  pos-tip              Show tooltip at point
  powerline            Rewrite of Powerline
  project-persist      A minor mode to allow loading and saving of project settings.
  projectile           Manage and navigate projects in Emacs easily
  python-environment   virtualenv API for Emacs Lisp
  pyvenv               Python virtual environment interface
  rainbow-delimiters   Highlight nested parens, brackets, braces a different color at each depth.
  rainbow-mode         Colorize color names in buffers
  recentf-ext          Recentf extensions
  s                    The long lost Emacs string manipulation library.
  scala-mode           Major mode for editing Scala code.
  scss-mode            Major mode for editing SCSS files
  sequential-command   Many commands into one command
  simple-httpd         pure elisp HTTP server
  skewer-mode          live browser JavaScript, CSS, and HTML interaction
  slim-mode            Major mode for editing Slim files
  slime                Superior Lisp Interaction Mode for Emacs
  smart-mode-line      A color coded smart mode-line.
  smartparens          Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
  smartrep             Support sequential operation which omitted prefix keys.
  smex                 M-x interface with Ido-style fuzzy matching.
  sr-speedbar          Same frame speedbar
  string-inflection    underscore -> UPCASE -> CamelCase conversion of names
  stylus-mode          Major mode for editing .jade files
  sublimity            smooth-scrolling, minimap and distraction-free mode
  sunrise-commander    Two-pane file manager for Emacs based on Dired and inspired by MC
  sws-mode             (S)ignificant (W)hite(S)pace mode
  syslog-mode          Major-mode for viewing log files
  textile-mode         Textile markup editing major mode
  tidy                 Interface to the HTML Tidy program
  tree-mode            A mode to manage tree widgets
  tuareg               OCaml mode for Emacs.
  ucs-utils            Utilities for Unicode characters
  undo-tree            Treat undo history as a tree
  vimrc-mode           Major mode for vimrc files
  volatile-highli...   Minor mode for visual feedback on some operations.
  w3m                  an Emacs interface to w3m
  web-mode             major mode for editing html templates
  wgrep                Writable grep buffer and apply the changes to files
  whitespace-clea...   Intelligently call whitespace-cleanup on save
  windata              convert window configuration to list
  windresize           Resize windows interactively
  yaml-mode            Major mode for editing YAML files
  yascroll             Yet Another Scroll Bar Mode
  yasnippet            Yet another snippet extension for Emacs.
  zenburn-theme        A low contrast color theme for Emacs.


</pre>

**Installation:**

- Clone the repository to ~/.emacs.d

**Requirements:**

- Emacs >= 24

**F.A.Q**
- *Flycheck is not working for language X.* You probably need to install the appropiate syntax checker in your system, see https://github.com/flycheck/flycheck#quick-start.
- *error: Package `xxxx' is not available for installation*. Sometimes the repositories give errors. Try relanunching Emacs a couple times and if it still doesn't work, file a bug.

You should also check:
- https://github.com/bbatsov/prelude
- https://github.com/rdallasgray/graphene
- https://github.com/xiaohanyu/oh-my-emacs


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/kovan/dopemacs/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

