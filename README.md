Emacs configuration
===================

 My Emacs configuration.

### Other configurations  ###

#### Start Emacs daemon if not available ####

**.bashrc**

    export ALTERNATE_EDITOR=""
    export EDITOR='emacsclient'
    alias ec='emacsclient -c'
    alias et='emacsclient -t'

**shortcut**

    emacsclient --alternate-editor="" -c
