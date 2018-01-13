# Parsenv #

## Motivation ##

I am a simple lad, with simple needs.  For example, 

I grew tired, oh so tired, dear reader, of maintaining similar but parallel environment files for both my shell initialization files and my emacs environment.  Shouldn't these be the same?  Couldn't one file serve both purposes?

Write a .env file once.  Source it from your shell, like so for bash:  
`[ -r ~/.bash.env ] && . ~/.bash.env]`  
Then you can use *parsenv* to load the same environment from Emacs.  Easy-peasy.

## How to use ##

You will need to see to it that "parsenv.el" exists in a directory which is part of the *load-path* variable in Emacs.

Then, put code such as the following in your init file:  

    (use-package parsenv  
     :config  
     (parsenv-load-env (expand-file-name (concat "~/.bash.env")))  
     (parsenv-adjust-exec-path))`
  
## Commands ##

### parsenv-load-env ###

If the provided file exists, load it by inserting into Emacs's *process-environment* variable the keys and values specified in each line of the file.  
Here are a few of the formats that will work:  

    a=one  
    export b=two  
    c=three #comments are discarded  
    d="four"  
    e="five-$a"  
    f=six-$a'  
    g="with-#-mark" #quotes inhibit comments`  

These result in the following *process-environment*:

| variable | value       |
| -------  | -----       |
| a        | one         |
| b        | two         |
| c        | three       |
| d        | four        |
| e        | five-one    |
| f        | six=$a      |
| g        | with=#=mark |

Note that environment variables are expanded within double-quotes, but not single-quotes.

### parsenv-adjust-exec-path ###

Sets Emacs's *exec-path* variable based on the current contents of the variable "PATH" inside Emacs's *process-environment*.  By convention this includes making its last entry be the *exec-directory*.  This should be called just once after setting "PATH" to the desired value.
