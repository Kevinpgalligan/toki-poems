## Description
Generate poems in the toki pona language! Now exists as a [Twitter bot](https://twitter.com/PonaBot). [Write-up here](https://kevingal.com/blog/toki-poetry.html).

> jan sama pi jan,  
> ike li pona ala,  
> tawa mi la tan.  
>  
> tenpo ni e kala.

## Installation
Requires: 1) a Common Lisp implementation, and 2) quicklisp (the de-facto package manager for Common Lisp). If you don't have either of those things, then [Portacle](https://portacle.github.io/) is the quickest way to get started.

Clone the repo into your quicklisp local-projects folder. Then, from a REPL, run `(ql:quickload 'toki)`.

## Usage
The first thing you need is a corpus of toki pona text. For this purpose, the repo includes a Python script for scraping reddit, but you don't have to use it if you have another source of text.

To use the script, you'll need 1) Python 3, and 2) a Reddit account + client ID + client secret (see [here](https://praw.readthedocs.io/en/latest/getting_started/quick_start.html) for more information).

```
$ pip3 install praw # the only dependency
...
$ python3 redscrape.py tokipona <client-id> <client-secret> <useragent> --path <path-to-output-file>
```

Now you have a corpus of toki pona text that you can use to generate poems. Within your Common Lisp REPL:

```lisp
CL-USER> (ql:quickload 'toki)
CL-USER> (in-package toki)
TOKI> (defparameter ch (make-toki-chain "/path/to/corpus.txt"))
TOKI> (generate-poem ch "5A 7B 5A / 5B")
"pilin e ni tan,
jan sin li pona en jan,
ni la mi o jan.

mi la mi lon tan."
```

## Bot Setup
Send tweets using the tweet.lisp script, which requires SBCL and quicklisp to be installed. It relies on the working directory being the same as the directory where the script is stored. It assumes that certain files (the corpus & credentials) are available in the same directory. And it assumes that '.sbclrc' is in the home directory. But such things should be straightforward to tweak.

Finally, here's the crontab entry that runs the bot once per day at 4pm:

```
0 16 * * * cd ~/proyectos/toki-poems/ && ./tweet.lisp >>tweet.log
```
