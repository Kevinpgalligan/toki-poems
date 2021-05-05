## Description
Generate poems in the toki pona language! Now exists as a [Twitter bot](https://twitter.com/PonaBot).

> jan sama pi jan,  
> ike li pona ala,  
> tawa mi la tan.  
>  
> tenpo ni e kala.

[Write-up here](https://kevingal.com/blog/toki-poetry.html).

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
For my own reference.

0. You should already have SBCL and quicklisp.
1. Make sure you have a corpus, save as corpus.txt in the base directory.
2. Decrypt credentials in creds.txt.gpg (be careful not to commit them in plaintext).
3. Copy tokibot.system to /etc/systemd/system/.
4. Kick off the systemd service: `sudo systemctl enable tokibot` and `sudo systemctl start tokibot`
