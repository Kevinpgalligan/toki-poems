"""
Scrape comments from a subreddit.

Requirements: just PRAW.
"""

import sys
import os.path
import itertools
import argparse
import re

import praw

URL_REG = re.compile(r"https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)")
# It's difficult to handle brackets and quotes.
# They tend to make output look ugly without complicated handling.
# So just remove them.
STRUCTURES_REG = re.compile(r"“.+”|\".+\"|\(.+\)|\[.+\]")
REDDIT_SHIT_REG = re.compile(r" [ru]/[^ ]+")

def main():
    parser = argparse.ArgumentParser(
        description="Scrape comments from a subreddit and dump them into a text file.")
    parser.add_argument("subreddit")
    parser.add_argument("clientid")
    parser.add_argument("clientsecret")
    parser.add_argument("useragent")
    parser.add_argument("--num-comments", default=5000, type=int)
    parser.add_argument("--path", required=False)
    args = parser.parse_args()

    reddit = praw.Reddit(
        client_id=args.clientid,
        client_secret=args.clientsecret,
        user_agent=args.useragent)
    if args.path:
        path = os.path.join(args.path, args.subreddit)
    else:
        path = args.subreddit

    with open(path, "w") as f:
        for i, comment in enumerate(generate_comments(reddit.subreddit(args.subreddit))):
            author = comment.author
            if (author is None
                    or "bot" in author.name
                    or "automoderator" in author.name):
                continue
            text = filter_comment(comment.body)
            if text:
                f.write(text)
                f.write("\n")
            if i >= args.num_comments:
                break
            elif i > 0 and i % 1000 == 0:
                print("scraped so far:", i)

def generate_comments(subreddit):
    return itertools.chain.from_iterable(
        map(
            load_comments_and_flatten,
            subreddit.hot(limit=None)))

def load_comments_and_flatten(submission):
    submission.comments.replace_more(limit=None)
    return submission.comments.list()

def filter_comment(text):
    # Removes junk from comment. URLs and whatnot.
    if text == "[deleted]":
        return ""
    text = " ".join(text.split("\n"))
    text = URL_REG.sub(" ", text)
    text = STRUCTURES_REG.sub(" ", text)
    text = REDDIT_SHIT_REG.sub(" ", text)
    text = text.replace('‘', '\'')
    text = text.replace('&amp;', '&')
    return text.strip()

if __name__ == "__main__":
    main()
