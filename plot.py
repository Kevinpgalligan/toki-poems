import argparse
from wordcloud import WordCloud
import matplotlib.pyplot as plt

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("t", choices=["cloud", "cumulative"])
    args = parser.parse_args()

    freqs = {}
    with open("word-counts.txt", "r") as f:
        for line in f.readlines():
            word, count = line.strip().split(" ")
            count = int(count)
            freqs[word] = count
    if args.t == "cloud":
        wc = WordCloud().generate_from_frequencies(freqs)
        plt.figure(facecolor='k')
        plt.imshow(wc, interpolation="bilinear")
        plt.axis("off")
    elif args.t == "cumulative":
        total = sum(count for word, count in freqs.items())
        pairs = sorted(freqs.items(), key=lambda pair: pair[1], reverse=True)
        xs = [0]
        ys = [0]
        s = 0
        for i, (word, count) in enumerate(pairs):
            s += count
            xs.append(i+1)
            ys.append(s/total)
        plt.plot(xs, ys, color="red")
        plt.grid(linestyle="--")
        plt.xlabel("words")
        plt.ylabel("cumulative frequency")
    plt.tight_layout(pad=0)
    plt.show()

if __name__ == "__main__":
    main()
