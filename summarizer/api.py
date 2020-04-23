from flask import Flask, jsonify, request
import requests
from bs4 import BeautifulSoup
from gensim.summarization import summarize

app = Flask(__name__)

@app.route('/')
def hello_world():
    page = requests.get(request.args['url']).text

    # Turn page into BeautifulSoup object to access HTML tags
    soup = BeautifulSoup(page)

    # Get text from all <p> tags.
    p_tags = soup.find_all('p')
    # Get the text from each of the “p” tags and strip surrounding whitespace.
    p_tags_text = [tag.get_text().strip() for tag in p_tags]

    # Filter out sentences that contain newline characters '\n' or don't contain periods.
    sentence_list = [sentence for sentence in p_tags_text if not '\n' in sentence]
    sentence_list = [sentence for sentence in sentence_list if '.' in sentence]
    # Combine list items into string.
    article = ' '.join(sentence_list)

    summary = summarize(article, word_count=20)

    return jsonify({'summary': summary})

if __name__ == '__main__':
    app.run()