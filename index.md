## Welcome to the famous shoebox project

This project is about a nice tool, we are pragramming together to help linguists to accomplish the catpurind and querying of language words and translations. In principle the tool helps with what is called "interlinearization", if you want you can lookup a short explanation in [wikipedia on interlinear gloss](https://en.wikipedia.org/wiki/Interlinear_gloss).

You can use the [editor on GitHub](https://github.com/frankfurt-haskell-user-group/shoebox/edit/gh-pages/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

Introduction to [markdown and jekyll](markdown).

<ul>
  {% for post in site.posts %}
    <li>
      <a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>

