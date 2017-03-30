## Welcome to the famous shoebox project

This project is about a nice tool, we are pragramming together to help linguists to accomplish the catpurind and querying of language words and translations. In principle the tool helps with what is called an "interlinear", if you want you can lookup a short explanation in [wikipedia on interlinear gloss](https://en.wikipedia.org/wiki/Interlinear_gloss).

## Posts

<ul>
  {% for post in site.posts %}
    <li>
        <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>


