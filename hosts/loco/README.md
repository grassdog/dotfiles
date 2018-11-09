# loco setup

Some post-bootstrap steps.

```sh
# Bundle apps
$ brew bundle -v --file=~/.Brewfile.apps

# Add asdf plugins
$ asdf plugin-add nodejs
$ asdf plugin-add elixir

$ npm install -g prettier tern jsonlint spaceship-prompt

$ gem install bundler pry-byebug pry rufo
```
