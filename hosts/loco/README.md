# loco setup

Some post-bootstrap steps.

```sh
# Bundle apps
$ brew bundle -v --file=~/.Brewfile.apps

# Add asdf plugins
$ asdf plugin-add ruby
$ asdf plugin-add nodejs
$ asdf plugin-add elixir

$ npm install -g prettier tern jsonlint

$ gem install bundler pry-byebug pry
```