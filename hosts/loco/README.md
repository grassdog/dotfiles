# loco setup

My personal machine since Feb 2015.

Some post-bootstrap steps.

```sh
# Bundle apps
$ brew bundle -v --file=~/.Brewfile.apps

# Add asdf plugins
$ asdf plugin-add ruby https://github.com/asdf-vm/asdf-ruby.git
$ asdf plugin-add nodejs https://github.com/asdf-vm/asdf-nodejs.git
$ asdf plugin-add elixir https://github.com/asdf-vm/asdf-elixir.git

$ npm install -g prettier tern jsonlint spaceship-prompt

$ gem install bundler pry-byebug pry rufo
```
