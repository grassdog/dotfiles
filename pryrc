require "rubygems"
require "awesome_print"

Pry.print = proc { |output, value| output.puts value.ai }
