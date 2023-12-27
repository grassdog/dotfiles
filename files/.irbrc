if Gem::Version.new(RUBY_VERSION) >= Gem::Version.new("3.3.0")
  Reline::Face.config(:completion_dialog) do |conf|
    conf.define :default, foreground: :white, background: :black
    conf.define :enhanced, foreground: :black, background: :white
    conf.define :scrollbar, foreground: :white, background: :black
  end
end
