require 'rake'

DOTFILES_ROOT = `pwd`.chomp
HOME          = ENV['HOME']

LEIN_DIR      = File.join(HOME, ".lein")
SERVICES_DIR  = File.join(HOME, "Library/Services")

UNLINKED = %w[Rakefile Brewfile README.md profiles.clj services osx_apps.rb osx_settings.rb .git vim]

desc "Link dotfiles into $HOME directory"
task :link_files, :force do |t, args|
  force = !!args[:force]

  puts "Overwriting existing links" if force

  source_files = Rake::FileList.new("*").exclude(*UNLINKED)

  source_files.each do |file|
    link_file(file, force)
  end
end

directory LEIN_DIR

task :link_lein_profiles => LEIN_DIR do
  link_file("profiles.clj") { |file| File.join(HOME, ".lein/#{file}") }
end

directory SERVICES_DIR

desc "Install mac services"
task :install_services => SERVICES_DIR do
  Dir["services/*"].each do |d|
    cp_r d, File.join(HOME, "Library/Services")
  end
end

desc "Setup Vim"
task :setup_vim do
  sh "cd vim && rake setup"
end

desc "Bootstrap the world"
task :bootstrap => [:link_files, :link_lein_profiles, :install_services, :setup_vim]

def link_file(src, force=false, &resolve_dest_path)
  resolve_dest_path ||= ->(file) { File.join(HOME, ".#{file}") }

  src_path  = File.join(DOTFILES_ROOT, "#{src}")
  dest_path = resolve_dest_path.call(src)

  command = force ? "ln -sf " : "ln -s "

  if !force && File.exist?(dest_path)
    puts "Skipping: '#{dest_path}' exists"
  else
    sh "#{command} #{src_path} #{dest_path}"
  end
end

