require 'rake'

DOTFILES_ROOT = `pwd`.chomp
HOME = ENV['HOME']

UNLINKED = %w[Rakefile Brewfile README.md profiles.clj services]

desc "Link dotfiles into $HOME directory"
task :link_files do
  source_files = Rake::FileList.new("*").exclude(*UNLINKED)

  source_files.each do |file|
    link_file(file)
  end
end

task :link_lein_profiles do
  mkdir_p File.join HOME, ".lein"
  link_file("profiles.clj") { |file| File.join(HOME, ".lein/#{file}") }
end

desc "Install mac services"
task :install_services do
  mkdir_p File.join(HOME, "Library/Services")
  Dir['services/*'].each do |d|
    cp_r d, File.join(HOME, "Library/Services")
  end
end

desc "Install Homebrew"
task :install_brew do
  sh 'ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"'
end

desc "Install Homebrew packages"
task :install_brew_packages do
  sh "brew bundle Brewfile"
end

desc "Bootstrap the world"
task :bootstrap => [:link_files, :link_lein_profiles, :install_services, :install_brew, :install_brew_packages]

def link_file(src, &resolve_dest_path)
  resolve_dest_path ||= ->(file) { File.join(HOME, ".#{file}") }

  src_path  = File.join(DOTFILES_ROOT, "#{src}")
  dest_path = resolve_dest_path.call(src)

  if File.exist?(dest_path)
    puts "Skipping: '#{dest_path}' exists"
  else
    sh "ln -s #{src_path} #{dest_path}"
  end
end

