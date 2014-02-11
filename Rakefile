require 'rake'

desc "Install files"
task :install => [:update, :dotfiles, :services]

desc "Install dot files into $HOME"
task :dotfiles do
  replace_all = false
  Dir['*'].reject {|i| %w[Rakefile README.md z].include? i }.each do |file|
    puts "\n** #{file} **"
    destination = File.join(ENV['HOME'], ".#{file}")

    if File.exist?(destination)
      if File.identical? file, destination
        puts "identical ~/.#{file}"
      elsif replace_all
        replace_file(file)
      else
        print "overwrite ~/.#{file}? [ynaq] "
        case $stdin.gets.chomp
        when 'a'
          replace_all = true
          replace_file(file)
        when 'y'
          replace_file(file)
        when 'q'
          exit
        else
          puts "skipping ~/.#{file}"
        end
      end
    else
      link_file(file)
    end
  end
end

desc 'Install mac services'
task :services do
  require 'fileutils'
  mkdir_p "#{ENV['HOME']}/Library/Services"
  Dir['services/*'].each do |d|
    cp_r d, "#{ENV['HOME']}/Library/Services"
  end
end

desc 'Update git submodules'
task :update do
  sh 'git submodule init && git submodule update'
end

def replace_file(file)
  system %Q{rm -rf "$HOME/.#{file}"}
  link_file(file)
end

def link_file(file)
  puts "linking ~/.#{file}"
  system %Q{ln -s "$PWD/#{file}" "$HOME/.#{file}"}
end
