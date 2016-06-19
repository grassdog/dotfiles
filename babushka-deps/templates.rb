require 'fileutils'

meta :dir do
  accepts_value_for :path, :basename
  template {
    met? { path.p.dir? }
    meet { log_shell "Creating #{path}", "mkdir -p #{path}" }
  }
end

meta :copieddir do
  accepts_value_for :source
  accepts_value_for :target

  template {
    met? { target.p.dir? }
    meet { log_shell "Copying directory", "cp -R '#{source}' '#{target}'" }
  }
end

meta :symlink do
  accepts_value_for :source
  accepts_value_for :target

  template {
    met? { target.p.readlink == source.p }
    meet { shell "ln -sF #{source} #{target}" }
  }
end

meta :render do
  accepts_value_for :target
  accepts_value_for :erb

  template {
    def erb_path
      dependency.load_path.parent / erb
    end

    met? { Babushka::Renderable.new(target).from?(erb_path) }
    meet {
      render_erb erb_path, :to => target
    }
  }
end

meta :file do
  accepts_value_for :source
  accepts_value_for :target

  template {
    met? {
      target.p.exists? && FileUtils.compare_file(source.p, target.p)
    }
    meet {
      log_shell "Copying file", "cp #{source} #{target}"
    }
  }
end

meta :archive do
  accepts_value_for :source
  accepts_value_for :name, :basename
  accepts_value_for :target

  template {

    met? { target.p.exist? }

    meet {
      Babushka::Resource.extract(source) { |archive|
        Dir.glob("**/*#{name}").select {|file|
          log_shell "Copying #{file} to #{target}", "cp -R #{file} #{target}"
        }
      }
    }
  }
end

meta :repo do
  accepts_value_for :source
  accepts_value_for :path

  template {
    def repo
      @repo ||= Babushka::GitRepo.new(path).tap do |r|
        r.repo_shell('git fetch') if r.exists?
      end
    end

    meet do
      if repo.exists?
        log_block "Updating to #{repo.current_remote_branch} (#{repo.resolve(repo.current_remote_branch)})" do
          repo.reset_hard!(repo.current_remote_branch)
        end
      else
        git source, :to => path
      end
    end
    met? { repo.exists? && !repo.behind? }
  }
end


meta :defaults do
  accepts_value_for :domain
  accepts_value_for :key
  accepts_value_for :value

  template {
    def read_value
      case value
      when true
        "1"
      when false
        "0"
      else
        value.to_s
      end
    end

    def type
      return "bool"  if [true, false].include? value
      return "int"   if value.is_a? Integer
      return "float" if value.is_a? Float
      return "string"
    end

    def write_value
      value.to_s.include?(" ") ? "'#{value.to_s}'" : value.to_s
    end

    met? { `defaults read #{domain} #{key}`.strip == read_value }
    meet { log_shell "Setting #{domain} #{key} to #{write_value}", "defaults write #{domain} #{key} -#{type} #{write_value}" }
  }
end
