
# Monkey patch extract to force yes answer to licence prompts
class Babushka::DmgAsset
  def extract &block
    in_download_dir {
      output = Babushka::ShellHelpers.log_shell "Force Attaching #{filename}", "yes | hdiutil attach '#{filename.p.basename}'"
      if output.nil?
        Babushka::LogHelpers.log_error "Couldn't mount #{filename.p}."
      elsif (path = mountpoint_for(output)).nil?
        raise "Couldn't find where `hdiutil` mounted #{filename.p}."
      else
        cd(path) {
          block.call(self)
        }.tap {
          Babushka::ShellHelpers.log_shell "Detaching #{filename}", "hdiutil detach '#{path}'"
        }
      end
    }
  end
end

# Monkey patch resource to maintain file names on download
class Babushka::Resource
  def self.download url, filename = url.to_s.p.basename
    if filename.p.exists? && !filename.p.empty?
      log_ok "Already downloaded #{filename}."
      filename
    elsif (result = shell(%Q{curl -J -I -X GET "#{url}"})).nil?
      log_error "Couldn't download #{url}: `curl` exited with non-zero status."
    else
      response_code = result.val_for(/HTTP\/1\.\d/) # not present for ftp://, etc.
      if response_code && response_code[/^[23]/].nil?
        log_error "Couldn't download #{url}: #{response_code}."
      elsif !(location = result.val_for(/^location:/i)).nil?
        log "Following redirect from #{url}"
        download URI.escape(location), location.p.basename
      else
        success = log_block "Downloading #{url}" do
          shell('curl', '-#', '-o', "#{filename}.tmp", url.to_s, :progress => /[\d\.]+%/) &&
          shell('mv', '-f', "#{filename}.tmp", filename)
        end
        filename if success
      end
    end
  end
end


