dep "all-fonts" do
  requires "forza.otf",
           "hack.ttf",
           "gothamc.otf",
           "idlewild.otf",
           "vitesse.otf",
           "opensans.ttf",
           "source sans.otf",
           "sullivan.otf",
           "crimson.otf",
           "mensch.ttf",
           "roboto.ttf",
           "metropolis.otf",
           "league-gothic.otf",
           "deconeue.ttf"

end

dep "user font dir exists" do
  met? {
    "~/Library/Fonts".p.dir?
  }
  meet {
    log_shell "Creating ~/Library/Fonts", "mkdir ~/Library/Fonts"
  }
end

meta "ttf" do
  accepts_list_for :source
  accepts_list_for :ttf_filename

  template {
    requires "user font dir exists"
    met? {
      "~/Library/Fonts/#{ttf_filename.first}".p.exists?
    }
    meet {
      source.each do |uri|
        Babushka::Resource.extract(uri) do
          Dir.glob("*.ttf") do |font|
            log_shell "Installing #{font}", "cp #{font} ~/Library/Fonts"
          end
        end
      end
    }
  }
end

meta "otf" do
  accepts_list_for :source
  accepts_list_for :otf_filename

  template {
    requires "user font dir exists"
    met? {
      "~/Library/Fonts/#{otf_filename.first}".p.exists?
    }
    meet {
      source.each do |uri|
        Babushka::Resource.extract(uri) do
          Dir.glob("**/*.otf") do |font|
            log_shell "Installing #{font}", "cp #{font} ~/Library/Fonts"
          end
        end
      end
    }
  }
end

dep "forza.otf" do
  source "https://dl.dropboxusercontent.com/u/103175/Fonts/Forza.zip"
  otf_filename "Forza-Black.otf"
end

dep "league-gothic.otf" do
  source "https://github.com/theleagueof/league-gothic/archive/master.zip"
  otf_filename "LeagueGothic-Regular.otf"
end

dep "hack.ttf" do
  source "https://dl.dropboxusercontent.com/u/103175/Fonts/Hack-ttf.zip"
  ttf_filename "Hack-Bold.ttf"
end


dep "roboto.ttf" do
  source "https://dl.dropboxusercontent.com/u/103175/Fonts/roboto.zip"
  ttf_filename "Roboto-Medium.ttf"
end

dep "gothamc.otf" do
  source "https://dl.dropboxusercontent.com/u/103175/Fonts/GothamCond.zip"
  otf_filename "GothamCond-Black.otf"
end

dep "idlewild.otf" do
  source "https://dl.dropboxusercontent.com/u/103175/Fonts/Idlewild.zip"
  otf_filename "Idlewild-Bold.otf"
end

dep "vitesse.otf" do
  source "https://dl.dropboxusercontent.com/u/103175/Fonts/Vitesse.zip"
  otf_filename "Vitesse-Black.otf"
end

dep "sullivan.otf" do
  source "https://dl.dropboxusercontent.com/u/103175/Fonts/Sullivan.zip"
  otf_filename "Sullivan-Regular.otf"
end

dep "opensans.ttf" do
  source "https://dl.dropboxusercontent.com/u/103175/Fonts/OpenSans.zip"
  ttf_filename "OpenSans-Bold.ttf"
end

dep "source sans.otf" do
  source "https://github.com/adobe-fonts/source-sans-pro/archive/2.010R-ro/1.065R-it.zip"
  otf_filename "SourceSansPro-Black.otf"
end

dep "crimson.otf" do
  source "http://internode.dl.sourceforge.net/project/crimsontext/crimson_101217.zip"
  otf_filename "Crimson-Roman.otf"
end

dep "mensch.ttf" do
  ttf_filename "Mensch.ttf"
  meet {
    `curl https://dl.dropboxusercontent.com/u/103175/Fonts/Mensch.ttf > ~/Library/Fonts/Mensch.ttf`
  }
end

dep "metropolis.otf" do
  otf_filename "Metropolis.otf"
  meet {
    `curl https://dl.dropboxusercontent.com/u/103175/Fonts/Metropolis.otf > ~/Library/Fonts/Metropolis.otf`
  }
end

dep "deconeue.ttf" do
  ttf_filename "DecoNeue-Light.ttf"
  meet {
    `curl https://dl.dropboxusercontent.com/u/103175/Fonts/DecoNeue-Light.ttf > ~/Library/Fonts/DecoNeue-Light.ttf`
  }
end
