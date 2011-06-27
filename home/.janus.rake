vim_plugin_task "html5", "git://github.com/othree/html5.vim.git"

vim_plugin_task "delek" do
  sh "curl http://www.vim.org/scripts/download_script.php?src_id=2382 > colors/delek.vim"
end

vim_plugin_task "MRU" do
  sh "curl http://www.vim.org/scripts/download_script.php?src_id=11919 > plugin/mru.vim"
end

vim_plugin_task "json" do
  sh "curl http://www.vim.org/scripts/download_script.php?src_id=10853 > syntax/json.vim"
end

vim_plugin_task "lesscss" do
  sh "curl http://leafo.net/lessphp/vim/less.vim > syntax/less.vim"
end

skip_vim_plugin "syntastic"
