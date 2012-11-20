#!/usr/bin/env ruby

########################################################################################################
# Usage:                                                                                               #
#  gitksdiff [-r:] [repository]                                                   #
#                                                                                                      #
# Example:                                                                                             #
#  gitksdiff                                       # == gitksdiff -rHEAD^:HEAD .                       #
#  gitksdiff -rHEAD^^:HEAD                         # == gitksdiff -rHEAD^^:HEAD .                      #
#  gitksdiff -master:origin/master                 # == gitksdiff -rmaster:origin/master .             #
#  gitksdiff /path/to/git/repository               # == gitksdiff -rHEAD^:HEAD /path/to/git/repository #
#  gitksdiff -rHEAD^:HEAD /path/to/git/repository  # == gitksdiff -rHEAD^:HEAD /path/to/git/repository #
#                                                                                                      #
# Authored by Simon Menke, with minor optimizations by Jonathan del Strother                           #
########################################################################################################

def checkout(origin, rev)
  tmp = "/tmp/git_ksdiff_#{rand}.tmp/"
  Dir.mkdir(tmp)
  Dir.chdir(origin) do
    # The clean, porcelainy way would be "git archive --format=tar '#{rev}' | (cd #{tmp} && tar xf -)"
    # I've found the plumbing to be marginally faster :
    system "git read-tree '#{rev}'; git checkout-index --prefix=#{tmp} -a; git read-tree HEAD"
  end
  return tmp
end

def diff(origin, revs={})
  revs = {:left=>"HEAD^",:right=>"HEAD"}.merge(revs)
  left  = checkout origin, revs[:left]
  right = checkout origin, revs[:right]
  system "ksdiff '#{left}' '#{right}'"
end

revs     = {}
git_dir = Dir.pwd
ARGV.each do |arg|
  if arg[0,2] == "-r"
    rev_strings = arg[2..-1].split(':')
    case rev_strings.size
    when 1
      revs[:left]  = rev_strings[0] unless rev_strings[0].empty?
    when 2
      revs[:left]  = rev_strings[0] unless rev_strings[0].empty?
      revs[:right] = rev_strings[1] unless rev_strings[1].empty?
    else
      revs = nil
    end
  else
    git_dir = arg unless arg.empty?
  end
end

diff(git_dir, revs)
