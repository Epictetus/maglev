#--
# Copyright 2006 by Chad Fowler, Rich Kilmer, Jim Weirich and others.
# All rights reserved.
# See LICENSE.txt for permissions.
#++

require 'rubygems'

# Mixin methods for --version and --platform Gem::Command options.
module Gem::VersionOption

  # Add the --platform option to the option parser.
  def add_platform_option(task = command, *wrap)
    OptionParser.accept Gem::Platform do |value|
      if value == Gem::Platform::RUBY then
        value
      else
        Gem::Platform.new value
      end
    end

    add_option('--platform PLATFORM', Gem::Platform,
               "Specify the platform of gem to #{task}", *wrap) do
                 |value, options|
      unless options[:added_platform] then
        Gem.platforms = [Gem::Platform::RUBY]
        options[:added_platform] = true
      end

      Gem.platforms << value unless Gem.platforms.include? value
    end
  end

  # Add the --version option to the option parser.
  def add_version_option(task = command, *wrap)
    OptionParser.accept Gem::Requirement do |value|
      Gem::Requirement.new value
    end

    # BEGIN GEMSTONE
    #
    # Trac 686: Maglev messes up passing *foo if foo is an empty array
    xargs = ['-v', '--version VERSION', Gem::Requirement, "Specify version of gem to #{task}"]
    xargs = xargs + wrap unless wrap.empty?
    # add_option('-v', '--version VERSION', Gem::Requirement,
    #            "Specify version of gem to #{task}", *wrap) do
    add_option(*xargs) do
    # END GEMSTONE
                 |value, options|
      options[:version] = value
      options[:prerelease] = true if value.prerelease?
    end
  end

end

