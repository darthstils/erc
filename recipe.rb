class erc < FPM::Cookery::Recipe

      source "file://#{ENV['WORKSPACE']}"

      name     "#{ENV['RPM_PROJECT']}"
      version  "#{ENV['GIT_TAG']}"
      revision '1.0.0'

      description 'Redis Client'

      provides "#{ENV['RPM_PROJECT']}"

      build_depends 'erlang19'

      section 'application'

      pre_install 'config/scripts/pre-install'
      post_install 'config/scripts/post-install'
      pre_uninstall 'config/scripts/pre-uninstall'
#      post_uninstall 'config/scripts/post-uninstall'

      directories "#{ENV['INSTALL_DIR']}/#{name}/bin", "#{ENV['INSTALL_DIR']}/#{name}/lib", "#{ENV['INSTALL_DIR']}/#{name}/priv", "#{ENV['INSTALL_DIR']}/#{name}/releases", "#{ENV['INSTALL_DIR']}/#{name}/#{ENV['ERTS_VERSION']}"

      def build
        make
      end

      def install
        %w(_rel/erc/* priv).each {|d| current_pathname_for("#{ENV['INSTALL_DIR']}/#{name}").install Dir[d]}
        current_pathname_for("#{ENV['INSTALL_DIR']}/#{name}/log").mkpath
        chmod 0750, current_pathname_for("#{ENV['INSTALL_DIR']}/#{name}/log")
        %w(config/app).each {|d| current_pathname_for("etc/init.d").install d}
        %w(config/app.conf config/sys.config).each {|d| current_pathname_for("opt/conf").install d}
      end
    end
