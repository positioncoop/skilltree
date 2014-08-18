Vagrant.configure("2") do |config|
  # All Vagrant configuration is done here. See: vagrantup.com.

  # Build against CentOS 6.4, without any config management utilities.
  config.vm.box = "debian7.5"
  config.vm.box_url = "https://vagrantcloud.com/ffuenf/debian-7.5.0-amd64/version/5/provider/virtualbox.box"

  # Enable host-only access to the machine using a specific IP.
  config.vm.network :private_network, ip: "192.168.33.33"
  config.vm.network "forwarded_port", guest: 8000, host: 8000

  config.vm.synced_folder ".", "/vagrant", :owner => "vagrant", :group => "www-data"


  # Provider-specific configuration for VirtualBox.
  config.vm.provider :virtualbox do |v|
    v.customize ["modifyvm", :id, "--name", "skilltree"]
    v.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
    v.customize ["modifyvm", :id, "--memory", 2560]
    v.customize ["modifyvm", :id, "--cpus", 4]
    v.customize ["modifyvm", :id, "--ioapic", "on"]
  end

  # Provisioning configuration for Ansible.
  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "provisioning/vagrant.yml"
    ansible.inventory_path = "provisioning/inventory"
    # Run commands as root.
    ansible.sudo = true
    ansible.limit = "devel"
    # ansible.raw_arguments = ['-v']
  end

  # Set the name of the VM. See: http://stackoverflow.com/a/17864388/100134
  config.vm.define :skilltree do |skilltree|
    skilltree.vm.hostname = "skilltree"
  end

end
