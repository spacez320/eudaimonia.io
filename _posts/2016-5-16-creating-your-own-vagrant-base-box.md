---
layout: post
title: Creating your own Vagrant base box
---

[Vagrant](https://www.vagrantup.com/) is a tool that allows the individual
developer to easily work within a standardized environment that is built
specifically for their application or platform. Base boxes are a useful feature
of Vagrant that introduce extensibility into their build process, and I want to
talk about the motivations and nuances behind using them, especially for the
lone developer.

## Why use Vagrant at all?

The VirtualBox + Vagrant combination is a really convenient and clean way to
set up isolated environments for any project you want to develop and test
locally.

Why convenient? You can easily reproduce the setup you need to work reliably on
your project, which can help if you walk away from it for a while or move to
another machine [^2]. Once you get it working once, you can be reasonably sure it
will work every time you `vagrant up`.

Why clean? You don't pollute your workstation with install dependencies.
  
Obviously this is useful for teams who want help standardizing their workflow
across multiple devs, but I also find it useful for my own, lonesome endeavors.

## Why build a base box? The problem with out-of-the-box Vagrant.

Some aspects of Vagrant can be cumbersome to deal with at setup time, and some
of the issues can be dependent on the platform you run it on. These
instructions will talk about [VirtualBox](https://www.virtualbox.org/) -- this
arguably being the easiest VM engine to get running quickly.

Some examples of this include:

- Upgrading system packages.
- Constantly having to download `-devel` and libraries.
- Setting up networking appropriately.
- Installing guest additions (VirtualBox specific, but absolutely necessary).
- Anything else you want all your Vagrant machines to have at setup time.

The key benefits of making your own base box have to do with solving all of
these issues just once and lowering the amount of time it takes to start new
projects with Vagrant.

## Setting up your base box.

As with any Vagrant project, you have to start with a `Vagrantfile`, and the
initialization command is a good place to start.

```shell
matthew@xerces ~ $ mkdir ./my-base-box
matthew@xerces ~ $ cd ./my-base-box

matthew@xerces my-base-box $ vagrant init

A `Vagrantfile` has been placed in this directory. You are now
ready to `vagrant up` your first virtual environment! Please read
the comments in the Vagrantfile as well as documentation on
`vagrantup.com` for more information on using Vagrant.
```

You should now have a bare-bones Vagrantfile with lots of helpful comments
written by the Vagrant contributors.

**1. Decide which box to start from.**

Even though we are making our own box, you still need to start from somewhere.

There are lots of machines available to you in the [**Vagrant
Atlas**](https://atlas.hashicorp.com/boxes/search?vagrantcloud=1) that come in
flavors you like. Since I prefer to work in Fedora, I based mine off of the
officially supported [`fedora/23-cloud-base`](https://atlas.hashicorp.com/fedora/boxes/23-cloud-base/)[^1][^4].


**2. Decide what your base box needs.**

Here is where you come up with a plan of what to add/remove/change to craft
your base box. For example, some common things I want to do are getting
system-wide packages up to date and installing utilities that most of the
platforms I work in (Ruby, Python, etc.) usually need for even the most basic
of projects.

```
# this will go in your `Vagrantfile`

config.vm.provision "shell" do |s|
  s.inline = <<-SHELL

# upgrade existing system, excluding the kernel
sudo dnf -y makecache
sudo dnf -y upgrade --exclude='kernel-core'

# install some necessary packages for building ... well, anything
sudo dnf -y install gcc kernel-devel-`uname-r` make rpm-build

  SHELL
end
```

We do a couple of things here: installing compilers, development headers, build
tools, etc. This is pretty essentially for things like Python's `pip` and
Ruby's `bundle` to work properly. As you develop on more platforms, you may
find you need to add to this list.

Upgrading is just cool, but I exclude the kernel in order to avoid any kernel
dependent things downstream from requiring a restart, which you can't script
into the Vagrantfile.

**3. Fix things that are VM platform specific.**

For VirtualBox, the big thing here is to install Guest Additions. Without GA,
your shared `/vagrant` directory will not automatically sync -- a huge
headache.

```
# more provisioner logic in your `Vagrantfile`, which should be merged with
# what you may already have

config.vm.provision "shell" do |s|
  s.inline = <<-SHELL

# install Guest Additions
sudo dnf -y install wget
wget --quiet http://download.virtualbox.org/virtualbox/5.0.18/VBoxGuestAdditions_5.0.18.iso
sudo mkdir --parents /media/VBoxGuestAdditions
sudo mount --options loop,ro VBoxGuestAdditions_5.0.18.iso /media/VBoxGuestAdditions
sudo sh /media/VBoxGuestAdditions/VBoxLinuxAdditions.run

  SHELL
end
```

This is one of the reasons why you want to keep the kernel version held back.
GA's functionality comes from kernel modules, which have to be buildable and
loadable. The limbo period between upgrading a kernel and restarting makes this
impossible and breaks your build.

Another thing that I've found with VirtualBox is that you have to modify the VM
to use the host's DNS resolvers. Without this, it won't be able to access the
named internet easily and probably breaks most application build tools. I'm not
sure why or even if this is still a problem, but it's easily fixable.

```
config.vm.provider "virtualbox" do |vb|
  # resolve DNS requests through NAT'ing
  vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
end
```

**4. Retain the default SSH key.**

When building, Vagrant will try to detect if it's using the [default keys
hard-coded into Vagrant](https://github.com/mitchellh/vagrant/tree/master/keys),
and automatically replace them, if so. This is great for your leaf box builds,
but base boxes need to retain this key through the build process.

So, make sure your base box leaves the default Vagrant key alone, or you'll
find that boxes you build from it won't be accessible.

```
config.ssh.insert_key = false
```

## Building your base box.

Vagrant's utilities take over from here, and this is largely a repeat of what
is available in Vagrant's documentation.

The first step is to create the box from the machine you've defined in the
`Vagrantfile`. This involves building the box like you normally would, and then
packaging it from the live instance.

```
matthew@xerces my-base-box $ vagrant up

# go get some coffee, because this takes a while
```

Vagrant will run through all the setup steps you've defined. The machine should
successfully start and you should be able to see it running.

```
matthew@xerces my-base-box $ vagrant status

Current machine states:

default                   running (virtualbox)
```

Now you can package the instance into your base box, and to do this we need the
VM's VirtualBox id, which is randomly generated by Vagrant.

```
matthew@xerces my-base-box $ VBoxManage list runningvms

"vagrant_default_1463323796124_26612" {e511781f-d04f-47b6-9438-a4b898d45335}
```

Here, `vagrant_default_1463323796124_26612` is what we want. Your specific box
id will obviously be different. Time to package.

```
matthew@xerces my-base-box $ vagrant package vagrant-default_1463323796124_26612

==> vagrant_default_1463323796124_26612: Attempting graceful shutdown of VM...
==> vagrant_default_1463323796124_26612: Clearing any previously set forwarded ports...
==> vagrant_default_1463323796124_26612: Exporting VM...
==> vagrant_default_1463323796124_26612: Compressing package to: /home/matthew/workspace/personal/vagrant/package.box
```

You should now have a big binary blob called `./package.box` in your current
working directory. All that remains is to make Vagrant aware of that box for
use in future builds.

```
matthew@xerces my-base-box $ vagrant box add ./package.box --name "supercharged-fedora-base"

==> box: Box file was not detected as metadata. Adding it directly...
==> box: Adding box 'supercharged-fedora-base' (v0) for provider: 
    box: Unpacking necessary files from: file:///home/matthew/my-base-box/package.box
==> box: Successfully added box 'supercharged-fedora-base' (v0) for 'virtualbox'!

matthew@xerces vagrant $ vagrant box list

supercharged-fedora-base    (virtualbox, 0)
```

Voila -- the box is available to us to add to future Vagrantfiles as we see
fit [^3].

## Further reading.

- [https://www.vagrantup.com/docs/boxes/base.html](https://www.vagrantup.com/docs/boxes/base.html)
- [https://www.vagrantup.com/docs/virtualbox/boxes.html](https://www.vagrantup.com/docs/virtualbox/boxes.html)

[^1]: Some people may ask why I'm not looking for a box that more closely fits my
      project needs, or has a further along setup. I should admit that I'm biased
      towards controlling and understanding the system-level requirements of my
      applications, but I also can't shake the feeling that grabbing boxes from
      randos on the Atlas is, in essence, downloading a Virtual machine from the
      internet and running it without the ability to easily inspect it -- scary. So,
      I prefer the heavily used boxes coming from trusted sources, even if it means a
      little more work is involved.

[^2]: I state that one of Vagrant's benefits to the lone-wolf developer is the
      ability to move development environments to other machines with the only
      requirement being Vagrant at the ready. Unfortunately, the custom base box
      build above introduces another dependency for moving development environments
      because your new box might not be available on new machines if you haven't
      added it directly. You could potentially solve this by moving your base box to
      the Atlas, or sharing it in some other way.

[^3]: Another issue with this process is that it doesn't version the box, which
      means you can't update future downstream boxes with changes to the base box
      without rebuilding everything.  Personally, I don't see much value in fixing
      this as I can usually just reconstruct my own projects without much hassle, but
      using Vagrant in a team setting or as part of an official, automated build
      process might make versioning a necessity. Vagrant has [documentation
      specifically mentioning how to do go about
      this](https://www.vagrantup.com/docs/boxes/versioning.html).

[^4]: This guide should also serve as instructions on how to manage Fedora or
      RHEL/CentOS instances with Vagrant + VirtualBox. Most of the instructions I've
      found on the internet, and in Vagrant's own documentation, are specific to
      Debian or Ubuntu.
