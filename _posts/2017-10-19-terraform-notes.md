---
layout: post
title: Terraform notes
categories: notes
---

Terraform code is a collection of configuration files that define
infrastructure resources on remote providers, like cloud platforms or local
hypervisors.

Usage
-----

First, initialize the local environment. This creates `.terraform`, and
downloads default providers.

    terraform init

Before applying changes, Terraform must build an execution plan, which will
show you what will be created, destroyed, or changed.

    terraform plan

This command will apply changes.

    terraform apply

Afterwards, you can view the new state of provisioned resources.

    terraform show

Configuration
-------------

Terraform uses JSON (`*.tf.json`) or its own syntax, HCL (`*.tf`).

- Files are appended to each other alphabetically.
- Resource identifiers must be unique globally.
- Override files are merged.

Valid data types include strings, numbers, booleans, lists, and maps. Terraform
configuration is declarative.

## String interpolation

Interpolation takes the form `"${foo.bar}"`. This also allows for embedded
simple math and conditionals.

## Conditionals

Terraform supports ternary conditionals.

    # Design pattern for enabling or disabling a resource based on some
    # variable.

    resource "foo_bar" "fizz" {
      count = "${var.something ? 1 : 0}"
    }

## Functions

Terraform comes with built-in functions allowing list manipulation, number
crunching, or CIDR manipulation, and etc.

State
-----

When creating resources, Terraform will create a `.tfstate` file that matches
metadata to resources on the provider. If sharing a state file, it should be
stored remotely, which also protects secrets in the state file.

*State locking* can facilitate multiple people building on the same Terraform
provider, and prevents double-writing. The implemented back-end must support
this.

Terraform state can be segregated into *workspaces*. The current workspace can
be referenced using the variable `${terraform.workspace}`.

Resources
---------

- `provider`: Define an external infrastructure platform to provision on. It
  defines resources and manages them.

  Check providers in current Terraform code with: 

      terraform providers

  Custom providers must be placed within `terraform.d/plugins/<arch>`.

  Multiple providers of the same name can be defined and are distinguished by
  the `alias` parameter.

- `resource`: Some unit of infrastructure, like a VM. Can be physical or
  logical.

        resource "[provider]_[resource-type]" "[name]" {
          ...
        }

- `data`: Defines a set of data that can optionally come from outside
  Terraform, or from computed resource values in Terraform.

- `module`: Defines the instantiation of a module.

- `locals`: Local variables inside a Terraform configuration file.

Previously declared resource data can be used as variable input to other
resources. Terraform will infer dependencies and apply resources in order, so
post-creation data should be available. This is called an _implict dependency_.

Example:

    resource "[provider]_[resource-type]" [name]" {
      size = "${[other-provider].[other-resource-type].size}"
    }

The `depends_on` attribute is available to all resources to define _explicit
dependencies_.

    depends_on = ["some.thing.name"]

*Tainted resources* are those that failed somewhere in the apply step.
Terraform will attempt to remove and recreate these.

Multiple resources can be declared using a map, the `lookup` function, and the
`count` metavar.

    variable "ips" {
      default = {
        0 = "1.1.1.1"
        1 = "1.1.1.2"
        2 = "1.1.1.3"
      }
    }

    resource "foo_bar" "fizz" {
      count = "3"
      ip    = "${lookup(var.ips, count.index)}"
    }

Variables
---------

**Variables** are defined using the `variable` keyword. They are assigned
values at runtime and have optional defaults.

    variable "foo" {}
    variable "bar" {
      default = "fizzbuzz"
    }

Variables can be interpolated into strings.

    resource "foo_bar" "fizz" {
      size = "${var.foo}"
    }

Variable assignments can be generated from, in order of preference:

- Command line `-var` arguments.
- From a file, `terraform.tfvars` or `*.auto.tfvars`, or files matching the
   `-var-file` argument.
- From environment variables, `TF_VAR_[name]`.
- UI input, when running apply.
- Defaults.

Terraform has lists.

    variable "my_list" { default = ["a", "b"] }

    "${var.my_list[0]}"

Terraform has maps.

    variable "my_map {
      type = "map"
      default = {
        "foo" = "bar"
        "fizz" = "buzz"
      }
    }

    "${var.my_map["foo"]}"

You can also use the `lookup` function to fetch a variable key. Maps are useful
for building lookup tables.

Output Variables
----------------

**Outputs** define a way to extract specific information from Terraform state
in order to view, or to provide input to other resources..

    output "an_output" {
      value = "${foo.bar.bizz}"
    }

New output modules need a `terraform apply` in order to be available.
Afterwards:

    terraform output an_output
    [something]

Output values can also conveniently show all values within a map or list.

    "${foo.bar.*.bizz}"

Provisioners
------------

**Provisioners** are resource types that can perform post-build tasks on
resources.  Available options include `local-exec` that runs a local command,
or others like Salt.

Provisioners only run on resource creation, and are meant for bootstrapping
(vs.  configuration management). If a provisioning step fails, the resource
will be marked *tainted* and recreated upon the next apply.

*Destroy provisioners* are available to run upon resource destruction. They
should be idempotent as they will run again after failure.

Provisioners will run in the order they are defined in configuration.

Modules
-------

**Modules** are self-contained collections of Terraform configurations that can
be logically grouped together. Modules need a `source` attribute that tells
Terraform where to retrieve it from (locally, Git, HTTP, etc.).

    module "foo" {
      source = "github.com/foo/bar"
      ...
    }

Aftewards, you'll need to perform the module retrieval.

    terraform get

To update modules, use the `-update` flag.

Variables can be retrieved from inside modules by using the syntax
`"${module.[module_name].[value]}"`. Variables defined within modules become
the module's parameters.

Plugins
-------

Plugins for Terraform can be stored in `~/.terraform.d/plugins` and
instantiated with `terraform init`.
