MultiKill
=========

Combine multiple imputation models for great justice. When given
multiple models from the MTLR implementation from
http://pssp.srv.ualberta.ca/, this program is able to average the
models in order to generate a single one. This is useful for when you
have many models generated from multiple imputation, and you want to
combine them in some way.

Installation
============

You should be able to install MultiKill with [nix](http://nixos.org/nix/) by entering the MultiKill directory and running

    nix-env -if .

Usage
=====

The arguments to MultiKill are as follows:

    MultiKill out-file in-files

Where in-files is a list of model files.