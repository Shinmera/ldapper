#!/bin/sh

exec sbcl --eval '(require "asdf")' --eval '(asdf:load-asd "/app/ldapper.asd")' --eval '(ql:quickload :ldapper)' --eval '(asdf:make :ldapper)'
