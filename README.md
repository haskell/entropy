# Introduction

This package allows Haskell users to easily acquire entropy for use in
critical security applications by calling out to either windows crypto api,
unix/linux's `/dev/urandom`, or the RDRAND instruction.

This package supports Windows, {li,u}nix, QNX, and has preliminary support for HaLVM.

[![Build Status](https://travis-ci.org/TomMD/entropy.svg?branch=master)](https://travis-ci.org/TomMD/entropy)
