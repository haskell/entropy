# Introduction

This package allows Haskell users to easily acquire entropy for use in critical
security applications by calling out to either windows crypto api or unix/linux's
`/dev/urandom`.

# TODO

Eventually this package should detect support for RDRAND and use that
instruction instead when available.
