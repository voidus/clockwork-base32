[![Test](https://github.com/nao1215/clockwork-base32/actions/workflows/test.yml/badge.svg)](https://github.com/nao1215/clockwork-base32/actions/workflows/test.yml)

# clockwork-base32

Clockwork Base32 is a simple variant of Base32 inspired by Crockford's Base32. See [Clockwork Base32 Specification](https://gist.github.com/szktty/228f85794e4187882a77734c89c384a8).

## Supported Haskell Versions & OS
- GHC 9.0 or later
- Linux, macOS, Windows

## Usage

```haskell
module Main where

import ClockworkBase32 (encode, decode)

main :: IO ()
main = do
    -- encode example
    let originalString = "foobar"
    putStrLn $ "Original string: " ++ originalString
    let encodedString = encode originalString
    putStrLn $ "Encoded (Base32): " ++ encodedString

    -- decode example
    let decodedResult = decode encodedString
    case decodedResult of
        Right decodedString -> putStrLn $ "Decoded string: " ++ decodedString
        Left err            -> putStrLn $ "Error decoding: " ++ err
```

## Other Implementations

### Reference Implementations

- C: [szktty/c-clockwork-base32](https://github.com/szktty/c-clockwork-base32)
- Erlang: [shiguredo/erlang-base32](https://github.com/shiguredo/base32_clockwork)
- Go: [szktty/go-clockwork-base32](https://github.com/szktty/go-clockwork-base32)
- Swift: [szktty/swift-clockwork-base32](https://github.com/szktty/swift-clockwork-base32)

### Third-party Implementations

- C++: [wx257osn2/clockwork_base32_cxx](https://github.com/wx257osn2/clockwork_base32_cxx)
- C++: [objectx/cpp-clockwork-base32](https://github.com/objectx/cpp-clockwork-base3)
- JavaScript: [mganeko/js_clockwork_base32](https://github.com/mganeko/js_clockwork_base32)
- AssemblyScript: [mganeko/as_clockwork_base32](https://github.com/mganeko/as_clockwork_base32)
- Rust: [woxtu/rust-clockwork-base32](https://github.com/woxtu/rust-clockwork-base32)
- Rust: [hnakamur/rs-clockwork-base32](https://github.com/hnakamur/rs-clockwork-base32)
- Go: [shogo82148/go-clockwork-base32](https://github.com/shogo82148/go-clockwork-base32)
- TypeScript: [niyari/base32-ts](https://github.com/niyari/base32-ts)


## Contributing

First off, thanks for taking the time to contribute! ❤️  See [CONTRIBUTING.md](./CONTRIBUTING.md) for more information.
Contributions are not only related to development. For example, GitHub Star motivates me to develop!

## Contact

If you would like to send comments such as "find a bug" or "request for additional features" to the developer, please use one of the following contacts.

- [GitHub Issue](https://github.com/nao1215/clockwork-base32/issues)


## LICENSE

[MIT License](./LICENSE)
