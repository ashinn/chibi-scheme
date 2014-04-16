
(import (scheme base) (chibi crypto sha2) (chibi test))

(test-begin "sha2")

(test "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f"
    (sha-224 ""))
(test "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7"
    (sha-224 "abc"))
(test "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525"
    (sha-224 "The quick brown fox jumps over the lazy dog"))

(test "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (sha-256 ""))
(test "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (sha-256 "abc"))
(test "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
    (sha-256 "The quick brown fox jumps over the lazy dog"))
(test "61f8fe4c4cdc8b3e10673933fcd0c5b1f6b46d3392550e42b265daefc7bc0d31"
    (sha-256 "abcdbcdecdefdefgefghfghighijhijkijkljklmklm"))
(test "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    (sha-256 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))

(test-end)
