package sources

import (
	"bufio"
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"net/url"
	"os"
)

func New(path string, digest string) (*Module, error) {
	url, err := url.Parse(path)
	if err != nil {
		return nil, fmt.Errorf("failed to parse %q as an URL: %w", path, err)
	}

	hash, err := hex.DecodeString(digest)
	if err != nil {
		return nil, fmt.Errorf("failed to decode hash %q: %w", digest, err)
	}

	// Default scheme to local file.
	if url.Scheme == "" {
		url.Scheme = "file"
	}

	if url.Scheme != "file" {
		return nil, &UnsupportedSchemeError{Scheme: url.Scheme}
	}

	return &Module{
		URL:    url,
		Digest: hash,
	}, nil
}

// Module identifies a source code document.
type Module struct {
	URL    *url.URL // It can be found with an URL.
	Digest []byte   // It's content's integrity is guaranteed by a SHA256 hash.
}

func (m *Module) Load() (io.RuneReader, error) {
	if m.URL.Scheme != "file" {
		return nil, &UnsupportedSchemeError{Scheme: m.URL.Scheme}
	}

	content, err := os.ReadFile(m.URL.Path)
	if err != nil {
		return nil, fmt.Errorf("Failed to read file %q: %w", m.URL.Path, err)
	}

	digest := sha256.Sum256(content)
	if !bytes.Equal(digest[:], m.Digest) {
		return nil, &DigestMismatchError{ExpectedDigest: m.Digest, ActualDigest: digest[:]}
	}

	return bufio.NewReader(bytes.NewReader(content)), nil
}
