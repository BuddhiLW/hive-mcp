package repository

import "github.com/BuddhiLW/arara/internal/pkg/config"

// DotfilesConfigRepository defines the interface for managing local dotfiles configuration.
// This repository handles loading and saving the arara.yaml file.
type DotfilesConfigRepository interface {
	// Load reads and parses the local arara.yaml file from the given path.
	// Returns the parsed configuration or an error if the file cannot be read or parsed.
	Load(path string) (*config.DotfilesConfig, error)

	// Save writes the provided configuration to the specified path.
	// Returns an error if the file cannot be written.
	Save(path string, cfg *config.DotfilesConfig) error

	// Exists checks if a configuration file exists at the given path.
	// Returns true if the file exists, false otherwise.
	Exists(path string) bool
}

// GlobalConfigRepository defines the interface for managing global namespace configuration.
// This repository handles loading, saving, and querying global configurations and namespaces.
type GlobalConfigRepository interface {
	// Load reads and parses the global namespace configuration.
	// Returns the parsed configuration or an error if the file cannot be read or parsed.
	Load() (*config.GlobalConfig, error)

	// Save persists the provided global configuration.
	// Returns an error if the configuration cannot be saved.
	Save(cfg *config.GlobalConfig) error

	// GetActiveNamespace retrieves the active namespace configuration.
	// Returns the namespace configuration or an error if it cannot be determined.
	GetActiveNamespace() (*config.NamespaceConfig, error)

	// GetDotfilesPath retrieves the path to the active dotfiles.
	// Returns the path or an error if it cannot be determined.
	GetDotfilesPath() (string, error)

	// AddNamespace adds a new namespace with the given name, path, and localBin directory.
	// Returns an error if the namespace cannot be added.
	AddNamespace(name, path, localBin string) error

	// RemoveNamespace removes the namespace with the given name.
	// Returns an error if the namespace cannot be removed.
	RemoveNamespace(name string) error

	// ListNamespaces lists all available namespaces.
	// Returns a slice of namespace names.
	ListNamespaces() []string
}