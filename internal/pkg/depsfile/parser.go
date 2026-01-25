package depsfile

import (
	"bufio"
	"os"
	"strings"
)

// ReadDepsFile reads a dependencies file and returns a list of dependencies.
// Each line in the file can contain multiple dependencies separated by whitespace.
// Lines starting with '#' are treated as comments and ignored.
// Dependencies prefixed with '-' are stripped of the prefix.
func ReadDepsFile(path string) ([]string, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var deps []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		lineDeps := strings.Fields(line)
		for _, dep := range lineDeps {
			dep = strings.TrimPrefix(dep, "-")
			dep = strings.TrimSpace(dep)
			if dep != "" {
				deps = append(deps, dep)
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return deps, nil
}