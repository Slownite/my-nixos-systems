#!/usr/bin/env python3
import os
import sys
import argparse


def parse_arguments():
    """
    Parses command line arguments.

    Returns:
        A namespace object with the source directory as an attribute.
    """
    parser = argparse.ArgumentParser(
        description="Link .nix files from a specified directory to /etc/nixos."
    )
    parser.add_argument(
        "source_dir", type=str, help="The directory containing .nix files to be linked."
    )
    return parser.parse_args()


def find_nix_files(source_dir):
    """
    Finds all .nix files in the specified directory, excluding common.nix.

    Args:
        source_dir (str): The directory to search for .nix files.

    Returns:
        A list of .nix files found in the directory.
    """
    return [
        f for f in os.listdir(source_dir) if f.endswith(".nix") and f != "common.nix"
    ]


def create_symlink(source_path, link_path):
    """
    Creates a symbolic link.

    Args:
        source_path (str): The path to the source file.
        link_path (str): The path where the symlink should be created.
    """
    try:
        os.symlink(source_path, link_path)
        print(f"Symlink created for {os.path.basename(link_path)}")
    except FileExistsError:
        print(f"A symlink for {os.path.basename(link_path)} already exists. Skipping.")


def link_nix_files(nix_files, source_dir):
    """
    Links .nix files to /etc/nixos and asks which should be linked as configuration.nix.

    Args:
        nix_files (list): A list of .nix files to be linked.
        source_dir (str): The directory containing the .nix files.
    """
    nixos_dir = "/etc/nixos/"
    for file in nix_files:
        source_path = os.path.join(source_dir, file)
        link_path = os.path.join(nixos_dir, file)
        create_symlink(source_path, link_path)

    # Ask for the file to be linked as configuration.nix
    configure_as_main(nix_files, source_dir)


def configure_as_main(nix_files, source_dir):
    """
    Asks which .nix file should be symlinked as configuration.nix.

    Args:
        nix_files (list): A list of .nix files.
        source_dir (str): The directory containing the .nix files.
    """
    while True:
        print("Which of these files should be linked as configuration.nix?")
        for i, file in enumerate(nix_files, 1):
            print(f"{i}. {file}")
        choice = input("Enter number: ")

        try:
            choice = int(choice) - 1
            if 0 <= choice < len(nix_files):
                selected_file = nix_files[choice]
                configuration_nix_path = os.path.join(
                    "/etc/nixos/", "configuration.nix"
                )
                source_path = os.path.join(source_dir, selected_file)

                # Remove existing configuration.nix symlink if it exists
                if os.path.exists(configuration_nix_path) or os.path.islink(
                    configuration_nix_path
                ):
                    os.remove(configuration_nix_path)

                # Create the new symlink for configuration.nix
                create_symlink(source_path, configuration_nix_path)
                break
            else:
                print("Invalid selection. Please try again.")
        except ValueError:
            print("Please enter a number.")


def main():
    """
    The main entry point of the script.
    """
    args = parse_arguments()
    nix_files = find_nix_files(args.source_dir)

    if not nix_files:
        print("No .nix files found in the directory.")
        sys.exit(1)

    link_nix_files(nix_files, args.source_dir)


if __name__ == "__main__":
    main()
