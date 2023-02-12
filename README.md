# Panacea

A patching tool for Ice-Pick Lodge's 2005 game [Pathologic](https://store.steampowered.com/app/384110/Pathologic_Classic_HD/). Compatible with both the original 2005 and the 2015 "Classic HD" versions of the game.

## Usage

To use Panacea, place `Panacea.exe` in the folder `<pathologic base directory>\data`.

### Unpacking VFS files

Currently, Panacea can unpack the VFS files used by Pathologic to store game assets such as textures, sounds and scripts. To do this, invoke `Panacea.exe` with the `--unpack-vfs` option and provide the name of the VFS file to unpack, for example:
    
    .\Panacea.exe --unpack-vfs Textures
    
This will unpack `Textures.vfs` into a folder called `Textures`.

### Unpacking the main.dat string file

Panacea is capable of unpacking the `main.dat` file, containing most in-game text including conversations and item descriptions. Get `main.dat` from `Strings.vfs`, then invoke:

    .\Panacea.exe --unpack-strings main.dat

This will unpack the strings into `text.csv`; the first column is the ID of the string and the second column is the string itself.

### Packing a CSV file into a .dat string file

Panacea can pack modified strings back into the format of `main.dat`. To do so, invoke:

    .\Panacea.exe --pack-strings text.csv

This will pack the strings in `text.csv` into a file called `main_modified.dat`.