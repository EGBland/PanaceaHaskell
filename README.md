# Panacea

A patching tool for Ice-Pick Lodge's 2005 game [Pathologic](https://store.steampowered.com/app/384110/Pathologic_Classic_HD/). Compatible with both the original 2005 and the 2015 "Classic HD" versions of the game.

## Usage

To use Panacea, place `Panacea.exe` in the folder `<pathologic base directory>\data`.

### Unpacking VFS files

Currently, Panacea can unpack the VFS files used by Pathologic to store game assets such as textures, sounds and scripts. To do this, invoke `Panacea.exe` and provide the name of the VFS file to unpack, for example:
    
    .\Panacea.exe Textures
    
This will unpack `Textures.vfs` into a folder called `Textures`.
