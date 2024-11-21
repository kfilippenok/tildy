
<p align="center">
  <img src="./docs/media/logo.svg" width="300">
</p>
<h1 align="center">Tiles Downloader</h1>
<p align="center">
  A CLI utility for download tiles from various map providers.
</p>
<br>

## Features

- Download tiles:
	- Completely the entire map
	- By the specified zoom levels
	- By the selected area
- Custom providers
- Save in two versions: by folders or by pattern
- Combine tiles from two specified providers
- Set the final resolution of tiles

## Usage

🇬🇧 [English](./docs/USAGE.md) | 🇷🇺 [Russian](./docs/USAGE_RU.md)

A detailed list of available options and their possible applications with examples of the use of both individual options and their combinations.

## Supported platforms

| OS | Bitness                | Aviability                                                                  |
| ------------ | ----------------------- | ------------------------------------------------------------------------------- |
| Linux            | `64`               | ✅                                              |
| Windows 10, 11           | `64`             | ✅                                                    |


## Restrictions

There are some restrictions when downloading tiles from OpenStreetMap and its like. [More details](./docs/RESTRICTIONS.md).

## Kanban

To display the work on the project, a [Kanban board](https://github.com/users/kfilippenok/projects/1) is used, implemented as a Github project.
 

## Dependencies

### Platform
- [FreePascal Compiler](https://www.freepascal.org/) >= 3.2.2 
- [Lazarus IDE](https://www.lazarus-ide.org/) >= 3.6

### Packages

- BGRABitmapPack (avialable in OPM)
 
### Libraries
 
 - OpenSSL library