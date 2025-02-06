<div align="right">
  🇬🇧 English
  |
  🇷🇺 <a href="./README_RU.md"> Русский</a>
</div>
<p align="center">
  <img src="./docs/media/logo.svg" width="300">
</p>
<h2 align="center">tildy</h2>
<p align="center">
  A CLI utility for download tiles from various map providers.
</p>
<br>

## Features

- Download tiles:
	- Completely the entire map
	- By the specified zoom levels
	- By the selected area
- Add Custom Providers
- Save tiles name and structuring according to the specified template
- Combining tiles from multiple providers into one common one
- Set the final resolution of tiles

## Usage

A [detailed list](./docs/USAGE.md) of available options and their possible applications with examples of the use of both individual options and their combinations.

## Supported platforms

| OS | Bitness                | Aviability                                                                  |
| ------------ | ----------------------- | ------------------------------------------------------------------------------- |
| Linux            | `64`               | ✅                                              |
| Windows 10, 11           | `64`             | ✅                                                    |


## Releases

You can download the utility on the [releases tab](https://github.com/kfilippenok/tildy/releases).

## Restrictions

There are some [restrictions](./docs/RESTRICTIONS.md) when downloading tiles from OpenStreetMap and its like.

## Kanban

To display the work on the project, a [Kanban board](https://github.com/users/tildy/projects/1) is used, implemented as a Github project.
 
## Dependencies

### Platform
- [FreePascal Compiler](https://www.freepascal.org/) >= 3.2.2 
- [Lazarus IDE](https://www.lazarus-ide.org/) >= 3.6

### Packages

- BGRABitmapPack4NoGUI (часть BGRABitmapPack)
 
### Libraries
 
 #### Windows
 
 - OpenSSL
 
 #### Linux:
 
 - linux-vdso.so.1
 - libc.so.6
 - ld-linux-x86-64.so.2