<div align="right">
  🇬🇧 English
  |
  🇷🇺 <a href="./USAGE_RU.md">Русский</a>
</div>

# USAGE

## Synopsis
./tildy **[OPTION]** **[PARAMETER]** ...


## Syntax

```
./tildy -provider osm-standard ...
```
```
./tildy --provider=osm-standard ...
```

## Options

### provider, p [String]

Choosing the provider that will provide the tiles. List of prepared providers: 

* `osm-standard` - OpenStreetMap Standard
* `railway-standard` - OpenRailwayMap Standard
* `railway-maxspeed` - OpenRailwayMap Maxspeed
* `railway-electrification` - OpenRailwayMap Electrification

Example:

```
... -provider osm-standard
```
```
... -p osm-standard
```

### providers, ps [String]

You can import your providers via the configuration file. Each provider starts with `[Provider]`. 

Fields used:

* `identifier` - the identifier to be used in layers or in *provider*
* `name` - the name of the provider, which will then be used when substituting macros in *out*
* `url` - a link with macros that will be used to get tiles.
* `cache` - identical to the *cache* option, the path to the folder with cached tiles
* `use-cache-only` - if ``yes`` is specified, then it will work only with the cache, in other cases it will not. by default, it works not only with the cache

Example of a configuration file `custom_providers.ini`:

```ini
[Provider]
ident=osm-standard-local
name=OpenStreetMap-Mapnik
url=http://localhost:8080/tile/{z}/{x}/{y}.png
```

Example of a provider that takes tiles only from the cache:
```ini
[Provider]
ident=osm-standard-cache
name=OpenStreetMap-Standard
url=http://localhost:8080/tile/{z}/{x}/{y}.png
cache=путь/до/папки/с/кешем/
# by default, "no" is used. use this option only if you need to work with the cache only.
use-cache-only=yes 
```

Example:
```
... -providers custom_providers.ini -provider osm-standard-local
```
```
... -ps custom_providers.ini -p osm-standard-local
```


****

### layers, ls [String] 

Using a configuration file with layers. The layers are tiles from selected providers. All the layers are superimposed on each other, eventually forming one single one. The layers are described in order from the bottom to the top. Each layer starts with `[Layer]`. 

Fields used:

* `provider` - the provider used
* `filter` - the filter used
* `opacity` - opacity from 0 to 255, maximum by default

Example of a configuration file `layers.ini`:

```ini
[Layer]
provider=osm-standard
filter=grayscale

[Layer]
provider=railway-standard
```

Example:
```
.. -layers layers.ini
```
```
.. -ls layers.ini
```

****

### areas, as [String] 

Using a file with the specified areas for download. Each area starts with `[Area]`. 

Fields used:

* `left` - left boundary (minimum longitude)
* `top` - upper boundary (maximum latitude)
* `right` - right border (maximum longitude)
* `bottom` - lower border (minimum latitude)

Example of a configuration file `areas.ini`:

```ini
# Moscow
[Area]
left=37.1
top=56
right=38
bottom=55.49

# Yekaterinburg
[Area]
left=60
top=57
right=61
bottom=56.6
```

Example:
```
.. -areas areas.ini
```
```
.. -as areas.ini
```

****

### monochrome, m [String] 

The color of monochroms tiles that should not be saved. Available variations:

* `#FFFFFF`
* `rgb(255, 255, 255)`
* `rgba(255, 255, 255, 255)`

Example:
```
.. -monochrome #FFFFFF
```
```
.. -m #FFFFFF
```

****

### monochromes, ms [String] 

Using a file with specified colors of monotonous tiles that should not be saved. Each color starts with `[Monochrome]`. 

Fields used:

* `color` - color in the following variations:
	- `#FFFFFF`
	- `rgb(255, 255, 255)`
	- `rgba(255, 255, 255, 255)`

Example of a configuration file `monochromes.ini`:

```ini
[Monochrome]
color=#C9C9C9

[Monochrome]
color=rgb(255, 255, 255)

[Monochrome]
color=rgba(0, 0, 0, 255)
```

Example:
```
.. -monochromes monochromes.ini
```
```
.. -ms monochromes.ini
```

****

### out, o [String] 

The absolute or relative path for program output. You can use macros that will be replaced with real values when saved. If there are no folders in the path, they will be created.

Macros:

* `{p}` - provider name
* `{z}` - zoom
* `{x}` - X of tile number
* `{y}` - Y of tile number

Example:
```
.. -out tiles/{p}/{z}/{x}_{y}
```
```
.. -o tiles/{p}/{z}/{x}_{y}
```

Default:
```
tiles/{p}/{z}/{x}/{y}
```

****

### cache, c [String] 

The path to the folder with the provider's tiles that have already been downloaded. When using this option, the utility, upon receiving the tile, will check whether there is a tile in the cache folder: if so, the file from the disk will be used, if not, the server will be contacted for the tile. Macros can be used.

Macros:

* `{p}` - provider name
* `{z}` - zoom
* `{x}` - X of tile number
* `{y}` - Y of tile number

Example:
```
.. -cache tiles/{p}/{z}/{x}_{y}
```
```
.. -c tiles/{p}/{z}/{x}_{y}
```

****

### use-cache-only, uco

When using this option, the utility will only work with cached tiles, the path to which is indicated via the *cache* option, without using the Internet.

****

### min-zoom, z [Unsigned Integer]

> Required option 

The lower limit of the zoom.

Example:
```
... -min-zoom 6
```
```
... -z 6
```

### max-zoom, Z [Unsigned Integer]

> Required option

The higher limit of the zoom.

Example:
```
... -max-zoom 7
```
```
... -Z 7
```

****

**Important!** In order for negative coordinate values to be taken into account, they must be specified using the following syntax:
```
... --left=-56.674619
```

### left, l [Double]

The left border of the selected area (minimum longitude).

Example:
```
... -left 57.02137767
```
```
... -l 57.02137767
```


### top, t [Double]

The upper limit of the selected area (maximum latitude).

Example:
```
... -top 120
```
```
... -t 120
```


### right, r [Double]

The right border of the selected area (maximum longitude).

Example:
```
... --bottom=143.1
```
```
... -b 143.1
```

### bottom, b [Double]

The lower boundary of the selected area (minimum latitude).

Example:
```
... -bottom 143.1
```
```
... -b 143.1
```

****

### bbox, bb

Setting the download area as in osmium. The following order is used:

```
... -bb MinLon,MinLat,MaxLon,MaxLat
```

****

### show-file-type, sft

Enabling the display of the `.png` extension in the file name. The extension is always *PNG*.

****

### skip-missing, skeep

Skipping missing tiles when received from the server.

****

### filter, f [String]

Applying a filter to tiles from the *provider*. List of prepared filters:

* `grayscale` - grayscale

Example:
```
... -filter grayscale
```
```
... -f grayscale
```

****

### tile-res, res [Unsigned Integer]

The resolution of the saved images. Use it if you are not satisfied with the original resolution.

Example:
```
./tildy -p railway-standard -z 0 -Z 2 -tile-res 256 
```
```
./tildy -p railway-standard -z 0 -Z 2 -res 256 
```

### version, v

Print program version in format:

``` 
tildy [major].[minor]
```