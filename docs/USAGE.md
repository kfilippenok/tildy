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

* *osm-standard* - OpenStreetMap Standard
* *railway-standard* - OpenRailwayMap Standard
* *railway-maxspeed* - OpenRailwayMap Maxspeed
* *railway-electrification* - OpenRailwayMap Electrification

Example:

```
... -provider osm-standard
```
```
... -p osm-standard
```

### providers, ps [String]

You can import your providers via the configuration file. Each provider starts with ``[Provider]``. 

Fields used:

* *identifier* - the identifier to be used in layers or in *provider*
* *name* - the name of the provider, which will then be used when substituting macros in *out*
* *url* - a link with macros that will be used to get tiles.

Example of a configuration file *custom_providers.ini*:

```ini
[Provider]
ident=osm-standard-local
name=OpenStreetMap-Mapnik
url=http://localhost:8080/tile/{z}/{x}/{y}.png
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

Using a configuration file with layers. The layers are tiles from selected providers. All the layers are superimposed on each other, eventually forming one single one. The layers are described in order from the bottom to the top. Each layer starts with ``[Layer]``. 

Fields used:

* *provider* - the provider used
* *filter* - the filter used
* *opacity* - opacity from 0 to 255, maximum by default

Example of a configuration file *layers.ini*:

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

### out, o [String] 

The absolute or relative path for program output. You can use macros that will be replaced with real values when saved. If there are no folders in the path, they will be created.

Macros:

* ``{p}`` - provider name
* ``{z}`` - zoom
* ``{x}`` - X of tile number
* ``{y}`` - Y of tile number

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

Enabling the display of the ``.png`` extension in the file name. The extension is always *PNG*.

****

### skip-missing, skeep

Skipping missing tiles when received from the server.

****

### filter, f [String]

Applying a filter to tiles from the ``provider``. List of prepared filters:

* *grayscale* - grayscale

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