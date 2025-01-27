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

### provider [String]

Choosing the provider that will provide the tiles. List of prepared providers: 

* *osm-standard* - OpenStreetMap Standard
* *railway-standard* - OpenRailwayMap Standard
* *railway-maxspeed* - OpenRailwayMap Maxspeed
* *railway-electrification* - OpenRailwayMap Electrification


### providers [String]

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


****

### layers [String] 

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

****

### out [String] 

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

Default:
```
tiles/{p}/{z}/{x}/{y}
```

****

### min-zoom [Unsigned Integer]

> Required option 

The lower limit of the zoom.

Example:
```
... -min-zoom 6
```

### max-zoom [Unsigned Integer]

> Required option

The higher limit of the zoom.

Example:
```
... -max-zoom 7
```

****

**Important!** In order for negative coordinate values to be taken into account, they must be specified using the following syntax:
```
... --left=-56.674619
```

### left [Double]

The left border of the selected area (minimum longitude).

Example:
```
... --left=57.02137767
```


### top [Double]

The upper limit of the selected area (maximum latitude).

Example:
```
... --top=120
```


### right [Double]

The right border of the selected area (maximum longitude).

Example:
```
... --right=42.7
```

### bottom [Double]

The lower boundary of the selected area (minimum latitude).

Example:
```
... --bottom=143.1
```

****

### show-file-type

Enabling the display of the ``.png`` extension in the file name. The extension is always *PNG*.

****

### skip-missing

Skipping missing tiles when received from the server.

****

### filter

Applying a filter to tiles from the ``provider``. List of prepared filters:

* *grayscale* - grayscale

****

### tile-res [Unsigned Integer]

The resolution of the saved images. Use it if you are not satisfied with the original resolution.

Example:
```
./tildy -provider railway-standard -min-zoom 0 -max-zoom 2 -tile-res 256
```

### version

Print program version in format:

``` 
tildy [major].[minor]
```