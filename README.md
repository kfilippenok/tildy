# tiledownloader

English | [Russian](./README_RU.md)

## Synopsis
./tilesdownloader **[OPTION]** **[PARAMETER]** ...

## Syntax

```
./tilesdownloader -provider osm ...
```
```
./tilesdownloader --provider=osm ...
```

## Options

### provider [String]

You can use prepared providers. 

* *osm* - OpenStreetMap
* *otm* - Open Topo Map
* *osm-cycle* - OpenStreetMap Cycle
* *railway* - OpenRailwayMap


### provider-name [String]

You can specify provider name. You will need this when saving images. Can be used in conjunction with [provider](#provider).

Example:
```
... -provider-name MyProviderName
```
```
... -provider-name My Provider Name
```


### provider-link [String]

Specifying your own link to the provider for downloading tiles without a slash at the end.

Example:
```
... -provider-link http://b.tiles.openrailwaymap.org/standard
```

****

### output [String]

You can specify the absolute or relative path where the images will be downloaded. 

Example:
```
/home/user1/tiles
```
```
mydir
```

Default:
```
tiles
```

****

### save-method [String]

You can choose which way the images will be sorted when downloading.  

* *folders* - By folders: ```provider/zoom/x/y```
* *pattern* - By divider in one folder: ```provider_zoom_x_y``` (Here used "_" as divider)

Default: *folders*


### divider [String]

Used in conjunction with ```pattern``` [save-method](#save-method). It is a symbol that will separate the keywords in the file name.

Example: 
```
... save-method pattern -divider _
```
Result file will be *ProviderName_zoom_x_y*

Default: _

****

### min-zoom [Unsigned Integer]

Lower zoom limit, in range 0..19.

Example:
```
... -min-zoom 6
```

Default: *6*

### max-zoom [Unsigned Integer]

Highest zoom limit, in range 0..19.

Example:
```
... -max-zoom 7
```

Default: *7*

****
![coordinates](docs/img/coordinates.png)


**Attention!** To work with negative values, you must use the following syntax

```
... --fcoord-lat=-56.674619
```


### fсoord-lat [Double]

Latitude of first coordinate.

Example:
```
... --fсoord-lat=56.674619
```


### fсoord-lon [Double]

Longtitude of first coordinate.

Example:
```
... --fсoord-lon=60.287416
```


### sсoord-lat [Double]

Latitude of second coordinate.

Example:
```
... --fсoord-lat=57.029763
```

### sсoord-lon [Double]

Longtitude of second coordinate.

Example:
```
... --fсoord-lat=60.921877
```

****

### show-file-type

If you need the file extension in the name, use this option. The parameter with the file extension is not specified, since the image is always downloaded as a *PNG*.

****

### full-map

Download full map. Coordinates are not used.

Example:
```
./tilesdownloader -provider osm -full-map
```