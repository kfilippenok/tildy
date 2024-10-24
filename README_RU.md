# tiledownloader

[English](./README.md) | Russian

## Синопсис
./tilesdownloader **[ОПЦИЯ]** **[ПАРАМЕТР]** ...

## Синтаксис

```
./tilesdownloader -provider osm ...
```
```
./tilesdownloader --provider=osm ...
```

## Опции

### provider [String]

Вы можете исопльзовать подготовленных провайдеров, у которых уже заполнены [provider-name](#provider-name) и [provider-link](#provider-link). 

* *osm* - OpenStreetMap
* *otm* - Open Topo Map
* *osm-cycle* - OpenStreetMap Cycle
* *railway* - OpenRailwayMap


### provider-name [String]

Вы можете указать имя провайдера. Оно понядобится при сохранении файлов. Можно использовать совместно с [provider](#provider), чтобы при сохранении применялось указанное имя.

Пример:
```
... -provider-name MyProviderName
```
```
... -provider-name My Provider Name
```


### provider-link [String]

Ссылка на провайдера, с которого будут скачиваться плитки. Указывается без слеша в конце. Нужно использовать в том случае, когда необходимого провайдера нет в списке подготовленных.

Пример:
```
... -provider-link http://b.tiles.openrailwaymap.org/standard
```

****

### output [String]

Абсолютный или относительный путь для вывода программы. Если папки в пути отсутствуют, они будут созданы.

Пример:
```
/home/user1/tiles
```
```
mydir
```

По умолчанию:
```
tiles
```

****

### save-method [String]

Способ сортировки изображений при скачивании.  

* *folders* - По папкам: ```provider/zoom/x/y```
* *pattern* - По шаблону в одну папку: ```provider_zoom_x_y``` (Здесь использован символ "_" как разделитель)

По умолчанию: *folders*


### divider [String]

Разделитель в названии файла при методе скачивания ```pattern```.

Пример: 
```
... save-method pattern -divider _
```
Result file will be *ProviderName_zoom_x_y*

По умолчанию: _

****

### min-zoom [Unsigned Integer]

Нижняя граница зума, в диапозоне 0..19.

Example:
```
... -min-zoom 6
```

По умолчанию: *6*

### max-zoom [Unsigned Integer]

Верхняя граница зума, в диапозоне 0..19.

Пример:
```
... -max-zoom 7
```

По умолчанию: *7*

****
![coordinates](docs/img/coordinates.png)


**Важно!** Чтобы отрицательные значения координат учитывались, их необходимо указывать через следующий синтаксис:
```
... --fcoord-lat=-87.5
```

### fсoord-lat [Double]

Широта первой координаты.

Пример:
```
... --fсoord-lat=56.674619
```


### fсoord-lon [Double]

Долгота первой координаты.

Пример:
```
... --fсoord-lon=60.287416
```


### sсoord-lat [Double]

Широта второй координаты.

Пример:
```
... --fсoord-lat=57.029763
```

### sсoord-lon [Double]

Долгота второй координаты.

Пример:
```
... --fсoord-lat=60.921877
```

****

### show-file-type

Включение отображение расширения ```.png``` в названии файла. Расширение всегда *PNG*.
