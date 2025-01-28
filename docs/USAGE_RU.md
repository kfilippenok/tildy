<div align="right">
  🇬🇧 <a href="./USAGE.md">English</a>
  |
  🇷🇺 Русский
</div>

# Использование

## Синопсис
./tildy **[ОПЦИЯ]** **[ПАРАМЕТР]** ...


## Синтаксис

```
./tildy -provider osm-standard ...
```
```
./tildy --provider=osm-standard ...
```

## Опции

### provider, p [String]

Выбор провайдера, который будет предоставлять плитки. Список подготовленных провайдеров: 

* *osm-standard* - OpenStreetMap Standard
* *railway-standard* - OpenRailwayMap Standard
* *railway-maxspeed* - OpenRailwayMap Maxspeed
* *railway-electrification* - OpenRailwayMap Electrification

Пример:
```
... -provider osm-standard
```
```
... -p osm-standard
```

### providers, ps [String]

Вы можете импортировать своих провайдеров, через конфигурационный файл. Каждый провайдер начинается с ``[Provider]``. 

Используемые поля:

* *ident* - идентификатор, который будет использоваться в слоях или в *provider*
* *name* - название провайдера, которое будет потом использоваться при подстановки макросов в *out*
* *url* - ссылка с макросами, по которой будут получаться плитки

Пример конфигурационного файла *custom_providers.ini*:

```ini
[Provider]
ident=osm-standard-local
name=OpenStreetMap-Mapnik
url=http://localhost:8080/tile/{z}/{x}/{y}.png
```

Пример:
```
... -providers custom_providers.ini -provider osm-standard-local
```
```
... -ps custom_providers.ini -p osm-standard-local
```


****

### layers, ls [String] 

Использование файла с конфигурацией со слоями. Слои представляют собой плитки от выбранных провайдеров. Все слои накладываются друг на друга по итогу формируя одну единую. Описание слоёв идёт по порядку с нижнего к верхнему. Каждый слой начинается с ``[Layer]``. 

Используемые поля:

* *provider* - используемый провайдер
* *filter* - используемый фильтр
* *opacity* - непрозрачность от 0 до 255, по умолчанию максимальная

Пример конфигурационного файла *layers.ini*:

```ini
[Layer]
provider=osm-standard
filter=grayscale

[Layer]
provider=railway-standard
```

Пример:
```
.. -layers layers.ini
```
```
.. -ls layers.ini
```

****

### out, o [String] 

Абсолютный или относительный путь для вывода программы. Можно использовать макросы, которые при сохранении будут подменятся на реальные значения. Если папки в пути отсутствуют, они будут созданы.

Макросы:

* ``{p}`` - Имя провайдера
* ``{z}`` - Уровень зума
* ``{x}`` - X номера плитки
* ``{y}`` - Y номера плитки

Пример:
```
.. -out tiles/{p}/{z}/{x}_{y}
```
```
.. -o tiles/{p}/{z}/{x}_{y}
```

По умолчанию:
```
tiles/{p}/{z}/{x}/{y}
```

****

### min-zoom, z [Unsigned Integer]

> Обязательный параметр

Нижняя граница зума.

Пример:
```
... -min-zoom 6
```
```
... -z 6
```

### max-zoom, Z [Unsigned Integer]

> Обязательный параметр

Верхняя граница зума.

Пример:
```
... -Z 7
```

****

**Важно!** Чтобы отрицательные значения координат учитывались, их необходимо указывать через следующий синтаксис:
```
... --left=-56.674619
```

### left, l [Double]

Левая граница выбранной области (минимальная долгота).

Пример:
```
... -left 57.02137767
```
```
... -l 57.02137767
```


### top, t [Double]

Верхняя граница выбранной области (максимальная широта).

Пример:
```
... -top 120
```
```
... -t 120
```


### right, r [Double]

Правая граница выбранной области (максимальная долгота).

Пример:
```
... -right 42.7
```
```
... -r 42.7
```

### bottom, b [Double]

Нижняя граница выбранной области (минимальная широта).

Пример:
```
... -bottom 143.1
```
```
... -b 143.1
```

****

### show-file-type, sft

Включение отображение расширения ```.png``` в названии файла. Расширение всегда *PNG*.

****

### skip-existing, ske

Skipping tiles that already exist on the disk when received from the server.

****

### skip-missing, skm

Пропуск отсутствующих плиток при получении с сервера.

****

### filter, f [String]

Наложение фильтра к плиткам от ``provider``. Список подготовленных фильтров: 

* *grayscale* - тёмно-серый

Пример:
```
... -filter grayscale
```
```
... -f grayscale
```

****

### tile-res, res [Unsigned Integer]

Разрешение сохраняемых изображений. Используйте, если оригинальное разрешение вас не устраивает.

Пример:
```
./tildy -p railway-standard -z 0 -Z 2 -tile-res 256 
```
```
./tildy -p railway-standard -z 0 -Z 2 -res 256 
```

### version, v

Вывод версии программы в формате:

``` 
tildy [мажор].[минор]
```