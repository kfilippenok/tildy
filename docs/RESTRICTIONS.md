# Ограничения

## OpenStreetMap

При загрузке плиток с серверов, принадлежащих OpenStreetMap, вы должны знать ограничения. Официально массовая загрузка плиток (по мои наблюдениями это более чем 1000) запрещена. Так, сервер ограничивает скорость скачивания и количество отдаваемых плиток. **В случае злоупотребления вас могут забанить, будьте осторожны!** Это связано с тем, что сервера работают за счёт скромных пожертвований, которых не хватает на сервера, мощность которых позволило бы не накладывать ограничения. 

> Подробнее. Tile Usage Policy - https://operations.osmfoundation.org/policies/tiles/

В качестве решения этих проблем, рекомендуется использовать свой локальный сервер, который будет генерировать плитки, аналогичные OSM.

## Решение. Локальный сервер OSM

Список используемых источников:
- *openstreetmap-tile-server* - https://github.com/Overv/openstreetmap-tile-server
- *Using OpenStreetMap Offline* - https://www.gibbard.me/openstreetmap/
- *switch2osm* - https://switch2osm.org/

Эта инструкция описывает то, как это делаю я. 

### Подготовка Docker

#### Установка

Alt Linux p11:
```bash
sudo apt-get install docker-engine
```

[Для других дистрибутивов](https://docs.docker.com/engine/install/)

#### Запуск

Dcoker работает как сервис, поэтому для его автоматического старта при каждом запуске нужно включить автозапуск. Так же мы сразу его запустим:

```bash
systemctl enable --now docker
```

Проверяем запустился ли Docker:

```bash
systemctl status docker
```

Должно быть так:

```bash
● docker.service - Docker Application Container Engine
     Loaded: loaded (/usr/lib/systemd/system/docker.service; enabled; preset: disabled)
     Active: active (running) since Tue 2024-11-19 13:56:58 +05; 2 days ago
TriggeredBy: ● docker.socket
```

#### Создание volume для данных

*Volume* в Docker имитирует работу разделов. Для работы нашего сервера достаточно создать одного *volume*:

```bash
docker volume create osm-data
```

Проверяем создался ли *volume*:

```bash
docker volume ls
```

Должно быть так:

```bash
DRIVER    VOLUME NAME
local     osm-data
```


### Картографические данные

Дальше необходимо скачать картографические данные в формате PBF, на основе которых будет заполнятся база данных внутри контейнера. Официальные PBF от OpenStreetMap можно скачать с сайта [download.geofabrik.de](https://download.geofabrik.de/). Для примера я возьму [данные Российской Федерации](https://download.geofabrik.de/russia-latest.osm.pbf).


### Импорт данных в СУБД контейнера

> Будьте готовы, что понадобится не мало места. Всё зависит от того, какого объёма у вас PBF.

На основе PBF сервер будет заполнять базу данных. Тут нужно определиться, где они должны будут находиться. Можно заполнять внутри *osm_data* (*volume*, который мы создали выше), которая хранится в системном каталоге вместе с docker. Тогда команда будет выглядеть так:

```bash
docker run \
    -v /absolute/path/to/russia-latest.osm.pbf:/data/region.osm.pbf \
    -v osm-data:/data/database/ \
    overv/openstreetmap-tile-server \
    import
```

Я же буду сохранять в домашнюю директорию, так как там больше места:

```bash
docker run \
    -v /home/kirill/osm_pbf/russia-latest.osm.pbf:/data/region.osm.pbf \
    -v /home/kirill/osm_database/:/data/database/ \
    overv/openstreetmap-tile-server \
    import
```

Убедитесь, что пути указаны верно, в особенности к PBF, так как если docker не найдёт по укзанному пути файл, котейнер загрузит PBF Люксембурга для примера.

Дальше у вас запустится процесс импорта данных. Если всё пройдёт успешно, в консоле выведется:
```bash
exit 0
```

### Запуск сервера

Для запуска нужно слегка модифицировать команду для импорта, добавив опцию для проброса портов контейнера на системные, и изменить команду import на run. В первом варианте она будет выглядеть так:

```bash
docker run \
    -p 8080:80 \
    -v osm-data:/data/database/ \
    -d overv/openstreetmap-tile-server \
    run
```

В варианте с сохранением в домашнюю директорию будет так:

```bash
docker run \
    -p 8080:80 \
    -v /home/kirill/osm_database/:/data/database/ \
    -d overv/openstreetmap-tile-server \
    run
```

Посмотреть список активных контейнеров:

```bash
docker container ls
```

или

```bash
docker ps
```

Должно быть так:

```bash
CONTAINER ID   IMAGE                             COMMAND         CREATED        STATUS        PORTS                                             NAMES
26cce2b17e6b   overv/openstreetmap-tile-server   "/run.sh run"   22 hours ago   Up 22 hours   5432/tcp, 0.0.0.0:8080->80/tcp, :::8080->80/tcp   thirsty_black
```

Ваш ```UPTIME``` должен быть меньше.

Если вы не видите в списке свой контейнер, попробуйте посмотреть полный список:

```bash
docker ps -a
```

Если в ```STATUS``` вы видите ```Exited (1)```, значит произошла ошибка. Подробнее об ошибке можно посмотреть через:

```bash
docker logs <название контейнера>
```

Название контейнера указано в столбце ```NAMES```.

### Получение плиток

После запуска контейнера, можно получать плитки по адресу:
```
http://localhost:8080/tile/{z}/{x}/{y}.png
```

Пробуем скачать нужные нам плитки:

```bash
./tilesdownloader \
    -min-zoom 10 \
    -max-zoom 10 \
    -provider-name MyProviderName \
    -provider-link http://localhost:8080/tile \
    -fcoord-lat 57 \
    -fcoord-lon 31 \
    -scoord-lat 50 \
    -scoord-lon 70 \
    -pattern %provider-name%_%x%_%y%_%z% \
    -output tiles/local \
```

И радуемся скорости загрузки :)

![coordinates](./media/localosm_download_demo.gif)

В первый раз это будет медленно, так как плитки будут генерироваться в реальном времени. Можно заранее их сгенерировать:

Подключаемся к терминалу контейнера:

```bash
docker exec -it thirsty_black /usr/bin/sh
```

Рендерим по уровню зума от 0 до 6 для всего мира, в 2 потока (-n 2):

```bash
render_list --all -z 0 -Z 6 -n 2
```

### Что можно улучшить

Полный список всех опций для повышения производительности, таких как: настройка занимаемой памяти, используемых потоков и пр. смотреть в списке источников, что указаны в начале раздела с OSM.