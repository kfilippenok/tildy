<div align="right">
  🇬🇧 English
  |
  🇷🇺 <a href="./RESTRICTIONS_RU.md">Русский</a>
</div>

# RESTRICTIONS

🇬🇧 English | 🇷🇺 [Русский](./RESTRICTIONS_RU.md)

## OpenStreetMap

When downloading tiles from servers owned by OpenStreetMap, you should be aware of the restrictions. Officially, the massive diwnload of tiles (according to my observations, it is more than 1000) is prohibited. So, the server restrict the download speed and the number of tiles to be given. **In case of abuse, you may be banned, be careful!** This is due to the fact that the servers operate at the expense of modest donations, which are not enough for servers whose capacity would allow them not to impose restrictions.

> Detailed. Tile Usage Policy - https://operations.osmfoundation.org/policies/tiles/

As a solution to these problems, it is recommended to use your local server, which will generate tiles similar to OSM.

## Solution. Local OSM Server

Used sources:
- *openstreetmap-tile-server* - https://github.com/Overv/openstreetmap-tile-server
- *Using OpenStreetMap Offline* - https://www.gibbard.me/openstreetmap/
- *switch2osm* - https://switch2osm.org/

This instruction describes how I do it.

### Prepare Docker

#### Install

Alt Linux p11:
```bash
sudo apt-get install docker-engine
```

[Для других дистрибутивов](https://docs.docker.com/engine/install/)

#### Lounch

Docker works as a service, so to start it automatically, you need to enable autorun every time you start it. We will also launch it immediately:

```bash
systemctl enable --now docker
```

Checking if Docker has started:

```bash
systemctl status docker
```

Must be:

```bash
● docker.service - Docker Application Container Engine
     Loaded: loaded (/usr/lib/systemd/system/docker.service; enabled; preset: disabled)
     Active: active (running) since Tue 2024-11-19 13:56:58 +05; 2 days ago
TriggeredBy: ● docker.socket
```

#### Create volume for data

*Volume* in Docker simulates the operation of partitions. For our server to work, it is enough to create one *volume*:

```bash
docker volume create osm-data
```

Check if the *volume* has been created:

```bash
docker volume ls
```

Must be:

```bash
DRIVER    VOLUME NAME
local     osm-data
```


### Cartographic data

Next, you need to download the cartographic data in PDF format, on the basis of which the database inside the container will be filled in. The official PBF from OpenStreetMap can be downloaded from the website [download.geofabrik.de](https://download.geofabrik.de/). For example, I'll take [данные Российской Федерации](https://download.geofabrik.de/russia-latest.osm.pbf).


### Importing data into a container DBMS

> Be prepared that you will need a lot of space. It all depends on how much PBF you have.

Based on the PBF, the server will populate the database. Here it is necessary to determine where they will have to be. You can fill in the *osm_data* (*volume*, which we created above), which is stored in the system directory along with docker. Then the command will look like this:

```bash
docker run \
    -v /absolute/path/to/russia-latest.osm.pbf:/data/region.osm.pbf \
    -v osm-data:/data/database/ \
    overv/openstreetmap-tile-server \
    import
```

I will save it to my home directory, since there is more space there:

```bash
docker run \
    -v /home/kirill/osm_pbf/russia-latest.osm.pbf:/data/region.osm.pbf \
    -v /home/kirill/osm_database/:/data/database/ \
    overv/openstreetmap-tile-server \
    import
```

Make sure that the paths are specified correctly, especially to PBF, because if docker does not find a file on the specified path, the container will load PBF Luxembourg for example.

Next, you will start the data import process. If everything goes well, the console will display:

```bash
exit 0
```

### Starting the server

To run, you need to slightly modify the import command by adding an option to forward the container ports to the system ports, and change the import command to run. In the first version, it will look like this:

```bash
docker run \
    -p 8080:80 \
    -v osm-data:/data/database/ \
    -d overv/openstreetmap-tile-server \
    run
```

In the case of saving to the home directory, it will be like this:

```bash
docker run \
    -p 8080:80 \
    -v /home/kirill/osm_database/:/data/database/ \
    -d overv/openstreetmap-tile-server \
    run
```

View the list of active containers:

```bash
docker container ls
```

or

```bash
docker ps
```

Must be:

```bash
CONTAINER ID   IMAGE                             COMMAND         CREATED        STATUS        PORTS                                             NAMES
26cce2b17e6b   overv/openstreetmap-tile-server   "/run.sh run"   22 hours ago   Up 22 hours   5432/tcp, 0.0.0.0:8080->80/tcp, :::8080->80/tcp   thirsty_black
```

Your ```UPTIME``` must be smaller.

If you don't see your container in the list, try to see the full list:

```bash
docker ps -a
```

If you see ```Exited (1)``` in ```STATUS```, then an error has occurred. More information about the error can be viewed via:

```bash
docker logs <container name>
```

The name of the container is indicated in the column ```NAMES```.

### Download tiles

After launching the container, you can receive tiles at::

```
http://localhost:8080/tile/{z}/{x}/{y}.png
```

Trying to download the tiles we need:

```bash
tildy/examples/custom_providers.sh
```

It will be slow the first time, as the tiles will be generated in real time. You can generate them in advance:

Connecting to the container terminal:

```bash
docker exec -it thirsty_black /usr/bin/sh
```

Rendering by zoom level from 0 to 6 for the whole world, in 2 streams (-n 2):

```bash
render_list --all -z 0 -Z 6 -n 2
```

### What can be improved

For a complete list of all options to improve performance, such as: configuring memory usage, threads used, etc., see the list of sources that are indicated at the beginning of the OSM section.