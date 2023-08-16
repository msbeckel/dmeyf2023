# Repositorios remotos

En general los sistemas de versionado se utilizan para trabajar
colaborativamente. Para eso uno tiene que publicar su código en algun lado
para que otros lo vean y puedan usarlo y agregar sus cambios.

Rara vez uno tiene un repositorio solo. En general lo usa en algun servidor
como GitHub o GitLab. Para eso git tiene un manejo de repositorios remotos con
`git remote`.

Dentro de nuestro repo probemos:

```shell
$ git remote
$
```

No pasó nada? Agreguemos una `-v` (de verborragia):
```shell
$ git remote -v
$
```

Nada? Bueno, tiene sentido. Nuestro repositorio es local y nunca lo bajamos de
otro lado ni lo publicamos. Probemos tomando un repositorio de algun otro lado

# Clonando repositorios

Busquemos uno en GitHub: el de la materia, por supuesto! https://github.com/dmecoyfin/dmeyf2023

Navegamos a algún lugar de nuestros directorios (uno que no sea ya un
repositorio git) y hacemos `git clone`:
```shell
$ git clone https://github.com/dmecoyfin/dmeyf2023.git
$ cd dmeyf2023
```

Nota: para la materia van a tener que tener su propio repositorio derivado
(fork) del de la materia. Estos pasos son ilustrativos.

Ahora probemos de vuelta que pasa con los remotos:

```shell
$ git remote
origin
$
```

Bueno es algo. Sabemos que existe un remoto que se llama `origin`. Es el
nombre por defecto del remoto de donde se clonó el repo. También podemos ver
más detalle agregando `-v`:

```shell
$ git remote -v
origin  https://github.com/dmecoyfin/dmeyf2023.git (fetch)
origin  https://github.com/dmecoyfin/dmeyf2023.git (push)
```

