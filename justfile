docker-image := "renatogarcia/hakyll-diagrams-ci"
ci-cache := "./ci-files"

install-cabal:
    docker run --rm -v {{ci-cache}}/ghcup:/root/.ghcup hakyll-diagrams-ci ghcup install cabal 3.12.1.0

install-ghc ghcver:
    docker run --rm -v {{ci-cache}}/ghcup:/root/.ghcup {{docker-image}} ghcup install ghc {{ghcver}}
    docker run --rm -v {{ci-cache}}/ghcup:/root/.ghcup {{docker-image}} ghcup set ghc {{ghcver}}

update-cabal: install-cabal
    docker run --rm -v {{ci-cache}}/ghcup:/root/.ghcup -v {{ci-cache}}/cabal:/root/.cabal {{docker-image}} cabal update

build projfile ghcver: (install-ghc ghcver) update-cabal
    docker run --rm -v .:/host -v {{ci-cache}}/ghcup:/root/.ghcup -v {{ci-cache}}/cabal:/root/.cabal -w /host {{docker-image}} cabal build --write-ghc-environment-files=always --project-file={{projfile}} --builddir=dist-{{ghcver}}

test projfile:
    docker run --rm -v .:/host -v {{ci-cache}}/ghcup:/root/.ghcup -v {{ci-cache}}/cabal:/root/.cabal -w /host -e HOME=/root {{docker-image}} cabal test --project-file={{projfile}}

build-all:
    jq -c '.[]' .justfile.json | while read row; do \
      just build                                    \
        $(jq -r '.projfile' <<< "$row")             \
        $(jq -r '.ghcver' <<< "$row");              \
    done

test-all:
    jq -c '.[]' .justfile.json | while read row; do \
       just test $(jq -r '.projfile' <<< "$row");   \
    done
